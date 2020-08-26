-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen, Alex Reinking
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving  #-}

-----------------------------------------------------------------------------
-- constctailctor reuse analysis
-----------------------------------------------------------------------------

module Core.CTail ( ctailOptimize, uctailOptimize ) where

import Lib.Trace (trace)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.IntMap as M

import Kind.Kind
import Kind.Newtypes
import Type.Type
import qualified Type.Pretty as Pretty
import Type.Assumption -- Gamma

import Lib.PPrint
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty

--------------------------------------------------------------------------
-- Reference count transformation
--------------------------------------------------------------------------
ctailOptimize :: Pretty.Env -> Platform -> Newtypes -> Gamma -> DefGroups -> Int -> (DefGroups,Int)
ctailOptimize penv platform newtypes gamma defs uniq
  = runUnique uniq (uctailOptimize penv platform newtypes gamma defs)

uctailOptimize :: Pretty.Env -> Platform -> Newtypes -> Gamma -> DefGroups -> Unique DefGroups
uctailOptimize penv platform newtypes gamma defs
  = ctailRun penv platform newtypes gamma (ctailDefGroups True defs)    
  where penv' = penv{Pretty.coreShowDef=True,Pretty.coreShowTypes=False,Pretty.fullNames=False}
        tr d = trace (show (vcat (map (prettyDefGroup penv') d)))

--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

ctailDefGroups :: Bool -> DefGroups -> CTail DefGroups
ctailDefGroups topLevel defs
  = do dss <- mapM (ctailDefGroup topLevel)  defs
       return (concat dss)

ctailDefGroup :: Bool -> DefGroup -> CTail [DefGroup]
ctailDefGroup topLevel dg
  = case dg of
      DefRec [def] | hasCTailCall (defName def) True (defExpr def) -> ctailDef topLevel def
      _ -> return [dg]
    

ctailDef :: Bool -> Def -> CTail [DefGroup]
ctailDef topLevel def
  = withCurrentDef def $
    do ctailTrace "ctail call definition"
       resRefType <- hasResultRefType (defType def)
       case resRefType of
         Nothing -> return [DefRec [def]]
         Just (tforall,tpreds,targs,teff,tres)
           -> do ctailTrace "- has reference type result"
                 ctailSlotType <- makeSlotType tres
                 let ctailName = makeHiddenName ".ctail" (defName def)
                     ctailSlot = newHiddenName ".acc"
                     ctailType = tForall tforall tpreds (TFun (targs ++ [(ctailSlot,ctailSlotType)]) teff typeUnit)
                     cdefExpr  = makeCDefExpr (TName ctailSlot ctailSlotType) (defExpr def)
                 expr <- ctailExpr (defExpr def)
                 return [DefRec [def]] -- {defExpr=expr}])

       
makeCDefExpr :: TName -> Expr -> Expr       
makeCDefExpr slot (TypeLam targs body)  = TypeLam targs (makeCDefExpr slot body)
makeCDefExpr slot (Lam args eff body)   = Lam (args ++ [slot]) eff body
makeCDefExpr slot body                  = failure $ "Core.CTail: illegal ctail function shape: " ++ show body
       
       
hasResultRefType :: Type -> CTail (Maybe ([TypeVar],[Pred],[(Name,Type)],Effect,Type))
hasResultRefType tp
  = case splitFunScheme tp of 
      Just t@(foralls,preds,targs,teff,tres) 
        -> do isRType <- hasRefType tres
              return (if (isRType) then (Just t) else Nothing)
      _ -> return Nothing
      
hasRefType :: Type -> CTail Bool      
hasRefType tp
  = case expandSyn tp of 
      TApp t ts   -> hasRefType t
      TCon con    -> not <$> isValueType (typeConName con)
      _           -> return False
      
      
      
--------------------------------------------------------------------------
-- Does there exist a tail call definition?
--------------------------------------------------------------------------
       
hasCTailCall :: Name -> Bool -> Expr -> Bool
hasCTailCall defName top expr
  = case expr of
      TypeLam tpars body  -> hasCTailCall defName top body
      TypeApp body targs  -> hasCTailCall defName top body
      Let dgs body        -> hasCTailCall defName top body
      Lam pars eff body   -> if (top) then hasCTailCall defName False body else False
      Case _ branches     -> any (hasCTailCallBranch defName) branches
      
      App (TypeApp (Con{}) _) args  -> hasCTailCallArg defName (reverse args)
      App (Con{}) args              -> hasCTailCallArg defName (reverse args)
      _                   -> False

hasCTailCallBranch defName (Branch pat guards) 
  = any (hasCTailCallGuard defName) guards
  
hasCTailCallGuard defName (Guard test expr)
  = hasCTailCall defName False expr


hasCTailCallArg :: Name -> [Expr] -> Bool
hasCTailCallArg defName [] = False
hasCTailCallArg defName (rarg:rargs)
  = case rarg of
      App (TypeApp (Var name _) targs) args   | (defName == getName name) -> True
      App (Var name _) args                   | defName == getName name -> True
      _ -> (isTotal rarg && hasCTailCallArg defName rargs)
       

--------------------------------------------------------------------------
-- Main PARC algorithm
--------------------------------------------------------------------------

ctailExpr :: Expr -> CTail Expr
ctailExpr expr
  = case expr of
      TypeLam tpars body
        -> TypeLam tpars <$> ctailExpr body
      TypeApp body targs
        -> (`TypeApp` targs) <$> ctailExpr body
      Lam pars eff body
        -> Lam pars eff <$> ctailExpr body
      App fn args
        -> liftM2 App (ctailExpr fn) (mapM ctailExpr args)

      Let dgs body
        -> do body' <- ctailExpr body
              return (Let dgs body)
      
      Case scrutinees branches
        -> liftM2 Case (mapM ctailExpr scrutinees) (ctailBranches branches)

      -- Var, Lit, Con
      _ -> return expr

ctailBranches :: [Branch] -> CTail [Branch]
ctailBranches branches
  = mapM ctailBranch branches
    
ctailBranch :: Branch -> CTail Branch
ctailBranch (Branch pats guards)
  = do guards' <- mapM ctailGuard guards             
       return (Branch pats guards')


ctailGuard :: Guard -> CTail (Guard)
ctailGuard (Guard test expr)  -- expects patAdded in depth-order
  = do test' <- ctailExpr test
       expr' <- ctailExpr expr
       return (Guard test' expr')

--------------------------------------------------------------------------
-- Utilities for readability
--------------------------------------------------------------------------

-- create a unique name specific to this module
uniqueTName :: Type -> CTail TName
uniqueTName tp = (`TName` tp) <$> uniqueName "ctail"

-- for mapping over a set and collecting the results into a list.
foldMapM :: (Monad m, Foldable t) => (a -> m b) -> t a -> m [b]
foldMapM f = foldr merge (return [])
  where merge x r = do y <- f x
                       (y:) <$!> r

maybeStats :: [Maybe Expr] -> Expr -> Expr
maybeStats xs expr
  = makeStats (catMaybes xs ++ [expr])

--------------------------------------------------------------------------
-- CTail monad
--------------------------------------------------------------------------

-----------------
-- definitions --


data Env = Env { currentDef :: [Def],
                 prettyEnv :: Pretty.Env,
                 platform  :: Platform,
                 newtypes :: Newtypes,
                 gamma :: Gamma
               }

data CTailState = CTailState { uniq :: Int }

type CTailM a = ReaderT Env (State CTailState) a

newtype CTail a = CTail (CTailM a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState CTailState)

instance HasUnique CTail where
  updateUnique f = do { old <- getUniq; modifyUniq f; return old }
  setUnique = setUniq

withEnv :: (Env -> Env) -> CTail a -> CTail a
withEnv = local

getEnv :: CTail Env
getEnv = ask

updateSt :: (CTailState -> CTailState) -> CTail ()
updateSt = modify

getSt :: CTail CTailState
getSt = get

ctailRun :: Pretty.Env -> Platform -> Newtypes -> Gamma -> CTail a -> Unique a
ctailRun penv platform newtypes gamma (CTail action)
  = withUnique $ \u ->
      let env = Env [] penv platform newtypes gamma 
          st = CTailState u
          (val, st') = runState (runReaderT action env) st
       in (val, uniq st')


-------------------
-- env accessors --

getCurrentDef :: CTail [Def]
getCurrentDef = currentDef <$> getEnv

withCurrentDef :: Def -> CTail a -> CTail a
withCurrentDef def = withEnv (\e -> e { currentDef = def : currentDef e })

--

getPrettyEnv :: CTail Pretty.Env
getPrettyEnv = prettyEnv <$> getEnv

withPrettyEnv :: (Pretty.Env -> Pretty.Env) -> CTail a -> CTail a
withPrettyEnv f = withEnv (\e -> e { prettyEnv = f (prettyEnv e) })

getPlatform :: CTail Platform
getPlatform = platform <$> getEnv

---------------------
-- state accessors --

getUniq :: CTail Int
getUniq = uniq <$> getSt

modifyUniq :: (Int -> Int) -> CTail ()
modifyUniq f = updateSt (\s -> s { uniq = f (uniq s) })

setUniq :: Int -> CTail ()
setUniq = modifyUniq . const


isValueType :: Name -> CTail Bool
isValueType name
  = do env <- getEnv
       case newtypesLookupAny name (newtypes env) of
         Just dataInfo -> return $ dataReprIsValue (fst (getDataRepr dataInfo))
         Nothing       -> return False


makeSlotType :: Type -> CTail Type
makeSlotType tp
 = return $ TApp (TCon (TypeCon nameTpResolveSlot (kindFun kindStar kindStar))) [tp]


--------------------------------------------------------------------------
-- Tracing
--------------------------------------------------------------------------

ctailTraceDoc :: (Pretty.Env -> Doc) -> CTail ()
ctailTraceDoc f
 = do pretty <- getPrettyEnv
      ctailTrace (show (f pretty))

ctailTrace :: String -> CTail ()
ctailTrace msg
 = do defs <- getCurrentDef
      trace ("Core.CTail: " ++ show (map defName defs) ++ ": " ++ msg) $
        return ()

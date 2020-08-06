-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen, Alex Reinking
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving  #-}

-----------------------------------------------------------------------------
-- constructor reuse analysis
-----------------------------------------------------------------------------

module Backend.C.ParcReuse ( parcReuseCore ) where

import Lib.Trace (trace)
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe ( catMaybes, fromMaybe )
import Data.Char
import Data.Set ( (\\) )
import qualified Data.Set as S
import qualified Data.IntMap as M

import Kind.Kind
import Kind.Newtypes
import Type.Type
import Type.TypeVar
import Type.Kind( getKind )
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.Name
import Common.Range
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax


import Core.Core
import Core.Pretty
import Core.CoreVar

import Platform.Runtime (unsafePerformIO)
import qualified System.Environment as Sys

{-# NOINLINE enabled #-}
enabled :: Bool
enabled = unsafePerformIO $ do
  e <- Sys.lookupEnv "KK_PARC"
  case e of
    Nothing -> return False
    Just val -> return $ map toLower val `elem` ["1", "on", "yes", "true", "y", "t"]

--------------------------------------------------------------------------
-- Reference count transformation
--------------------------------------------------------------------------

parcReuseCore :: Pretty.Env -> Newtypes -> Core -> Unique Core
parcReuseCore penv newtypes core
  | not enabled = return core
  | otherwise   = do defs <- runReuse penv newtypes (ruDefGroups True (coreProgDefs core))
                     trace (show (vcat (map (prettyDefGroup penv{Pretty.coreShowDef=True,Pretty.coreShowTypes=False,Pretty.fullNames=False})
                                            defs))) $
                      return core{ coreProgDefs  = defs }

--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

ruDefGroups :: Bool -> DefGroups -> Reuse DefGroups
ruDefGroups topLevel = mapM (ruDefGroup topLevel)

ruDefGroup :: Bool -> DefGroup -> Reuse DefGroup
ruDefGroup topLevel dg
  = case dg of
      DefRec    defs -> DefRec    <$> mapM (ruDef topLevel) defs
      DefNonRec def  -> DefNonRec <$> ruDef topLevel def

ruDef :: Bool -> Def -> Reuse Def
ruDef topLevel def
  = -- (if topLevel then isolated_ else id) $
    withCurrentDef def $
    do expr <- ruExpr (defExpr def)
       return def{defExpr=expr}

--------------------------------------------------------------------------
-- Main PARC algorithm
--------------------------------------------------------------------------

ruExpr :: Expr -> Reuse Expr
ruExpr expr
  = case expr of    
      App con@(Con cname repr) args
        -> do args' <- mapM ruExpr args
              newtypes <- getNewtypes
              let size = constructorSize newtypes repr (map typeOf args)
              available <- getAvailable
              case (M.lookup size available) of
                Just tnames | not (S.null tnames) 
                  -> do let tname   = S.elemAt 0 tnames
                            tnames' = S.deleteAt 0 tnames
                        setAvailable (if (S.null tnames') then M.delete size available
                                                          else M.insert size tnames' available)
                        return (genAllocAt tname (App con args'))                        
                _ -> return (App con args')
      TypeLam tpars body
        -> TypeLam tpars <$> ruExpr body
      TypeApp body targs
        -> (`TypeApp` targs) <$> ruExpr body
      Lam pars eff body
        ->Lam pars eff <$> ruExpr body
      App fn args
        -> do args' <- mapM ruExpr args
              fn'   <- ruExpr fn
              return $ App fn' args'
      Var _ _ 
        -> return expr
      Lit _
        -> return expr
      Con ctor repr
        -> return expr
        
      Let [] body
        -> ruExpr body
      Let (DefNonRec def:dgs) body
        -> do body' <- ruExpr $ Let dgs body
              def'  <- ruDef False def
              return $ makeLet [DefNonRec def'] body'
      Let _ _
        -> failure "Backend.C.Reuse.ruExpr"
      Case scrutinees branches
        -> do scruts'   <- mapM ruExpr scrutinees
              branches' <- ruBranches branches
              return (Case scruts' branches')

ruBranches :: [Branch] -> Reuse [Branch]
ruBranches branches
  = do (branches', avs) <- unzip <$> mapM ruBranch branches
       setAvailable  (availableIntersect avs)
       return branches'

ruBranch :: Branch -> Reuse (Branch, Available)
ruBranch (Branch pats guards)
  = do (pats',reuses) <- unzip <$> mapM (ruPattern Nothing) pats
       (guards',avs)  <- unzip <$> mapM ruGuard guards
       -- TODO: 
       -- generate unique names for each binding in reuses list
       -- add those to available, 
       -- visit guards, 
       -- generate drop_reuse for each binding that is no longer available (i.e. reused)
       -- remove any other binding in reuses from available
       return (Branch pats' guards', availableIntersect avs)

ruGuard :: Guard -> Reuse (Guard, Available)
ruGuard (Guard test expr)
  = isolateAvailable $
    do test' <- ruExpr test
       expr' <- ruExpr expr
       return (Guard test' expr')


ruPattern :: Maybe TName -> Pattern -> Reuse (Pattern, [(TName,Int)])
ruPattern mbVar pat
  = case pat of
      PatCon name params repr targs exists tres conInfo
        -> do (params',reusess) <- unzip <$> mapM (ruPattern Nothing) params
              newtypes <- getNewtypes
              let size = constructorSize newtypes repr targs
              (reuses,f) <- if (size <= 0) then return ([],id)
                              else do (u,f) <- case mbVar of
                                                 Just tname -> return (tname,id)
                                                 _          -> do u <- uniqueTName "ru" tres
                                                                  return (u,PatVar u)
                                      return ([(u,size)], f)
              return (f (PatCon name params' repr targs exists tres conInfo), concat(reuses:reusess))
                
      PatVar tname arg
        -> do (arg',reuse) <- ruPattern (Just tname) arg
              return (PatVar tname arg', reuse)
      PatWild  -> return (pat,[])
      PatLit _ -> return (pat,[])
      
uniqueTName base tp
  = do nm <- uniqueName base
       return (TName nm tp)
      

-- Generate a reuse of a constructor
genDropReuse :: TName -> Expr
genDropReuse tname
  = App (Var (TName nameReuse funTp) (InfoExternal [(C, "drop_reuse_datatype(#1,current_context())")]))
        [Var tname InfoNone]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeReuse

-- generate allocation-at of a constructor application
-- at should have tyep `typeReuse`
-- conApp should have form  App (Con _ _) conArgs    : length conArgs >= 1
genAllocAt :: TName -> Expr -> Expr
genAllocAt at conApp 
  = App (Var (TName nameAllocAt typeAllocAt) (InfoArity 0 1)) [Var at InfoNone, conApp]
  where
    conTp = typeOf conApp
    typeAllocAt = TFun [(nameNil,conTp)] typeTotal conTp 


--------------------------------------------------------------------------
-- Utilities for readability
--------------------------------------------------------------------------

-- for mapping over a set and collecting the results into a list.
foldMapM :: (Monad m, Foldable t) => (a -> m b) -> t a -> m [b]
foldMapM f = foldr merge (return [])
  where merge x r = do y <- f x
                       (y:) <$!> r

maybeStats :: [Maybe Expr] -> Expr -> Expr
maybeStats xs expr
  = makeStats (catMaybes xs ++ [expr])

--------------------------------------------------------------------------
-- Reuse monad
--------------------------------------------------------------------------

-----------------
-- definitions --

type Available = M.IntMap TNames

data Env = Env { currentDef :: [Def],
                 prettyEnv :: Pretty.Env,
                 newtypes :: Newtypes
               }

data ReuseState = ReuseState { uniq :: Int, available :: Available }

type ReuseM a = ReaderT Env (State ReuseState) a

newtype Reuse a = Reuse (ReuseM a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState ReuseState)

instance HasUnique Reuse where
  updateUnique f = do { old <- getUniq; modifyUniq f; return old }
  setUnique = setUniq

withEnv :: (Env -> Env) -> Reuse a -> Reuse a
withEnv = local

getEnv :: Reuse Env
getEnv = ask

updateSt :: (ReuseState -> ReuseState) -> Reuse ()
updateSt = modify

getSt :: Reuse ReuseState
getSt = get

runReuse :: Pretty.Env -> Newtypes -> Reuse a -> Unique a
runReuse penv newtypes (Reuse action)
  = withUnique $ \u ->
      let env = Env [] penv newtypes
          st = ReuseState u M.empty
          (val, st') = runState (runReaderT action env) st
       in (val, uniq st')


-------------------
-- env accessors --

getCurrentDef :: Reuse [Def]
getCurrentDef = currentDef <$> getEnv

withCurrentDef :: Def -> Reuse a -> Reuse a
withCurrentDef def = withEnv (\e -> e { currentDef = def : currentDef e })

--

getPrettyEnv :: Reuse Pretty.Env
getPrettyEnv = prettyEnv <$> getEnv

withPrettyEnv :: (Pretty.Env -> Pretty.Env) -> Reuse a -> Reuse a
withPrettyEnv f = withEnv (\e -> e { prettyEnv = f (prettyEnv e) })

--

getNewtypes :: Reuse Newtypes
getNewtypes = newtypes <$> getEnv

withNewtypes :: (Newtypes -> Newtypes) -> Reuse a -> Reuse a
withNewtypes f = withEnv (\e -> e { newtypes = f (newtypes e) })

--

getAvailable :: Reuse Available
getAvailable = available <$> getSt

updateAvailable :: (Available -> Available) -> Reuse ()
updateAvailable f = updateSt (\s -> s { available = f (available s) })

setAvailable :: Available -> Reuse ()
setAvailable = updateAvailable . const

availableIntersect :: [Available] -> Available
availableIntersect avs
  = M.filter (not . S.null) (M.unionsWith S.intersection avs)
      
---------------------
-- state accessors --

getUniq :: Reuse Int
getUniq = uniq <$> getSt

modifyUniq :: (Int -> Int) -> Reuse ()
modifyUniq f = updateSt (\s -> s { uniq = f (uniq s) })

setUniq :: Int -> Reuse ()
setUniq = modifyUniq . const


isolateAvailable :: Reuse a -> Reuse (a,Available) 
isolateAvailable action
  = do avs0 <- getAvailable
       x <- action
       avs1 <- getAvailable
       setAvailable avs0
       return (x,avs1)



--------------------------------------------------------------------------
-- Tracing
--------------------------------------------------------------------------

ruTraceDoc :: (Pretty.Env -> Doc) -> Reuse ()
ruTraceDoc f
 = do pretty <- getPrettyEnv
      ruTrace (show (f pretty))

ruTrace :: String -> Reuse ()
ruTrace msg
 = do defs <- getCurrentDef
      trace ("Core.Reuse: " ++ show (map defName defs) ++ ": " ++ msg) $ return ()

----------------
      
-- return the allocated size of a constructor. Return 0 for value types or singletons 
constructorSize :: Newtypes -> ConRepr -> [Type] -> Int
constructorSize newtypes conRepr paramTypes
  = if (dataReprIsValue (conDataRepr conRepr) || null paramTypes)
     then 0
     else sum (map (fieldSize newtypes) paramTypes)

-- return the field size of a type 
fieldSize :: Newtypes -> Type -> Int
fieldSize newtypes tp
  = case extractDataDefType tp of
      Nothing   -> 1  -- regular datatype is 1 pointer
      Just name -> case newtypesLookupAny name newtypes of
                     Nothing -> failure $ "Backend.C.Reuse.typeSize: cannot find type: " ++ show name
                     Just di -> case dataInfoDef di of 
                                  DataDefValue raw scan -> raw + scan  -- todo: take raw fields real size into account
                                  _ -> 1 -- pointer to allocated data

extractDataDefType :: Type -> Maybe Name
extractDataDefType tp
  = case expandSyn tp of
      TApp t _      -> extractDataDefType t
      TForall _ _ t -> extractDataDefType t
      TCon tc       -> Just (typeConName tc)
      _             -> Nothing
      

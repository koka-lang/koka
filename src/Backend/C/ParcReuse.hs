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
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.IntMap as M

import Kind.Newtypes
import Type.Type
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty


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
  | otherwise   = do let defs = tr (coreProgDefs core) $ coreProgDefs core
                     defs' <- runReuse penv newtypes (ruDefGroups True defs)
                     tr defs' $ return core{ coreProgDefs  = defs' }
  where penv' = penv{Pretty.coreShowDef=True,Pretty.coreShowTypes=False,Pretty.fullNames=False}
        tr d = trace (show (vcat (map (prettyDefGroup penv') d)))

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
  = withCurrentDef def $
    do expr <- ruExpr (defExpr def)
       return def{defExpr=expr}

--------------------------------------------------------------------------
-- Main PARC algorithm
--------------------------------------------------------------------------

ruExpr :: Expr -> Reuse Expr
ruExpr expr
  = case expr of
      App con@(Con _ repr) args
        -> do args' <- mapM ruExpr args
              ruTryReuseCon repr (map typeOf args) (App con args')
      App ta@(TypeApp (Con _ repr) _) args
        -> do args' <- mapM ruExpr args
              ruTryReuseCon repr (map typeOf args) (App ta args')

      TypeLam tpars body
        -> TypeLam tpars <$> ruExpr body
      TypeApp body targs
        -> (`TypeApp` targs) <$> ruExpr body
      Lam pars eff body
        -> Lam pars eff <$> ruExpr body
      App fn args
        -> liftM2 App (ruExpr fn) (mapM ruExpr args)

      Let [] body
        -> ruExpr body
      Let (DefNonRec def:dgs) body
        -> liftM2 makeLet' (ruDef False def) (ruExpr (Let dgs body))
           where makeLet' def' = makeLet [DefNonRec def']
      Let _ _
        -> failure "Backend.C.Reuse.ruExpr"

      Case scrutinees branches
        -> liftM2 Case (mapM ruExpr scrutinees) (ruBranches branches)

      -- Var, Lit, Con
      _ -> return expr

ruTryReuseCon :: ConRepr -> [Type] -> Expr -> Reuse Expr
ruTryReuseCon repr paramTypes conApp
  = do newtypes <- getNewtypes
       let size = constructorSize newtypes repr paramTypes
       available <- getAvailable
       case M.lookup size available of
         Just tnames | not (S.null tnames)
           -> do let (tname, tnames') = S.deleteFindMin tnames
                 setAvailable (M.insert size tnames' available)
                 return (genAllocAt tname conApp)
         _ -> return conApp

ruBranches :: [Branch] -> Reuse [Branch]
ruBranches branches
  = do (branches', avs) <- unzip <$> mapM ruBranch branches
       setAvailable (availableIntersect avs)
       return branches'

ruBranch :: Branch -> Reuse (Branch, Available)
ruBranch (Branch pats guards)
  = do pats' <- mapM patAddNames pats
       reuses <- concat <$> mapM ruPattern pats'
       added <- mapM addAvailable reuses
       (guards', avs)  <- unzip <$> mapM (ruGuard added) guards
       return (Branch pats' guards', availableIntersect avs)
  where
    addAvailable :: (TName, Int) -> Reuse (TName, TName, Int)
    addAvailable (patName, size)
      = do reuseName <- uniqueTName typeReuse
           updateAvailable (M.insertWith S.union size (S.singleton reuseName))
           return (reuseName, patName, size)

patAddNames :: Pattern -> Reuse Pattern
patAddNames pat
  = case pat of
      PatVar{patPattern=c@PatCon{patConPatterns}}
        -> do pats' <- mapM patAddNames patConPatterns
              return pat{patPattern=c{patConPatterns=pats'}}
      PatCon{patConPatterns,patTypeRes}
        -> do name <- uniqueTName patTypeRes
              pats' <- mapM patAddNames patConPatterns
              return $ PatVar name pat{patConPatterns=pats'}
      _ -> return pat

ruPattern :: Pattern -> Reuse [(TName, Int)]
ruPattern (PatVar tname PatCon{patConPatterns,patConRepr,patTypeArgs})
  = do reuses <- concat <$> mapM ruPattern patConPatterns
       newtypes <- getNewtypes
       let size = constructorSize newtypes patConRepr patTypeArgs
       if size > 0
         then return ((tname, size):reuses)
         else return reuses
ruPattern _ = return []

ruGuard :: [(TName, TName, Int)] -> Guard -> Reuse (Guard, Available)
ruGuard patAdded (Guard test expr)
  = isolateAvailable $
    do test' <- withNoneAvailable $ ruExpr test
       expr' <- ruExpr expr
       mbDefs <- mapM ruTryReuse patAdded
       let dgs = map DefNonRec (catMaybes mbDefs)
       return (Guard test' (makeLet dgs expr'))

-- generate drop_reuse for each reused in patAdded
ruTryReuse :: (TName, TName, Int) -> Reuse (Maybe Def)
ruTryReuse (reuseName, patName, size)
  = do av <- getAvailable
       case M.lookup size av of
         Just tnames | S.member reuseName tnames
           -> do let tnames' = S.delete reuseName tnames
                 setAvailable (M.insert size tnames' av)
                 return Nothing
         _ -> return (Just (makeTDef reuseName (genDropReuse patName)))

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

-- create a unique name specific to this module
uniqueTName :: Type -> Reuse TName
uniqueTName tp = (`TName` tp) <$> uniqueName "ru"

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


---------------------
-- state accessors --

getUniq :: Reuse Int
getUniq = uniq <$> getSt

modifyUniq :: (Int -> Int) -> Reuse ()
modifyUniq f = updateSt (\s -> s { uniq = f (uniq s) })

setUniq :: Int -> Reuse ()
setUniq = modifyUniq . const

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

--

withNoneAvailable :: Reuse a -> Reuse a
withNoneAvailable action
  = do av0 <- getAvailable
       setAvailable M.empty
       x <- action
       setAvailable av0
       return x

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
      trace ("Core.Reuse: " ++ show (map defName defs) ++ ": " ++ msg) $
        return ()

----------------

-- return the allocated size of a constructor. Return 0 for value types or singletons
constructorSize :: Newtypes -> ConRepr -> [Type] -> Int
constructorSize newtypes conRepr paramTypes
  = if dataReprIsValue (conDataRepr conRepr)
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

-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen, Ningning Xie
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Lift all local and anonymous functions to top level. No more letrec :-)
-----------------------------------------------------------------------------

module Core.FunLift( liftFunctions
                   ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.Error
import Common.Syntax

import Kind.Kind
import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Core.Core
import qualified Core.Core as Core
import Core.Pretty
import Core.CoreVar

trace s x =
  Lib.Trace.trace s
    x

enableLifting = True

liftFunctions :: Pretty.Env -> Int -> DefGroups -> (DefGroups,Int)
liftFunctions penv u defs
  = if enableLifting then runLift penv u (liftDefGroups True defs)
    else (defs, u)


{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}

liftDefGroups :: Bool   -- top-level functions are allowed
              -> DefGroups -> Lift DefGroups
liftDefGroups topLevel defGroups
  = do -- traceDoc (\penv -> text "lifting")
       fmap concat $ mapM (liftDefGroup topLevel) defGroups

liftDefGroup :: Bool -> DefGroup -> Lift DefGroups
liftDefGroup True (DefNonRec def)
  = do (def', groups) <- collectLifted $ liftDef True def
       return $  groups ++ [DefNonRec def'] -- all lifted definitions are put before the current definition

liftDefGroup True (DefRec defs)
  = do (defs', groups) <- collectLifted $ mapM (liftDef True) defs
       let groups' = flattenDefGroups groups
       return [DefRec (groups' ++ defs')] -- defs' depend on groups', groups' might depend on defs'

liftDefGroup False (DefNonRec def)
  = do def' <- liftDef False def
       return [DefNonRec def']

liftDefGroup False (DefRec defs)
  = do (callExprs, liftedDefs0) <- fmap unzip $ mapM (makeDef fvs tvs) exprs
       let subst       = zip names callExprs
           liftedDefs  = map (substWithLiftedExpr subst) liftedDefs0
       groups <- liftDefGroup True (DefRec liftedDefs) -- lift all recs to top-level
       emitLifteds groups

       let defs' = zipWith (\def callExpr -> def{ defExpr = callExpr
                                                , defSort = liftSort False (defSort def)})
                          defs callExprs
       return (map DefNonRec defs') -- change a DefRec to all DefNonRecs
  where exprs = map defExpr defs
        names = map defTName defs
        fvs = tnamesList $ tnamesRemove names (tnamesUnions $ map freeLocals exprs)
        tvs = tvsList $ tvsUnions $ map ftv exprs

        substWithLiftedExpr subst def
          = let body = case defExpr def of
                        (TypeLam tpars (Lam pars eff lbody)) -> TypeLam tpars (Lam pars eff (subst |~> lbody))
                        (Lam pars eff lbody)                 -> Lam pars eff (subst |~> lbody)
                        expr -> failure $ ("Core.FunLift.liftDefGroup False DefRec: lifting non-function? " ++ show expr)
            in def{defExpr = body}

liftDef :: Bool -> Def -> Lift Def
liftDef topLevel def
  = withCurrentDef def $
    do expr' <- liftExpr topLevel (defExpr def)
       return def{ defExpr = expr', defSort = liftSort topLevel (defSort def)}

liftSort :: Bool -> DefSort -> DefSort
liftSort False DefFun = DefVal
liftSort _ sort = sort

liftExpr :: Bool
         -> Expr
         -> Lift Expr
liftExpr topLevel expr
  = case expr of
    App f args
      -> do f' <- liftExpr False f
            args' <- mapM (liftExpr False) args
            return (App f' args')

    Lam args eff body
      -> do body' <- liftExpr topLevel body
            let expr' = Lam args eff body'
            -- top level functions are allowed
            if topLevel then return expr'
            -- lift local functions
            else liftLocalFun expr' eff
    Let defgs body
      -> do -- liftTrace ("let hi "  ++ show expr)
            defgs' <- liftDefGroups False defgs
            body'  <- liftExpr False body
            return (Let defgs' body')

    Case exprs bs
      -> do exprs' <- mapM (liftExpr False) exprs
            bs'    <- mapM liftBranch bs
            return (Case exprs' bs')

    TypeLam tvars (Lam pars eff lbody)  | not topLevel
      -> do expr1 <- liftExpr False lbody
            liftLocalFun (TypeLam tvars (Lam pars eff expr1)) eff
    TypeLam tvars body
      -> do body' <- liftExpr topLevel body
            return (TypeLam tvars body')

    TypeApp body tps
      -> do body' <- liftExpr topLevel body
            return (TypeApp body' tps)

    _ -> return expr

liftLocalFun :: Expr -> Effect -> Lift Expr
liftLocalFun expr eff
  = do let fvs = tnamesList $ freeLocals expr
           tvs = tvsList (ftv expr)
       (expr2, liftDef) <- makeDef fvs tvs expr
       emitLifted (DefNonRec liftDef)
       return expr2


makeDef :: [TName] -> [TypeVar] -> Expr -> Lift (Expr, Def)
makeDef fvs tvs expr
  = do -- liftTrace (show expr)
       name <- uniqueNameCurrentDef
       return (etaExpr name, liftedDef name)
  where
    (tpars,pars,eff,body) -- :: ([TypeVar],[TName],Type)
      = case expr of
          (TypeLam tpars (Lam pars eff lbody)) -> (tpars, map unwild pars, eff, lbody)
          (Lam pars eff lbody)                 -> ([], map unwild pars, eff, lbody)
          _ -> failure $ ("Core.FunLift.makeDef: lifting non-function? " ++ show expr)

    unwild (TName name tp)
      = TName (if (head (nameId name) == '_') then prepend "wild" name else name) tp

    alltpars = tvs ++ tpars
    allpars  = fvs ++ pars
    allargs  = [Var tname InfoNone | tname <- allpars]

    liftedFun = addTypeLambdas alltpars $ Lam allpars eff body
    liftedTp  = typeOf liftedFun
    liftedDef name = Def name liftedTp liftedFun Private DefFun rangeNull "// lift"

    funExpr name
      = Var (TName name liftedTp) (InfoArity (length alltpars) (length allargs))

    etaExpr name
      = case (tvs,fvs) of
         ([],[]) -> funExpr name
         _ -> addTypeLambdas tpars $ Lam pars eff $
               App (addTypeApps (alltpars) (funExpr name)) (allargs)

liftBranch :: Branch -> Lift Branch
liftBranch (Branch pat guards)
  = do guards' <- mapM liftGuard guards
       return (Branch pat guards')

liftGuard :: Guard -> Lift Guard
liftGuard (Guard guard body)
  = do guard' <- liftExpr False guard
       body'  <- liftExpr False body
       return (Guard guard' body')

uniqueNameCurrentDef :: Lift Name
uniqueNameCurrentDef =
  do env <- getEnv
     let defNames = map defName (currentDef env)
     i <- unique
     let -- base     = concatMap (\name -> nameId name ++ "-") (tail $ reverse defNames) ++ "x" ++ show i
         udefName =  toHiddenUniqueName i "lift" (last defNames)
     return udefName


{--------------------------------------------------------------------------
  Lift monad
--------------------------------------------------------------------------}
newtype Lift a = Lift (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env }

data State = State{ uniq :: Int }

data Result a = Ok a State [DefGroup]

runLift :: Pretty.Env -> Int -> Lift a -> (a,Int)
runLift penv u (Lift c)
  = case c (Env [] penv) (State u) of
      Ok x st [] -> (x,uniq st)
      Ok x st _  -> failure $ "Core.FunLift.runLift: unprocessed defgroups"

instance Functor Lift where
  fmap f (Lift c)  = Lift (\env st -> case c env st of
                                        Ok x st' dgs -> Ok (f x) st' dgs)

instance Applicative Lift where
  pure  = return
  (<*>) = ap

instance Monad Lift where
  return x       = Lift (\env st -> Ok x st [])
  (Lift c) >>= f = Lift (\env st -> case c env st of
                                      Ok x st' dgs -> case f x of
                                                        Lift d -> case d env st' of
                                                                    Ok x' st'' dgs' -> Ok x' st'' (dgs ++ dgs'))

instance HasUnique Lift where
  updateUnique f = Lift (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) } [])
  setUnique  i   = Lift (\env st -> Ok () st{ uniq = i} [])

withEnv :: (Env -> Env) -> Lift a -> Lift a
withEnv f (Lift c)
  = Lift (\env st -> c (f env) st)

getEnv :: Lift Env
getEnv
  = Lift (\env st -> Ok env st [])

updateSt :: (State -> State) -> Lift State
updateSt f
  = Lift (\env st -> Ok st (f st) [])

collectLifted :: Lift a -> Lift (a, DefGroups)
collectLifted (Lift d)
  = Lift (\env st -> case d env st of
                       Ok x st' dgs -> Ok (x,dgs) st' [])

emitLifted :: DefGroup -> Lift ()
emitLifted dg
  = Lift (\env st -> Ok () st [dg])

emitLifteds :: DefGroups -> Lift ()
emitLifteds dg
  = Lift (\env st -> Ok () st dg)

withCurrentDef :: Def -> Lift a -> Lift a
withCurrentDef def action
  = -- trace ("lifting: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $
    action

traceDoc :: (Pretty.Env -> Doc) -> Lift ()
traceDoc f
  = do env <- getEnv
       liftTrace (show (f (prettyEnv env)))

liftTrace :: String -> Lift ()
liftTrace msg
  = do env <- getEnv
       trace ("lift: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

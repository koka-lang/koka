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

test = False


liftFunctions :: Pretty.Env -> Int -> DefGroups -> (DefGroups,Int)
liftFunctions penv u defs
  = if test then runLift penv u (liftDefGroups defs)
    else (defs, u)


{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}

-- TopLevel = True
liftDefGroups :: DefGroups -> Lift DefGroups
liftDefGroups defGroups
  = do traceDoc (\penv -> text "lifting")
       fmap concat $ mapM liftDefGroup defGroups

liftDefGroup :: DefGroup -> Lift DefGroups
liftDefGroup (DefNonRec def)
  = do (def', groups) <- collectLifted $ liftDef def
       return $  groups ++ [DefNonRec def']


liftDefGroup (DefRec defs)
  = do (defs', groups) <- fmap unzip $ mapM liftDef defs
       let groups' = flattenAllDefGroups groups
       return [DefRec (defs' ++ groups')]

liftDef :: Def -> Lift Def
liftDef  def
  = withCurrentDef def $
    do expr' <- liftExpr True (defExpr def)
       return def{ defExpr = expr'}

-- TopLevel = False
liftDefGroupsX :: DefGroups -> Lift DefGroups
liftDefGroupsX defGroups
  = do defs <- mapM liftDefGroupX defGroups
       return (concat defs)

defSubst :: Def -> (TName, Expr)
defSubst def = (defTName def, defExpr def)


exprSort :: Expr -> DefSort
exprSort expr = if isValueExpr expr then DefVal else DefFun


liftDefGroupX :: DefGroup -> Lift [DefGroup]  -- list because we may change a DefRec into all non-recursive bindings
liftDefGroupX (DefNonRec def)
  = do def' <- liftDefX def
       return [DefNonRec def']

liftDefGroupX (DefRec defs)
  -- = return (DefRec defs, [])
  = do let exprs = map defExpr defs
           names = map defTName defs
           fvs = tnamesList $ tnamesRemove names (tnamesUnions $ map freeLocals exprs)
           tvs = tvsList $ tvsUnions $ map ftv exprs
       (expr2, liftDefs) <- fmap unzip $ mapM (liftExprDef fvs tvs) exprs
       traceDoc (\env -> text "liftDefGroupX: " <+> vcat (map (prettyExpr env) expr2))
       let subst = zip names expr2
           liftDefs2 = map (subst |~> ) liftDefs
       groups <- liftDefGroup (DefRec liftDefs2)

       let defs' = zipWith (\def expr -> def{defExpr = expr, defSort = exprSort expr})
                           defs expr2

       return (map DefNonRec defs',  groups)



liftDefX :: Def -> Lift Def
liftDefX def
  = withCurrentDef def $
    do expr' <- liftExpr False (defExpr def)
       let sort' = case defSort def of
                    DefFun  -> DefVal
                    sort    -> sort
       return ( def{ defExpr = expr', defSort = sort' }) -- always value? what are DefVar?

liftExpr :: Bool    -- top-level functions are allowed
         -> Expr
         -> Lift Expr
liftExpr topLevel expr
  = case expr of
    App f args
      -> do f' <- liftExpr False f
            args' <- mapM (liftExpr False) args
            return (App f' args')

    Lam args eff body
      -- top level functions are allowed
      | topLevel -> liftLambda
      -- lift local functions
      | otherwise ->
          do (expr1, groups) <- liftLambda
             let fvs = tnamesList $ freeLocals expr1
                 tvs = tvsList (ftv expr1)
             (expr2, liftDef) <- liftExprDef fvs tvs expr1
             emitLifted [DefNonRec liftDef]
             return (expr2)
      where liftLambda
              = do (body', groups) <- liftExpr topLevel body
                   return (Lam args eff body', groups)
    Let defgs body
      -> do liftTrace ("let hi "  ++ show expr)
            (defgs', groups1) <- liftDefGroupsX defgs
            (body', groups2) <- liftExpr False body
            return (Let defgs' body', groups1 ++ groups2)

    Case exprs bs
      -> do (exprs', groups1) <- fmap unzip $ mapM (liftExpr False) exprs
            (bs', groups2) <- fmap unzip $ mapM liftBranch bs
            return (Case exprs' bs', (concat groups1) ++ (concat groups2))

    TypeLam tvars body
      | not topLevel
      , Lam _ eff _ <- body
      -> do (expr1, groups) <- liftTypeLambda
            let fvs = tnamesList $ freeLocals expr1
                tvs = tvsList (ftv expr1)
            (expr2, liftDef) <- liftExprDef fvs tvs expr1
            return (expr2, groups ++ [DefNonRec liftDef])
      | otherwise -> liftTypeLambda
      where liftTypeLambda
              = do (body', groups) <- liftExpr topLevel body
                   return (TypeLam tvars body', groups)
    TypeApp body tps
      -> do (body', groups) <- liftExpr topLevel body
            return (TypeApp body' tps, groups)

    _ -> return (expr, [])

liftExprDef :: [TName] -> [TypeVar] -> Expr -> Lift (Expr, Def)
liftExprDef fvs tvs expr
  = do liftTrace (show expr)
       let liftExp1 = addLambdasTName fvs (getEffExpr expr) expr
           liftExp2 = addTypeLambdas tvs liftExp1
       name <- uniqueNameCurrentDef
       let ty = typeOf liftExp2
           liftDef = Def name ty liftExp2 Private DefFun rangeNull "// lift"
       let expr1 = Var (TName name ty) (InfoArity (getTypeArityExpr liftExp2)
                                                  (getParamArityExpr liftExp2))
           expr2 = addTypeApps tvs expr1
           expr3 = addApps (map (\name -> Var name InfoNone) fvs) expr2

       return (expr3, liftDef)

liftBranch :: Branch -> Lift (Branch, DefGroups)
liftBranch (Branch pat guards)
  = do (guards', groups) <- fmap unzip $ mapM liftGuard guards
       return $ (Branch pat guards', concat groups)

liftGuard :: Guard -> Lift (Guard, DefGroups)
liftGuard (Guard guard body)
  = do (guard', groups1) <- liftExpr False guard
       (body', groups2)  <- liftExpr False body
       return (Guard guard' body', groups1 ++ groups2 )

uniqueNameCurrentDef :: Lift Name
uniqueNameCurrentDef =
  do env <- getEnv
     let defNames = map defName (currentDef env)
     uniName <- uniqueName "lift"
     return $ foldr1 (\localName name -> postpend ("-" ++ show localName) name) (uniName:defNames)

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

collectLifted :: Lift a -> Lift (a, [DefGroups])
collectLifted (Lift d)
  = Lift (\env st -> case d env st of
                       Ok x st' dgs -> Ok (x,dgs) st' [])

emitLifted :: DefGroup -> Lift ()
emitLifted dg
  = Lift (\env st -> Ok () st [dg])


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

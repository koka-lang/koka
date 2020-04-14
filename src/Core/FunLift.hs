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
  = do (def', groups) <- liftDef True tnamesEmpty def
       return $  groups ++ [DefNonRec def']

liftDefGroup (DefRec defs)
  = do (defs', groups) <- fmap unzip $ mapM (liftDef True tnamesEmpty) defs
       let groups' = flattenAllDefGroups groups
       return [DefRec (defs' ++ groups')]

-- TopLevel = False
liftDefGroupsX :: TNames -> DefGroups -> Lift (DefGroups, DefGroups)
liftDefGroupsX tnames defGroups
  = do (defs, groups) <- fmap unzip $ mapM (liftDefGroupX tnames) defGroups
       return (defs, concat groups)

liftDefGroupX :: TNames -> DefGroup -> Lift (DefGroup, DefGroups)
liftDefGroupX tnames group
  = do return (group, []) -- TODO

liftDef :: Bool -> TNames -> Def -> Lift (Def, DefGroups)
liftDef topLevel tnames def
  = withCurrentDef def $
    do (expr', defs) <- liftExpr topLevel tnames (defExpr def)
       return ( def{ defExpr = expr'}, defs)

liftExpr :: Bool    -- top-level functions are allowed
         -> TNames  -- only local variables should be abstracted. E.g., {id y}.
                    -- Here y should be abstracted while id shouldn't.
         -> Expr
         -> Lift (Expr, DefGroups)
liftExpr topLevel tnames expr
  = case expr of
    App f args
      -> do (f', groups1) <- liftExpr False tnames f
            (args', groups2) <- fmap unzip $ mapM (liftExpr False tnames) args
            return (App f' args', groups1 ++ concat groups2)

    Lam args eff body
      -- top level functions are allowed
      | topLevel -> liftLambda
      -- lift local functions
      | otherwise ->
          do (expr1, groups) <- liftLambda
                 -- abstract over free variables
                 -- rename?
             let fvs = tnamesList (tnamesUnion (fv expr1) tnames)
                 expr2 = addLambdasTName fvs eff expr1 -- poison?

                 -- abstract over type variables
                 tvs = tvsList (ftv expr2)
                 expr3 = addTypeLambdas tvs expr2

             name <- uniqueNameCurrentDef
             let ty = typeInt -- TODO: get the type of expr
                 def = DefNonRec $ Def name ty expr3 Private DefFun rangeNull ""

             let liftExp1 = Var (TName name ty)
                                (InfoArity (length tvs) (length fvs + length args))
                 liftExp2 = addTypeApps tvs liftExp1
                 liftExp3 = addApps (map (\name -> Var name InfoNone) fvs) liftExp2 -- InfoNone?

             return (liftExp3, groups ++ [def])
      where liftLambda
              = do (body', groups) <- liftExpr topLevel (tnamesInsertAll tnames args) body
                   return (Lam args eff body', groups)
    Let defgs body
      -> do (defgs', groups1) <- liftDefGroupsX tnames defgs
            (body', groups2) <- liftExpr False (tnamesUnion tnames (defGroupsTNames defgs')) body
            return (Let defgs' body', groups1 ++ groups2)

    Case exprs bs
      -> do (exprs', groups1) <- fmap unzip $ mapM (liftExpr False tnames) exprs
            (bs', groups2) <- fmap unzip $ mapM (liftBranch tnames) bs
            return (Case exprs' bs', (concat groups1) ++ (concat groups2))

    TypeLam tvars body
      | not topLevel
      , Lam _ eff _ <- body
      ->
          do (expr1, groups) <- liftTypeLambda
             let fvs = tnamesList (tnamesUnion (fv expr1) tnames)
                 expr2 = addLambdasTName fvs eff expr1

                 tvs = tvsList (ftv expr2)
                 expr3 = addTypeLambdas tvs expr2
             name <- uniqueNameCurrentDef
             let ty = typeInt -- TODO: get the type of expr
                 def = DefNonRec $ Def name ty expr3 Private DefFun rangeNull ""

             let tvarLen = if null fvs then length tvars + length tvs else length tvs
                 liftExp1 = Var (TName name ty)
                                (InfoArity tvarLen (length fvs))
                 liftExp2 = addTypeApps tvs liftExp1
                 liftExp3 = addApps (map (\name -> Var name InfoNone) fvs) liftExp2

             return (liftExp3, groups ++ [def])
      | otherwise -> liftTypeLambda
      where liftTypeLambda
              = do (body', groups) <- liftExpr topLevel tnames body
                   return (TypeLam tvars body', groups)
    TypeApp body tps
      -> do (body', groups) <- liftExpr topLevel tnames body
            return (TypeApp body' tps, groups)

    _ -> return (expr, [])

liftBranch :: TNames -> Branch -> Lift (Branch, DefGroups)
liftBranch tnames (Branch pat guards)
  = do let tnames' = tnamesUnion tnames (bv pat)
       (guards', groups) <- fmap unzip $ mapM (liftGuard tnames') guards
       return $ (Branch pat guards', concat groups)

liftGuard :: TNames -> Guard -> Lift (Guard, DefGroups)
liftGuard tnames (Guard guard body)
  = do (guard', groups1) <- liftExpr False tnames guard
       (body', groups2)  <- liftExpr False tnames body
       return (Guard guard' body', groups1 ++ groups2)

currentDefName :: Lift String
currentDefName =
  do env <- getEnv
     return $ concatMap (show . defName) (currentDef env)


uniqueNameCurrentDef :: Lift Name
uniqueNameCurrentDef =
  do defName <- currentDefName
     uniqueName defName


{--------------------------------------------------------------------------
  Lift monad
--------------------------------------------------------------------------}
newtype Lift a = Lift (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env }

data State = State{ uniq :: Int }

data Result a = Ok a State

runLift :: Pretty.Env -> Int -> Lift a -> (a,Int)
runLift penv u (Lift c)
  = case c (Env [] penv) (State u) of
      Ok x st -> (x,uniq st)

instance Functor Lift where
  fmap f (Lift c)  = Lift (\env st -> case c env st of
                                        Ok x st' -> Ok (f x) st')

instance Applicative Lift where
  pure  = return
  (<*>) = ap

instance Monad Lift where
  return x       = Lift (\env st -> Ok x st)
  (Lift c) >>= f = Lift (\env st -> case c env st of
                                      Ok x st' -> case f x of
                                                    Lift d -> d env st' )

instance HasUnique Lift where
  updateUnique f = Lift (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = Lift (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> Lift a -> Lift a
withEnv f (Lift c)
  = Lift (\env st -> c (f env) st)

getEnv :: Lift Env
getEnv
  = Lift (\env st -> Ok env st)

updateSt :: (State -> State) -> Lift State
updateSt f
  = Lift (\env st -> Ok st (f st))

withCurrentDef :: Def -> Lift a -> Lift a
withCurrentDef def action
  = -- trace ("mon def: " ++ show (defName def)) $
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

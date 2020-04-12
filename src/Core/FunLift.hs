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

liftDefGroups :: DefGroups -> Lift DefGroups
liftDefGroups defGroups
  = do traceDoc (\penv -> text "lifting")
       mapM liftDefGroup defGroups

liftDefGroup :: DefGroup -> Lift DefGroup
liftDefGroup (DefRec defs)
  = do defs' <- fmap (concat) $ mapM liftDef defs
       return (DefRec defs')
liftDefGroup (DefNonRec def)
  = do def' <- liftDef def
       return (DefRec def')
       -- not right yet
       -- after lifting it might be a recursive group
       -- more to do here

liftDef :: Def -> Lift Defs
liftDef def
  = withCurrentDef def $
    do (expr', defs) <- liftExpr True tnamesEmpty (defExpr def)
       return $ def{ defExpr = expr'} : defs

liftExpr :: Bool -> TNames -> Expr -> Lift (Expr, Defs)
liftExpr topLevel tnames expr
  = case expr of
    App f args
      -> do (f', defs1) <- liftExpr False tnames f
            (args', defs2) <- fmap (fmap concat . unzip) $ mapM (liftExpr False tnames) args
            return (App f' args', defs1 ++ defs2)

    Lam args eff body
      -- top level functions are allowed
      | topLevel -> liftLambda
      -- lift local functions
      | otherwise ->
          do (expr1, defs) <- liftLambda
                 -- abstract over free variables
                 -- rename?
             let fvs = tnamesList (tnamesDiff (fv expr1) tnames)
                 -- TODO: recursion?
                 expr2 = addLambdasTName fvs eff expr1 -- poison?

                 -- abstract over type variables
                 tvs = tvsList (ftv expr2)
                 expr3 = addTypeLambdas tvs expr2

             name <- uniqueNameCurrentDef
             let ty = typeInt -- TODO: get the type of expr
                 def = Def name ty expr3 Private DefFun rangeNull ""

                 liftExp1 = Var (TName name ty)
                                (InfoArity (length tvs) (length fvs + length args))
                 liftExp2 = addTypeApps tvs liftExp1
                 liftExp3 = addApps (map (\name -> Var name InfoNone) fvs) liftExp2 -- InfoNone?

             return (liftExp3, def:defs)
      where liftLambda
              = do (body', defs) <- liftExpr topLevel (tnamesInsertAll tnames args) body
                   return (Lam args eff body', defs)

    _ -> return (expr, [])

currentDefName :: Lift String
currentDefName = return "name" -- TODO

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

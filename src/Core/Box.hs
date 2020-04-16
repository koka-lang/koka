-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Insert box/unbox functions for certain backends (e.g. for C)
-----------------------------------------------------------------------------

module Core.Box( boxTransformDefs ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Failure
import Common.NamePrim ( nameEffectOpen )
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

trace s x =
  -- Lib.Trace.trace s
    x



boxTransformDefs :: Pretty.Env -> Int -> DefGroups -> (DefGroups,Int)
boxTransformDefs penv u defs
  = runBox penv u $
    do boxDefGroups defs


{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}
boxDefGroups :: DefGroups -> Box DefGroups
boxDefGroups defGroups
  = do -- traceDoc (\penv -> text "box")
       mapM boxDefGroup defGroups

boxDefGroup (DefRec defs)
  = do defs' <- mapM boxDef defs
       return (DefRec defs')
boxDefGroup (DefNonRec def)
 = do def' <- boxDef def
      return (DefNonRec def')

boxDef :: Def -> Box Def
boxDef def
  = withCurrentDef def $
    do expr' <- boxExpr (defExpr def)
       return def{ defExpr = expr' }

boxExpr :: Expr -> Box Expr
boxExpr expr
  = case expr of
    -- Applications
    App f args
      -> do args' <- mapM boxExpr args
            f' <- boxExpr f
            return (App f' args')

    -- regular cases
    Lam args eff body
      -> do -- boxTraceDoc $ \env -> text "not effectful lambda:" <+> niceType env eff
            body' <- boxExpr body
            return (Lam args eff body')

    Let defgs body
      -> do defgs' <- boxDefGroups defgs
            body'  <- boxExpr body
            return (Let defgs' body')
    Case exprs bs
      -> do exprs' <- mapM boxExpr exprs
            bs'    <- mapM boxBranch bs
            return (Case exprs' bs')

    -- type application and abstraction
    TypeLam tvars body
      -> do body' <- boxExpr body
            return $ TypeLam tvars body'

    TypeApp body tps
      -> do body' <- boxExpr body
            return $ TypeApp body' tps

    -- the rest
    Con _ _ -> return expr
    Var _ _ -> return expr
    Lit _   -> return expr


boxBranch :: Branch -> Box Branch
boxBranch (Branch pat guards)
  = do guards' <- mapM boxGuard guards
       return $ Branch pat guards'

boxGuard :: Guard -> Box Guard
boxGuard (Guard guard body)
  = do guard' <- boxExpr guard
       body'  <- boxExpr body
       return $ Guard guard' body'


{--------------------------------------------------------------------------
  Box boxad
--------------------------------------------------------------------------}
newtype Box a = Box (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env
              }

data State = State{ uniq :: Int }

data Result a = Ok a State

runBox :: Pretty.Env -> Int -> Box a -> (a,Int)
runBox penv u (Box c)
  = case c (Env [] penv) (State u) of
      Ok x st -> (x,uniq st)

instance Functor Box where
  fmap f (Box c)  = Box (\env st -> case c env st of
                                        Ok x st' -> Ok (f x) st')

instance Applicative Box where
  pure  = return
  (<*>) = ap

instance Monad Box where
  return x       = Box (\env st -> Ok x st)
  (Box c) >>= f = Box (\env st -> case c env st of
                                      Ok x st' -> case f x of
                                                    Box d -> d env st' )

instance HasUnique Box where
  updateUnique f = Box (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = Box (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> Box a -> Box a
withEnv f (Box c)
  = Box (\env st -> c (f env) st)

getEnv :: Box Env
getEnv
  = Box (\env st -> Ok env st)

updateSt :: (State -> State) -> Box State
updateSt f
  = Box (\env st -> Ok st (f st))

withCurrentDef :: Def -> Box a -> Box a
withCurrentDef def action
  = -- trace ("box def: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $
    action

traceDoc :: (Pretty.Env -> Doc) -> Box ()
traceDoc f
  = do env <- getEnv
       boxTrace (show (f (prettyEnv env)))

boxTrace :: String -> Box ()
boxTrace msg
  = do env <- getEnv
       trace ("box: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

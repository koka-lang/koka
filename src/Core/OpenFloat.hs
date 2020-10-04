-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen, Ningning Xie
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Flt all local and anonymous functions to top level. No more letrec :-)
-----------------------------------------------------------------------------

module Core.OpenFloat( openFloat ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative
import Data.Maybe( catMaybes )
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
   Lib.Trace.trace s
    x
    
enable = True  -- set to True to enable the transformation

openFloat :: Pretty.Env -> Gamma -> Int -> DefGroups -> (DefGroups,Int)
openFloat penv gamma u defs
  = runFlt penv gamma u $
    fltDefGroups defs
    

{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}
fltDefGroups :: DefGroups -> Flt DefGroups
fltDefGroups [] = return []
fltDefGroups (dg:dgs) = fltDefGroup dg (fltDefGroups dgs)

fltDefGroup (DefRec defs) next
  = do defs' <- mapM fltDef defs
       dgs <- next
       return (DefRec defs':dgs)
       
fltDefGroup (DefNonRec def) next
 = do def' <- fltDef def
      dgs <-  next
      return (DefNonRec def':dgs)

fltDef :: Def -> Flt Def
fltDef def
  = withCurrentDef def $
    do expr' <- fltExpr (defExpr def)
       return def{ defExpr = expr' }

fltExpr :: Expr -> Flt Expr
fltExpr expr
  = case expr of
    App f args
      -> do args' <- mapM fltExpr args
            f' <- fltExpr f
            return (App f' args')

    Lam args eff body
      -> do -- fltTraceDoc $ \env -> text "not effectful lambda:" <+> niceType env eff
            body' <- fltExpr body
            return (Lam args eff body')

    Let defgs body
      -> do defgs' <- fltDefGroups defgs
            body'  <- fltExpr body
            return (Let defgs' body')
    Case exprs bs
      -> do exprs' <- mapM fltExpr exprs
            bs'    <- mapM fltBranch bs
            return (Case exprs' bs')

    -- type application and abstraction
    TypeLam tvars body
      -> do body' <- fltExpr body
            return $ TypeLam tvars body'

    TypeApp body tps
      -> do body' <- fltExpr body
            return $ TypeApp body' tps

    -- the rest
    _ -> return expr


fltBranch :: Branch -> Flt Branch
fltBranch (Branch pat guards)
  = do guards' <- mapM fltGuard guards
       return $ Branch pat guards'

fltGuard :: Guard -> Flt Guard
fltGuard (Guard guard body)
  = do guard' <- fltExpr guard
       body'  <- fltExpr body
       return $ Guard guard' body'


{--------------------------------------------------------------------------
  Flt monad
--------------------------------------------------------------------------}
newtype Flt a = Flt (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env,
                gamma :: Gamma
              }

data State = State{ uniq :: Int }

data Result a = Ok a State

runFlt :: Pretty.Env -> Gamma -> Int -> Flt a -> (a,Int)
runFlt penv gamma u (Flt c)
  = case c (Env [] penv gamma) (State u) of
      Ok x st -> (x,uniq st)

instance Functor Flt where
  fmap f (Flt c)  = Flt (\env st -> case c env st of
                                        Ok x st' -> Ok (f x) st')

instance Applicative Flt where
  pure  = return
  (<*>) = ap

instance Monad Flt where
  return x       = Flt (\env st -> Ok x st)
  (Flt c) >>= f = Flt (\env st -> case c env st of
                                      Ok x st' -> case f x of
                                                    Flt d -> d env st' )

instance HasUnique Flt where
  updateUnique f = Flt (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = Flt (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> Flt a -> Flt a
withEnv f (Flt c)
  = Flt (\env st -> c (f env) st)

--withUnique :: (Int -> (a,Int)) -> Flt a
--withUnique f
-- = Flt (\env st -> let (x,u') = f (uniq st) in Ok x (st{ uniq = u'}))

getEnv :: Flt Env
getEnv
  = Flt (\env st -> Ok env st)

updateSt :: (State -> State) -> Flt State
updateSt f
  = Flt (\env st -> Ok st (f st))

withCurrentDef :: Def -> Flt a -> Flt a
withCurrentDef def action
  = -- trace ("inl def: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $
    do -- traceDoc $ (\penv -> text "\ndefinition:" <+> prettyDef penv{Pretty.coreShowDef=True} def)
       action

traceDoc :: (Pretty.Env -> Doc) -> Flt ()
traceDoc f
  = do env <- getEnv
       fltTrace (show (f (prettyEnv env)))

fltTrace :: String -> Flt ()
fltTrace msg
  = do env <- getEnv
       trace ("open float: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

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



liftFunctions :: Pretty.Env -> Int -> DefGroups -> (DefGroups,Int)
liftFunctions penv u defs
  = runLift penv u (liftDefGroups defs)


{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}
liftDefGroups :: DefGroups -> Lift DefGroups
liftDefGroups defGroups
  = do traceDoc (\penv -> text "hi")
       return defGroups


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

-----------------------------------------------------------------------------
-- Copyright 2020-2022, Microsoft Research, Daan Leijen, Anton Lorenzen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Check if a function is FBIP
-----------------------------------------------------------------------------

module Core.CheckFBIP( checkFBIP
                     ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative
import Data.List( partition, intersperse )

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.Error
import Common.Syntax

import Kind.Kind
import Kind.Newtypes

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
import Core.Borrowed

trace s x =
  Lib.Trace.trace s
    x


checkFBIP :: Pretty.Env ->  Platform -> Newtypes -> Borrowed ->  CorePhase ()
checkFBIP penv platform newtypes borrowed
  = do uniq      <- unique
       defGroups <- getCoreDefs
       let (_,docs) = runChk penv uniq platform newtypes borrowed (chkDefGroups True defGroups) 
       mapM (\doc -> liftError (warningMsg (rangeNull, doc))) docs
       return ()
       


{--------------------------------------------------------------------------
  check definition groups
--------------------------------------------------------------------------}

chkDefGroups :: Bool -> DefGroups -> Chk ()
chkDefGroups topLevel defGroups
  = return ()


{--------------------------------------------------------------------------
  Chk monad
--------------------------------------------------------------------------}
newtype Chk a = Chk (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env,
                platform  :: Platform,
                newtypes  :: Newtypes,
                borrowed  :: Borrowed 
              }

data State = State{ uniq :: Int }

data Result a = Ok a State [Doc]

runChk :: Pretty.Env -> Int -> Platform -> Newtypes -> Borrowed -> Chk a -> (a,[Doc])
runChk penv u platform newtypes borrowed (Chk c)
  = case c (Env [] penv platform newtypes borrowed) (State u) of
      Ok x st docs -> (x,docs)
      
instance Functor Chk where
  fmap f (Chk c)  = Chk (\env st -> case c env st of
                                        Ok x st' dgs -> Ok (f x) st' dgs)

instance Applicative Chk where
  pure  = return
  (<*>) = ap

instance Monad Chk where
  return x       = Chk (\env st -> Ok x st [])
  (Chk c) >>= f = Chk (\env st -> case c env st of
                                      Ok x st' dgs -> case f x of
                                                        Chk d -> case d env st' of
                                                                    Ok x' st'' dgs' -> Ok x' st'' (dgs ++ dgs'))

instance HasUnique Chk where
  updateUnique f = Chk (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) } [])
  setUnique  i   = Chk (\env st -> Ok () st{ uniq = i} [])


withEnv :: (Env -> Env) -> Chk a -> Chk a
withEnv f (Chk c)
  = Chk (\env st -> c (f env) st)

getEnv :: Chk Env
getEnv
  = Chk (\env st -> Ok env st [])

updateSt :: (State -> State) -> Chk State
updateSt f
  = Chk (\env st -> Ok st (f st) [])

-- track the current definition for nicer error messages
withCurrentDef :: Def -> Chk a -> Chk a
withCurrentDef def action
  = -- trace ("chking: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $
    action

currentDefNames :: Chk [Name]
currentDefNames 
  = do env <- getEnv
       return (map defName (currentDef env))

traceDoc :: (Pretty.Env -> Doc) -> Chk ()
traceDoc f
  = do env <- getEnv
       chkTrace (show (f (prettyEnv env)))

chkTrace :: String -> Chk ()
chkTrace msg
  = do env <- getEnv
       trace ("chk: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

emitDoc :: Doc -> Chk ()
emitDoc doc
  = Chk (\env st -> Ok () st [doc])

emitWarning :: Doc -> Chk ()
emitWarning doc
  = do names <- currentDefNames
       let fdoc = text (show names) <.> colon <+> doc
       emitDoc fdoc
-----------------------------------------------------------------------------
-- Copyright 2016 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

module Core.Cps( cpsTransform ) where


-- import Lib.Trace 
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim
import Common.Error

import Kind.Kind( kindStar )
import Type.Type
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Core.Core

cpsTransform :: Pretty.Env -> DefGroups -> Error DefGroups
cpsTransform penv defs
  = runCps penv 0 (cpsDefGroups defs)

{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}  
cpsDefGroups :: DefGroups -> Cps DefGroups
cpsDefGroups cpsDefGroups
  = do defGroupss <- mapM cpsDefGroup cpsDefGroups
       return (concat defGroupss)

cpsDefGroup (DefRec defs) 
  = do defss <- mapM  cpsDef defs
       return [DefRec (concat defss)]

cpsDefGroup (DefNonRec def)
  = do defs <- cpsDef def
       return (map DefNonRec defs)

{--------------------------------------------------------------------------
  transform a definition
--------------------------------------------------------------------------}  
cpsDef :: Def -> Cps [Def]
cpsDef def 
  = withCurrentDef def $
    do return [def]



{--------------------------------------------------------------------------
  Cps monad
--------------------------------------------------------------------------}  
newtype Cps a = Cps (Env -> State -> Result a)

data Env = Env{ prettyEnv :: Pretty.Env, currentDef :: [Def] }

data State = State{ uniq :: Int }

data Result a = Ok a State

runCps :: Monad m => Pretty.Env -> Int -> Cps a -> m a
runCps penv u (Cps c)
  = case c (Env penv []) (State u) of
      Ok x _ -> return x

instance Functor Cps where
  fmap f (Cps c)  = Cps (\env st -> case c env st of 
                                      Ok x st' -> Ok (f x) st')
                                                      
instance Applicative Cps where
  pure  = return
  (<*>) = ap                    

instance Monad Cps where
  return x      = Cps (\env st -> Ok x st)
  (Cps c) >>= f = Cps (\env st -> case c env st of 
                                    Ok x st' -> case f x of 
                                                   Cps d -> d env st' )

instance HasUnique Cps where
  updateUnique f = Cps (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = Cps (\env st -> Ok () st{ uniq = i} )

withCurrentDef :: Def -> Cps a -> Cps a
withCurrentDef def (Cps c)
  = Cps (\env st -> c env{ currentDef = def:currentDef env} st)

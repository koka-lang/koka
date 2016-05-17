-----------------------------------------------------------------------------
-- Copyright 2016 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-- Normalize core for further transformations. In particular, lift out
-- function definitions from expressions and name them explicitly so the
-- norm transformation becomes easier.
-----------------------------------------------------------------------------

module Core.Normalize( normalize ) where


import Lib.Trace 
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim(  )
import Common.Error
import Common.Syntax

import Kind.Kind
import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Type.Operations( freshTVar )
import Core.Core
import qualified Core.Core as Core
import Core.Pretty

normalize :: Pretty.Env -> DefGroups -> DefGroups
normalize penv defgroups
  = runNorm penv 0 (normDefGroups defgroups)

normDefGroups :: DefGroups -> Norm DefGroups
normDefGroups defgroups
  = mapM normDefGroup defgroups

normDefGroup :: DefGroup -> Norm DefGroup
normDefGroup (DefRec defs) 
  = do defs' <- mapM (normDef True) defs
       return (DefRec defs')

normDefGroup (DefNonRec def)
  = do def' <- normDef False def
       return (DefNonRec def')


{--------------------------------------------------------------------------
  transform a definition
--------------------------------------------------------------------------}  
normDef :: Bool -> Def -> Norm Def
normDef recursive def
  = do expr' <- normExpr True (defExpr def)
       return (def{ defExpr = expr' id })


normExpr :: Bool -> Expr -> Norm (Trans Expr)
normExpr topLevel expr
  = case expr of
      TypeLam tpars (Lam pars eff body)  | not topLevel
        -> createDef expr
      TypeApp (TypeLam tvars body) tps  | length tvars == length tps
        -> normExpr topLevel (subNew (zip tvars tps) |-> body)     


createDef :: Expr -> Norm (Trans Expr)
createDef body
  = do body' <- normExpr True body
       name  <- uniqueName "f"
       return $ \k -> 
         body' $ \bb -> 
          let def = Def name (typeOf bb) bb Private DefFun rangeNull ""
              var = Var (TName name (typeOf bb)) InfoNone -- todo: get arity info
          in Let [DefNonRec def] (k var)

type Trans a = TransX a a 
type TransX a b  = (a -> b) ->b

{--------------------------------------------------------------------------
  Normalize monad
--------------------------------------------------------------------------}  
newtype Norm a = Norm (Env -> State -> Result a)

data Env = Env{ prettyEnv :: Pretty.Env, currentDef :: [Def] }

data State = State{ uniq :: Int }

data Result a = Ok a State

runNorm :: Pretty.Env -> Int -> Norm a -> a
runNorm penv u (Norm c)
  = case c (Env penv []) (State u) of
      Ok x _ -> x

instance Functor Norm where
  fmap f (Norm c)  = Norm (\env st -> case c env st of 
                                      Ok x st' -> Ok (f x) st')
                                                      
instance Applicative Norm where
  pure  = return
  (<*>) = ap                    

instance Monad Norm where
  return x      = Norm (\env st -> Ok x st)
  (Norm c) >>= f = Norm (\env st -> case c env st of 
                                    Ok x st' -> case f x of 
                                                   Norm d -> d env st' )

instance HasUnique Norm where
  updateUnique f = Norm (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = Norm (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> Norm a -> Norm a
withEnv f (Norm c)
  = Norm (\env st -> c (f env) st)

getEnv :: Norm Env
getEnv 
  = Norm (\env st -> Ok env st)

updateSt :: (State -> State) -> Norm State
updateSt f
  = Norm (\env st -> Ok st (f st))

withCurrentDef :: Def -> Norm a -> Norm a
withCurrentDef def 
  = -- trace ("norm def: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env})

normTraceDoc :: (Pretty.Env -> Doc) -> Norm ()
normTraceDoc f
  = do env <- getEnv
       normTrace (show (f (prettyEnv env)))

normTrace :: String -> Norm ()
normTrace msg
  = do env <- getEnv
       trace ("norm: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()



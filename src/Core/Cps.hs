-----------------------------------------------------------------------------
-- Copyright 2016 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

module Core.Cps( cpsTransform ) where


import Lib.Trace 
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim
import Common.Error

import Kind.Kind( kindStar, isKindEffect )
import Type.Type
import Type.Kind
import Type.TypeVar
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
  = do pureTvs <- getPureTVars
       if (needsCpsDef pureTvs def) -- only translate when necessary
        then cpsTransDef def
        else return [def]

cpsTransDef :: Def -> Cps [Def]
cpsTransDef def
  = withCurrentDef def $
    return [def]


{--------------------------------------------------------------------------
  Check if expressions need to be cps translated
--------------------------------------------------------------------------}  

-- Does this definition need any cps translation (sometimes deeper inside)
needsCpsDef :: Tvs -> Def -> Bool
needsCpsDef pureTvs def
  = needsCpsType pureTvs (defType def) || needsCpsExpr pureTvs (defExpr def)

needsCpsExpr :: Tvs -> Expr -> Bool
needsCpsExpr pureTvs expr
  = case expr of
      App (TypeApp (Var open _) [_, effTo]) [_] | getName open == nameEffectOpen
        -> needsCpsEffect pureTvs effTo
      App f args 
        -> any (needsCpsExpr pureTvs) (f:args)
      Lam pars eff body
        -> needsCpsEffect pureTvs eff || needsCpsExpr pureTvs body
      TypeApp body targs
        -> any (needsCpsType pureTvs) targs || needsCpsExpr pureTvs body
      TypeLam tpars body
        -> any (isKindEffect . getKind) tpars || needsCpsExpr (tvsRemove tpars pureTvs) body
      Let defs body
        -> any (needsCpsDefGroup pureTvs) defs || needsCpsExpr pureTvs body
      Case exprs bs
        -> any (needsCpsExpr pureTvs) exprs || any (needsCpsBranch pureTvs) bs
      _ -> False

needsCpsDefGroup pureTvs defGroup
  = case defGroup of
      DefRec defs -> any (needsCpsDef pureTvs) defs
      DefNonRec def -> needsCpsDef pureTvs def

needsCpsBranch pureTvs (Branch pat guards)
  = any (needsCpsGuard pureTvs) guards

needsCpsGuard pureTvs (Guard g e)
  = needsCpsExpr pureTvs g || needsCpsExpr pureTvs e

-- Is the type a function with a handled effect?
needsCpsType :: Tvs -> Type -> Bool
needsCpsType pureTvs tp
  = case expandSyn tp of
      TForall vars preds t -> needsCpsType pureTvs t
      TFun args eff res    -> needsCpsEffect pureTvs eff
      _ -> False

needsCpsEffect :: Tvs -> Effect -> Bool
needsCpsEffect pureTvs eff
  = let (ls,tl) = extractEffectExtend eff 
    in any isHandledEffect ls || needsCpsTVar pureTvs tl

needsCpsTVar :: Tvs -> Type -> Bool
needsCpsTVar pureTvs tp
  = case expandSyn tp of
      TVar tv -> not (tvsMember tv pureTvs)
      _       -> False


{--------------------------------------------------------------------------
  Cps monad
--------------------------------------------------------------------------}  
newtype Cps a = Cps (Env -> State -> Result a)

data Env = Env{ pureTVars :: Tvs, prettyEnv :: Pretty.Env, currentDef :: [Def] }

data State = State{ uniq :: Int }

data Result a = Ok a State

runCps :: Monad m => Pretty.Env -> Int -> Cps a -> m a
runCps penv u (Cps c)
  = case c (Env tvsEmpty penv []) (State u) of
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

withEnv :: (Env -> Env) -> Cps a -> Cps a
withEnv f (Cps c)
  = Cps (\env st -> c (f env) st)

getEnv :: Cps Env
getEnv 
  = Cps (\env st -> Ok env st)

updateSt :: (State -> State) -> Cps State
updateSt f
  = Cps (\env st -> Ok st (f st))

withCurrentDef :: Def -> Cps a -> Cps a
withCurrentDef def 
  = trace ("cps def: " ++ show (defName def)) $
    withEnv (\env -> env{ currentDef = def:currentDef env})

withPureTVars :: [TypeVar] -> Cps a -> Cps a
withPureTVars vs
  = withEnv (\env -> env{ pureTVars = tvsUnion (tvsNew vs) (pureTVars env)})

getPureTVars :: Cps Tvs
getPureTVars
  = do env <- getEnv
       return (pureTVars env)  

isPureTVar :: TypeVar -> Cps Bool
isPureTVar tv 
  = do env <- getEnv
       return (tvsMember tv (pureTVars env))
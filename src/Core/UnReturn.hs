-----------------------------------------------------------------------------
-- Copyright 2016-2017 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

{----------------------------------------------------------------------------
 Transform core with return statements into pure core.
 This is necessary for transformations like "URadic" that introduce new
 lambda abstractions over pieces of code; if those would still contain return
 statements, these would now return from the inner function instead of the 
 outer one.
-----------------------------------------------------------------------------}

module Core.UnReturn( unreturn
                   ) where


import qualified Lib.Trace 
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim( nameReturn )
import Common.Error
import Common.Syntax

import Kind.Kind
import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty

import Core.Core
import qualified Core.Core as Core
import Core.Pretty
import Core.CoreVar

trace s x =
   -- Lib.Trace.trace s
    x

unreturn :: Pretty.Env -> DefGroups -> Error DefGroups
unreturn penv defs
  = do kdefgs <- runUR penv 0 (urDefGroups defs)
       case kdefgs of
         U -> return defs
         I defgs -> return defgs
         R e -> failure ("Core.UnReturn.unreturn: return occurred in toplevel definition group")

{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}  
urDefGroups :: DefGroups -> UR (K DefGroups)
urDefGroups defgs
  = do kdefgs <- mapM urDefGroup defgs
       return (listK combine defgs kdefgs)
  where
    combine :: DefGroup -> Expr -> Expr
    combine dg expr  = makeLet [dg] expr
    
urDefGroup :: DefGroup -> UR (K DefGroup)
urDefGroup (DefRec defs) 
  = do kdefs <- mapM (urDef True) defs
       return (mapK DefRec (listK err defs kdefs))
  where
    err dg expr = failure ("Core.UnReturn.urDefGroup: return occurred in recursive definition group.")

urDefGroup (DefNonRec def)
  = do kdef <- urDef False def
       return (mapK DefNonRec kdef)


{--------------------------------------------------------------------------
  transform a definition
--------------------------------------------------------------------------}  
urDef :: Bool -> Def -> UR KDef
urDef recursive def 
  = withCurrentDef def $
    do kexpr <- urExpr (defExpr def)
       return (mapK (\e -> def{ defExpr = e }) kexpr)

urExpr :: Expr -> UR KExpr
urExpr expr
  = case expr of
      -- lambdas and type lambdas use UnK to contain returns inside their body
      Lam pars eff body
        -> do kbody <- urExpr body
              return (emapUnK (Lam pars eff) kbody)

      TypeLam tvars body
        -> do kbody <- urExpr body
              return (emapUnK (TypeLam tvars) kbody)

      -- type applications may contain return (? todo check this)
      TypeApp body targs
        -> do kbody <- urExpr body
              return (emapK (\b -> TypeApp b targs) kbody)

      -- bindings
      Let defgs body 
        -> do kdefgs <- urDefGroups defgs
              kbody  <- urExpr body
              return (compose Let defgs kdefgs kbody)

      -- case: scrutinee cannot contain return due to grammar
      
      
      -- return
      App ret@(Var v _) [arg] | getName v == nameReturn
        -> return (R arg)

      -- pure expressions that do not contain return (as checked by the grammar)
      _ -> return U


data K a = U
         | R Expr
         | I a

type KExpr = K Expr
type KDef  = K Def



mapK :: (a -> b) -> K a -> K b
mapK f kexpr
  = case kexpr of
      U   -> U
      R e -> R e
      I x -> I (f x)

listK :: (a -> Expr -> Expr) -> [a] -> [K a] -> K [a]
listK combine orgs ks 
  = if (all isU ks) then U else foldr f (I []) (zip orgs ks)
  where
    isU U = True
    isU _ = False

    f (org,kexpr) k
      = case kexpr of
          U     -> cons org k
          R e   -> R e
          I x   -> cons x k

    cons x k
       = case k of
           I xs  -> I (x:xs)
           R e   -> R (combine x e)
           _     -> failure ("Core.UnReturn.listK: should not happen")


emapUnK :: (Expr -> Expr) -> KExpr -> KExpr
emapUnK f kexpr
  = case kexpr of
      U   -> U
      R e -> I (f e)
      I e -> I (f e)

emapK :: (Expr -> Expr) -> KExpr -> KExpr
emapK f kexpr
  = case kexpr of
      U   -> U
      R e -> R (f e)
      I e -> I (f e)      


compose :: (a -> Expr -> Expr) -> a -> K a -> KExpr -> KExpr
compose combine a ke1 ke2 
  = case (ke1,ke2) of
      (U, k)   -> emapK (combine a) k
      (R e, _) -> R e
      (I x, k) -> emapK (combine x) k


{--------------------------------------------------------------------------
  UR monad
--------------------------------------------------------------------------}  
newtype UR a = UR (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def], prettyEnv :: Pretty.Env }

data State = State{ uniq :: Int }

data Result a = Ok a State

runUR :: Monad m => Pretty.Env -> Int -> UR a -> m a
runUR penv u (UR c)
  = case c (Env [] penv) (State u) of
      Ok x _ -> return x

instance Functor UR where
  fmap f (UR c)  = UR (\env st -> case c env st of 
                                      Ok x st' -> Ok (f x) st')
                                                      
instance Applicative UR where
  pure  = return
  (<*>) = ap                    

instance Monad UR where
  return x      = UR (\env st -> Ok x st)
  (UR c) >>= f = UR (\env st -> case c env st of 
                                    Ok x st' -> case f x of 
                                                   UR d -> d env st' )

instance HasUnique UR where
  updateUnique f = UR (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = UR (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> UR a -> UR a
withEnv f (UR c)
  = UR (\env st -> c (f env) st)

getEnv :: UR Env
getEnv 
  = UR (\env st -> Ok env st)

updateSt :: (State -> State) -> UR State
updateSt f
  = UR (\env st -> Ok st (f st))

withCurrentDef :: Def -> UR a -> UR a
withCurrentDef def action
  = -- trace ("mon def: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $ action


urTraceDoc :: (Pretty.Env -> Doc) -> UR ()
urTraceDoc f
  = do env <- getEnv
       urTrace (show (f (prettyEnv env)))

urTrace :: String -> UR ()
urTrace msg
  = do env <- getEnv
       trace ("unreturn: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()


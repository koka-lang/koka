-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Core.Parc ( parcCore ) where

import qualified Lib.Trace
import Control.Applicative hiding (empty)
import Control.Monad
import Data.List ( intersperse, partition )
import Data.Char
-- import Data.Maybe
-- import Data.Monoid ( mappend )
import qualified Data.Set as S

import Kind.Kind
import Kind.Newtypes
import Type.Type
import Type.TypeVar
import Type.Kind( getKind )
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.Name
import Common.Range
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty
import Core.CoreVar

trace s x =
  Lib.Trace.trace s
    x

--------------------------------------------------------------------------
-- Reference count transformation
--------------------------------------------------------------------------

parcCore :: Pretty.Env -> Int -> Core -> (Core,Int)
parcCore penv u core
  = let (defs',u1) = runParc penv u (parcDefGroups (coreProgDefs core))
    in (core{ coreProgDefs  = defs' }, u1)

{--------------------------------------------------------------------------
  definition groups
--------------------------------------------------------------------------}

parcDefGroups :: DefGroups -> Parc DefGroups
parcDefGroups defGroups
  = do traceDoc (\penv -> text "parcDefGroups")
       return defGroups


{-
val x = y
f(g(x),y,x)
~>

f(g(dup(x)),y,x) | {x,y,f}

~>

f(g(x),y,dup(x)) | {x,y,f}

fun foo(x) {
  val f = (dup(x); allocate_fun(x)) + fun(y | x){ x + y }
  if (x==1) then 2 else f(3) + f(5)
}


parcExpr :: Expr -> Parc Expr
parcExpr (App fun args)
  = do args' <- reverseMapM parcExpr args
       fun'  <- parcExpr fun
       return (App fun' args')
       
parcExpr (Lam pars body)
  = do body' <- withNoUse $ parcExpr body
       -- for each par in pars, if inuse = ok, otherwise drop
       -- remove all pars from "inuse"
       
parcExpr (Let dgs body)
  = do body' <- parcExpr body
       dgs'  <- parcDefGroups dgs
       return (Let dgs' body')

parcExpr expr@(Con cname info)   
  = do if availableReuse then allocReuse else ..
    
parcExpr expr@(Var vname InfoNone)   -- InfoArity, InfoExternal
  = do inuse <- getInUse(vname)
       if (inuse) 
        then return expr  -- dup it
        else do addInUse(vname)
                return expr 
       
parcExpr expr  
  = return expr

-}
  
    
    
{--------------------------------------------------------------------------
 Parc monad
--------------------------------------------------------------------------}
newtype Parc a = Parc (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env }

data State = State{ uniq :: Int }

data Result a = Ok a State

runParc :: Pretty.Env -> Int -> Parc a -> (a,Int)
runParc penv u (Parc c)
 = case c (Env [] penv) (State u) of
     Ok x st -> (x,uniq st)     

instance Functor Parc where
 fmap f (Parc c)  = Parc (\env st -> case c env st of
                                       Ok x st' -> Ok (f x) st')

instance Applicative Parc where
 pure  = return
 (<*>) = ap

instance Monad Parc where
 return x       = Parc (\env st -> Ok x st)
 (Parc c) >>= f = Parc (\env st -> case c env st of
                                     Ok x st' -> case f x of
                                                   Parc d -> case d env st' of
                                                               Ok x' st'' -> Ok x' st'')

instance HasUnique Parc where
 updateUnique f = Parc (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
 setUnique  i   = Parc (\env st -> Ok () st{ uniq = i})

withEnv :: (Env -> Env) -> Parc a -> Parc a
withEnv f (Parc c)
 = Parc (\env st -> c (f env) st)

getEnv :: Parc Env
getEnv
 = Parc (\env st -> Ok env st)

updateSt :: (State -> State) -> Parc State
updateSt f
 = Parc (\env st -> Ok st (f st))


withCurrentDef :: Def -> Parc a -> Parc a
withCurrentDef def action
 = -- trace ("Parcing: " ++ show (defName def)) $
   withEnv (\env -> env{currentDef = def:currentDef env}) $
   action

traceDoc :: (Pretty.Env -> Doc) -> Parc ()
traceDoc f
 = do env <- getEnv
      parcTrace (show (f (prettyEnv env)))

parcTrace :: String -> Parc ()
parcTrace msg
 = do env <- getEnv
      trace ("Core.Parc: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

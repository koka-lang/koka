-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Analyse divergence of core match statements
    Currently very simple where only termination of inductively defined
    single recursive functions is analyzed.
-}
-----------------------------------------------------------------------------

module Core.Divergent( analyzeDivergence ) where

-- import Lib.Trace
import Control.Applicative
import Control.Monad
import Data.List( transpose, permutations )
import Common.Name
import Common.NamePrim( nameSubStr1, namesSameSize, nameEffectOpen, nameDecreasing )
import Common.Failure
import Common.Syntax
import qualified Common.NameSet as S
import Type.Type
import Type.Pretty ()
import Core.Core
import Core.Pretty()
import Core.Uniquefy( uniquefyDefGroup )

-- | Fix a group of recursive definition to potentially include
-- the divergence effect in their type.
analyzeDivergence :: [Def] -> Bool
analyzeDivergence defs0
  = -- trace (" analyzeDivergence: " ++ show (map defName defs0)) $
    let mayDiverge = case defs0 of
                      [] -> False
                      (def1:def2:rest) 
                        -> True -- for mutual recursion, we assume it is divergent for now
                      [def0]
                        -> let (DefRec [def1]) = uniquefyDefGroup (DefRec [def0])
                           in isDivergent (def1)
    in -- trace (" analyzeDivergence: " ++ show (map defName defs0) ++ ": " ++ show mayDiverge) $
       mayDiverge

-- | Is a recursive definition group potentially divergent? 
-- Assumes that the the definitions are uniquefied.
-- A valid result is always to return 'True'
isDivergent :: Def -> Bool
isDivergent def
  = isDivergentBody (defName def) (defExpr def) 

isDivergentBody dname body
  = case body of
      TypeLam tpars expr
        -> isDivergentBody dname expr
      TypeApp expr targs
        -> isDivergentBody dname expr
      Lam pars eff body  
        -> isDivFun dname pars body
      _ -> -- ctrace DarkRed ("Core.Divergent.isDivergentBody: not a function? " ++ show body) $
           True  -- assume that non-functions are divergent



isDivFun :: Name -> [TName] -> Expr -> Bool
isDivFun name [] body
  = True
isDivFun name pars body
  = let (_,calls) = runDiv name pars (divExpr body)
        orders    = map transpose (permutations (transpose calls)) 
        divergent = not (any isAnOrder orders)
        isAnOrder cs
          = all (\call -> case dropWhile (==Eq) call of
                            (Lt:_) -> True
                            _      -> False) cs

        -- call      = foldr (zipWith max) (replicate (length pars) Lt) calls
        -- divergent = (head (sort call) /= Lt)
    in -- trace (" divergence: " ++ show name ++ (if divergent then ": divergent" else ": terminating") ++ ", recursive calls= " ++ show calls ++ ", " ++ show orders) $
       divergent



newtype Div a = Div (Rel -> (a,[Call]))

data Rel   = Rel Name [S.NameSet] [S.NameSet]  -- set of equal and smaller variables per argument
type Call  = [Size]       -- arguments sizes of a recursive call
data Size  = Lt | Eq | Unknown
           deriving (Eq,Ord)

instance Show Size where
  show sz = case sz of
              Lt -> "<"
              Eq -> "="
              Unknown -> "?"

runDiv :: Name -> [TName] -> Div a -> (a,[Call])
runDiv defName args (Div d)
  = let rel = Rel defName (map (S.singleton . getName) args) (replicate (length args) S.empty)
    in d rel

instance Functor Div where
  fmap f (Div d)   = Div (\rel -> case d rel of (x,calls) -> (f x, calls))

instance Applicative Div where
  pure  = return
  (<*>) = ap  

instance Monad Div where
  return x  = Div (\rel -> (x,[]))
  (Div d) >>= f  = Div (\rel -> case d rel of
                                  (x,calls1) -> case f x of
                                                  Div d2 -> case d2 rel of
                                                              (y,calls2) -> (y,calls1 ++ calls2))
      

getRel :: Div Rel
getRel
  = Div (\rel -> (rel,[]))

withRel :: Rel -> Div a -> Div a
withRel rel (Div d)
  = Div (\rel0 -> d rel)

addCall :: Name -> [Size] -> Div ()
addCall name call
  = Div (\rel -> ((),[call]))


at :: [S.NameSet] -> Int -> S.NameSet
at xs i
  = if (i >= length xs) 
     then S.empty -- turns out this can happen for ill-typed programs but this caught later on. see test: static/wrong/rec1
     else (xs !! i)  

isRecursiveCall :: Name -> Div Bool
isRecursiveCall name
  = do (Rel rname _ _) <- getRel
       return (rname == name)

lookupSize :: Name -> Int -> Name -> Div Size
lookupSize defName argPos argName
  = do (Rel rname eqs lts) <- getRel
       if (rname /= defName)
        then return Unknown
        else let equal = eqs `at` argPos
                 lower = lts `at` argPos
             in if (S.member argName lower)
                 then return Lt
                 else if (S.member argName equal)
                  then return Eq
                  else return Unknown

addRelation :: Size -> Name -> Name -> Div a -> Div a
addRelation sz name1 name2 div
  = if (sz == Unknown)
     then div
     else do (Rel rname eqs lts) <- getRel
             let (eqs',lts') = unzip (zipWith add eqs lts)
                 add eq lt   = if (S.member name1 eq)
                                then case sz of
                                       Lt -> (eq,S.insert name2 lt)
                                       Eq -> (S.insert name2 eq,lt)
                                       _  -> failure "COre.Divergent.addRelation: encountered Unknown"
                                else if (S.member name1 lt)
                                      then (eq,S.insert name2 lt)
                                      else (eq,lt)
             withRel (Rel rname eqs' lts') div     



divExpr :: Expr -> Div ()
divExpr expr
  = case expr of
      Lam tnames eff expr   
        -> divExpr expr
      -- Ignore .open effect calls
      App (App (TypeApp (Var openName _) _) [f]) args  | getName openName == nameEffectOpen        
        -> divExpr (App f args)
      App (TypeApp (App (TypeApp (Var openName _) _) [f]) targs) args  | getName openName == nameEffectOpen        
        -> divExpr (App (TypeApp f targs) args)
      -- applications        
      App (TypeApp var@(Var tname info) targs) args
        -> divExpr (App var args)
      App (Var tname info) args
        -> do isRec <- isRecursiveCall (getName tname)
              if isRec 
                then do call <- mapM (argumentSize (getName tname)) (zip [0..] args) -- todo: should we add 'Unknown's for partial applications?
                        addCall (getName tname) call
                else return ()
              mapM_ divExpr args

      Var tname info  -- recursive call may appear as argument, say id(recfun)(x)
        -> do isRec <- isRecursiveCall (getName tname)
              if isRec then addCall (getName tname) [Unknown] else return ()
              
      App f args        
        -> do divExpr f
              mapM_ divExpr args
      TypeLam tvs expr  
        -> divExpr expr
      TypeApp expr tps 
        -> divExpr expr
      Let defGroups expr  
        -> do mapM_ divDefGroup defGroups
              divExpr expr
      Case exprs branches 
        -> do mapM_ divExpr exprs
              mapM_ (divBranch exprs) branches
      _ -> return ()

divBranch :: [Expr] -> Branch -> Div ()
divBranch exprs (Branch patterns guards)
  = do fs <- mapM (divPattern Eq) (zip (map extractName exprs) patterns)
       mapM_ (divGuard fs) guards
  where
    divGuard    :: [Div () -> Div ()] -> Guard -> Div ()
    divGuard fs (Guard test expr)
      = compose fs $ do divExpr test
                        divExpr expr
    extractName :: Expr -> Maybe Name
    extractName expr
      = case expr of
          Var tname _ -> Just (getName tname)
          App (TypeApp (Var sameSize _) _) [Var tname _] | getName sameSize `elem` namesSameSize 
              -> Just(getName tname) 
          _   -> Nothing

divPattern :: Size -> (Maybe Name,Pattern) -> Div (Div a -> Div a)
divPattern size (mbName,pat)
  = case (mbName,pat) of
      (Just name, PatVar pname pat)
        -> do f <- divPattern size (mbName,pat)
              return (addRelation size name (getName pname) . f)  
      (_, PatCon _ patterns _ _ _ _ info)
        -> do fs <- mapM (\pat -> divPattern (if conInfoTypeSort info == Inductive then Lt else size) (mbName,pat)) patterns
              return (compose fs)
      (_, _)
        -> return id


divDefGroup :: DefGroup -> Div ()
divDefGroup defgroup
  = case defgroup of
      DefNonRec def -> divDef def
      DefRec defs   -> mapM_ divDef defs
    
divDef def
  = divExpr (defExpr def)




argumentSize :: Name -> (Int,Expr) -> Div Size
argumentSize name (pos,arg)
  = case arg of
      Var tname info
        -> lookupSize name pos (getName tname)
      -- Ignore .open effect calls
      App (App (TypeApp (Var openName _) _) [f]) args  | getName openName == nameEffectOpen        
        -> argumentSize name (pos,App f args)
      App (TypeApp (App (TypeApp (Var openName _) _) [f]) targs) args  | getName openName == nameEffectOpen        
        -> argumentSize name (pos,App (TypeApp f targs) args)
      -- special 'unsafeDecreasing' call
      App (TypeApp (Var name _) [targ]) [arg] | getName name == nameDecreasing
        -> return Lt  
      -- special case substr1
      App (Var substrName _) (Var sname _ : args) | getName substrName == nameSubStr1
        -> do sz <- lookupSize name pos (getName sname)
              return (reduceSize sz) 
      -- these two cases state that the call to a function f where f < name is itself < name.
      App (Var tname info) args
        -> lookupSize name pos (getName tname)
      App (TypeApp (Var tname info) targs) args
        -> lookupSize name pos (getName tname)
      _ -> return Unknown
  where
    reduceSize Eq    = Lt
    reduceSize other = other      

compose :: [(a -> a)] -> a -> a
compose [] x = x 
compose (f:fs) x = f (compose fs x)

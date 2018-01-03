-----------------------------------------------------------------------------
-- Copyright 2017 Daan Leijen, Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Analyse partiality of core match statements
-}
-----------------------------------------------------------------------------

module Core.AnalysisResume( ResumeKind(..), analyzeResume ) where


-- import Lib.Trace
import Lib.PPrint( Doc, text )
import qualified Data.Set as S
import Common.Syntax( Target(..) )
import Common.Id
import Common.Name
import Common.Range
import Common.Failure
import Common.Unique()
import Common.NamePrim( namePatternMatchError, nameSystemCore )
import Kind.Kind( kindStar )
import Type.Type
import Type.Pretty ()
import Core.Core
import Core.CoreVar

data ResumeKind 
  = ResumeNever
  | ResumeTail
  | ResumeOnce
  | ResumeNormal
  | ResumeShallow
  deriving (Eq,Ord,Enum)

analyzeResume :: Expr -> ResumeKind
analyzeResume expr
  = case expr of
      Lam pars eff body -> arTailExpr body
      TypeLam _ (Lam pars eff body) -> arTailExpr body
      _                 -> failure "Core.AnalysisResume.analyzeResume: invalid branch expression"

arTailExpr expr  = arExpr' ResumeTail expr
arExpr expr      = arExpr' ResumeOnce expr

arExpr' appResume expr
  = case expr of
      Lam tnames eff body
        -> if (resumeName `elem` map getName tnames) then ResumeNever else arExpr' appResume body
      App (Var tname info) args  | getName tname == resumeName      
        -> appResume `rand` arExprsAnd args
      App f args        
        -> arExprsAnd (f:args)
      TypeLam tvs body  
        -> arExpr body
      TypeApp body tps   
        -> arExpr body
      Var tname info    
        -> if (resumeName == getName tname) then ResumeNormal else ResumeNever
      Con tname repr     
        -> ResumeNever
      Lit lit            
        -> ResumeNever
      Let defGroups expr -- TODO: be more sophisticated here?
        -> if (resumeName `elem` map getName (S.elems (bv defGroups)))
            then ResumeNormal
           else if (resumeName `elem` map getName (S.elems (fv defGroups))) 
            then ResumeNormal
            else arExpr' appResume expr 
      Case exprs branches 
        -> arExprsAnd exprs `rand` arBranches appResume branches

arBranches :: ResumeKind -> [Branch] -> ResumeKind
arBranches appResume branches
  = arOr  (map (arBranch appResume) branches)

arBranch ::ResumeKind -> Branch -> ResumeKind
arBranch appResume (Branch pat guards)
  = if (resumeName `elem` map getName (S.elems (bv pat))) then ResumeNever
     else arOr (map (arGuard appResume) guards)

arGuard :: ResumeKind -> Guard -> ResumeKind
arGuard appResume (Guard guardExpr expr)
  = arExpr guardExpr `rand` arExpr' appResume expr     


arExprsAnd :: [Expr] -> ResumeKind
arExprsAnd exprs
  = foldr rand ResumeNever (map arExpr exprs)

arOr :: [ResumeKind] -> ResumeKind
arOr rks
  = foldr ror ResumeNever rks

rand,ror,rmax :: ResumeKind -> ResumeKind -> ResumeKind
rand rk1 rk2
  = case (rk1,rk2) of
      (ResumeNever,rk) -> rk
      (rk,ResumeNever) -> rk
      _                -> ResumeNormal

ror rk1 rk2
  = rmax rk1 rk2

rmax rk1 rk2
  = if (rk1 == ResumeNormal)  -- increase laziness
     then ResumeNormal
     else toEnum (max (fromEnum rk1) (fromEnum rk2))

resumeName = newName "resume"
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


import Lib.Trace
import Lib.PPrint
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

instance Show ResumeKind where
  show rk = case rk of 
              ResumeNever -> "never"
              ResumeTail  -> "tail"
              ResumeOnce  -> "once"
              ResumeNormal -> "normal"
              ResumeShallow -> "shallow"

analyzeResume :: Name -> Name -> Expr -> ResumeKind
analyzeResume defName opName expr
  = case expr of
      Lam pars eff body -> let rk = arTailExpr body 
                           in traceDoc (text "operator branch" <+> parens (pretty defName) <+> pretty opName <> text ": resume" <+> text (show rk)) $
                              rk
      TypeLam _ body    -> analyzeResume defName opName body
      TypeApp body _    -> analyzeResume defName opName body
      App _ [body]      -> analyzeResume defName opName body  -- for toAny (...)
      _                 -> failure "Core.AnalysisResume.analyzeResume: invalid branch expression"


arTailExpr expr  = arExpr' ResumeTail expr
arExpr expr      = arExpr' ResumeOnce expr

isResumingElem tnames = S.member resumeName tnames || S.member finalizeName tnames
isResuming tname = tname == resumeName || tname == finalizeName

arExpr' appResume expr
  = case expr of
      Lam tnames eff body
        -> if (isResumingElem (fv expr)) then ResumeNormal else ResumeNever
      App (Var tname info) args  | isResuming tname
        -> appResume `rand` arExprsAnd args
      App f args        
        -> arExprsAnd (f:args)
      TypeLam tvs body  
        -> arExpr body
      TypeApp body tps   
        -> arExpr body
      Var tname info    
        -> if (isResuming tname) then ResumeNormal else ResumeNever
      Con tname repr     
        -> ResumeNever
      Lit lit            
        -> ResumeNever
      Let [DefNonRec def] expr 
        | defName def == getName finalizeName  -- TODO: too weak a check; improve it
        -> arExpr' appResume expr 
      Let defGroups expr -- TODO: be more sophisticated here?
        -> if (isResumingElem (bv defGroups `S.union` fv defGroups))
            then ResumeNormal else arExpr' appResume expr 
      Case exprs branches 
        -> arExprsAnd exprs `rand` arBranches appResume branches

arBranches :: ResumeKind -> [Branch] -> ResumeKind
arBranches appResume branches
  = rors  (map (arBranch appResume) branches)

arBranch ::ResumeKind -> Branch -> ResumeKind
arBranch appResume (Branch pat guards)
  = -- even if resume or finalize is bound, the analysis is conversative enough
    rors (map (arGuard appResume) guards)

arGuard :: ResumeKind -> Guard -> ResumeKind
arGuard appResume (Guard guardExpr expr)
  = arExpr guardExpr `rand` arExpr' appResume expr     


arExprsAnd :: [Expr] -> ResumeKind
arExprsAnd exprs
  = rands (map arExpr exprs)

rors, rands :: [ResumeKind] -> ResumeKind
rors rks
  = foldr ror ResumeNever rks
rands rks
  = foldr rand ResumeNever rks  

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

resumeName = TName (newName "resume") typeVoid
finalizeName = TName (newName "finalize") typeVoid
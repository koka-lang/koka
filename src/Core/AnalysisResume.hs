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
import Type.Pretty (defaultEnv)
import Core.Core
import Core.CoreVar
import Core.Pretty

data ResumeKind
  = ResumeNever
  | ResumeTail
  | ResumeScopedOnce
  | ResumeScoped
  | ResumeOnce
  | ResumeNormal
  | ResumeOnceRaw
  | ResumeNormalRaw
  deriving (Eq,Ord,Enum)

instance Show ResumeKind where
  show rk = case rk of
              ResumeNever -> "never"
              ResumeTail  -> "tail"
              ResumeScopedOnce -> "scoped once"
              ResumeScoped -> "scoped"
              ResumeOnce  -> "once"
              ResumeNormal -> "normal"
              ResumeOnceRaw -> "once (no finalization)"
              ResumeNormalRaw -> "normal (no finalization)"

analyzeResume :: Name -> Name -> Bool -> Expr -> ResumeKind
analyzeResume defName opName raw expr
  = case expr of
      Lam pars eff body -> let rk0 = arTailExpr body
                               rk  = if (not raw) then rk0
                                      else ResumeNormalRaw
                                        {- if (raw && (rk0==ResumeOnce || rk0<=ResumeScopedOnce))
                                            then ResumeOnceRaw else ResumeNormalRaw -}
                           in traceDoc (text "operator branch" <+> parens (pretty defName) <+> pretty opName <.> text ": resume" <+> text (show rk)
                                      --  </> prettyExpr defaultEnv body
                                       ) $
                              rk
      TypeLam _ body    -> analyzeResume defName opName raw body
      TypeApp body _    -> analyzeResume defName opName raw body
      App _ [body]      -> analyzeResume defName opName raw body  -- for toAny (...)
      _                 -> failure "Core.AnalysisResume.analyzeResume: invalid branch expression"


arTailExpr expr  = arExpr' ResumeTail expr
arExpr expr      = arExpr' ResumeScopedOnce expr

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
        | defName def == getName resumeName  -- TODO: too weak a check!! improve it!! we just need to skip the first definition..
        -> arExpr' appResume expr
      Let defGroups body -- TODO: be more sophisticated here?
        -> if (isResumingElem (bv defGroups `S.union` fv defGroups))
            then ResumeNormal else arExpr' appResume body
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



{-
*and*        never   tail    sonce   scoped  once    normal
-----------------------------------------------------------
never        never   tail    sonce   scoped  once    normal
tail         tail    *scoped scoped  scoped  normal  normal
sonce        sonce   scoped  scoped  scoped  normal  normal
scoped       scoped  scoped  scoped  scoped  normal  normal
once         once    normal  normal  normal  normal  normal
normal       normal  normal  normal  normal  normal  normal

*or*         never   tail    sonce   scoped  once    normal
-----------------------------------------------------------
never        never   tail    sonce   scoped  once    normal
tail         tail    tail    sonce   scoped  once    normal
sonce        sonce   sonce   sonce   scoped  once    normal
scoped       scoped  scoped  scoped  scoped  *normal normal
once         once    once    once    *normal *normal normal
normal       normal  normal  normal  normal  normal  normal
-}

rand,ror :: ResumeKind -> ResumeKind -> ResumeKind
rand rk1 rk2
  = case (rk1,rk2) of
      (ResumeNever,rk) -> rk
      (rk,ResumeNever) -> rk
      _                -> if (isScoped rk1 && isScoped rk2) then ResumeScoped else ResumeNormal

ror rk1 rk2
  = case (rk1,rk2) of
      (ResumeOnce,rk) | rk >= ResumeScoped -> ResumeNormal
      (rk,ResumeOnce) | rk >= ResumeScoped -> ResumeNormal
      _ -> if (rk1 >= rk2) then rk1 else rk2


isScoped ResumeNormal = False
isScoped ResumeOnce   = False
isScoped _            = True

resumeName = TName (newName "resume") typeVoid
finalizeName = TName (newName "finalize") typeVoid

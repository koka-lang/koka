-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen, Ningning Xie
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Flt all local and anonymous functions to top level. No more letrec :-)
-----------------------------------------------------------------------------

module Core.OpenFloat( openFloat ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative
import Data.Maybe( catMaybes )
import Lib.PPrint
import Common.Failure
import Common.NamePrim ( nameEffectOpen )
import Common.Name
import Common.Range
import Common.Unique
import Common.Error
import Common.Syntax

import Kind.Kind
import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Core.Core
import qualified Core.Core as Core
import Core.Pretty

trace s x =
   Lib.Trace.trace s
    x

enable = -- set to True to enable the transformation
  True
  -- False

openFloat :: Pretty.Env -> Gamma -> CorePhase ()
openFloat penv gamma
  = liftCorePhaseUniq $ \uniq defs ->
    let
      (expr, i) = runFlt penv gamma uniq $
        (if enable then fltDefGroups else return) defs
    in (expr, i)


{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}
fltDefGroups :: DefGroups -> Flt DefGroups
fltDefGroups = foldr fltDefGroup (return [])

fltDefGroup :: DefGroup -> Flt [DefGroup] -> Flt [DefGroup]
fltDefGroup (DefRec defs) next
  = do defs' <-  mapM fltDef defs
       next' <- next 
       return $ DefRec defs' : next'

fltDefGroup (DefNonRec def) next
 = do def' <- fltDef def
      next' <-  next
      return $ DefNonRec def' : next'

fltDef :: Def -> Flt Def
fltDef def
  = withCurrentDef def $
    do (expr', _) <- fltExpr (defExpr def)
       return def{ defExpr = expr' }

fltExpr :: Expr -> Flt (Expr, Req)
fltExpr expr
  = case expr of
    -- flt open[...](f)(args)  = flt f(args)
    App (App eopen@(TypeApp (Var open _) [effFrom,effTo,tpFrom,tpTo]) [f]) args | getName open == nameEffectOpen
      -> do fltExpr $ App f args

    App f args
      -> do (f', rqf) <- fltExpr f
            args_rq' <- mapM fltExpr args
            tp <- getFunType f  -- debug
            let (args', rqs) = unzip args_rq'
                Just(_, feff, _) = splitFunType tp
                rqSup = sup $ Eff feff  : rqf : rqs
                frest = smartRestrictExpr rqf rqSup f'
                f'' = smartOpenExpr (Eff feff) rqSup frest
                args'' = map (\(e, rq)-> smartRestrictExpr rq rqSup e) args_rq'
            return (App f'' args'', rqSup)
            where
              getFunType :: Expr -> Flt Type
              getFunType expr = case typeOf expr of
                funtp@TFun{} -> return funtp
                tp -> do traceDoc $ \env -> text "App" <+> niceType env tp
                         return tp

    Lam args eff body
      -> do traceDoc $ \env -> text "lambda:" <+> niceType env eff
            (body', rq) <- fltExpr body
            return (Lam args eff body', Bottom)

    Let defgs body
      -> do defgIR_rqs <- mapM fltDefGroupAux defgs
            (body', rq) <- fltExpr body
            let (defgIRs, rqs) = unzip defgIR_rqs
                rqSup = sup $ rq:rqs
            defgs' <- mapM (restrictToDG rqSup) defgIRs
            return (Let defgs' body', rqSup)
    Case exprs bs
      -> do exprIR_rqs <- mapM fltExpr exprs
            bIR_rqs <- mapM fltBranchAux bs
            let rqe = foldl supb Bottom $ map snd exprIR_rqs
                (bIRs, rqbs) = unzip bIR_rqs
                rqSup = sup $ rqe : rqbs
            exprs'' <- mapM (restrictToE rqSup) exprIR_rqs
            bs'' <- mapM (restrictToB rqSup) bIRs
            return (Case exprs'' bs'', rqSup)

    -- type application and abstraction
    TypeLam tvars body
      -> do (body', rq) <- fltExpr body
            return $ (TypeLam tvars body', Bottom)

    TypeApp body tps
      -> do (body', rq) <- fltExpr body
            return $ (TypeApp body' tps, rq)

    -- the rest
    _ -> return (expr, Bottom)
    where
      restrictToD :: Req -> DefIR -> Flt Def
      restrictToD rqSup (DefIR def@Def{defExpr=expr} rq) = return $ def{defExpr= smartRestrictExpr rq rqSup expr}
      restrictToDG :: Req -> DefGroupIR -> Flt DefGroup
      restrictToDG rqSup (DefRecIR defIRs) =
        do defs <- mapM (restrictToD rqSup) defIRs
           return $ DefRec defs
      restrictToDG rqSup (DefNonRecIR defIR) =
        do def <- restrictToD rqSup defIR
           return $ DefNonRec def
      restrictToE :: Req -> (Expr, Req) -> Flt Expr
      restrictToE rqSup (e, rq) = return $ smartRestrictExpr rq rqSup e
      restrictToB :: Req -> BranchIR -> Flt Branch
      restrictToB rqSup (BranchIR pt gIRs) =
        do guards' <- mapM (restrictToG rqSup) gIRs
           return $ Branch pt guards'
      restrictToG :: Req -> GuardIR -> Flt Guard
      restrictToG rqSup (GuardIR t g) =
        do testExpr' <- restrictToE rqSup t
           guardExpr' <- restrictToE rqSup g
           return $ Guard testExpr' guardExpr'

fltDefAux :: Def -> Flt (DefIR, Req)
fltDefAux def@Def{defExpr=expr} =
  do (expr', rq) <- fltExpr expr
     return (DefIR def{defExpr=expr'} rq, rq)

fltDefGroupAux :: DefGroup -> Flt (DefGroupIR, Req)
fltDefGroupAux (DefRec defs) =
  do defIR_rqs <- mapM fltDefAux defs
     let (defIRs, rqs) = unzip defIR_rqs
     return (DefRecIR defIRs, sup rqs)
fltDefGroupAux (DefNonRec def) =
  do (defIR, rq) <- fltDefAux def
     return (DefNonRecIR defIR, rq)

fltBranchAux :: Branch -> Flt (BranchIR, Req)
fltBranchAux (Branch pat guards)
  = do guard_rqs <- mapM fltGuardAux guards
       let (guards', rqs) = unzip guard_rqs
       return $ (BranchIR pat guards', sup rqs)

fltGuardAux :: Guard -> Flt (GuardIR, Req)
fltGuardAux (Guard guard body)
  = do (guard', rqg) <- fltExpr guard
       (body', rqb)  <- fltExpr body
       return $ (GuardIR (guard', rqg) (body', rqb), supb rqg rqb)


-- optApp :: Expr -> Expr -> Expr -> Flt Expr
-- optApp ef@(App (TypeApp topen1@(Var open0 _) [effFrom0,effTo0,tpFrom0,tpTo0]) [f])
--        ee1@(App (App (TypeApp (Var open1 _) [effFrom1,effTo1,tpFrom1,tpTo1]) [e1]) [arg1])
--        ee2@(App (App (TypeApp (Var open2 _) [effFrom2,effTo2,tpFrom2,tpTo2]) [e2]) [arg2])
--    | getName open0 == nameEffectOpen && getName open1 == nameEffectOpen && getName open2 == nameEffectOpen
--      && matchType effFrom0 effFrom1 && matchType effFrom1 effFrom2
--      && matchType effTo0 effTo1 && matchType effTo1 effTo2
--    = do traceDoc $ \penv -> text "found it!"
--         return $ App (TypeApp topen1 [effFrom0,effTo0,tpFrom0,tpTo0])
--                     [Lam [] effFrom0
--                        (App f [App e1 [arg1], App e2 [arg2]])]

-- optApp f e1 e2
--    = do return (App f [e1,e2])

{--------------------------------------------------------------------------
  Requirement
--------------------------------------------------------------------------}

data Req = Eff Effect | Bottom deriving (Eq, Show)

sup :: [Req] -> Req
sup = foldl supb Bottom

supb :: Req -> Req -> Req
supb Bottom rq = rq
supb rq Bottom = rq
supb (Eff eff1) (Eff eff2) = Eff $ supbEffect eff1 eff2


supbEffect :: Effect -> Effect -> Effect
supbEffect eff1 eff2 =
  let
    (labs1, tl1) = extractOrderedEffect eff1
    (labs2, tl2) = extractOrderedEffect eff2
    tl = assertion
           ("OpenFlaot. sup undefined between:\n" ++ "A. " ++ show tl1 ++ "\nB. " ++ show tl2)
           (isEffectEmpty tl1 || isEffectEmpty tl2 || tl1 `matchEffect` tl2 )
           (if isEffectEmpty tl1 then tl2 else tl1)
    labs = mergeLabs labs1 labs2
  in effectExtends labs tl
  where
    compareLabel :: Tau -> Tau -> Ordering
    compareLabel l1 l2 = labelNameCompare (labelName l1) (labelName l2)
    mergeLabs :: [Tau] -> [Tau] -> [Tau]
    mergeLabs [] labs = labs
    mergeLabs labs [] = labs
    mergeLabs labs1@(l1:ls1) labs2@(l2:ls2) = case l1 `compareLabel` l2 of
      EQ -> l1:mergeLabs ls1 ls2
      LT -> l1:mergeLabs ls1 labs2
      GT -> l2:mergeLabs labs1 ls2


matchEffect :: Effect -> Effect -> Bool
matchEffect eff1 eff2 = matchType (orderEffect eff1) (orderEffect eff2)

{--------------------------------------------------------------------------
  smart open
--------------------------------------------------------------------------}

smartRestrictExpr :: Req -> Req -> Expr  -> Expr
smartRestrictExpr Bottom _ expr = expr
smartRestrictExpr (Eff _) Bottom _ = undefined
smartRestrictExpr (Eff effFrom) (Eff effTo) expr =
  if matchEffect effFrom effTo then expr else
    let tp = typeOf expr
        nameRestrictParam = newHiddenName "RESTRICT"
        in App (openEffectExpr
                  effFrom
                  effTo
                  (TFun [(nameRestrictParam, typeUnit)] effFrom tp)
                  (TFun [(nameRestrictParam, typeUnit)] effTo tp)
                  (Lam [TName nameRestrictParam typeUnit] effFrom expr))
               [exprUnit]

smartOpenExpr :: Req -> Req -> Expr -> Expr
smartOpenExpr Bottom _ e = e
smartOpenExpr (Eff _) Bottom _ = undefined
smartOpenExpr (Eff effFrom) (Eff effTo) expr =
  if matchEffect effFrom effTo then expr else
    let tp@(TFun tpPar feff tpRes)= typeOf expr
        in
          assertion "smart open check" (matchEffect effFrom feff) $
          openEffectExpr effFrom effTo tp (TFun tpPar effTo tpRes) expr

-- smartRestrictDefGroups :: Req -> (DefGroups, Req) -> DefGroups 
-- smartRestrictDefGroup :: Req -> 


{--------------------------------------------------------------------------
  IRs : Return type of auxiliary functions. Each Expr has own rq in order to be restricted.
--------------------------------------------------------------------------}

data BranchIR = BranchIR { branchPatterns :: [Pattern]
                         , branchGuardIRs :: [GuardIR]}
data GuardIR = GuardIR { guardTestIR :: (Expr, Req)
                       , guardExprIR :: (Expr, Req)}
data DefIR = DefIR { def :: Def, req :: Req}
data DefGroupIR = DefRecIR [DefIR] | DefNonRecIR DefIR

{--------------------------------------------------------------------------
  Flt monad
--------------------------------------------------------------------------}
newtype Flt a = Flt (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env,
                gamma :: Gamma
              }

data State = State{ uniq :: Int }

data Result a = Ok a State

runFlt :: Pretty.Env -> Gamma -> Int -> Flt a -> (a,Int)
runFlt penv gamma u (Flt c)
  = case c (Env [] penv gamma) (State u) of
      Ok x st -> (x,uniq st)

instance Functor Flt where
  fmap f (Flt c)  = Flt (\env st -> case c env st of
                                        Ok x st' -> Ok (f x) st')

instance Applicative Flt where
  pure  = return
  (<*>) = ap

instance Monad Flt where
  return x       = Flt (\env st -> Ok x st)
  (Flt c) >>= f = Flt (\env st -> case c env st of
                                      Ok x st' -> case f x of
                                                    Flt d -> d env st' )

instance HasUnique Flt where
  updateUnique f = Flt (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = Flt (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> Flt a -> Flt a
withEnv f (Flt c)
  = Flt (\env st -> c (f env) st)

--withUnique :: (Int -> (a,Int)) -> Flt a
--withUnique f
-- = Flt (\env st -> let (x,u') = f (uniq st) in Ok x (st{ uniq = u'}))

getEnv :: Flt Env
getEnv
  = Flt (\env st -> Ok env st)

updateSt :: (State -> State) -> Flt State
updateSt f
  = Flt (\env st -> Ok st (f st))

withCurrentDef :: Def -> Flt a -> Flt a
withCurrentDef def action
  = -- trace ("inl def: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $
    do -- traceDoc $ (\penv -> text "\ndefinition:" <+> prettyDef penv{Pretty.coreShowDef=True} def)
       action

traceDoc :: (Pretty.Env -> Doc) -> Flt ()
traceDoc f
  = do env <- getEnv
       fltTrace (show (f (prettyEnv env)))

fltTrace :: String -> Flt ()
fltTrace msg
  = do env <- getEnv
       trace ("open float: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen, Naoya Furudono
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
import Lib.PPrint
import Common.Failure
import Common.NamePrim ( nameEffectOpen )
import Common.Name
import Common.Unique

import Type.Type 
import Type.TypeVar
import Type.Kind
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Core.Core
import qualified Core.Core as Core
import Data.Maybe (fromJust)

trace s x =
  --  Lib.Trace.trace s
    x

debug =  -- set True to enable debug (e.g., Checking type invariant at all sub-expressions)
  True
  -- False

openFloat :: Pretty.Env -> Gamma -> CorePhase ()
openFloat penv gamma
  = liftCorePhaseUniq $ \uniq defs ->
      runFlt penv gamma uniq $ fltDefGroups defs


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
    -- XXX Is this assumption hold in Koka compiler? Ok in formal system but not sure about compiler.
    do (expr', _) <- fltExpr (defExpr def) (Just effectEmpty)  
       return def{ defExpr = expr' }


{--------------------------------------------------------------------------
  float expressions
--------------------------------------------------------------------------}

-- e | eff ~> e', rq
fltExpr :: Expr -> Maybe Effect -> Flt (Expr, Req)
fltExpr expr mbEff
  =
    -- pass the assigned effect to sub-expression
    let recurse e = fltExpr e mbEff in
    case expr of
    -- flt open[...](f)(args)  = flt f(args)
    App (App eopen@(TypeApp (Var open _) [effFrom,effTo,tpFrom,tpTo]) [f]) args | getName open == nameEffectOpen
      -> do recurse (App f args)
    App f args
      -> do (f', rqf) <- recurse f
            args_rq' <- mapM recurse args
            let
                (args', rqs) = unzip args_rq'
                feff = getEffectType f
                rqSup = sup (fromJust mbEff) $ Eff feff  : rqf : rqs
                frest = smartRestrictExpr rqf rqSup f'
                f'' = smartOpenExpr (Eff feff) rqSup frest
                args'' = map (\(e, rq)-> smartRestrictExpr rq rqSup e) args_rq'
            -- traceDoc $ \env -> text "app: " <+> niceType env (typeOf f'')
            return (assertTypeInvariant $ App f'' args'', rqSup)
            where
              getEffectType :: Expr -> Effect
              getEffectType e = case splitFunType $ typeOf e of
                Just(_, eff, _) -> eff
                Nothing -> failure $ "Core.OpenFloat/getEffectType: invalid input expr\nOperator must have function type.\nfound: " ++ show (typeOf e)
    Lam args eff body
      ->
        if not $ canbeEffective eff
          then 
            return (expr, Bottom)
          else
            do
                -- The source of effect, which is input of fltExpr
                -- Not recurse, but use the annotated effect!
                (body', rq) <- fltExpr body (Just eff)
                let
                  body'' = smartRestrictExpr rq (Eff eff) body'
                -- TODO `betterExpr` traverses both AST. We should optimize it. (cache or select at earlier phase)
                return (assertTypeInvariant $ betterExpr expr (Lam args eff body''), Bottom)

    -- TODO Refactor this case. Use foldr or something like that
    Let defgs@[defg] body ->
      do
          defgIR_rqs <- mapM (\dgs -> fltDefGroupAux dgs mbEff) defgs
          (body', rq) <- recurse body
          let (defgIRs, rqs) = unzip defgIR_rqs
              rqSup = sup (fromJust mbEff) $ rq:rqs
              body'' = smartRestrictExpr rq rqSup body'
          defgs' <- mapM (restrictToDG rqSup) defgIRs
          return (assertTypeInvariant $ Let defgs' body'', rqSup)
    Let [] body ->
      do
        (expr', rq) <- recurse body
        return (Let [] expr', rq)
    Let defgs body ->
        -- daan: this should be fold over the defgs ?
        -- expand because open calls in same defGroups cannot be merged in the middle of the defGroups
        recurse (expandLetExpr expr)
    Case exprs bs
      -> do exprIR_rqs <- mapM recurse exprs
            bIR_rqs <- mapM (\b -> fltBranch b mbEff) bs
            let rqe = sup (fromJust mbEff) $ map snd exprIR_rqs
                (bIRs, rqbs) = unzip bIR_rqs
                rqSup = sup (fromJust mbEff) $ rqe : rqbs
            exprs'' <- mapM (restrictToE rqSup) exprIR_rqs
            bs'' <- mapM (restrictToB rqSup) bIRs
            return (assertTypeInvariant $ Case exprs'' bs'', rqSup)

    -- type application and abstraction
    TypeLam tvars body
      -> do (body', rq) <- recurse body
            return (assertTypeInvariant $ TypeLam tvars body', Bottom)

    TypeApp body tps
      -> do (body', rq) <- recurse body
            return (assertTypeInvariant $ TypeApp body' tps, rq)

    -- the rest
    _ -> return (expr, Bottom)
  where
    assertTypeInvariant :: Expr -> Expr
    assertTypeInvariant expr' = if not debug then expr' else
      let tpBefore = typeOf expr
          tpAfter = typeOf expr' in
          assertion "Core/OpenFloat.fltExpr Type invariant violation." (matchType tpBefore tpAfter) expr'

    canbeEffective :: Effect -> Bool
    canbeEffective eff = 
      let (ls, tl) = extractHandledEffect eff in
      isEffectFixed tl || length ls > 2

    betterExpr :: Expr -> Expr -> Expr
    betterExpr exprOriginal@(Lam args1 eff1 body1) exprNew@(Lam args2 eff2 body2) =
      if countExpr body1 <= countExpr body2 then exprOriginal else exprNew
    betterExpr _ _ = failure "Core/OpenFloat.betterExpr Invalid functioncall"


fltBranch :: Branch -> Maybe Effect -> Flt (BranchIR, Req)
fltBranch (Branch pat guards) mbEff =
  do guard_rqs <- mapM (\b -> fltGuard b mbEff) guards
     let (guards', rqs) = unzip guard_rqs
     return (BranchIR pat guards', sup (fromJust mbEff) rqs)

fltGuard :: Guard -> Maybe Effect -> Flt (GuardIR, Req)
fltGuard (Guard guard body) mbEff =
  let recurse e = fltExpr e mbEff in
  do (guard', rqg) <- recurse guard
     (body', rqb)  <- recurse body
     return (GuardIR (guard', rqg) (body', rqb), sup (fromJust mbEff) [rqg, rqb])


-- daan: why are there separate Aux functions instead of using fltDefGroup?

fltDefGroupAux :: DefGroup -> Maybe Effect -> Flt (DefGroupIR, Req)
fltDefGroupAux (DefRec defs) mbEff =
  do defIR_rqs <- mapM (\d -> fltDefAux d mbEff) defs
     let (defIRs, rqs) = unzip defIR_rqs
     return (DefRecIR defIRs, sup (fromJust mbEff) rqs)
fltDefGroupAux (DefNonRec def) mbEff =
  do (defIR, rq) <- (\d -> fltDefAux d mbEff) def
     return (DefNonRecIR defIR, rq)

fltDefAux :: Def -> Maybe Effect -> Flt (DefIR, Req)
fltDefAux def@Def{defExpr=expr} mbEff =
  do (expr', rq) <- fltExpr expr mbEff
     return (DefIR def{defExpr=expr'} rq, rq)


-- daan: instead of splitting Let, should be instead fold over the groups in the fltExpr.Let case?
expandLetExpr :: Expr -> Expr
expandLetExpr expr = case expr of
  Let defgs body -> foldr (\d b -> Let [d] b) body defgs
  _ -> expr



{--------------------------------------------------------------------------
  Restrict
--------------------------------------------------------------------------}

restrictToD :: Req -> DefIR -> Flt Def
restrictToD rqSup (DefIR def@Def{defExpr=expr} rq)
  = return $ def{defExpr= smartRestrictExpr rq rqSup expr}

restrictToDG :: Req -> DefGroupIR -> Flt DefGroup
restrictToDG rqSup (DefRecIR defIRs) =
  do defs <- mapM (restrictToD rqSup) defIRs
     return $ DefRec defs
restrictToDG rqSup (DefNonRecIR defIR) =
  do def <- restrictToD rqSup defIR
     return $ DefNonRec def

restrictToE :: Req -> (Expr, Req) -> Flt Expr
restrictToE rqSup (e, rq)
  = return $ smartRestrictExpr rq rqSup e

restrictToB :: Req -> BranchIR -> Flt Branch
restrictToB rqSup (BranchIR pt gIRs) =
  do guards' <- mapM (restrictToG rqSup) gIRs
     return $ Branch pt guards'

restrictToG :: Req -> GuardIR -> Flt Guard
restrictToG rqSup (GuardIR t g) =
  do testExpr' <- restrictToE rqSup t
     guardExpr' <- restrictToE rqSup g
     return $ Guard testExpr' guardExpr'



{--------------------------------------------------------------------------
  Requirement
--------------------------------------------------------------------------}

data Req = Eff Effect | Bottom deriving (Eq, Show)

-- sup effUp rqs = sup { rq \in Req | rq <= (Eff effUp) and (rq' <= rq, forall rq' in rqs) }
--                 i.e., sup restricted on Req_{<= Eff effUp} $ rqs

-- Using effUp, we can disambiguate such case:
--     Eff effUp  other     
--          |\   / |
--          |  X   /
--          | / | /
--           o  o   : rqs

-- e.g., (`sup` [ < loc<h1> >, < loc<h2> > ])  < loc<h1>, ask, exn, loc<h2> >
--     = < loc<h1>, loc<h2> >  // NOT confused with < loc<h2>, loc<h1> >
sup :: Effect -> [Req] -> Req
sup effUpperBound rqs =
  let effs = map fromReq $ filter (/= Bottom) rqs in
  if null effs 
    then Bottom 
    else
  -- do we need to order eff here? can we assume input is ordered?
  Eff $ let (labsUpper, tlUpper) = extractOrderedEffect effUpperBound
            (labss {- ::[[Tau]] -}, tls)= unzip $ map extractOrderedEffect effs
            tl' = assertValidTail tlUpper $ foldl decideTail effectEmpty tls
            -- we can optimize it. change filter to specific one
            labs' = (`filter` labsUpper) (\l -> any (l `belongTo`) labss) in
        effectExtends labs' tl'
    where
      -- Assume list is ascending ordered for not too slow compilation
      belongTo :: Tau -> [Tau] -> Bool
      belongTo _ [] = False
      belongTo l (l1:ls) = case labelCompare l l1 of
                             EQ -> True
                             GT -> belongTo l ls
                             LT -> False
      decideTail :: Effect-> Effect -> Effect
      decideTail tl1 tl2 = case (isEffectEmpty tl1, isEffectEmpty tl2) of
        (True, _) -> tl2
        (_, True) -> tl1
        _ | tl1 == tl2 -> tl1
        _ -> failure "Core/OpenFloat.sup: Unrelated pair of tail found in candidates"
      -- TODO: Introduce Flt monad and show pretty types
      assertValidTail :: Effect -> Effect -> Effect
      assertValidTail upper actual = 
        if not debug || isEffectEmpty actual || actual == upper
          then actual 
          else failure $ "Core/OpenFloat.sup: decided tail of the row is not smaller or equal to given upper bound\n" ++ (show upper) ++ "\n" ++ (show actual)

fromReq :: Req -> Effect
fromReq Bottom = failure "Core/OpenFloat.fromReq: Bottom"
fromReq (Eff eff) = eff

matchEffect :: Effect -> Effect -> Bool
matchEffect eff1 eff2 = matchType (orderEffect eff1) (orderEffect eff2)

{--------------------------------------------------------------------------
  smart open
--------------------------------------------------------------------------}

-- How should I define restrict?
smartRestrictExpr :: Req -> Req -> Expr -> Expr
smartRestrictExpr Bottom _ expr     = expr
smartRestrictExpr (Eff _) Bottom _  = failure "Core.OpenFloat.smartRestrictExpr: unexpected Bottom?"
smartRestrictExpr (Eff effFrom) (Eff effTo) expr =
  if matchEffect effFrom effTo
    then expr
    else let tp = typeOf expr
         in App (openEffectExpr effFrom effTo (TFun [] effFrom tp) (TFun [] effTo tp) (Lam [] effFrom expr)) []

smartOpenExpr :: Req -> Req -> Expr -> Expr
smartOpenExpr Bottom _ e = e
smartOpenExpr (Eff _) Bottom _ = undefined
smartOpenExpr (Eff effFrom) (Eff effTo) expr =
  if matchEffect effFrom effTo then expr else
    let tp@(TFun tpPar feff tpRes)= typeOf expr
        in
          assertion "smart open check" (matchEffect effFrom feff) $
          openEffectExpr effFrom effTo tp (TFun tpPar effTo tpRes) expr

{--------------------------------------------------------------------------
  IRs : Return type of auxiliary functions. Each Expr of constructs has its own rq, in order to be restricted.
--------------------------------------------------------------------------}

data BranchIR = BranchIR { branchPatterns :: [Pattern]
                         , branchGuardIRs :: [GuardIR]}
data GuardIR = GuardIR { guardTestIR :: (Expr, Req)
                       , guardExprIR :: (Expr, Req)}
data DefIR = DefIR { def :: Def, req :: Req}
data DefGroupIR = DefRecIR [DefIR] | DefNonRecIR DefIR

{--------------------------------------------------------------------------
  Count opens: This utility is used to select to apply floating or not.
   Open floating is effective only if floating up 
    open calls which opens from effects of two or more length to longer one.
--------------------------------------------------------------------------}

countExpr :: Expr -> Int
-- open-app
countExpr (App (App eopen@(TypeApp (Var open _) [effFrom,effTo,tpFrom,tpTo]) [f]) args) | getName open == nameEffectOpen =
  countExpr (App f args) + (if effectLength effFrom >= 2 then 1 else 0)
countExpr (App f args) = countExpr f + mapsum countExpr args
countExpr (Lam args eff body) = countExpr body
countExpr (Let defgs body) = countDefGroups defgs + countExpr body
countExpr (Case exprs bs) = mapsum countExpr exprs + mapsum countBranch bs
countExpr (TypeLam tvars body) = countExpr body
countExpr (TypeApp body tps) = countExpr body
countExpr _ = 0

countDefGroups :: DefGroups -> Int
countDefGroups defgs = mapsum countDefGroup defgs

countDefGroup  :: DefGroup -> Int
countDefGroup (DefRec defs) = mapsum countDef defs
countDefGroup (DefNonRec def) = countDef def

countDef :: Def -> Int
countDef = countExpr . defExpr

countBranch :: Branch -> Int
countBranch (Branch pat guards) = mapsum countGuard guards

countGuard (Guard guard body) = countExpr guard + countExpr body

effectLength :: Effect -> Int
effectLength eff = 
      let (ls, tl) = extractHandledEffect eff in
        length ls

mapsum :: (a -> Int) -> [a] -> Int
mapsum f lst = foldl (\acc x -> f x + acc) 0 lst

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

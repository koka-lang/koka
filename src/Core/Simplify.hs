-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-    Core simplification
-}
-----------------------------------------------------------------------------

module Core.Simplify (simplify, uniqueSimplify, simplifyDefs) where

import Data.List
import Control.Monad
import Control.Applicative
import Lib.Trace
import Lib.PPrint
import Common.Failure
import Common.Range
import Common.Syntax
import Common.NamePrim( nameEffectOpen, nameToAny, nameReturn, nameOptionalNone, nameIsValidK
                       , nameLift, nameBind, nameEvvIndex, nameClauseTailNoYield, isClauseTailName
                       , nameBox, nameUnbox, nameAssert
                       , nameAnd, nameOr, isNameTuple
                       , nameCCtxCompose, nameCCtxComposeExtend, nameCCtxEmpty )

import Common.Unique
import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty as Pretty
import Core.Core
import Core.Pretty
import Core.CoreVar
import Core.Uniquefy( uniquefyExpr )
import qualified Common.NameMap as M
import qualified Data.Set as S

-- data Env = Env{ inlineMap :: M.NameMap Expr }
-- data Info = Info{ occurrences :: M.NameMap Int }

simplifyDefs :: Pretty.Env -> Bool -> Bool -> Int -> Int -> CorePhase ()
simplifyDefs penv unsafe ndebug nRuns duplicationMax
  = liftCorePhaseUniq $ \uniq defs ->
    runSimplify unsafe ndebug duplicationMax uniq penv (simplifyN nRuns (uniquefyDefBodies defs))

-- simplifyN :: Simplify a => Int -> a -> Simp a
simplifyN nRuns defs
  = if (nRuns <= 0) then return defs
    else do defs' <- simplify defs
            simplifyN (nRuns-1) defs'

uniqueSimplify :: (HasUnique m, Simplify a) => Pretty.Env -> Bool -> Bool -> Int -> Int -> a -> m a
uniqueSimplify penv unsafe ndebug nRuns duplicationMax expr
  = do u <- unique
       let (x,u') = runSimplify unsafe ndebug duplicationMax u penv 
                     (simplifyN nRuns expr) -- (simplify expr) -- (simplifyN (if nRuns <= 0 then 1 else nRuns) expr)
       setUnique u'
       return x


uniquefyDefBodies :: [DefGroup] -> [DefGroup]
uniquefyDefBodies dgs  = map uniquefyDefGroupBody dgs

uniquefyDefGroupBody :: DefGroup -> DefGroup
uniquefyDefGroupBody (DefRec defs) = DefRec (map uniquefyDefBody defs)
uniquefyDefGroupBody (DefNonRec def) = DefNonRec (uniquefyDefBody def)

uniquefyDefBody def = def{ defExpr = uniquefyExpr (defExpr def) }


class Simplify a where
  simplify :: a -> Simp a

{--------------------------------------------------------------------------
  -down optimizations

  These optimizations must be careful to call simplify recursively
  when necessary.
--------------------------------------------------------------------------}

topDown :: Expr -> Simp Expr

-- Inline simple let-definitions
topDown (Let dgs body)
  = topDownLet [] [] dgs body
  where
    subst sub expr
      = if null sub then expr else (sub |~> expr)
    extend :: (TName,Expr) -> [(TName,Expr)] -> [(TName,Expr)]
    extend (name,e) sub
      = (name,e):sub

    topDownLet :: [(TName,Expr)] -> [DefGroup] -> [DefGroup] -> Expr -> Simp Expr
    topDownLet sub acc [] body
      = case subst sub body of
          Let sdgs sbody -> topDownLet [] acc sdgs sbody  -- merge nested Let's
          sbody -> if (null acc)
                    then return sbody
                    else return $ Let (reverse acc) sbody

    topDownLet sub acc [DefNonRec (Def{defName=name,defType=tp,defExpr=e})] (Var v _) | getName v == name
      = topDownLet sub acc [] e

    topDownLet sub acc (DefNonRec def@(Def{defName=x,defType=tp,defExpr=letexpr@(Let dgs' body')}) : dgs) body  
      = -- lift nested let bindings
        assertion "Core.Simplify.topDownLet.liftLets" (bv dgs' `tnamesDisjoint` fv body) $ -- due to uniquefy at start of simplify
        topDownLet sub acc (dgs' ++ [DefNonRec def{defExpr = body'}] ++ dgs) body

    topDownLet sub acc (dg:dgs) body
      = let sdg = subst sub dg
        in case sdg of
          DefRec [def]
            -> -- trace ("don't simplify recursive lets: " ++ show (map defName defs)) $
               if (occursNot (defName def) (Let dgs body))
                then -- trace ("simplify dead: " ++ show (defName def)) $
                     topDownLet sub (acc) dgs body -- dead definition
                else -- trace ("no simplify rec def: " ++ show (defName def)) $
                     topDownLet sub (sdg:acc) dgs body -- don't inline recursive ones
          DefRec defs'
            -> -- trace ("don't simplify recursive lets: " ++ show (map defName defs)) $
               topDownLet sub (sdg:acc) dgs body -- don't inline recursive ones

          DefNonRec def@(Def{defName=x,defType=tp,defExpr=se})  | not (isTotal se)
            -> -- cannot inline effectful expressions
               topDownLet sub (sdg:acc) dgs body
          DefNonRec def@(Def{defName=x,defType=tp,defExpr=se})  -- isTotal se
             -> -- trace ("simplify let: " ++ show x ++ " = " ++ show se) $
                do maxSmallOccur <- getDuplicationMax  -- should be 10 or higher to inline partial bind applications
                   let inlineExpr = topDownLet (extend (TName x tp, se) sub) acc dgs body
                   case occurrencesOf x (Let dgs body) of
                     -- no occurrence, disregard
                     Occur 0 m n 0
                       -> -- trace "no occurrence" $
                          topDownLet sub (acc) dgs body
                     -- occurs once, always inline (TODO: maybe only if it is not very big?)
                     Occur acnt m n vcnt | vcnt + acnt == 1
                       -> -- trace "occurs once: inline" $
                          inlineExpr
                     -- occurs fully applied, check if it small enough to inline anyways;
                     -- as it is a function, make it expensive to inline partial applications to avoid too much duplication
                     Occur acnt m n vcnt  | ((acnt + vcnt*3) * sizeOfExpr se) <= maxSmallOccur
                       -> -- trace "occurs as cheap function: inline" $
                          inlineExpr
                     -- occurs multiple times as variable, check if it small enough to inline anyways
                     Occur 0 m n vcnt | (vcnt * sizeOfExpr se) <= maxSmallOccur
                       -> -- trace "occurs as cheap value: inline" $
                          inlineExpr
                     -- inline total and very small expressions
                     Many n | (n*sizeOfExpr se) <= maxSmallOccur
                       -> -- trace "occurs many as cheap value: inline" $
                          inlineExpr

                     -- dont inline
                     oc -> -- trace ("no inline: occurrences: " ++ show oc ++ ", size: " ++ show (sizeOfExpr se)) $
                           topDownLet sub (sdg:acc) dgs body


-- Remove assertions if optimized
topDown (App assert@(Var name _) [msg,cond])  | getName name == nameAssert
  =  do ndebug <- getNDebug
        if (ndebug)
          then return exprUnit
          else return (App assert [msg,cond])

-- Remove identity open applications; need to be done before open resolve to enable tail call optimization               
topDown expr@(App app@(TypeApp (Var openName _) [effFrom,effTo,tpFrom,tpTo]) [arg])
  | getName openName == nameEffectOpen &&
    (hasNoEffectExpr arg ||                -- arg uses no effects, or, the open is an identity
      (matchType tlFrom tlTo && length lsFrom == length lsTo && and [matchType t1 t2 | (t1,t2) <- zip lsFrom lsTo]))
  = return arg
  where
    (lsFrom,tlFrom) = extractHandledEffect effFrom
    (lsTo,tlTo)     = extractHandledEffect effTo
    hasNoEffectExpr expr
      = case expr of
          TypeApp e targs -> hasNoEffectExpr e
          Lit{} -> True
          Con{} -> True
          -- Var _ InfoExternal{} -> True  -- TODO: maybe too liberal?
          _     -> False

-- Remove effect open applications; only if 'unsafe' is enabled since
-- the effect types won't match up
topDown expr@(App app@(TypeApp (Var openName _) _) [arg])  | getName openName == nameEffectOpen
  = do unsafe <- getUnsafe
       if (unsafe)
        then return arg
        else do return (App app [arg])

-- Remove identity externals of the form "#1"; only if 'unsafe' is enabled since
-- usually the effect types won't match up
topDown expr@(App app@(TypeApp (Var _ (InfoExternal [(Default,"#1")])) _) [arg])
  = do unsafe <- getUnsafe
       if (unsafe)
        then return arg
        else do return (App app [arg])

-- Direct function applications
topDown expr@(App (Lam pars eff body) args) | length pars == length args
  = do newNames <- mapM uniqueTName pars
       let sub = [(p,Var np InfoNone) | (p,np) <- zip pars newNames]
           expr' = makeLet (zipWith makeDef newNames args) (sub |~> body)
       return expr'
  where
    makeDef (TName npar nparTp) arg
      = DefNonRec (Def npar nparTp arg Private DefVal InlineAuto rangeNull "")


-- Direct function applications
topDown expr@(App (TypeApp (TypeLam tpars (Lam pars eff body)) targs) args) 
  | length pars == length args && length tpars == length targs
  = do let tsub    = subNew (zip tpars targs)
       newNames <- -- trace ("topDown function app: " ++ show (zip tpars targs) ++ "\n expr:" ++ show expr) $
                   mapM uniqueTName [TName nm (tsub |-> tp) | (TName nm tp) <- pars]
       let sub     = [(p,Var np InfoNone) | (p,np) <- zip pars newNames]        
       return (Let (zipWith makeDef newNames args) (sub |~> (substitute tsub body)))  
  where
    makeDef (TName npar nparTp) arg
      = DefNonRec (Def npar nparTp arg Private DefVal InlineAuto rangeNull "")


-- case-of-let
topDown (Case [Let dgs expr] branches) 
  = assertion "Core.Simplify.topDown.Case-Of-Let" (bv dgs `tnamesDisjoint` fv branches) $
    return (Let dgs (Case [expr] branches))

-- case-of-case: currently makes reuse worse in some cases (like bench/koka/rbtree)
topDown (Case [Case scruts0 branches0] branches1) | doesNotDuplicate 
  = return (Case scruts0 (map (pushCase branches1) branches0))
  where
    pushCase branches (Branch pats guards) 
      = Branch pats (map (pushCaseG branches) guards)
    pushCaseG branches (Guard guard expr)
      = Guard guard (Case [expr] branches)

    doesNotDuplicate 
      = hasSingleBranch branches0 || 
        length (filter (simplifiesOn branches1) branches0) + 1 >= length branches0

    hasSingleBranch :: [Branch] -> Bool
    hasSingleBranch branches
      = case branches of
          [Branch pats [guard]] -> True
          _                     -> False

    simplifiesOn :: [Branch] -> Branch -> Bool
    simplifiesOn branches (Branch pats guards)
      = all (simplifiesOnG branches) guards

    simplifiesOnG :: [Branch] -> Guard -> Bool
    simplifiesOnG branches (Guard guard expr)
      = simplifiesOnE branches expr

    simplifiesOnE :: [Branch] -> Expr -> Bool
    simplifiesOnE branches expr
      = case expr of 
          Let dgs body -> simplifiesOnE branches body
          _ -> case kmatchBranches [expr] branches of
                 Just _ -> True
                 _      -> False

-- App of case to case of app's
topDown expr@(App (Case scruts branches) args)
  = do (sbinders,bscruts) <- bindExprs scruts
       (binders,bargs)    <- bindExprs args
       return $ sbinders (binders (Case bscruts (map (makeBranchApp bargs) branches)))
  where
    makeBranchApp bargs (Branch pats guards)  = Branch pats (map (makeGuardApp bargs) guards)
    makeGuardApp bargs (Guard test body)      = Guard test (App body bargs)


-- TypeApp of let
topDown (TypeApp (Let binds expr) targs)
  = return (Let binds (TypeApp expr targs))

-- Float let out of applications (important for TRMC)
topDown (App f args)
  = return (floatLetApp f args)
  where
    floatLetApp f args
      = let (fbinds,fexpr)   = floatLet f
            (abindss,aexprs) = unzip (map floatLet args)
        in makeLet (concat (fbinds:abindss)) (App fexpr aexprs)  -- note: assume unique bindings!

    floatLet (Let binds body)     = let (binds',body') = floatLet body in (binds ++ binds', body')
    floatLet expr                 = ([],expr)


-- No optimization applies
topDown expr
  = do -- traceS ("no topdown match: " ++ show expr) 
       return expr


-- normalize evaluation by binding sub expressions
bindExprs :: [Expr] -> Simp (Expr -> Expr, [Expr])
bindExprs exprs
  = do (defss,bexprs) <- unzip <$> mapM uniqueBind exprs
       return (makeLet (concat defss), bexprs)
  where
    uniqueBind expr
      = case expr of 
          Var{} -> return ([], expr)
          Lit{} -> return ([], expr)
          Con{} -> return ([], expr)
          _     -> do name <- uniqueName "norm"
                      let tname = TName name (typeOf expr)
                      return ([DefNonRec (makeTDef tname expr)], Var tname InfoNone)


{--------------------------------------------------------------------------
  Bottom-up optimizations

  These optimizations can assume their children have already been simplified.
--------------------------------------------------------------------------}

bottomUp :: Expr -> Expr

-- replace "(/\a. body) t1" with "body[a |-> t1]"
bottomUp expr@(TypeApp (TypeLam tvs body) tps)
  = if (length tvs == length tps)
     then let sub = subNew (zip tvs tps)
          in -- trace ("bottomUp type app: " ++ show (zip tvs tps) ++ "\n  expr: " ++ show expr) $
             seq sub (sub |-> body)
     else expr

-- eta-contract "/\a. (body a)" to "body"
bottomUp expr@(TypeLam tvs (TypeApp body tps))
  = if (length tvs == length tps && all varEqual (zip tvs tps) && all (\tv -> not (tvsMember tv (ftv body))) tvs)
     then body
     else expr
  where
    varEqual (tv,TVar tw) = tv == tw
    varEqual _            = False


-- Direct function applications with arguments that have different free variables than the parameters
bottomUp (App (Lam pars eff body) args) | length pars == length args  && all free pars
  = Let (zipWith makeDef pars args) body
  where
    makeDef (TName npar nparTp) arg
      = DefNonRec (Def npar nparTp arg Private DefVal InlineAuto rangeNull "")

    free parName
      = not (parName `S.member` fv args)


-- bind( lift(arg), cont ) -> cont(arg)
bottomUp (App (TypeApp (Var bind _) _) [App (TypeApp (Var lift _) _) [arg], cont]) | getName bind == nameBind && getName lift == nameLift
  = App cont [arg]


-- composition extension: c[ctx hole] -> c
bottomUp (App (TypeApp (Var cextend _) _) [ctx1, App (TypeApp (Var cempty _) _) []]) | getName cextend == nameCCtxComposeExtend && getName cempty == nameCCtxEmpty
  = ctx1

-- context composition: c ++ ctx _  == c  == ctx _ ++ c
bottomUp (App (TypeApp (Var ctxcomp _) _) [ctx1, App (TypeApp (Var cempty _) _) []]) | getName ctxcomp == nameCCtxCompose && getName cempty == nameCCtxEmpty
  = ctx1

bottomUp (App (TypeApp (Var ctxcomp _) _) [App (TypeApp (Var cempty _) _) [],ctx2]) | getName ctxcomp == nameCCtxCompose && getName cempty == nameCCtxEmpty
  = ctx2


-- continuation validation
bottomUp expr@(App (TypeApp (Var isValidK _) _) [arg])  | getName isValidK == nameIsValidK
  = case arg of
      Var optNone _  | getName optNone == nameOptionalNone  -> exprFalse
      Lam _ _ _ -> exprTrue
      App _ _ -> exprTrue
      _ -> expr

     
-- case on a single constructor, including tuples.
-- extracts the arguments to do a direct multi-pattern match
bottomUp expr@(Case [App (TypeApp (Con name ConSingle{}) targs) args] branches)  
  | length (branchPatterns (head branches)) == 1 && all (isMatchOnCon name (length args)) branches
  = Case args (map (extractMatchOnCon (length args)) branches)

bottomUp expr@(Case [App (Con name ConSingle{}) args] branches)  
  | length (branchPatterns (head branches)) == 1 && all (isMatchOnCon name (length args)) branches
  = Case args (map (extractMatchOnCon (length args)) branches)


-- lift common continuation; often generated by cps (`test/algeff/cps-cgen1`)
-- match(e) {               f( match(e) { 
--   b1 -> f(e1)                 b1 -> e1
--   ...                ~>       ...
--   bn -> f(en)                 bn -> en
-- }                           })
bottomUp expr@(Case scruts bs)  | commonContinue
  = -- trace "case bottomUp 3 " $ 
    case mbCont of
      Nothing           -> expr -- cannot happen
      Just (common,cbs) -> App common [bottomUp (Case scruts cbs)]  -- add bottomUp for potential case of known
  where
    commonContinue = case mbCont of
                       Nothing -> False
                       Just _  -> True

    mbCont = findCommonCont bs

    findCommonCont bs
      = case bs of
          (Branch pat (Guard _ (App v@(Var name _) [_]) : _) : _)  | not (S.member name (bv pat))
            -> extractCommonCont v name bs
          _ -> Nothing

    extractCommonCont contVar contName bs
      = case flattenJust [] (map extract bs) of
          Nothing   -> Nothing
          Just(cbs) -> Just (contVar,cbs)
      where
        extract (Branch pats guards)
          = case flattenJust [] (map extractG guards) of
              Nothing -> Nothing
              Just guards' -> Just (Branch pats guards')

        extractG guard
          = case guard of
              (Guard guards (App (Var name _) [arg]))  | name == contName  -> Just (Guard guards arg)
              _ -> Nothing

    flattenJust acc (Nothing:xs)  = Nothing
    flattenJust acc (Just x:xs)   = flattenJust (x:acc) xs
    flattenJust acc  []           = Just (reverse acc)


-- generic case of known constructor
bottomUp expr@(Case scruts branches)
  = -- trace "bottomUp generic case of known " $
    case kmatchBranches scruts branches of
      Just b -> b
      _      -> expr


-- simplify evv-index(l) to i if l has a known offset
bottomUp (App (TypeApp (Var evvIndex _) [effTp,hndTp]) [htag]) | getName evvIndex == nameEvvIndex && isEffectFixed effTp
  = makeEvIndex (effectOffset (effectLabelFromHandler hndTp) effTp)


-- simplify clause-tailN to clause-tail-noyieldN if it cannot yield
bottomUp (App (TypeApp (Var clauseTail info) (effTp:tps)) [op]) | Just n <- isClauseTailName (getName clauseTail), ([],_) <- extractHandledEffect effTp
  = (App (TypeApp (Var (TName (nameClauseTailNoYield n) (typeOf clauseTail)) info) (effTp:tps)) [op])

-- box(unbox(e)) ~> e   unbox(box(e)) ~> e
bottomUp (App (Var v _) [App (Var w _) [arg]])  | (getName v == nameUnbox && getName w == nameBox) || (getName w == nameUnbox && getName v == nameBox)
  = arg


-- direct application of arguments to a lambda: fun(x1...xn) { f(x1,...,xn) }  -> f
bottomUp (Lam pars eff (App f@(Var _ info) args))   | notExternal && length pars == length args && argsMatchPars 
  = f
  where
    argsMatchPars = and (zipWith argMatchPar pars args)
    argMatchPar par (Var name _)  = par == name
    argMatchPar _    _            = False

    notExternal = case info of
                    InfoExternal{} -> False
                    _ -> True


-- return immediately from a lambda
bottomUp (Lam pars eff (App (Var ret _) [arg]))  | getName ret == nameReturn
  = Lam pars eff arg

-- No optimization applies
bottomUp expr
  = -- trace ("no bottomup match: " ++ show expr) 
    expr


----------------------------------------------------------------
-- match known constructors
----------------------------------------------------------------

data Match a = Match a | Unknown | NoMatch
             deriving(Show)

isNoMatch NoMatch = True
isNoMatch _       = False

instance Functor Match where
  fmap f (Match x) = Match (f x)
  fmap f NoMatch   = NoMatch
  fmap f Unknown   = Unknown
   

instance Applicative Match where
  pure x      = Match x
  m1 <*> m2   = case m1 of 
                  Match f -> fmap f m2
                  Unknown -> Unknown
                  NoMatch -> NoMatch

instance Monad Match where
  m >>= f   = case m of
                Match x -> f x
                Unknown -> Unknown
                NoMatch -> NoMatch
  

matchFirst :: [Match a] -> Match a
matchFirst (NoMatch : matches) = matchFirst matches
matchFirst (other : matches)   = other
matchFirst []                  = NoMatch

matchAll :: [Match a] -> Match [a]
matchAll []                   = Match []
matchAll (Match x : matches)  = fmap (x:) (matchAll matches)
matchAll (NoMatch : matches)  = NoMatch
matchAll (Unknown : matches)  = if (any isNoMatch matches) then NoMatch else Unknown

--For each branch and check if an exact match is found and return the expr as is, else move onto the next branch
kmatchBranches :: [Expr] -> [Branch] -> Maybe Expr
kmatchBranches scruts branches
  = let matches = map (kmatchBranch scruts) branches
    in case matchFirst matches of
        Match expr -> Just (uniquefyExpr expr)
        _          -> Nothing

-- For every branch, compare all its pats with the scruts and if they all match, return the matched bindings+scruts combined with the branch body
kmatchBranch :: [Expr] -> Branch -> Match Expr
kmatchBranch scruts branch@(Branch pats guards) 
  = --trace ("kmatchBranch start: " ++ show scruts ++ " ___branch___ " ++ show branch) $
    do bindings <- kmatchPatterns scruts pats 
       expr     <- matchFirst (map matchGuard guards) 
       Match (Let (map DefNonRec bindings) expr)

  where
    matchGuard (Guard guard expr)
      = if (isExprTrue guard) then Match expr
        else if (isExprFalse guard) then NoMatch
        else Unknown       


-- For every scrut - pat tuple, get the binding and new scrutinee and collect them into a single expr
kmatchPatterns :: [Expr] -> [Pattern] -> Match Defs
kmatchPatterns scruts pats
  = do ds <- matchAll (zipWith kmatchPattern scruts pats) 
       Match (concatMap (\(defs,newscrut) -> defs ++ [makeDef (newHiddenName "scrut") newscrut]) ds)

-- Returns the bindings and modified scrutinee if the scrutinee and the pattern match
kmatchPattern :: Expr -> Pattern -> Match (Defs, Expr)
kmatchPattern scrut PatWild
  = Match ([], scrut)

kmatchPattern scrut (PatVar name pat)
 = --trace ("kmatchPat PatVar " ++ show scrut ++ " ___pat___ " ++ show (PatVar name pat)) $
   do (defs,newscrut) <- kmatchPattern scrut pat 
      Match (defs ++ [makeDef (getName name) newscrut], Var name InfoNone)
    
kmatchPattern scrut@(Lit lit) (PatLit pLit)
  = --trace "kmatchPat PatLit " $
    if lit /= pLit then NoMatch else Match ([], scrut) 

kmatchPattern scrut@(Con name _repr) (PatCon pname [] _prepr _ _ _ _info _)
  = --trace ("kmatchPat PatCon empty pats " ++ show name ++ " ___pat___ " ++ show pname) $
    if name /= pname then NoMatch else
      --trace ("kmatchPat PatCon empty pats match " ++ show name) $
      Match ([], scrut)
      
kmatchPattern scrut@(App con@(Con name conRepr) args) (PatCon pname pats _ _ _ _ _ _)
  = --trace "kmatchPat PatCon non empty pats " $
    if name /= pname then NoMatch else
      do ds <- matchAll (zipWith kmatchPattern args pats) 
         let (defs,scruts) = unzip ds
         Match (concat defs, App con scruts)
      
kmatchPattern scrut@(App con@(TypeApp (Con name conRepr) targs) args) (PatCon pname pats _ _ _ _ _ _)
  = --trace "kmatchPat PatCon non empty pats " $
    if name /= pname then NoMatch else
      do ds <- matchAll (zipWith kmatchPattern args pats)
         let (defs,scruts) = unzip ds
         Match (concat defs, App con scruts)

kmatchPattern (Let letDefns letBody) pat
  = --trace ("kmatchPat Let scrut " ++ show letDefns ++ " ____pat____ " ++ show letBody) $
    do (bindings,newscrut) <- kmatchPattern letBody pat 
       Match (flattenDefGroups letDefns ++ bindings, newscrut)

kmatchPattern scrut pat
  = --trace ("kmatchPat Unknown " ++ show scrut ++ " ___pat___ " ++ show pat) $
    Unknown


{--------------------------------------------------------------------------
  optimization for match on ConSingle (like tuples)
--------------------------------------------------------------------------}

isMatchOnCon name n branch
  = case branchPatterns branch of
      [PatCon{patConName=cname,patConPatterns=pats}] 
                     -> -- trace ("noMatchOnCon: " ++ show cname ++ ": " ++ show (n,length pats)) $ 
                        (cname == name) && (length pats == n)
      [PatVar v pat] | not (tnamesMember v (freeLocals (branchGuards branch))) 
                     -> isMatchOnCon name n (branch{ branchPatterns=[pat]})
      [PatVar v pat] -> False -- TODO: we can potentially allow it if v `notin` fv(guards) and we generate a binding for it                       
      [PatWild]      -> True
      [PatLit _]     -> False
      _              -> False -- trace ("noMatchOnCon: " ++ show name ++ ": " ++ show branch) $ False

extractMatchOnCon n (Branch [PatCon{patConPatterns=pats}] guards) = Branch pats guards
extractMatchOnCon n (Branch [PatWild] guards)                     = Branch [PatWild | _ <- [1..n]] guards
extractMatchOnCon n (Branch [PatVar _ pat] guards)                = extractMatchOnCon n (Branch [pat] guards) -- name is free ! (due to isMatchOnCon)
extractMatchOnCon n (Branch patterns guards) = failure $ "Core.Simplify.bottomUp.Case.Singleton: invalid pattern: " ++ show patterns
  



{--------------------------------------------------------------------------
  Effects
--------------------------------------------------------------------------}

-- effectOffset (effectTypeFromHandler hndTp) effTp)
effectLabelFromHandler :: Type -> Name
effectLabelFromHandler tp
  = fromHandlerName (labelName tp)

effectOffset :: Name -> Type -> Integer
effectOffset l effTp
  = let (ls,_) = extractHandledEffect effTp
        ofs = findMatch 0 l ls
    in -- trace ("found offset " ++ show ofs ++ " for " ++ show l ++ " in " ++ show (map (show . labelName) ls)) $    
       ofs

findMatch :: Integer -> Name -> [Type] -> Integer
findMatch i lname (l:ls)  = if (labelName l == lname) then i else findMatch (i+1) lname ls
findMatch i lname []      = failure $ "Core.Simplify.findMatch: label " ++ show lname ++ " is not in the labels"



{--------------------------------------------------------------------------
  Definitions
--------------------------------------------------------------------------}

instance Simplify DefGroup where
  simplify (DefRec    defs) = fmap DefRec (mapM simplify defs)
  simplify (DefNonRec def ) = fmap DefNonRec (simplify def)

instance Simplify Def where
  simplify def@(Def name tp expr vis sort inl nameRng doc)
    = -- trace ("simplifying " ++ show name ) $ -- ++ ": " ++ show expr) $
      do expr' <- inDef name $
                  case expr of
                    TypeLam tvs (Lam pars eff body)
                      -> do body' <- simplify body
                            return $ TypeLam tvs (Lam pars eff body')
                    Lam pars eff body
                      -> do body' <- simplify body
                            return $ Lam pars eff body'
                    _ -> simplify expr
         return $ Def name tp expr' vis sort inl nameRng doc

instance Simplify a => Simplify [a] where
  simplify  = mapM simplify

{--------------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------------}

instance Simplify Expr where
  simplify e
    = do -- traceS ("simplify: " ++ show e)
         td <- topDown e
         se <- case td of
                Lam pars eff expr
                  -> do x <- simplify expr
                        return (Lam pars eff x)
                Var tname info
                  -> return td
                App f args
                  -> do f'    <- simplify f
                        args' <- simplify args
                        return $ App f' args'
                TypeLam tpars expr
                  -> do x <- simplify expr
                        return (TypeLam tpars x)
                TypeApp expr tps
                  -> do x <- simplify expr
                        return (TypeApp x tps)
                Con tname repr
                  -> return td
                Lit lit
                  -> return td
                Let defGroups expr
                  -> do dgs <- simplify defGroups
                        x   <- simplify expr
                        return $ Let dgs x
                Case exprs branches
                  -> do xs <- simplify exprs
                        bs <- simplify branches
                        return $ Case xs bs
         -- bottomUpM e'
         let bu = bottomUp se
         -- traceS ("simplified:\n  " ++ show e ++ "\n  to:\n  " ++ show bu)
         return bu

instance Simplify Branch where
  simplify (Branch patterns guards)
    = fmap (Branch patterns) (mapM simplify guards)

instance Simplify Guard where
  simplify (Guard test expr)
    = do xt <- simplify test
         xe <- simplify expr
         return $ Guard xt xe


{--------------------------------------------------------------------------
  Occurrences
--------------------------------------------------------------------------}

-- for the size of an expression, we count nodes but also consider
-- any expression that could potentially allocate as large 
sizeOfExpr :: Expr -> Int
sizeOfExpr expr
  = case expr of
      Var tname info     -> 0
      Con tname repr     -> 0
      Lit lit            -> 0
      Lam tname eff body -> 1 + sizeOfExprX body
      App e args         -> 1 + sizeOfFun e + sum (map sizeOfExpr args)
      TypeLam tvs e      -> sizeOfExpr e
      TypeApp e tps      -> sizeOfExpr e
      Let defGroups body -> sum (map sizeOfDefGroup defGroups) + (sizeOfExpr body)
      Case exprs branches -> 1 + sum (map sizeOfExpr exprs) + sum (map sizeOfBranch branches)
  where
    sizeOfBranch (Branch patterns guards)
      = sum (map sizeOfGuard guards)

    sizeOfGuard (Guard test expr)
      = sizeOfExpr test + sizeOfExpr expr

    sizeOfLocalDef :: Def -> Int
    sizeOfLocalDef def
      = sizeOfExpr (defExpr def)

    sizeOfDefGroup dg
      = case dg of
          DefRec defs   -> sum (map sizeOfLocalDef defs)
          DefNonRec def -> sizeOfLocalDef def


sizeOfFun :: Expr -> Int   -- for functions we must be conservative or we could lose sharing from allocations 
sizeOfFun expr 
  = case expr of
      Con _ _    -> maxSize
      Var _ _    -> 1
      Lit _      -> 0  -- cannot happen?
      Lam tname eff body -> 1 + sizeOfExpr body
      App e args         -> 1 + sizeOfFun e + sum (map sizeOfExpr args)
      TypeLam tvs e      -> sizeOfFun e
      TypeApp e tps      -> sizeOfFun e
      _          -> maxSize  -- give up
  where
    maxSize = 10000

sizeOfExprX expr
  = case expr of
      Var tname info     -> 0
      Con tname repr     -> 0
      Lit lit            -> 0
      Lam tname eff body -> 1 + sizeOfExprX body
      App e args         -> 1 + sizeOfExprX e + sum (map sizeOfExprX args)
      TypeLam tvs e      -> sizeOfExprX e
      TypeApp e tps      -> sizeOfExprX e
      Let defGroups body -> sum (map sizeOfDefGroup defGroups) + (sizeOfExprX body)
      Case exprs branches -> 1 + sum (map sizeOfExprX exprs) + sum (map sizeOfBranch branches)
  where
    sizeOfBranch (Branch patterns guards)
      = sum (map sizeOfGuard guards)

    sizeOfGuard (Guard test expr)
      = sizeOfExprX test + sizeOfExprX expr

    sizeOfLocalDef :: Def -> Int
    sizeOfLocalDef def
      = sizeOfExprX (defExpr def)

    sizeOfDefGroup dg
      = case dg of
          DefRec defs   -> sum (map sizeOfLocalDef defs)
          DefNonRec def -> sizeOfLocalDef def



occursNot :: Name -> Expr -> Bool
occursNot name expr
  = case M.lookup name (occurrences expr) of
      Nothing -> True
      Just oc -> False



occursAtMostOnce :: Name -> Expr -> Bool
occursAtMostOnce name expr
  = case M.lookup name (occurrences expr) of
      Nothing -> True
      Just oc -> case oc of
                   Occur acnt _ _ vcnt -> ((acnt + vcnt) == 1)
                   _ -> False


-- occurs at most once; and if so, it was fully applied to `tn` type arguments and `n` arguments.
occursAtMostOnceAndApplied :: Name -> Int -> Int -> Expr -> Bool
occursAtMostOnceAndApplied name tn n expr
  = case M.lookup name (occurrences expr) of
      Nothing -> True
      Just oc -> case oc of
                   Occur 1 tm m 0 -> (tn == tm && m == n)
                   _ -> False

occursOnceApplied :: Name -> Int -> Int -> Expr -> Maybe Int
occursOnceApplied name tn n expr
 = case M.lookup name (occurrences expr) of
     Nothing -> Nothing
     Just oc -> case oc of
                 Occur 1 tm m vcnt | (tn == tm && m == n) -> Just vcnt
                 _ -> Nothing

occurrencesOf :: Name -> Expr -> Occur
occurrencesOf name expr
  = case M.lookup name (occurrences expr) of
      Nothing -> Occur 0 0 0 0
      Just oc -> oc


-- either: applyCount applications to typeArgsCount/argsCount, and varCount bare occurrences,
-- or more that two occurrences in different forms
data Occur = Occur{ applyCount :: Int, typeArgsCount :: Int, argsCount :: Int, varCount :: Int }
           | Many { count :: Int }

instance Show Occur where
  show (Occur acnt m n vcnt) = show (acnt,m,n,vcnt)
  show (Many m)              = show m

add oc1 oc2
  = case oc1 of
      Many n -> case oc2 of
                  Many m -> Many (n+m)
                  Occur acnt2 m2 n2 vcnt2 -> Many (acnt2 + vcnt2 + n)
      Occur acnt1 m1 n1 vcnt1
        -> case oc2 of
             Many n -> Many (acnt1 + vcnt1 + n)
             Occur acnt2 m2 n2 vcnt2   | acnt2==0 -> Occur acnt1 m1 n1 (vcnt1 + vcnt2)
                                       | acnt1==0 -> Occur acnt2 m2 n2 (vcnt1 + vcnt2)
                                       | m1==m2 && n1==n2 -> Occur (acnt1 + acnt2) m1 n1 (vcnt1 + vcnt2)
                                       | otherwise -> Many (acnt1 + acnt2 + vcnt1 + vcnt2)

occurrences :: Expr -> M.NameMap Occur
occurrences expr
  = case expr of
      App (TypeApp (Var v _) targs) args
        -> ounions (M.singleton (getName v) (Occur 1 (length targs) (length args) 0) : map occurrences args)
      App (Var v _) args
        -> ounions (M.singleton (getName v) (Occur 1 0 (length args) 0) : map occurrences args)
      Var v _ -> M.singleton (getName v) (Occur 0 0 0 1)

      Con{} -> M.empty
      Lit{} -> M.empty
      App f args        -> ounions (occurrences f : map occurrences args)
      Lam pars eff body -> foldr M.delete (occurrences body) (map getName pars)
      TypeLam _ body    -> occurrences body
      TypeApp body _    -> occurrences body
      Let dgs body      -> foldr occurrencesDefGroup (occurrences body) dgs
      Case scruts bs    -> ounions (map occurrences scruts ++ map occurrencesBranch bs)

occurrencesBranch :: Branch -> M.NameMap Occur
occurrencesBranch (Branch pat guards)
  = foldr M.delete (ounions (map occurrencesGuard guards)) (map getName (S.elems (bv pat)))

occurrencesGuard (Guard g e)
  = ounion (occurrences g) (occurrences e)

ounion :: M.NameMap Occur -> M.NameMap Occur -> M.NameMap Occur
ounion oc1 oc2
  = M.unionWith add oc1 oc2

ounions :: [M.NameMap Occur] -> M.NameMap Occur
ounions ocs
  = M.unionsWith add ocs

occurrencesDefGroup :: DefGroup -> M.NameMap Occur -> M.NameMap Occur
occurrencesDefGroup dg oc
  = case dg of
      DefNonRec def -> ounion (M.delete (defName def) oc) (occurrences (defExpr def))
      DefRec defs   -> foldr M.delete (ounions (oc : map (occurrences . defExpr) defs))
                                      (map defName defs)


uniqueTName (TName name tp)
  = do i <- unique
       return (TName (postpend ("." ++ show i) name) tp)


{--------------------------------------------------------------------------
  Simplify Monad
--------------------------------------------------------------------------}

newtype Simp a = Simplify (Int -> SEnv -> Result a)

runSimplify :: Bool -> Bool -> Int -> Int -> Pretty.Env -> Simp a -> (a,Int)
runSimplify unsafe ndebug dupMax uniq penv (Simplify c)
  = case (c uniq (SEnv unsafe ndebug dupMax penv [] )) of
      Ok x u' -> (x,u')



data SEnv = SEnv{ unsafe :: Bool, ndebug :: Bool, dupMax :: Int, penv :: Pretty.Env, currentDef :: [Name] }

data Result a = Ok a Int

instance Functor Simp where
  fmap f (Simplify c)  = Simplify (\u env -> case c u env of Ok x u' -> Ok (f x) u')

instance Applicative Simp where
  pure  = return
  (<*>) = ap

instance Monad Simp where
  return x      = Simplify (\u g -> Ok x u)
  (Simplify c) >>= f  = Simplify (\u g -> case c u g of
                                      Ok x u' -> case f x of
                                                   Simplify d -> d u' g)

instance HasUnique Simp where
  updateUnique f = Simplify (\u g -> Ok u (f u))
  setUnique  i   = Simplify (\u g -> Ok () i)

getEnv :: Simp SEnv
getEnv
  = Simplify (\u g -> Ok g u)

withEnv :: SEnv -> Simp a -> Simp a
withEnv env (Simplify c)
  = Simplify (\u _ -> c u env)

updateEnv :: (SEnv -> SEnv) -> Simp a -> Simp a
updateEnv f (Simplify c)
  = Simplify (\u env -> c u (f env))

getUnsafe :: Simp Bool
getUnsafe
  = do env <- getEnv
       return (unsafe env)

getDuplicationMax :: Simp Int
getDuplicationMax
  = do e <- getEnv
       return (dupMax e)

getNDebug :: Simp Bool
getNDebug
  = do env <- getEnv
       return (ndebug env)

inDef :: Name -> Simp a -> Simp a       
inDef name simp
  = updateEnv (\env -> env{ currentDef = name:currentDef env }) simp

traceS :: String -> Simp ()
traceS msg
  = do env <- getEnv
       trace ("Core.Simplify: " ++ concat (intersperse "." (map show (reverse (currentDef env)))) ++ ": " ++ msg) $ 
         return ()

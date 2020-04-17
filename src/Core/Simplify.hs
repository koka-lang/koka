-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Core simplification
-}
-----------------------------------------------------------------------------

module Core.Simplify (simplify, uniqueSimplify, simplifyDefs) where

import Control.Monad
import Control.Applicative
import Lib.Trace
import Lib.PPrint
import Common.Failure
import Common.Range
import Common.Syntax
import Common.NamePrim( nameEffectOpen, nameToAny, nameEnsureK, nameReturn, nameOptionalNone, nameIsValidK
                       , nameLift, nameBind, nameEvvIndex )
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

simplifyDefs :: Bool -> Int -> Int -> Int -> Pretty.Env -> DefGroups -> (DefGroups,Int)
simplifyDefs unsafe nRuns duplicationMax uniq penv defs
  = runSimplify unsafe duplicationMax uniq penv (simplifyN nRuns (uniquefyDefBodies defs))

simplifyN :: Int -> DefGroups -> Simp DefGroups
simplifyN nRuns defs
  = if (nRuns <= 0) then return defs
    else do defs' <- simplify defs
            simplifyN (nRuns-1) defs'

uniqueSimplify :: Simplify a => Int -> a -> Unique a
uniqueSimplify duplicationMax expr
  = do u <- unique
       let (x,u') = runSimplify False duplicationMax u Pretty.defaultEnv (simplify expr)
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
  Top-down optimizations

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

    topDownLet :: [(TName,Expr)] -> [DefGroup] -> [DefGroup] -> Expr -> Simp Expr
    topDownLet sub acc [] body
      = case subst sub body of
          Let sdgs sbody -> topDownLet [] acc sdgs sbody  -- merge nested Let's
          sbody -> if (null acc)
                    then topDown sbody
                    else return $ Let (reverse acc) sbody

    topDownLet sub acc [DefNonRec (Def{defName=name,defType=tp,defExpr=e})] (Var v _) | getName v == name
      = topDownLet sub acc [] e

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
          DefRec defs
            -> -- trace ("don't simplify recursive lets: " ++ show (map defName defs)) $
               topDownLet sub (sdg:acc) dgs body -- don't inline recursive ones
          DefNonRec def@(Def{defName=x,defType=tp,defExpr=se})  | not (hasNoEffect se)
            -> -- cannot inline effectful expressions
               topDownLet sub (sdg:acc) dgs body
          DefNonRec def@(Def{defName=x,defType=tp,defExpr=se})  -- isTotal se
             -> -- trace ("simplify let: " ++ show x) $
                do maxSmallOccur <- getDuplicationMax
                   let inlineExpr = topDownLet (extend (TName x tp, se) sub) acc dgs body
                   case occurrencesOf x (Let dgs body) of
                     -- no occurrence, disregard
                     Occur 0 m n 0
                       -> -- trace "no occurrence" $
                          topDownLet sub (acc) dgs body
                     -- occurs once, always inline (TODO: maybe only if it is not very big?)
                     Occur vcnt m n acnt | vcnt + acnt == 1
                       -> -- trace "occurs once: inline" $
                          inlineExpr
                     -- occurs fully applied, check if it small enough to inline anyways;
                     -- as it is a function, make it expensive to inline partial applications to avoid too much duplication
                     Occur acnt m n vcnt  | ((acnt + vcnt*3) * sizeOfExpr se) < maxSmallOccur
                       -> -- trace "occurs as cheap function: inline" $
                          inlineExpr
                     -- occurs multiple times as variable, check if it small enough to inline anyways
                     Occur 0 m n vcnt | (vcnt * sizeOfExpr se) < maxSmallOccur
                       -> -- trace "occurs as cheap value: inline" $
                          inlineExpr
                     -- inline total and very small expressions
                     Many n | (n*sizeOfExpr se) < maxSmallOccur
                       -> -- trace "occurs many as cheap value: inline" $
                          inlineExpr
                     -- dont inline
                     oc -> -- trace ("no inline: occurrences: " ++ show oc ++ ", size: " ++ show (sizeOfExpr se)) $
                           topDownLet sub (sdg:acc) dgs body

            {-
               if (isTotalAndCheap se)
                then -- inline total and small expressions
                     -- trace (" inline small") $
                     topDownLet (extend (TName x tp, se) sub) acc dgs body
               else case extractFun se of
                Just (tpars,pars,_,_)
                  | occursAtMostOnceApplied
                    -- occursAtMostTwiceApplied -- TODO: this is probably too aggresive but inlines all yield-bind next arguments...
                     x (length tpars) (length pars) (Let dgs body) -- todo: exponential revisits of occurs
                  -> -- function that occurs once in the body and is fully applied; inline to expose more optimization
                     -- let f = \x -> x in f(2) ~> 2
                     -- trace (" inline once & applied") $
                     topDownLet (extend (TName x tp, se) sub) acc dgs body
                Just ([],pars,eff,fbody) | isSmall fbody -- App (Var _ _) args)  | all cheap args
                  -> -- inline functions that are small
                     -- trace (" inline direct app") $
                     topDownLet (extend (TName x tp, se) sub) acc dgs body
                _ | isTotal se && isSmall se && occursAtMostOnce x (Let dgs body) -- todo: exponential revisits of occurs
                  -> -- inline small total expressions
                     -- trace (" inline small total once") $
                     topDownLet (extend (TName x tp, se) sub) acc dgs body
                _ -> -- no inlining
                     -- trace (" don't inline") $
                     topDownLet sub (sdg:acc) dgs body
          -}
    extend :: (TName,Expr) -> [(TName,Expr)] -> [(TName,Expr)]
    extend (name,e) sub
      = (name,e):sub

    extractFun expr
      = case expr of
          TypeLam tpars (Lam pars eff body) -> Just (tpars,pars,eff,body)
          Lam pars eff body                 -> Just ([],pars,eff,body)
          _ -> Nothing



-- Remove effect open applications; only if 'unsafe' is enabled since
-- the effect types won't match up
topDown expr@(App (TypeApp (Var openName _) _) [arg])  | getName openName == nameEffectOpen
  = do unsafe <- getUnsafe
       if (unsafe)
        then topDown arg
        else return expr

-- Direct function applications
topDown expr@(App (Lam pars eff body) args) | length pars == length args
  = do newNames <- mapM uniqueTName pars
       let sub = [(p,Var np InfoNone) | (p,np) <- zip pars newNames]
           argsopt = replicate (length pars - length args) (Var (TName nameOptionalNone typeAny) InfoNone)
           expr' = Let (zipWith makeDef newNames (args++argsopt)) (sub |~> body)
       trace("simplify: " ++ show expr ++ " to " ++ show expr') $
        topDown expr'
  where
    makeDef (TName npar nparTp) arg
      = DefNonRec (Def npar nparTp arg Private DefVal rangeNull "")

-- Direct function applications
topDown (App (TypeApp (TypeLam tpars (Lam pars eff body)) targs) args) | length pars == length args && length tpars == length targs
  = do newNames <- mapM uniqueTName pars
       let sub = [(p,Var np InfoNone) | (p,np) <- zip pars newNames]
           argsopt = replicate (length pars - length args) (Var (TName nameOptionalNone typeAny) InfoNone)
       topDown $
        substitute (subNew (zip tpars targs)) $
         Let (zipWith makeDef newNames (args++argsopt)) (sub |~> body)
  where
    makeDef (TName npar nparTp) arg
      = DefNonRec (Def npar nparTp arg Private DefVal rangeNull "")



{-

-- Fast & Bind functions
topDown (App def@(Let [DefNonRec (Def nnil _ (App (Var (TName inBindCtx _) _) []) _ DefVal _ _)] (TypeApp (Var (TName name tp) (InfoArity m n PolyMon)) targs)) args)
  | nnil == nameNil && inBindCtx == nameInBindCtx
  = topDown $ App (TypeApp (Var (TName (makeMonName name) tp) (InfoArity m n AlwaysMon)) targs) args
  | otherwise
  = do args' <- mapM topDown args
       return (App def args')

topDown (App (TypeApp (Var (TName name tp) (InfoArity m n PolyMon)) targs) args)
  = topDown $ App (TypeApp (Var (TName (makeNoMonName name) tp) (InfoArity m n NoMon)) targs) args
-}

-- No optimization applies
topDown expr
  = return expr



{--------------------------------------------------------------------------
  Bottom-up optimizations

  These optimizations can assume their children have already been simplified.
--------------------------------------------------------------------------}

bottomUp :: Expr -> Expr


-- replace "(/\a. body) t1" with "body[a |-> t1]"
bottomUp expr@(TypeApp (TypeLam tvs body) tps)
  = if (length tvs == length tps)
     then let sub = subNew (zip tvs tps)
          in sub |-> body
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
      = DefNonRec (Def npar nparTp arg Private DefVal rangeNull "")

    free parName
      = not (parName `S.member` fv args)

-- eta contract
{-
bottomUp (Lam pars eff (App expr args)) | parsMatchArgs
  = expr
  where
    parsMatchArgs = length pars == length args && all match (zip pars args)
    match (par,arg)
      = case arg of
          Var v _  -> v == par
          _ -> False
-}




-- bind( lift(arg), cont ) -> cont(arg)
bottomUp (App (TypeApp (Var bind _) _) [App (TypeApp (Var lift _) _) [arg], cont]) | getName bind == nameBind && getName lift == nameLift
  = App cont [arg]

-- continuation validation
bottomUp expr@(App (TypeApp (Var isValidK _) _) [arg])  | getName isValidK == nameIsValidK
  = case arg of
      Var optNone _  | getName optNone == nameOptionalNone  -> exprFalse
      Lam _ _ _ -> exprTrue
      App _ _ -> exprTrue
      _ -> expr

-- case on singleton constructor
bottomUp expr@(Case [con@(Con name repr)] bs)
  = case matchBranches con bs of
      Just b -> b
      _ -> expr

-- lift common continuation; often generated by cps (`test/algeff/cps-cgen1`)
bottomUp expr@(Case scruts bs)  | commonContinue
  = case mbCont of
      Nothing           -> expr -- cannot happen
      Just (common,cbs) -> App common [Case scruts cbs]
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


-- simplify evv-index(l) to i if l has a known offset
bottomUp (App (TypeApp (Var evvIndex _) [effTp,hndTp]) [htag]) | getName evvIndex == nameEvvIndex && isEffectFixed effTp
  = makeInt32 (effectOffset (effectLabelFromHandler hndTp) effTp)


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


bottomUp (App f args)
  = App f (map bottomUpArg args)

-- No optimization applies
bottomUp expr
  = expr

bottomUpArg :: Expr -> Expr
bottomUpArg arg
  = case arg of
      App (Var v _) [expr] | getName v == nameEnsureK -> expr
      _ -> arg


matchBranches :: Expr -> [Branch] -> Maybe Expr
matchBranches scrutinee branches
  = case (foldl f NoMatch branches) of
      Match expr -> Just expr
      _ -> Nothing
  where
    f NoMatch branch = matchBranch scrutinee branch
    f found _ = found

matchBranch :: Expr -> Branch -> Match Expr
matchBranch scrut (Branch [pat] [Guard guard expr]) | isExprTrue guard
  = case (scrut,pat) of
      (Con name _repr, PatCon pname [] _prepr _ _ _ _info)
        | name == pname -> Match expr
        | otherwise     -> NoMatch
      (_,PatVar name PatWild)
        -> let def = Def (getName name) (typeOf name) scrut Private DefVal rangeNull ""
           in Match (Let [DefNonRec def] expr)
      (_,PatWild)
        -> let def = Def (nameNil) (typeOf scrut) scrut Private DefVal rangeNull ""
           in Match (Let [DefNonRec def] expr)
      _ -> Unknown
matchBranch scrut branch
  = Unknown


data Match a = Match a | Unknown | NoMatch

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
    in findMatch 0 l ls

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
  simplify (Def name tp expr vis sort nameRng doc)
    = do expr' <- case expr of
                    TypeLam tvs (Lam pars eff body)
                      -> do body' <- simplify body
                            return $ TypeLam tvs (Lam pars eff body')
                    Lam pars eff body
                      -> do body' <- simplify body
                            return $ Lam pars eff body'
                    _ -> simplify expr
         return $ Def name tp expr' vis sort nameRng doc

instance Simplify a => Simplify [a] where
  simplify  = mapM simplify

{--------------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------------}

instance Simplify Expr where
  simplify e
    = do td <- topDown e
         e' <- case td of
                Lam tnames eff expr
                  -> do x <- simplify expr; return $ Lam tnames eff x
                Var tname info
                  -> return td
                App e1 e2
                  -> do x1 <- simplify e1
                        x2 <- simplify e2
                        return $ App x1 x2
                TypeLam tv expr
                  -> fmap (TypeLam tv) (simplify expr)
                TypeApp expr tp
                  -> do x <- simplify expr; return $ TypeApp x tp
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
         return (bottomUp e')

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


isTotalAndCheap :: Expr -> Bool
isTotalAndCheap expr
  = case expr of
      Var{} -> True
      Con{} -> True
      Lit{} -> True
      -- toany(x)
      App (TypeApp (Var v _) [_]) [arg] | getName v == nameToAny -> isTotalAndCheap arg
      App (TypeApp (Var v _) _) [arg]   | getName v == nameEffectOpen -> isTotalAndCheap arg
      -- functions that are immediately applied to something cheap (cps generates this for resumes)
      -- Lam pars eff (App e args)
        --  -> isTotalAndCheap e && all isTotalAndCheap args -- matchParArg (zip pars args)
      -- type application / abstraction
      TypeLam _ body -> isTotalAndCheap body
      TypeApp body _ -> isTotalAndCheap body
      Lam params eff body -> isTotalAndCheap body
      Case exprs branches -> all isTotalAndCheap exprs && all isTotalAndCheapBranch branches
      _     -> False
  where
    matchParArg (par,Var v _) = par == v
    matchParArg (par,App (TypeApp (Var v _) [_]) [Var arg _]) = par == arg && getName v == nameToAny
    matchParArg _ = False

    matchTParTArg (tv1,TVar tv2) = tv1 == tv2
    matchTParTArg _ = False

isTotalAndCheapBranch (Branch pats guards)
  = all isTotalAndCheapGuard guards
isTotalAndCheapGuard (Guard test expr)
  = isTotalAndCheap test && isTotalAndCheap expr


cheap expr
  = isSmallX 1 expr

isSmall expr
  = isSmallX 3 expr -- at most 3 applications deep

isSmallX n expr
  = if (n <= 0) then False
    else case expr of
      Var{} -> True
      Con{} -> True
      Lit{} -> True
      TypeLam _ e -> isSmallX n e
      TypeApp e _ -> isSmallX n e
      -- next one enables inlining of resume; improve performance on 'test/algeff/perf2'
      App (TypeApp (Var v _) [_]) [e] | getName v == nameToAny      -> cheap e
      App (TypeApp (Var v _) _) [e]   | getName v == nameEffectOpen -> cheap e
      App f args  -> all (isSmallX (n-1)) (f:args)
      _ -> False



sizeOfExpr :: Expr -> Int
sizeOfExpr expr
  = case expr of
      Var tname info     -> 1
      Con tname repr     -> 1
      Lit lit            -> 1
      Lam tname eff body -> 1 + sizeOfExpr body
      App e args         -> 1 + sizeOfExpr e + sum (map sizeOfExpr args)
      TypeLam tvs e      -> sizeOfExpr e
      TypeApp e tps      -> sizeOfExpr e
      Let defGroups body -> sum (map sizeOfDefGroup defGroups) + (sizeOfExpr body)
      Case exprs branches -> 1 + sum (map sizeOfExpr exprs) + sum (map sizeOfBranch branches)

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

runSimplify :: Bool -> Int -> Int -> Pretty.Env -> Simp a -> (a,Int)
runSimplify unsafe dupMax uniq penv (Simplify c)
  = case (c uniq (SEnv unsafe dupMax penv [] )) of
      Ok x u' -> (x,u')



data SEnv = SEnv{ unsafe :: Bool, dupMax :: Int, penv :: Pretty.Env, currentDef :: [Def] }

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

getUnsafe :: Simp Bool
getUnsafe
  = do env <- getEnv
       return (unsafe env)

getDuplicationMax :: Simp Int
getDuplicationMax
  = do e <- getEnv
       return (dupMax e)



-- | a core expression that cannot cause any evaluation _for sure_
hasNoEffect :: Expr -> Bool
hasNoEffect  expr
 = case expr of
     Lam _ _ _   -> True
     Var _ _     -> True
     TypeLam _ e -> hasNoEffect e
     TypeApp e _ -> hasNoEffect e
     Con _ _     -> True
     Lit _      -> True
     Let dgs e  -> all hasNoEffectDef (flattenDefGroups dgs) && hasNoEffect e
     Case exps branches -> all hasNoEffect exps && all hasNoEffectBranch branches
     _          -> False  -- todo: a let or case could be total


hasNoEffectDef def = hasNoEffect (defExpr def)

hasNoEffectBranch (Branch pat guards) = all hasNoEffectGuard guards
hasNoEffectGuard (Guard test expr)    = hasNoEffect test && hasNoEffect expr

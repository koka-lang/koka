-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Unification and subsumption
-----------------------------------------------------------------------------

module Type.Unify ( Unify, UnifyError(..), runUnify
                  , unify
                  , subsume
                  , overlaps
                  , matchNamed
                  , matchArguments
                  , extractNormalizeEffect
                  ) where

import Control.Applicative
import Control.Monad
import Lib.PPrint
import Common.Range
import Common.Unique
import Common.Failure
import Common.Name
import Common.NamePrim( namePredHeapDiv, nameTpCont )
import Kind.Kind
import qualified Kind.Unify( match )
import Type.Type
import Type.TypeVar
import Type.Kind
import Type.Pretty()
import Type.Operations
import qualified Core.Core as Core

import qualified Lib.Trace(trace)

trace s x =
   Lib.Trace.trace s
    x

-- | Do two types overlap on the argument types? Used to check for overlapping definitions of overloaded identifiers.
overlaps :: Range -> Tvs -> Type -> Type -> Unify ()
overlaps range free tp1 tp2
  = do rho1 <- instantiate range tp1
       rho2 <- instantiate range tp2
       case (splitFunType rho1, splitFunType rho2) of
         -- values always overlap
         (Nothing,_) -> return ()
         (_,Nothing) -> return ()
         -- rest
         (Just (targs1,_,_), Just (targs2,_,_))
          -> {-
             if (length targs1 /= length targs2)
              then unifyError NoMatch
              else unifies (map snd targs1) (map snd targs2)
             -}
             let (fixed1,optional1) = span (not . isOptional) (map snd targs1)
                 (fixed2,optional2) = span (not . isOptional) (map snd targs2)
             {-
                 len1 = length fixed1
                 len2 = length fixed2
             in if ((null optional1 && len1 < len2) || (null optional2 && len1 > len2))
                 then unifyError NoMatch
                 else let lo = min len1 len2
                      in do unifies (take lo fixed1) (take lo fixed2)
                            return () -- todo: this is slightly too strict: if the longest prefix of fixed arguments match, we consider them overlapped
                      -}
                 hi  = max (length fixed1) (length fixed2)
                 fo1 = take hi (fixed1 ++ map unOptional optional1)
                 fo2 = take hi (fixed2 ++ map unOptional optional2)
             in if (length fo1 /= length fo2)
                 then unifyError NoMatch  -- one has more fixed arguments than the other can ever get
                 else do unifies fo1 fo2
                         return ()



-- | Does a type have the given named arguments?
matchNamed :: Range -> Type -> Int -> [Name] -> Unify ()
matchNamed range tp n named
  = do rho1 <- instantiate range tp
       case splitFunType rho1 of
         Nothing -> unifyError NoMatch
         Just (pars,_,_)
          -> if (n + length named > length pars)
              then unifyError NoMatch
              else let npars = drop n pars
                       names = map fst npars
                   in if (all (\name -> name `elem` names) named)
                       then let rest = [tp | (nm,tp) <- npars, not (nm `elem` named)]
                            in if (all isOptional rest)
                                then return ()
                                else unifyError NoMatch
                       else unifyError NoMatch


-- | Does a function type match the given arguments? if the first argument 'matchSome' is true,
-- it is considered a match even if not all arguments to the function are supplied
matchArguments :: Bool -> Range -> Tvs -> Type -> [Type] -> [(Name,Type)] -> Unify ()
matchArguments matchSome range free tp fixed named
  = do rho1 <- instantiate range tp
       case splitFunType rho1 of
         Nothing -> unifyError NoMatch
         Just (pars,_,_)
          -> if (length fixed + length named > length pars)
              then unifyError NoMatch
              else do -- subsume fixed parameters
                      let (fpars,npars) = splitAt (length fixed) pars
                      mapM_  (\(tpar,targ) -> subsume range free (unOptional tpar) targ) (zip (map snd fpars) fixed)
                      -- subsume named parameters
                      mapM_ (\(name,targ) -> case lookup name npars of
                                               Nothing   -> unifyError NoMatch
                                               Just tpar -> subsume range free tpar (makeOptional targ)) named
                      -- check the rest is optional
                      let rest = [tpar | (nm,tpar) <- npars, not (nm `elem` map fst named)]
                      if (matchSome || all isOptional rest)
                       then return ()
                       else unifyError NoMatch

{--------------------------------------------------------------------------
  Subsumption
--------------------------------------------------------------------------}
-- | @subsume free t1 t2@ holds if @t2@ can be instantiated to @t1@ where
-- @free@ are the free type variables in the assumption. Returns
-- under which predicates this holds and a core transformer that needs to be
-- applied to the expression of type @t2@. Also returns a new type for the
-- expected type @tp1@ where 'some' types have been properly substituted (and
-- may be quantified).
subsume :: Range -> Tvs -> Type -> Type -> Unify (Type,[Evidence], Core.Expr -> Core.Expr)
subsume range free tp1 tp2
  = -- trace (" subsume: " ++ show (tp1,tp2) ++ ", free: " ++ show (tvsList free)) $
    do -- skolemize,instantiate and unify
       (sks,evs1,rho1,core1) <- skolemizeEx range tp1
       (tvs,evs2,rho2,core2) <- instantiateEx range tp2
       unify rho2 rho1

       -- escape check: no skolems should escape into the environment
       -- entailment check: predicates should be entailed
       -- todo: we should check for skolems since predicates with skolems must be entailed directly
       sub  <- getSubst
       -- trace (" escape check: " ++ show (rho1,rho2) ++ " sub: " ++ show (subList sub)) $ return ()
       let allfree = tvsUnion free (ftv tp1)
           escaped = fsv $ [tp  | (tv,tp) <- subList sub, tvsMember tv allfree]
       -- trace (" escape check: skolems: " ++ show sks ++ " vs. escaped: " ++ show (tvsList escaped)) $ return ()
       if (tvsDisjoint (tvsNew sks) escaped)
         then return ()
         else unifyError NoSubsume
       (evsEnt,coreEnt) <- entails (tvsNew sks) (sub |-> evs1) (sub |-> evs2)
       -- final type
       (vars,ssub) <- freshSub Bound sks
       let subx = ssub @@ sub
           tp = quantifyType vars (qualifyType [(subx |-> evPred ev) | ev <- evs1] (subx |-> rho1)) -- TODO: do rho1 and we get skolem errors: see 'Prelude.choose'
       -- return
       return (tp, subx |-> evsEnt,
                (\expr -> Core.addTypeLambdas vars $     -- generalize
                          subx |-> (coreEnt $                      -- apply evidence evs2 & abstract evidence evs1
                                    Core.addTypeApps tvs expr)))   -- instantiate


-- | @entails skolems known preds@ returns both predicates that need to be proved
-- and a core transformer that applies the evidence for @preds@ and abstracts for
-- thos in @known@. The @preds@ are entailed by
-- @known@ and predicates containing a type variable in @skolems@ must be entailed
-- completely by other predicates (not containing such skolems).
entails :: Tvs -> [Evidence] -> [Evidence] -> Unify ([Evidence], Core.Expr -> Core.Expr)
entails skolems known []
  = return ([],id)
entails skolems known evs | map evPred known == map evPred evs
  = return (evs,id)   -- todo: should construct evidence from known to preds (simple one-to-one name mapping)
entails skolems known (ev:evs)
  = case evPred ev of
      PredIFace name [_,_,_]  | name == namePredHeapDiv  -- can always be solved
        -> entails skolems known evs
      _ -> -- trace ("Type.Unify.subsume.entails: cannot show entailment: " ++ show (tvsList skolems,known,ev:evs)) $
           unifyError NoEntail


{--------------------------------------------------------------------------
  Unification
--------------------------------------------------------------------------}
-- | Unify two types.
unify :: Type -> Type -> Unify ()

-- effects
unify t1@(TApp (TCon tc1) _) t2@(TApp (TCon tc2) _)  | tc2 == tconEffectExtend && tc1 == tconEffectExtend
  = unifyEffect t1 t2

unify t1@(TApp (TCon tc1) _) (TVar tv2)  | tc1 == tconEffectExtend && isMeta tv2
  = unifyEffectVar tv2 t1

unify (TVar tv1) t2@(TApp (TCon tc2) _)  | tc2 == tconEffectExtend && isMeta tv1
  = unifyEffectVar tv1 t2

-- type variables
unify (TVar v1) (TVar v2) | v1 == v2
  = return () -- todo: kind check?

-- unify t1@(TVar (TypeVar id1 _ Meta)) t2@(TVar (TypeVar id2 _ Meta)) | id1 < id2 -- arbitrary order
--  = unify t2 t1

unify (TVar tv@(TypeVar id kind Meta)) tp
  = unifyTVar tv tp

unify tp (TVar tv@(TypeVar id kind Meta))
  = unifyTVar tv tp

-- constants
unify (TCon tc1) (TCon tc2)  | tc1 == tc2
  = return ()

-- applications
unify (TApp t1 ts1) (TApp u1 us2)   | length ts1 == length us2
  = do unify t1 u1
       unifies ts1 us2

-- functions
unify (TFun args1 eff1 res1) (TFun args2 eff2 res2) | length args1 == length args2
  = do unify eff1 eff2
       unifies (res1:map snd args1) (res2:map snd args2)

-- quantified types
unify (TForall vars1 preds1 tp1) (TForall vars2 preds2 tp2) | length vars1 == length vars2 && length preds1 == length preds2
  = do -- match kinds of quantifiers
       let kinds1 = map getKind vars1
           kinds2 = map getKind vars2
       matchKinds kinds1 kinds2
       -- replace with shared bound variables in both types
       -- note: assumes ordered quantifiers and ordered predicates
       -- note: we don't use Skolem as a Meta variable can unify with a Skolem but not with a Bound one
       vars <- mapM (\kind -> freshTVar kind Bound) kinds1
       let sub1 = subNew (zip vars1 vars)
           sub2 = subNew (zip vars2 vars)
           stp1 = sub1 |-> tp1
           stp2 = sub2 |-> tp2
           spreds1 = sub1 |-> preds1
           spreds2 = sub2 |-> preds2
       -- and unify the results
       unify stp1 stp2
       unifyPreds preds1 preds2
       -- no need to check for escaping skolems as we don't unify to bound variables

-- special unsafe(!) handling of continuations; just for cps translation :-(
unify t1@(TSyn syn1 args1 tp1) t2@(TSyn syn2 args2 tp2) | typesynName syn1 == nameTpCont && typesynName syn2 == nameTpCont
  = -- trace ("cont==cont") $
    unifies (take (n-1) args1) (take (n-1) args2)
  where
    n = length args1

unify t1@(TSyn syn1 args1 tp1) t2@(TFun [(_,tpar)] teff tres) | typesynName syn1 == nameTpCont
  = -- trace ("cont==fun") $
    unifies (take 2 args1) [tpar,teff]

-- synonyms
unify t1@(TSyn syn1 args1 tp1) t2@(TSyn syn2 args2 tp2)
  = if (typesynRank syn1 > typesynRank syn2)
     then unify tp1 t2
     else unify t1 tp2

unify (TSyn _ _ tp1) tp2
  = unify tp1 tp2

unify tp1 (TSyn _ _ tp2)
  = unify tp1 tp2


-- no match
unify tp1 tp2
  = -- trace ("no match: " ++  show (pretty tp1, pretty tp2)) $
    unifyError NoMatch


-- | Unify a type variable with a type
unifyTVar :: TypeVar -> Type -> Unify ()
unifyTVar tv@(TypeVar id kind Meta) tp
  = let etp = expandSyn tp in
    if (tvsMember tv (fuv etp))
     then -- trace ("unifyTVar: " ++ show tv ++ ":=" ++ show tp ++ " is infinite") $
          case expandSyn tp of
             TVar tv2 | tv == tv2 -> return ()  -- ie. a ~ id<a>
             _        -> unifyError Infinite
     else case etp of
            TVar (TypeVar _ _ Bound)
              -> unifyError NoMatch -- can't unify with bound variables
            TVar tv2@(TypeVar id2 _ Meta) | id <= id2
              -> if (id < id2)
                  then unifyTVar tv2 (TVar tv)
                  else return () -- todo: kind check?
            _ -> if (not (matchKind kind (getKind tp)))
                  then -- trace ("unifyTVar: kinds: " ++ show (kind,getKind tp) ++ ", " ++ show tp) $
                       unifyError NoMatchKind
                  else do -- trace ("unifyVar: " ++ show tv ++ ":=" ++ show tp) $ return ()
                          extendSub tv tp
                          return ()

unifyTVar tv tp
  = failure "Type.Unify.unifyTVar: called with skolem or bound variable"




-- | Unify two equal lenght lists of types, and apply a substitution before each unification
unifies :: [Type] -> [Type] -> Unify ()
unifies [] []
  = return ()
unifies (t:ts) (u:us)
  = do st <- subst t
       su <- subst u
       unify st su
       unifies ts us
unifies _ _
  = failure "Type.Unify.unifies"


-- | Unify predicates (applies a substitution before each unification)
unifyPreds :: [Pred] -> [Pred] -> Unify ()
unifyPreds [] []
  = return ()
unifyPreds (p1:ps1) (p2:ps2)
  = do sp1 <- subst p1
       sp2 <- subst p2
       unifyPred p1 p2
       unifyPreds ps1 ps2
unifyPreds _ _
  = failure "Type.Unify.unifyPreds"


unifyPred :: Pred -> Pred -> Unify ()
unifyPred (PredSub t1 t2) (PredSub u1 u2)
  = do unify t1 u1
       st2 <- subst t2
       su2 <- subst u2
       unify st2 su2
unifyPred (PredIFace name1 ts1) (PredIFace name2 ts2)  | name1 == name2
  = unifies ts1 ts2
unifyPred _ _
  = unifyError NoMatchPred


-- | Unify effects
unifyEffect tp1 tp2
  = do (ls1,tl1) <- extractNormalizeEffect tp1
       (ls2,tl2) <- extractNormalizeEffect tp2
       (ds1,ds2) <- unifyLabels ls1 ls2
       case (expandSyn tl1, expandSyn tl2) of
         (TVar (TypeVar id1 kind1 Meta), TVar (TypeVar id2 kind2 Meta)) | id1 == id2 && not (null ds1 && null ds2)
             -> do -- trace ("unifyEffect: unification of " ++ show (tp1,tp2) ++ " is infinite") $ return ()
                   unifyError Infinite
         _   -> do tail1 <- if null ds1 then return tl1
                                        else do tv1 <- freshTVar kindEffect Meta
                                                unify tl1 (effectExtends ds1 tv1)
                                                return tv1
                   stl2  <- subst tl2
                   tail2 <- if null ds2 then return stl2
                                        else do tv2 <- freshTVar kindEffect Meta
                                                unify stl2 (effectExtends ds2 tv2)
                                                return tv2
                   stail1 <- subst tail1
                   unify stail1 tail2

                   stp1 <- subst tp1
                   stp2 <- subst tp2
                   -- trace ("unifyEffect: " ++ show (tp1,tp2) ++ " to " ++ show (stp1,stp2) ++ " with " ++ show (ds1,ds2)) $ return ()

                   return ()

extractNormalizeEffect :: Type -> Unify ([Type],Type)
extractNormalizeEffect tp
  = do tp' <- subst tp
       return $ extractOrderedEffect tp'


unifyEffectVar tv1 tp2
  = do let (ls2,tl2) = extractOrderedEffect tp2  -- ls2 must be non-empty
       case expandSyn tl2 of
         TVar tv2 | tv1 == tv2  -- e ~ <div,exn|e>  ~> e := <div,exn|e'>
           -> -- trace ("unifyEffectVar: " ++ show tv1 ++ ":=" ++ show tp2 ++ " is infinite") $
                 unifyError Infinite
         _ -> do tv <- freshTVar kindEffect Meta
                 unifyTVar tv1 (effectExtends ls2 tl2)


-- | Unify lists of ordered labels; return the differences.
unifyLabels :: [Tau] -> [Tau] -> Unify ([Tau],[Tau])
unifyLabels ls1 ls2
  = case (ls1,ls2) of
      ([],[])
        -> return ([],[])
      ((_:_),[])
        -> return ([],ls1)
      ([],(_:_))
        -> return (ls2,[])
      (l1:ll1, l2:ll2)
        -> case compareLabel l1 l2 of
            LT ->do (ds1,ds2) <- unifyLabels ll1 ls2
                    return (ds1,l1:ds2)
            GT ->do (ds1,ds2) <- unifyLabels ls1 ll2
                    return (l2:ds1,ds2)
            EQ -> -- labels are equal
                 do unify l1 l2  -- for heap effects and kind checks
                    ll1' <- subst ll1
                    ll2' <- subst ll2
                    unifyLabels ll1 ll2

compareLabel l1 l2
  = let (name1,i1) = labelNameEx l1
        (name2,i2) = labelNameEx l2
    in case compare name1 name2 of
         -- EQ | i1 /= 0 && i2 /= 0 -> compare i1 i2
         cmp -> cmp



-- | Match a list of kinds
matchKinds :: [Kind] -> [Kind] -> Unify ()
matchKinds kinds1 kinds2
  = if (and [Kind.Unify.match k1 k2 | (k1,k2) <- zip kinds1 kinds2])
     then return ()
     else unifyError NoMatchKind

matchKind :: Kind -> Kind -> Bool
matchKind (KCon c1) (KCon c2)         = (c1 == c2)
matchKind (KApp k1 k2) (KApp l1 l2)   = (matchKind k1 l1) && (matchKind k2 l2)
matchKind _ _ = False


{--------------------------------------------------------------------------
  Unify monad
--------------------------------------------------------------------------}
data Unify a  = Unify (St -> Res a)
data Res a    = Ok !a !St
              | Err UnifyError !St
data St       = St{ uniq :: !Int, sub :: !Sub }

data UnifyError
  = NoMatch
  | NoMatchKind
  | NoMatchPred
  | NoSubsume
  | NoEntail
  | Infinite
  | NoArgMatch Int Int
  deriving Show

runUnify :: HasUnique m => Unify a -> m (Either UnifyError a,Sub)
runUnify (Unify f)
  = do i <- unique
       case f (St i subNull) of
         Ok x (St j sub)    -> do setUnique j
                                  return (Right x,sub)
         Err err (St j sub) -> do setUnique j
                                  return (Left err,sub)

instance HasUnique Unify where
  updateUnique f = Unify (\st -> Ok (uniq st) (st{ uniq = f (uniq st) }))

instance Functor Unify where
  fmap f (Unify u)  = Unify (\st1 -> case u st1 of
                                       Ok x st2 -> Ok (f x) st2
                                       Err err st2 -> Err err st2)

instance Applicative Unify where
  pure  = return
  (<*>) = ap

instance Monad Unify where
  return x          = Unify (\st -> Ok x st)
  (Unify u) >>= f   = Unify (\st1 -> case u st1 of
                                       Ok x st2 -> case f x of
                                                     Unify u2 -> u2 st2
                                       Err err st2 -> Err err st2)

getSubst :: Unify Sub
getSubst
  = Unify (\st -> Ok (sub st) st)

extendSub :: TypeVar -> Type -> Unify ()
extendSub tv tp
  = Unify (\st -> Ok () (st{ sub = subExtend tv tp (sub st) }))

unifyError :: UnifyError -> Unify a
unifyError err
  = Unify (\st -> Err err st)

subst :: HasTypeVar a => a -> Unify a
subst x
  = do sub <- getSubst
       return (sub |-> x)

-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-- Functions on type variables, including type variable substitution.
-----------------------------------------------------------------------------

module Type.TypeVar(-- * Type substitutable entities
                      HasTypeVar( substitute, ftv, btv )
                    , (|->) 
                    , alltv, fuv, fbv, fsv
                    -- * Ordered free type variables
                    , HasOrderedTypeVar, oftv, ofuv
                    -- * Substitution from type variables to types
                    , Sub
                    , subNull, subNew, subSingle
                    , (@@), subCompose, subExtend
                    , subRemove, subIsNull, subCount
                    -- ** Breaks abstraction
                    , subDom, subRange, subList, subCommon, subFind
                    -- * Set of type variables
                    , Tvs
                    , tvsNew, tvsEmpty, tvsIsEmpty, tvsSingle, tvsInsert, tvsInsertAll, tvsMember, tvsList
                    , tvsDiff, tvsUnion, tvsUnions, tvsIntersect, tvsCommon
                    , tvsRemove, tvsFilter, tvsDisjoint, tvsIsSubsetOf
                    -- * Effects
                    , posnegEffects
                    -- * Requiring HasUnique
                    , freshTypeVar
                    -- * Internal
                    , subInsert, subInserts

                    -- Equal types
                    , matchType
                    ) where

import Data.List(nub,partition)
-- import Lib.Trace
import Common.Range
import Common.Unique
import qualified Data.Map as M
import qualified Data.Set as S
import Common.Failure ( assertion )
import Kind.Pretty    ( ) -- instances
import Type.Type
import Type.Kind
import Kind.Kind

{--------------------------------------------------------------------------

--------------------------------------------------------------------------}
-- | The free unifiable (eg 'Meta') type variables
fuv :: HasTypeVar t => t -> Tvs
fuv t
  = tvsFilter isMeta (ftv t)

-- | The free 'Skolem' type variables
fsv :: HasTypeVar t => t -> Tvs
fsv t
  = tvsFilter isSkolem (ftv t)

-- | The free 'Bound' type variables
fbv :: HasTypeVar t => t -> Tvs
fbv t
  = tvsFilter isBound (ftv t)

-- | All type variables (including the bound ones)
alltv :: HasTypeVar t => t -> Tvs
alltv t
  = tvsUnion (btv t) (ftv t)


{--------------------------------------------------------------------------
  Substitution

  For a substitution is should hold that:
  (s1@@s2) |-> x  <==>  s1 |-> (s2 -> x)

  We can implement this by:
  1) at composition we apply the first substitution to the other, and
     finding an identifier is a simple lookup.
  or,
  2) we compose by simple union and perform a fixpoint at lookup.

  We have chosen (1) here, but it can be interesting to compare
  performance with strategy (2).
--------------------------------------------------------------------------}

-- | A substitution from type variables to 'Tau's.
newtype Sub = Sub (M.Map TypeVar Tau)

unSub (Sub sub)
  = sub

subCount :: Sub -> Int
subCount (Sub sub)
  = M.size sub

subNull :: Sub
subNull
  = Sub M.empty

subIsNull :: Sub -> Bool
subIsNull (Sub sub)
  = M.null sub

subNew :: [(TypeVar, Tau)] -> Sub
subNew sub 
  = -- assertion "Type.TypeVar.subNew" (all (\tv -> length (filter (==tv) tvs) == 1) tvs) $
    -- assertion "Type.TypeVar.subNew.Tau" (all isTau taus) $ 
    assertion ("Type.TypeVar.subNew.KindMismatch: " ++ show (length sub) 
                ++ concatMap (\(x,t) -> "(" ++ showTypeVar x ++ " |-> " ++ showTp t ++ ")") sub) 
     (all (\(x, t) -> getKind x == getKind t) sub) $
    Sub (M.fromList sub)

subDom :: Sub -> Tvs
subDom (Sub sub) 
  = tvsNew (M.keys sub)

subRange :: Sub -> [Tau]
subRange (Sub sub)
  = M.elems sub

subList :: Sub -> [(TypeVar,Tau)]
subList (Sub sub)
  = M.toList sub

subCommon :: Sub -> Sub -> [(TypeVar,(Tau,Tau))]
subCommon (Sub sub1) (Sub sub2)
  = M.toList (M.intersectionWith (,) sub1 sub2)

subSingle :: TypeVar -> Tau -> Sub
subSingle tvar tau
  = -- Top assertion is invalid; it can happen (and happens) in the CoreF typechecker when
    -- typechecking (forall a. f a) with f :: forall b. b -> b, that a bound variable (b) with
    -- number ID must be substituted for another bound variable (a), which *could* have the same
    -- ID. If we want to avoid this, we must ensure that all IDs are distinct; in particular, 
    -- the IDs of built-in types such as .select must be distinct from further IDs generated
    -- by the compiler.
    assertion ("Type.TypeVar.subSingle: recursive type: " ++ showTVar tvar) 
               (not (tvsMember tvar (ftv tau))) $
    -- assertion ("Type.TypeVar.subSingle: not a tau") (isTau tau) $
    assertion "Type.TypeVar.subSingle.KindMismatch" (getKind tvar == getKind tau) $
    Sub (M.singleton tvar tau)

subLookup :: TypeVar -> Sub -> Maybe Tau
subLookup tvar (Sub sub)
  = M.lookup tvar sub

subRemove :: [TypeVar] -> Sub -> Sub
subRemove tvars (Sub sub)
  = Sub (foldr M.delete sub tvars)

subFind :: TypeVar -> Sub -> Tau
subFind tvar sub
  = case subLookup tvar sub of
      Nothing   -> TVar tvar
      Just tau  -> assertion ("Type.TypeVar.subFind: incompatible kind: "
                             ++ showTVar tvar ++ ":" ++ show (getKind tvar) ++ ","
                             ++ "?" ++ ":" ++ show (getKind tau))
                             (getKind tvar == getKind tau) $
                   tau

showTVar (TypeVar id _ _)
  = show id

-- | Compose two substitutions.
-- @(s1 \@\@ s2) |-> x == s1 |-> (s2 |-> x)@
(@@) :: Sub -> Sub -> Sub
sub1 @@ sub2
  = subCompose sub1 sub2

subCompose :: Sub -> Sub -> Sub
subCompose sub1 sub2
  = Sub (M.union (unSub sub1) (unSub (sub1 |-> sub2)))    --ASSUME: left biased union

subExtend :: TypeVar -> Tau -> Sub -> Sub
subExtend tvar tau sub@(Sub s)
  = subSingle tvar tau @@ sub

-- | Insert a new substitution. (Note: breaks abstraction barrier).
subInsert :: TypeVar -> Tau -> Sub -> Sub
subInsert tvar tau (Sub s)
  = assertion ("Type.TypeVar.subSingle: recursive type: " ++ showTVar tvar) 
              (not (tvsMember tvar (ftv tau))) $
    assertion ("Type.TypeVar.subSingle: not a tau") (isTau tau) $
    Sub (M.insert tvar tau s)

subInserts :: [(TypeVar,Tau)] -> Sub -> Sub
subInserts assoc (Sub sub)
  = Sub (M.union (M.fromList assoc) sub)  --ASSUME: left-biased union

(|->) :: HasTypeVar a => Sub -> a -> a
sub |-> x
  = if subIsNull sub then x else 
     (sub `substitute` x)


instance HasTypeVar Sub where
  sub `substitute` (Sub s)
    = Sub (M.map (\k -> sub `substitute` k) s)

  ftv (Sub sub)
    = tvsUnion (tvsNew (M.keys sub)) (ftv (M.elems sub))

  btv sub
    = tvsEmpty
  

instance HasTypeVar a => HasTypeVar (Maybe a) where
  sub `substitute` mb  = case mb of
                  Just x -> Just (sub `substitute` x)
                  Nothing -> Nothing
  ftv mb      = case mb of
                  Just x -> ftv x
                  Nothing -> tvsEmpty
  btv mb      = case mb of
                  Just x -> btv x
                  Nothing -> tvsEmpty

{--------------------------------------------------------------------------
  Type variables
--------------------------------------------------------------------------}
-- | A set of type variables
newtype Tvs = Tvs (S.Set TypeVar)

tvsEmpty :: Tvs
tvsEmpty
  = Tvs S.empty

tvsIsEmpty :: Tvs -> Bool
tvsIsEmpty (Tvs set)
  = S.null set

tvsSingle :: TypeVar -> Tvs
tvsSingle tvar
  = Tvs (S.singleton tvar)

tvsInsert :: TypeVar -> Tvs -> Tvs
tvsInsert tvar (Tvs set)
  = {- assertion "Type.TypeVar.tvsInsert" (case S.lookup tvar set of
                                                Nothing    -> True
                                                Just tvar' -> getKind tvar == getKind tvar') $
    -}
    Tvs (S.insert tvar set)

tvsInsertAll :: [TypeVar] -> Tvs -> Tvs
tvsInsertAll vars (Tvs set)
  = Tvs (foldr S.insert set vars)

tvsNew :: [TypeVar] -> Tvs
tvsNew tvars
  = Tvs (S.fromList tvars)

tvsList :: Tvs -> [TypeVar]
tvsList (Tvs set)
  = S.toList set

tvsRemove :: [TypeVar] -> Tvs -> Tvs
tvsRemove tvar (Tvs set)
  = Tvs (foldr S.delete set tvar)

tvsDiff :: Tvs -> Tvs -> Tvs
tvsDiff (Tvs set1) (Tvs set2)
  = Tvs (S.difference set1 set2)

tvsUnion :: Tvs -> Tvs -> Tvs
tvsUnion (Tvs set1) (Tvs set2)
  = Tvs (S.union set1 set2)

tvsFilter :: (TypeVar -> Bool) -> Tvs -> Tvs
tvsFilter p (Tvs set)
  = Tvs (S.filter p set)

tvsUnions :: [Tvs] -> Tvs
tvsUnions xs
  = foldr tvsUnion tvsEmpty xs

tvsMember :: TypeVar -> Tvs -> Bool
tvsMember tvar (Tvs set)
  = S.member tvar set

tvsIntersect :: Tvs -> Tvs -> Tvs
tvsIntersect (Tvs set1) (Tvs set2)
  = Tvs (S.intersection set1 set2)

tvsCommon :: Tvs -> Tvs -> Bool
tvsCommon tvs1 tvs2
  = not (tvsDisjoint tvs1 tvs2)
  
tvsDisjoint :: Tvs -> Tvs -> Bool
tvsDisjoint tvs1 tvs2
  = tvsIsEmpty (tvsIntersect tvs1 tvs2)

tvsIsSubsetOf :: Tvs -> Tvs -> Bool
tvsIsSubsetOf tvs1 tvs2
  = tvsIsEmpty (tvsFilter (\tvar -> tvsMember tvar tvs2) tvs1)

{--------------------------------------------------------------------------
  Entities with type variables
--------------------------------------------------------------------------}

-- | Return the free unifiable type variables in a particular order
ofuv :: HasOrderedTypeVar a => a -> [TypeVar]
ofuv x = filter isMeta (oftv x)


-- | Return the free type variables in a particular order
oftv :: HasOrderedTypeVar a => a -> [TypeVar]
oftv x = let vs      = nub (odftv x)
             (es,ws) = partition (\v -> isSpecialKind (typevarKind v)) vs
         in ws ++ es
       where
        isSpecialKind kind
          = (kind == kindEffect || kind==kindHeap || kind==kindLabel || kind==kindFun kindLabel kindEffect)

-- | Entitities that contain type variables.
class HasTypeVar a where
  -- | Substitute type variables by 'Tau' types
  substitute :: Sub -> a -> a
  -- | Return free type variables
  ftv   :: a -> Tvs
  -- | Return bound type variables
  btv   :: a -> Tvs

-- | Entities that contain type variables that can be put in a particular order
class HasOrderedTypeVar a where
  -- | Return free type variables in a particular order, may contain duplicates
  odftv  :: a -> [TypeVar]

instance (HasTypeVar a,HasTypeVar b,HasTypeVar c,HasTypeVar d,HasTypeVar e) => HasTypeVar (a,b,c,d,e) where
  sub `substitute` (a,b,c,d,e)
    = (sub `substitute` a, sub `substitute` b, sub `substitute` c, sub `substitute` d, sub `substitute` e)
  ftv (a,b,c,d,e)
    = tvsUnions [ftv a, ftv b, ftv c, ftv d, ftv e] 
  btv (a,b,c,d,e)
    = tvsUnions [btv a, btv b, btv c, btv d, btv e]


instance (HasTypeVar a,HasTypeVar b) => HasTypeVar (a,b) where
  sub `substitute` (x,y)
    = (sub `substitute` x, sub `substitute` y)
  ftv (x,y)
    = tvsUnion (ftv x) (ftv y)
  btv (x,y)
    = tvsUnion (btv x) (btv y)

instance (HasOrderedTypeVar a,HasOrderedTypeVar b) => HasOrderedTypeVar (a,b) where
  odftv (x,y)
    = odftv x ++ odftv y

instance HasTypeVar a => HasTypeVar [a] where
  sub `substitute` xs
    = map (sub `substitute`) xs
  ftv xs
    = tvsUnions (map ftv xs)
  btv xs
    = tvsUnions (map btv xs)

instance HasTypeVar Range where
  sub `substitute` r   = r
  ftv r       = tvsEmpty
  btv r       = tvsEmpty

instance HasOrderedTypeVar a => HasOrderedTypeVar [a] where
  odftv xs
    = concatMap odftv xs

instance HasTypeVar Type where
  sub `substitute` tp
    = case tp of
        TForall vars preds tp   -> let sub' = subRemove vars sub
                                   in TForall vars (sub' |-> preds) (sub' |-> tp)
        TFun args effect result -> TFun (map (\(name,tp) -> (name,sub `substitute` tp)) args) (sub `substitute` effect) (sub `substitute` result)
        TCon tcon               -> TCon tcon
        TVar tvar               -> subFind tvar sub
        TApp tp arg             -> TApp (sub `substitute` tp) (sub `substitute` arg)
        TSyn syn xs tp          -> TSyn syn (sub `substitute` xs) (sub `substitute` tp)

  ftv tp
    = case tp of
        TForall vars preds tp   -> tvsRemove vars (tvsUnion (ftv preds) (ftv tp))
        TFun args effect result -> tvsUnions (ftv effect : ftv result : map (ftv . snd) args)
        TCon tcon               -> tvsEmpty
        TVar tvar               -> tvsSingle tvar
        TApp tp arg             -> tvsUnion (ftv tp) (ftv arg)
        TSyn syn xs tp          -> tvsUnion (ftv xs) (ftv tp)

  btv tp
    = case tp of
        TForall vars preds tp   -> tvsInsertAll vars (tvsUnion (ftv preds) (btv tp))
        TFun args effect result -> tvsUnions (btv effect : btv result : map (btv . snd) args)
        TSyn syn xs tp          -> btv tp 
        TApp tp arg             -> tvsUnion (btv tp) (btv arg)
        _                       -> tvsEmpty


instance HasOrderedTypeVar Type where
  odftv tp
    = case tp of
        TForall vars preds tp   -> filter (\tv -> not (tv `elem` vars)) (odftv tp ++ odftv preds)
        TFun args effect result -> concatMap odftv (map snd args ++ [effect,result])
        TCon tcon               -> []
        TVar tvar               -> [tvar]
        TApp tp arg             -> odftv tp ++ odftv arg
        TSyn syn xs tp          -> odftv tp ++ concatMap odftv xs


instance HasTypeVar Pred where
  subst `substitute` pred
    = case pred of
        PredSub sub super      -> PredSub (subst `substitute` sub) (subst `substitute` super)
        PredIFace name args    -> PredIFace name (subst `substitute` args)

  ftv pred
    = case pred of
        PredSub sub super      -> tvsUnion (ftv sub) (ftv super)
        PredIFace name args    -> ftv args

  btv pred
    = tvsEmpty

instance HasOrderedTypeVar Pred where
  odftv pred
    = case pred of
        PredSub sub super      -> odftv [sub,super]
        PredIFace name args    -> odftv args


{--------------------------------------------------------------------------
  Strictly positive and negative type variables
--------------------------------------------------------------------------}
posnegEffects :: Type -> ([TypeVar],[TypeVar])
posnegEffects tp
  = let tvs1 = positive tp
        tvs2 = negative tp
        tvsPos = tvsDiff tvs1 tvs2
        tvsNeg = tvsDiff tvs2 tvs1
    in (tvsList $ tvsFilter (\tv -> getKind tv == kindEffect) tvsPos
       ,tvsList $ tvsFilter (\tv -> getKind tv == kindEffect) tvsNeg
       )

positive,negative :: Type -> Tvs
positive = posneg True
negative = posneg False

posneg :: Bool -> Type -> Tvs
posneg isPos tp
  = case tp of
      TForall vars preds tp   -> tvsRemove vars (posneg isPos tp)
      TFun args effect result -> tvsUnions (posneg isPos effect : posneg isPos result : map (posneg (not isPos) . snd) args)
      TCon tcon               -> tvsEmpty
      TVar tvar               -> if (isPos) then tvsSingle tvar else tvsEmpty
      TApp tp args            -> tvsUnions (posneg isPos tp : map (posneg isPos) args)
      TSyn syn xs tp          -> posneg isPos tp
  


{--------------------------------------------------------------------------
  Operations requiring HasUnique 
--------------------------------------------------------------------------}

freshTypeVar :: HasUnique m => Kind -> Flavour -> m TypeVar
freshTypeVar kind flavour
  = do id <- uniqueId (case flavour of
                         Meta   -> "_v"
                         Skolem -> "$v"
                         Bound  -> "v")
       -- trace ("Type.TypeVar.freshTypeVar: " ++ show id) $
       return $ TypeVar id kind flavour



{--------------------------------------------------------------------------
  Equality between types
--------------------------------------------------------------------------}
matchType :: Type -> Type -> Bool
matchType tp1 tp2
  = case (expandSyn tp1,expandSyn tp2) of
      (TForall vs1 ps1 t1, TForall vs2 ps2 t2)  -> if (vs1==vs2)
                                                    then (matchPreds ps1 ps2 && matchType t1 t2)
                                                    else if (length vs1 == length vs2 && all (\(v1,v2) -> getKind v1 == getKind v2) (zip vs1 vs2)) 
                                                          then let sub = subNew (zip vs1 (map TVar vs2))
                                                               in (matchPreds (sub |-> ps1) ps2 && matchType (sub |-> t1) t2)
                                                          else False                                                  
      (TFun pars1 eff1 t1, TFun pars2 eff2 t2)  -> (matchTypes (map snd pars1) (map snd pars2) && matchEffect eff1 eff2 && matchType t1 t2)
      (TCon c1, TCon c2)                        -> c1 == c2
      (TVar v1, TVar v2)                        -> v1 == v2
      (TApp t1 ts1, TApp t2 ts2)                -> (matchType t1 t2 && matchTypes ts1 ts2)
      -- (TSyn syn1 ts1 t1, TSyn syn2 ts2 t2)      -> (syn1 == syn2 && matchTypes ts1 ts2 && matchType t1 t2)
      _ -> False

matchTypes ts1 ts2
  = and (zipWith matchType ts1 ts2)

matchEffect eff1 eff2
  = matchType (orderEffect eff1) (orderEffect eff2)

matchPreds ps1 ps2
  = and (zipWith matchPred ps1 ps2)

matchPred :: Pred -> Pred -> Bool
matchPred p1 p2
  = case (p1,p2) of
      (PredSub sub1 sup1, PredSub sub2 sup2)  -> (matchType sub1 sub2 && matchType sup1 sup2)
      (PredIFace n1 ts1, PredIFace n2 ts2)    -> (n1 == n2 && matchTypes ts1 ts2)
      (_,_) -> False



------------------------------------------------------------------------------
-- This is just for debugging purposes as we cannot include Type.Pretty :-(
------------------------------------------------------------------------------
showTypeVar :: TypeVar -> String
showTypeVar (TypeVar name kind _)
  = show name ++ "::" ++ show kind

showTp :: Type -> String
showTp tp
  = case tp of
      TVar tvar   -> showTypeVar tvar
      TCon tcon   -> show (typeconName tcon) ++ "::" ++ show (typeconKind tcon)
      TApp tp args-> showTp tp ++ "<" ++ concatMap (\t -> showTp t ++ ",") args ++ ">"
      TSyn syn args body -> "(syn:" ++ show (typesynName syn) ++ "::" ++ show (typesynKind syn) ++ "<" ++ concatMap (\t -> showTp t ++ ",") args ++ ">" ++ "[" ++ showTp body ++ "])"
      _ -> "?"

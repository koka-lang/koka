-----------------------------------------------------------------------------
-- Copyright 2026, Microsoft Research, Daan Leijen, Tim Whiting.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
-- | Re-express a mutually recursive function group as a single self-recursive
--   driver over a GADT, so that downstream optimizations -- which mostly give
--   up on mutual recursion -- can fire, and mutual tail recursion runs in
--   constant stack. A whole mutually recursive `DefRec {f1..fn}` becomes:
--
--    * one GADT `@mutrec-*` value type indexed by a single result type `r`,
--      with a `@Call-fi` constructor per member holding fi's arguments and a
--      `@type-eq<fi-target, r>` witness (see `std/core/types`); fi's own value
--      type variables become existentials of `@Call-fi`, so members may differ
--      in result type, type parameter count/order, and instantiation;
--    * one self-recursive driver `@run` with one branch per member holding
--      that member's body, its tail leaves `@coerce`d to `r`; every sibling
--      call, tail or not, goes through the member's wrapper -- except a
--      modulo-cons call, redirected in-pass to bare `@run<r>(@Call-fj(args,
--      @ev))` (see "Tail calls vs. inlining" below for why only that case
--      needs it);
--    * a thin `forceinline` wrapper per member that keeps its signature as the
--      external entry point, entering `@run` at `r := fi-target` with the
--      reflexive witness `refl/type-eq`.
--
--   > fun my-even( n : int ) : div bool = if n == 0 then True  else my-odd(n - 1)
--   > fun my-odd ( n : int ) : div bool = if n == 0 then False else my-even(n - 1)
--
--   becomes
--
--   > value type @mutrec-my-even<r>
--   >   @Call-my-even( n : int, @ev : @type-eq<bool,r> )
--   >   @Call-my-odd ( n : int, @ev : @type-eq<bool,r> )
--   >
--   > fun @run-my-even<r>( @b : @mutrec-my-even<r> ) : div r
--   >   match @b
--   >     @Call-my-even(n,@ev) -> if n == 0 then @coerce(True, @ev)
--   >                                       else @run-my-even(@Call-my-odd(n - 1, @ev))
--   >     @Call-my-odd (n,@ev) -> if n == 0 then @coerce(False, @ev)
--   >                                       else @run-my-even(@Call-my-even(n - 1, @ev))
--   >
--   > forceinline fun my-even( n : int ) : div bool = @run-my-even<bool>(@Call-my-even(n, refl))
--   > forceinline fun my-odd ( n : int ) : div bool = @run-my-even<bool>(@Call-my-odd (n, refl))
--
--   The result index is generalized by a `Mode`: member results are seen as
--   `W<element>` for a shared outer wrapper `W`, with `r` indexing the element
--   and the driver returning `W<r>`. The default `W` is the identity (then `r`
--   is the whole result, as above); a real wrapper (e.g. `list`) is tried
--   whenever all results share one single-argument type constructor -- not
--   only when a modulo-cons call is already visible, since later inlining can
--   expose one -- falling back to the identity `W` if that turns out not to
--   rebuild (see `fuseDefRec`).
--
--   Tail calls vs. inlining
--   -----------------------
--   No sibling call is special-cased by this pass except a modulo-cons
--   rebuild; every other sibling call, tail or not, is left as a call to the
--   member's forceinline wrapper (wrappers are emitted *before* `@run` so the
--   single forward-pass inliner can see them). A non-tail call inlines to
--   exactly the term this pass would have built itself, so those sites need
--   nothing special.
--
--   A tail call recovers differently: inlining its wrapper alone produces
--   `@coerce( @run-ping<int>( @Call-pong(n-1,refl) ), @ev )` -- the recursive
--   call trapped under a coerce. `Core.Simplify`'s `@coerce` rule fuses
--   exactly this shape into the bare tail form
--   `@run-ping<r>( @Call-pong(n-1,@ev) )`, retagging the index using the
--   reflexive witness (sound by type erasure: every witness erases to the
--   unit `@TypeEq`) -- which is why this pass does not need to redirect
--   ordinary tail calls itself. `Compile.TypeCheck` runs `Core.Inline` right
--   after this pass to inline the wrappers and trigger that fusion, so the
--   fused driver's constant-stack guarantee holds independent of the general
--   optimizer settings.
--
--   A modulo-cons rebuild is the one case that must still redirect in-pass:
--   deferring it would leave an ill-typed intermediate (the rebuilt
--   constructor's hole needs `W<r>`, but only an element witness exists at
--   that point).
--
--   Runs in Compile.TypeCheck right after type checking -- after `unreturn`
--   (the tail walk does not descend through `return` nodes) and the initial
--   simplify (which removes the identity `@open`s around sibling calls).
-----------------------------------------------------------------------------
module Core.Fusion ( fuseMutRec ) where

import Control.Applicative ( (<|>) )
import Data.List          ( elemIndex )
import qualified Data.Map.Strict as M

import Common.Name
import Common.NamePrim    ( nameTpTypeEq, nameCoerce, nameRefl )
import Common.Range       ( rangeNull )
import Common.Unique      ( HasUnique(..) )
import Common.Syntax      ( Visibility(..), DataKind(..), DataEffect(..), Platform, valueReprNew
                          , DataDef(..), DefSort(..), DefInline(..), noFip, valueReprScanCount )

import Kind.Kind          ( Kind, kindStar, kindFun )
import Kind.Newtypes      ( Newtypes, newtypesLookupAny )
import Kind.Repr          ( createDataDefMin )

import Type.Type
import Type.TypeVar       ( ftv, tvsList, subNew, (|->) )

import qualified Type.Pretty as Pretty

import Core.Core
import Core.Pretty        ()  -- HasTypeVar instances for Expr/Def (orphan instances)

--------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------

-- | Rewrite every mutually recursive top-level `DefRec` group into a GADT
--   driver + wrappers. Returns the synthesized mr types for the caller to
--   append to coreProgTypeDefs.
fuseMutRec :: Pretty.Env -> Newtypes -> Platform -> CorePhase b [TypeDefGroup]
fuseMutRec penv newtypes platform
  = do defs <- getCoreDefs
       uniq <- unique
       let (defs', typeDefs, uniq') = runFuse newtypes platform uniq defs
       setUnique uniq'
       setCoreDefs defs'
       return typeDefs

-- | Only transform the top-level DefGroups list (never descends into
--   Case/Let bodies), so local/nested recursion is left untouched.
runFuse :: Newtypes -> Platform -> Int -> DefGroups -> (DefGroups, [TypeDefGroup], Int)
runFuse newtypes platform uniq defs
  = case makeTypeEq newtypes of
      Nothing     -> (defs, [], uniq)   -- std/core/types not loaded (should not happen post-typecheck)
      Just typeEq -> foldr (transformGroup typeEq) ([], [], uniq) defs
  where
    transformGroup typeEq dg (accDefs, accTypes, u)
      = case dg of
          DefRec ds | length ds >= 2
            , Just (grps, tdef, u') <- fuseDefRec newtypes platform typeEq u ds
            -> (grps ++ accDefs, tdef : accTypes, u')
          _ -> (dg : accDefs, accTypes, u)

--------------------------------------------------------------------------
-- The transformation
--------------------------------------------------------------------------

-- | Fuse a whole mutually recursive `DefRec` (all members) into one mr
--   type, one `@run` driver, and one wrapper per member. Nothing if a member is
--   not a function or the members' result effects cannot be unified.
--
--   Tries the wrapper mode first (`wrapperCandidate`), falling back to the
--   identity mode if the wrapper rebuild fails on some leaf. The identity
--   mode's rebuild never fails, so this always finds a fusion when one exists.
fuseDefRec :: Newtypes -> Platform -> TypeEq -> Int -> [Def]
                 -> Maybe ([DefGroup], TypeDefGroup, Int)
fuseDefRec newtypes platform typeEq uniq defs
  = build wrapperCandidate <|> build (const identityMode)
  where
    build chooseMode
      = do (mr, uniq') <- makeMutRecInfo newtypes platform typeEq uniq defs chooseMode
           (runDef, uniq'') <- makeRunDef mr defs uniq'  -- Nothing if a tail leaf cannot be rebuilt at `r`
           let wrappers = map (makeWrapperDef mr runDef) defs
           -- Tail calls are redirected inside @run (making it genuinely
           -- self-recursive, hence DefRec), while non-tail sibling calls still name
           -- the forceinline wrappers. Emit the wrappers BEFORE @run: the inliner
           -- is a single forward pass (a def is only inlineable into later defs),
           -- so this definition-before-use order is what lets it collapse those
           -- wrapper calls into direct @run calls. (Top-level defs are not scoped
           -- by group order, so the wrappers may freely reference the later @run.)
           -- See "Tail calls vs. inlining" in the module header for why only the
           -- non-tail sites can be delegated to the inliner.
           return ( map DefNonRec wrappers ++ [DefRec [runDef]]
                  , TypeDefGroup [Data (mrDataInfo mr)]
                  , uniq'' )

--------------------------------------------------------------------------
-- The type-equality witness primitives (from std/core/types)
--------------------------------------------------------------------------

-- | Builders for the three pieces the transform borrows from `std/core/types`:
--   the `@type-eq<a,b>` type, the *reflexive* witness `refl : forall<a>. @type-eq<a,a>`,
--   and the `@coerce` cast that consumes a witness. Non-reflexive witnesses are
--   never fabricated here -- the driver's branches reuse the pattern-bound `@ev`.
data TypeEq = TypeEq
  { teqType   :: Type -> Type -> Type                    -- ^ @type-eq<a,b>
  , teqRefl   :: Type -> Expr                            -- ^ refl<a> : @type-eq<a,a>
  , teqCoerce :: Type -> Type -> Expr -> Expr -> Expr    -- ^ @coerce<a,b>(x,ev) : b
  }

makeTypeEq :: Newtypes -> Maybe TypeEq
makeTypeEq newtypes
  = do dataInfo <- newtypesLookupAny nameTpTypeEq newtypes
       case dataInfoConstrs dataInfo of
         [_] ->
           let tcon        = TypeCon nameTpTypeEq (dataInfoKind dataInfo)
               teqType a b  = TApp (TCon tcon) [a,b]
               -- refl : forall<a>. () -> @type-eq<a,a> (from std/core/types)
               ra           = TypeVar 0 kindStar Bound
               reflTp       = TForall [ra] (TFun [] typeTotal (teqType (TVar ra) (TVar ra)))
               reflVar      = Var (TName nameRefl reflTp) (InfoArity 1 0)
               teqRefl a    = App (TypeApp reflVar [a]) []
               -- @coerce : forall<a,b>. (a, @type-eq<a,b>) -> b, built inline like @open
               ca           = TypeVar 0 kindStar Bound
               cb           = TypeVar 1 kindStar Bound
               coerceTp     = TForall [ca,cb]
                                (TFun [(nameNil,TVar ca),(nameNil,teqType (TVar ca) (TVar cb))]
                                      typeTotal (TVar cb))
               coerceVar    = Var (TName nameCoerce coerceTp) (InfoExternal "#1")
               teqCoerce a b x ev = App (TypeApp coerceVar [a,b]) [x,ev]
           in Just (TypeEq teqType teqRefl teqCoerce)
         _ -> Nothing

--------------------------------------------------------------------------
-- Aligning members onto the shared mr type
--------------------------------------------------------------------------

-- | How member results relate to the driver's result index `r`: every result
--   is seen as `W<element>` for one shared outer wrapper `W`, `r` indexes the
--   element, and the driver returns `W<r>`. The default `W` is the *identity*
--   (`r` is the whole result and every tail leaf is a single element); a real
--   single-argument wrapper (e.g. `list`) is tried whenever all results share
--   it (`wrapperCandidate`), with `fuseDefRec` falling back to the identity
--   mode if that fails to rebuild. The driver's concrete `W<r>` result is what
--   lets ctail's TRMC engage on a modulo-cons call (it rejects a bare `r`),
--   turning `Cons(x, f(xs))` into `Cons(@coerce(x), @run(..):W<r>)`.
data Mode = Mode
  { modeWrap :: Type -> Type    -- ^ r |-> W<r>   (identity when no wrapper found)
  , modeElem :: Type -> Type    -- ^ W<e> |-> e   (identity when no wrapper found)
  }

identityMode :: Mode
identityMode = Mode id id

-- | Everything the driver, its branches, and the wrappers need about one member.
data MemberInfo = MemberInfo
  { miCon     :: ConInfo         -- ^ this member's @Call-* constructor
  , miRepr    :: ConRepr
  , miTvs     :: [TypeVar]       -- ^ the member's own type parameters (original order)
  , miExists  :: [TypeVar]       -- ^ the member's value type parameters (existential in @Call-*)
  , miEffTvs  :: [TypeVar]       -- ^ the member's type parameters that fill the shared effect params
  , miArgs    :: [TName]         -- ^ argument binders, effect-substituted onto shared params
  , miResult  :: Type            -- ^ full result type `W<element>`, effect-substituted
  , miTarget  :: Type            -- ^ the element (the witness's first index); = miResult for identity `W`
  , miBody    :: Expr            -- ^ member body (type lambda stripped), effect-substituted
  }

data MutRecInfo = MutRecInfo
  { mrDataInfo  :: DataInfo
  , mrType      :: Type            -- ^ @mutrec-*<mrParams>
  , mrParams    :: [TypeVar]       -- ^ shared effect parameters ++ [result index]
  , mrResultIdx :: TypeVar         -- ^ the result index `r` (last of mrParams)
  , mrResultTp  :: Type            -- ^ the driver's result type `W<r>`
  , mrEff       :: Effect          -- ^ the driver's (shared) result effect
  , mrMode      :: Mode
  , mrMembers   :: M.Map Name MemberInfo
  , mrTypeEq    :: TypeEq
  }

-- | Each shared parameter keeps its own kind: the result index is `*`, an
--   effect parameter is `E`.
paramsKind :: [TypeVar] -> Kind
paramsKind = foldr (\tv k -> kindFun (typevarKind tv) k) kindStar

-- | One member split into the pieces alignment needs.
data Part = Part
  { partName :: Name
  , partTvs  :: [TypeVar]
  , partEff  :: Effect
  , partRes  :: Type
  , partArgs :: [TName]
  , partBody :: Expr
  }

-- | One member after its effect variables are mapped onto the shared parameters.
data Aligned = Aligned
  { aName    :: Name
  , aTvs     :: [TypeVar]
  , aExists  :: [TypeVar]
  , aEffTvs  :: [TypeVar]
  , aArgs    :: [TName]
  , aFields  :: [(Name,Type)]
  , aResult  :: Type
  , aBody    :: Expr
  }

{- The mr value type: one constructor per member, over a single result
   index `r`. A member's own type parameters become existentials; the extra
   witness field records how that member's result relates to `r`.

   value type @mutrec-f<a,r>              -- `a` shared via the effect, `r` the index
     @Call-f<>( x : a, n : int,  @ev : @type-eq<int,r> )
     @Call-g<b>( s : b,          @ev : @type-eq<list<b>,r> )   -- b existential

   Nothing if a member is not a function, or the members' result effects cannot
   be unified (`@run` needs one effect).
-}
makeMutRecInfo :: Newtypes -> Platform -> TypeEq -> Int -> [Def] -> ([Type] -> Mode) -> Maybe (MutRecInfo, Int)
makeMutRecInfo newtypes platform typeEq uniq0 defs chooseMode
  = do parts <- mapM memberParts defs
       -- shared parameters: the type variables free in the result effect, taken
       -- from the first member and aligned by position across the rest; then a
       -- fresh result index `r`. Minted from the running unique counter, exactly
       -- as Type.Operations.freshTVar would (it also allocates via `uniqueId`,
       -- the same HasUnique counter CorePhase threads into this function) --
       -- NOT by scanning past the highest type variable id seen locally, which
       -- cannot see ids already allocated elsewhere in the module or by another
       -- fused group and so could mint a colliding, lasting bound type variable.
       let first     = head parts
           effTvs0   = [ tv | tv <- tvsList (ftv (partEff first)), tv `elem` partTvs first ]
           sharedEff = [ TypeVar (uniq0 + i) (typevarKind tv) Bound
                       | (i,tv) <- zip [0..] effTvs0 ]
           resultIdx = TypeVar (uniq0 + length effTvs0) kindStar Bound
           uniq      = uniq0 + length effTvs0 + 1
           mrParams = sharedEff ++ [resultIdx]
           driverEff = subNew (zip effTvs0 (map TVar sharedEff)) |-> partEff first
       aligned <- mapM (alignMember sharedEff driverEff) parts
       let mode       = chooseMode (map aResult aligned)
           resultTp   = modeWrap mode (TVar resultIdx)
           targetOf a = modeElem mode (aResult a)   -- the witness's first index
       let mrName = makeHiddenName "mutrec" (partName first)
           mrTp   = TApp (TCon (TypeCon mrName (paramsKind mrParams))) (map TVar mrParams)
           witnessField a = (newHiddenName "ev", teqType typeEq (targetOf a) (TVar resultIdx))
           callCon0 tag a
             = let fields = aFields a ++ [witnessField a]
               in ConInfo { conInfoName          = makeHiddenName "Call" (aName a)
                          , conInfoTypeName      = mrName
                          , conInfoForalls       = mrParams
                          , conInfoExists        = aExists a
                          , conInfoParams        = fields
                          , conInfoType          = tForall (mrParams ++ aExists a) (TFun fields typeTotal mrTp)
                          , conInfoTypeSort      = Inductive
                          , conInfoRange         = rangeNull
                          , conInfoParamRanges   = map (const rangeNull) fields
                          , conInfoParamVis      = map (const Public) fields
                          , conInfoSingleton     = False
                          , conInfoOrderedParams = fields
                          , conInfoValueRepr     = valueReprNew 0 0 0
                          , conInfoLazy          = Nothing
                          , conInfoTag           = tag
                          , conInfoVis           = Private
                          , conInfoDoc           = ""
                          }
           conInfos0 = zipWith callCon0 [1..] aligned
       -- compute the value representation (a multi-constructor value type needs a
       -- struct _tag discriminant slot, unless every constructor is a nullary enum)
       let lookupDataInfo n = Just (newtypesLookupAny n newtypes)
           noEmit _         = Just ()
           maxFields        = maximum (0 : map (length . conInfoParams) conInfos0)
           extraFields      = if maxFields >= 1 then 1 else 0
           makeDataDef minScan = createDataDefMin noEmit noEmit lookupDataInfo platform
                                   mrName True False Inductive extraFields minScan
                                   (DataDefValue (valueReprNew 0 0 0)) conInfos0
       (ddef0, conInfos1) <- makeDataDef 0
       -- constructors with differing scan-field counts yield DataDefNormal; retry
       -- with minScanCount = the maximum so that padding fields get appended
       (ddef, conInfos) <- case ddef0 of
         DataDefValue _ -> return (ddef0, conInfos1)
         _ -> do let maxScan = maximum (0 : map (valueReprScanCount . conInfoValueRepr) conInfos1)
                 (ddef1, conInfos2) <- makeDataDef maxScan
                 case ddef1 of
                   DataDefValue _ -> return (ddef1, conInfos2)
                   _              -> Nothing  -- give up: leave the group alone
       let dataInfo = DataInfo
             { dataInfoSort    = Inductive
             , dataInfoName    = mrName
             , dataInfoKind    = paramsKind mrParams
             , dataInfoParams  = mrParams
             , dataInfoConstrs = conInfos
             , dataInfoRange   = rangeNull
             , dataInfoDef     = ddef
             , dataInfoEffect  = DataNoEffect
             , dataInfoIsRec   = False
             , dataInfoVis     = Private
             , dataInfoDoc     = ""
             }
           members = M.fromList
             [ (aName a, MemberInfo { miCon    = con
                                    , miRepr   = getConRepr dataInfo con
                                    , miTvs    = aTvs a
                                    , miExists = aExists a
                                    , miEffTvs = aEffTvs a
                                    , miArgs   = aArgs a
                                    , miResult = aResult a
                                    , miTarget = targetOf a
                                    , miBody   = aBody a })
             | (a, con) <- zip aligned conInfos ]
       return ( MutRecInfo { mrDataInfo  = dataInfo
                           , mrType      = mrTp
                           , mrParams    = mrParams
                           , mrResultIdx = resultIdx
                           , mrResultTp  = resultTp
                           , mrEff       = driverEff
                           , mrMode      = mode
                           , mrMembers   = members
                           , mrTypeEq    = typeEq }
              , uniq )
  where
    memberParts d
      = do (tvs, _, eff, res) <- splitFunScheme (defType d)
           (_,   args, _, body) <- splitFunExpr (defExpr d)
           return (Part (defName d) tvs eff res args body)

    -- Map a member's effect variables onto the shared parameters (positional,
    -- by kind). The value type variables are left as-is and become existentials.
    -- Fails when the effect rows cannot be unified onto the shared parameters.
    alignMember sharedEff driverEff p
      = do let effTvs = [ tv | tv <- tvsList (ftv (partEff p)), tv `elem` partTvs p ]
           if map typevarKind effTvs /= map typevarKind sharedEff then Nothing else do
             let sub    = subNew (zip effTvs (map TVar sharedEff))
             if not (matchEffect (sub |-> partEff p) driverEff) then Nothing else do
               let exists = filter (`notElem` effTvs) (partTvs p)
                   args   = [ TName (getName a) (sub |-> tnameType a) | a <- partArgs p ]
               return Aligned { aName   = partName p
                              , aTvs    = partTvs p
                              , aExists = exists
                              , aEffTvs = effTvs
                              , aArgs   = args
                              , aFields = [ (getName a, tnameType a) | a <- args ]
                              , aResult = sub |-> partRes p
                              , aBody   = sub |-> partBody p }

--------------------------------------------------------------------------
-- Building the driver and the wrappers
--------------------------------------------------------------------------

-- | `@Call-fi<tyArgs>(argExprs, witness)`.
makeCallApp :: MemberInfo -> [Type] -> [Expr] -> Expr -> Expr
makeCallApp mi tyArgs argExprs witness
  = let con = Con (TName (conInfoName (miCon mi)) (conInfoType (miCon mi))) (miRepr mi)
    in App (TypeApp con tyArgs) (argExprs ++ [witness])

-- | `@run<tyArgs>(callExpr)`.
makeRunApp :: MutRecInfo -> TName -> [Type] -> Expr -> Expr
makeRunApp mr runTName tyArgs callExpr
  = let run = Var runTName (InfoArity (length (mrParams mr)) 1)
    in App (TypeApp run tyArgs) [callExpr]

{- The fused, self-recursive-through-wrappers driver: one match branch per
   member holding that member's body verbatim, coerced to the shared index `r`.
   No calls are rewritten -- the body's calls to sibling members still name the
   (forceinline) wrappers.

   fun @run-f<r>( @b : @mutrec-f<r> ) : <eff> r
     match @b
       @Call-f(x1..xn,@ev) -> @coerce(<f's body>, @ev)
       @Call-g(y1..ym,@ev) -> @coerce(<g's body>, @ev)
-}
makeRunDef :: MutRecInfo -> [Def] -> Int -> Maybe (Def, Int)
makeRunDef mr members uniq
  = do let params    = mrParams mr
           runName   = makeHiddenName "run" (defName (head members))
           mrTp  = mrType mr
           runType   = tForall params (TFun [(newHiddenName "b", mrTp)]
                                            (mrEff mr) (mrResultTp mr))
           runTName  = TName runName runType
           param     = TName (newHiddenName "b") mrTp
           -- the final branch needs no tag check: no alternatives remain
           isLast    = replicate (length members - 1) False ++ [True]
           mis       = [ mrMembers mr M.! defName d | d <- members ]
       branches <- sequence (zipWith (makeFusedBranch mr runTName) isLast mis)
       let runBody = addTypeLambdas params (Lam [param] (mrEff mr) (Case [Var param InfoNone] branches))
       return ( Def { defName      = runName
                    , defType      = runType
                    , defExpr      = runBody
                    , defVis       = Private
                    , defSort      = DefFun [] noFip
                    , defInline    = InlineNever
                    , defNameRange = rangeNull
                    , defDoc       = ""
                    }
              , uniq )

-- | `@Call-fi(x1..xk,@ev) -> <body producing the driver result W<r>>`, reusing
--   fi's own parameter TNames as the pattern variables (its @Call fields were
--   built from exactly those), plus a fresh binder for the equality witness.
--   Nothing if a tail leaf cannot be rebuilt at `r`.
makeFusedBranch :: MutRecInfo -> TName -> Bool -> MemberInfo -> Maybe Branch
makeFusedBranch mr runTName isLast mi
  = do let callCon = miCon mi
           evType  = teqType (mrTypeEq mr) (miTarget mi) (TVar (mrResultIdx mr))
           evTName = TName (newHiddenName "ev") evType
           patVars = miArgs mi ++ [evTName]
           pat = PatCon { patConName     = TName (conInfoName callCon) (conInfoType callCon)
                        , patConPatterns = [ PatVar tn PatWild | tn <- patVars ]
                        , patConRepr     = miRepr mi
                        , patTypeArgs    = map snd (conInfoParams callCon)
                        , patExists      = miExists mi
                        , patTypeRes     = mrType mr
                        , patConInfo     = callCon
                        , patConSkip     = isLast
                        }
       body <- fuseBody mr runTName mi evTName (miBody mi)
       return (Branch [pat] [Guard exprTrue body])


--------------------------------------------------------------------------
-- Rebuilding member bodies to produce the driver result `W<r>`
--------------------------------------------------------------------------

-- | Pick the type arguments of a call at the given member type variables (its
--   effect params, or its existentials). `targs` are aligned to the member's
--   original parameters (Core always fully instantiates a polymorphic
--   reference, so `targs` should have exactly as many entries as the member's
--   own type parameters) -- Nothing if that ever fails to hold, rather than
--   silently dropping the type variables that don't line up.
valueTyArgs :: [TypeVar] -> MemberInfo -> [Type] -> Maybe [Type]
valueTyArgs tvs mi targs
  = mapM pick tvs
  where
    pick tv = do i <- elemIndex tv (miTvs mi)
                 if i < length targs then Just (targs !! i) else Nothing

-- | Branch body producing the driver result `W<r>`: walk down the control flow
--   (Let/Case) and rebuild each tail leaf. Non-tail sibling calls are left as
--   (forceinline) wrapper calls. Nothing if a tail leaf cannot be rebuilt.
fuseBody :: MutRecInfo -> TName -> MemberInfo -> TName -> Expr -> Maybe Expr
fuseBody mr runTName mi evTName = walk
  where
    walk expr = case expr of
      Let dgs body -> Let dgs <$> walk body
      Case ss brs  -> Case ss <$> mapM walkBranch brs
      _            -> rebuildAtR mr runTName mi evTName expr
    walkBranch (Branch ps gs) = Branch ps <$> mapM (\(Guard g e) -> Guard g <$> walk e) gs

-- | A sibling call in tail (or modulo-cons) position redirected into the driver
--   at the driver's own index `r`, producing `W<r>` directly -- valid exactly
--   there, since a tail call's result is the branch result, which also means
--   the pattern-bound witness `ev` already has the constructor field's type, so
--   no non-reflexive witness is fabricated. The result: the recursive call sits
--   bare in tail position (no coerce or box/unbox around it) and loops. This is
--   a re-typing at `r`, not an unfolding, which is why it must happen in-pass
--   while non-tail sites are left to the inliner -- see "Tail calls vs.
--   inlining" in the module header for the worked example.
redirectTailCall :: MutRecInfo -> TName -> Expr -> TName -> [Type] -> [Expr] -> Maybe Expr
redirectTailCall mr runTName ev tn targs args
  = do target  <- M.lookup (getName tn) (mrMembers mr)
       valArgs <- valueTyArgs (miExists target) target targs
       let conTyArgs = map TVar (mrParams mr) ++ valArgs
           callExpr  = makeCallApp target conTyArgs args ev
       Just (makeRunApp mr runTName (map TVar (mrParams mr)) callExpr)

-- | Rebuild one tail leaf of type `W<elem>` into the driver result `W<r>`:
--   a sibling call becomes `@run(..):W<r>`, a wrapper constructor is rebuilt at
--   `r` with its element fields coerced, and an element-typed leaf is coerced
--   to `r`. With the identity wrapper `elem` IS the whole result, so the last
--   case coerces every remaining leaf and rebuilding never fails; a real
--   wrapper fails on a leaf it cannot rebuild (e.g. an opaque `W<elem>` value)
--   -- `fuseDefRec` then retries the group in identity mode.
rebuildAtR :: MutRecInfo -> TName -> MemberInfo -> TName -> Expr -> Maybe Expr
rebuildAtR mr runTName mi evTName = rebuild
  where
    elemTp = miTarget mi                       -- the wrapper element type
    wrapTp = miResult mi                       -- W<elemTp>
    identityWrapper = eqType elemTp wrapTp     -- W = id: every leaf is an element
    r      = TVar (mrResultIdx mr)
    ev     = Var evTName InfoNone

    -- With the identity wrapper every leaf IS the element and is simply
    -- coerced -- including sibling tail calls: the call is inlined to
    -- `@run<Tj>(@Call(args, refl))` by the inliner, and the generic
    -- coerce-fusion rule in Core.Simplify retags it to the tail form
    -- `@run<r>(@Call(args, @ev))` -- see "Tail calls vs. inlining" in the
    -- module header. A real wrapper rebuilds (and redirects) in-pass:
    -- deferring would leave the rebuilt constructor's hole ill-typed
    -- (`W<r>` needed, only an element witness exists).
    rebuild expr
      | identityWrapper = Just (coerceElem expr)
      | otherwise
      = case expr of
          App (Var tn _) args
            | Just red <- redirectTailCall mr runTName ev tn [] args -> Just red
          App (TypeApp (Var tn _) targs) args
            | Just red <- redirectTailCall mr runTName ev tn targs args -> Just red
          App (TypeApp (Con c repr) [telem]) fields
            | eqType telem elemTp
            -> do fields' <- mapM rebuildField fields
                  Just (App (TypeApp (Con c repr) [r]) fields')
          TypeApp (Con c repr) [telem]              -- nullary wrapper con (e.g. Nil)
            | eqType telem elemTp
            -> Just (TypeApp (Con c repr) [r])
          _ -> Nothing

    coerceElem e = teqCoerce (mrTypeEq mr) elemTp r e ev

    rebuildField f
      | eqType (typeOf f) elemTp       = Just (coerceElem f)   -- element -> r
      | eqType (typeOf f) wrapTp       = rebuild f             -- W<elem> -> W<r>
      | disjointTvs (typeOf f) elemTp  = Just f                -- unaffected
      | otherwise                      = Nothing

    disjointTvs a b = null [ () | x <- tvsList (ftv a), x `elem` tvsList (ftv b) ]

--------------------------------------------------------------------------
-- Choosing the mode
--------------------------------------------------------------------------

-- | Extract the single type argument of a wrapper application `W<elem>`.
wrapperElem :: Type -> Type
wrapperElem (TApp _ [e]) = e
wrapperElem tp           = tp   -- unreachable once a real wrapper is chosen

-- | The wrapper mode candidate: real iff every member's result shares one
--   single-argument outer type constructor `W`. `fuseDefRec` tries this first
--   and falls back to the identity mode if it fails to rebuild -- so this
--   does not need to check for a modulo-cons call itself.
wrapperCandidate :: [Type] -> Mode
wrapperCandidate results
  | sameWrapper = Mode (\r -> TApp (TCon wcon) [r]) wrapperElem
  | otherwise   = identityMode
  where
    wrappers    = [ (w,e) | TApp (TCon w) [e] <- results ]
    sameWrapper = length wrappers == length results
                    && all ((== typeConName wcon) . typeConName . fst) wrappers
    wcon        = fst (head wrappers)

{- Replace a member's body with a `forceinline` call into the driver, keeping
   its own signature. The mr type is driven at `r := this member's result`,
   so the witness is reflexive; forceinline lets later inlining collapse the
   @run <-> wrappers cycle into a directly self-recursive @run.

   forceinline fun g<b>( y1..ym : .. ) : <eff> res
     @run-f<b's effect args, res>(@Call-g<..>(y1..ym, @TypeEq))
-}
makeWrapperDef :: MutRecInfo -> Def -> Def -> Def
makeWrapperDef mr runDef member
  = case (splitFunExpr (defExpr member), M.lookup (defName member) (mrMembers mr)) of
      (Just (tvs, args, eff, _), Just mi) ->
        let Just (_,_,_,res) = splitFunScheme (defType member)
            runTName  = TName (defName runDef) (defType runDef)
            -- drive the mr type at (this member's effect args, its element)
            driveTgt  = modeElem (mrMode mr) res
            runTyArgs = map TVar (miEffTvs mi) ++ [driveTgt]
            conTyArgs = runTyArgs ++ map TVar (miExists mi)
            witness   = teqRefl (mrTypeEq mr) driveTgt
            argExprs  = map (\tn -> Var tn InfoNone) args
            callExpr  = makeCallApp mi conTyArgs argExprs witness
            runCall   = makeRunApp mr runTName runTyArgs callExpr
        in member{ defExpr = addTypeLambdas tvs (Lam args eff runCall), defInline = InlineAlways }
      _ -> member

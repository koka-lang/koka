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
--      `@type-eq<fi-result, r>` witness (see `std/core/types`); fi's own value
--      type variables become existentials of `@Call-fi`, so members may differ
--      in result type, type parameter count/order, and instantiation;
--    * one self-recursive driver `@run` with one branch per member, each
--      simply `@coerce`ing its member's body (verbatim, calls to siblings
--      included) to `r`;
--    * a thin `forceinline` wrapper per member that keeps its signature as the
--      external entry point, entering `@run` at `r := fi-result` with the
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
--   >                                       else @coerce(my-odd(n - 1), @ev)
--   >     @Call-my-odd (n,@ev) -> if n == 0 then @coerce(False, @ev)
--   >                                       else @coerce(my-even(n - 1), @ev)
--   >
--   > forceinline fun my-even( n : int ) : div bool = @run-my-even<bool>(@Call-my-even(n, refl))
--   > forceinline fun my-odd ( n : int ) : div bool = @run-my-even<bool>(@Call-my-odd (n, refl))
--
--   Tail calls vs. inlining
--   -----------------------
--   `@run-my-even` above is not self-recursive yet -- `my-odd(n-1)` still
--   names the wrapper. Inlining that wrapper produces
--   `@coerce( @run-my-even<bool>( @Call-my-odd(n-1,refl) ), @ev )`: the
--   recursive call trapped under a coerce, not in tail position. The needed
--   form, `@run-my-even<r>( @Call-my-odd(n-1,@ev) )`, re-*types* the call at
--   the branch's own index `r` with the pattern-bound witness; that is a
--   typing change, not an unfolding, so no amount of inlining alone produces
--   it. `Core.Simplify`'s generic `@coerce` rules do, though:
--
--   >   @coerce( f<T>(C(args,refl)), ev )  ~>  f<r>(C(args,ev))
--
--   sound by type erasure, since every witness erases to the unit `@TypeEq`.
--   So the inline-then-simplify pipeline (wrappers are emitted *before*
--   `@run` so the single forward-pass inliner can see them) is what turns
--   this pass's output into an actually self-recursive, constant-stack `@run`.
--
--   This pass does not yet fuse a sibling call *nested* under a constructor
--   (`Cons(x, f(xs))`, "modulo cons") into the loop -- `f(xs)` there is a
--   plain non-tail call, correct but O(n) stack, same as before fusion.
--
--   Runs in Compile.TypeCheck right after type checking -- after `unreturn`
--   (the tail walk does not descend through `return` nodes) and the initial
--   simplify (which removes the identity `@open`s around sibling calls).
-----------------------------------------------------------------------------
module Core.Fusion ( fuseMutRec ) where

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
fuseDefRec :: Newtypes -> Platform -> TypeEq -> Int -> [Def]
                 -> Maybe ([DefGroup], TypeDefGroup, Int)
fuseDefRec newtypes platform typeEq uniq defs
  = do (mr, uniq') <- makeMutRecInfo newtypes platform typeEq uniq defs
       let (runDef, uniq'') = makeRunDef mr defs uniq'
           wrappers = map (makeWrapperDef mr runDef) defs
       -- @run only becomes genuinely self-recursive once the inliner collapses
       -- a sibling's wrapper call into a direct @run call and Core.Simplify's
       -- coerce rules retag it at tail position (see "Tail calls vs. inlining"
       -- in the module header); it is marked DefRec regardless, since
       -- Core.Optimize re-establishes that grouping before ctail either way.
       -- Emit the wrappers BEFORE @run: the inliner is a single forward pass
       -- (a def is only inlineable into later defs), so this
       -- definition-before-use order is what lets it collapse those wrapper
       -- calls into direct @run calls. (Top-level defs are not scoped by
       -- group order, so the wrappers may freely reference the later @run.)
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

-- | Everything the driver, its branches, and the wrappers need about one member.
data MemberInfo = MemberInfo
  { miCon     :: ConInfo         -- ^ this member's @Call-* constructor
  , miRepr    :: ConRepr
  , miTvs     :: [TypeVar]       -- ^ the member's own type parameters (original order)
  , miExists  :: [TypeVar]       -- ^ the member's value type parameters (existential in @Call-*)
  , miEffTvs  :: [TypeVar]       -- ^ the member's type parameters that fill the shared effect params
  , miArgs    :: [TName]         -- ^ argument binders, effect-substituted onto shared params
  , miResult  :: Type            -- ^ the member's own result type, effect-substituted; also the
                                  --   witness's first index (the driver's result index is the second)
  , miBody    :: Expr            -- ^ member body (type lambda stripped), effect-substituted
  }

data MutRecInfo = MutRecInfo
  { mrDataInfo  :: DataInfo
  , mrType      :: Type            -- ^ @mutrec-*<mrParams>
  , mrParams    :: [TypeVar]       -- ^ shared effect parameters ++ [result index]
  , mrResultIdx :: TypeVar         -- ^ the result index `r` (last of mrParams)
  , mrEff       :: Effect          -- ^ the driver's (shared) result effect
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
makeMutRecInfo :: Newtypes -> Platform -> TypeEq -> Int -> [Def] -> Maybe (MutRecInfo, Int)
makeMutRecInfo newtypes platform typeEq uniq0 defs
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
       let mrName = makeHiddenName "mutrec" (partName first)
           mrTp   = TApp (TCon (TypeCon mrName (paramsKind mrParams))) (map TVar mrParams)
           witnessField a = (newHiddenName "ev", teqType typeEq (aResult a) (TVar resultIdx))
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
                                    , miBody   = aBody a })
             | (a, con) <- zip aligned conInfos ]
       return ( MutRecInfo { mrDataInfo  = dataInfo
                           , mrType      = mrTp
                           , mrParams    = mrParams
                           , mrResultIdx = resultIdx
                           , mrEff       = driverEff
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
makeRunDef :: MutRecInfo -> [Def] -> Int -> (Def, Int)
makeRunDef mr members uniq
  = let params    = mrParams mr
        runName   = makeHiddenName "run" (defName (head members))
        mrTp  = mrType mr
        runType   = tForall params (TFun [(newHiddenName "b", mrTp)]
                                         (mrEff mr) (TVar (mrResultIdx mr)))
        runTName  = TName runName runType
        param     = TName (newHiddenName "b") mrTp
        -- the final branch needs no tag check: no alternatives remain
        isLast    = replicate (length members - 1) False ++ [True]
        mis       = [ mrMembers mr M.! defName d | d <- members ]
        branches  = zipWith (makeFusedBranch mr) isLast mis
        runBody   = addTypeLambdas params (Lam [param] (mrEff mr) (Case [Var param InfoNone] branches))
    in ( Def { defName      = runName
             , defType      = runType
             , defExpr      = runBody
             , defVis       = Private
             , defSort      = DefFun [] noFip
             , defInline    = InlineNever
             , defNameRange = rangeNull
             , defDoc       = ""
             }
       , uniq )

-- | `@Call-fi(x1..xk,@ev) -> <body coerced to the driver result r>`, reusing
--   fi's own parameter TNames as the pattern variables (its @Call fields were
--   built from exactly those), plus a fresh binder for the equality witness.
makeFusedBranch :: MutRecInfo -> Bool -> MemberInfo -> Branch
makeFusedBranch mr isLast mi
  = let callCon = miCon mi
        evType  = teqType (mrTypeEq mr) (miResult mi) (TVar (mrResultIdx mr))
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
        body = fuseBody mr mi evTName (miBody mi)
    in Branch [pat] [Guard exprTrue body]


--------------------------------------------------------------------------
-- Rebuilding member bodies to produce the driver result `r`
--------------------------------------------------------------------------

-- | Branch body producing the driver result `r`: walk down the control flow
--   (Let/Case) and coerce each tail leaf, verbatim, to `r`.
fuseBody :: MutRecInfo -> MemberInfo -> TName -> Expr -> Expr
fuseBody mr mi evTName = walk
  where
    walk expr = case expr of
      Let dgs body -> Let dgs (walk body)
      Case ss brs  -> Case ss (map walkBranch brs)
      _            -> teqCoerce (mrTypeEq mr) (miResult mi) (TVar (mrResultIdx mr)) expr
                                 (Var evTName InfoNone)
    walkBranch (Branch ps gs) = Branch ps (map (\(Guard g e) -> Guard g (walk e)) gs)

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
            -- drive the mr type at (this member's effect args, its own result)
            runTyArgs = map TVar (miEffTvs mi) ++ [res]
            conTyArgs = runTyArgs ++ map TVar (miExists mi)
            witness   = teqRefl (mrTypeEq mr) res
            argExprs  = map (\tn -> Var tn InfoNone) args
            callExpr  = makeCallApp mi conTyArgs argExprs witness
            runCall   = makeRunApp mr runTName runTyArgs callExpr
        in member{ defExpr = addTypeLambdas tvs (Lam args eff runCall), defInline = InlineAlways }
      _ -> member

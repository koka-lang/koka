{-# OPTIONS -cpp #-}
{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Type inference. Relies on results from kind inference
-----------------------------------------------------------------------------

module Type.Infer (inferTypes, coreVarInfoFromNameInfo ) where

import Lib.Trace hiding (traceDoc)
import Data.List(partition,sortBy,sortOn)
import qualified Data.List(find)
import Data.Ord(comparing)
import Data.Maybe(catMaybes)
import Control.Monad(when)
import Lib.PPrint
import Core.Pretty
import Common.Failure
import Common.Error
import Common.Name
import Common.NamePrim( nameTpOptional, nameOptional, nameOptionalNone, nameCopy, nameTpDelay
                      , nameReturn, nameRef, nameByref, nameDeref, nameAssign
                      , nameRefSet, nameTpUnit, nameTuple
                      , namePatternMatchError, nameSystemCore
                      , nameTpHandled, nameTpHandled1, nameTpNHandled1, nameTpNHandled
                      , nameToAny, nameFalse, nameTrue
                      , nameCons, nameListNil, nameVector
                      , nameTpPartial
                      , nameTpLocalVar, nameTpLocal, nameRunLocal, nameLocalGet, nameLocalSet, nameLocalNew, nameLocalVar
                      , nameClause, nameIdentity
                      , nameMaskAt, nameMaskBuiltin, nameEvvIndex, nameHTag, nameTpHTag, nameTpEv
                      , nameInternalInt32, nameOr, nameAnd, nameEffectOpen
                      , nameCCtxCreate, nameCCtxHoleCreate, isNameTuple
                      , nameCoreFileLine, nameCoreFileFile, nameCoreFileModule
                       )
import Common.Range
import Common.Unique
import Common.Syntax
import qualified Common.NameSet as S
import qualified Data.Map as M

import Syntax.Syntax
import qualified Core.Core as Core

import Kind.Kind
import Kind.Constructors
import Kind.Synonym
import Kind.Newtypes
import Kind.ImportMap

import Type.Type
import Type.Kind( getKind, labelIsLinear, isHandledEffect )
import Type.Pretty
import Type.Assumption
import Type.TypeVar
import qualified Type.Operations as Op
import Type.InferMonad

import qualified Core.CoreVar as CoreVar
import Core.AnalysisMatch( analyzeBranches )
import Core.AnalysisCCtx( analyzeCCtx )
-- import Common.ResumeKind
-- import Core.AnalysisResume( analyzeResume )
import Core.Divergent( analyzeDivergence )
import Core.BindingGroups( regroup )
-- import Core.Simplify( uniqueSimplify )

import qualified Syntax.RangeMap as RM


{--------------------------------------------------------------------------
  Infer Types
--------------------------------------------------------------------------}
inferTypes :: Env -> Maybe RM.RangeMap -> Synonyms -> Newtypes -> Constructors -> ImportMap -> Gamma -> Name -> DefGroups Type
                -> Core.CorePhase b (Gamma, Core.DefGroups, Maybe RM.RangeMap )
inferTypes prettyEnv mbRangeMap syns newTypes cons imports gamma0 context defs
  = -- error "Type.Infer.inferTypes: not yet implemented"
    -- return (gamma0,[],uniq0)
    do uniq0 <- unique
       ((gamma1, coreDefs),uniq1,mbRm) <- Core.liftError $
                                          runInfer prettyEnv mbRangeMap syns newTypes imports gamma0 context (uniq0 + 10 {- to not clash with at least 10 bound type variables -})
                                            (inferDefGroups True (arrange defs))
       setUnique uniq1
       return (gamma1,coreDefs,mbRm)
  where
    arrange defs
      = if (context /= nameSystemCore)
         then defs
         else -- pull in front certain key definitions that are used in functions generated from constructors (like accessors)
              let (first,rest) = partition isKeyDef defs
              in first ++ rest

    isKeyDef (DefNonRec def)  = defName def `elem` (map unqualify keyDefNames)
    isKeyDef _                = False

    keyDefNames = [{-namePatternMatchError,-}nameRef,nameRefSet,nameDeref]

{--------------------------------------------------------------------------
  Definition groups
--------------------------------------------------------------------------}
inferDefGroups :: Bool -> DefGroups Type -> Inf (Gamma,Core.DefGroups)
inferDefGroups topLevel (defGroup : defGroups)
  = inferDefGroupX topLevel defGroup (inferDefGroups topLevel defGroups)
       -- (g,cores) <- extendGamma core (inferDefGroups topLevel defGroups)
       -- return (g,core:cores)
inferDefGroups topLevel []
  = do gamma <- getGamma
       return (gamma,[])


inferDefGroupX :: Bool -> DefGroup Type -> Inf (Gamma,Core.DefGroups) -> Inf (Gamma,Core.DefGroups)
inferDefGroupX topLevel defGroup cont
  = do (cgroups0,(g,cgroups1)) <- inferDefGroup topLevel defGroup cont
       -- resetUnique
       -- zapSubst
       return (g,cgroups0 ++ cgroups1)

inferDefGroup :: Bool -> DefGroup Type -> Inf a -> Inf ([Core.DefGroup], a)
inferDefGroup topLevel (DefNonRec def) cont
  = --- trace ("\ninfer single " ++ show (defName def)) $
    do core <- inferDef (Generalized True) def
       mod  <- getModuleName

       (x,core1) <- let cgroup = [Core.DefNonRec core]
                    in if topLevel
                           then let core0 = core{ Core.defName = qualify mod (Core.defName core) }
                                in extendGammaCore False {- already canonical? -} [Core.DefNonRec core0] $
                                    do coreDef <- fixCanonicalName False core0
                                       x <- cont
                                       return (x,coreDef)
                           else do x <- extendInfGammaCore topLevel [Core.DefNonRec core] cont
                                   return (x,core)
       addRangeInfoCoreDef topLevel mod def core1
       let cgroup1 = Core.DefNonRec core1
       return ([cgroup1],x)
inferDefGroup topLevel (DefRec defs) cont
  = -- trace ("\ninfer group: " ++ show (map defName defs)) $
    do (gamma,infgamma) <- createGammas [] [] defs
       --coreDefs0 <- extendGamma gamma (mapM (inferRecDef topLevel infgamma) defs)
       (coreDefsX,assumed) <- extendGamma False gamma $ extendInfGammaEx topLevel [] infgamma $
                                 do assumed <- mapM (\def -> lookupInfName (getName def)) defs
                                    coreDefs0 <- mapM (\def -> inferDef Instantiated def) defs
                                    coreDefs1 <- mapM (fixCanonicalName True) coreDefs0
                                    return (coreDefs1,assumed)
       -- re-analyze the mutual recursive groups
       scoreDefsX <- subst coreDefsX
       let coreGroups0 = regroup scoreDefsX
       when topLevel (mapM_ checkRecVal coreGroups0)
       -- now analyze divergence
       (coreGroups1,divTNames)
            <- fmap unzip $
               mapM (\cgroup -> case cgroup of
                                   Core.DefRec cdefs | analyzeDivergence cdefs -> do cdefs' <- addDivergentEffect cdefs
                                                                                     return (Core.DefRec cdefs',map Core.defTName cdefs')
                                   _ -> return (cgroup,[])) $
               coreGroups0
       -- build a mapping from core name to original definition and assumed type
       -- hack: we map from the name range since there may be overloaded names, and the types are not fully determined yet..
       let coreMap = M.fromList (map (\(def,tp) -> (binderNameRange (defBinder def), (def,tp))) (zip defs assumed))
       -- check assumed types agains inferred types
       coreGroups2 <- mapMDefs (\cdef -> inferRecDef2 topLevel cdef ((Core.defTName cdef) `elem` concat divTNames) (find (Core.defNameRange cdef) coreMap)) coreGroups1
       -- add range info (for documentation)
       mod <- getModuleName
       mapMDefs_ (\cdef -> addRangeInfoCoreDef topLevel mod (fst (find (Core.defNameRange cdef) coreMap)) cdef) coreGroups2
       -- TODO: fix local info in the core; test/algeff/nim.kk with no types for bobTurn and aliceTurn triggers this
       let sub = map (\cdef -> let tname    = Core.defTName cdef
                                   nameInfo = -- trace ("fix local info: " ++ show (Core.defName cdef)) $
                                              createNameInfoX Public (Core.defName cdef) (Core.defSort cdef) (Core.defNameRange cdef) (Core.defType cdef) (Core.defDoc cdef)
                                   varInfo  = coreVarInfoFromNameInfo nameInfo
                                   var      = Core.Var tname varInfo
                               in (tname, var)) (Core.flattenDefGroups coreGroups2)
           coreGroups3 = (CoreVar.|~>) sub coreGroups2
       -- extend gamma
       x <- (if topLevel then extendGammaCore True {- already canonical -} else extendInfGammaCore False {-toplevel -}) coreGroups3 cont
       return (coreGroups3,x)

       {-
       coreDefsY <- if analyzeDivergence coreDefsX
                     then addDivergentEffect coreDefsX
                     else return coreDefsX
       coreDefs1 <- mapM inferRecDef2 (zip (zip defs coreDefsY) assumed)
       mod <- getModuleName
       mapM_ (\(def,coreDef) -> addRangeInfoCoreDef topLevel mod def coreDef) (zip defs coreDefs1 )
       let cgroup  = fixLocalInfo topLevel mod $ Core.DefRec coreDefs1
       x <- (if topLevel then extendGammaCore else extendInfGammaCore) cgroup cont
       return (cgroup,x)
       -}
  where
    -- we use a bit of trickery here:
    -- * things on toplevel with full types get added to the gamma since only the gamma can distinguish
    --   multiple recursive definitions with the same overloaded name
    --   this can only be done on toplevel, or otherwise we may do the scoping wrong with
    --   respect to infgamma
    -- * anything else gets added to infgamma -- this means that in a toplevel recursive
    --   group, some defs end up in infgamma and others in gamma: but at the toplevel that
    --   is ok while infering the types of the recursive group. Eventually, all inferred
    --   types will end up in gamma.
    createGammas :: [(Name,NameInfo)] -> [(Name,NameInfo)] -> [Def Type] -> Inf ([(Name,NameInfo)],[(Name,NameInfo)])
    createGammas gamma infgamma []
      = return (reverse gamma, reverse infgamma)
    createGammas gamma infgamma (Def (ValueBinder name () expr nameRng vrng) rng vis sort inl doc : defs)
      = case (lookup name infgamma) of
          (Just _)
            -> do env <- getPrettyEnv
                  if topLevel
                   then infError nameRng (text "recursive functions with the same overloaded name must all have a full type signature" <+> parens (ppName env name) <->
                                          text " hint: give a type annotation for each function (including the effect type).")
                   else infError nameRng (text "recursive functions with the same overloaded name cannot be defined as local definitions" <+> parens (ppName env name) <->
                                          text " hint: use different names for each function.")

          Nothing
            -> case expr of
                  Ann _ tp _  | topLevel && tvsIsEmpty (ftv tp)
                    -> do qname <- qualifyName name
                          let nameInfo = createNameInfoX Public qname sort nameRng tp doc -- (not topLevel || isValue) nameRng tp  -- NOTE: Val is fixed later in "FixLocalInfo"
                          -- trace ("*** createGammas: assume: " ++ show name ++ ": " ++ show nameInfo) $ return ()
                          createGammas ((qname,nameInfo):gamma) infgamma defs
                  _ -> case lookup name gamma of
                         Just _
                          -> do env <- getPrettyEnv
                                infError nameRng (text "recursive functions with the same overloaded name must have a full type signature" <+> parens (ppName env name))
                         Nothing
                          -> do qname <- if (topLevel) then qualifyName name else return name
                                info <- case expr of
                                          Ann _ tp _ -> return (createNameInfoX Public qname sort nameRng tp doc)  -- may be off due to incomplete type: get fixed later in inferRecDef2
                                          Lam pars _ _
                                            -> do tpars <- mapM (\b -> do{ t <- Op.freshTVar kindStar Meta; return (binderName b,t) }) pars
                                                  teff  <- Op.freshTVar kindEffect Meta
                                                  tres  <- Op.freshTVar kindStar Meta
                                                  let tp = TFun tpars teff tres
                                                  return (createNameInfoX Public qname DefVal nameRng tp doc)
                                          _ -> do tp <- Op.freshTVar kindStar Meta
                                                  -- trace ("*** assume defVal: " ++ show qname) $
                                                  return (createNameInfoX Public qname DefVal nameRng tp doc)  -- must assume Val for now: get fixed later in inferRecDef2
                                -- trace ("*** createGammasx: assume: " ++ show qname ++ ": " ++ show info) $ return ()
                                createGammas gamma ((qname,info):infgamma) defs

checkRecVal :: Core.DefGroup -> Inf ()
checkRecVal (Core.DefNonRec def) = return ()
checkRecVal (Core.DefRec defs)
  = mapM_ checkDef defs
  where
    checkDef def
      = if (not (Core.defIsVal def)) then return () else
         do infError (Core.defNameRange def) (text ("value definition is recursive.\n  recursive group: " ++ show (map Core.defName defs)))

fixCanonicalName :: Bool -> Core.Def -> Inf Core.Def
fixCanonicalName isRec def
  = do -- first look in the inf gamma for recursive definitions (since they may not resolve unambiguously)
       mbAssumedType <- if isRec then lookupInfName (Core.defName def) else return Nothing
       case mbAssumedType of
         Just (qname,_)
           -> return (def{ Core.defName = qname })
         _ -> -- failure ("Type.Infer.fixCanonicalName: cannot find in infGamma: " ++ show (Core.defName def))
              -- otherwise, we resolve normally
              do (_,_,info) <- resolveName (Core.defName def) (Just (Core.defType def, Core.defNameRange def)) (Core.defNameRange def) -- should never fail
                 let cname = infoCanonicalName (Core.defName def) info
                 return (def{ Core.defName = cname })




mapMDefs :: Monad m => (Core.Def -> m Core.Def) -> Core.DefGroups -> m Core.DefGroups
mapMDefs f cgroups
  = mapM (\cgroup -> case cgroup of
                       Core.DefRec cdefs   -> do cdefs' <- mapM f cdefs
                                                 return (Core.DefRec cdefs')
                       Core.DefNonRec cdef -> do cdef' <- f cdef
                                                 return (Core.DefNonRec cdef')) cgroups

mapMDefs_ :: Monad m => (Core.Def -> m ()) -> Core.DefGroups -> m ()
mapMDefs_ f cgroups
  = mapM_ (\cgroup -> case cgroup of
                        Core.DefRec cdefs   -> mapM_ f cdefs
                        Core.DefNonRec cdef -> f cdef) cgroups


addRangeInfoCoreDef topLevel mod def coreDef
  = let qname = if (topLevel && not (isQualified (Core.defName coreDef)))
                 then qualify mod (Core.defName coreDef)
                 else Core.defName coreDef
        sort = (if defIsVal def then "val" else "fun")
    in do addRangeInfo (Core.defNameRange coreDef) (RM.Id qname (RM.NIValue sort (Core.defType coreDef) (defDoc def) (True)) [] True)
          addRangeInfo (defRange def) (RM.Decl sort qname (RM.mangle qname (Core.defType coreDef)) (Just (Core.defType coreDef)))


-- | Add divergent effect to the type of the core definitions
-- Should really fully instantiate and eta-expand to insert evidence
-- but for now, we just fix up the types as necessary without evidence insertion
addDivergentEffect :: [Core.Def] -> Inf [Core.Def]
addDivergentEffect coreDefs0
  = mapM addDiv coreDefs0
  where
    addDiv def
      = do let rng = Core.defNameRange def
           (tp0,_,coref) <- instantiateNoEx rng (Core.defType def) -- no effect extension or otherwise div can be added even if the user specified total for example.
           case splitFunType tp0 of
             Nothing
              -> -- failure ( "Type.Infer.addDivergentEffect: unexpected non-function type:\n " ++ show coreDefs0) -- ?? should never happen?
                 -- can happen if a value contains a data structure containing recursive functions that refer to the value
                 return def
             Just (targs,teff,tres)
              -> do -- trace ("addDivergent: " ++ show (Core.defName def) ++ ": " ++ show (Core.defType def, tp0)) $ return ()
                    -- seff <- subst teff
                    -- let snewEff = effectExtendNoDup typeDivergent seff
                    tv <- freshEffect
                    let newEff = effectExtend typeDivergent tv
                    inferUnify (checkEffectSubsume rng) rng newEff teff
                    snewEff <- subst newEff
                    let tp1 = TFun targs snewEff tres
                    (resTp,resCore) <- generalize rng rng True typeTotal (TFun targs snewEff tres) (coref (Core.defExpr def))
                    inferSubsume (checkEffectSubsume rng) rng (Core.defType def) resTp
                    -- fix up the core since the recursive tname still refers to the old type without the 'div' effect
                    -- let name = unqualify (Core.defName def)
                    --    resCore1 = (CoreVar.|~>) [(name, Core.Var (Core.TName name resTp) Core.InfoNone)] resCore
                    return (def{ Core.defType = resTp, Core.defExpr = resCore })



{--------------------------------------------------------------------------
  Definition
--------------------------------------------------------------------------}

-- TODO: for multiple recursive definitions, the "typeapp" substitution fails; we should
-- collect all substitions and apply them all definitions afterwards; similarly for the
-- VarInfo's that are now done separately
inferRecDef2 :: Bool -> Core.Def -> Bool -> (Def Type,Maybe (Name,Type)) -> Inf (Core.Def)
inferRecDef2 topLevel coreDef divergent (def,mbAssumed)
   = -- trace (" infer rec def: " ++ (if divergent then "div " else "") ++ show (defName def) ++ ": " ++ show (Core.defType coreDef)) $
     do let rng = defRange def
            nameRng = binderNameRange (defBinder def)
        (resTp0,assumedTp,coref0)
                        <- case mbAssumed of
                            Nothing
                              -> return (Core.defType coreDef, Core.defType coreDef, id)
                            Just (_,assumed)
                              -> do assumedTp     <- subst assumed
                                    (resTp,coref) <- inferSubsume (checkRec rng) nameRng assumedTp (Core.defType coreDef)

                                    -- trace (" infer subsume: " ++ show (Core.defName coreDef) ++ ": " ++ show (assumedTp, Core.defType coreDef)) $ return ()
                                    return (resTp,assumedTp,coref)

        (resTp1,resCore1) <- generalize rng nameRng True typeTotal resTp0 (coref0 (Core.defExpr coreDef)) -- typeTotal is ok since only functions are recursive (?)

        let name = Core.defName coreDef
            csort = if (topLevel || CoreVar.isTopLevel coreDef) then Core.defSort coreDef else DefVal
            info = coreVarInfoFromNameInfo (createNameInfoX Public name csort (defRange def) resTp1 (defDoc def))
        penv <- getPrettyEnv
        (resTp2,coreExpr)
              <- case (mbAssumed,resCore1) of
                         (Just (_,rho), Core.TypeLam tvars expr) | isRho rho  -- we assumed a monomorphic type, but generalized eventually
                            -> -- fix it up by adding the polymorphic type application
                               do assumedTpX <- subst assumedTp >>= normalize True -- resTp0
                                  -- resTpX <- subst resTp0 >>= normalize
                                  simexpr <- return expr -- liftUnique $ uniqueSimplify penv False False 1 {-runs-} 0 expr
                                  coreX <- subst simexpr
                                  let -- coreX = simplify expr -- coref0 (Core.defExpr coreDef)
                                      mvars = [TypeVar id kind Bound | TypeVar id kind _ <- tvars]
                                      msub  = subNew (zip tvars (map TVar mvars))


                                      resCoreX = (CoreVar.|~>) [(Core.TName ({- unqualify -} name) assumedTpX,
                                                              Core.TypeApp (Core.Var (Core.TName ({- unqualify -} name) (resTp1)) info) (map TVar tvars))] -- TODO: wrong for unannotated polymorphic recursion: see codegen/wrong/rec2
                                                 (msub |-> coreX)

                                      bsub  = subNew (zip mvars (map TVar tvars))
                                      resCoreY = Core.TypeLam tvars (bsub |-> resCoreX)
                                  -- trace (" substitute typeapp\n" ++ show (resTpX, assumedTpX, msub |-> coreX)) $ return ()
                                  -- generalize rng nameRng typeTotal resTp0 resCoreX
                                  return (resTp1,resCoreY)
                               {-
                                  let resCore2 = Core.TypeLam tvars ((CoreVar.|~>) [(Core.TName (unqualify name) resTp1, Core.TypeApp (Core.Var (Core.TName (unqualify name) (resTp1)) Core.InfoNone) (map TVar tvars))] expr)
                                  trace ("\n ~> \n" ++ show resCore2) $
                                   return resCore2
                               -}
                         (Just (_,_), _) | divergent  -- we added a divergent effect, fix up the occurrences of the assumed type
                            -> do assumedTpX <- normalize True assumedTp >>= subst -- resTp0
                                  simResCore1 <- return resCore1 -- liftUnique $ uniqueSimplify penv False False 1 0 resCore1
                                  coreX <- subst simResCore1
                                  let resCoreX = (CoreVar.|~>) [(Core.TName ({- unqualify -} name) assumedTpX, Core.Var (Core.TName ({- unqualify -} name) resTp1) info)] coreX
                                  return (resTp1, resCoreX)
                         (Just _,_)  -- ensure we insert the right info  (test: static/div2-ack)
                            -> do assumedTpX <- normalize True assumedTp >>= subst
                                  simResCore1 <- return resCore1 -- liftUnique $ uniqueSimplify penv False False 1 0 resCore1
                                  coreX <- subst simResCore1
                                  let resCoreX = (CoreVar.|~>) [(Core.TName ({- unqualify -} name) assumedTpX, Core.Var (Core.TName ({- unqualify -} name) resTp1) info)] coreX
                                  return (resTp1, resCoreX)
                         (Nothing,_)
                            ->    return (resTp1,resCore1) -- (CoreVar.|~>) [(unqualify name, Core.Var (Core.TName (unqualify name) resTp1) Core.InfoNone)] resCore1


        -- coref2      <- checkEmptyPredicates rng
        -- resTp2      <- subst resTp1
        coreDef2    <- subst (Core.Def (Core.defName coreDef) resTp2 coreExpr (Core.defVis coreDef) csort (Core.defInline coreDef) (Core.defNameRange coreDef) (Core.defDoc coreDef))
        return (coreDef2)

inferRecDef :: Bool -> [(Name,NameInfo)] -> Def Type -> Inf Core.Def
inferRecDef topLevel infgamma def
  = -- trace ("inferRecDef: " ++ show (getName def)) $
    do let rng = defRange def
           nameRng = binderNameRange (defBinder def)
       eitherRes <-
          extendInfGammaEx topLevel [] infgamma $
          do mbAssumedType <- lookupInfName (getName def)
             coreDef <- inferDef Instantiated def
             case mbAssumedType of
               Nothing -- there was a full type signature that has already been taken care of
                -> -- trace "no assumed type" $
                    return (Left coreDef)
               Just (qname,assumed) -- otherwise, we need assure it matches the returned type
                -> case assumed of
                    TVar tv
                      -> {- if (not topLevel)
                          then do inferUnify (checkRec rng) nameRng assumed (Core.defType coreDef)
                                  return (Left coreDef)
                                  -- return (Left (coreDef{ Core.defType = resTp0, Core.defExpr = coref0 (Core.defExpr coreDef) }))
                          else
                        -}
                          do -- trace (" match recursive assumed type") $ return ()
                             (resTp0,coref0) <- inferSubsume (checkRec rng) nameRng assumed (Core.defType coreDef)
                             return (Right (resTp0,coreDef,coref0 (Core.defExpr coreDef)))
                    _  -> return (Left coreDef) -- the user gave a type signature but it ended up in infgamma anyways
       case eitherRes of
          Left cdef
            -> return cdef
          Right (resTp0,coreDef,resCore0)
            -> -- trace ("right recursive: " ++ show (Core.defName coreDef)) $
               do (resTp1,resCore1) <- generalize rng nameRng True typeTotal resTp0 resCore0 -- typeTotal is ok since only functions are recursive (?)

                  let name     = Core.defName coreDef
                      coreExpr = case resCore1 of
                                   Core.TypeLam tvars expr
                                      ->  -- trace ("substitute typeapp in " ++ show name ++ ": " ++ show resCore1) $
                                          Core.TypeLam tvars ((CoreVar.|~>) [(Core.TName (unqualify name) (Core.defType coreDef), Core.TypeApp (Core.Var (Core.TName (unqualify name) (resTp1)) Core.InfoNone) (map TVar tvars))] expr)
                                   _  -> resCore1

                  coref2      <- checkEmptyPredicates rng
                  resTp2      <- subst resTp1
                  coreDef2    <- subst (Core.Def (Core.defName coreDef) resTp2 (coref2 coreExpr) (Core.defVis coreDef) (Core.defSort coreDef) (Core.defInline coreDef) (Core.defNameRange coreDef) (Core.defDoc coreDef))

                  if (False && not topLevel && not (CoreVar.isTopLevel coreDef2) && not (isRho (Core.typeOf coreDef2)))
                   then do -- trace ("local rec with free vars: " ++ show coreDef2) $ return ()
                           typeError rng nameRng (text "local recursive definitions with free (type) variables cannot have a polymorphic type" <->
                                                  text " hint: make the function a top-level definition?" ) (Core.typeOf coreDef2) []
                   else return ()

                  return (coreDef2)


inferDef :: Expect -> Def Type -> Inf Core.Def
inferDef expect (Def (ValueBinder name mbTp expr nameRng vrng) rng vis sort inl doc)
 =do penv <- getPrettyEnv
     if (verbose penv >= 4)
      then Lib.Trace.trace ("infer: " ++ show sort ++ " " ++ show name) $ return ()
      else return ()
     withDefName name $ disallowHole $
      (if (not (isDefFun sort) || nameIsNil name) then id else allowReturn True) $
        do (tp,eff,coreExpr) <- inferExpr Nothing expect expr
                                -- Just annTp -> inferExpr (Just (annTp,rng)) (if (isRho annTp) then Instantiated else Generalized) (Ann expr annTp rng)

           -- traceDoc $ \env -> text " infer def before gen:" <+> pretty name <+> colon <+> ppType env tp
           (resTp0,resCore0) <- maybeGeneralize rng nameRng eff expect tp coreExpr -- may not have been generalized due to annotation
           -- traceDoc $ \env -> text " infer def:" <+> pretty name <+> colon <+> ppType env resTp
           inferUnify (checkValue rng) nameRng typeTotal eff
           when (verbose penv >= 4) $
            Lib.Trace.trace (show (text " inferred" <+> pretty name <.> text ":" <+> niceType penv resTp0)) $ return ()
           resTp   <- subst resTp0
           resCore <- subst resCore0

           when (isDefFun sort) $
             case splitFunScheme resTp of
               Just (_,_,_,effTp,resultTp)
                 -> let tp = makeValueOperation effTp resultTp -- pretty prints nicely as `-> eff res`
                    in addRangeInfo (endOfRange vrng {-')'-}) (RM.Id (newName "result") (RM.NIValue "expr" tp "" False) [] True)
               _ -> return ()

           subst (Core.Def name resTp resCore vis sort inl nameRng doc)  -- must 'subst' since the total unification can cause substitution. (see test/type/hr1a)

isAnnotatedBinder :: ValueBinder (Maybe Type) x -> Bool
isAnnotatedBinder (ValueBinder _ Just{} _ _ _) = True
isAnnotatedBinder _                                 = False

inferBindDef :: Def Type -> Inf (Effect,Core.Def)
inferBindDef def@(Def (ValueBinder name () expr nameRng vrng) rng vis sort inl doc)
  = -- trace ("infer bind def: " ++ show name ++ ", var?:" ++ show (sort==DefVar)) $
    do withDefName name $ disallowHole $
        do (tp,eff,coreExpr) <- inferExpr Nothing Instantiated expr
           stp <- subst tp
                                --  Just annTp -> inferExpr (Just (annTp,rng)) Instantiated (Ann expr annTp rng)
           coreDef <- if (sort /= DefVar)
                       then return (Core.Def name tp coreExpr vis sort inl nameRng doc)
                       else do hp <- Op.freshTVar kindHeap Meta
                               (qrefName,_,info) <- resolveName nameRef Nothing rng
                               let refTp  = typeApp typeRef [hp,tp]
                                   refVar = coreExprFromNameInfo qrefName info
                                   refExpr = Core.App (Core.TypeApp refVar [hp,stp]) [coreExpr] -- TODO: fragile: depends on order of quantifiers of the ref function!
                               -- traceDoc $ \penv -> text "reference" <+> pretty name <.> colon <+> ppType penv stp
                               return (Core.Def name refTp refExpr vis sort inl nameRng doc)

           if (not (isWildcard name))
            then let sort = (if defIsVal def then "val" else "fun")
                 in addRangeInfo nameRng (RM.Id name (RM.NIValue sort (Core.defType coreDef) doc (isAnnot expr)) [] True)
            else if (isTypeUnit (Core.typeOf coreDef))
             then return ()
             else do seff <- subst eff
                     -- traceDoc $ \env -> text "wildcard definition:" <+> pretty name <.> colon <+> niceType env seff
                     let (ls,tl) = extractEffectExtend seff
                     case (ls,tl) of
                       ([],tl) | isTypeTotal tl -> unusedWarning rng
                       ([],TVar tv)
                         -> do occ <- occursInContext tv (ftv tp)
                               if (not occ) then unusedWarning rng else return ()
                               -- return ()
                       _ -> return ()
           return (eff,coreDef)


checkValue        = Check "Values cannot have an effect"
unusedWarning rng = infWarning rng (text "expression has no effect and is unused" <-->
                                    text " hint: did you forget an operator? or used \"fun\" instead of \"fn\"?" )
{--------------------------------------------------------------------------
  Expression
--------------------------------------------------------------------------}
data Expect = Generalized Bool
            | Instantiated
            deriving (Show,Eq)

inferIsolated :: Range -> Range -> Expr a -> Inf (Type,Effect,Core.Expr) -> Inf (Type,Effect,Core.Expr)
inferIsolated contextRange range body inf
  = do (tp,eff,core) <- inf
       res@(itp,ieff,icore) <- improve contextRange range True eff tp  core
       case hasVarDecl body of
         Nothing   -> return res
         Just vrng -> do seff <- subst ieff
                         let (ls,tl) = extractOrderedEffect seff
                         case filter (\l -> labelName l == nameTpLocal) ls of
                           (_:_) -> typeError contextRange vrng
                                      (text "reference to a local variable escapes its lexical scope") seff []
                           _ -> return ()
                         return (itp,seff,icore)
   where
     hasVarDecl expr
       = case expr of
           Parens x _ _ _ -> hasVarDecl x
           Let _ x _  -> hasVarDecl x
           Bind _ x _ -> hasVarDecl x
           Ann x _ _  -> hasVarDecl x
           Inject _ x _ _ -> hasVarDecl x
           App (Var name _ rng) _ _ | name == nameLocalNew -> Just rng
           _ -> Nothing

-- | @inferExpr propagated expect expr@ takes a potential propagated type, whether the result is expected to be generalized or instantiated,
-- and the expression. It returns its type, effect, and core expression. Note that the resulting type is not necessarily checked that it matches
-- the propagated type: the propagated type is just a hint (used for example to resolve overloaded names).
inferExpr :: Maybe (Type,Range) -> Expect -> Expr Type -> Inf (Type,Effect,Core.Expr)
inferExpr propagated expect (Lam bindersL body0 rng)
  = isNamedLam $ \isNamed ->
    disallowHole $
    do -- traceDoc $ \env -> text "inferExpr.Lam:" <+> pretty (show expect) <+> text ", propagated:" <+> ppProp env propagated
       (bindersX,unpackImplicitss) <- unzip <$> mapM inferImplicitParam bindersL
       let body = foldr (\f x -> f x) body0 unpackImplicitss

       (propArgs,propEff,propBody,skolems,expectBody) <- matchFun (length bindersX) propagated

       let binders0 = [case binderType binder of
                         Nothing -> binder{ binderType = fmap snd mbProp }
                         Just _  -> binder
                      | (binder,mbProp) <- zip bindersX propArgs]
       binders1 <- mapM instantiateBinder binders0
       -- traceDoc $ \env -> text "infexExpr.Lam: binder types: " <+> list [ppName env (binderName b) <+> text "=" <+> ppType env (binderType b) | b <- binders1] <+>
       --                    text ", propagated body: " <+> ppProp env propBody

       eff <- case propEff of
                Nothing  -> freshEffect  -- TODO: use propEff?
                Just (eff,_) -> return eff
       (infgamma,sub,defs) <- inferOptionals eff [] binders1
       let coref c = Core.makeLet (map Core.DefNonRec defs) ((CoreVar.|~>) sub c)

       returnTp <- case propBody of
                     Nothing     -> Op.freshTVar kindStar Meta
                     Just (tp,_) -> return tp

       (tp,eff1,core) <- extendInfGamma infgamma  $
                           extendInfGamma [(nameReturn,createNameInfoX Public nameReturn DefVal (getRange body) returnTp "")] $
                           (if (isNamed) then inferIsolated rng (getRange body) body else id) $
                           -- inferIsolated rng (getRange body) body $
                           inferExpr propBody expectBody body

       -- spropEff <- subst propEff
       -- seff1    <- subst eff1
       -- traceDoc $ \env -> text " inferExpr.Lam: propagated effect" <+> ppProp env spropEff <+> text ", body effect:" <+> ppType env seff1 <+> text ", unsubst " <+> ppType env eff1
       inferUnify (checkReturnResult rng) (getRange body) returnTp tp
       inferUnify (Infer rng) (getRange body) eff eff1

       -- traceDoc $ \env -> text " inferExpr.Lam: body tp:" <+> ppType env tp
       topEff <- case propEff of
                   Nothing -> do -- traceDoc $ \env -> text (" inferExpr.Lam. no prop eff")
                                 subst eff
                   Just (topEff,r) -> do -- traceDoc (\env -> text (" inferExpr.Lam.propEff: ") <+> ppType env eff <+> text ", top: " <+> ppType env topEff)
                                          -- inferUnifies (checkEffect rng) [(r,topEff),(getRange body,eff)]
                                         inferUnify (checkEffectSubsume rng) r eff topEff
                                         return topEff
                                         -- subst eff
       -- traceDoc $ \env -> text " inferExpr.Lam: body eff:" <+> ppType env eff <+> text ", topeff: " <+> ppType env topEff
       parTypes2 <- subst (map binderType binders1)
       let optPars   = zip (map binderName binders1) parTypes2 -- (map binderName binders1) parTypes2
           bodyCore1 = Core.addLambdas optPars topEff (Core.Lam [] topEff (coref core))
       bodyCore2 <- subst bodyCore1
       stopEff <- subst topEff
       let pars = optPars
                {- zipWith renameImplicitParams bindersL optPars
                where
                  renameImplicitParams binder (_,parTp)
                    = let pname = case binderExpr binder of
                                    Just (Var ename _ rng) | isImplicitParamName (binderName binder)
                                      -> namedImplicitParamName (binderName binder) ename
                                    _ -> binderName binder
                      in (pname, parTp) -}

       -- check skolem escape
       sftp0 <- subst (typeFun pars stopEff tp)
       -- traceDoc $ \env -> text " inferExpr.Lam: check skolems:" <+> ppType env sftp0 <+> text ", " <+> pretty skolems
       checkSkolemEscape rng sftp0 Nothing skolems tvsEmpty  -- TODO: not having this check improves error messages but is it really safe?

       -- substitute back skolems to meta variables
       let subSkolems = subNew [(tv,TVar tv{typevarFlavour=Meta}) | tv <- skolems]
           sftp1 = subSkolems |-> sftp0
       -- traceDoc $ \env -> text " inferExpr.Lam: fun type:" <+> ppType env sftp1
       (ftp,fcore) <- maybeGeneralize rng (getRange body) typeTotal expect sftp1 bodyCore2
       -- traceDoc $ \env -> text " inferExpr.Lam: subst fun type:" <+> ppType env ftp

       -- check for polymorphic parameters (this has to be done after generalize since some substitution may only exist as a constraint up to that point)
       unannotBinders <- mapM (\b -> do tp <- subst (binderType b); return b{ binderType = tp })
                            [b1  | (b0,b1) <- zip binders0 binders1, isNothing (binderType b0)]

       -- trace ("unannotBinders: " ++ show tp ++ ": " ++ show [(binderName b, binderType b) | b <- unannotBinders]) $ return ()
       let polyBinders = filter (not . isTau . binderType) unannotBinders
       if (null polyBinders)
        then return ()
        else let b = head polyBinders
             in typeError (rng) (binderNameRange b) (text "unannotated parameters cannot be polymorphic") (binderType b) [(text "hint",text "annotate the parameter with a polymorphic type")]

       mapM_ (\(binder,tp) -> addRangeInfo (binderNameRange binder) (RM.Id (binderName binder)
                                (RM.NIValue "val" tp "" (case (propagated,binderType binder) of
                                                           (Just (_,rng), Just _) | rangeIsHidden rng -> True -- there was an actual annotation
                                                           _   -> False
                                                        )) [] True))
             (zip binders0 parTypes2)
       eff <- freshEffect
       return (ftp, eff, fcore )

inferExpr propagated expect (Let defgroup body rng)
  = do (cgroups,(tp,eff,core)) <- inferDefGroup False defgroup (inferExpr propagated expect body)
       return (tp,eff,Core.Let cgroups core)

inferExpr propagated expect (Bind def body rng)
  = -- trace ("infer bind") $
    do (eff1,coreDef) <- inferBindDef def
       mod  <- getModuleName
       let cgroup = Core.DefNonRec coreDef
       (tp,eff2,coreBody) <- extendInfGammaCore False [cgroup] (inferExpr propagated expect body)
       -- topEff <- addTopMorphisms rng [(defRange def,eff1),(getRange body,eff2)]
       inferUnify (checkEffect rng) (getRange rng) eff1 eff2
       return (tp,eff2,Core.Let [cgroup] coreBody)

-- | Return expressions
inferExpr propagated expect (App (Var name _ nameRng) [(_,expr)] rng)  | name == nameReturn
  = do allowed <- isReturnAllowed
       if (False && not allowed)
        then infError rng (text "illegal expression context for a return statement")
        else  do (tp,eff,core) <- inferExpr propagated expect expr
                 mbTp <- lookupInfName nameReturn -- (unqualify nameReturn)
                 case mbTp of
                   Nothing
                    -> do infError rng (text "illegal context for a return statement")
                   Just (_,retTp)
                    -> do inferUnify (checkReturn rng) (getRange expr) retTp tp
                 resTp <- Op.freshTVar kindStar Meta
                 let typeReturn = typeFun [(nameNil,tp)] typeTotal resTp
                 addRangeInfo nameRng (RM.Id (newName "return") (RM.NIValue "expr" tp "" False) [] False)
                 return (resTp, eff, Core.App (Core.Var (Core.TName nameReturn typeReturn)
                                      (Core.InfoExternal [(Default,"return #1")])) [core])
-- | Assign expression
inferExpr propagated expect (App assign@(Var name _ arng) [lhs@(_,lval),rhs@(_,rexpr)] rng) | name == nameAssign
  = case lval of
      App fun args lrng  -- array[i] := e
          -> do xargs <- case args of
                          ((mbName,arg@(Var target _ vrng)) : rest)  -- var_vec[i] := e   TODO: perhaps unsafe for general use?
                            -> do (_,gtp,_) <- resolveName target Nothing vrng
                                  (tp,_,_) <- instantiate vrng gtp
                                  -- traceDoc $ \penv -> text "setting:" <+> pretty target <+> text ":" <+> ppType penv tp
                                  if (isTypeLocalVar tp)
                                   then return ((mbName,App (Var nameByref False vrng) [(Nothing, arg)] lrng) : rest)
                                   else return args
                          _ -> return args
                inferExpr propagated expect (App fun (xargs ++ [(Nothing,rexpr)]) rng)
      Var target _ lrng
        -> do (_,gtp,_) <- resolveName target Nothing lrng
              (tp,_,_) <- instantiateEx lrng gtp
              -- traceDoc $ \penv -> text "setting:" <+> pretty target <+> text ":" <+> ppType penv tp
              nameSet <- if (isTypeLocalVar tp)
                           then return nameLocalSet
                           else do r <- freshRefType
                                   inferUnify (checkAssign rng) lrng r tp
                                   return nameRefSet
              inferExpr propagated expect
                        (App (Var nameSet False arng) [(Nothing,App (Var nameByref False (before lrng)) [lhs] lrng), rhs] rng)

      _ -> errorAssignable
  where
    errorAssignable
      = do contextError rng (getRange lval) (text "not an assignable expression") [(text "because",text "an assignable expression must be an application, index expression, or variable")]
           return (typeUnit,typeTotal,Core.Con (Core.TName (nameTuple 0) typeUnit) (Core.ConEnum nameTpUnit Core.DataEnum valueReprZero 0))

    checkAssign
      = Check "an assignable identifier must have a reference type"

    freshRefType
      = do hvar <- Op.freshTVar kindHeap Meta
           xvar <- Op.freshTVar kindStar Meta
           return (typeApp typeRef [hvar,xvar])


{-
-- | Assign expressions on indexed l-values
inferExpr propagated expect (App assign@(Var name _ _) ((_,App index@(Var iname _ _) iargs _) : args) rng)  | unqualify name == unqualify nameAssign && unqualify iname == unqualify nameIndex
  = inferExpr propagated expect (App assign (iargs ++ args) rng)

-- | Assign expressions on local variables
inferExpr propagated expect (App assign@(Var name _ _) args@[lhs@(_,Var target _ lrng),rhs] rng)  | unqualify name == unqualify nameAssign
  = do (_,_,info) <- resolveName target Nothing lrng
       case info of
        InfoVal{ infoIsVar = True }
           -> inferExpr propagated expect (App assign [(Nothing,App (Var nameByref False lrng) [lhs] lrng), rhs] rng)
        _  -> inferApp propagated expect assign args rng
-}

-- | applied handlers are treated specially by allowing automatic masking of local effects
inferExpr propagated expect (App (h@Handler{hndlrAllowMask=Nothing}) [action] rng)
  = do lvars <- getLocalVars
       let allow = not (usesLocals (S.fromList (map fst lvars)) (snd action))
       inferExpr propagated expect (App h{hndlrAllowMask=Just allow} [action] rng)

-- | Byref expressions
inferExpr propagated expect (App (Var byref _ _) [(_,Var name _ rng)] _)  | byref == nameByref
  = inferVar propagated expect name rng False

-- | Hole expressions
inferExpr propagated expect (App fun@(Var hname _ nameRng) [] rng)  | hname == nameCCtxHoleCreate
  = do ok <- useHole
       when (not ok) $
         contextError rng rng (text "ill-formed constructor context")
            [(text "because",text "there can be only one hole, and it must occur under a constructor context 'ctx'")]
       (tp,eff,core) <- inferApp propagated expect fun [] rng
       addRangeInfo nameRng (RM.Id (newName "hole") (RM.NIValue "expr" tp "" False) [] False)
       return (tp,eff,core)

-- | Context expressions
inferExpr propagated expect (App (Var ctxname _ nameRng) [(_,expr)] rng)  | ctxname == nameCCtxCreate
  = do tpv <- Op.freshTVar kindStar Meta
       holetp <- Op.freshTVar kindStar Meta
       let ctxTp = TApp typeCCtxx [tpv,holetp]
       prop <- case propagated of
                 Nothing -> return Nothing
                 Just (ctp,crng) -> do inferUnify (checkMatch crng) nameRng ctp ctxTp
                                       stp <- subst tpv
                                       return (Just (stp,rng))
       ((tp,eff,core),hole) <- allowHole $ inferExpr prop Instantiated expr
       inferUnify (Infer rng) nameRng tp tpv
       when (not hole) $
          contextError rng rng (text "ill-formed constructor context") [(text "because",text "the context has no 'hole'")]
       newtypes <- getNewtypes
       score <- subst core
       (ccore,errs) <- withUnique (analyzeCCtx rng newtypes score)
       mapM_ (\(rng,err) -> infError rng err) errs
       let ctp = Core.typeOf ccore
       addRangeInfo nameRng (RM.Id (newName "ctx") (RM.NIValue "expr" ctp "" False) [] False)
       return (Core.typeOf ccore,eff,ccore)

-- | Application nodes. Inference is complicated here since we need to disambiguate overloaded identifiers.
inferExpr propagated expect (App fun nargs rng)
  = inferApp propagated expect fun nargs rng

inferExpr propagated expect (Ann expr annTp0 rng)
  = do -- match with propagated type first
       annTp <- case propagated of
                  Just (propAnn,propRng) -> do inferUnify (checkAnn propRng) rng propAnn annTp0
                                               subst annTp0
                  Nothing -> return annTp0
       -- traceDoc $ \env -> text "infer annotation:" <+> ppType env annTp <+> text ", propagated:" <+> ppProp env propagated

       (tp,eff,core) <- inferExpr (Just (annTp,rangeHide rng)) (if isRho annTp then Instantiated else Generalized False) expr
       sannTp <- subst annTp
       stp    <- subst tp
       -- traceDoc $ \env -> text "  subsume annotation:" <+> ppType env sannTp <+> text " to: " <+> ppType env stp
       (resTp0,coref) <- -- withGammaType rng sannTp $
                          inferSubsume (checkAnn rng) (getRange expr) sannTp tp
       -- (resTp,resCore) <- maybeInstantiateOrGeneralize expect annTp (coref core)
       -- return (resTp,eff,resCore)
       resTp  <- subst resTp0
       resEff <- subst eff
       resCore <- subst (coref core)
       -- traceDoc $ \env -> text "  subsumed to:" <+> ppType env resTp
       return (resTp,resEff,resCore)


inferExpr propagated expect (Handler handlerSort scoped HandlerNoOverride mbAllowMask mbEff pars reinit ret final branches hrng rng)
  = let allowMask = case mbAllowMask of
                      Just True -> True
                      _         -> False
    in inferHandler propagated expect handlerSort scoped allowMask mbEff pars reinit ret final branches hrng rng
inferExpr propagated expect (Handler handlerSort scoped HandlerOverride mbAllowMask mbEff pars reinit ret final branches hrng rng)
  = do heff <- inferHandledEffect hrng handlerSort mbEff branches
       {-
       case mbhxeff of
         Nothing
          -> do termError rng (text "cannot determine the effect type for the override") typeTotal []
                inferHandler propagated expect handlerSort scoped mbEff pars reinit ret final branches hrng rng
         Just heff -> -}
       {-
       let h = (Handler handlerSort scoped HandlerNoOverride mbEff pars reinit ret final branches hrng rng)
           name = newHiddenName "override-action"
           binder = ValueBinder name Nothing Nothing rng rng
           var    = Var name False rng
           lam    = Lam [binder] (Inject heff (Lam [] (App h [(Nothing,var)] rng) rng) False rng) rng
       inferExpr propagated expect lam
       -}
       let h = (Handler handlerSort scoped HandlerNoOverride mbAllowMask mbEff pars reinit ret final branches hrng rng)
           actionName = newHiddenName "override-action"
           actionVar  = Var actionName False rng
           actionBind = ValueBinder actionName Nothing Nothing rng rng
           mask   = if (isHandlerInstance handlerSort)
                      then let instName = newHiddenName "override-inst"
                               instBind = ValueBinder instName Nothing Nothing rng rng
                               instVar  = Var instName False rng
                               instLam  = Lam [] (App actionVar [(Nothing,instVar)] rng) rng
                           in Lam [instBind] (Inject heff instLam True rng) rng  -- mask behind
                      else Lam [] (Inject heff actionVar True rng) rng  -- mask behind
           lam    = Lam [actionBind] (App h [(Nothing,mask)] rng) rng
       inferExpr propagated expect lam

inferExpr propagated expect (Case expr branches rng)
  = -- trace " inferExpr.Case" $
    do (ctp,ceff,ccore) <- allowReturn False $ disallowHole $ inferExpr Nothing Instantiated expr
       -- infer branches
       let matchedNames = extractMatchedNames expr
       bress <- disallowHole $
                case (propagated,branches) of
                  (Nothing,(b:bs)) -> -- propagate the type of the first branch
                    do bres@(tpeffs,_) <- inferBranch propagated ctp (getRange expr) matchedNames b
                       let tp = case tpeffs of
                                  (tp,_):_ -> tp
                                  _        -> failure $ "Type.Infer.inferExpr.Case: branch without guard"
                       bress <- mapM (inferBranch (Just (tp,getRange b)) ctp (getRange expr) matchedNames) bs
                       return (bres:bress)
                  _ -> mapM (inferBranch propagated ctp (getRange expr) matchedNames) branches
       let (tpeffss,bcores) = unzip bress
           (tps,effs) = unzip (concat tpeffss)
       -- ensure branches match
       let rngs = map (getRange . branchGuards) branches
           brngs = map getRange branches
       resTp  <- inferUnifyTypes checkMatch (zip tps (zip brngs rngs))
       -- resEff <- addTopMorphisms rng ((getRange expr,ceff):(zip rngs effs))
       {-
       resEff <- inferUnifies (checkEffect rng) ((getRange expr,ceff):(zip rngs effs))
       -}
       resEff <- freshEffect
       mapM_ (\(rng,eff) -> inferUnify (checkEffectSubsume rng) rng eff resEff) ((getRange expr,ceff):(zip rngs effs))
       -- check scrutinee type
       stp <- subst ctp
       if (typeIsCaseLegal stp)
        then return ()
        else typeError rng (getRange expr) (text "can only match on literals or data types") stp []
       -- get data info and analyze branches
       dataInfo <- findDataInfo (getTypeName stp)
       defName  <- currentDefName
       sbcores  <- subst bcores
       newtypes <- getNewtypes
       let (matchIsTotal,warnings,cbranches) = analyzeBranches newtypes defName rng sbcores [stp] [dataInfo]
       mapM_ (\(rng,warning) -> infWarning rng warning) warnings
       cbranches <- if matchIsTotal
                  then return cbranches
                  else do moduleName <- getModuleName
                          let litPos = Lit (LitString (show moduleName ++ show rng) rng)
                              litDef = Lit (LitString (show defName) rng)
                              exnBranch = Branch (PatWild rng) [Guard guardTrue
                                              (App (Var namePatternMatchError False rng) [(Nothing, litPos), (Nothing, litDef)] rng)]
                          cexnBranch <- inferBranch (Just (resTp,rng)) ctp (getRange expr) matchedNames exnBranch
                          case cexnBranch of
                            ((tp, eff):_, cbranch) ->
                                 do -- inferUnify (checkMatch rng) rng tp resTp  -- already propagated
                                    inferUnify (checkEffectSubsume rng) rng eff resEff
                                    return (cbranches ++ [cbranch])
                            _ -> failure "Type.Infer.inferExpr.Case: should never happen, cexnBranch always contains a guard"
       -- return core
       core    <- subst (Core.Case [ccore] cbranches)
       sresEff <- subst resEff
       (gresTp,gcore) <- maybeInstantiateOrGeneralize rng (getRange branches) sresEff expect resTp core
       return (gresTp,sresEff,gcore)
  where
    typeIsCaseLegal tp
      = case expandSyn tp of
          TApp (TCon _) _  -> True
          TCon _           -> True
          -- TApp (TVar _) _  -> True
          -- TVar _           -> True
          _                -> False

    getTypeName tp
      = case expandSyn tp of
          TApp (TCon tc) _  -> typeconName tc
          TCon tc           -> typeconName tc
          _                 -> failure ("Type.Infer.inferExpr.Case.getTypeName: not a valid scrutinee? " ++ show tp)


    extractMatchedNames expr
      = case expr of
          Parens e _ _ _              -> extractMatchedNames e
          App (Var tname _ _) args _  | isNameTuple tname -> concat (map (extractMatchedNamesX . snd) args)
          _                           -> extractMatchedNamesX expr

    extractMatchedNamesX expr
      = case expr of
          Var name _ _ -> [name]
          _            -> []


inferExpr propagated expect (Var name isOp rng)
  = inferVar propagated expect name rng True

inferExpr propagated expect (Lit lit)
  = do let (tp,core,rng,docs) =
              case lit of
                LitInt i r  -> (typeInt,Core.Lit (Core.LitInt i),r,
                                   ["dec  = " ++ show i] ++
                                    if i < toInteger (minBound :: Int) || i > toInteger (maxBound :: Int)
                                      then []
                                      else let x0 = (fromInteger i) :: Int
                                               x  = if x0 >= 0 then x0
                                                      else if x0 >= -0x80 then 0x100 + x0
                                                      else if x0 >= -0x8000 then 0x10000 + x0
                                                      else if x0 >= -0x80000000 then 0x100000000 + x0
                                                      else fromInteger (0x10000000000000000 + i)
                                           in if (x < 0) then []
                                                else if (x <= 0x7F || (i < 0 && x <= 0xFF))
                                                  then ["hex8 = 0x" ++ showHex 2 x,"bit8 = 0b" ++ showBinary 8 x]
                                                  else if (x <= 0x7FFF || (i < 0 && x <= 0xFFFF))
                                                    then ["hex16= 0x" ++ showHex 4 x,"bit16= 0b" ++ showBinary 16 x]
                                                    else if (x <= 0x7FFFFFFF || (i < 0 && x <= 0xFFFFFFFF))
                                                      then ["hex32= 0x" ++ showHex 8 x, "bit32= 0b" ++ showBinary 32 x]
                                                      else ["hex64= 0x" ++ showHex 16 x, "bit64= 0b" ++ showBinary 64 x]
                                )
                LitChar c r  -> (typeChar,Core.Lit (Core.LitChar c),r,
                                     let i = fromEnum c
                                     in ["unicode= " ++
                                          if (i < 0 || i > 0xFFFFF)
                                            then show i ++ " (out of range)"
                                            else if i <= 0xFFFF
                                                   then "u" ++ showHex 4 i
                                                   else "U" ++ showHex 6 i]
                                 )
                LitFloat f r  -> (typeFloat,Core.Lit (Core.LitFloat f),r,
                                     ["hex64= " ++ showHexFloat f])
                LitString s r  -> (typeString,Core.Lit (Core.LitString s),r,
                                     ["count= " ++ show (length s)])
       addRangeInfo rng (RM.Id (newName "literal") (RM.NIValue "expr" tp "" False) (map text docs) False)
       eff <- freshEffect
       return (tp,eff,core)


inferExpr propagated expect (Parens expr name pre rng)
  = do (tp,eff,core) <- inferExpr propagated expect expr
       if (name /= nameNil)
         then do addRangeInfo rng (RM.Id name (RM.NIValue (if null pre then "expr" else pre) tp "" False) [] False)
         else return ()
       return (tp,eff,core)

inferExpr propagated expect (Inject label expr behind rng)
  = do eff0 <- freshEffect
       let eff = if (not behind) then eff0 else (effectExtend label eff0)

       let tfun r = typeFun [] eff r
           prop = case propagated of
                    Nothing  -> Nothing
                    Just (ptp,prng) -> case splitPredType ptp of
                                        (foralls,preds,rho)
                                          -> Just (quantifyType foralls $ qualifyType preds $ tfun rho, prng)
       (exprTp,exprEff,exprCore) <- inferExpr prop Instantiated expr

       res <- Op.freshTVar kindStar Meta
       inferUnify (checkInject rng) rng (tfun res) exprTp
       resTp <- subst res

       -- traceDoc $ \penv -> text "infer inject :" <+> ppType penv label
       (mbHandled,effName) <- effectNameCore label rng
       effTo <- subst $ effectExtend label eff

       sexprTp <- subst exprTp
       -- traceDoc $ \env -> text "inject: effTo:" <+> ppType env effTo <+> text "," <+> ppType env exprEff <+> text ", exprTp: " <+> ppType env sexprTp
       let coreLevel  = if behind then Core.exprTrue else Core.exprFalse -- Core.Lit (Core.LitInt (if behind then 1 else 0))
       core <- case mbHandled of
                 -- general handled effects use "@inject-effect"
                 Just coreHTag
                   -> do (maskQName,maskTp,maskInfo) <- resolveFunName nameMaskAt (CtxFunArgs 3 [] Nothing) rng rng
                         (evvIndexQName,evvIndexTp,evvIndexInfo) <- resolveFunName nameEvvIndex (CtxFunArgs 1 [] Nothing) rng rng
                         let coreMask = coreExprFromNameInfo maskQName maskInfo
                             coreIndex= Core.App (Core.TypeApp (coreExprFromNameInfo evvIndexQName evvIndexInfo) [effTo])
                                                 [coreHTag]
                             core     = Core.App (Core.TypeApp coreMask [resTp,eff,effTo]) [coreIndex,coreLevel,exprCore]
                         return core
                 Nothing
                   -> do (maskQName,maskTp,maskInfo) <- resolveFunName nameMaskBuiltin (CtxFunArgs 1 [] Nothing) rng rng
                         let coreMask = coreExprFromNameInfo maskQName maskInfo
                             core     = Core.App (Core.TypeApp coreMask [resTp,eff,effTo]) [exprCore]
                         return core
       return (resTp,effTo,core)


inferCheckedExpr expectTp expr
  = do (_,_,core) <- inferExpr Nothing Instantiated (Ann expr expectTp (getRange expr))
       return core

{-
inferExpr propagated expect expr
  = todo ("Type.Infer.inferExpr")
-}

inferUnifyTypes contextF [] = matchFailure "Type.Infer.inferinferUnifyTypes"
inferUnifyTypes contextF [(tp,_)]  = subst tp
inferUnifyTypes contextF ((tp1,r):(tp2,(ctx2,rng2)):tps)
  = do inferUnify (contextF ctx2) rng2 tp1 tp2
       inferUnifyTypes contextF ((tp1,r):tps)


{--------------------------------------------------------------------------
  infer effect handlers
--------------------------------------------------------------------------}

inferHandler :: Maybe (Type,Range) -> Expect -> HandlerSort -> HandlerScope -> Bool
                      -> Maybe Effect
                      -> [ValueBinder (Maybe Type) ()] -> Maybe (Expr Type) -> Maybe (Expr Type) -> Maybe (Expr Type)
                      -> [HandlerBranch Type] -> Range -> Range -> Inf (Type,Effect,Core.Expr)

-- Regular handler
inferHandler propagated expect handlerSort handlerScoped allowMask
             mbEffect (_:localPars) initially ret finally branches hrng rng
  = do contextError hrng rng (text "Type.Infer.inferHandler: TODO: not supporting local parameters") []
       failure "abort"
inferHandler propagated expect handlerSort handlerScoped allowMask
             mbEffect [] initially ret finally branches hrng rng
  = do -- get the handled effect
       heff <- inferHandledEffect hrng handlerSort mbEffect branches
       let isInstance = isHandlerInstance handlerSort
           effectName = effectNameFromLabel heff
           handlerConName = toHandlerConName effectName

       -- traceDoc $ \penv -> text "checking handler: " <+> ppType penv heff <.> text ", effect name:" <+> ppName penv effectName

       -- check operations
       checkCoverage rng heff handlerConName branches

       -- infer the result type to improve inference
       res  <- case (propagated,ret) of
                (Nothing,Just expr) -> do (tp,_,_) <- inferExpr propagated Instantiated expr
                                          case splitFunScheme tp of
                                            Just (_,_,_,_,retTp) -> return retTp
                                            _ -> Op.freshTVar kindStar Meta
                (Just (retTp,_),_) -> return retTp
                _ -> Op.freshTVar kindStar Meta
       eff  <- Op.freshTVar kindEffect Meta
       resumeArgs <- mapM (\_ -> Op.freshTVar kindStar Meta) branches  -- TODO: get operation result types to improve inference

       -- construct the handler
       -- traceDoc $ \penv -> text "infer handler: heff:" <+> ppType penv heff <+> text ", propagated:" <+> (case propagated of { Nothing  -> text "none"; Just (tp,_)  -> ppType penv tp })
       let -- create expressions for each clause
           opName b1 b2 = compare (show (unqualify (hbranchName b1))) (show (unqualify (hbranchName b2)))
           clause (HandlerBranch opName pars body opSort nameRng patRng, resumeArg)
            = do (clauseName, cparams, prefix) <- case opSort of
                          OpVal        -> return (nameClause "tail" (length pars), pars, "val")
                          OpFun        -> return (nameClause "tail" (length pars), pars, "fun")
                          OpExcept     -> return (nameClause "never" (length pars), pars, "final ctl")
                          -- don't optimize ctl to exc since exc runs the finalizers before the clause (unlike ctl)
                          -- OpControl    | not (hasFreeVar body (newName "resume"))
                          --             -> (nameClause "never" (length pars), pars)  -- except
                          OpControl    -> do let resumeTp = TFun [(nameNil,resumeArg)] eff res
                                                 resumeDoc shorten = if shorten then text "resume" else empty
                                             addRangeInfo nameRng (RM.Implicits resumeDoc)
                                             return (nameClause "control" (length pars),
                                                     pars ++ [ValueBinder (newName "resume") (Just resumeTp) () (rangeHide nameRng) nameRng],
                                                     "ctl")
                          OpControlRaw -> do let eff0 = effectExtend heff eff
                                                 resumeContextTp = typeResumeContext resumeArg eff eff0 res
                                                 resumeDoc shorten = if shorten then text "rcontext" else empty
                                             addRangeInfo nameRng (RM.Implicits resumeDoc)
                                             return (nameClause "control-raw" (length pars),
                                                     pars ++ [ValueBinder (newName "rcontext") (Just resumeContextTp) () (rangeHide hrng) patRng],
                                                     "raw ctl")
                          OpControlErr -> failure "Type.Infer.inferHandler: using a bare operation is deprecated.\n  hint: start with 'val', 'fun', 'brk', or 'ctl' instead."
                          -- _            -> failure $ "Type.Infer.inferHandler: unexpected resume kind: " ++ show rkind
                 -- traceDoc $ \penv -> text "resolving:" <+> text (showPlain opName) <+> text ", under effect:" <+> text (showPlain effectName)
                 (_,gtp,_) <- resolveFunName (if isQualified opName then opName else qualify (qualifier effectName) opName)
                                               (CtxFunArgs (length pars + (if isInstance then 1 else 0)) [] Nothing) patRng nameRng -- todo: resolve more specific with known types?
                 (tp,_,_)  <- instantiateEx nameRng gtp
                 let parTps = case splitFunType tp of
                                Just (tpars,_,_) -> (if (isInstance) then tail else id) $ -- drop the first parameter of an op for an instance (as it is the instance name)
                                                    map (Just . snd) tpars ++ repeat Nothing  -- TODO: propagate result type as well?
                                _ -> failure $ "Type.Infer.inferHandler: bad operation type: " ++ show opName ++ ": " ++ show (pretty gtp)
                     cparamsx = map (\(b,mbtp) -> case b of
                                                    ValueBinder name Nothing _ nameRng rng -> ValueBinder name mbtp Nothing nameRng rng
                                                    ValueBinder name annTp _ nameRng rng   -> ValueBinder name annTp Nothing nameRng rng)
                                $ zip (cparams :: [ValueBinder (Maybe Type) ()]) (parTps)
                     frng = combineRanged nameRng body
                     cname = case opSort of
                               OpVal -> fromValueOperationsName opName
                               _     -> opName
                     capp  = App (Var clauseName False (rangeHide hrng))
                                 [(Nothing,Parens (Lam cparamsx body (getRange body)) cname prefix nameRng)] frng
                 -- addRangeInfo nameRng (RM.Id cname (RM.NIValue "fun" gtp "" False) [] False)
                 return (Nothing, capp)

       clauses <- mapM clause (zip (sortBy opName branches) resumeArgs)

       let grng = rangeNull
       let handlerCon = let hcon = Var handlerConName False hrng
                        in App hcon ([(Nothing,Lit (LitInt handlerCfc grng))] ++ clauses) rng
           handlerCfc = -- (\i -> App (Var nameInternalInt32 False grng) [(Nothing,Lit (LitInt i grng))] grng) $
                        if (null branches) then 1 --linear
                                           else foldr1 cfcLub (map hbranchCfc branches)
                      where
                        cfcLub x y   = if ((x==0&&y==1)||(x==1&&y==0)) then 2 else max x y
                        hbranchCfc b = case hbranchSort b of -- todo: more refined analysis
                                        OpVal    -> 1
                                        OpFun    -> 1
                                        OpExcept -> 0
                                        _        -> 3 --multi/wild
           -- create handler expression
           actionName = newHiddenName "action"
           handleName = makeHiddenName "handle" effectName
           handleRet  = case ret of -- todo: optimize return by using maybe<a->b> value in case no clause was given?
                          Nothing -> let argName = (newHiddenName "x")
                                     in Lam [ValueBinder argName Nothing Nothing rng rng] (Var argName False rng) hrng -- don't pass `id` as it needs to be opened
                          Just expr -> expr
           handleExpr action = App (Var handleName False rng)
                                [{-(Nothing,handlerCfc),-}(Nothing,handlerCon),(Nothing,handleRet),(Nothing,action)] hrng



       -- extract the action type for the case where it is higher-ranked (for scoped effects)
       -- this way we can annotate the action parameter with a higher-rank type if needed
       -- so it is propagated automatically.
       penv <- getPrettyEnv
       (_,handleTp,_)  <- resolveFunName handleName CtxNone rng rng
       (handleRho,_,_) <- instantiateEx rng handleTp
       let actionTp = case splitFunType handleRho of
                        Just ([_,_,actionTp],effTp,resTp) -> snd actionTp
                        _ -> failure ("Type.Infer: unexpected handler type: " ++ show (ppType penv handleRho))
       -- traceDoc $ \penv -> text " the handler action type: " <+> ppType penv actionTp <.> text ", prop: " <+> ppProp penv propagated
       let handlerExpr = Parens (Lam [ValueBinder actionName (Just actionTp) Nothing rng rng]
                                     (handleExpr (Var actionName False rng)) hrng) (newName "handler") "expr" rng

       -- and check the handle expression
       hres@(xhtp,_,_) <- inferExpr propagated expect handlerExpr
       htp <- subst xhtp
       -- traceDoc $ \penv -> text " the handler expr type: " <+> ppType penv htp <+> text ", prop: " <+> ppProp penv propagated

       -- extract handler effect
       let (actionTp1,heffect) = case splitFunScheme(htp) of
                        Just (_,_,[arg],heff,hresTp) -> (snd arg,heff)
                        _ -> failure $ "Type.Infer.inferHandler: unexpected handler type: " ++ show (ppType penv htp)

       if (not (labelIsLinear heff))
        then return ()
        else checkLinearity effectName heffect branches hrng rng

       -- insert a mask<local> over the action?
       if (not (containsLocalEffect heffect) || not allowMask)
         then do -- traceDoc $ \penv -> text "handler: no local in:" <+> ppType penv htp
                 return hres
         else do -- traceDoc $ \penv -> text "handler: found local:" <+> ppType penv htp
                 hp <- Op.freshTVar kindHeap Meta
                 let actionTp2  = removeLocalEffect penv actionTp1
                     handlerExprMask
                        = if isInstance
                           then let instName   = newHiddenName "hname"
                                in Lam [ValueBinder actionName (Just actionTp2) Nothing rng rng]
                                    (handleExpr (Lam [ValueBinder instName Nothing Nothing rng rng]
                                                   (Inject (TApp typeLocal [hp])
                                                      (Lam [] (App (Var actionName False rng) [(Nothing,Var instName False rng)] rng) rng)
                                                      False hrng) hrng)) hrng
                           else Lam [ValueBinder actionName (Just actionTp2) Nothing rng rng]
                                  (handleExpr (Lam [] (Inject (TApp typeLocal [hp]) (Var actionName False rng) False hrng) hrng)) hrng
                 inferExpr propagated expect handlerExprMask  -- and re-infer :-)

containsLocalEffect eff
  = let (ls,tl) = extractOrderedEffect eff
    in not (null (filter (\l -> labelName l == nameTpLocal) ls))

removeLocalEffect penv funTp
  = case splitFunScheme  funTp of
      Just (foralls,[],argTps,eff,resTp)
        -> let (ls,tl) = extractOrderedEffect eff
           in quantifyType foralls (TFun argTps (foldr effectExtend tl (filter (\l -> labelName l /= nameTpLocal) ls)) resTp)
      _ -> failure $ "Type.Infer.removeLocaEffect: unexpected type:" ++ show (ppType penv funTp)

checkLinearity effectName heffect branches hrng rng
  = do checkLinearClauses
       checkLinearEffect
  where
    checkLinearClauses
      = mapM_ check branches
      where
        check hbranch
          = if (hbranchSort hbranch <= OpFun) then return ()
             else do penv <- getPrettyEnv
                     contextError rng (hbranchPatRange hbranch)
                        (text "operation" <+> ppName penv (hbranchName hbranch) <+>
                         text ("needs to be linear but is handled in a non-linear way (as '" ++ show (hbranchSort hbranch) ++ "')"))
                        [(text "hint",text "use a 'val' or 'fun' operation clause instead")]

    checkLinearEffect
      = do let (effs,tl) = extractEffectExtend heffect
           --traceDoc $ \env -> text "operation" <+> text (show opName) <+> text ": " <+> niceType env effBranch -- hsep (map (\tp -> niceType env tp) effs)
           case (dropWhile labelIsLinear effs) of
             (e:_) -> do penv <- getPrettyEnv
                         contextError rng hrng
                            (text "handler for" <+> (ppName penv effectName) <+>
                             text "needs to be linear but uses a non-linear effect:" <+> ppType penv e)
                            [(text "hint",text "ensure only linear effects are used in a handler")]
             [] -> return ()



-- Infer the handled effect from looking at the operation clauses
inferHandledEffect :: Range -> HandlerSort -> Maybe Effect -> [HandlerBranch Type] -> Inf (Effect)
inferHandledEffect rng handlerSort mbeff ops
  = case mbeff of
      Just eff -> return (eff)
      Nothing  -> case ops of
        (HandlerBranch name pars expr opSort nameRng rng: _)
          -> -- todo: handle errors if we find a non-operator
             do let isInstance = isHandlerInstance handlerSort
                (qname,tp,info) <- resolveFunName name (CtxFunArgs (length pars + (if isInstance then 1 else 0)) [] Nothing) rng nameRng
                (rho,_,_) <- instantiateEx nameRng tp
                case splitFunType rho of
                  Just((opname,rtp):_,_,_) | isHandlerInstance handlerSort && opname == newHiddenName "hname"
                                -> do -- traceDoc $ \env -> text "effect instance: " <+> ppType env rtp
                                      case rtp of
                                        TApp (TCon ev) [teff]  | typeConName ev == nameTpEv -> return teff
                                        _  -> failure "Type.Infer.inferHandledEffect: illegal named effect type in operation?"
                  Just(_,eff,_) | not (isHandlerInstance handlerSort)
                                -> case extractEffectExtend eff of
                                    (ls,_) ->
                                      case filter isHandledEffect ls of
                                        (l:_) -> return (l)  -- TODO: can we assume the effect comes first?
                                        _ -> failure $ "Type.Infer.inferHandledEffect: cannot find handled effect in " ++ show eff
                  _ -> infError rng (text ("cannot resolve effect operation: " ++ show qname ++ ".") <--> text " hint: maybe wrong number of parameters?")
        _ -> infError rng (text "unable to determine the handled effect." <--> text " hint: use a `handler<eff>` declaration?")


-- Check coverage is not needed for type inference but gives nicer error messages
checkCoverage :: Range -> Effect -> Name -> [HandlerBranch Type] -> Inf ()
checkCoverage rng effect handlerConName branches
  = do (_,gconTp,conRepr,conInfo) <- resolveConName handlerConName Nothing rng
       let opNames = map (fieldToOpName . fst) (drop 1 {-cfc-} (conInfoParams conInfo))
           branchNames = map branchToOpName branches
       checkCoverageOf rng (map fst opNames) opNames branchNames
       return ()
  where
    modName = qualifier handlerConName

    fieldToOpName fname
      = let (pre,post)      = span (/='-') (nameLocal fname)
            (opSort,opName) = case (readOperationSort (drop 1 pre),post) of
                                (Just opSort, _:opName) | take 1 pre == "@" -> (opSort,opName)
                                _ -> failure $ "Type.Infer.checkCoverage: illegal operation field name: " ++ show fname ++ " in " ++ show handlerConName
        in (qualify modName (newQualified (nameModule fname) opName), opSort)

    branchToOpName hbranch
      = (qualify modName $
         if (isValueOperationName (hbranchName hbranch))     -- .val-<op>
          then fromValueOperationsName (hbranchName hbranch) else hbranchName hbranch,
         hbranchSort hbranch)

    checkCoverageOf :: Range -> [Name] -> [(Name,OperationSort)] -> [(Name,OperationSort)] -> Inf ()
    checkCoverageOf rng allOpNames opNames branchNames
      = -- trace ("check coverage: " ++ show opNames ++ " vs. " ++ show branchNames) $
        do env <- getPrettyEnv
           case opNames of
            [] -> if null branchNames
                   then return ()
                   -- should not occur if branches typechecked previously
                   else case (filter (\(bname,bsort) -> not (bname `elem` allOpNames)) branchNames) of
                          ((bname,bsort):_) -> termError rng (text "operator" <+> ppOpName env bname <+>
                                                     text "is not part of the handled effect") effect
                                                      [] -- hints
                          _        -> infError rng (text "some operators are handled multiple times for effect " <+> ppType env effect)
            ((opName,opSort):opNames')
              -> do let (matches,branchNames') = partition (\(bname,_) -> bname==opName) branchNames
                    case matches of
                      [(bname,bsort)]
                          -> if (opSort==OpVal && bsort /= opSort)
                              then infError rng (text "cannot handle a 'val' operation" <+> ppOpName env opName <+> text "with" <+> squotes (text (show bsort)))
                             else if (bsort > opSort)
                              then infWarning rng (text "operation" <+> ppOpName env opName <+> text "is declared as '" <.> text (show opSort) <.> text "' but handled here using '" <.> text (show bsort) <.> text "'")
                              else return ()
                      []  -> infError rng (text "operator" <+> ppOpName env opName <+> text "is not handled")
                      _   -> infError rng (text "operator" <+> ppOpName env opName <+> text "is handled multiple times")
                    checkCoverageOf rng allOpNames opNames' branchNames'
      where
        ppOpName env cname
          = ppName env cname


effectNameCore :: Effect -> Range -> Inf (Maybe Core.Expr,Name)
effectNameCore effect range
  = case expandSyn effect of
      -- handled effects (from an `effect` declaration)
      TApp (TCon tc) [hx]
        | (typeConName tc == nameTpHandled || typeConName tc == nameTpHandled1 ||
           typeConName tc == nameTpNHandled || typeConName tc == nameTpNHandled1)
        -> do let effName = effectNameFromLabel hx
              (effectNameVar,_,effectNameInfo) <- resolveName (toEffectTagName effName) Nothing range
              let effectNameCore = coreExprFromNameInfo effectNameVar effectNameInfo
              return (Just effectNameCore,effName)
      -- builtin effect
      _ -> do let effName = effectNameFromLabel effect
              return (Nothing,effName)

effectNameFromLabel :: Effect -> Name
effectNameFromLabel effect
  = case expandSyn effect of
      TApp (TCon tc) [hx]
        | (typeConName tc == nameTpHandled || typeConName tc == nameTpHandled1 ||
           typeConName tc == nameTpNHandled || typeConName tc == nameTpNHandled1) -> effectNameFromLabel hx
      TCon tc -> typeConName tc
      TSyn syn _ _ -> typeSynName syn
      TApp (TCon tc) targs -> typeConName tc
      _ -> failure ("Type.Infer.effectNameFromLabel: invalid effect: " ++ show effect)



{--------------------------------------------------------------------------
  infer applications and resolve overloaded identifiers
--------------------------------------------------------------------------}

inferApp :: Maybe (Type,Range) -> Expect -> Expr Type -> [(Maybe (Name,Range),Expr Type)] -> Range -> Inf (Type,Effect,Core.Expr)
inferApp propagated expect fun nargs rng
  = -- trace "infer: App" $
    do (fixed,named) <- splitNamedArgs nargs
       amb <- case rootExpr fun of
                (Var name _ nameRange)
                  -> do let sctx = fixedCountContext propagated (length fixed) (map (fst . fst) named)
                        matches <- lookupAppName False name sctx rng nameRange
                        -- traceDefDoc $ \env -> text "matched for: " <+> ppName env name <+> text " = " <+> pretty (length matches)
                        case matches of
                          Right (tp,funExpr,implicits)
                              -> return (Just (Just (tp,rng), funExpr, implicits)) -- known type, propagate the function type into the parameters
                          _   -> return Nothing -- many matches, -- start with argument inference and try to resolve the function type
                                 -- note: lookupAppName never unifies type variables so we should not emit errors on `Left []`.
                                 -- for example, in `fn(f) f()` the `f` has a type `_a` and will not match `sctx`.
                _ -> return (Just (Nothing,fun,[])) -- function expression first
       case amb of
         Just (prop,funExpr,implicits)
                  -> inferAppFunFirst prop funExpr [] fixed named implicits
         Nothing  -> inferAppFromArgs fixed named

  where
    -- infer the function type first, and propagate it to the arguments
    -- can take an `fresolved` list of fixed arguments that have been inferred already (in the case
    -- where a overloaded function name could only be resolved after inferring some arguments)
    inferAppFunFirst :: Maybe (Type,Range) -> Expr Type -> [(Int,FixedArg)] ->
                          [Expr Type] -> [((Name,Range),Expr Type)] -> [((Name,Range), Expr Type, (Bool -> Doc))] ->
                            Inf (Type,Effect,Core.Expr)
    inferAppFunFirst prop funExpr fresolved fixed named0 implicits
      = do -- traceDefDoc $ \penv -> text " inferAppFunFirst: fun:" <+> text (show funExpr) <+>
                                  --  text ("fixed count: " ++ show (length fixed)) <.>
                                  --  text (", named: " ++ show named0 ++ ", implicits: " ++ show implicits) <->
                                  --  text (", fres count: " ++ show (length fresolved)) <+> text ":" <+> ppProp penv prop

           -- only add resolved implicits that were not already named
           let alreadyGiven = [name | ((name,_),_) <- named0]
               rimplicits   = [imp | imp@((name,_),_,_) <- implicits, not (name `elem` alreadyGiven)]
               named        = named0 ++ [((name,rangeNull) {-so no range info is emmitted when checking -}
                                          , expr) | ((name,_),expr,_) <- rimplicits]

           penv <- getPrettyEnv
           mapM_ (\((name,_),_,fdoc) -> addRangeInfo (getRange funExpr) (RM.Implicits fdoc)) rimplicits

           -- infer type of function
           (ftp,eff1,fcore) <- allowReturn False $ inferExpr prop Instantiated funExpr
           -- traceDefDoc $ \penv -> text "inferred type of fun: " <+> ppType penv ftp

           -- match the type with a function type, wrap optional arguments, and order named arguments.
           -- traceDoc $ \env -> text "infer fun first, tp:" <+> ppType env ftp
           (iargs,pars0,funEff0,funTp0,coreApp) <- matchFunTypeArgs rng funExpr ftp fresolved fixed named


           -- match propagated type with the function result type
           -- note: we may disable this in the future?
           (pars,funEff,funTp) <- case propagated of
              Just (propRes,propRng) -> do inferSubsume (checkAnn propRng) rng funTp0 propRes
                                           pars1   <- subst pars0
                                           funEff1 <- subst funEff0
                                           funTp1  <- subst funTp0
                                           return (pars1,funEff1,funTp1)
              _ -> return (pars0,funEff0,funTp0)

           -- traceDefDoc $ \env -> text "infer App: propagated args:" <+> list (map (ppType env . snd) pars)

           -- infer the argument expressions and subsume the types
           (effArgs,coreArgs) <- -- withGammaType rng (TFun pars funEff funTp) $ -- ensure the free 'some' types are free in gamma
                                 do let check = case (fun) of
                                                   Var name _ _ | name == nameRunLocal
                                                     -> checkLocalScope rng
                                                   _ -> Infer rng
                                    inferArgsN check rng (zip (map snd pars) (map snd iargs))

           -- ensure arguments are evaluated in the declaration order
           core <- case shortCircuit fcore coreArgs of
                    Just cexpr -> return cexpr
                    Nothing ->
                      -- ensure named arguments are evaluated in the correct order
                      if (monotonic (map fst iargs) || all Core.isTotal coreArgs)
                        then return (coreApp fcore coreArgs)
                        else do -- let bind in evaluation order
                                vars <- mapM (\_ -> uniqueName "arg") iargs
                                let vargs = zip vars [(i,carg) | (carg,(i,_)) <- zip coreArgs iargs]
                                    eargs = sortBy (\(_,(i,_)) (_,(j,_)) -> compare i j) vargs
                                    defs  = [Core.DefNonRec (Core.Def var (Core.typeOf arg) arg Core.Private DefVal InlineAuto rangeNull "") | (var,(_,arg)) <- eargs]
                                    cargs = [Core.Var (Core.TName var (Core.typeOf arg)) Core.InfoNone | (var,(_,arg)) <- vargs]
                                if (Core.isTotal fcore)
                                then return (Core.makeLet defs (coreApp fcore cargs))
                                else do fname <- uniqueName "fct"
                                        let fdef = Core.DefNonRec (Core.Def fname ftp fcore Core.Private (defFun [] {-all own, TODO: maintain borrow annotations?-}) InlineAuto rangeNull "")
                                            fvar = Core.Var (Core.TName fname ftp) Core.InfoNone
                                        return (Core.Let (fdef:defs) (coreApp fvar cargs))
           -- take top effect
           -- todo: sub effecting should add core terms
           topEff <- inferUnifies (checkEffect rng) ((getRange fun, eff1) : zip (map (getRangeArg . snd) iargs) effArgs)
           inferUnify (checkEffectSubsume rng) (getRange fun) funEff topEff
           -- traceDoc $ \env -> (text " ** effects: " <+> tupled (map (ppType env) ([topEff, funEff, eff1] ++ effArgs)))

           -- instantiate or generalize result type
           funTp1 <- subst funTp
           -- traceDefDoc $ \env -> text " inferAppFunFirst: inst or gen:" <+> pretty (show expect) <+> colon <+> ppType env funTp1 <.> text ", top eff: " <+> ppType env topEff
           (resTp,resCore) <- maybeInstantiateOrGeneralize rng (getRange fun) topEff expect funTp1 core

           -- traceDoc $ \env -> text " inferAppFunFirst: resTp:" <+> ppType env resTp <.> text ", top eff: " <+> ppType env stopEff
           return (resTp,topEff,resCore )

    -- we cannot resolve an overloaded function name: infer types of arguments without propagation first.
    -- The code handles inferring arguments in any order by keeping track of the index, but at the moment
    -- we always infer from left to right until we can resolve the function type and then propagate argument types.
    inferAppFromArgs :: [Expr Type] -> [((Name,Range),Expr Type)] -> Inf (Type,Effect,Core.Expr)
    inferAppFromArgs [] named       -- there are no fixed arguments, use fun first anyways (and error..)
      = inferAppFunFirst Nothing fun [] [] named []
    inferAppFromArgs fixed named
      = do inferAppArgsFirst [] ({-sortBy (comparing (weight . snd))-} (zip [0..] fixed)) fixed named
      {-
      where
        weight expr
          = case expr of
              Lit _         -> 0
              Ann _ _ _     -> 0
              _             -> 10
      -}

    -- inferAppFirst <guessed fixed arg types> <priority order fixed args> <fixed args> <named args>
    inferAppArgsFirst :: [(Int,FixedArg)] -> [(Int,Expr Type)] -> [Expr Type] -> [((Name,Range),Expr Type)] -> Inf (Type,Effect,Core.Expr)
    inferAppArgsFirst fresolved [] fixed named     -- we inferred all fixed arguments
      = -- this always fails since we have not been able to resolve the function name
        {- if (not (null named))
          then infError rng (text "named arguments can only be used if the function is unambiguously determined by the context" <-> text " hint: annotate the function parameters?" )
          else -} inferAppFunFirst Nothing fun fresolved fixed named []

    inferAppArgsFirst fresolved ((idx,fix):fixs) fixed named  -- try to improve our guess
      = do --traceDoc $ \penv -> text "inferAppArgsFirst: "
           (tpArg,effArg,coreArg)  <- allowReturn False $ inferExpr Nothing Instantiated fix
           -- sfresolved <- mapM (\(i,(rng,tp,eff,carg)) -> do{ stp <- subst tp; return (i,(rng,stp,eff,carg)) }) fresolved
           let fresolved' = fresolved ++ [(idx,(getRange fix,tpArg,effArg,coreArg))]
           amb <- case rootExpr fun of
                    (Var name _ nameRange)
                      -> do sctx    <- fixedContext propagated fresolved' (length fixed) (map (fst . fst) named)
                            matches <- lookupAppName (null fixs {- allow disambiguate -}) name sctx rng nameRange
                            -- traceDoc $ \env -> text "app args matched for " <+> ppName env name <+>
                            --                     text " =" <+> pretty (length matches) <+> text ", " <+> pretty (length fixs) <+>
                            --                     text ", res:" <+> ppProp env propagated <+>
                            --                     text ", args:" <+> list (map (ppType env) (map (\(_,(_,tp,_,_)) -> tp) fresolved')) <+>
                            --                     text ", named:" <+> list (map (pretty . fst . fst) named)
                            case matches of
                              Right (itp,funExpr,implicits)
                                 -> return (Just ((itp,rng),funExpr,implicits))
                              _  -> return Nothing
                    _ -> return Nothing

           case amb of
             Just (prop,funExpr,implicits)
                      -> inferAppFunFirst (Just prop) funExpr fresolved' fixed named implicits
             Nothing  -> inferAppArgsFirst fresolved' fixs fixed named


getRangeArg :: ArgExpr -> Range
getRangeArg (ArgExpr expr _)      = getRange expr
getRangeArg (ArgCore (rng,_,_,_)) = rng
getRangeArg (ArgImplicit _ rng _) = rng


{--------------------------------------------------------------------------
  infer variables
--------------------------------------------------------------------------}

inferVar :: HasCallStack => Maybe (Type,Range) -> Expect -> Name -> Range -> Bool -> Inf (Type,Effect,Core.Expr)

-- constructor
inferVar propagated expect name rng isRhs  | isConstructorName name
  = -- trace("inferVar: constructor: " ++ show name)$
    do (qname1,tp1,conRepr,conInfo) <- resolveConName name (fmap fst propagated) rng
       let info1 = InfoCon Public tp1 conRepr conInfo rng (conInfoDoc conInfo)
       (qname,tp,info) <- do defName <- currentDefName
                             let creatorName = newCreatorName qname1
                             -- trace ("inferCon: " ++ show (defName,creatorName,qname1,nameCopy)) $ return ()
                             if (defName /= unqualify creatorName && defName /= nameCopy) -- a bit hacky, but ensure we don't call the creator function inside itself or the copy function
                               then do mbRes <- lookupFunName creatorName propagated rng
                                       case mbRes of
                                          Just (qname',tp',info') ->
                                            -- trace "creator found" $
                                            do return (qname',tp',info')
                                          Nothing  ->
                                            -- trace "no creator found" $
                                            do return (qname1,tp1,info1)
                               else return (qname1,tp1,info1)
       let coreVar = coreExprFromNameInfo qname info
       addRangeInfo rng (RM.Id (infoCanonicalName qname1 info1) (RM.NICon tp (infoDocString info1)) [] False)
       (itp,coref) <- maybeInstantiate rng expect tp
       -- traceDoc $ \env -> text "Type.Infer.Con: " <+> ppName env qname <+> text ":" <+> ppType env itp
       eff <- freshEffect
       return (itp,eff,coref coreVar)

-- variable
inferVar propagated expect name rng isRhs
  = -- trace("inferVar: " ++ show name) $
    -- we cannot directly "resolveName" with a propagated type
    -- as sometimes the types do not match and need to coerce due to local variables or references on the right-hand-side
    do vinfo <- case propagated of
                  Just prop | isRhs -> do -- traceDefDoc $ \penv -> text "inferVar" <+> ppParam penv (name,fst prop)
                                          resolveRhsName name prop rng
                  _                 -> resolveName name propagated rng
       inferVarName propagated expect name rng isRhs vinfo

inferVarName propagated expect name rng isRhs (qname,tp,info)
  = do -- traceDoc $ \env -> text "inferVarName:" <+> pretty name <+> colon <+> ppType env{showIds=True} tp
       if (isTypeLocalVar tp && isRhs)
        then do -- traceDoc $ \penv -> text "localvar:" <+> pretty name <+> text ":" <+> ppType penv tp
                let irng = extendRange rng (-1)
                (tp1,eff1,core1) <- inferExpr propagated expect (Parens (App (Var nameLocalGet False irng)
                                                                             [(Nothing,App (Var nameByref False irng)
                                                                                           [(Nothing,Var name False irng)] irng)] irng)
                                                                        name "var" rng)
                addRangeInfo rng (RM.Id qname (RM.NIValue (infoSort info) tp1  (infoDocString info) False) [] False)
                -- traceDoc $ \env -> text " deref" <+> pretty name <+> text "to" <+> ppType env tp1
                return (tp1,eff1,core1)
        else case info of
         InfoVal{ infoIsVar = True }  | isRhs  -- is it a right-hand side variable?
           -> do (tp1,eff1,core1) <- inferExpr propagated expect (App (Var nameDeref False rng) [(Nothing,App (Var nameByref False rng) [(Nothing,Var name False rng)] rng)] rng)
                 addRangeInfo rng (RM.Id qname (RM.NIValue (infoSort info) tp1 (infoDocString info) False) [] False)
                 return (tp1,eff1,core1)
         InfoVal{} | isValueOperation tp
           -> do addRangeInfo rng (RM.Id qname (RM.NIValue (infoSort info) tp (infoDocString info) False) [] False)
                 inferExpr propagated expect (App (Var (toValueOperationName qname) False rangeNull) [] rangeNull)
         _ -> do --  inferVarX propagated expect name rng qname1 tp1 info1
                 eff <- freshEffect
                 case lookup qname compilationConstants of
                  Just (tp,fcore)
                    -> do mod  <- getModuleName
                          return (tp,eff,fcore mod rng)
                  Nothing
                    -> do let coreVar = coreExprFromNameInfo qname info
                          -- traceDoc $ \env -> text "inferVar:" <+> pretty name <+> text ":" <+> text (show info) <.> text ":" <+> ppType env tp
                          (itp,coref) <- maybeInstantiate rng expect tp
                          sitp <- subst itp
                          addRangeInfo rng (RM.Id (infoCanonicalName qname info) (RM.NIValue (infoSort info) sitp (infoDocString info) False) [] False)
                          -- traceDoc $ \env -> (text " Type.Infer.Var: " <+> pretty name <.> colon <+> ppType env{showIds=True} sitp)
                          return (itp,eff,coref coreVar)

compilationConstants :: [(Name,(Type,Name -> Range -> Core.Expr))]
compilationConstants
  = [(nameCoreFileFile,   (typeString, \mod rng ->
        -- Core.Lit (Core.LitString (sourceName (rangeSource rng))))),
        Core.Lit (Core.LitString (showPlain mod ++ ".kk")))),  -- for now, use the module name to not leak info of a dev system
     (nameCoreFileLine,   (typeString, \mod rng -> Core.Lit (Core.LitString (show (posLine $ rangeStart rng))))),
     (nameCoreFileModule, (typeString, \mod rng -> Core.Lit (Core.LitString (showPlain mod))))
   ]

{--------------------------------------------------------------------------
  infer branches and patterns
--------------------------------------------------------------------------}

inferBranch :: Maybe (Type,Range) -> Type -> Range -> [Name] -> Branch Type -> Inf ([(Type,Effect)],Core.Branch)
inferBranch propagated matchType matchRange matchedNames branch@(Branch pattern guards)
  = inferPattern matchType (getRange branch) pattern (
    \pcore gcores ->
       do when (rangeNull /= getRange pattern) $
            -- check for unused pattern bindings
            do let defined = CoreVar.bv pcore
                   free    = S.fromList $ map Core.getName $ S.toList $ CoreVar.fv gcores
               case filter (\tname -> not (S.member (Core.getName tname) free)) (Core.tnamesList defined) of
                  [] -> return ()
                  (name:_) -> do env <- getPrettyEnv
                                 infWarning (getRange pattern) (text "pattern variable" <+> ppName env (Core.getName name) <+> text "is unused (or a wrongly spelled constructor?)" <->
                                                                text " hint: prepend an underscore to make it a wildcard pattern")
          return (Core.Branch [pcore] gcores)
    )
    $ \infGamma ->
     -- infGamma <- extractInfGamma pcore
     extendInfGammaEx False matchedNames infGamma $
      do -- check guard expressions
         unzip <$> mapM (inferGuard propagated (getRange branch)) guards

         {-
         (gtp,geff,gcore) <- allowReturn False $ inferExpr (Just (typeBool,getRange guard)) Instantiated guard
         inferUnify (checkGuardTotal (getRange branch)) (getRange guard) typeTotal geff
         inferUnify (checkGuardBool (getRange branch)) (getRange guard) typeBool gtp
         -- check branch expression
         (btp,beff,bcore) <- -- inferIsolated matchRange (getRange expr) $
                             inferExpr propagated Instantiated expr
         coreGuard <- subst (Core.Guard gcore bcore)
         return (btp,beff,coreGuard)
         -}
         {-
         resCore <- subst (Core.Branch [pcore] [Core.Guard gcore bcore])
         -- check for unused pattern variables
         let defined = CoreVar.bv pcore
             free    = S.fromList $ map Core.getName $ S.toList $ S.union (CoreVar.fv gcore) (CoreVar.fv bcore)
         case filter (\tname -> not (S.member (Core.getName tname) free)) (Core.tnamesList defined) of
           [] -> return ()
           (name:_) -> do env <- getPrettyEnv
                          infWarning (getRange pattern) (text "pattern variable" <+> ppName env (Core.getName name) <+> text "is unused (or a wrongly spelled constructor?)" <->
                                                       text " hint: prepend an underscore to make it a wildcard pattern")
         return (btp,beff,resCore)
         -}
  where
    extractInfGamma :: Core.Pattern -> Inf [(Name,Type)]
    extractInfGamma pattern
      = case pattern of
          Core.PatVar (Core.TName name tp) pat -> do stp <- subst tp
                                                     xs  <- extractInfGamma pat
                                                     return ((name,stp) : xs)
          Core.PatCon _ args _ _ _ _ _ _   -> do xss <- mapM extractInfGamma args
                                                 return (concat xss)
          Core.PatWild                     -> return []
          Core.PatLit _                    -> return []


inferGuard :: Maybe (Type,Range) -> Range -> Guard Type -> Inf ((Type,Effect),Core.Guard)
inferGuard propagated branchRange (Guard test expr)
  = do  (gtp,geff,gcore) <- allowReturn False $ inferExpr (Just (typeBool,getRange test)) Instantiated test
        inferUnify (checkGuardTotal branchRange) (getRange test) typeTotal geff
        inferUnify (checkGuardBool branchRange) (getRange test) typeBool gtp
        -- check branch expression
        (btp,beff,bcore) <- -- inferIsolated matchRange (getRange expr) $
                            inferExpr propagated Instantiated expr
        coreGuard <- subst (Core.Guard gcore bcore)
        return ((btp,beff),coreGuard)


inferPatternX :: Type -> Range -> Pattern Type -> Inf (Core.Pattern,[(Name,NameInfo)])
inferPatternX matchType branchRange pattern
  = do (_,res) <- inferPattern matchType branchRange pattern (\pcore infGamma -> return (pcore,infGamma)) $ \infGamma ->
                     do smatchType <- subst matchType
                        return ([(smatchType,typeTotal)],infGamma)
       return res

inferPattern :: HasTypeVar a => Type -> Range -> Pattern Type -> (Core.Pattern -> a -> Inf b)
                  -> ([(Name,NameInfo)] -> Inf ([(Type,Effect)],a))
                  -> Inf ([(Type,Effect)],b)
inferPattern matchType branchRange (PatCon name patterns0 nameRange range) withPattern inferGuards
  = do (qname,gconTp,repr,coninfo) <- resolveConName name Nothing range
       addRangeInfo nameRange (RM.Id qname (RM.NICon gconTp (conInfoDoc coninfo)) [] False)
       -- traceDoc $ \env -> text "inferPattern.constructor:" <+> pretty qname <.> text ":" <+> ppType env gconTp

       useSkolemizedCon coninfo gconTp branchRange range $ \conRho xvars ->
        do -- (conRho,tvars,_) <- instantiate range gconTp
           let (conParTps,conEffTp,conResTp) = splitConTp conRho
           inferUnify (checkConTotal range) nameRange conEffTp typeTotal
           inferUnify (checkConMatch range) nameRange conResTp matchType
           patterns <- matchPatterns range nameRange conRho conParTps patterns0
                       {-
                       if (length conParTps < length patterns0)
                        then do typeError range nameRange (text "constructor has too many arguments") (conTp) []
                                return (take (length conParTps) patterns0)
                        else return (patterns0 ++ (replicate (length conParTps - length patterns0) (Nothing,PatWild range)))
                       -}
           (cpatterns,infGammas) <- fmap unzip $ mapM (\(parTp,pat) ->
                                                   do sparTp <- subst parTp
                                                      inferPatternX sparTp branchRange pat)
                                            (zip (map snd conParTps) (patterns))
           let infGamma  = concat infGammas
           (btpeffs,coreGuards0) <- inferGuards infGamma
           {-
           (btp,beff,bcore0) <- inferBranchCont pcore infGamma
           )
           return ((btp,beff,bcore), ftv btp `tvsUnion` ftv beff)
           -}
           let (pcore,coreGuards)
                = if (null xvars) then (Core.PatCon (Core.TName qname conRho) cpatterns repr (map snd conParTps) [] conResTp coninfo False, coreGuards0)
                  else let bindExists = [(TypeVar id kind Bound) | (TypeVar id kind _) <- xvars]
                           subExists  = subNew [(TypeVar id kind Skolem, TVar (TypeVar id kind Bound)) | TypeVar id kind _ <- bindExists]
                           pcore     = Core.PatCon (Core.TName qname conRho) (subExists |-> cpatterns) repr (subExists |-> (map snd conParTps)) bindExists conResTp coninfo False
                           coreGuards = subExists |-> coreGuards0
                       in (pcore,coreGuards)

           bcores <- withPattern pcore coreGuards
           return ((btpeffs,bcores),ftv btpeffs)
  where
    useSkolemizedCon :: ConInfo -> Type -> Range -> Range -> (Rho -> [TypeVar] -> Inf (a,Tvs)) -> Inf a
    useSkolemizedCon coninfo gconTp range nameRange cont  | null (conInfoExists coninfo)
      = do (conRho,_,_) <- instantiate nameRange gconTp
           (res,_) <- cont conRho []
           return res

    useSkolemizedCon coninfo gconTp range nameRange cont
      = do conResTp <- Op.freshTVar kindStar Meta
           let conExistsTp = TForall (conInfoExists coninfo) [] (if (null (conInfoParams coninfo)) then conResTp else TFun (conInfoParams coninfo) typeTotal conResTp)
           withSkolemized range conExistsTp Nothing $ \conXRho0 xvars ->
            do conXRho <- Op.instantiate nameRange (TForall (conInfoForalls coninfo) [] conXRho0)
               (iconRho,_,_)  <- instantiate nameRange gconTp
               -- traceDoc $ \env -> text " conXRho:" <+> ppType env conXRho <+> text ", versus iconRho:" <+> ppType env iconRho
               inferUnify (checkOp range) nameRange conXRho iconRho
               conRho <- subst iconRho
               cont conRho xvars


inferPattern matchType branchRange (PatVar binder) withPattern inferPart
  =  do addRangeInfo (binderNameRange binder) (RM.Id (binderName binder) (RM.NIValue "val" matchType "" (isAnnotatedBinder binder)) [] True)
        case (binderType binder) of
          Just tp -> inferUnify (checkAnn (getRange binder)) (binderNameRange binder) matchType tp
          Nothing -> return ()
        (cpat,infGamma0) <- inferPatternX matchType branchRange (binderExpr binder)
        let infGamma = ([(binderName binder,(createNameInfoX Public (binderName binder) DefVal (binderNameRange binder) matchType ""))] ++ infGamma0)
        (btpeffs,x) <- inferPart infGamma
        res <- withPattern (Core.PatVar (Core.TName (binderName binder) matchType) cpat) x
        return (btpeffs,res)

inferPattern matchType branchRange (PatWild range) withPattern inferPart
  =  do (btpeffs,x) <- inferPart []
        res <- withPattern Core.PatWild  x
        return (btpeffs,res)

inferPattern matchType branchRange (PatAnn pat tp range) withPattern inferPart
  = inferPattern tp range pat withPattern $ \infGamma ->
      do inferUnify (checkAnn range) (getRange pat) matchType tp  -- TODO: improve error message
         inferPart infGamma

inferPattern matchType branchRange (PatParens pat range) withPattern inferPart
  = inferPattern matchType branchRange pat withPattern inferPart

inferPattern matchType branchRange (PatLit lit) withPattern inferPart
  = let (pat,tp) = case lit of
                    LitInt i _  -> (Core.PatLit (Core.LitInt i), typeInt)
                    LitChar c _  -> (Core.PatLit (Core.LitChar c), typeChar)
                    LitFloat f _  -> (Core.PatLit (Core.LitFloat f), typeFloat)
                    LitString s _  -> (Core.PatLit (Core.LitString s), typeString)
    in do inferUnify (checkLitMatch (getRange lit)) branchRange tp matchType
          (btpeffs,x) <- inferPart []
          res <- withPattern pat x
          return (btpeffs,res)
{-
inferPattern matchType branchRange pattern
  = todo ("Type.Infer.inferPattern")
-}

splitConTp :: Type -> ([(Name,Type)],Effect,Type)
splitConTp tp
  = case expandSyn tp of
      TFun args eff res -> (args,eff,res)
      res               -> ([],typeTotal,res)


inferBinders :: [(Name,NameInfo)] -> [ValueBinder Type ()] -> [(Name,NameInfo)]
inferBinders infgamma binders
  = case binders of
      [] -> infgamma
      (par:pars) ->
        let info = (binderName par,createNameInfoX Public (binderName par) DefVal (getRange par) (binderType par) "")
        in inferBinders (infgamma ++ [info]) pars



-- infer implicit parameter declaration:
-- check if the expression is a single identifier and possibly generate unpacking of fields
inferImplicitParam par
  = if isImplicitParamName (binderName par)
     then  do -- let pname = plainImplicitParamName (binderName par)
              unpack <- case binderExpr par of
                Just (Parens (Var qname _ rng) _ _ _) -- encoded in the parser as a default expression
                           -> inferImplicitUnpack (rangeHide (binderRange par)) (rangeHide rng) (binderName par) qname
                Nothing    -> return id
                Just expr  -> do contextError (getRange par) (getRange expr) (text "the value of an implicit parameter must be a single identifier") []
                                 return id
              return (par{ -- leave the binder name locally qualified as `@implicit/name` -- binderName = pname,
                           binderExpr = Nothing }, unpack)
     else return (par, id)

inferImplicitUnpack :: Range -> Range -> Name -> Name -> Inf (Expr Type -> Expr Type)
inferImplicitUnpack rng nrng pname qname
  = do nt <- getNewtypes
       case newtypesLookupAny qname nt of
        Just (DataInfo{dataInfoSort=Inductive,
                        dataInfoConstrs=[conInfo],
                        dataInfoDef=ddef})  | not (dataDefIsOpen ddef)
        -- struct: unpack the fields
           -> let pats = [PatVar (ValueBinder fname Nothing (PatWild nrng) nrng rng)
                            | (fname,ftp) <- conInfoParams conInfo, not (nameIsNil fname)]
                  pat  = PatCon (conInfoName conInfo) [(Nothing,p) | p <- pats] nrng rangeNull {- no warnings for unused pattern variables by using a null range -}
                  unpack body
                       = Case (Var pname False nrng) [Branch pat [Guard guardTrue body]] rng
                  bases :: [(Name,Name)]
                  bases = [(fname,fqname) | (fname,ftp) <- conInfoParams conInfo,
                                            nameStartsWith fname "base",
                                            fqname <- extractStructType ftp]
                  compose [] e     = e
                  compose (f:fs) e = f (compose fs e)

                  extractStructType :: Type -> [Name]
                  extractStructType tp
                    = case expandSyn tp of
                        TApp (TCon tcon) targs -> [typeConName tcon]
                        TCon tcon              -> [typeConName tcon]
                        _                      -> []

              in do unpackBases <- mapM (\(fname,fqname) -> inferImplicitUnpack rng nrng fname fqname) bases  -- todo: stop recursion!
                    return (compose (unpack:unpackBases))

        _  -> do traceDefDoc $ \penv -> text "inferImplicitUnpack: cannot resolve" <+> text (show qname)
                 return id


-- | Infer automatic unwrapping for parameters with default values, and adjust their type from optional<a> to a
-- Takes an accumulated InfGamma (initially empty), a list of parameters (as value binders) and returns
-- the new InfGamma and a substitution from optional paramter names to the local unique names (of type a)
-- and a list of core (non-recursive) bindings (where the substitution has already been applied)
inferOptionals :: Effect -> [(Name,NameInfo)] -> [ValueBinder Type (Maybe (Expr Type))] -> Inf ([(Name,NameInfo)],[(Core.TName,Core.Expr)],[Core.Def])
inferOptionals eff infgamma []
  = return (infgamma,[],[])

inferOptionals eff infgamma (par:pars)
  = case binderExpr par of
     Nothing
      -> inferOptionals eff (infgamma ++ [(binderName par,createNameInfoX Public (binderName par) DefVal (getRange par) (binderType par) "")]) pars

     Just expr  -- default value
      -> do let fullRange = combineRanged par expr
                -- partp = binderType par
                -- optTp = makeOptional partp
                optTp = binderType par

            -- infer parameter type from optional
            tvar <- Op.freshTVar kindStar Meta
            inferUnify (Infer fullRange) (getRange par) optTp (makeOptionalType tvar)
            partp <- subst tvar

            -- infer expression
            (exprTp,exprEff,coreExpr) <- extendInfGamma infgamma $ inferExpr (Just (partp,getRange par))
                                             (if isRho partp then Instantiated else Generalized False) expr
            inferUnify (checkOptional fullRange) (getRange expr) partp exprTp

            -- check optional expressions; avoid unifying with `eff` type if possible
            sexprEff <- subst exprEff
            case (sexprEff) of
              TVar _ -> inferUnify (checkOptionalTotal fullRange) (getRange expr) typeTotal exprEff
              _ -> if (isTypeTotal exprEff) then return ()
                    else -- trace ("optional effect type: " ++ show exprEff) $
                         inferUnify (Infer fullRange) (getRange expr) eff exprEff

            tp <- subst partp
            let infgamma' = infgamma ++ [(binderName par,createNameInfoX Public (binderName par) DefVal (getRange par) tp "")]

            -- build up core to get the optional value
            local <- uniqueNameFrom (binderName par)
            temp  <- uniqueNameFrom (binderName par)
            -- let coreVar (qname,tp,info) = Core.Var (Core.TName qname tp) (coreVarInfoFromNameInfo info)
            dataInfo <- findDataInfo nameTpOptional
            (coreNameOpt,coreTpOpt,coreReprOpt,conInfoOpt) <- resolveConName nameOptional Nothing fullRange
            (coreNameOptNone,coreTpOptNone,coreReprOptNone,conInfoOptNone) <- resolveConName nameOptionalNone Nothing fullRange
            let tempName = Core.TName temp tp
            let parName  = Core.TName (binderName par) optTp
                corePar = Core.Var parName Core.InfoNone
                coreResTp = TApp (TCon (TypeCon (dataInfoName dataInfo) (dataInfoKind dataInfo))) [tp]
                init = Core.Case [corePar]
                       [  Core.Branch [ Core.PatCon (Core.TName coreNameOpt coreTpOpt)
                                                    [Core.PatVar tempName Core.PatWild]
                                                    (coreReprOpt)
                                                    [tp]
                                                    []
                                                    coreResTp
                                                    conInfoOpt
                                                    False
                                      ]
                                      [ Core.Guard   Core.exprTrue (Core.Var tempName Core.InfoNone) ]
                       ,  Core.Branch [ Core.PatCon (Core.TName coreNameOptNone coreTpOptNone)
                                                    []
                                                    (coreReprOptNone)
                                                    []
                                                    []
                                                    coreResTp
                                                    conInfoOptNone
                                                    True {-skip test-}
                                      ]
                                      [ Core.Guard   Core.exprTrue coreExpr ]
                       ]
                def  = Core.Def local partp init Private DefVal InlineNever (binderNameRange par) ""
                sub  = [(Core.TName (binderName par) tp, Core.Var (Core.TName local partp) Core.InfoNone)]
                -- coref core
                --   = Core.Let [Core.DefNonRec def] ((CoreVar.|~>) sub core)

            -- infer the rest
            (infgamma2,sub2,defs2) <- inferOptionals eff infgamma' pars
            return (infgamma2,sub ++ sub2,def : ((CoreVar.|~>) sub defs2))


checkFun        = Check "function type does not match the argument types"
checkMatch      = Check "branch has not the same type as previous branches"
checkAnn        = Check "type cannot be instantiated to match the annotation"
checkRec        = Check "recursive invocations do not match the assumed type; add a type annotation?"
checkGuardTotal = Check "guard expressions must be total"
checkGuardBool  = Check "guard expressions must be of a boolean type"
checkConMatch   = Check "constructor must have the same the type as the matched term"
checkLitMatch   = Check "literal pattern does not match the type of the matched term"
checkConTotal   = Check "constructor in a pattern must have a total effect type"
checkLit        = Check "literal does not match the expected type"
checkOptional   = Check "default value does not match the parameter type"
checkOptionalTotal = Check "default value expression must be total"
checkInject     = Check "inject expects a parameter-less function argument"

checkEffect        = Infer
checkEffectSubsume = Check "effect cannot be subsumed"

checkReturnResult  = Check "function returns values of different types"
checkReturn        = Check "return type does not match an earlier return type"

checkOp         = Check "operator type does not match the parameter types"
checkMakeHandler= Check "handler types do not match the handler maker; please report this as a bug!"
checkMakeHandlerBranch = Check "handle branch types do not match the handler branch maker; please report this as a bug!"
checkEffectTp   = Check "operator type does not match the effect type"

checkLocalScope = Check "a reference to a local variable escapes it's scope"



rootExpr expr
  = case expr of
      -- Let defs e _ -> rootExpr e
      -- Bind def e _ -> rootExpr e
      -- Ann e t r  -> rootExpr e  -- better to do FunFirst in this case
      Parens e _ _ r -> rootExpr e
      _              -> expr



{--------------------------------------------------------------------------
  infer arguments
--------------------------------------------------------------------------}

-- infer the types of argument expressions, some of which may have already been inferred (`:FixedArg`).
inferArgsN :: Context -> Range -> [(Type,ArgExpr)] -> Inf ([Effect],[Core.Expr])
inferArgsN ctx range parArgs
  = do res <- inferArg [] parArgs
       return (unzip res)
  where
    inferArgExpr tp argexpr hidden -- TODO: add context for better error messages for implicit parameters?
      = (if hidden then withNoRangeInfo else id) $
        allowReturn False $
        inferExpr (Just (tp,getRange argexpr))
          (if isRho tp then Instantiated else Generalized False)
          argexpr

    inferArg acc []
      = return (reverse acc)
    inferArg acc ((tpar,arg):args)
      = do tpar0 <- subst tpar
           let subsumeArg (targ,teff,core)
                 = do tpar1  <- subst tpar0
                      (_,coref)  <- inferSubsume ctx (getRangeArg arg) tpar1 targ
                                    {- case arg of
                                      ArgExpr argexpr | isAnnot argexpr
                                        -> do -- traceDefDoc $ \env -> text "inferArgN1:" <+> ppType env tpar1 <+> text "~" <+> ppType env targ
                                              inferSubsume ctx (getRangeArg arg) tpar1 targ
                                              -- inferUnify ctx (getRange argexpr) tpar1 targ
                                              return (tpar1,id)
                                      _ -> do -- traceDefDoc $ \env -> text "inferArgN2:" <+> (ppType env teff) <+> colon <+> ppType env tpar1 <+> text "~" <+> ppType env targ
                                              inferSubsume ctx (getRangeArg arg) tpar1 targ
                                      -}

                      teff1  <- subst teff
                      return (teff1,coref core)

           (eff,core)  <- case arg of
                            ArgExpr argexpr hidden
                              -> do res <- inferArgExpr tpar0 argexpr hidden
                                    subsumeArg res
                            ArgCore (_,ctp,ceff,carg)
                              -> do -- traceDefDoc $ \penv -> text "inferArgN: argCore:" <+>
                                    --                            ppType penv tpar0 <+> text ", ctp:"
                                    --                            <+> ppType penv ctp
                                    res <- if isRho tpar0
                                            then return (ctp,ceff,carg)
                                            else do (gtp,garg) <- generalize range range False ceff ctp carg
                                                    return (gtp,ceff,garg)
                                    subsumeArg res
                            ArgImplicit name rng nameRng
                              -> do (argExpr,termInfo) <- resolveImplicitName name tpar0 rng (endOfRange rng)
                                    -- for any errors in `rng` we can show a nice representation as `termInfo` since
                                    -- the `argExpr` is generated by implicit name resolving
                                    withHiddenTermDoc rng termInfo $
                                      do res <- inferArgExpr tpar0 argExpr True {- hidden -}
                                         subsumeArg res

           inferArg ((eff,core) : acc) args


-- | Is an expression annotated?
isAnnot (Parens expr _ _ rng) = isAnnot expr
isAnnot (Ann expr tp rng)     = True
isAnnot (Let defs body rng)   = isAnnot body
isAnnot (Bind defs body rng)  = isAnnot body
isAnnot _                     = False

splitNamedArgs :: Ranged e => [(Maybe (Name,Range),e)] -> Inf ([e],[((Name,Range),e)])
splitNamedArgs nargs
  = do let (nfixed,rest)      = span (isNothing . fst) nargs
           (nnamed,morefixed) = span (not . isNothing . fst) rest
           fixed              = map snd nfixed
           named              = [((name,rng),expr) | (Just (name,rng),expr) <- nnamed]
       -- check that named arguments all come after the positional ones
       case morefixed of
         [] -> return ()
         (arg:_) -> infError (getRange (snd arg)) (text "positional arguments cannot follow named arguments")
       checkDuplicates [] named

       return (fixed,named)
  where
    checkDuplicates seen named
      = case named of
          [] -> return ()
          (((name,rng),_):named)
            -> if (name `elem` seen)
                then do env <- getPrettyEnv
                        infError rng (text "named argument" <+> ppName env name <+> text "is given more than once")
                else checkDuplicates (name:seen) named

isNothing Nothing = True
isNothing _       = False


matchPatterns :: Range -> Range -> Type -> [(Name,Type)] -> [(Maybe (Name,Range),Pattern Type)] -> Inf [Pattern Type]
matchPatterns context nameRange conTp conParTypes patterns0
  = do patterns1 <- if (length conParTypes < length patterns0)
                     then do typeError context nameRange (text "constructor has too many arguments") (conTp) []
                             return (take (length conParTypes) patterns0)
                     else return patterns0

       (fixed,named) <- splitNamedArgs patterns1
       (pars1,pats1) <- matchFixed (zip [0..] conParTypes) fixed
       ipats2        <- matchNamed pars1 named
       return (pats1 ++ map snd (sortBy (\(i,_) (j,_) -> compare i j) ipats2))
  where
    matchFixed :: [(Int,(Name,Type))] -> [Pattern Type] -> Inf ([(Int,(Name,Type))],[Pattern Type])
    matchFixed pars pats
      = return (drop (length pats) pars, pats)

    matchNamed :: [(Int,(Name,Type))] -> [((Name,Range),Pattern Type)] -> Inf [(Int,Pattern Type)]
    matchNamed [] []
      = return []
    matchNamed [] (((_,rng),pat):_)
      = do -- typeError context (combineRanged rng pat) (text "constructor is given too many arguments") tp []
           return []
    matchNamed pars (((name,rng),pat):named)
      = case remove name [] pars of
          Nothing -> do typeError context rng (text "there is no constructor field with name" <+> pretty name) conTp []
                        matchNamed pars named
          Just (i,pars1)
              -> do rest <- matchNamed pars1 named
                    return ((i,pat):rest)
    matchNamed pars []
      = return [(i,PatWild context) | (i,_) <- pars]

    remove name acc []
      = Nothing
    remove name acc (par@(i,(parName,parType)):pars)
      = if (name == parName)
         then Just (i,reverse acc ++ pars)
         else remove name (par:acc) pars


-- Match given fixed arguments and named arguments with a function type.
-- some fixed arguments may already have been inferred (`fresolved`).
-- also returns a list of implicit arguments that need to be resolved.
-- returns:
-- - iargs: a list of indexed argument terms, either user expressions, already resolved (from `fresolved`), or implicit names
--          the indices indicate the required evaluation order (due to named arguments).
-- - pars: list of the full function parameters (name,type)
-- - effect of the function
-- - the result type of the function
-- - a core transformer that modifies the application appropriately (for constructor copy)

data ArgExpr
  = ArgExpr (Expr Type) Bool {- hide range info -}
  | ArgCore FixedArg
  | ArgImplicit Name Range {- application range -} Range {- name range -}

matchFunTypeArgs :: Range -> Expr Type -> Type -> [(Int,FixedArg)] -> [Expr Type] -> [((Name,Range),Expr Type)]
                     -> Inf ([(Int,ArgExpr)], [(Name,Type)], Effect, Type, Core.Expr -> [Core.Expr] -> Core.Expr)
matchFunTypeArgs context fun tp fresolved fixed named
  = case tp of
       TFun pars eff res   -> do iargs <- matchParameters pars fresolved fixed named
                                 -- trace ("matched parameters: " ++ show (pars,map fst args)) $
                                 return (iargs,pars,eff,res,Core.App)
       TSyn _ _ t          -> matchFunTypeArgs context fun t fresolved fixed named
       TVar tv             -> do if (null named)  -- TODO: take fresolved into account
                                  then return ()
                                  else infError range (text "cannot used named arguments on an inferred function" <-> text " hint: annotate the parameters")
                                 targs <- mapM (\name -> do{ tv <- Op.freshTVar kindStar Meta; return (name,tv)}) ([nameNil | a <- fixed] ++ map (fst . fst) named)
                                 teff  <- Op.freshTVar kindEffect Meta
                                 tres  <- Op.freshTVar kindStar Meta
                                 -- trace ("Type.matchFunType: " ++ show tv ++ ": " ++ show (targs,teff,tres)) $
                                 extendSub (subSingle tv (TFun targs teff tres))
                                 return (zip [0..] (map (\x -> ArgExpr x False) (fixed ++ map snd named)), targs,teff,tres,Core.App)
       _  -> do -- apply the copy constructor if we can find it
                matches <- lookupNameCtx isInfoValFunExt nameCopy (CtxFunTypes True [tp] [] Nothing) range
                case matches of
                  [(qname,info)]
                    -> do (contp,_,coreInst) <- instantiateEx range (infoType info)
                          (iargs,pars,eff,res,_) <- matchFunTypeArgs context fun contp fresolved (fun:fixed) named
                          let coreAddCopy core coreArgs
                                = let coreVar = coreExprFromNameInfo qname info
                                  in (Core.App (coreInst coreVar) (coreArgs))
                          return (iargs,pars,eff,res,coreAddCopy)
                  _ -> do typeError context range (text "only functions or types with a copy constructor can be applied") tp []
                          return (zip [1..] (map (\x -> ArgExpr x True) (fixed ++ map snd named)), [], typeTotal, typeUnit, Core.App)
  where
    range = getRange fun

    matchParameters :: [(Name,Type)] -> [(Int,FixedArg)] -> [Expr Type] -> [((Name,Range),Expr Type)] -> Inf [(Int,ArgExpr)]
    matchParameters pars fresolved fixed named
      = -- trace ("match parameters: " ++ show (pars,length fixed,map (fst.fst) named)) $
        do (pars1,args1) <- matchFixed pars (zip [0..] fixed) fresolved
           iargs2        <- matchNamed (zip [length fixed..] pars1) (zip [length fixed..] named)
           return (args1 ++ map snd (sortBy (\(i,_) (j,_) -> compare i j) iargs2))

    matchFixed :: [(Name,Type)] -> [(Int,Expr Type)] -> [(Int,FixedArg)] -> Inf ([(Name,Type)],[(Int,ArgExpr)])
    matchFixed pars [] fresolved
      = return (pars,[])
    matchFixed ((name,tp):pars) ((i,arg):fixed) fresolved
      = case lookup i fresolved of
          Just (crng,ctp,ceff,carg) ->
            do (newtp,newarg) <- if (isOptional tp)
                                   then return (makeOptionalType ctp, Core.wrapOptional ctp carg)
                                   else return (ctp,carg)
               (prest,rest) <- matchFixed pars fixed fresolved
               return (prest, (i,ArgCore (crng,newtp,ceff,newarg)):rest)
          Nothing ->
            do newarg <- if (isOptional tp)
                          then return (wrapOptional arg)
                        else if (isDelay tp)
                          then wrapDelay arg
                          else return arg
               (prest,rest) <- matchFixed pars fixed fresolved
               return (prest, (i,ArgExpr newarg False):rest)

    matchFixed [] ((i,arg):_) fresolved
      = do typeError context (getRange fun) (text "function is applied to too many arguments") tp []
           return ([],[])

    -- in the result, the first int is position of the parameter `j`, the second int `i` is the original position of
    -- the argument (so we can evaluate in argument order)
    matchNamed :: [(Int,(Name,Type))] -> [(Int,((Name,Range),Expr Type))] -> Inf [(Int,(Int,ArgExpr))]
    matchNamed [] []
      = return []
    matchNamed [] ((i,((name,rng),arg)):named)
      = do typeError context (getRange fun) {- (combineRanged rng arg) -} (text "function is applied to too many arguments") tp []
           return []
    matchNamed pars ((i,((name,rng),arg)):named)
      = case extract name [] pars of
          Nothing -> do typeError context (getRange fun) (text "there is no parameter with name" <+> pretty name) tp []
                        matchNamed pars named
          Just (j,tp,pars1)
              -> do newarg  <- if (isOptional tp)
                                then return (wrapOptional arg)
                               else if (isDelay tp)
                                then wrapDelay arg
                                else return arg
                    rest <- matchNamed pars1 named
                    return ((j,(i,ArgExpr newarg (rangeIsNull rng))):rest)
    matchNamed pars []
      = do if (all (Op.isOptionalOrImplicit . snd) pars)
            then do let (optionals,implicits) = span (isOptional . snd . snd) pars
                        opts = [(j,(i,ArgExpr makeOptionalNone True))
                                | (i,(j,(name,tpar))) <- zip [(length fixed + length named)..] optionals]
                        imps = [(j,(i,ArgImplicit (snd (splitImplicitParamName name)) context range))
                                | (i,(j,(name,tpar))) <- zip [(length fixed + length named + length optionals)..] implicits]
                    return (opts ++ imps)
            else do let hints = case rootExpr fun of
                                  (Var name isOp nameRange) | name == newName "resume"
                                    -> [(text "hint", text "cannot use \"resume\" inside a val/fun/except clause")]
                                  _ -> []
                    typeError context range (text "function has not enough arguments") tp hints
                    return []

    extract name acc []
      = Nothing
    extract name acc (par@(i,(parName,parType)):pars)
      = if (name == parName || name == fst (splitImplicitParamName parName))
         then Just (i,parType,reverse acc ++ pars)
         else extract name (par:acc) pars

    wrapOptional :: Expr Type -> Expr Type
    wrapOptional expr
      = App (Var nameOptional False (rangeNull {- getRange expr -}))  -- use a null range so it doesn't show in the documentation
            [(Nothing,expr)] (getRange expr)

    wrapDelay :: Expr Type -> Inf (Expr Type)
    wrapDelay expr
      = do delayed <- isDelayed expr
           if delayed
            then return expr
            else return (Lam [] expr (getRange expr))
      where
        isDelayed expr
          = case expr of
              Lam [] _ _   -> return True
              Var name _ _ -> do matches <- lookupNameCtx isInfoValFunExt name (CtxFunArgs 0 [] Nothing) (getRange expr)
                                 case matches of
                                   [(_,info)] -> return (isDelayedType (infoType info))
                                   _          -> return False
              _            -> return False

        isDelayedType tp
          = case expandSyn tp of
              TFun [] _ _ -> True
              _           -> False

    makeOptionalNone :: Expr Type
    makeOptionalNone
      = Var nameOptionalNone False rangeNull

    isDelay :: Type -> Bool
    isDelay tp
      = case tp of
          TSyn syn [_,_] _ -> (typesynName syn == nameTpDelay)
          _ -> False



{--------------------------------------------------------------------------
  Effects
--------------------------------------------------------------------------}
{-
addTopMorphisms :: Range -> [(Range,Effect)] -> Inf Effect
addTopMorphisms range effs0
  = -- trace ("add morphisms: " ++ show (map snd effs0)) $
    do effs1 <- subst effs0
       topEffects range (filter (not . isTypeTotal) (map snd effs1))
-}



freshEffect :: Inf Effect
freshEffect
  = Op.freshTVar kindEffect Meta


{--------------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------------}
instantiateBinder :: ValueBinder (Maybe Type) e -> Inf (ValueBinder Type e)
instantiateBinder binder
  = do tp <- case binderType binder of
              Just tp -> return tp
              Nothing -> Op.freshTVar kindStar Meta
       return binder{ binderType = tp }

maybeGeneralize :: Range -> Range -> Effect -> Expect -> Rho -> Core.Expr-> Inf (Scheme,Core.Expr)
maybeGeneralize contextRange range eff expect tp core
  = case expect of
      Instantiated -> return (tp,core)
      Generalized close  -> generalize contextRange range close eff tp core

maybeInstantiate :: Range -> Expect -> Scheme -> Inf (Rho,Core.Expr -> Core.Expr)
maybeInstantiate range expect tp
  = case expect of
      Generalized close  -> return (tp,id)
      Instantiated -> do (rho,_,coref) <- instantiate range tp
                         return (rho,coref)


maybeInstantiateOrGeneralize :: Range -> Range -> Effect -> Expect -> Type -> Core.Expr -> Inf (Type,Core.Expr)
maybeInstantiateOrGeneralize contextRange range eff expect tp core
  = case expect of
      Generalized close  -> generalize contextRange range close eff tp core
      Instantiated -> do (tp,_,coref) <- instantiate range tp
                         return (tp, coref core)


-- | Try to match the propagated type with a function type,
-- returning the propagated argument, effect, and result type, and the expected instantiation of the result
matchFun :: Int -> Maybe (Type,Range) -> Inf ([Maybe (Name,Type)],Maybe (Type,Range), Maybe (Type,Range), [TypeVar], Expect)
matchFun nArgs mbType
  = case mbType of
      Nothing       -> return (replicate nArgs Nothing,Nothing,Nothing,[],Instantiated)
      Just (tp,rng) -> do -- (rho,_,_) <- instantiate rng tp
                          -- let skolems = []
                          -- traceDoc $ \penv -> text "matchFun: " <+> ppType penv{showKinds=True,showIds=True} tp
                          (skolems,_,rho,_) <- Op.skolemizeEx rng tp
                          -- traceDoc $ \penv -> text " skolemized: " <+> ppType penv rho
                          -- let sub = subNew [(tv,TVar (tv{typevarFlavour=Meta})) | tv <- skolems]
                          case splitFunType rho of
                           Nothing
                             -> do -- traceDoc $ \penv -> text "  matchfun.no match:" <+> ppType penv tp
                                   return (replicate nArgs Nothing,Nothing,Nothing,skolems,Instantiated)
                           Just (args,eff,res)
                            -> let m = length args
                               in -- can happen: see test/type/wrong/hm4 and hm4a
                                  --assertion ("Type.Infer.matchFun: expecting " ++ show nArgs ++ " arguments but found propagated " ++ show m ++ " arguments!") (nArgs >= m) $
                                  do -- traceDoc $ \penv -> text "  matchfun.args: " <+> tupled (map (ppType penv . snd) args)
                                     return (take nArgs (map Just args ++ replicate (nArgs - m) Nothing),
                                               Just (eff,rng), Just (res,rng), skolems,
                                                 if isRho res then Instantiated else Generalized False)

monotonic :: [Int] -> Bool
monotonic []  = True
monotonic [i] = True
monotonic (i:j:xs) = (i < j && monotonic (j:xs))

before range
  = makeRange (rangeStart range) (rangeStart range)


find :: Range -> M.Map Range a -> a
find range rm
  = case M.lookup range rm of
      Just x -> x
      Nothing -> failure ("Type.Infer.find: could not find: " ++ show range)

coreVector:: Type -> [Core.Expr] -> Inf Core.Expr
coreVector tp cs
  = do (vecName,vecTp,vecInfo) <- resolveFunName nameVector (CtxFunArgs 0 [newName "xs"] Nothing) rangeNull rangeNull -- todo: lookup vector less fragile?
       xs <- coreList tp cs
       return (Core.App (Core.TypeApp (coreExprFromNameInfo vecName vecInfo) [tp]) [xs])


coreList :: Type -> [Core.Expr] -> Inf Core.Expr
coreList tp cs
  = do (consName,consTp,consRepr,_) <- resolveConName nameCons Nothing rangeNull
       (nilName,nilTp,nilRepr,_) <- resolveConName nameListNil Nothing rangeNull
       let consx = Core.TypeApp (Core.Con (Core.TName consName consTp) consRepr) [tp]
           cons x xs = Core.App consx [x,xs]
           nil  = Core.TypeApp (Core.Con (Core.TName nilName nilTp) nilRepr) [tp]
       return (foldr cons nil cs)

unzip4 xs = unzipx4 [] [] [] [] xs
unzipx4 acc1 acc2 acc3 acc4 []           = (reverse acc1, reverse acc2, reverse acc3, reverse acc4)
unzipx4 acc1 acc2 acc3 acc4 ((x,y,z,zz):xs) = unzipx4 (x:acc1) (y:acc2) (z:acc3) (zz:acc4) xs

ppProp env Nothing = text "(nothing)"
ppProp env (Just (tp,_))  = ppType env tp


usesLocals :: S.NameSet -> Expr Type -> Bool
usesLocals lvars expr
  = case expr of
      App (Var newLocal False rng) [_,(_, Parens (Lam [ValueBinder name _ _ _ _] body _) _ _ _)] _  -- fragile: expects this form from the parser
         | newLocal == nameLocalVar
         -> usesLocals (S.delete name lvars) body

      Lam    binds expr rng  -> usesLocals lvars expr
      Let    defs expr range -> usesLocalsDefs lvars defs || usesLocals lvars expr
      Bind   def expr range  -> usesLocalsDef lvars def || usesLocals lvars expr
      App    fun nargs range -> any (usesLocals lvars) (fun : map snd nargs)
      Ann    expr tp range   -> usesLocals lvars expr
      Case   expr brs range  -> usesLocals lvars expr || any (usesLocalsBranch lvars) brs
      Parens expr name pre range -> usesLocals lvars expr
      Handler shallow scoped override allowMask eff pars reinit ret final ops hrng rng
                             -> or (usesLocalsMb lvars reinit : usesLocalsMb lvars ret : usesLocalsMb lvars final : map (usesLocalsOp lvars) ops)
      Inject tp expr b range -> usesLocals lvars expr
      Var name _ _           -> S.member name lvars
      _                      -> False

usesLocalsDefs :: S.NameSet -> DefGroup Type -> Bool
usesLocalsDefs lvars (DefRec defs) = any (usesLocalsDef lvars) defs
usesLocalsDefs lvars (DefNonRec d) = usesLocalsDef lvars d

usesLocalsDef :: S.NameSet -> Def Type -> Bool
usesLocalsDef lvars d = usesLocals lvars (binderExpr (defBinder d))

usesLocalsBranch :: S.NameSet -> Branch Type -> Bool
usesLocalsBranch lvars b = any (usesLocalsGuard lvars) (branchGuards b)

usesLocalsGuard :: S.NameSet -> Guard Type -> Bool
usesLocalsGuard lvars (Guard e1 e2) = (usesLocals lvars e1) || (usesLocals lvars e2)

usesLocalsMb :: S.NameSet -> Maybe (Expr Type) -> Bool
usesLocalsMb lvars Nothing = False
usesLocalsMb lvars (Just expr) = usesLocals lvars expr

usesLocalsOp :: S.NameSet -> HandlerBranch Type -> Bool
usesLocalsOp lvars b = usesLocals lvars (hbranchExpr b)


shortCircuit :: Core.Expr -> [Core.Expr] -> Maybe Core.Expr
shortCircuit fun [expr1,expr2]
  = isAndOr fun
  where
    exprAnd = Just (Core.makeIfExpr expr1 expr2 Core.exprFalse)
    exprOr  = Just (Core.makeIfExpr expr1 Core.exprTrue expr2)

    isAndOr expr
      = case expr of
          Core.App (Core.TypeApp (Core.Var open _) _) [body]  | Core.getName open == nameEffectOpen
            -> isAndOr body
          Core.Var name _ | Core.getName name == nameAnd
            -> exprAnd
          Core.Var name _ | Core.getName name == nameOr
            -> exprOr
          _ -> Nothing


shortCircuit fun args
  = Nothing
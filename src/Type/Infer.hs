{-# OPTIONS -cpp #-}
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
                      , nameReturn, nameRef, nameByref, nameDeref
                      , nameRefSet, nameAssign, nameTpUnit, nameTuple
                      , nameMakeHandler, nameMakeHandlerRet
                      , namePatternMatchError, nameSystemCore, nameTpHandled, nameTpHandled1
                      , nameToAny, nameFalse, nameTrue
                      , nameTpYld
                      , nameTpHandlerBranch0, nameTpHandlerBranch1,nameCons,nameNull,nameVector
                      , nameInject, nameInjectExn, nameTpPartial
                      , nameMakeNull, nameConstNull, nameReturnNull, nameReturnNull1
                      , nameMakeContextTp
                      , nameTpLocalVar, nameTpLocal, nameRunLocal, nameLocalGet, nameLocalSet, nameLocalNew, nameLocal
                      , nameTpValueOp, nameClause, nameIdentity
                      , nameMaskAt, nameMaskBuiltin, nameEvvIndex, nameHTag, nameTpHTag
                      , nameInt32, nameOr, nameAnd, nameEffectOpen
                      , nameCCtxCreate, nameCCtxHoleCreate
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
import Type.Kind( handledToLabel, getKind, labelIsLinear )
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

traceDoc fdoc = do penv <- getPrettyEnv
                   trace (show (fdoc penv)) $ return ()

{--------------------------------------------------------------------------
  Infer Types
--------------------------------------------------------------------------}
inferTypes :: Env -> Maybe RM.RangeMap -> Synonyms -> Newtypes -> Constructors -> ImportMap -> Gamma -> Name -> DefGroups Type
                -> Core.CorePhase (Gamma, Core.DefGroups, Maybe RM.RangeMap )
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
                                    do coreDef <- fixCanonicalName core0
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
       (coreDefsX,assumed) <- extendGamma False gamma $ extendInfGamma topLevel infgamma $
                                 do assumed <- mapM (\def -> lookupInfName (getName def)) defs
                                    coreDefs0 <- mapM (\def -> inferDef Instantiated def) defs
                                    coreDefs1 <- mapM fixCanonicalName coreDefs0
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
                                              createNameInfoX Public (Core.defName cdef) (Core.defSort cdef) (Core.defNameRange cdef) (Core.defType cdef)
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
                          let nameInfo = createNameInfoX Public qname sort nameRng tp -- (not topLevel || isValue) nameRng tp  -- NOTE: Val is fixed later in "FixLocalInfo"
                          -- trace ("*** createGammas: assume: " ++ show name ++ ": " ++ show nameInfo) $ return ()
                          createGammas ((name,nameInfo):gamma) infgamma defs
                  _ -> case lookup name gamma of
                         Just _
                          -> do env <- getPrettyEnv
                                infError nameRng (text "recursive functions with the same overloaded name must have a full type signature" <+> parens (ppName env name))
                         Nothing
                          -> do qname <- if (topLevel) then qualifyName name else return name
                                info <- case expr of
                                          Ann _ tp _ -> return (createNameInfoX Public qname sort nameRng tp)  -- may be off due to incomplete type: get fixed later in inferRecDef2
                                          _          -> do tp <- Op.freshTVar kindStar Meta
                                                           -- trace ("*** assume defVal: " ++ show qname) $
                                                           return (createNameInfoX Public qname DefVal nameRng tp)  -- must assume Val for now: get fixed later in inferRecDef2
                                -- trace ("*** createGammasx: assume: " ++ show name ++ ": " ++ show info) $ return ()
                                createGammas gamma ((qname,info):infgamma) defs

checkRecVal :: Core.DefGroup -> Inf ()
checkRecVal (Core.DefNonRec def) = return ()
checkRecVal (Core.DefRec defs)
  = mapM_ checkDef defs
  where 
    checkDef def
      = if (not (Core.defIsVal def)) then return () else    
         do infError (Core.defNameRange def) (text ("value definition is recursive.\n  recursive group: " ++ show (map Core.defName defs)))

fixCanonicalName :: Core.Def -> Inf Core.Def
fixCanonicalName def
  = do (_,_,info) <- resolveName (Core.defName def) (Just (Core.defType def, Core.defNameRange def)) (Core.defNameRange def) -- should never fail
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
    in do addRangeInfo (Core.defNameRange coreDef) (RM.Id qname (RM.NIValue (Core.defType coreDef)) True)
          addRangeInfo (defRange def) (RM.Decl (if defIsVal def then "val" else "fun") qname (RM.mangle qname (Core.defType coreDef)))


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
                    -- inferSubsume (checkEffectSubsume rng) rng (Core.defType def) resTp
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
            info = coreVarInfoFromNameInfo (createNameInfoX Public name csort (defRange def) resTp1)
        penv <- getPrettyEnv
        (resTp2,coreExpr)
              <- case (mbAssumed,resCore1) of
                         (Just (_,(TVar _)), Core.TypeLam tvars expr)  -- we assumed a monomorphic type, but generalized eventually
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
          extendInfGamma topLevel infgamma $
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
     if (verbose penv >= 3)
      then Lib.Trace.trace ("infer: " ++ show sort ++ " " ++ show name) $ return ()
      else return ()
     withDefName name $ disallowHole $
      (if (not (isDefFun sort) || nameIsNil name) then id else allowReturn True) $
        do (tp,eff,coreExpr) <- inferExpr Nothing expect expr
                                -- Just annTp -> inferExpr (Just (annTp,rng)) (if (isRho annTp) then Instantiated else Generalized) (Ann expr annTp rng)

           -- traceDoc $ \env -> text " infer def before gen:" <+> pretty name <+> colon <+> ppType env tp
           (resTp,resCore) <- maybeGeneralize rng nameRng eff expect tp coreExpr -- may not have been generalized due to annotation
           -- traceDoc $ \env -> text " infer def:" <+> pretty name <+> colon <+> ppType env resTp
           inferUnify (checkValue rng) nameRng typeTotal eff
           if (verbose penv >= 3)
            then Lib.Trace.trace (show (text " inferred" <+> pretty name <.> text ":" <+> niceType penv tp)) $ return ()
            else return ()
           subst (Core.Def name resTp resCore vis sort inl nameRng doc)  -- must 'subst' since the total unification can cause substitution. (see test/type/hr1a)

inferBindDef :: Def Type -> Inf (Effect,Core.Def)
inferBindDef (Def (ValueBinder name () expr nameRng vrng) rng vis sort inl doc)
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
            then addRangeInfo nameRng (RM.Id name (RM.NIValue (Core.defType coreDef)) True)
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
           Parens x _ _ -> hasVarDecl x
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
inferExpr propagated expect (Lam binders body rng)
  = isNamedLam $ \isNamed ->
    disallowHole $
    do -- traceDoc $ \env -> text " inferExpr.Lam:" <+> pretty (show expect) <+> text ", propagated:" <+> ppProp env propagated
       (propArgs,propEff,propBody,skolems,expectBody) <- matchFun (length binders) propagated

       let binders0 = [case binderType binder of
                         Nothing -> binder{ binderType = fmap snd mbProp }
                         Just _  -> binder
                      | (binder,mbProp) <- zip binders propArgs]
       binders1 <- mapM instantiateBinder binders0
       eff <- case propEff of
                Nothing  -> freshEffect  -- TODO: use propEff?
                Just (eff,_) -> return eff
       (infgamma,sub,defs) <- inferOptionals eff [] binders1
       let coref c = Core.makeLet (map Core.DefNonRec defs) ((CoreVar.|~>) sub c)

       returnTp <- case propBody of
                     Nothing     -> Op.freshTVar kindStar Meta
                     Just (tp,_) -> return tp

       (tp,eff1,core) <- extendInfGamma False infgamma  $
                           extendInfGamma False [(nameReturn,createNameInfoX Public nameReturn DefVal (getRange body) returnTp)] $
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
       let optPars   = zip (map binderName binders1) parTypes2
           bodyCore1 = Core.addLambdas optPars topEff (Core.Lam [] topEff (coref core))
       bodyCore2 <- subst bodyCore1
       stopEff <- subst topEff
       let pars = optPars

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

       mapM_ (\(binder,tp) -> addRangeInfo (binderNameRange binder) (RM.Id (binderName binder) (RM.NIValue tp) True)) (zip binders1 parTypes2)
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
                 mbTp <- lookupInfName (unqualify nameReturn)
                 case mbTp of
                   Nothing
                    -> do infError rng (text "illegal context for a return statement")
                   Just (_,retTp)
                    -> do inferUnify (checkReturn rng) (getRange expr) retTp tp
                 resTp <- Op.freshTVar kindStar Meta
                 let typeReturn = typeFun [(nameNil,tp)] typeTotal resTp
                 addRangeInfo nameRng (RM.Id (newName "return") (RM.NIValue tp) False)
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
                inferExpr propagated expect (App fun (xargs ++ [(Nothing {- Just (nameAssigned,rangeNull) -},rexpr)]) rng)
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
              {-
              (_,_,info) <- resolveName target Nothing lrng
              case info of
                InfoVal{ infoIsVar = True }
                  -> inferExpr propagated expect
                        (App (Var nameRefSet False lrng) [(Nothing,App (Var nameByref False lrng) [lhs] lrng), rhs] rng)
                _ -> errorAssignable
              -}
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
inferExpr propagated expect (App fun@(Var hname _ _) [] rng)  | hname == nameCCtxHoleCreate
  = do ok <- useHole
       when (not ok) $
         contextError rng rng (text "ill-formed constructor context") 
            [(text "because",text "there can be only one hole, and it must occur under a constructor context 'ctx'")]           
       inferApp propagated expect fun [] rng

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
       return (Core.typeOf ccore,eff,ccore)

-- | Application nodes. Inference is complicated here since we need to disambiguate overloaded identifiers.
inferExpr propagated expect (App fun nargs rng)
  = inferApp propagated expect fun nargs rng

inferExpr propagated expect (Ann expr annTp rng)
  = do -- traceDoc $ \env -> text "infer annotation:" <+> ppType env annTp
       (tp,eff,core) <- inferExpr (Just (annTp,rng)) (if isRho annTp then Instantiated else Generalized False) expr
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
       bress <- disallowHole $
                case (propagated,branches) of
                  (Nothing,(b:bs)) -> -- propagate the type of the first branch
                    do bres@(tpeffs,_) <- inferBranch propagated ctp (getRange expr) b
                       let tp = case tpeffs of
                                  (tp,_):_ -> tp
                                  _        -> failure $ "Type.Infer.inferExpr.Case: branch without guard"
                       bress <- mapM (inferBranch (Just (tp,getRange b)) ctp (getRange expr)) bs
                       return (bres:bress)
                  _ -> mapM (inferBranch propagated ctp (getRange expr)) branches
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
       let (isTotal,warnings,cbranches) = analyzeBranches newtypes defName rng sbcores [stp] [dataInfo]
       mapM_ (\(rng,warning) -> infWarning rng warning) warnings
       topEff <- if isTotal
                  then return resEff
                  else -- addTopMorphisms rng [(rng,typePartial),(rng,resEff)]
                       -- return (orderEffect (effectExtend typePartial resEff))
                       -- do subsumeEffect (checkEffectSubsume rng) rng typePartial resEff
                       --   return resEff
                       do sresEff <- subst resEff
                          return (effectExtendNoDup typePartial sresEff)
       -- return core
       core  <- subst (Core.Case [ccore] cbranches)
       stopEff <- subst topEff
       (gresTp,gcore) <- maybeInstantiateOrGeneralize rng (getRange branches) stopEff expect resTp core
       return (gresTp,stopEff,gcore)
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


inferExpr propagated expect (Var name isOp rng)
  = inferVar propagated expect name rng True

inferExpr propagated expect (Lit lit)
  = do let (tp,core) =
              case lit of
                LitInt i _  -> (typeInt,Core.Lit (Core.LitInt i))
                LitChar c _  -> (typeChar,Core.Lit (Core.LitChar c))
                LitFloat f _  -> (typeFloat,Core.Lit (Core.LitFloat f))
                LitString s _  -> (typeString,Core.Lit (Core.LitString s))
       eff <- freshEffect
       return (tp,eff,core)


inferExpr propagated expect (Parens expr name rng)
  = do (tp,eff,core) <- inferExpr propagated expect expr
       if (name /= nameNil) 
         then do addRangeInfo rng (RM.Id name (RM.NIValue tp) True)
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

       (mbHandled,effName) <- effectNameCore label rng
       effTo <- subst $ effectExtend label eff

       sexprTp <- subst exprTp
       -- traceDoc $ \env -> text "inject: effTo:" <+> ppType env effTo <+> text "," <+> ppType env exprEff <+> text ", exprTp: " <+> ppType env sexprTp
       let coreLevel  = if behind then Core.exprTrue else Core.exprFalse -- Core.Lit (Core.LitInt (if behind then 1 else 0))
       core <- case mbHandled of
                 -- general handled effects use ".inject-effect"
                 Just coreHTag
                   -> do (maskQName,maskTp,maskInfo) <- resolveFunName nameMaskAt (CtxFunArgs 3 []) rng rng
                         (evvIndexQName,evvIndexTp,evvIndexInfo) <- resolveFunName nameEvvIndex (CtxFunArgs 1 []) rng rng
                         let coreMask = coreExprFromNameInfo maskQName maskInfo
                             coreIndex= Core.App (Core.TypeApp (coreExprFromNameInfo evvIndexQName evvIndexInfo) [effTo])
                                                 [coreHTag]
                             core     = Core.App (Core.TypeApp coreMask [resTp,eff,effTo]) [coreIndex,coreLevel,exprCore]
                         return core
                 Nothing
                   -> do (maskQName,maskTp,maskInfo) <- resolveFunName nameMaskBuiltin (CtxFunArgs 3 []) rng rng
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
           handlerConName = toConstructorName (toHandlerName effectName)

       -- traceDoc $ \penv -> text "checking handler: " <+> ppType penv heff

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
            = do let (clauseName, cparams) = case opSort of
                          OpVal        -> (nameClause "tail" (length pars), pars)
                          OpFun        -> (nameClause "tail" (length pars), pars)
                          OpExcept     -> (nameClause "never" (length pars), pars)
                          -- don't optimize ctl to exc since exc runs the finalizers before the clause (unlike ctl)
                          -- OpControl    | not (hasFreeVar body (newName "resume")) 
                          --             -> (nameClause "never" (length pars), pars)  -- except
                          OpControl    -> let resumeTp = TFun [(nameNil,resumeArg)] eff res
                                          in (nameClause "control" (length pars), pars ++ [ValueBinder (newName "resume") (Just resumeTp) () hrng patRng])
                          OpControlRaw -> let eff0 = effectExtend heff eff
                                              resumeContextTp = typeResumeContext resumeArg eff eff0 res
                                          in (nameClause "control-raw" (length pars), pars ++ [ValueBinder (newName "rcontext") (Just resumeContextTp) () hrng patRng])
                          -- _            -> failure $ "Type.Infer.inferHandler: unexpected resume kind: " ++ show rkind
                 -- traceDoc $ \penv -> text "resolving:" <+> text (showPlain opName) <+> text ", under effect:" <+> text (showPlain effectName)
                 (_,gtp,_) <- resolveFunName (if isQualified opName then opName else qualify (qualifier effectName) opName)
                                               (CtxFunArgs (length pars) []) patRng nameRng -- todo: resolve more specific with known types?
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
                     -- use Parens for better rangeMap info
                     cname = case opSort of
                               OpVal -> fromValueOperationsName opName
                               _     -> opName                               
                     capp  = App (Var clauseName False hrng) [(Nothing,Parens (Lam cparamsx body nameRng) cname nameRng)] frng
                 return (Nothing, capp) 

       clauses <- mapM clause (zip (sortBy opName branches) resumeArgs)

       let grng = rangeNull
       let handlerCon = let hcon = Var handlerConName False hrng
                        in if null clauses then hcon else App hcon clauses rng
           handlerCfc = (\i -> App (Var nameInt32 False grng) [(Nothing,Lit (LitInt i grng))] grng) $
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
                                [(Nothing,handlerCfc),(Nothing,handlerCon),(Nothing,handleRet),(Nothing,action)] hrng
           
           

       -- extract the action type for the case where it is higher-ranked (for scoped effects)
       -- this way we can annotate the action parameter with a higher-rank type if needed
       -- so it is propagated automatically.
       penv <- getPrettyEnv
       (_,handleTp,_)  <- resolveFunName handleName CtxNone rng rng
       (handleRho,_,_) <- instantiateEx rng handleTp
       let actionTp = case splitFunType handleRho of
                        Just ([_,_,_,actionTp],effTp,resTp) -> snd actionTp
                        _ -> failure ("Type.Infer: unexpected handler type: " ++ show (ppType penv handleRho))
       -- traceDoc $ \penv -> text " action type is" <+> ppType penv actionTp
       let handlerExpr = Parens (Lam [ValueBinder actionName (Just actionTp) Nothing rng rng]
                                     (handleExpr (Var actionName False rng)) hrng) (newName "handler") rng

       -- and check the handle expression
       hres@(xhtp,_,_) <- inferExpr propagated expect handlerExpr
       htp <- subst xhtp

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
             do (qname,tp,info) <- resolveFunName name (CtxFunArgs (length pars) []) rng nameRng
                (rho,_,_) <- instantiateEx nameRng tp
                case splitFunType rho of
                  Just((opname,rtp):_,_,_) | isHandlerInstance handlerSort && opname == newHiddenName "hname"
                                -> do -- traceDoc $ \env -> text "effect instance: " <+> ppType env rtp
                                      return rtp
                  Just(_,eff,_) | not (isHandlerInstance handlerSort)
                                -> case extractEffectExtend eff of
                                    ((l:_),_) -> return (l)  -- TODO: can we assume the effect comes first?
                                    _ -> failure $ "Type.Infer.inferHandledEffect: invalid effect type: " ++ show eff
                  _ -> infError rng (text ("cannot resolve effect operation: " ++ show qname ++ ".") <--> text " hint: maybe wrong number of parameters?")
        _ -> infError rng (text "unable to determine the handled effect." <--> text " hint: use a `handler<eff>` declaration?")


-- Check coverage is not needed for type inference but gives nicer error messages
checkCoverage :: Range -> Effect -> Name -> [HandlerBranch Type] -> Inf ()
checkCoverage rng effect handlerConName branches
  = do (_,gconTp,conRepr,conInfo) <- resolveConName handlerConName Nothing rng
       let opNames = map (fieldToOpName . fst)  (conInfoParams conInfo)
           branchNames = map branchToOpName branches
       checkCoverageOf rng (map fst opNames) opNames branchNames
       return ()
  where
    modName = qualifier handlerConName

    fieldToOpName fname
      = let (pre,post)      = span (/='-') (nameId fname)
            (opSort,opName) = case (readOperationSort pre,post) of
                                (Just opSort, _:opName) -> (opSort,opName)
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
        | (typeConName tc == nameTpHandled || typeConName tc == nameTpHandled1)
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
        | (typeConName tc == nameTpHandled || typeConName tc == nameTpHandled1) -> effectNameFromLabel hx
      TCon tc -> typeConName tc
      TSyn syn _ _ -> typeSynName syn
      TApp (TCon tc) targs -> typeConName tc
      _ -> failure ("Type.Infer.effectNameFromLabel: invalid effect: " ++ show effect)


inferApp :: Maybe (Type,Range) -> Expect -> Expr Type -> [(Maybe (Name,Range),Expr Type)] -> Range -> Inf (Type,Effect,Core.Expr)
inferApp propagated expect fun nargs rng
  = -- trace "infer: App" $
    do (fixed,named) <- splitNamedArgs nargs
       amb <- case rootExpr fun of
                (Var name isOp nameRange)
                  -> do matches <- lookupNameN name (length fixed) (map (fst . fst) named) nameRange
                        -- traceDoc $ \env -> text "matched for: " <+> ppName env name <+> text " = " <+> pretty (length matches)
                        case matches of
                          []         -> do -- emit an error
                                           resolveFunName name (CtxFunArgs (length fixed) (map (fst . fst) named)) rng nameRange
                                           return (Just Nothing)  -- error
                          [(_,info)] -> return (Just (Just (infoType info, rng))) -- known type
                          _          -> return Nothing -- many matches
                _ -> return (Just Nothing) -- fun first
       case amb of
         Nothing   -> inferAppFromArgsX fixed named
         Just prop -> inferAppFunFirst prop fixed named
  where
    -- (names,args) = unzip nargs
    inferAppFunFirst :: Maybe (Type,Range) -> [Expr Type] -> [((Name,Range),Expr Type)] -> Inf (Type,Effect,Core.Expr)
    inferAppFunFirst prop fixed named
      = -- trace (" inferAppFunFirst") $
        do -- infer type of function
           (ftp,eff1,fcore)     <- allowReturn False $ inferExpr prop Instantiated fun
           -- match the type with a function type
           (iargs,pars,funEff,funTp,coreApp)  <- matchFunTypeArgs rng fun ftp fixed named

           -- todo: match propagated type with result type?
           -- subsume arguments
           (effArgs,coreArgs) <- -- withGammaType rng (TFun pars funEff funTp) $ -- ensure the free 'some' types are free in gamma
                                 do let check = case (fun) of
                                                   Var name _ _ | name == nameRunLocal
                                                     -> checkLocalScope rng
                                                   _ -> Infer rng
                                    inferSubsumeN check rng (zip (map snd pars) (map snd iargs))
           -- traceDoc $ \env -> text "inferAppFunFirst:" <+> prettyExpr env fcore
           core <- case shortCircuit fcore coreArgs of
                    Just cexpr -> return cexpr
                    Nothing -> 
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
                                else do fname <- uniqueName "fun"
                                        let fdef = Core.DefNonRec (Core.Def fname ftp fcore Core.Private (defFun [] {-all own, TODO: maintain borrow annotations?-}) InlineAuto rangeNull "")
                                            fvar = Core.Var (Core.TName fname ftp) Core.InfoNone
                                        return (Core.Let (fdef:defs) (coreApp fvar cargs))
           -- take top effect
           -- todo: sub effecting should add core terms
           -- topEff <- addTopMorphisms rng ((getRange fun, eff1):(rng,funEff):zip (map (getRange . snd) iargs) effArgs)
           topEff <- inferUnifies (checkEffect rng) ((getRange fun, eff1) : zip (map (getRange . snd) iargs) effArgs)
           inferUnify (checkEffectSubsume rng) (getRange fun) funEff topEff
           -- traceDoc $ \env -> (text " ** effects: " <+> tupled (map (ppType env) ([topEff, funEff, eff1] ++ effArgs)))

           -- instantiate or generalize result type
           funTp1         <- subst funTp
           -- traceDoc $ \env -> text " inferAppFunFirst: inst or gen:" <+> pretty (show expect) <+> colon <+> ppType env funTp1 <.> text ", top eff: " <+> ppType env topEff
           (resTp,resCore) <- maybeInstantiateOrGeneralize rng (getRange fun) topEff expect funTp1 core
           --stopEff <- subst topEff
           -- traceDoc $ \env -> text " inferAppFunFirst: resTp:" <+> ppType env resTp <.> text ", top eff: " <+> ppType env stopEff
           return (resTp,topEff,resCore )

    inferAppFromArgs :: [Expr Type] -> [((Name,Range),Expr Type)] -> Inf (Type,Effect,Core.Expr)
    inferAppFromArgs fixed named
      = trace ("inferApp From Args") $
        do mbargs <- mapM (\fix -> tryRun $ inferExpr Nothing Instantiated fix) fixed
           let iargs = catMaybes mbargs
           if (length iargs==length mbargs && null named) -- TODO: we can extend inferAppFixedArgs to deal with named arguments?
            then inferAppFixedArgs (zipWith (\(tpArg,eff,cexpr) fix -> (tpArg,(getRange fix,eff),cexpr)) iargs fixed)
            else do argtps <- mapM (\mbarg -> case mbarg of
                                                Nothing -> Op.freshTVar kindStar Meta
                                                Just(tpArg,_,_) -> subst tpArg)  mbargs
                    let ctx = CtxFunTypes False argtps [] -- TODO: can we add the named arguments here?
                    prop <- case rootExpr fun of
                            (Var name _ nameRange) | isConstructorName name
                              -> do matches <- lookupNameEx (isInfoCon {- const True -}) name ctx nameRange
                                    traceDoc $ \env -> text " app args matched for constructor " <+> ppName env name <+> text " = " <+> pretty (length matches)
                                    case matches of
                                      [(_,info)] -> return (Just (infoType info, rng))
                                      _          -> do -- emit an error
                                                       resolveConName name Nothing nameRange
                                                       return Nothing
                                     -- _          -> return Nothing
                            (Var name _ nameRange)
                              -> do matches <- lookupNameEx (isInfoValFunExt {- const True -}) name ctx nameRange
                                    traceDoc $ \env -> text " app args matched for " <+> ppName env name <+> text " = " <+> pretty (length matches) <+> text ", " <+> pretty (length fixed) <+> text ", args: " <+> list (niceTypes env argtps )
                                    case matches of
                                      [(_,info)] -> return (Just (infoType info, rng))
                                      _          -> do -- emit an error
                                                       resolveFunName name ctx rng nameRange
                                                       return Nothing
                                      -- _          -> return Nothing
                            _ -> return Nothing
                    -- and reinfer!  TODO: very bad because this can cause exponential backtracking...
                    -- traceDoc $ \env -> "REINFER!!"
                    inferAppFunFirst prop fixed named

    -- we cannot determine what function is called, infer types of arguments without propagation
    -- first we order the arguments to infer arguments with simple expressions first
    inferAppFromArgsX :: [Expr Type] -> [((Name,Range),Expr Type)] -> Inf (Type,Effect,Core.Expr)
    inferAppFromArgsX fixed named
      = do guesses <- mapM (\fix -> do tv <- Op.freshTVar kindStar Meta
                                       return (tv,(getRange fix,typeTotal),failure "Infer.InferApp.inferAppFromArgs")) fixed
           inferAppArgsFirst guesses ({-sortBy (comparing (weight . snd))-} (zip [0..] fixed)) fixed named
      where
        weight expr
          = case expr of
              Lit _         -> 0
              Ann _ _ _     -> 0
              --Var _ _ _     -> 1
              --Parens e _    -> weight e
              --App e args _  -> 1 + weight e + sum (map (weight . snd) args)
              _             -> 10
    reorder :: [(Int,a)] -> [a]
    reorder xs = map snd (sortBy (comparing fst) xs)

    inferAppArgsFirst :: [(Type,(Range,Effect),Core.Expr)] -> [(Int,Expr Type)] -> [Expr Type] -> [((Name,Range),Expr Type)] -> Inf (Type,Effect,Core.Expr)
    inferAppArgsFirst [] [] [] named       -- no fixed arguments, try FunFirst
      = inferAppFunFirst Nothing [] named
    inferAppArgsFirst acc [] fixed []      -- we tried all fixed arguments
      = inferAppFixedArgs acc
    inferAppArgsFirst acc [] fixed named
      = infError rng (text "named arguments can only be used if the function is unambiguously determined by the context" <-> text " hint: annotate the function parameters?" )

    inferAppArgsFirst acc ((idx,fix):fixs) fixed named  -- try to improve our guess
      = do -- traceDoc $ \env -> "infer app args first :-(: "
           (tpArg,effArg,coreArg)  <- allowReturn False $ inferExpr Nothing Instantiated fix
           let acc' = take idx acc ++ [(tpArg,(getRange fix,effArg),coreArg)] ++ drop (idx+1) acc
           amb <- case rootExpr fun of
                    (Var name _ nameRange) | isConstructorName name
                      -> do matches <- lookupNameEx (isInfoCon {- const True -}) name (CtxFunTypes True (map fst3 acc') []) nameRange
                            -- traceDoc $ \env -> text "app args matched for constructor " <+> ppName env name <+> text " = " <+> pretty (length matches) <+> text ", " <+> pretty (length fixs) <+> text ", args: " <+> list (map (ppType env) (map fst3 acc') )
                            case matches of
                              []         -> do -- emit an error
                                               resolveConName name Nothing nameRange
                                               return Nothing
                              [(_,info)] -> return (Just (infoType info, rng))
                              _          -> return Nothing
                    (Var name _ nameRange)
                      -> do matches <- lookupNameEx (isInfoValFunExt {- const True -}) name (CtxFunTypes True (map fst3 acc') []) nameRange
                            -- traceDoc $ \env -> text "app args matched for " <+> ppName env name <+> text " = " <+> pretty (length matches) <+> text ", " <+> pretty (length fixs) <+> text ", args: " <+> list (map (ppType env) (map fst3 acc)  )
                            case matches of
                              []         -> do -- emit an error
                                               resolveFunName name (CtxFunTypes True (map fst3 acc') []) rng nameRange
                                               return Nothing
                              [(_,info)] -> return (Just (infoType info, rng))
                              _          -> return Nothing
                    _ -> return Nothing

           case amb of
             Just prop  -> -- TODO: we re-infer the fixed arguments again. it is hard to optimize this due to optional and delayed arguments that need wrapping...
                           -- what we could do is check if the check arguments up to this point (in acc) are not optional or delayed, and in that case
                           -- we can avoid redoing the inference for those.
                           -- TODO: this can lead to exponential behavior... really bad
                           -- trace(" reinfer") $
                            inferAppFunFirst (Just prop) fixed named
             Nothing    -> {-
                           if (not (null named0))
                            then infError rng (text "named arguments can only be used if the function is unambiguously determined by the context" <-> text " hint: annotate the function parameters?" )
                            else do (tpArgs,effArgs,coreArgs)  <- fmap unzip3 $ mapM (inferExpr Nothing Instantiated) fixed
                                    inferAppFixedArgs (tpArg1:tpArgs) (zip (map getRange fixed) (effArg1:effArgs)) (coreArg1:coreArgs)
                           -}
                           inferAppArgsFirst acc' fixs fixed named
    {-
    -- lets try again on all arguments
    inferAppArgs fixed named
      = do (tpArgs,effArgs,coreArgs)    <- fmap unzip3 $ mapM (inferExpr Nothing Instantiated) fixed
           (tpNArgs,effNArgs,coreNArgs) <- fmap unzip3 $ mapM (inferExpr Nothing Instantiated) (map snd named)
           let tpNamedArgs = zip (map fst named) tpNArgs

           -- inferAppFixedArgs tpArgs (zip (map getRange fixed) effArgs) (coreArgs)

           amb <- case rootExpr fun of
                    (Var name _ nameRange)
                      -> do matches <- lookupNameEx (const True) name (CtxFunTypes False tpArgs [(name,tp) | ((name,_),tp) <- tpNamedArgs]) nameRange
                            case matches of
                              []         -> return Nothing
                              [(_,info)] -> return (Just (infoType info, rng))
                              _          -> return Nothing
                    _ -> return Nothing
           case amb of
             Just prop  -> -- todo: for now, redo all the inference of the arguments to share code, but this could be optimized
                           inferAppFunFirst (Just prop) [] fixed named
             Nothing    -> if (null named)
                            then inferAppFixedArgs tpArgs (zip (map getRange fixed) effArgs) (coreArgs)
                            else infError rng (text "named arguments can only be used if the function is unambiguously determined by the context" <-> text " hint: annotate the function parameters?" )
     -}

    inferAppFixedArgs :: [(Type,(Range,Effect),Core.Expr)] -> Inf (Type,Effect,Core.Expr)
    inferAppFixedArgs acc
      = -- trace ("inferAppFixedArgs") $
        do -- (tpArgs,effArgs,coreArgs) <- fmap unzip3 $ mapM (inferExpr Nothing Instantiated) args  -- todo: what about higher-ranked types?
           let (tpArgs,effArgs,coreArgs) = unzip3 acc
           stpArgs <- mapM subst tpArgs

           funEff <- freshEffect
           expTp  <- case propagated of
                         Just (tp,_) -> return tp
                         _           -> Op.freshTVar kindStar Meta
           let propType = TFun [(newName "",targ) | targ <- stpArgs] funEff expTp
           (ftp,eff1,fcore) <- allowReturn False $ inferExpr (Just (propType,rng)) Instantiated fun
           -- check the inferred type matches the arguments
           inferUnify (checkFun rng) rng propType ftp
           -- add morphisms
           -- topEff <- addTopMorphisms rng ((getRange fun, eff1):(rng,funEff):effArgs)
           topEff <- inferUnifies (checkEffect rng) ((getRange fun, eff1) : effArgs)
           inferUnify (checkEffectSubsume rng) (getRange fun) funEff topEff

           let appexpr = case shortCircuit fcore coreArgs of
                          Just cexpr -> cexpr
                          Nothing    -> Core.App fcore coreArgs

           -- instantiate or generalize result type
           resTp1          <- subst expTp
           (resTp,resCore) <- maybeInstantiateOrGeneralize rng (getRange fun) topEff expect resTp1 appexpr
           return (resTp,topEff,resCore )

    fst3 (x,y,z) = x

inferVar :: Maybe (Type,Range) -> Expect -> Name -> Range -> Bool -> Inf (Type,Effect,Core.Expr)
inferVar propagated expect name rng isRhs  | isConstructorName name
  = -- trace("inferVar: constructor: " ++ show name)$
    do (qname1,tp1,conRepr,conInfo) <- resolveConName name (fmap fst propagated) rng
       let info1 = InfoCon Public tp1 conRepr conInfo rng
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
       addRangeInfo rng (RM.Id (infoCanonicalName qname1 info1) (RM.NICon tp) False)
       (itp,coref) <- maybeInstantiate rng expect tp
       -- traceDoc $ \env -> text "Type.Infer.Con: " <+> ppName env qname <+> text ":" <+> ppType env itp
       eff <- freshEffect
       return (itp,eff,coref coreVar)

inferVar propagated expect name rng isRhs
  = -- trace("inferVar; " ++ show name) $
    do (qname,tp,info) <- resolveName name propagated rng
       -- traceDoc $ \env -> text "inferVar:" <+> pretty name <+> colon <+> ppType env{showIds=True} tp
       if (isTypeLocalVar tp && isRhs)
        then do -- traceDoc $ \penv -> text "localvar:" <+> pretty name <+> text ":" <+> ppType penv tp
                let irng = extendRange rng (-1)
                (tp1,eff1,core1) <- inferExpr propagated expect (Parens (App (Var nameLocalGet False irng) 
                                                                             [(Nothing,App (Var nameByref False irng) 
                                                                                           [(Nothing,Var name False irng)] irng)] irng)
                                                                        name rng)
                addRangeInfo rng (RM.Id qname (RM.NIValue tp1) False)
                -- traceDoc $ \env -> text " deref" <+> pretty name <+> text "to" <+> ppType env tp1
                return (tp1,eff1,core1)
        else case info of
         InfoVal{ infoIsVar = True }  | isRhs  -- is it a right-hand side variable?
           -> do (tp1,eff1,core1) <- inferExpr propagated expect (App (Var nameDeref False rng) [(Nothing,App (Var nameByref False rng) [(Nothing,Var name False rng)] rng)] rng)
                 addRangeInfo rng (RM.Id qname (RM.NIValue tp1) False)
                 return (tp1,eff1,core1)
         InfoVal{} | isValueOperation tp
           -> do addRangeInfo rng (RM.Id qname (RM.NIValue tp) True)
                 inferExpr propagated expect (App (Var (toValueOperationName qname) False rangeNull) [] rangeNull)                 
         _ -> --  inferVarX propagated expect name rng qname1 tp1 info1
              do let coreVar = coreExprFromNameInfo qname info
                 -- traceDoc $ \env -> text "inferVar:" <+> pretty name <+> text ":" <+> text (show info) <.> text ":" <+> ppType env tp
                 addRangeInfo rng (RM.Id (infoCanonicalName qname info) (RM.NIValue tp) False)
                 (itp,coref) <- maybeInstantiate rng expect tp
                 sitp <- subst itp
                 -- traceDoc $ \env -> (text " Type.Infer.Var: " <+> pretty name <.> colon <+> ppType env{showIds=True} sitp)
                 eff <- freshEffect
                 return (itp,eff,coref coreVar)

{-
inferVar propagated expect name rng isRhs
  = do (qname1,tp1,info1) <- resolveName name (propagated) rng
       inferVarX propagated expect name rng qname1 tp1 info1

inferVarX propagated expect name rng qname1 tp1 info1
  = do (qname,tp,info,rngConValue)
                      <- case info1 of
                           InfoCon{ infoCon = conInfo } -- conInfoCreator conInfo -- does it have a special creator function?
                            -> do defName <- currentDefName
                                  let creatorName = newCreatorName qname1
                                  -- trace ("inferCon: " ++ show (defName,creatorName,qname1)) $ return ()
                                  if (defName /= unqualify creatorName && defName /= nameCopy) -- a bit hacky, but ensure we don't call the creator function inside itself or the copy function
                                   then do mbRes <- lookupFunName creatorName propagated rng
                                           case mbRes of
                                              Just (qname',tp',info') -> return (qname',tp',info',RM.NICon)
                                              Nothing  -> return (qname1,tp1,info1,RM.NICon)
                                   else return (qname1,tp1,info1,RM.NICon)
                           _ -> return (qname1,tp1,info1,RM.NIValue)
       let coreVar = coreExprFromNameInfo qname info
       addRangeInfo rng (RM.Id (infoCanonicalName qname1 info1) (rngConValue tp) False)
       (itp,coref) <- maybeInstantiate rng expect tp
       -- trace ("Type.Infer.Var: " ++ show (name,itp)) $ return ()
       eff <- freshEffect
       return (itp,eff,coref coreVar)
-}

inferBranch :: Maybe (Type,Range) -> Type -> Range -> Branch Type -> Inf ([(Type,Effect)],Core.Branch)
inferBranch propagated matchType matchRange branch@(Branch pattern guards)
  = inferPattern matchType (getRange branch) pattern (
    \pcore gcores ->
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
     extendInfGamma False infGamma $
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
       addRangeInfo nameRange (RM.Id qname (RM.NICon gconTp) False)
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
  = do {-
       mb <- lookupConName (binderName binder) (binderType binder) (binderNameRange binder)
       case mb of
         Just (qname,conTp,info)
           -- it is actually a constructor
           -> do checkCasing (binderNameRange binder) (binderName binder) qname info
                 inferPattern matchType branchRange (PatCon qname [] (binderNameRange binder) (binderNameRange binder))
         Nothing
           -- it is a variable indeed
           -> -}
              do addRangeInfo (binderNameRange binder) (RM.Id (binderName binder) (RM.NIValue matchType) True)
                 case (binderType binder) of
                   Just tp -> inferUnify (checkAnn (getRange binder)) (binderNameRange binder) matchType tp
                   Nothing -> return ()
                 (cpat,infGamma0) <- inferPatternX matchType branchRange (binderExpr binder)
                 let infGamma = ([(binderName binder,(createNameInfoX Public (binderName binder) DefVal (binderNameRange binder) matchType))] ++ infGamma0)
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
        let info = (binderName par,createNameInfoX Public (binderName par) DefVal (getRange par) (binderType par))
        in inferBinders (infgamma ++ [info]) pars


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
      -> inferOptionals eff (infgamma ++ [(binderName par,createNameInfoX Public (binderName par) DefVal (getRange par) (binderType par))]) pars

     Just expr  -- default value
      -> do let fullRange = combineRanged par expr
                -- partp = binderType par
                -- optTp = makeOptional partp
                optTp = binderType par

            -- infer parameter type from optional
            tvar <- Op.freshTVar kindStar Meta
            inferUnify (Infer fullRange) (getRange par) optTp (makeOptional tvar)
            partp <- subst tvar

            -- infer expression
            (exprTp,exprEff,coreExpr) <- extendInfGamma False infgamma $ inferExpr (Just (partp,getRange par))
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
            let infgamma' = infgamma ++ [(binderName par,createNameInfoX Public (binderName par) DefVal (getRange par) tp)]

            -- build up core to get the optional value
            local <- uniqueName (show (binderName par))
            temp  <- uniqueName (show (binderName par))
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

isAmbiguous :: NameContext -> Expr Type -> Inf Bool
isAmbiguous ctx expr
  = case expr of
      (Var name isOp nameRange)
        -> do matches <- lookupNameEx (const True) name ctx nameRange
              case matches of
                []  -> return False
                [_] -> return False
                _   -> return True
      -- Handler{}        -> return True
      Parens (Lam{}) _ _ -> return True -- make parenthesized lambdas later in inference
      Parens body _ _    -> isAmbiguous ctx body
      _ -> return False


rootExpr expr
  = case expr of
      -- Let defs e _ -> rootExpr e
      -- Bind def e _ -> rootExpr e
      -- Ann e t r  -> rootExpr e  -- better to do FunFirst in this case
      Parens e _ r -> rootExpr e
      _            -> expr



{--------------------------------------------------------------------------
  inferSubsumeN
--------------------------------------------------------------------------}

-- | Infer types of function arguments
inferSubsumeN :: Context -> Range -> [(Type,Expr Type)] -> Inf ([Effect],[Core.Expr])
inferSubsumeN ctx range parArgs
  = do res <- inferSubsumeN' ctx range [] (zip [1..] parArgs)
       return (unzip res)

inferSubsumeN' ctx range acc []
  = return (map snd (sortBy (\(i,_) (j,_) -> compare i j) acc))
inferSubsumeN' ctx range acc parArgs
  = do lsArgs <- pickArgument parArgs
       let ((i,(tpar,arg)):rest) = lsArgs
       -- traceDoc $ \env -> text "inferSubsume: enter " <+> text (if isRho tpar then "instantiated" else "generalized") <+> text "expression"
       (targ,teff,core) <- allowReturn False $ inferExpr (Just (tpar,getRange arg)) (if isRho tpar then Instantiated else Generalized False) arg
       tpar1  <- subst tpar
       steff  <- subst teff
       (_,coref)  <- if isAnnot arg
                      then do -- traceDoc $ \env -> text "inferSubsumeN1:" <+> ppType env tpar1 <+> text "~" <+> ppType env targ
                              inferUnify ctx (getRange arg) tpar1 targ
                              return (tpar1,id)
                      else do -- traceDoc $ \env -> text "inferSubsumeN2:" <+> parens (ppType env steff) <+> colon <+> ppType env tpar1 <+> text "~" <+> ppType env targ
                              inferSubsume ctx (getRange arg) tpar1 targ
       rest1 <- mapM (\(j,(tpar,arg)) -> do{ stpar <- subst tpar; return (j,(stpar,arg)) }) rest
       teff1 <- subst teff
       -- traceDoc $ \env -> text " inferSubsumeEffect: " <+> ppType env teff <+> text " ~> " <+> ppType env teff1
       inferSubsumeN' ctx range ((i,(teff1,coref core)):acc) rest1

-- | Pick an argument that can be subsumed unambigiously..
-- split arguments on non-ambiguous variables and ambiguous ones
-- then we look for annotated arguments since their type is fully determined (and need no type propagation)
-- finally we look at parameters that are not a type variable since those are unambigiously determined (and benefit from type propagation)
-- and finally, we do the ones that are left over (the order of those does not matter)
pickArgument args
  = do ambs  <- mapM (\(i,(tpar,arg)) -> isAmbiguous (CtxType tpar) arg) args
       let (ambargs,args1)   = partition fst (zip ambs args)
           (annots,args2)    = partition (\(i,(tp,arg)) -> isAnnot arg) (map snd args1)
           (nonvars,args3)   = partition (\(i,(tp,arg)) -> not (isTVar tp)) args2
       return (annots ++ nonvars ++ args3 ++ map snd ambargs)

-- | Is an expression annotated?
isAnnot (Parens expr _ rng)   = isAnnot expr
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


matchFunTypeArgs :: Range -> Expr Type -> Type -> [Expr Type] -> [((Name,Range),Expr Type)] -> Inf ([(Int,Expr Type)],[(Name,Type)], Effect, Type,Core.Expr -> [Core.Expr] -> Core.Expr)
matchFunTypeArgs context fun tp fixed named
  = case tp of
       TFun pars eff res   -> do args <- matchParameters pars fixed named
                                 -- trace ("matched parameters: " ++ show (pars,map fst args)) $
                                 return (args,pars,eff,res,Core.App)
       TSyn _ _ t          -> matchFunTypeArgs context fun t fixed named
       TVar tv             -> do if (null named)
                                  then return ()
                                  else infError range (text "cannot used named arguments on an inferred function" <-> text " hint: annotate the parameters")
                                 targs <- mapM (\name -> do{ tv <- Op.freshTVar kindStar Meta; return (name,tv)}) ([nameNil | a <- fixed] ++ map (fst . fst) named)
                                 teff  <- Op.freshTVar kindEffect Meta
                                 tres  <- Op.freshTVar kindStar Meta
                                 -- trace ("Type.matchFunType: " ++ show tv ++ ": " ++ show (targs,teff,tres)) $
                                 extendSub (subSingle tv (TFun targs teff tres))
                                 return (zip [0..] (fixed ++ map snd named), targs,teff,tres,Core.App)
       _  -> do -- apply the copy constructor if we can find it
                matches <- lookupNameEx (const True) nameCopy (CtxFunTypes True [tp] []) range
                case matches of
                  [(qname,info)]
                    -> do (contp,_,coreInst) <- instantiateEx range (infoType info)
                          (args,pars,eff,res,_) <- matchFunTypeArgs context fun contp (fun:fixed) named
                          let coreAddCopy core coreArgs
                                = let coreVar = coreExprFromNameInfo qname info
                                  in (Core.App (coreInst coreVar) (coreArgs))
                          return (args,pars,eff,res,coreAddCopy)
                  _ -> do typeError context range (text "only functions or types with a copy constructor can be applied") tp []
                          return (zip [1..] (fixed ++ map snd named), [], typeTotal, typeUnit, Core.App)
  where
    range = getRange fun

    matchParameters :: [(Name,Type)] -> [Expr Type] -> [((Name,Range),Expr Type)] -> Inf [(Int,Expr Type)]
    matchParameters pars fixed named
      = -- trace ("match parameters: " ++ show (pars,length fixed,map (fst.fst) named)) $
        do (pars1,args1) <- matchFixed pars (zip [0..] fixed)
           iargs2        <- matchNamed (zip [length fixed..] pars1) (zip [length fixed..] named)
           return (args1 ++ map snd (sortBy (\(i,_) (j,_) -> compare i j) iargs2))

    matchFixed :: [(Name,Type)] -> [(Int,Expr Type)] -> Inf ([(Name,Type)],[(Int,Expr Type)])
    matchFixed pars []
      = return (pars,[])
    matchFixed ((name,tp):pars) ((i,arg):fixed)
      = do newarg <- if (isOptional tp)
                      then return (wrapOptional arg)
                     else if (isDelay tp)
                      then wrapDelay arg
                      else return arg
           (prest,rest) <- matchFixed pars fixed
           return (prest, (i,newarg):rest)
    matchFixed [] ((i,arg):_)
      = do typeError context (getRange fun) (text "function is applied to too many arguments") tp []
           return ([],[])

    -- in the result, the first int is position of the parameter, the second int it the original position of
    -- the argument (so we can evaluate in argument order)
    matchNamed :: [(Int,(Name,Type))] -> [(Int,((Name,Range),Expr Type))] -> Inf [(Int,(Int,Expr Type))]
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
                    return ((j,(i,newarg)):rest)
    matchNamed pars []
      = do if (all (isOptional . snd . snd) pars)
            then return [(j,(i,makeOptionalNone)) | (i,(j,(name,tpar))) <- zip [(length fixed + length named)..] pars]
            else do let hints = case rootExpr fun of
                                  (Var name isOp nameRange) | name == newName "resume"
                                    -> [(text "hint", text "cannot use \"resume\" inside a val/fun/except clause")]
                                  _ -> []
                    typeError context range (text "function has not enough arguments") tp hints
                    return []

    extract name acc []
      = Nothing
    extract name acc (par@(i,(parName,parType)):pars)
      = if (name == parName)
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
              Var name _ _ -> do matches <- lookupNameEx (const True) name (CtxFunArgs 0 []) (getRange expr)
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
  = do (vecName,vecTp,vecInfo) <- resolveFunName nameVector (CtxFunArgs 0 [newName "xs"]) rangeNull rangeNull -- todo: lookup vector less fragile?
       xs <- coreList tp cs
       return (Core.App (Core.TypeApp (coreExprFromNameInfo vecName vecInfo) [tp]) [xs])


coreList :: Type -> [Core.Expr] -> Inf Core.Expr
coreList tp cs
  = do (consName,consTp,consRepr,_) <- resolveConName nameCons Nothing rangeNull
       (nilName,nilTp,nilRepr,_) <- resolveConName nameNull Nothing rangeNull
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
      App (Var newLocal False rng) [_,(_, Parens (Lam [ValueBinder name _ _ _ _] body _ ) _ _)] _  -- fragile: expects this form from the parser
         | newLocal == nameLocal
         -> usesLocals (S.delete name lvars) body           
                                             
      Lam    binds expr rng  -> usesLocals lvars expr
      Let    defs expr range -> usesLocalsDefs lvars defs || usesLocals lvars expr
      Bind   def expr range  -> usesLocalsDef lvars def || usesLocals lvars expr
      App    fun nargs range -> any (usesLocals lvars) (fun : map snd nargs)      
      Ann    expr tp range   -> usesLocals lvars expr
      Case   expr brs range  -> usesLocals lvars expr || any (usesLocalsBranch lvars) brs
      Parens expr name range -> usesLocals lvars expr 
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
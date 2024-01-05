-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

module Type.InferMonad( Inf, InfGamma
                      , runInfer, tryRun
                      , traceDoc, traceDefDoc

                      -- * substitutation
                      , zapSubst
                      , subst, extendSub

                      -- * Environment
                      , getGamma
                      , extendGamma, extendGammaCore
                      , extendInfGamma, extendInfGammaEx, extendInfGammaCore
                      , withGammaType

                      -- * Name resolution

                      , resolveName
                      , resolveRhsName
                      , resolveFunName
                      , resolveConName
                      , resolveImplicitName

                      , lookupAppName
                      , lookupFunName
                      , lookupNameCtx
                      , lookupInfName
                      , NameContext(..), maybeToContext

                      , qualifyName
                      , getModuleName
                      , findDataInfo
                      , withDefName
                      , currentDefName
                      , isNamedLam
                      , getLocalVars

                      , FixedArg
                      , fixedContext, fixedCountContext

                      -- * Misc.
                      , allowReturn, isReturnAllowed
                      , useHole, allowHole, disallowHole
                      , withLhs, isLhs
                      , getPrettyEnv
                      , splitEffect
                      , occursInContext

                      -- * Operations
                      , generalize
                      , improve
                      , instantiate, instantiateNoEx, instantiateEx
                      , checkEmptyPredicates
                      , checkCasing
                      , normalize
                      , getResolver
                      , getNewtypes

                      -- * Unification
                      , Context(..)
                      , inferUnify, inferUnifies
                      , inferSubsume
                      , withSkolemized, checkSkolemEscape

                      , typeError
                      , contextError
                      , termError
                      , infError, infWarning
                      , withHiddenTermDoc

                      -- * Documentation, Intellisense
                      , addRangeInfo

                      ) where

import Data.List( partition, sortBy, nub, nubBy, intersperse, foldl')
import Data.Ord(comparing)
import Control.Applicative
import Control.Monad

import Lib.PPrint
import Common.Range hiding (Pos)
import Common.Unique
import Common.Failure
import Common.Error
import Common.Syntax( Visibility(..))
import Common.File(endsWith,normalizeWith)
import Common.Name
import Common.NamePrim(nameTpVoid,nameTpPure,nameTpIO,nameTpST,nameTpAsyncX,
                       nameTpRead,nameTpWrite,namePredHeapDiv,nameReturn,
                       nameTpLocal, nameCopy)
-- import Common.Syntax( DefSort(..) )
import Common.ColorScheme
import Kind.Kind
import Kind.ImportMap
import Kind.Newtypes
import Kind.Synonym
import Type.Type
import Type.TypeVar
import Type.Kind
import qualified Type.Pretty as Pretty
import qualified Core.Core as Core

import Type.Operations hiding (instantiate, instantiateNoEx, instantiateEx)
import qualified Type.Operations as Op
import Type.Assumption
import Type.InfGamma

import Type.Unify
import Common.Message( docFromRange, table, tablex)

import Core.Pretty()

import Syntax.RangeMap( RangeMap, RangeInfo(..), rangeMapInsert )
import Syntax.Syntax(Expr(..),ValueBinder(..))

import qualified Lib.Trace( trace )

trace s x =
  Lib.Trace.trace (" " ++ s)
   x

{--------------------------------------------------------------------------
  Generalization
--------------------------------------------------------------------------}
generalize :: Range -> Range -> Bool -> Effect -> Rho -> Core.Expr -> Inf (Scheme,Core.Expr )
generalize contextRange range close eff  tp@(TForall _ _ _)  core0
  = {-
    trace ("generalize forall: " ++ show tp) $
    return (tp,core0)
    -}
    do seff <- subst eff
       stp  <- subst tp
       free0 <- freeInGamma
       let free = tvsUnion free0 (fuv seff)
       ps0  <- splitPredicates free
       if (tvsIsEmpty (fuv ({- seff, -} stp)))
        then -- Lib.Trace.trace ("generalize forall: " ++ show (pretty stp)) $
              return (tp,core0)
        else -- Lib.Trace.trace ("generalize forall-inst: " ++ show (pretty seff, pretty stp) ++ " with " ++ show ps0) $
             do (rho,tvars,icore) <- instantiateNoEx range stp
                generalize contextRange range close seff rho (icore core0)

generalize contextRange range close eff0 rho0 core0
  = do seff <- subst eff0
       srho  <- subst rho0
       free0 <- freeInGamma
       let free = tvsUnion free0 (fuv seff)
       ps0  <- splitPredicates free
       score0 <- subst core0

       sub <- getSub
       -- trace ("generalize: " ++ show (pretty seff,pretty srho) ++ " with " ++ show ps0)
                  {- ++ " and free " ++ show (tvsList free) -}
                  {- ++ "\n subst=" ++ show (take 10 $ subList sub) -}
                  {- ++ "\ncore: " ++ show score0 -}
       --        $ return ()
       -- simplify and improve predicates
       (ps1,(eff1,rho1),core1) <- simplifyAndResolve contextRange free ps0 (seff,srho)
       -- trace (" improved to: " ++ show (pretty eff1, pretty rho1) ++ " with " ++ show ps1 ++ " and free " ++ show (tvsList free) {- ++ "\ncore: " ++ show score0 -}) $ return ()
       let -- generalized variables
           tvars0 = filter (\tv -> not (tvsMember tv free)) (ofuv (TForall [] (map evPred ps1) rho1))

       if (null tvars0)
        then do addPredicates ps1 -- add them back to solve later (?)
                score <- subst (core1 core0)

                -- substitute more free variables in the core with ()
                let score1 = substFree free score
                nrho <- normalizeX close free rho1
                -- trace ("generalized to (as rho type): " ++ show (pretty nrho)) $ return ()
                return (nrho,score1)

        else do -- check that the computation is total
                if (close)
                 then inferUnify (Check "Generalized values cannot have an effect" contextRange) range typeTotal eff1
                 else return ()
                -- simplify and improve again since we can have substituted more
                (ps2,(eff2,rho2),core2) <- simplifyAndImprove contextRange free ps1 (eff1,rho1)
                -- due to improvement, our constraints may need to be split again
                addPredicates ps2
                ps3 <- splitPredicates free
                -- simplify and improve again since we can have substituted more
                (ps4,(eff4,rho4),core4) <- simplifyAndImprove contextRange free ps3 (eff2,rho2)

                -- check for satisifiable constraints
                checkSatisfiable contextRange ps4
                score <- subst (core4 (core2 (core1 core0)))
                -- trace (" before normalize: " ++ show (eff4,rho4) ++ " with " ++ show ps4) $ return ()

                -- update the free variables since substitution may have changed it
                free1 <- freeInGamma
                let free = tvsUnion free1 (fuv eff4)

                -- (rho5,coref) <- isolate free rho4
                let rho5 = rho4
                    coref = id

                nrho <- normalizeX close free rho5
                -- trace (" normalized: " ++ show (nrho) ++ " from " ++ show rho4) $ return ()
                let -- substitute to Bound ones
                    tvars = filter (\tv -> not (tvsMember tv free)) (ofuv (TForall [] (map evPred ps4) nrho))
                    bvars = [TypeVar id kind Bound | TypeVar id kind _ <- tvars]
                    bsub  = subNew (zip tvars (map TVar bvars))
                    (TForall [] ps5 rho5) = bsub |-> (TForall [] (map evPred ps4) nrho)
                    -- core
                    core5 = Core.addTypeLambdas bvars $
                            bsub |-> score
                            -- no lambdas for now...
                            -- (Core.addLambda (map evName ps4) score)

                    resTp = quantifyType bvars (qualifyType ps5 rho5)
                -- extendSub bsub
                -- substitute more free variables in the core with ()
                let core6 = substFree free core5
                -- trace ("generalized to: " ++ show (pretty resTp)) $ return ()
                return (resTp, core6)

  where
    substFree free core
      = core
      -- TODO: check why we need to do the below?
      {-
        let fvars = tvsDiff (ftv core) free
            tcon kind
              = if (kind == kindEffect)
                 then typeTotal
                 else if (kind == kindStar)
                  then typeVoid
                  else TCon (TypeCon nameTpVoid kind) -- TODO: make something up for now
        in if (tvsIsEmpty fvars)
            then core
            else let sub = subNew [(tv,tcon (getKind tv)) | tv <- tvsList fvars]
                 in sub |-> core
       -}


improve :: Range -> Range -> Bool -> Effect -> Rho -> Core.Expr -> Inf (Rho,Effect,Core.Expr )
improve contextRange range close eff0 rho0 core0
  = do seff  <- subst eff0
       srho  <- subst rho0
       free  <- freeInGamma
       -- let free = tvsUnion free0 (fuv seff)
       sps    <- splitPredicates free
       score0 <- subst core0
       -- trace (" improve: " ++ show (Pretty.niceTypes Pretty.defaultEnv [seff,srho]) ++ " with " ++ show sps ++ " and free " ++ show (tvsList free) {- ++ "\ncore: " ++ show score0 -}) $ return ()

       -- isolate: do first to discharge certain hdiv predicates.
       -- todo: in general, we must to this after some improvement since that can lead to substitutions that may enable isolation..
       (ps0,eff0,coref0) <- isolate contextRange (tvsUnions [free,ftv srho]) sps seff

       -- simplify and improve predicates
       (ps1,(eff1,rho1),coref1) <- simplifyAndResolve contextRange free ps0 (eff0,srho)
       addPredicates ps1  -- add unsolved ones back
       -- isolate
       -- (eff2,coref2) <- isolate (tvsUnions [free,ftv rho1,ftv ps1]) eff1

       (nrho) <- normalizeX close free rho1
       -- trace (" improve normalized: " ++ show (nrho) ++ " from " ++ show rho1) $ return ()
       -- trace (" improved to: " ++ show (pretty eff1, pretty nrho) ++ " with " ++ show ps1) $ return ()
       return (nrho,eff1,coref1 (coref0 core0))

getResolver :: Inf (Name -> Core.Expr)
getResolver
  = do env <- getEnv
       return (\name -> case gammaLookup name (gamma env) of
                          [(qname,info)] -> coreExprFromNameInfo qname info
                          _              -> failure $ "Type.InferMonad:getResolver: called with unknown name: " ++ show name)


instantiate :: Range -> Scheme -> Inf (Rho,[TypeVar],Core.Expr -> Core.Expr)
instantiate = instantiateEx

instantiateEx :: Range -> Scheme -> Inf (Rho,[TypeVar],Core.Expr -> Core.Expr)
instantiateEx range tp | isRho tp
  = do (rho,coref) <- Op.extend tp
       return (rho,[],coref)
instantiateEx range tp
  = do (tvars,ps,rho,coref) <- Op.instantiateEx range tp
       addPredicates ps
       return (rho, tvars, coref)

instantiateNoEx :: Range -> Scheme -> Inf (Rho,[TypeVar],Core.Expr -> Core.Expr)

instantiateNoEx range tp | isRho tp
  = return (tp,[],id)
instantiateNoEx range tp
  = do (tvars,ps,rho,coref) <- Op.instantiateNoEx range tp
       addPredicates ps
       return (rho, tvars, coref)

-- | Automatically remove heap effects when safe to do so.
isolate :: Range -> Tvs -> [Evidence] -> Effect -> Inf ([Evidence],Effect, Core.Expr -> Core.Expr)
{-
isolate rng free ps eff  | src `endsWith` "std/core/hnd.kk"
  = return (ps,eff,id)
  where
    src = normalizeWith '/' (sourceName (rangeSource rng))
-}
isolate rng free ps eff
  = -- trace ("isolate: " ++ show eff ++ " with free " ++ show (tvsList free)) $
    let (ls,tl) = extractOrderedEffect eff
    in case filter (\l -> labelName l `elem` [nameTpLocal,nameTpRead,nameTpWrite]) ls of
          (lab@(TApp labcon [TVar h]) : _)
            -> -- has heap variable 'h' in its effect
               do -- trace ("isolate:" ++ show (sourceName (rangeSource rng)) ++ ": " ++ show (pretty eff)) $ return ()
                  (polyPs,ps1) <- splitHDiv h ps
                  let isLocal = (labelName lab == nameTpLocal)
                  if not (-- null polyPs ||  -- TODO: we might want to isolate too if it is not null?
                                             -- but if we allow null polyPS, injecting state does not work (see `test/resource/inject2`)
                          tvsMember h free || tvsMember h (ftv ps1))
                    then do -- yeah, we can isolate, and discharge the polyPs hdiv predicates
                            tv <- freshTVar kindEffect Meta
                            if isLocal
                             then do trace ("isolate local") $ return ()
                                     nofailUnify $ unify (effectExtend lab tv) eff
                             else do mbSyn <- lookupSynonym nameTpST
                                     let (Just syn) = mbSyn
                                         [bvar] = synInfoParams syn
                                         st     = subNew [(bvar,TVar h)] |-> synInfoType syn
                                     nofailUnify $ unify (effectExtend st tv) eff
                            neweff <- subst tv
                            sps    <- subst ps1
                            -- trace ("isolate to:"  ++ show (pretty neweff)) $ return ()
                            -- return (sps, neweff, id) -- TODO: supply evidence (i.e. apply the run function)
                            -- and try again
                            (sps',eff',coref) <- isolate rng free sps neweff
                            let coreRun cexpr = if (isLocal)
                                                 then cexpr
                                                 else cexpr  -- TODO: apply runST?
                            return (sps',eff',coreRun . coref)
                     else return (ps,eff,id)
          _ -> return (ps,eff,id)

  where
    -- | 'splitHDiv h ps' splits predicates 'ps'. Predicates of the form hdiv<h,tp,e> where tp does
    -- not contain h are returned as the first element, all others as the second. This includes
    -- constraints where hdiv<h,a,e> for example where a is polymorphic. Normally, we need to assume
    -- divergence conservatively in such case; however, when we isolate, we know it cannot be instatiated
    -- to contain a reference to h and it is safe to discharge them during isolation without implying
    -- divergence. See test\type\talpin-jouvelot1 for an example: fun rid(x) { r = ref(x); return !r }
    splitHDiv :: TypeVar -> [Evidence] -> Inf ([Evidence],[Evidence])
    splitHDiv heapTv []
      = return ([],[])
    splitHDiv heapTv (ev:evs)
      = do (evs1,evs2) <- splitHDiv heapTv evs
           let defaultRes = (evs1,ev:evs2)
           case evPred ev of
            PredIFace name [hp,tp,eff]  | name == namePredHeapDiv
             -> do shp <- subst hp
                   case expandSyn shp of
                     h@(TVar tv)  | tv == heapTv
                       -> do stp <- subst tp
                             if (not (h `elem` heapTypes stp))
                              then return (ev:evs1,evs2) -- even if polymorphic, we are ok if we isolate
                              else return defaultRes
                     _ -> return defaultRes
            _ -> return defaultRes



data Variance = Neg | Inv | Pos
              deriving (Eq,Ord,Enum,Show)

vflip Neg = Pos
vflip Pos = Neg
vflip Inv = Inv

normalize :: Bool -> Rho -> Inf Rho
normalize close tp
  = do free <- freeInGamma
       normalizeX close free tp

normalizeX :: Bool -> Tvs -> Rho -> Inf Rho
normalizeX close free tp
  = case tp of
      TForall [] [] t
        -> normalizeX close free t
      TSyn syn targs t
        -> do t' <- normalizeX close free t
              return (TSyn syn targs t')
      TFun args eff res
        -> do (ls,tl) <- nofailUnify $ extractNormalizeEffect eff
              -- trace (" normalizeX: " ++ show (map pretty ls,pretty tl)) $ return ()
              eff'    <- case expandSyn tl of
                          -- remove tail variables in the result type
                          (TVar tv) | close && isMeta tv && not (tvsMember tv free) && not (tvsMember tv (ftv (res:map snd args)))
                            -> -- trace ("close effect: " ++ show (pretty tp)) $
                               do nofailUnify $ unify typeTotal tl
                                  (subst eff) -- (effectFixed ls)
                          _ -> do ls' <- mapM (normalizex Pos) ls
                                  tl' <- normalizex Pos tl
                                  return (effectExtends ls' tl')
              args' <- mapM (\(name,arg) -> do{arg' <- normalizex Neg arg; return (name,arg')}) args
              res'  <- normalizex Pos res
              niceEff <- nicefyEffect eff'
              return (TFun args' niceEff res')
      _ -> normalizex Pos tp
  where
    normalizex Inv tp
      = return tp
    normalizex var tp
      = case tp of
          TFun args eff res
            -> do (ls,tl) <- nofailUnify $ extractNormalizeEffect eff
                  eff'    <- case expandSyn tl of
                  -- we can only do this if 'tl' does not also occur anywhere else without
                  -- the same label present...
                  -- see 'catch' and 'run' for example
                  {-
                              (TVar tv) | isMeta tv && var == Neg -- remove labels in extensible argument types
                                -> normalizex var tl
                  -}
                              _ -> do ls' <- mapM (normalizex var) ls
                                      tl' <- normalizex var tl
                                      return $ effectExtends ls' tl'
                  args' <- mapM (\(name,arg) -> do{arg' <- normalizex (vflip var) arg; return (name,arg')}) args
                  res'  <- normalizex var res
                  niceEff <- nicefyEffect eff'
                  return (TFun args' niceEff res')
          TForall vars preds t
            -> do t' <- normalizex var t
                  return (TForall vars preds t')
          TApp t args
            -> do t' <- normalizex var t
                  return (TApp t' args)
          TSyn syn args t
            -> do t' <- normalizex var t
                  return (TSyn syn args t')
          _ -> return tp


nicefyEffect :: Effect -> Inf Effect
nicefyEffect eff
  = do let (ls,tl) = extractOrderedEffect eff
       ls' <- matchAliases [nameTpIO, nameTpST, nameTpPure, nameTpAsyncX] ls
       return (foldr (\l t -> TApp (TCon tconEffectExtend) [l,t]) tl ls') -- cannot use effectExtends since we want to keep synonyms
  where
    matchAliases :: [Name] -> [Tau] -> Inf [Tau]
    matchAliases names ls
      = case names of
          [] -> return ls
          (name:ns)
            -> do (pre,post) <- tryAlias ls name
                  post' <- matchAliases ns post
                  return (pre ++ post')

    tryAlias :: [Tau] -> Name -> Inf ([Tau],[Tau])
    tryAlias [] name
      = return ([],[])
    tryAlias ls name
      = do mbsyn <- lookupSynonym name
           case mbsyn of
             Nothing -> return ([],ls)
             Just syn
              -> let (ls2,tl2) = extractOrderedEffect (synInfoType syn)
                 in if (null ls2 || not (isEffectEmpty tl2))
                     then return ([],ls)
                     else let params      = synInfoParams syn
                              (sls,insts) = findInsts params ls2 ls
                          in -- Lib.Trace.trace ("* try alias: " ++ show (synInfoName syn, ls, sls)) $
                             case (isSubset [] sls ls) of
                                Just rest
                                  -> -- Lib.Trace.trace (" synonym replace: " ++ show (synInfoName syn, ls, sls, rest)) $
                                      return ([TSyn (TypeSyn name (synInfoKind syn) (synInfoRank syn) (Just syn)) insts (effectFixed sls)], rest)
                                _ -> return ([], ls)

findInsts :: [TypeVar] -> [Tau] -> [Tau] -> ([Tau],[Tau])
findInsts [] ls _
  = (ls,[])
findInsts params ls1 ls2
  = case filter matchParams ls1 of
      [] -> (ls1,map TVar params)
      (tp:_)
        -> let name = labelName tp
           in case filter (\t -> labelName t == name) ls2 of
                (TApp _ args : _) | length args == length params
                  -> (subNew (zip params args) |-> ls1, args)
                _ -> (ls1, map TVar params)
  where
    matchParams (TApp _ args) = (map TVar params == args)
    matchParams _ = False



isSubset :: [Tau] -> [Tau] -> [Tau] -> Maybe [Tau]
isSubset acc ls1 ls2
  = case (ls1,ls2) of
      ([],[])       -> Just (reverse acc)
      ([],(l2:ll2)) -> Just (reverse acc ++ ls2)
      (l1:ll1, [])  -> Nothing
      (l1:ll1,l2:ll2)
        -> if (labelName l1 < labelName l2)
            then Nothing
           else if (labelName l1 > labelName l2)
            then isSubset (l2:acc) ls1 ll2
           else if (l1 == l2)
            then isSubset acc ll1 ll2
            else Nothing

splitEffect :: Effect -> Inf ([Tau],Effect)
splitEffect eff
  = nofailUnify (extractNormalizeEffect eff)


-- | Simplify and improve contraints.
simplifyAndImprove :: Range -> Tvs -> [Evidence] -> (Effect,Type) -> Inf ([Evidence],(Effect,Type),Core.Expr -> Core.Expr)
simplifyAndImprove range free [] efftp
  = return ([],efftp,id)
simplifyAndImprove range free evs efftp
  = do (evs1,core1) <- improveEffects range free evs efftp
       efftp1 <- subst efftp
       return (evs1,efftp1,core1)

-- | Simplify and resolve contraints.
simplifyAndResolve :: Range -> Tvs -> [Evidence] -> (Effect,Type) -> Inf ([Evidence],(Effect,Type),Core.Expr -> Core.Expr)
simplifyAndResolve range free [] efftp
  = return ([],efftp,id)
simplifyAndResolve range free evs efftp
  = do evs0   <- resolveHeapDiv free evs  -- must be done *before* improveEffects since it can add "div <= e" constraints
       (evs1,core1) <- improveEffects range free evs0 efftp
       efftp1 <- subst efftp
       return (evs1,efftp1,core1)


resolveHeapDiv :: Tvs -> [Evidence] -> Inf [Evidence]
resolveHeapDiv free []
  = return []
resolveHeapDiv free (ev:evs)
  = case evPred ev of
      PredIFace name [hp,tp,eff]  | name == namePredHeapDiv
        -> -- trace (" resolveHeapDiv: " ++ show (hp,tp,eff)) $
           do stp <- subst tp
              shp <- subst hp
              let tvsTp = ftv stp
                  tvsHp = ftv hp
              if (expandSyn shp `elem` heapTypes stp ||
                  not (tvsIsEmpty (ftv stp)) -- conservative guess...
                 )
               then do -- return (ev{ evPred = PredSub typeDivergent eff } : evs')
                       tv   <- freshTVar kindEffect Meta
                       let divEff = effectExtend typeDivergent tv
                       inferUnify (Infer (evRange ev)) (evRange ev) eff divEff
                       resolveHeapDiv free evs
               else resolveHeapDiv free evs -- definitely ok
      _ -> do evs' <- resolveHeapDiv free evs
              return (ev:evs')


heapTypes :: Type -> [Type]
heapTypes tp
  = case expandSyn tp of
      TForall _ ps r -> concatMap heapTypesPred ps ++ heapTypes r
      TFun xs e r    -> concatMap (heapTypes . snd) xs ++ heapTypes e ++ heapTypes r
      TApp    t ts   | getKind tp /= kindHeap
                     -> concatMap heapTypes (t:ts)
      t              -> if (getKind t == kindHeap) then [t] else []

heapTypesPred p
  = case p of
      PredSub t1 t2  -> heapTypes t1 ++ heapTypes t2
      PredIFace _ ts -> concatMap heapTypes ts

improveEffects :: Range -> Tvs -> [Evidence] -> (Effect,Type) -> Inf ([Evidence],Core.Expr -> Core.Expr)
improveEffects contextRange free evs etp
  = return (evs,id)

{--------------------------------------------------------------------------
  Satisfiable constraints
--------------------------------------------------------------------------}

checkEmptyPredicates :: Range -> Inf (Core.Expr -> Core.Expr)
checkEmptyPredicates contextRange
  = do free <- freeInGamma
       ps <- getPredicates
       (ps1,_,core1) <- simplifyAndImprove contextRange free ps (typeTotal,typeUnit)
       setPredicates ps1
       checkSatisfiable contextRange ps1
       return core1

-- | Check if all constraints are potentially satisfiable. Assumes that
-- the constraints have already been simplified and improved.
checkSatisfiable :: Range -> [Evidence] -> Inf ()
checkSatisfiable contextRange ps
  = do mapM_ check ps
  where
    check ev
      = case evPred ev of
          PredSub _  _ -> predicateError contextRange (evRange ev) "Constraint cannot be satisfied" (evPred ev)
          _            -> return ()



{--------------------------------------------------------------------------
  Unify Helpers
--------------------------------------------------------------------------}
data Context = Check String Range
             | Infer Range

instance Ranged Context where
  getRange (Check _ rng) = rng
  getRange (Infer rng)   = rng

inferUnify :: Context -> Range -> Type -> Type -> Inf ()
inferUnify context range expected tp
  = do (sexp,stp) <- subst (expected,tp)
       -- trace ("infer unify: " ++ show (Pretty.niceTypes Pretty.defaultEnv [sexp,stp])) $ return ()
       res <- doUnify (unify sexp stp)
       case res of
         Right () -> return ()
         Left err -> unifyError context range err sexp stp


inferUnifies :: Context -> [(Range,Type)] -> Inf Type
inferUnifies context tps
  = case tps of
      [] -> matchFailure "Type.InferMonad.inferUnifies"
      [(rng,tp)] -> return tp
      ((rng1,tp1):(rng2,tp2):rest)
        -> do let rng = combineRange rng1 rng2
              inferUnify context rng tp1 tp2
              tp <- subst tp1
              inferUnifies context ((rng,tp):rest)

inferSubsume :: Context -> Range -> Type -> Type -> Inf (Type,Core.Expr -> Core.Expr)
inferSubsume context range expected tp
  = do free <- freeInGamma
       (sexp,stp) <- subst (expected,tp)
       -- trace ("inferSubsume: " ++ show (tupled [pretty sexp,pretty stp]) ++ " with free " ++ show (tvsList free)) $ return ()
       res <- doUnify (subsume range free sexp stp)
       case res of
         Right (t,_,ps,coref) -> do addPredicates ps
                                    return (t,coref)
         Left err             -> do unifyError context range err sexp stp
                                    return (expected,id)

nofailUnify :: Unify a -> Inf a
nofailUnify u
  = do res <- runUnify u
       case res of
         (Right x,sub)
          -> do extendSub sub
                return x
         (Left err,sub)
          -> do extendSub sub
                failure ("Type.InferMonad.runUnify: should never fail!")

withSkolemized :: Range -> Type -> Maybe Doc -> (Type -> [TypeVar] -> Inf (a,Tvs)) -> Inf a
withSkolemized rng tp mhint action
  = do (xvars,_,xrho,_) <- Op.skolemizeEx rng tp
       (x,extraFree) <- action xrho xvars
       checkSkolemEscape rng xrho mhint xvars extraFree
       return x
       {-
       --sub <- getSub
       free <- freeInGamma
       let allfree = tvsUnion free extraFree
           --escaped = fsv $ [tp  | (tv,tp) <- subList sub, tvsMember tv allfree]
       if (tvsDisjoint (tvsNew xvars) allfree)
         then return ()
         else do sxrho <- subst xrho
                 let escaped = [v | v <- xvars, tvsMember v allfree]
                 termError rng (text "abstract type(s) escape(s) into the context") (sxrho) (maybe [] (\hint -> [(text "hint",hint)]) mhint)
       return x
       -}

checkSkolemEscape :: Range -> Type -> Maybe Doc -> [TypeVar] -> Tvs -> Inf ()
checkSkolemEscape rng tp mhint [] extraFree
  = return ()
checkSkolemEscape rng tp mhint skolems extraFree
  = do free <- freeInGamma
       let allfree = tvsUnion free extraFree
           --escaped = fsv $ [tp  | (tv,tp) <- subList sub, tvsMember tv allfree]
       -- penv <- getPrettyEnv
       -- trace (show (text "checkSkolemEscape:" <+> tupled [Pretty.ppType penv tp, pretty skolems, pretty (tvsList allfree)])) $
       if (tvsDisjoint (tvsNew skolems) allfree)
         then return ()
         else do stp <- subst tp
                 let escaped = [v | v <- skolems, tvsMember v allfree]
                 termError rng (text "abstract type(s) escape(s) into the context") (stp)
                               (maybe [(text "hint",text "give a higher-rank type annotation to a function parameter?")]
                                      (\hint -> [(text "hint",hint)]) mhint)




doUnify :: Unify a -> Inf (Either UnifyError a)
doUnify u
  = do res <- runUnify u
       case res of
         (Right x,sub)
          -> do extendSub sub
                return (Right x)
         (Left err,sub)
          -> do extendSub sub
                return (Left err)

occursInContext :: TypeVar -> Tvs -> Inf Bool
occursInContext tv extraFree
  = do free <- freeInGamma
       let allFree = tvsUnion free extraFree
       return (tvsMember tv allFree)

{--------------------------------------------------------------------------
  Unification errors
--------------------------------------------------------------------------}
unifyError :: Context -> Range -> UnifyError -> Type -> Type -> Inf a
unifyError context range (NoMatchEffect eff1 eff2) _ _
  = unifyError context range NoMatch eff2 eff1
unifyError context range err xtp1 xtp2
  = do free <- freeInGamma
       tp1 <- subst xtp1 >>= normalizeX False free
       tp2 <- subst xtp2 >>= normalizeX False free
       env <- getEnv
       unifyError' (prettyEnv env){Pretty.fullNames = False} context range err tp1 tp2

unifyError' env context range err tp1 tp2
  = do termDoc <- getTermDoc "term" range
       infError range $
        text message <->
        table ([(text "context", docFromRange (Pretty.colors env) rangeContext)
               , termDoc
               ,(text ("inferred " ++ nameType), nice2)
               ]
               ++ nomatch
               ++ extra
               ++ hint
              )
  where
    (rangeContext,extra)
      = case context of
          Check msg range -> (range,[(text "because", text msg)])
          Infer range     -> (range,[])

    [nice1,nice2]
      = Pretty.niceTypes showEnv [tp1,tp2]

    showEnv
      = case err of
          NoMatchKind -> env{ Pretty.showKinds = True }
          _           -> env

    nomatch
      = case err of
          NoSubsume       -> [(text "is less general than",nice1)]
          NoEntail        -> [(text "is not entailed by",nice1)]
          NoArgMatch _ _  -> []
          _               -> [(text ("expected " ++ nameType),nice1)]

    nameType
      = if (getKind tp1 == kindEffect)
         then "effect"
         else "type"


    (message,hint)
      = case err of
          NoMatch     -> (nameType ++ "s do not match",[])
          NoMatchKind -> ("kinds do not match",[])
          NoMatchPred -> ("predicates do not match",[])
          NoMatchSkolem kind
                      -> ("abstract types do not match",if (not (null extra))
                                                         then []
                                                         else [(text "hint", if (isKindHeap kind || isKindScope kind)
                                                                         then text "a local variable or reference escapes its scope?"
                                                                         else text "an higher-rank type escapes its scope?")])
          NoSubsume   -> ("type is not polymorphic enough",[(text "hint",text "give a higher-rank type annotation to a function parameter?")])
          NoEntail    -> ("predicates cannot be resolved",[])
          Infinite    -> ("types do not match (due to an infinite type)",[(text "hint",text "give a type to the function definition?")])
          NoMatchEffect{}-> ("effects do not match",[])
          NoArgMatch n m -> if (m<0)
                             then ("only functions can be applied",[])
                             else ("application has too " ++ (if (n > m) then "few" else "many") ++ " arguments"
                                  ,[(text "hint",text ("expecting " ++ show n ++ " argument" ++ (if n == 1 then "" else "s") ++ " but has been given " ++ show m))])

predicateError :: Range -> Range -> String -> Pred -> Inf ()
predicateError contextRange range message pred
  = do env <- getEnv
       spred <- subst pred
       predicateError' (prettyEnv env) contextRange range message spred

predicateError' env contextRange range message pred
  = do termDoc <- getTermDoc "origin" range
       infError range $
        text message <->
        table  [(text "context", docFromRange (Pretty.colors env) contextRange)
               , termDoc
               ,(text "constraint", nicePred)
               ]
  where
    nicePred  = Pretty.ppPred env pred


typeError :: Range -> Range -> Doc -> Type -> [(Doc,Doc)] -> Inf ()
typeError contextRange range message xtp extra
  = do env  <- getEnv
       free <- freeInGamma
       tp   <- subst xtp >>= normalizeX False free
       typeError' (prettyEnv env) contextRange range message tp extra

typeError' env contextRange range message tp extra
  = do termDoc <- getTermDoc "term" range
       infError range $
        message <->
        table ([(text "context", docFromRange (Pretty.colors env) contextRange)
              , termDoc
              ,(text "inferred type", Pretty.niceType env tp)
              ] ++ extra)

contextError :: Range -> Range -> Doc -> [(Doc,Doc)] -> Inf ()
contextError contextRange range message extra
  = do env <- getEnv
       contextError' (prettyEnv env) contextRange range message extra

contextError' env contextRange range message extra
  = do termDoc <- getTermDoc "term" range
       infError range $
        message <->
        table  ([(text "context", docFromRange (Pretty.colors env) contextRange)
                , termDoc
                ]
                ++ extra)

termError :: Range -> Doc -> Type -> [(Doc,Doc)] -> Inf ()
termError range message tp extra
  = do env <- getEnv
       termError' (prettyEnv env) range message tp extra

termError' env range message tp extra
  = do termDoc <- getTermDoc "term" range
       infError range $
        message <->
        table  ([ termDoc
                ,(text "inferred type", Pretty.niceType env tp)
                ]
                ++ extra)



----------------------------------------------------------------
-- Resolve names
----------------------------------------------------------------

-- | Lookup a name with a certain type and return the fully qualified name and its type
resolveName :: Name -> Maybe (Type,Range) -> Range -> Inf (Name,Type,NameInfo)
resolveName name mbType range
  = case mbType of
      Just (tp,ctxRange) -> resolveNameEx infoFilter (Just infoFilterAmb) name (CtxType tp) ctxRange range
      Nothing            -> resolveNameEx infoFilter (Just infoFilterAmb) name CtxNone range range
  where
    infoFilter = isInfoValFunExt
    infoFilterAmb = not . isInfoImport

-- | Lookup a name with a certain type and return the fully qualified name and its type
-- because of local variables and references a typed lookup may fail as we need to
-- dereference first. So we do a typed lookup first and fall back to untyped lookup
resolveRhsName :: Name -> (Type,Range) -> Range -> Inf (Name,Type,NameInfo)
resolveRhsName name (tp,ctxRange) range
  = do candidates <- lookupNameCtx isInfoValFunExt name (CtxType tp) range
       case candidates of
         -- unambiguous and matched
         [(qname,info)]
              -> do checkCasing range name qname info
                    return (qname,infoType info,info)
         -- not found; this may be due to needing a coercion term
         []   -> resolveName name Nothing range    -- try again without type info
         -- still ambiguous (even with a type), call regular lookup to throw an error
         amb  -> resolveName name (Just (tp,ctxRange)) range


-- | Lookup a name with a number of arguments and return the fully qualified name and its type
resolveFunName :: Name -> NameContext -> Range -> Range -> Inf (Name,Type,NameInfo)
resolveFunName name ctx rangeContext range
  = resolveNameEx infoFilter (Just infoFilterAmb) name ctx rangeContext range
  where
    infoFilter = isInfoValFunExt
    infoFilterAmb = not . isInfoImport

resolveConName :: Name -> Maybe (Type) -> Range -> Inf (Name,Type,Core.ConRepr,ConInfo)
resolveConName name mbType range
  = do (qname,tp,info) <- resolveNameEx isInfoCon Nothing name (maybeToContext mbType) range  range
       return (qname,tp,infoRepr info,infoCon info)


resolveNameEx :: (NameInfo -> Bool) -> Maybe (NameInfo -> Bool) -> Name -> NameContext -> Range -> Range -> Inf (Name,Type,NameInfo)
resolveNameEx infoFilter mbInfoFilterAmb name ctx rangeContext range
  = do matches <- lookupNameCtx infoFilter name ctx range
       case matches of
        []   -> do amb <- case ctx of
                            CtxNone -> return []
                            _       -> lookupNameCtx infoFilter name CtxNone range
                   env <- getEnv
                   let penv = prettyEnv env
                       ctxTerm rangeContext = [(text "context", docFromRange (Pretty.colors penv) rangeContext)
                                              ,(text "term", docFromRange (Pretty.colors penv) range)]
                   case (ctx,amb) of
                    (CtxType tp, [(qname,info)])
                      -> do let [nice1,nice2] = Pretty.niceTypes penv [tp,infoType info]
                            infError range (text "identifier" <+> Pretty.ppName penv name <+> text "does not match the argument types" <->
                                               table (ctxTerm rangeContext ++
                                                      [(text "inferred type",nice2)
                                                      ,(text "expected type",nice1)]))
                    (CtxType tp, (_:rest))
                      -> infError range (text "identifier" <+> Pretty.ppName penv name <+> text "has no matching definition" <->
                                         table (ctxTerm rangeContext ++
                                                [(text "inferred type", Pretty.niceType penv tp)
                                                ,(text "candidates", align (tablex 0 (ppCandidates env  "" amb)))]))
                    (CtxFunArgs fixed named (Just resTp), (_:rest))
                      -> do let message = "with " ++ show (fixed + length named) ++ " argument(s) matches the result type"
                            infError range (text "no function" <+> Pretty.ppName penv name <+> text message <+>
                                            Pretty.niceType penv resTp <.> ppAmbiguous env "" amb)
                    (CtxFunArgs fixed named Nothing, (_:rest))
                      -> do let message = "takes " ++ show (fixed + length named) ++ " argument(s)" ++
                                          (if null named then "" else " with such parameter names")
                            infError range (text "no function" <+> Pretty.ppName penv name <+> text message <.> ppAmbiguous env "" amb)
                    (CtxFunTypes partial fixed named mbResTp, (_:rest))
                      -> do let docs = Pretty.niceTypes penv (fixed ++ map snd named)
                                fdocs = take (length fixed) docs
                                ndocs = [color (colorParameter (Pretty.colors penv)) (pretty n <+> text ":") <+> tpdoc |
                                           ((_,n),tpdoc) <- zip named (drop (length fixed) docs)]
                                pdocs = if partial then [text "..."] else []
                                argsDoc = color (colorType (Pretty.colors penv)) $
                                           parens (hsep (punctuate comma (fdocs ++ ndocs ++ pdocs))) <+>
                                           text "-> ..." -- todo: show nice mbResTp if present
                            infError range (text "no function" <+> Pretty.ppName penv name <+> text "is defined that matches the argument types" <->
                                         table (ctxTerm rangeContext ++
                                                [(text "inferred type", argsDoc)
                                                ,(text "candidates", align (tablex 0 (ppCandidates env  "" amb)))]
                                                ++
                                                (if (name == newName "+")
                                                  then [(text "hint", text "did you mean to use append (++)? (instead  of addition (+) )")]
                                                  else [])
                                               ))

                    _ -> do amb2 <- case mbInfoFilterAmb of
                                      Just infoFilterAmb -> lookupNameCtx infoFilterAmb name ctx range
                                      Nothing            -> return []
                            case amb2 of
                              (_:_)
                                -> infError range ((text "identifier" <+> Pretty.ppName penv name <+> text "cannot be found") <->
                                                   (text "perhaps you meant: " <.> ppOr penv (map fst amb2)))
                              _ -> infError range (text "identifier" <+> Pretty.ppName penv name <+> text "cannot be found")

        [(qname,info)]
           -> do -- when (not asPrefix) $  -- todo: check casing for asPrefix as well
                 checkCasing range name qname info
                 return (qname,infoType info,info)
        _  -> do env <- getEnv
                 infError range (text "identifier" <+> Pretty.ppName (prettyEnv env) name <+> text "is ambiguous" <.> ppAmbiguous env hintTypeSig matches)
  where
    hintTypeSig = "give a type annotation to the function parameters or qualify the name?"


----------------------------------------------------------------
-- Resolving of implicit expressions
----------------------------------------------------------------

-- An implicit expression is an expression that can be passed to an implicit parameter.
data ImplicitExpr = ImplicitExpr{ ieDoc        :: Doc,            -- pretty expression
                                  ieType       :: Type,           -- type of the expression
                                  ieExpr       :: Expr Type,      -- the expression
                                  ieDepth      :: Int,            -- depth of the recursive implicit arguments, e.g. depth `list/eq(_,_,?eq=int/eq)` is 2
                                  ieLocalRoots :: [Name]     -- the leaves of the expression are always names, this is the list of _local_ names at the leaves.
                                }

instance Pretty ImplicitExpr where
  pretty iexpr = ieDoc iexpr

-- prefer 1. most local roots, and 2. shortest chain of implicit parameters
ieCompare :: ImplicitExpr -> ImplicitExpr -> Ordering
ieCompare ie1 ie2
  = compare (-(length (ieLocalRoots ie1)), ieDepth ie1) (-(length (ieLocalRoots ie2)), ieDepth ie2)


-- lookup an application name `f(...)` where the name context usually contains (partially) inferred
-- argument types.
lookupAppNameX :: Bool -> Name -> NameContext -> Range ->
                   Inf (Either [(Name,NameInfo)] (Type,Expr Type,[((Name,Range),Expr Type, Doc)]))
lookupAppNameX allowDisambiguate name ctx range
  = do -- find all implicit solutions
       iapps <- if not (isConstructorName name)
                  then -- normal identifier
                       lookupAppNamesEx isInfoValFunExt name
                  else -- constructor application
                       do let cname = newCreatorName name
                          defName <- currentDefName
                          -- traceDefDoc $ \penv -> text "lookupAppName, constructor name:" <+> Pretty.ppName penv name <+> text "in definition" <+> Pretty.ppName penv defName
                          if (defName == unqualify cname || defName == nameCopy) -- a bit hacky, but ensure we don't call the creator function inside itself or the copy function
                            then lookupAppNamesEx isInfoCon name
                            else do iapps1 <- lookupAppNamesEx isInfoFun cname
                                    iapps2 <- lookupAppNamesEx isInfoCon name
                                    let cnames = [name | (_,(name,_,_,_)) <- iapps1]
                                        iapps3 = filter (\(_,(name,_,_,_)) -> not (newCreatorName name `elem` cnames)) iapps2
                                    return (iapps1 ++ iapps3)
       -- try to find a best candidate
       traceDefDoc $ \penv -> text "lookupAppName: disambiguate:" <+> (pretty allowDisambiguate) <->
                              indent 2 (vcat [(ieDoc iexpr) | (iexpr,_) <- iapps])
       case pick allowDisambiguate fst iapps of
          Right (imp,(qname,info,itp,iexprs))
            -> do -- traceDefDoc $ \penv -> text "resolved app name" <+> pretty imp
                  return (Right (itp, Var qname False range, [((iname,range),ieExpr iexpr,ieDoc iexpr) | (iname,iexpr) <- iexprs]))
          Left xs
            -> let matches = nubBy (\x y -> fst x == fst y) [(name,info) | (_,(name,info,_,_)) <- xs]
              in if (allowDisambiguate && not (null matches))
                    then do env <- getEnv
                            let hintQualify = "qualify the name to disambiguate it?"
                            infError range (text "identifier" <+> Pretty.ppName (prettyEnv env) name <+> text "is ambiguous" <.> ppAmbiguous env hintQualify matches)
                            return (Left matches)
                    else return (Left matches)
  where
    lookupAppNamesEx :: (NameInfo -> Bool) -> Name -> Inf [(ImplicitExpr,(Name,NameInfo,Rho,[(Name,ImplicitExpr)]))]
    lookupAppNamesEx infoFilter xname
      = do iapps <- lookupAppNames 0 -- False {- no bypass -}
                       (not allowDisambiguate) {- allow unitFunVal: at first when allowDisambiguate is False we like to see all possible instantations -}
                       infoFilter xname ctx range
           penv  <- getPrettyEnv
           return [(toImplicitAppExpr False penv "" xname range iapp, iapp) | iapp <- iapps]



-- resolve an implicit name to an expression
resolveImplicitNameX :: Name -> Type -> Range -> Inf (Expr Type, Doc)
resolveImplicitNameX name tp range
  = do iexprs <- lookupImplicitNames 0 isInfoValFunExt name (implicitTypeContext tp) range
       penv <- getPrettyEnv
       case pick True id iexprs of
         Right iexpr -> do -- traceDefDoc $ \penv -> text "resolved implicit" <+> Pretty.ppParam penv (name,tp) <+> text ", to" <+> pretty iexpr
                           let contextDoc = ieDoc iexpr
                           return (ieExpr iexpr, contextDoc)
         Left iexprs -> do let docs0 = map ieDoc iexprs
                           infError range
                              (text "cannot resolve implicit parameter" <->
                               table [(text "term",       docFromRange (Pretty.colors penv) range),
                                      (text "parameter",  text "?" <.> ppNameType penv (name,tp)),
                                      (text "candidates", if null docs0 then text "<none>"
                                                          else let docs = take 10 docs0 ++ (if length docs0 > 10 then [text "..."] else [])
                                                                in align (vcat docs)),
                                      (text "hint", text "add a (implicit) parameter to the function?")])
                           return (Var name False range, Lib.PPrint.empty)


-- pick the best match in a list of candidates
-- if allowDisambiguate is False, only one candidate is allowed, otherwise there
-- must be a "best" candidate (most local roots, shortest chain)
pick :: Bool -> (a -> ImplicitExpr) -> [a] -> Either [a] a
pick allowDisambiguate select xs
  = case xs of
      []  -> Left []
      [x] -> Right x
      _   -> if not allowDisambiguate
              then Left xs
              else case sortBy (\x y -> ieCompare (select x) (select y)) xs of
                      (x:y:_) | (ieCompare (select x) (select y) == LT) -> Right x
                      ys      -> Left ys




-- lookup an application name `f(...)` where the name context usually contains (partially) inferred
-- argument types.
lookupAppName :: Bool -> Name -> NameContext -> Range ->
                   Inf (Either [Doc] (Type,Expr Type,[((Name,Range),Expr Type, Doc)]))
lookupAppName allowDisambiguate name ctx range
  = do -- find all implicit solutions
       roots <- if not (isConstructorName name)
                  then -- normal identifier
                       return [(isInfoValFunExt,name,ctx,range)]
                  else -- constructor application
                       do let cname = newCreatorName name
                          defName <- currentDefName
                          -- traceDefDoc $ \penv -> text "lookupAppName, constructor name:" <+> Pretty.ppName penv name <+> text "in definition" <+> Pretty.ppName penv defName
                          if (defName == unqualify cname || defName == nameCopy) -- a bit hacky, but ensure we don't call the creator function inside itself or the copy function
                            then return [(isInfoCon,name,ctx,range)]
                            else return [(isInfoFun,cname,ctx,range),(isInfoCon,name,ctx,range)]

       res <- resolveImplicitArg allowDisambiguate
                                (not allowDisambiguate) {- allow unitFunVal: at first, when allowDisambiguate is False, we like to see all possible instantations -}
                                roots

       case res of
          Right iarg@(ImplicitArg qname _ rho iargs)
            -> do when (not (null iargs)) $
                    traceDefDoc $ \penv -> text "resolved app name with implicits:" <+> prettyImplicitArg penv iarg
                  penv <- getPrettyEnv
                  let implicits = [((pname,range),
                                     toImplicitArgExpr (endOfRange range) iarg,
                                     prettyImplicitAssign penv False "" pname iarg) | (pname, Done iarg) <- iargs]
                  return (Right (rho, Var qname False range, implicits))
          Left docs0
            -> if (allowDisambiguate && not (null docs0))
                then do env <- getEnv
                        infError range (text "identifier" <+> Pretty.ppName (prettyEnv env) name <+> text "is ambiguous" <->
                                        table [(text "candidates", if null docs0 then text "<none>"
                                                                    else let docs = take 10 docs0 ++ (if length docs0 > 10 then [text "..."] else [])
                                                                         in align (vcat docs)),
                                               (text "hint", text "qualify the name?")])
                        return (Left docs0)
                else return (Left docs0)


-- resolve an implicit name to an expression
resolveImplicitName :: Name -> Type -> Range -> Inf (Expr Type, Doc)
resolveImplicitName name tp range
  = do res <- resolveImplicitArg True {-disambiguate-} True {-allow unit fun val-}
                                  [(isInfoValFunExt, name, implicitTypeContext tp, range)]
       penv <- getPrettyEnv
       case res of
         Right iarg  -> do traceDefDoc $ \penv -> text "resolved implicit" <+> prettyImplicitAssign penv False "?" name iarg
                           return (toImplicitArgExpr range iarg, prettyImplicitArg penv iarg)
         Left docs0  -> do infError range
                              (text "cannot resolve implicit parameter" <->
                               table [(text "term",       docFromRange (Pretty.colors penv) range),
                                      (text "parameter",  text "?" <.> ppNameType penv (name,tp)),
                                      (text "candidates", if null docs0 then text "<none>"
                                                          else let docs = take 10 docs0 ++ (if length docs0 > 10 then [text "..."] else [])
                                                                in align (vcat docs)),
                                      (text "hint", text "add a (implicit) parameter to the function?")])
                           return (Var name False range, Lib.PPrint.empty)

-----------------------------------------------------------------------
-- Looking up application names and implicit names
-- This is done in a breadth-first search to reduce exponential search times
-----------------------------------------------------------------------

data ImplicitArg   = ImplicitArg{ iaName :: Name
                                , iaInfo :: NameInfo
                                , iaType :: Rho
                                , iaImplicitArgs :: [(Name, Partial)]
                                }

data Partial   = Step (Inf [ImplicitArg])
               | Done ImplicitArg


data Cost  = Least Int
           | Exact Int

instance Ord Cost where
  compare x y
    = case (x,y) of
        (Exact i, Exact j) -> compare i j
        (Exact i, Least j) -> LT
        (Least i, Exact j) -> GT
        (Least i, Least j) -> compare i j

instance Eq Cost where
  x == y  = (compare x y == EQ)

cadd x y
   = case (x,y) of
        (Exact i, Exact j) -> Exact (i + j)
        (Exact i, Least j) -> Least (i + j)
        (Least i, Exact j) -> Least (i + j)
        (Least i, Least j) -> Least (i + j)

csum xs
  = foldl' cadd (Exact 0) xs


-- Is an implicit arg fully evaluated?
isDone :: ImplicitArg -> Bool
isDone (ImplicitArg _ _ _ iargs)
  = all (\(pname,partial) -> case partial of
                              Done iarg  -> isDone iarg
                              Step _     -> False
        ) iargs


-- cost: chain depth + 100*qualified name (while locals cost zero)
implicitArgCost :: ImplicitArg -> Cost
implicitArgCost iarg
  = let base = if isQualified (iaName iarg) then 100 else 0
    in cadd (Exact base) (csum (map (partialCost . snd) (iaImplicitArgs iarg)))

partialCost :: Partial -> Cost
partialCost (Step inf)  = Least 1
partialCost (Done iarg)    = cadd (Exact 1) (implicitArgCost iarg)

-- let isLocal = not (isQualified iname)
--             withColor clr doc = color (clr (Pretty.colors penv)) doc
--             docName = (if (shorten && toImplicitParamName name == iname)  -- ?cmp = ?cmp
--                         then Lib.PPrint.empty
--                         else withColor colorImplicitParameter (text prefix <.> Pretty.ppNamePlain penv name <.> text "="))
--                       <.> withColor colorImplicitExpr (Pretty.ppNamePlain penv iname)

prettyImplicitArg :: Pretty.Env -> ImplicitArg -> Doc
prettyImplicitArg penv (ImplicitArg name info rho iargs)
  = let withColor clr doc = color (clr (Pretty.colors penv)) doc in
    withColor colorImplicitExpr (Pretty.ppNamePlain penv name) <.>
    if null iargs then Lib.PPrint.empty
                  else let fcount = case splitFunType rho of
                                      Just (ipars,_,_) -> let (fixed,_,_) = splitOptionalImplicit ipars
                                                          in length fixed
                                      _ -> 0 -- should never happen? (since we got implicit arguments)
                           docargs  = [prettyPartial penv pname partial | (pname,partial) <- iargs]
                           docfixed = [text "_" | _ <- [1..fcount]]
                       in parens (hcat (intersperse comma (docfixed ++ docargs)))

prettyPartial :: Pretty.Env -> Name -> Partial -> Doc
prettyPartial penv pname partial
  = case partial of
      Step _    -> Pretty.ppNamePlain penv pname <.> text "=" <.> text "..."
      Done iarg -> prettyImplicitAssign penv True "" pname iarg

prettyImplicitAssign :: Pretty.Env -> Bool -> String -> Name -> ImplicitArg -> Doc
prettyImplicitAssign penv shorten prefix pname iarg
  = let withColor clr doc = color (clr (Pretty.colors penv)) doc in
    withColor colorImplicitParameter ({- text prefix <.> -} Pretty.ppNamePlain penv pname) <.>
    (if shorten && pname == iaName iarg
       then Lib.PPrint.empty
       else text "=" <.> prettyImplicitArg penv iarg)



resolveImplicitArg :: Bool -> Bool -> [(NameInfo -> Bool, Name, NameContext, Range)] -> Inf (Either [Doc] (ImplicitArg))
resolveImplicitArg allowDisambiguate allowUnitFunVal roots
  = do candidates1 <- concatMapM (\(infoFilter,name,ctx,range) -> lookupImplicitArg allowUnitFunVal infoFilter name ctx range) roots
       let candidates2 = filter (not . existConCreator candidates1) candidates1
       resolveBest allowDisambiguate 0 candidates2
  where
    -- | always prefer a creator definition over a plain constructor if it exists
    existConCreator :: [ImplicitArg] -> ImplicitArg -> Bool
    existConCreator candidates (ImplicitArg name info _ _)
      = isInfoCon info && any (\iarg -> iaName iarg == cname) candidates
      where
        cname = newCreatorName name

resolveBest :: Bool -> Int -> [ImplicitArg] -> Inf (Either [Doc] ImplicitArg)
resolveBest allowDisambiguate depth candidates | depth > 5
  = do penv <- getPrettyEnv
       return (Left (map (prettyImplicitArg penv) candidates))
resolveBest allowDisambiguate depth candidates
  = case findBest allowDisambiguate candidates of
      Found iarg       -> assertion "Type.InferMonad.resolveBest: unresolved implicit" (isDone iarg) $
                          return (Right iarg)
      Continue sorted  -> do candidates' <- resolveStep sorted
                             resolveBest allowDisambiguate (depth + 1) candidates'
      _                -> do penv <- getPrettyEnv
                             return (Left (map (prettyImplicitArg penv) candidates))

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs


resolveStep :: [ImplicitArg] -> Inf [ImplicitArg]
resolveStep iargs
  = concatMapM step iargs
  where
    step :: ImplicitArg -> Inf [ImplicitArg]
    step iarg
      = if isDone iarg
          then return [iarg]
          else do let (pnames,partials) = unzip (iaImplicitArgs iarg)
                  yss <- sequence <$> mapM (\partial -> case partial of
                                                  Done arg -> resolveStep [arg]
                                                  Step inf -> inf
                                           ) partials
                  return [iarg{ iaImplicitArgs = zip pnames (map Done ys) } | ys <- yss]


data Select a  = Found a
               | None
               | Amb
               | Continue [a]

findBest :: Bool -> [ImplicitArg] -> Select (ImplicitArg)
findBest allowDisambiguate candidates
  = case candidates of
      []     -> None
      [iarg] -> if isDone iarg then Found iarg else Continue [iarg]
      _      -> if not allowDisambiguate
                  then if length (filter isDone candidates) > 1
                         then Amb
                         else Continue candidates
                  else case sortBy (\x y -> compare (implicitArgCost x) (implicitArgCost y)) candidates of
                         sorted@(x:ys) -> case implicitArgCost x of
                            (Least _) -> Continue sorted
                            (Exact i) -> let keep = filter (\y -> case implicitArgCost y of
                                                                        Exact j -> i == j
                                                                        Least j -> i >= j) sorted
                                         in case keep of
                                              [_]   -> Found x
                                              _     -> if all (\y -> implicitArgCost y == Exact i) keep
                                                         then Amb else Continue keep


-- Lookup application name `f` in an expression `f(...)` where the name  context usually contains
-- types of (partially) inferred (fixed) arguments.
-- Returns list of resolved names together with any required implicit arguments.
lookupImplicitArg :: Bool -> (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [ImplicitArg]
lookupImplicitArg allowUnitFunVal infoFilter name ctx range
  = do -- traceDefDoc $ \penv -> text "lookupImplicitArg:" <+> pretty name
       candidates0 <- lookupNames infoFilter name ctx range
       candidates1 <- case ctx of
                        CtxType expect | allowUnitFunVal && not (isFun expect)
                           -> lookupNames infoFilter name (CtxFunTypes False [] [] (Just expect)) range
                        _  -> return []
       return (map toImplicitArg (candidates0 ++ candidates1))
  where
    toImplicitArg :: (Name,NameInfo,Rho) -> ImplicitArg
    toImplicitArg (iname,info,itp {- instantiated type -})
      = let iargs = case splitFunType itp of
                      Just (ipars,ieff,iresTp)  | any Op.isOptionalOrImplicit ipars
                        -- resolve further implicit parameters
                        -> map resolveImplicit (implicitsToResolve ipars)
                      _ -> []
        in (ImplicitArg iname info itp iargs)

    implicitsToResolve :: [(Name,Type)] -> [(Name,Type)]
    implicitsToResolve ipars
      = -- only return implicits that were not already given explicitly by the user (in `named`)
        let (_,_,implicits)  = splitOptionalImplicit ipars
            alreadyGiven     = case ctx of
                                  CtxFunTypes partial fixed named mbResTp
                                    -> map fst named
                                  CtxFunArgs n named mbResTp
                                    -> named
                                  _ -> []
        in filter (\(name,_) -> not (name `elem` alreadyGiven)) implicits

    resolveImplicit :: (Name,Type) -> (Name,Partial)
    resolveImplicit (pname,ptp)
      = -- recursively solve further implicits (but return the computation to allow for breath first search)
        let (pnameName,pnameExpr) = splitImplicitParamName pname
        in  (pnameName, Step $
                        lookupImplicitArg True {- allow unit val -}
                          infoFilter pnameExpr
                          (implicitTypeContext ptp) (endOfRange range)) -- use end of range to deprioritize with hover info


toImplicitArgExpr :: Range -> ImplicitArg -> Expr Type
toImplicitArgExpr range (ImplicitArg iname info itp iargs)
      = case iargs of
          [] -> Var iname False range
          _  -> case splitFunType itp of
                  Just (ipars,ieff,iresTp) | any Op.isOptionalOrImplicit ipars -- eta-expand
                    -- eta-expand and resolve further implicit parameters
                    -- todo: eta-expansion may become part of subsumption?
                    ->  let (fixed,opt,implicits) = splitOptionalImplicit ipars in
                        assertion "Type.InferMonad.toImplicitAppExpr" (length implicits == length iargs) $
                        let nameFixed    = [makeHiddenName "arg" (newName ("x" ++ show i)) | (i,_) <- zip [1..] fixed]
                            argsFixed    = [(Nothing,Var name False range) | name <- nameFixed]
                            argsImplicit = [(Just (pname,range), toImplicitArgExpr (endOfRange range) iarg) | (pname,Done iarg) <- iargs]
                            etaTp        = TFun fixed ieff iresTp
                            eta          = (if null fixed then id
                                            else \body -> Lam [ValueBinder name Nothing Nothing range range | name <- nameFixed] body range)
                                              (App (Var iname False range)
                                                      (argsFixed ++ argsImplicit)
                                                      range)
                        in eta
                  _ -> failure ("Type.InferMonad.toImplicitAppExpr: illegal type for implicit? " ++ show range ++ ", " ++ show iname)


-----------------------------------------------------------------------
-- Looking up application names and implicit names is mutually recursive
-----------------------------------------------------------------------

-- lookup an implicit name, potentially eta-expanding functions to provide implicit arguments recursively
lookupImplicitNames :: Int -> (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [ImplicitExpr]
lookupImplicitNames recurseDepth infoFilter name ctx range
  = do traceDefDoc $ \penv -> text "lookupImplicitNames:" <+> pretty recurseDepth <+> pretty name
       candidates <- lookupAppNames (recurseDepth+1) {- True {-allow bypass-} -} True {-allow unitFunVal's-} infoFilter name ctx range
       penv <- getPrettyEnv
       return (map (toImplicitAppExpr (recurseDepth > 0) penv "?" name range) candidates)

toImplicitAppExpr :: Bool -> Pretty.Env -> String -> Name -> Range -> (Name,NameInfo,Rho,[(Name,ImplicitExpr)]) -> ImplicitExpr
toImplicitAppExpr shorten penv prefix name range (iname,info,itp,iargs)
      = let isLocal = not (isQualified iname)
            withColor clr doc = color (clr (Pretty.colors penv)) doc
            docName = (if (shorten && toImplicitParamName name == iname)  -- ?cmp = ?cmp
                        then Lib.PPrint.empty
                        else withColor colorImplicitParameter (text prefix <.> Pretty.ppNamePlain penv name <.> text "="))
                      <.> withColor colorImplicitExpr (Pretty.ppNamePlain penv iname)
            -- coreVar = coreExprFromNameInfo iname info
        in case iargs of
            [] -> ImplicitExpr docName itp (Var iname False range) 1 (if isLocal then [iname] else [])
            _  -> case splitFunType itp of
                    Just (ipars,ieff,iresTp) | any Op.isOptionalOrImplicit ipars -- eta-expand
                      -- eta-expand and resolve further implicit parameters
                      -- todo: eta-expansion may become part of subsumption?
                      ->  let (fixed,opt,implicits) = splitOptionalImplicit ipars in
                          assertion "Type.InferMonad.toImplicitAppExpr" (length implicits == length iargs) $
                          let nameFixed    = [makeHiddenName "arg" (newName ("x" ++ show i)) | (i,_) <- zip [1..] fixed]
                              argsFixed    = [(Nothing,Var name False range) | name <- nameFixed]
                              etaTp         = TFun fixed ieff iresTp
                              eta          = (if null fixed then id
                                              else \body -> Lam [ValueBinder name Nothing Nothing range range | name <- nameFixed] body range)
                                                (App (Var iname False range)
                                                    (argsFixed ++
                                                      [(Just (pname,range),ieExpr iexpr) | (pname,iexpr) <- iargs])
                                                    range)

                              depth         = 1 + sum [ieDepth iexpr | (_,iexpr) <- iargs]
                              localRoots    = if null iargs && isLocal
                                                then [iname]
                                                else nub (concatMap (ieLocalRoots . snd) iargs)
                              doc           = docName <.> withColor colorImplicitExpr (
                                                            parens (hcat (intersperse comma ([text "_" | _ <- fixed] ++ [ieDoc iexpr | (_,iexpr) <- iargs])))
                                                          )
                          in ImplicitExpr doc etaTp eta depth localRoots
                    _ -> failure ("Type.InferMonad.toImplicitAppExpr: illegal type for implicit? " ++ show range ++ ", " ++ show (ppNameType penv (name,itp)))

-- Lookup application name `f` in an expression `f(...)` where the name  context usually contains
-- types of (partially) inferred (fixed) arguments.
-- Returns list of resolved names together with any required implicit arguments.
lookupAppNames :: Int -> Bool -> (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [(Name,NameInfo,Rho,[(Name,ImplicitExpr)])]
lookupAppNames recurseDepth allowUnitFunVal infoFilter name ctx range | recurseDepth > 2
  = do traceDefDoc $ \penv -> text "lookupAppNames:" <+> pretty recurseDepth <+> pretty name <+> text "stop recursion"
       return []
lookupAppNames recurseDepth allowUnitFunVal infoFilter name ctx range
  = do traceDefDoc $ \penv -> text "lookupAppNames:" <+> pretty recurseDepth <+> pretty name
       candidates0 <- lookupNames infoFilter name ctx range
       candidates1 <- case ctx of
                        CtxType expect | allowUnitFunVal && not (isFun expect)
                           -> lookupNames infoFilter name (CtxFunTypes False [] [] (Just expect)) range
                        _  -> return []
       concatMapM lookupImplicits (candidates0 ++ candidates1)
  where
    concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
    concatMapM f xs = concat <$> mapM f xs

    lookupImplicits :: (Name,NameInfo,Rho) -> Inf [(Name,NameInfo,Rho,[(Name,ImplicitExpr)])]
    lookupImplicits (iname,info,itp {- instantiated type -})
      = do traceDefDoc (\penv -> text "lookupAppNames" <+> pretty recurseDepth <+> text "found implicit: " <+> ppNameType penv (iname,itp))
           iargss <-  case splitFunType itp of
                        Just (ipars,ieff,iresTp)  | any Op.isOptionalOrImplicit ipars
                          -- resolve further implicit parameters
                          -> resolveImplicits (implicitsToResolve ipars)
                        _ -> return [[]]
           return [(iname,info,itp,iargs) | iargs <- iargss]

    implicitsToResolve :: [(Name,Type)] -> [(Name,Type)]
    implicitsToResolve ipars
      = -- only return implicits that were not already given explicitly by the user (in `named`)
        let (_,_,implicits)  = splitOptionalImplicit ipars
            alreadyGiven     = case ctx of
                                  CtxFunTypes partial fixed named mbResTp
                                    -> map fst named
                                  CtxFunArgs n named mbResTp
                                    -> named
                                  _ -> []
        in filter (\(name,_) -> not (name `elem` alreadyGiven)) implicits

    resolveImplicits :: [(Name,Type)] -> Inf [[(Name,ImplicitExpr)]] -- all possible solutions (up to recursion depth)
    resolveImplicits []  = return [[]]
    resolveImplicits implicits
      = -- recursively lookup required implicit arguments
        sequence <$> -- cartesian product of all possible argument
        mapM (\(pname,ptp) ->
                let (pnameName,pnameExpr) = splitImplicitParamName pname
                in do iexprs <- lookupImplicitNames recurseDepth
                                    infoFilter (pnameExpr)
                                    (implicitTypeContext ptp) (endOfRange range) -- use end of range to deprioritize with hover info
                      return [(pnameName,iexpr) | iexpr <- iexprs]
            ) implicits


----------------------------------------------------------------
-- Lookup names
----------------------------------------------------------------

lookupFunName :: Name -> Maybe (Type,Range) -> Range -> Inf (Maybe (Name,Type,NameInfo))
lookupFunName name mbType range
  = do matches <- lookupNameCtx isInfoFun name (maybeRToContext mbType) range
       case matches of
        []   -> return Nothing
        [(name,info)]  -> return (Just (name,infoType info,info))
        _    -> do env <- getEnv
                   infError range (text "identifier" <+> Pretty.ppName (prettyEnv env) name <+> text "is ambiguous" <.> ppAmbiguous env hintQualify matches)
  where
    hintQualify = "qualify the name to disambiguate it?"

lookupNameCtx :: (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [(Name,NameInfo)]
lookupNameCtx infoFilter name ctx range
  = do candidates <- lookupNames infoFilter name ctx range
       -- traceDefDoc $ \penv -> text " lookupNameCtx:" <+> ppNameCtx penv (name,ctx) <+> colon
       --                       <+> list [Pretty.ppParam penv (name,rho) | (name,info,rho) <- candidates]
       return [(name,info) | (name,info,_) <- candidates]


-- lookup names that match the given name context
-- `allowBypass` allows looking beyond a local name and return global matches as well.
-- generally, a local name that matches the type is never bypassed though.
-- However, `allowTypeBypass` even allows looking beyond a local name that fits the type
-- just to see if there are any possible ambiguities -- this is used for function applications
-- to stop inferring arguments whenever there is no more ambiguity (as a local may at first match
-- the type for some polymorphic function but no longer after some arguments have been inferred)
lookupNames :: (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [(Name,NameInfo,Rho)]
lookupNames infoFilter name ctx range
  = do env <- getEnv
       -- traceDoc $ \penv -> text " lookupNames:" <+> ppNameCtx penv (name,ctx)
       locals <- lookupLocalName infoFilter name
       if (not (null locals))
         then -- a local name that matches exactly was found; use it always
              filterMatchNameContextEx range ctx locals
         else  do -- otherwise consider globals as well, and also locally qualified implicits in the local scope
                  ilocals <- lookupLocalName infoFilter (toImplicitParamName name)  -- consider "@implicit/<name>"
                  let globals0 = ilocals ++
                                  filter (infoFilter . snd) (gammaLookup name (gamma env))
                  --  traceDefDoc $ \penv -> text " lookupNames:" <+> ppNameCtx penv (name,ctx)
                  --     <+> text ", locals0:" <+> list [Pretty.ppName penv (name) | (name,info) <- locals0]
                  --     <+> text ", globals0:" <+> list [Pretty.ppName penv (name) | (name,info) <- globals']
                  globals1 <- filterMatchNameContextEx range ctx globals0
                  --  traceDefDoc $ \penv -> text " lookupNames:" <+> ppNameCtx penv (name,ctx)
                  --     <+> text ", locals:" <+> list [Pretty.ppParam penv (name,rho) | (name,info,rho) <- locals1]
                  --     <+> text ", globals:" <+> list [Pretty.ppParam penv (name,rho) | (name,info,rho) <- globals1]
                  return globals1

lookupLocalName :: (NameInfo -> Bool) -> Name -> Inf [(Name,NameInfo)]
lookupLocalName infoFilter name
  = do env <- getEnv
       mod <- getModuleName
       let locname = if (qualifier name == mod) then unqualify name        -- could use a qualified name to refer to a recursive function itself
                                                else requalifyLocally name -- or refer to an implicit `implicit/x`
       case infgammaLookupX locname (infgamma env) of
                     Just info | infoFilter info -> do sinfo <- subst info
                                                       let lname = infoCanonicalName name info
                                                       return [(lname,sinfo)]
                     _ -> return []


filterMatchNameContext :: Range -> NameContext -> [(Name,NameInfo)] -> Inf [(Name,NameInfo)]
filterMatchNameContext range ctx candidates
  = do xs <- filterMatchNameContextEx range ctx candidates
       return [(name,info) | (name,info,_) <- xs]

filterMatchNameContextEx :: Range -> NameContext -> [(Name,NameInfo)] -> Inf [(Name,NameInfo,Rho)]
filterMatchNameContextEx range ctx candidates
  = case ctx of
      CtxNone         -> return [(name,info,infoType info) | (name,info) <- candidates]
      CtxType expect  -> do mss <- mapM (matchType expect) candidates
                            return (concat mss)
      CtxFunArgs n named mbResTp
                      -> do mss <- mapM (matchNamedArgs n named mbResTp) candidates
                            return (concat mss)
      CtxFunTypes partial fixed named mbResTp
                      -> do mss <- mapM (matchArgs partial fixed named mbResTp) candidates
                            return (concat mss)
  where
    matchType :: Type -> (Name,NameInfo) -> Inf [(Name,NameInfo,Rho)]
    matchType expect (name,info)
      = do free <- freeInGamma
           res <- runUnify (subsume range free expect (infoType info))
           case res of
             (Right (_,rho,_,_),_)  -> return [(name,info,rho)]
             (Left _,_)             -> return []

    matchNamedArgs :: Int -> [Name] -> Maybe Type -> (Name,NameInfo) -> Inf [(Name,NameInfo,Rho)]
    matchNamedArgs n named mbResTp (name,info)
      = do free <- freeInGamma
           res <- runUnify (matchNamed range free (infoType info) n named mbResTp)
           case res of
             (Right rho,_)  -> return [(name,info,rho)]
             (Left _,_)     -> return []

    matchArgs :: Bool -> [Type] -> [(Name,Type)] -> Maybe Type -> (Name,NameInfo) -> Inf [(Name,NameInfo,Rho)]
    matchArgs matchSome fixed named mbResTp (name,info)
      = do free <- freeInGamma
            --  traceDefDoc $ \penv -> text "  match fixed:" <+> list [Pretty.ppType penv fix | fix <- fixed]
            --                               <+> text ", named" <+> list [Pretty.ppParam penv nametp | nametp <- named]
            --                               <+> text "on" <+> Pretty.ppParam penv (name,infoType info)
           res <- runUnify (matchArguments matchSome range free (infoType info) fixed named mbResTp)
           case res of
             (Right rho,_) -> return [(name,info,rho)]
             (Left _,_)    -> return []



----------------------------------------------------------------
-- Name Context
----------------------------------------------------------------

data NameContext
  = CtxNone       -- ^ just a name
  | CtxType Type  -- ^ a name that can appear in a context with this type
  | CtxFunArgs  Int [Name] (Maybe Type)         -- ^ function name with @n@ fixed arguments and followed by the given named arguments and a possible result type.
  | CtxFunTypes Bool [Type] [(Name,Type)] (Maybe Type)  -- ^ are only some arguments supplied?, function name, with fixed and named arguments, maybe a (propagated) result type
  deriving (Show)

ppNameContext :: Pretty.Env -> NameContext -> Doc
ppNameContext penv ctx
  = case ctx of
      CtxNone -> text "CtxNone"
      CtxType tp -> text "CtxType" <+> Pretty.ppType penv tp
      CtxFunArgs n names mbResTp -> text "CtxFunArgs" <+> pretty n <+> list [Pretty.ppName penv name | name <- names] <+> ppMbType penv mbResTp
      CtxFunTypes some fixed named mbResTp
        -> text "CtxFunTypes" <+> pretty some <+> list [Pretty.ppType penv atp | atp <- fixed]
           <+> list [Pretty.ppParam penv nt | nt <- named] <+> ppMbType penv mbResTp
  where
    ppMbType penv Nothing   = Lib.PPrint.empty
    ppMbType penv (Just tp) = text ":" <+> Pretty.ppType penv tp

ppNameCtx :: Pretty.Env -> (Name,NameContext) -> Doc
ppNameCtx penv (name,ctx) = Pretty.ppName penv name <+> text ":" <+> ppNameContext penv ctx


-- Create a name context where the argument count is known (and perhaps some named arguments)
fixedCountContext :: Maybe (Type,Range) -> Int -> [Name] -> NameContext
fixedCountContext propagated fixedCount named
  = CtxFunArgs fixedCount named (fmap fst propagated)

-- A fixed argument that has been inferred
type FixedArg = (Range,Type,Effect,Core.Expr)

-- A context where some fixed arguments have been inferred
fixedContext :: Maybe (Type,Range) -> [(Int,FixedArg)] -> Int -> [Name] -> Inf NameContext
fixedContext propagated fresolved fixedCount named
  = do fargs <- fixedGuessed fresolved
       nargs <- namedGuessed
       return (CtxFunTypes (fixedCount > length fresolved) fargs nargs (fmap fst propagated))
  where
    tvars :: Int -> Inf [Type]
    tvars n  = mapM (\_ -> Op.freshTVar kindStar Meta) [1..n]

    fixedGuessed :: [(Int,FixedArg)] -> Inf [Type]
    fixedGuessed xs   = fill 0 (sortBy (comparing fst) xs)
                      where
                        fill j []  = tvars (fixedCount - j)
                        fill j ((i,(_,tp,_,_)):rest)
                          = do post <- fill (i+1) rest
                               pre  <- tvars (i - j)
                               stp  <- subst tp
                               return (pre ++ [stp] ++ post)

    namedGuessed :: Inf [(Name,Type)]
    namedGuessed
      = mapM (\name -> do{ tv <- Op.freshTVar kindStar Meta; return (name,tv) }) named

implicitTypeContext :: Type -> NameContext
implicitTypeContext tp
  = case splitFunType tp of
      Just (ppars,peff,prestp) -> CtxFunTypes False (map snd ppars) [] (Just prestp) -- can handle further implicits better
      _                        -> CtxType tp

maybeToContext :: Maybe Type -> NameContext
maybeToContext mbType
  = case mbType of
      Just tp -> CtxType tp
      Nothing -> CtxNone

maybeRToContext :: Maybe (Type,Range) -> NameContext
maybeRToContext mbTypeRange
  = maybeToContext (fmap fst mbTypeRange)



----------------------------------------------------------------
-- Error Helpers
----------------------------------------------------------------

checkCasingOverlaps :: Range -> Name -> [(Name,NameInfo)] -> Inf ()
checkCasingOverlaps range name matches
  = -- this is called when various definitions (possibly from different modules) match with a name
    -- we could check here that all these definitions agree on the casing
    -- .. but I think it is better to only complain if the actual definition
    -- used has a different casing to reduce potential conflicts between modules
    return ()

checkCasingOverlap :: Range -> Name -> Name -> NameInfo -> Inf ()
checkCasingOverlap range name qname info
  = do case caseOverlaps name qname info of
         Just qname1
           -> do env <- getEnv
                 infError range (text (infoElement info) <+> Pretty.ppName (prettyEnv env) (unqualify name) <+> text "is already in scope with a different casing as" <+> Pretty.ppName (prettyEnv env) (importsAlias qname1 (imports env)))
         _ -> return ()

checkCasing :: Range -> Name -> Name -> NameInfo -> Inf ()
checkCasing range name qname info
  = do case caseOverlaps name qname info of
         Nothing -> return ()
         Just qname1
          -> do env <- getEnv
                infError range (text (infoElement info) <+> Pretty.ppName (prettyEnv env) (unqualify name) <+> text "should be cased as" <+> Pretty.ppName (prettyEnv env) (importsAlias qname1 (imports env)))


caseOverlaps :: Name -> Name -> NameInfo -> (Maybe Name)
caseOverlaps name qname info
  = let qname1 = case info of
                   InfoImport{infoAlias = alias} -> alias
                   _                             -> qname
    in if not (isLocallyQualified qname) && -- TODO: fix casing check for internally qualified names
          (nameCaseOverlap ((if isQualified name then id else unqualify) ({- nonCanonicalName -} qname1)) name)
        then Just qname1
        else Nothing

ppOr :: Pretty.Env -> [Name] -> Doc
ppOr env []     = Lib.PPrint.empty
ppOr env [name] = Pretty.ppName env name
ppOr env names  = hcat (map (\name -> Pretty.ppName env name <.> text ", ") (init names)) <+> text "or" <+> Pretty.ppName env (last names)


ppAmbiguous :: Env -> String -> [(Name,NameInfo)] -> Doc
ppAmbiguous env hint infos
  = text ". Possible candidates: " <-> table (ppCandidates env hint infos)

ppCandidates :: Env -> String -> [(Name,NameInfo)] -> [(Doc,Doc)]
ppCandidates env hint nameInfos
   = let penv = prettyEnv env
         modName = context env
         n = 10
         sorted      = sortBy (\(name1,info1) (name2,info2) ->
                                if (qualifier name1 == modName && qualifier name2 /= modName)
                                 then LT
                                else if (qualifier name1 /= modName && qualifier name2 == modName)
                                 then GT
                                else compare (not (isRho (infoType info1))) (not (isRho (infoType info2)))
                              ) nameInfos
         (defs,rest) = splitAt n sorted
     in (if null rest
          then map (ppNameInfo env) defs
          else map (ppNameInfo env) (init defs) ++ [(text "...", text "or" <+> pretty (length rest + 1) <+> text "other definitions")])
        ++
        (if (null hint) then [] else [(text "hint",text hint)])

ppNameInfo env (name,info)
  = (Pretty.ppName (prettyEnv env) (importsAlias name (imports env)), Pretty.ppType (prettyEnv env) (infoType info))



{--------------------------------------------------------------------------
  Inference monad
--------------------------------------------------------------------------}

data Inf a  = Inf (Env -> St -> Res a)

data Res a  = Ok !a !St ![(Range,Doc)]
            | Err !(Range,Doc) ![(Range,Doc)]

data Env    = Env{ prettyEnv :: !Pretty.Env
                 , context  :: !Name  -- | current module name
                 , currentDef :: !Name
                 , namedLam :: !Bool
                 , types :: !Newtypes
                 , synonyms :: !Synonyms
                 , gamma :: !Gamma
                 , infgamma :: !InfGamma
                 , imports :: !ImportMap
                 , returnAllowed :: !Bool
                 , inLhs :: !Bool
                 , hiddenTermDoc :: Maybe (Range,Doc)
                 }
data St     = St{ uniq :: !Int, sub :: !Sub, preds :: ![Evidence], holeAllowed :: !Bool, mbRangeMap :: Maybe RangeMap }


runInfer :: Pretty.Env -> Maybe RangeMap -> Synonyms -> Newtypes -> ImportMap -> Gamma -> Name -> Int -> Inf a -> Error b (a,Int,Maybe RangeMap)
runInfer env mbrm syns newTypes imports assumption context unique (Inf f)
  = case f (Env env context (newName "") False newTypes syns assumption infgammaEmpty imports False False Nothing)
           (St unique subNull [] False mbrm) of
      Err err warnings -> addWarnings warnings (errorMsg (ErrorType [err]))
      Ok x st warnings -> addWarnings warnings (ok (x, uniq st, (sub st) |-> mbRangeMap st))


zapSubst :: Inf ()
zapSubst
  = do env <- getEnv
       assertion "not an empty infgamma" (infgammaIsEmpty (infgamma env)) $
        do updateSt (\st -> assertion "no empty preds" (null (preds st)) $
                            st{ sub = subNull, preds = [], mbRangeMap = (sub st) |-> mbRangeMap st } ) -- this can be optimized further by splitting the rangemap into a 'substited part' and a part that needs to be done..
           return ()

instance Functor Inf where
  fmap f (Inf i)  = Inf (\env st -> case i env st of
                                      Ok x st1 w -> Ok (f x) st1 w
                                      Err err w  -> Err err w)

instance Applicative Inf where
  pure x = Inf (\env st -> Ok x st [])
  (<*>)  = ap

instance Monad Inf where
  -- return = pure
  (Inf i) >>= f   = Inf (\env st0 -> case i env st0 of
                                       Ok x st1 w1 -> case f x of
                                                        Inf j -> case j env st1 of
                                                                   Ok y st2 w2 -> Ok y st2 (w1++w2)
                                                                   Err err w2 -> Err err (w1++w2)
                                       Err err w -> Err err w)

tryRun :: Inf a -> Inf (Maybe a)
tryRun (Inf i) = Inf (\env st -> case i env st of
                                   Ok x st1 w -> Ok (Just x) st1 w
                                   Err err w  -> Ok Nothing st [])

instance HasUnique Inf where
  updateUnique f  = Inf (\env st -> Ok (uniq st) st{uniq = f (uniq st)} [])

getEnv :: Inf Env
getEnv
  = Inf (\env st -> Ok env st [])

withEnv :: (Env -> Env) -> Inf a -> Inf a
withEnv f (Inf i)
  = Inf (\env st -> i (f env) st)

updateSt :: (St -> St) -> Inf St
updateSt f
  = Inf (\env st -> Ok st (f st) [])

infError :: Range -> Doc -> Inf a
infError range doc
  = do addRangeInfo range (Error doc)
       Inf (\env st -> Err (range,doc) [])

infWarning :: Range -> Doc -> Inf ()
infWarning range doc
  = do addRangeInfo range (Warning doc)
       Inf (\env st -> Ok () st [(range,doc)])

getPrettyEnv :: Inf Pretty.Env
getPrettyEnv
  = do env <- getEnv
       return (prettyEnv env)

lookupSynonym :: Name -> Inf (Maybe SynInfo)
lookupSynonym name
  = do env <- getEnv
       return (synonymsLookup name (synonyms env) )

addRangeInfo :: Range -> RangeInfo -> Inf ()
addRangeInfo rng info
  = Inf (\env st -> Ok () (st{ mbRangeMap = case mbRangeMap st of { Just rm -> Just (rangeMapInsert rng info rm); other -> other }}) [])

{--------------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------------}

getSt :: Inf St
getSt
  = updateSt id

setSt :: St -> Inf St
setSt st
  = updateSt (const st)

allowReturn :: Bool -> Inf a -> Inf a
allowReturn allow inf
  = withEnv (\env -> env{ returnAllowed = allow }) inf

withLhs :: Inf a -> Inf a
withLhs inf
  = withEnv (\env -> env{ inLhs = True }) inf

withHiddenTermDoc :: Range -> Doc -> Inf a -> Inf a
withHiddenTermDoc range doc inf
  = withEnv (\env -> env{ hiddenTermDoc = Just (range,doc) }) inf

isLhs :: Inf Bool
isLhs
  = do env <- getEnv
       return (inLhs env)

isReturnAllowed :: Inf Bool
isReturnAllowed
  = do env <- getEnv
       return (returnAllowed env)

getTermDoc :: String -> Range -> Inf (Doc,Doc)
getTermDoc term range
  = do env <- getEnv
       case hiddenTermDoc env of
         Just (rng,doc) -- | range == rng
           -> return (text "implicit" <+> text term, doc)
         _ -> return (text term, docFromRange (Pretty.colors (prettyEnv env)) range)

useHole :: Inf Bool
useHole
  = do st0 <- updateSt (\st -> st{ holeAllowed = False } )
       return (holeAllowed st0)

disallowHole :: Inf a -> Inf a
disallowHole action
  = do st0 <- updateSt(\st -> st{ holeAllowed = False })
       let prev = holeAllowed st0
       x <- action
       updateSt(\st -> st{ holeAllowed = prev })
       return x

allowHole :: Inf a -> Inf (a,Bool {- was the hole used? -})
allowHole action
  = do st0 <- updateSt(\st -> st{ holeAllowed = True })
       let prev = holeAllowed st0
       x <- action
       st1 <- updateSt(\st -> st{ holeAllowed = prev })
       return (x,not (holeAllowed st1))



getSub :: Inf Sub
getSub
  = do st <- getSt
       return (sub st)

subst :: HasTypeVar a => a -> Inf a
subst x
  = do sub <- getSub
       return (sub |-> x)

extendSub :: Sub -> Inf ()
extendSub s
  = do -- trace ("Type.InferMonad.extendSub: " ++ show (subList s)) $
       updateSt (\st -> st{ sub = s @@ (sub st) })
       return ()

substWatch :: Inf a -> Inf (Bool,a)
substWatch inf
  = do sub1 <- getSub
       x <- inf
       sub2 <- getSub
       return (subCount sub1 /= subCount sub2, x)


getGamma :: Inf Gamma
getGamma
  = do env <- getEnv
       return (gamma env)

extendGammaCore :: Bool -> [Core.DefGroup] -> Inf a -> Inf (a)
extendGammaCore isAlreadyCanonical [] inf
  = inf
extendGammaCore isAlreadyCanonical (coreGroup:coreDefss) inf
  = extendGamma isAlreadyCanonical (nameInfos coreGroup) (extendGammaCore isAlreadyCanonical coreDefss inf)
  where
    nameInfos (Core.DefRec defs)    = map coreDefInfoX defs
    nameInfos (Core.DefNonRec def)
      = [coreDefInfoX def]  -- used to be coreDefInfo

-- Specialized for recursive defs where we sometimes get InfoVal even though we want InfoFun? is this correct for the csharp backend?
coreDefInfoX def@(Core.Def name tp expr vis sort inl nameRng doc)
  = (name {- nonCanonicalName name -}, createNameInfoX Public name sort nameRng tp doc)

-- extend gamma with qualified names
extendGamma :: Bool -> [(Name,NameInfo)] -> Inf a -> Inf (a)
extendGamma isAlreadyCanonical defs inf
  = do env <- getEnv
       (gamma') <- extend (prettyEnv env) (context env) defs (gamma env)
       withEnv (\env -> env{ gamma = gamma' }) inf
  where
    extend penv ctx [] (gamma)
      = return (gamma)
    extend penv ctx ((name,info):rest) (gamma)
      = do let matches = gammaLookup name gamma
               localMatches = [(qname,info) | (qname,info) <- matches, not (isInfoImport info),
                                              qualifier qname == ctx || qualifier qname == nameNil,
                                              unqualify name == unqualify qname,
                                              isSameNamespace qname name ]
           case localMatches of
             ((qname,qinfo):_) -> infError (infoRange info) (text "definition" <+> Pretty.ppName penv name <+>
                                                             text "is already defined in this module, at" <+> text (show (rangeStart (infoRange qinfo))) <->
                                                             text "hint: use a local qualifier?")
             [] -> return ()
           extend penv ctx rest (gammaExtend name info gamma)
           {-
           mapM (checkNoOverlap ctx name info) localMatches
           trace (" extend gamma: " ++ show (name,info)) $ return ()
           let (cinfo)
                   = -- if null localMatches then (info) else
                    if (isAlreadyCanonical) then info else
                       let cname = canonicalName (length localMatches) (if isQualified name then name else qualify ctx name)
                       in case info of
                            InfoVal{} -> info{ infoCName = cname }  -- during recursive let's we use InfoVal sometimes for functions..
                            InfoFun{} -> info{ infoCName = cname }
                            InfoExternal{} -> info{ infoCName = cname }
                            _ -> info
           -- Lib.Trace.trace (" extend gamma: " ++ show (pretty name, pretty (infoType info), show cinfo) ++ " with " ++ show (infoCanonicalName name cinfo) ++ " (matches: " ++ show (length matches,ctx,map fst matches)) $
           extend ctx rest (gammaExtend name cinfo gamma)
           -}


    checkNoOverlap :: Name -> Name -> NameInfo -> (Name,NameInfo) -> Inf ()
    checkNoOverlap ctx name info (name2,info2)
      = do checkCasingOverlap (infoRange info) name name2 info
           free <- freeInGamma
           res  <- runUnify (overlaps (infoRange info) free (infoType info) (infoType info2))
           case fst res of
            Right _ ->
              do env <- getEnv
                 let [nice1,nice2] = Pretty.niceTypes (prettyEnv env) [infoType info,infoType info2]
                     (_,_,rho1)    = splitPredType (infoType info)
                     (_,_,rho2)    = splitPredType (infoType info2)
                     valueType     = not (isFun rho1 && isFun rho2)
                 if (isFun rho1 && isFun rho2)
                  then infError (infoRange info) (text "definition" <+> Pretty.ppName (prettyEnv env) name <+> text "overlaps with an earlier definition of the same name" <->
                                                  table ([(text "type",nice1)
                                                         ,(text "overlaps",nice2)
                                                         ,(text "because", text "definitions with the same name must differ on the argument types")])
                                                 )
                  else infError (infoRange info) (text "definition" <+> Pretty.ppName (prettyEnv env) name <+> text "is already defined in this module" <->
                                                  text "because: only functions can have overloaded names")
            Left _ -> return ()


extendInfGammaCore :: Bool -> [Core.DefGroup] -> Inf a -> Inf a
extendInfGammaCore topLevel [] inf
  = inf
extendInfGammaCore topLevel (coreDefs:coreDefss) inf
  = extendInfGammaEx topLevel [] (extracts coreDefs) (extendInfGammaCore topLevel coreDefss inf)
  where
    extracts (Core.DefRec defs) = map extract defs
    extracts (Core.DefNonRec def) = [extract def]
    extract def
      = coreDefInfo def -- (Core.defName def,(Core.defNameRange def, Core.defType def, Core.defSort def))

extendInfGamma :: [(Name,NameInfo)] -> Inf a -> Inf a
extendInfGamma tnames inf
  = extendInfGammaEx False [] tnames inf

extendInfGammaEx :: Bool -> [Name] -> [(Name,NameInfo)] -> Inf a -> Inf a
extendInfGammaEx topLevel ignores tnames inf
  = do env <- getEnv
       infgamma' <- extend (context env) (gamma env) [] [(unqualify name,info) | (name,info) <- tnames, not (isWildcard name)] (infgamma env)
       withEnv (\env -> env{ infgamma = infgamma' }) inf
  where
    extend :: Name -> Gamma -> [(Name,NameInfo)] -> [(Name,NameInfo)] -> InfGamma -> Inf InfGamma
    extend ctx gamma seen [] infgamma
      = return infgamma
    extend ctx gamma seen (x@(name,info):rest) infgamma
      = do let qname = infoCanonicalName name info
               range = infoRange info
               tp    = infoType info
           case (lookup name seen) of
            Just (info2)
              -> do checkCasingOverlap range name (infoCanonicalName name info2) info2
                    env <- getEnv
                    infError range (Pretty.ppName (prettyEnv env) name <+> text "is already defined at" <+> pretty (show (infoRange info2))
                                     <-> text " hint: if these are potentially recursive definitions, give a full type signature to disambiguate them.")
            Nothing
              -> do case (infgammaLookupX name infgamma) of
                      Just info2 | infoCanonicalName name info2 /= nameReturn
                        -> do checkCasingOverlap range name (infoCanonicalName name info2) info2
                              env <- getEnv
                              if (not (isHiddenName name) && show name /= "resume" && show name /= "resume-shallow" && not (name `elem` ignores))
                               then infWarning range (Pretty.ppName (prettyEnv env) name <+> text "shadows an earlier local definition or parameter")
                               else return ()
                      _ -> return ()
           extend ctx gamma (x:seen) rest (infgammaExtend qname (info{ infoCName =  if topLevel then createCanonicalName ctx gamma qname else qname}) infgamma)

createCanonicalName ctx gamma qname
  = let matches = gammaLookup (unqualify qname) gamma
        localMatches = [(qname,info) | (qname,info) <- matches, not (isInfoImport info), qualifier qname == ctx || qualifier qname == nameNil ]
        cname = {- canonicalName (length localMatches) -} qname
    in cname

withGammaType :: Range -> Type -> Inf a -> Inf a
withGammaType range tp inf
  = do defName <- currentDefName
       name <- uniqueName (show defName)
       extendInfGamma [(name,(InfoVal Public name tp range False ""))] inf

currentDefName :: Inf Name
currentDefName
  = do env <- getEnv
       return (currentDef env)

withDefName :: Name -> Inf a -> Inf a
withDefName name inf
  = withEnv (\env -> env{ currentDef = name, namedLam = True }) inf

isNamedLam :: (Bool -> Inf a) -> Inf a
isNamedLam action
    = do env <- getEnv
         withEnv (\env -> env{ namedLam = False }) (action (namedLam env))

qualifyName :: Name -> Inf Name
qualifyName name
  = do env <- getEnv
       return (qualify (context env) name)

getModuleName :: Inf Name
getModuleName
  = do env <- getEnv
       return (context env)

freeInGamma :: Inf Tvs
freeInGamma
  = do env <- getEnv
       sub <- getSub
       return (ftv (sub |-> (infgamma env)))  -- TODO: fuv?

splitPredicates :: Tvs -> Inf [Evidence]
splitPredicates free
  = do st <- getSt
       ps <- subst (preds st)
       let (ps0,ps1) = -- partition (\p -> not (tvsIsEmpty (tvsDiff (fuv p) free))) ps
                       partition (\p -> let tvs = (fuv p) in (tvsIsEmpty tvs || not (tvsIsEmpty (tvsDiff tvs free)))) ps
       setSt (st{ preds = ps1 })
       -- trace ("splitpredicates: " ++ show (ps0,ps1)) $ return ()
       return ps0

addPredicates :: [Evidence] -> Inf ()
addPredicates []
  = return ()
addPredicates ps
  = do updateSt (\st -> st{ preds = (preds st) ++ ps })
       return ()

getPredicates :: Inf [Evidence]
getPredicates
  = do st <- getSt
       subst (preds st)

setPredicates :: [Evidence] -> Inf ()
setPredicates ps
  = do updateSt (\st -> st{ preds = ps })
       return ()

getNewtypes :: Inf Newtypes
getNewtypes
 = do env <- getEnv
      return (types env)


getLocalVars :: Inf [(Name,Type)]
getLocalVars
  = do env <- getEnv
       return (filter (isTypeLocalVar . snd) (infgammaList (infgamma env)))

lookupInfName :: Name -> Inf (Maybe (Name,Type))
lookupInfName name
  = do env <- getEnv
       return (infgammaLookup name (infgamma env))


findDataInfo :: Name -> Inf DataInfo
findDataInfo typeName
  = do env <- getEnv
       case newtypesLookupAny typeName (types env) of
         Just info -> return info
         Nothing   -> failure ("Type.InferMonad.findDataInfo: unknown type: " ++ show typeName ++ "\n in: " ++ show (types env))


traceDefDoc :: (Pretty.Env -> Doc) -> Inf ()
traceDefDoc f
  = do def <- currentDefName
       traceDoc (\penv -> Pretty.ppName penv def <+> text ":" <+> f penv)

traceDoc :: (Pretty.Env -> Doc) -> Inf ()
traceDoc f
  = do penv <- getPrettyEnv
       trace (show (f penv)) $ return ()

ppNameType penv (name,tp)
  = Pretty.ppName penv name <+> colon <+> Pretty.ppType penv tp

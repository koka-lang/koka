-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

module Type.InferMonad( Inf, InfGamma
                      , runInfer, tryRun
                      , traceDoc, traceDefDoc, traceIndent

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
                      , resolveConName, resolveConPatternName
                      , resolveImplicitName

                      , lookupAppName
                      , lookupFunName
                      , lookupLocalName
                      , lookupNameCtx
                      , lookupInfName
                      , NameContext(..), maybeToContext

                      , qualifyName
                      , getModuleName
                      , findDataInfo
                      , withDefName
                      , currentDefName, currentDefNames
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
                      , checkCasing
                      , normalize
                      , getNewtypes

                      -- * Unification
                      , Context(..)
                      , inferUnify, inferUnifies
                      , inferSubsume
                      , withSkolemized, checkSkolemEscape
                      , substImplicitConstraints
                      , scopeImplicitConstraints

                      , typeError
                      , contextError
                      , termError
                      , infError, infWarning
                      , withHiddenTermDoc, inHiddenTermDoc

                      -- run-local
                      , withLocalScope, withNoLocalScope, localScopeDepth

                      -- scope depth
                      , withScope, getScopeDepth

                      -- * Documentation, Intellisense
                      , addRangeInfo, withNoRangeInfo
                      , withNiceNames, lookupNiceName


                      , freeInGamma, ppTvs, ignoreErrors

                      ) where

import Data.Maybe(isNothing)
import Data.List( partition, sortBy, nub, nubBy, intersperse, foldl', find)
import Data.Ord(comparing)
import qualified Data.Set as S
import Control.Applicative
import Control.Monad

import Lib.PPrint
import Common.Range hiding (Pos)
import Common.Unique
import Common.Failure
import Common.Error
import Common.Syntax( Visibility(..), DefSort(..))
import Common.File(endsWith,normalizeWith, seqqList)
import Common.Name
import Common.NamePrim(nameTpVoid,nameTpPure,nameTpIO,nameTpST,nameTpAsyncX,
                       nameTpRead,nameTpWrite,nameTypeHeapDiv,nameHeapDiv,nameEvHeapDiv,nameEvHeapNoDiv,
                       nameReturn,nameTpLocal, nameCopy)

import qualified Common.NameMap as NM

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
import Core.Pretty

import Type.Operations hiding (instantiate, instantiateNoEx, instantiateEx)
import qualified Type.Operations as Op
import Type.Assumption
import Type.InfGamma

import Type.Unify
import Common.Message( docFromRange, table, tablex)

import Core.Pretty()

import Syntax.RangeMap( RangeMap, RangeInfo(..), rangeMapInsert, rangeMapAppend )
import Syntax.Syntax(Expr(..),ValueBinder(..))

import qualified Debug.Trace as DT
import Type.Pretty (ppTypeVar)

trace s x =
  DT.trace (" " ++ s)
   x

{--------------------------------------------------------------------------
  Generalization
--------------------------------------------------------------------------}
generalize :: HasCallStack => Range -> Range -> Bool -> Inf (Rho,Effect,Core.Expr) -> Inf (Scheme,Effect,Core.Expr)
generalize contextRange range close inf
  = (if close then id else scopeImplicitConstraints) $
    do res <- inf
       generalizeX contextRange range close res

generalizeX :: HasCallStack => Range -> Range -> Bool -> (Rho,Effect,Core.Expr) -> Inf (Scheme,Effect,Core.Expr )
generalizeX contextRange range close (tp@(TForall _ _),eff,core0)
  = do stp  <- subst tp
       seff <- subst eff
       if (tvsIsEmpty (fuv stp))
        then return (tp,seff,core0)
        else do (rho,tvars,icore) <- instantiateNoEx range stp  -- instantiate first
                generalizeX contextRange range close (rho,seff,icore core0)

generalizeX contextRange range close (rho0,eff0,bodycore0)
  = do -- traceDefDoc $ \penv -> text "generalizing:" <+> Pretty.ppType penv rho0 <+> text "|" <+> Pretty.ppType penv eff0
       -- check that the computation is total
       if (close)
         then inferUnify (Check "Generalized values cannot have an effect" contextRange) range typeTotal eff0
         else return ()

       -- solve implicit constraints
       seff0 <- subst eff0
       free0 <- freeInGamma
       let free1 = tvsUnion free0 (fuv seff0)

       iccore <- tryResolveImplicitConstraints close free1
       let bodycore1 = iccore bodycore0

       -- normalize type
       seff  <- subst eff0
       srho  <- subst rho0
       let free = tvsUnion free0 (fuv seff)
       nrho <- normalizeX close free srho

       -- generalized type variables
       let tvars = filter (\tv -> not (tvsMember tv free)) (ofuv nrho)

      --  ics <- getImplicitConstraints
      --  traceDefDoc $ \penv -> text "generalize:" <+> Pretty.ppType penv nrho <+> text "|" <+> Pretty.ppType penv seff
      --                         <-> text "  genvars:" <+> ppTvs penv (tvsNew tvars)
      --                         <-> text "  free:" <+> ppTvs penv free
      --                         <-> text "  remaining ics:" <+> ppConstraints penv ics

       if (null tvars)
        then do return (nrho,seff,bodycore1)
        else do -- create fresh type variables for the bounds
                -- important to avoid duplicate names (`test/algeff/exn3`)
                (bvars,bsub) <- freshSub Bound tvars
                let (TForall [] rho5) = bsub |-> (TForall [] nrho)
                    -- core
                    corePre = bsub |-> bodycore1
                    core1 = Core.addTypeLambdas bvars corePre
                    resTp = quantifyType bvars rho5

                -- traceDoc $ \penv -> text "corePre:" <+> prettyExpr penv{Pretty.coreShowTypes=True} corePre
                return (resTp,seff,core1)



improve :: Range -> Range -> Bool -> Effect -> Rho -> Inf (Rho,Effect,Core.Expr -> Core.Expr )
improve contextRange range close eff0 rho0
  = do seff  <- subst eff0
       srho  <- subst rho0
       free0 <- freeInGamma
       (eff1,coref) <- mapImplicitConstraints $ \ics ->
                       do let free = tvsUnion free0 (ftv srho)
                          (ics1,eff1,coref) <- isolate contextRange close free ics seff
                          return ((eff1,coref),ics1)
       (nrho) <- normalizeX close free0 srho  -- use free0 or otherwise function results are not closed, see `test/type/talpin-jouvelot1/#t1`
       return (nrho,eff1,coref)

instantiate :: Range -> Scheme -> Inf (Rho,[TypeVar],Core.Expr -> Core.Expr)
instantiate = instantiateEx

instantiateEx :: Range -> Scheme -> Inf (Rho,[TypeVar],Core.Expr -> Core.Expr)
instantiateEx range tp | isRho tp
  = do (rho,coref) <- Op.extend tp
       return (rho,[],coref)
instantiateEx range tp
  = do (tvars,rho,coref) <- Op.instantiateEx range tp
       -- addPredicates ps
       return (rho, tvars, coref)

instantiateNoEx :: Range -> Scheme -> Inf (Rho,[TypeVar],Core.Expr -> Core.Expr)

instantiateNoEx range tp | isRho tp
  = return (tp,[],id)
instantiateNoEx range tp
  = do (tvars,rho,coref) <- Op.instantiateNoEx range tp
       -- addPredicates ps
       return (rho, tvars, coref)

-- | Automatically remove heap effects when safe to do so.
isolate :: Range -> Bool -> Tvs -> [ImplicitConstraint] -> Effect -> Inf ([ImplicitConstraint], Effect, Core.Expr -> Core.Expr)
isolate rng close free ics eff
  = do -- traceDefDoc $ \penv -> text "isolate:" <+> Pretty.ppType penv eff
                                -- <-> text "  free" <+> ppTvs penv free
                                -- <-> text "  ics:" <+> list (map (ppConstraint penv) ics)
       let (ls,tl) = extractOrderedEffect eff
       case filter (\l -> labelName l `elem` [nameTpLocal,nameTpRead,nameTpWrite]) ls of
          (lab@(TApp labcon [TVar h]) : _)
            -> -- has heap variable 'h' in its effect
               do (polyIcs,ics1) <- splitHDiv h ics
                  let isLocal = (labelName lab == nameTpLocal)
                  -- determineds <- mapM (\ic -> (icCanSolve ic) free ic) polyIcs
                  if not (tvsMember h free) -- || and determineds --  not (.. || tvsMember h (ftv ics1))
                    then do -- we can isolate, and discharge the polyIcs hdiv predicates
                            -- traceDefDoc $ \penv -> text "can isolate:" <+> Pretty.ppType penv eff <+> text ", poly ics" <+> list (map (ppConstraint penv) polyIcs)
                            tv <- freshEffect
                            if isLocal
                             then do -- trace ("isolate local") $ return ()
                                varScope <- getScopeDepth
                                if varScope == 0 then
                                     nofailUnify $ unify (effectExtend lab tv) eff
                                else return ()
                             else do mbSyn <- lookupSynonym nameTpST
                                     let (Just syn) = mbSyn
                                         [bvar] = synInfoParams syn
                                         st     = subNew [(bvar,TVar h)] |-> synInfoType syn
                                     -- traceDoc $ \penv -> text "isolate st: " <+> Pretty.ppType  penv{Pretty.showKinds=True,Pretty.showIds=True} st
                                     nofailUnify $ unify (effectExtend st tv) eff
                            coref  <- resolveImplicitConstraints free polyIcs

                            neweff <- subst tv
                            sics   <- subst ics1
                            -- trace ("isolate to:"  ++ show (pretty neweff)) $ return ()
                            -- return (sps, neweff, id) -- TODO: supply evidence (i.e. apply the run function)
                            -- and try again
                            (ics',eff',coref') <- isolate rng close free sics neweff
                            let coreRun cexpr = if (isLocal)
                                                 then cexpr
                                                 else cexpr  -- TODO: apply runST?
                            return (ics',eff',coreRun . coref' . coref)
                     else do -- traceDefDoc $ \penv -> text "cannot isolate:" <+> Pretty.ppType penv eff <+> text ", poly ics" <+> list (map (ppConstraint penv) polyIcs) <+> text ", free ics:" <+> list (map (ppConstraint penv) ics1)
                             coref <- tryResolveImplicitConstraints close free
                             return (ics,eff,coref)
          (lab@(TApp labcon [TCon global]) : _) -> do
            coref <- tryResolveImplicitConstraints close free
            return (ics,eff,coref)
          _ -> return (ics,eff,id)

  where
    -- | 'splitHDiv h ics' splits constraints 'ics'. Constraints of the form hdiv<h,tp,e> where tp does
    -- not contain h are returned as the first element, all others as the second. This includes
    -- constraints where hdiv<h,a,e> for example where a is polymorphic. Normally, we need to assume
    -- divergence conservatively in such case; however, when we isolate, we know it cannot be instatiated
    -- to contain a reference to h and it is safe to discharge them during isolation without implying
    -- divergence. See test\type\talpin-jouvelot1 for an example: fun rid(x) { val r = ref(x) in !r }
    splitHDiv :: TypeVar -> [ImplicitConstraint] -> Inf ([ImplicitConstraint],[ImplicitConstraint])
    splitHDiv heapTv []
      = return ([],[])
    splitHDiv heapTv (ic:ics)
      = do (ics1,ics2) <- splitHDiv heapTv ics
           let defaultRes = (ics1,ic:ics2)
           tp <- implicitConstraintType ic
           case expandSyn tp of
              TApp (TCon tcon) [tpHeap,tpVal,tpEff]  | typeConName tcon == nameTypeHeapDiv
                -> do shp <- subst tpHeap
                      case expandSyn shp of
                        hp@(TVar tv) | tv == heapTv
                          -> do {- stp <- subst tpVal
                                if (isNothing (find (\ht -> eqType hp ht) (heapTypes stp)))
                                  then do let icnodiv = ic{ icSolve = resolveHeapDivConstraint True {-always no div-} tpHeap tpVal tpEff }
                                          return (icnodiv:ics1,ics2) -- even if polymorphic, we are ok if we isolate
                                  else -- return defaultRes -}
                                       return (ic:ics1,ics2)
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
      TForall [] t
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
                            -> do -- traceDefDoc $ \penv -> text "close effect:" <+> Pretty.ppType penv tp <-> text "  free:" <+> ppTvs penv free
                                  nofailUnify $ unify typeTotal tl
                                  (subst eff) -- (effectFixed ls)
                          _ -> do -- traceDefDoc $ \penv -> text "cannot close effect:" <+> Pretty.ppType penv tp <-> text "  free:" <+> ppTvs penv free
                                  ls' <- mapM (normalizex Pos) ls
                                  tl' <- normalizex Pos tl
                                  return (effectExtends ls' tl')
              args' <- mapM (\(name,arg) -> do {arg' <- normalizex Neg arg; return (name,arg')}) args
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
                  args' <- mapM (\(name,arg) -> do {arg' <- normalizex (vflip var) arg; return (name,arg')}) args
                  res'  <- normalizex var res
                  niceEff <- nicefyEffect eff'
                  return (TFun args' niceEff res')
          TForall vars t
            -> do t' <- normalizex var t
                  return (TForall vars t')
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
                                  -> -- trace (" synonym replace: " ++ show (synInfoName syn, ls, sls, rest)) $
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
    matchParams (TApp _ args) = eqTypes (map TVar params) args
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
           else if (eqType l1 l2)
            then isSubset acc ll1 ll2
            else Nothing

splitEffect :: Effect -> Inf ([Tau],Effect)
splitEffect eff
  = nofailUnify (extractNormalizeEffect eff)


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
         Right (t,_,coref)   -> do
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
                failure ("Type.InferMonad.runUnify: should never fail!: " ++ show err)

withSkolemized :: Range -> Type -> Maybe Doc -> (Type -> [TypeVar] -> Inf (a,Tvs)) -> Inf a
withSkolemized rng tp mhint action
  = do (xvars,xrho,_) <- Op.skolemizeEx rng tp
       (x,extraFree)    <- -- trace ("withSkolemized: " ++ show xvars ) $
                           action xrho xvars
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
unifyError context range (NoMatchEffect eff1 eff2 effectMismatch) _ _
  = unifyError context range (NoMatch (Just effectMismatch)) eff2 eff1
unifyError context range err xtp1 xtp2
  = do free <- freeInGamma
       tp1 <- subst xtp1 >>= normalizeX False free
       tp2 <- subst xtp2 >>= normalizeX False free
       env <- getEnv
       unifyError' (prettyEnv env){Pretty.fullNames = False} context range err tp1 tp2

unifyError' env context range err tp1 tp2
  = do termInfo <- getTermDoc "term" range
       infError range $
        text message <->
        table ([(text "context", docFromRange (Pretty.colors env) rangeContext)
               , termInfo
               ,(text ("inferred " ++ nameType), nice2)
               ]
               ++ nomatch
               ++ effectDiffs
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
          NoArgMatch _ _  -> []
          _               -> [(text ("expected " ++ nameType),nice1)]

    nameType
      = if (getKind tp1 == kindEffect)
         then "effect"
         else "type"

    effectDiffs
      = case err of
          NoMatch (Just diff) -> effectDiffs' diff
          NoMatchEffect _ _ diff -> effectDiffs' diff
          _ -> []

    effectDiffs' diff = unexpectedMessages ++ missingMessages
      where
        niceUnexpected = map (Pretty.niceType env) (unexpectedEffectLabels diff)
        niceMissing = map (Pretty.niceType env) (missingEffectLabels diff)
        unexpectedMessages = if null niceUnexpected
          then []
          else [(text "unexpected", hsep (punctuate comma niceUnexpected))]

        missingMessages = if null niceMissing
          then []
          else [(text "missing",
            vsep [
              hsep (punctuate comma niceMissing),
              hsep [
                text "Consider using",
                Pretty.keyword env "mask",
                text "or adding it to the function's effect type"
              ]
            ]
          )]

    (message,hint)
      = case err of
          NoMatch _ -> (nameType ++ "s do not match",[])
          NoMatchKind -> ("kinds do not match",[])
          NoMatchSkolem kind
                      -> ("abstract types do not match",if (not (null extra))
                                                         then []
                                                         else [(text "hint", if (isKindHeap kind || isKindScope kind)
                                                                         then text "a local variable or reference escapes its scope?"
                                                                         else text "an higher-rank type escapes its scope?")])
          NoSubsume   -> ("type is not polymorphic enough",[(text "hint",text "give a higher-rank type annotation to a function parameter?")])
          Infinite    -> ("types do not match (due to an infinite type)",[(text "hint",text "give a type to the function definition?")])
          NoMatchEffect _ _ _ -> ("effects do not match",[])
          NoArgMatch n m -> if (m<0)
                             then ("only functions can be applied",[])
                             else ("application has too " ++ (if (n > m) then "few" else "many") ++ " arguments"
                                  ,[(text "hint",text ("expecting " ++ show n ++ " argument" ++ (if n == 1 then "" else "s") ++ " but has been given " ++ show m))])


typeError :: Range -> Range -> Doc -> Type -> [(Doc,Doc)] -> Inf ()
typeError contextRange range message xtp extra
  = do env  <- getEnv
       free <- freeInGamma
       tp   <- subst xtp >>= normalizeX False free
       typeError' (prettyEnv env) contextRange range message tp extra

typeError' env contextRange range message tp extra
  = do termInfo <- getTermDoc "term" range
       infError range $
        message <->
        table ([(text "context", docFromRange (Pretty.colors env) contextRange)
              , termInfo
              ,(text "inferred type", Pretty.niceType env tp)
              ] ++ extra)

contextError :: Range -> Range -> Doc -> [(Doc,Doc)] -> Inf ()
contextError contextRange range message extra
  = do env <- getEnv
       contextError' (prettyEnv env) contextRange range message extra

contextError' env contextRange range message extra
  = do termInfo <- getTermDoc "term" range
       infError range $
        message <->
        table  ([(text "context", docFromRange (Pretty.colors env) contextRange)
                , termInfo
                ]
                ++ extra)

termError :: Range -> Doc -> Type -> [(Doc,Doc)] -> Inf ()
termError range message tp extra
  = do env <- getEnv
       termError' (prettyEnv env) range message tp extra

termError' env range message tp extra
  = do termInfo <- getTermDoc "term" range
       infError range $
        message <->
        table  ([ termInfo
                ,(text "inferred type", Pretty.niceType env tp)
                ]
                ++ extra)



----------------------------------------------------------------
-- Resolve names
----------------------------------------------------------------

-- | Lookup a name with a certain type and return the fully qualified name and its type
resolveName :: HasCallStack =>  Name -> Maybe (Type,Range) -> Range -> Inf (Name,Type,NameInfo)
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
resolveRhsName :: HasCallStack => Name -> (Type,Range) -> Range -> Inf (Name,Type,NameInfo)
resolveRhsName name (tp,ctxRange) range
  = do -- traceDefDoc $ \penv -> text "resolveRhsName:" <+> text (show name)
       candidates <- lookupNameCtx isInfoValFunExt name (CtxType tp) range
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

resolveConPatternName :: Name -> Type -> Int -> Range -> Inf (Name,Type,Core.ConRepr,ConInfo)
resolveConPatternName name matchType patternCount range
  = do (qname,tp,info) <- resolveNameEx isInfoCon Nothing name ctx range  range
       return (qname,tp,infoRepr info,infoCon info)
  where
    ctx = CtxFunArgs True {-partial?-} patternCount [] (Just matchType)
          {- if patternCount > 0
            then CtxFunArgs True {-partial?-} patternCount [] (Just matchType)
            else CtxType matchType -}


resolveNameEx :: HasCallStack => (NameInfo -> Bool) -> Maybe (NameInfo -> Bool) -> Name -> NameContext -> Range -> Range -> Inf (Name,Type,NameInfo)
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
                            infError range (Pretty.ppName penv name <+> text "does not match the argument types" <->
                                               table (ctxTerm rangeContext ++
                                                      [(text "inferred type",nice2)
                                                      ,(text "expected type",nice1)]))
                    (CtxType tp, (_:rest))
                      -> infError range (text "identifier" <+> Pretty.ppName penv name <+> text "has no matching definition" <->
                                         table (ctxTerm rangeContext ++
                                                [(text "inferred type", Pretty.niceType penv tp)
                                                ,(text "candidates", ppCandidates env amb)] ++ ppImplicitsHint env amb))
                    (CtxFunArgs matchSome fixed named (Just resTp), (_:rest))
                      -> do let message = "with " ++ show (fixed + length named) ++ " argument(s) matches the result type"
                            infError range (text "no function" <+> Pretty.ppName penv name <+> text message <+>
                                            Pretty.niceType penv resTp <.> ppAmbiguous env "" amb)
                    (CtxFunArgs matchSome fixed named Nothing, (_:rest))
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
                                                ,(text "candidates", ppCandidates env amb)] ++ ppImplicitsHint env amb
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
                              _ | nameIsEtaHole name
                                -> do inCtx <- holeAllowed <$> getSt
                                      let header = text "eta-expansion of \"_\" is not allowed for top-level expressions"
                                          message = if inCtx
                                                      then header <-> text "hint: perhaps you meant to use the \"hole\" keyword to denote the hole in a constructor context?"
                                                      else header
                                      infError range message
                                      error "done"
                              _ -> do -- when (isImplicitConstraintEvidenceName name) $ error ("evidence " ++ show name ++ " cannot be found")
                                      infError range (text "identifier" <+> Pretty.ppName penv name <+> text "cannot be found")
                                      error "done"

        [(qname,info)]
           -> do -- when (not asPrefix) $  -- todo: check casing for asPrefix as well
                 checkCasing range name qname info
                 return (qname,infoType info,info)
        _  -> do env <- getEnv
                 (term,termInfo) <- getTermDoc "context" rangeContext
                 infError range (text "identifier" <+> Pretty.ppName (prettyEnv env) name <+> text "cannot be resolved." <->
                                 table ([(term, termInfo),
                                         (text "inferred type", ppNameContext (prettyEnv env) ctx),
                                         (text "candidates", ppCandidates env matches),
                                         (text "hint", text "give a type annotation or qualify the name?")] ++ ppImplicitsHint env matches))
  where
    hintTypeSig = "give a type annotation to the function parameters or qualify the name?"


----------------------------------------------------------------
-- Resolving of implicit expressions
----------------------------------------------------------------

-- lookup an application name `f(...)` where the name context usually contains (partially) inferred
-- argument types. We reuse the lookup for implicit arguments as it works the same
-- (except that for implicit arguments we allow value types to be resolved with unit functions (for conversions))
lookupAppName :: Bool -> Name -> NameContext -> Range -> Range ->
                   Inf (Either [Doc] (Type,Expr Type,[((Name,Range),Expr Type, (Bool -> Doc))]))
lookupAppName allowDisambiguate name ctx contextRange range
  = do roots <- if not (isConstructorName name)
                  then -- normal identifier
                       return [(isInfoValFunExt,name)]
                  else -- constructor application: we need to consider creator functions too (for default fields)
                       do let cname = newCreatorName name
                          defName <- currentDefName
                          -- traceDefDoc $ \penv -> text "lookupAppName, constructor name:" <+> Pretty.ppName penv name <+> text "in definition" <+> Pretty.ppName penv defName
                          if (defName == unqualify cname || defName == nameCopy) -- a bit hacky, but ensure we don't call the creator function inside itself or the copy function
                            then return [(isInfoCon,name)]
                            else return [(isInfoFun,cname),(isInfoCon,name)]

       -- try to find a unique solution
       res <- resolveImplicitArg allowDisambiguate
                                 (not allowDisambiguate) {- allow unitFunVal: at first, when allowDisambiguate is False, we like to see all possible instantations -}
                                 ctx range roots
       case res of
          Right iarg@(ImplicitArg qname _ rho iargs)
            -> do -- when (not (null iargs)) $ traceDefDoc $ \penv -> text "resolved app name with implicits:" <+> prettyImplicitArg penv iarg
                  -- traceDefDoc $ \penv -> text "lookupAppName:" <+> Pretty.ppName penv name <.> text " to:" <+> prettyImplicitArg penv iarg
                  penv <- getPrettyEnv
                  let implicits = [((pname,range),
                                     toImplicitArgExpr (endOfRange range) iarg,
                                     prettyImplicitAssign penv "" pname iarg) | (pname,iarg) <- iargs]
                  return (Right (rho, Var qname False range, implicits))
          Left docs
            -> if (allowDisambiguate && not (null docs))
                then do env <- getEnv
                        (term,termInfo) <- getTermDoc "context" contextRange
                        infError range (text "identifier" <+> Pretty.ppName (prettyEnv env) name <+> text "cannot be resolved" <->
                                        table [(term, termInfo),
                                               (text "inferred type", ppNameContext (prettyEnv env) ctx),
                                               (text "candidates", ppAmbDocs docs),
                                               (text "hint", text "qualify the name?")])
                        return (Left docs)
                else return (Left docs)


-- resolve an implicit argument name to an expression
resolveImplicitName :: Name -> Type -> Range -> Range -> Inf (Expr Type, Doc)
resolveImplicitName name tp contextRange range
  = do res <- resolveImplicitArg True {-disambiguate-} True {-allow unit fun val for conversions -}
                                 (implicitTypeContext tp) range [(isInfoValFunExt, name)]
       penv <- getPrettyEnv
       case res of
         Right iarg   -> do traceDefDoc $ \penv -> text "resolved implicit" <+> prettyImplicitAssign penv "?" name iarg False
                            return (toImplicitArgExpr range iarg, prettyImplicitArg penv iarg)
         Left docs    -> do (term,termInfo) <- getTermDoc "context" contextRange
                            infError range
                                (text "cannot resolve implicit parameter" <->
                                table [(term, termInfo),
                                        (text "parameter",  text "?" <.> ppNameType penv (name,tp)),
                                        (text "candidates", ppAmbDocs docs),
                                        (text "hint", text "add a (implicit) parameter to the function signature?")])
                            return (Var name False range, Lib.PPrint.empty)


ppAmbDocs :: [Doc] -> Doc
ppAmbDocs docs
  = if null docs
      then text "..."
      else let cutdocs = take 10 docs ++ (if length docs > 10 then [text "..."] else [])
           in align (vcat cutdocs)


-----------------------------------------------------------------------
-- Implicit arguments
-----------------------------------------------------------------------

-- A resolved implicit argument is always a name together with a list of further
-- implicit arguments (in case it is a function itself)
data ImplicitArg   = ImplicitArg{ iaName :: !Name
                                , iaInfo :: !NameInfo
                                , iaType :: !Rho          -- instantiated type
                                , iaImplicitArgs :: ![(Name, ImplicitArg)]
                                }

-- special empty implicit arg for display purposes (as "...")
emptyImplicitArg :: ImplicitArg
emptyImplicitArg = ImplicitArg nameNil emptyInfo typeUnit []
  where
    emptyInfo = InfoVal Private nameNil typeUnit 0 rangeNull True False ""

prettyImplicitArg :: Pretty.Env -> ImplicitArg -> Doc
prettyImplicitArg penv (ImplicitArg name info rho iargs)  | nameIsNil name
  = text "..."
prettyImplicitArg penv (ImplicitArg name info rho iargs)
  = let withColor clr doc = color (clr (Pretty.colors penv)) doc in
    withColor colorImplicitExpr (Pretty.ppNamePlain penv name) <.>
    -- Pretty.ppType penv rho <+>
    if null iargs then Lib.PPrint.empty
                  else let fcount   = case splitFunType rho of
                                      Just (ipars,_,restp) -> let (fixed,_,_) = splitOptionalImplicit ipars
                                                              in length fixed
                                      _ -> 0 -- should never happen? (since we got implicit arguments)
                           docargs  = [prettyImplicitAssign penv "" pname iarg True | (pname,iarg) <- iargs]
                           docfixed = [text "_" | _ <- [1..fcount]]
                       in parens (hcat (intersperse comma (docfixed ++ docargs)))


prettyImplicitAssign :: Pretty.Env -> String -> Name -> ImplicitArg -> (Bool -> Doc)
prettyImplicitAssign penv prefix pname iarg
  = let pardoc = color (colorImplicitParameter (Pretty.colors penv)) (Pretty.ppNamePlain penv pname) <.> text "="
    in seq pardoc $
        if ((pname == iaName iarg && null (iaImplicitArgs iarg)) || fromImplicitParamName pname == unqualifyFull (iaName iarg) || nameIsNil (iaName iarg))
         then (\shorten -> (if shorten then Lib.PPrint.empty else pardoc) <.> prettyImplicitArg penv iarg)
         else (\shorten -> pardoc <.> prettyImplicitArg penv iarg)

iargScopeDepth :: Name -> NameInfo -> Int
iargScopeDepth name info
  = if isImplicitConstraintEvidenceName name then -2   -- never prefer compiler generated constraints
    else infoScopeDepth info

iaScopeDepth :: ImplicitArg -> Int
iaScopeDepth iarg
  = iargScopeDepth (iaName iarg) (iaInfo iarg)

-- Convert an implicit argument to an expression (that is supplied as the argument)
toImplicitArgExpr :: Range -> ImplicitArg -> Expr Type
toImplicitArgExpr xrange (ImplicitArg iname info itp iargs)
      = let range = rangeHide xrange in  -- don't add things in the expression to the rangemap
        case iargs of
          [] -> Var iname False range
          _  -> case splitFunType itp of
                  Just (ipars,ieff,iresTp) | any Op.isOptionalOrImplicit ipars -- eta-expansion needed?
                    -- eta-expand and resolve further implicit parameters
                    -- todo: eta-expansion may become part of subsumption?
                    ->  let (fixed,opt,implicits) = splitOptionalImplicit ipars in
                        assertion "Type.InferMonad.toImplicitAppExpr" (length implicits == length iargs) $
                        let nameFixed    = [makeHiddenName "arg" (newName ("x" ++ show i)) | (i,_) <- zip [1..] fixed]
                            argsFixed    = [(Nothing,Var name False range) | name <- nameFixed]
                            argsImplicit = [(Just (pname,range), toImplicitArgExpr (endOfRange range) iarg) | (pname,iarg) <- iargs]
                            etaTp        = TFun fixed ieff iresTp
                            eta          = (if null fixed then id
                                            else \body -> Lam [ValueBinder name Nothing Nothing range range | name <- nameFixed] body False range)
                                              (App (Var iname False range)
                                                      (argsFixed ++ argsImplicit)
                                                      range)
                        in eta
                  _ -> failure ("Type.InferMonad.toImplicitAppExpr: illegal type for implicit? " ++ show range ++ ", " ++ show iname)


-- We use typed arguments during implicit argument resolving
type TypedArg = (Name,NameInfo,Rho)

prettyTypedArg :: Pretty.Env -> TypedArg -> Doc
prettyTypedArg penv (name,info,tp)
  = Pretty.ppParam penv (name,tp)


toImplicitArg :: [(Name, ImplicitArg)] -> TypedArg -> ImplicitArg
toImplicitArg iargs (name,info,rho) = ImplicitArg name info rho iargs

-----------------------------------------------------------------------
-- Resolving implicit names
-----------------------------------------------------------------------
resolveMaxChainDepth :: Int
resolveMaxChainDepth = 32   -- just in case: prevent infinite expansion (not required anymore?)

-- We can find a unique solution, or none, or surely ambiguous.
-- The `selInfinite` tracks infinite chains, while `selCandidates` ambigious ones. Both are for error messages only.
data ImplicitSelect
  = None                  -- no solution
  | Found !ImplicitArg    -- a single solution
  | Amb   ![ImplicitArg]  -- multiple solutions (failure)
  | Infty !ImplicitArg    -- infinite chain (failure)

allCandidates :: ImplicitSelect -> [ImplicitArg]
allCandidates sel
  = case sel of
      None    -> []
      Found x -> [x]
      Amb xs  -> xs
      Infty x -> [x]

mapCandidates :: (ImplicitArg -> ImplicitArg) -> ImplicitSelect -> ImplicitSelect
mapCandidates f sel
  = case sel of
      None    -> None
      Found x -> Found (f x)
      Amb xs  -> Amb (map f xs)
      Infty x -> Infty (f x)

merge :: ImplicitSelect -> ImplicitSelect -> ImplicitSelect
merge sel1 sel2
  = case (sel1,sel2) of
      (None, _)    -> sel2
      (_,    None) -> sel1
      -- any other combination becomes ambigious.
      -- in particular, Infty + Found must be treated as ambigious to be sound
      _            -> Amb (allCandidates sel1 ++ allCandidates sel2)


prettySelect :: Pretty.Env -> ImplicitSelect -> Doc
prettySelect penv None         = text "None"
prettySelect penv (Found iarg) = text "Found" <+> prettyImplicitArg penv iarg
prettySelect penv (Infty iarg) = text "Infty" <+> prettyImplicitArg penv iarg
prettySelect penv (Amb xs)     = text "Amb" <+> list (map (prettyImplicitArg penv) xs)


-- Resolve an implicit argument fully
resolveImplicitArg :: Bool -> Bool -> NameContext -> Range -> [(NameInfo -> Bool, Name)] -> Inf (Either [Doc] (ImplicitArg))
resolveImplicitArg allowDisambiguate allowUnitFunVal ctx range roots
  = do env <- getEnv
       sel <- resolveImplicitArgEx allowDisambiguate allowUnitFunVal (allowInfiniteChains env) [] ctx range roots
       case sel of
         Found iarg -> return (Right iarg)
         _          -> do penv <- getPrettyEnv
                          return $ Left (map (prettyImplicitArg penv) (allCandidates sel))

-- Resolve an implicit argument fully. This is recursively used with the `chain` of previously resolved implicit names
resolveImplicitArgEx :: Bool -> Bool -> Bool -> [TypedArg] -> NameContext -> Range -> [(NameInfo -> Bool, Name)] -> Inf ImplicitSelect
resolveImplicitArgEx allowDisambiguate allowUnitFunVal allowInfiniteChains chain ctx range roots
  = do candidates1 <- concatMapM (\(infoFilter,name) -> lookupImplicitArg allowUnitFunVal infoFilter name ctx range) roots
       let candidates2 = filter (not . existConCreator candidates1) candidates1
           sorted = sortCandidates candidates2
      --  when (length chain >= 4) $
      --     traceDefDoc $ \penv -> text "resolveImplicitArg: chain:" <+> list (map (prettyTypedArg penv) chain) <.> text ", continue with:" <->
      --                             indent 2 (vcat (map (prettyTypedArg penv) (map fst sorted)))
       resolveUniquely allowDisambiguate allowInfiniteChains chain ctx range None sorted
  where
    -- always prefer a creator definition over a plain constructor if it exists
    existConCreator :: [TypedArg] -> TypedArg -> Bool
    existConCreator candidates (name,info,_)
      = isInfoCon info && any (\(iargName,_,_) -> iargName == cname) candidates
      where
        cname = newCreatorName name

    sortCandidates :: [TypedArg] -> [(TypedArg,[(Name,Type)] {- further implicits to resolve-} )]
    sortCandidates candidates
      = let -- find for each candidate which further implicits need to be resolved
            icandidates  = map (implicitsToResolve ctx) candidates
            -- now we can sort them according to their cost
            cost ((name,info,_),iargs)  = ( -(iargScopeDepth name info) -- inner scopes first
                                          , length iargs                -- least further implicits arguments first
                                          )
        in sortBy (\x y -> compare (cost x) (cost y)) icandidates


-- Resolve the best candidate for an implicit parameter
resolveUniquely :: Bool -> Bool -> [TypedArg] -> NameContext -> Range -> ImplicitSelect -> [(TypedArg,[(Name,Type)])] -> Inf ImplicitSelect
resolveUniquely allowDisambiguate allowInfiniteChains chain ctx range current []
  = -- nothing further to explore
    do -- traceDefDoc $ \penv -> text "resolveUniquely: explored all solutions:" <+> prettySelect penv current
       return current

resolveUniquely allowDisambiguate allowInfiniteChains chain ctx range current@(Amb ambs@(_:_:_)) candidates
  = -- once we are ambiguous with 2 or more (possible) solutions, we don't need to explore further options
    do -- traceDefDoc $ \penv -> text "resolveUniquely: ambigious:" <+> prettySelect penv current
       let extra = map (toImplicitArg [] . fst) candidates  -- include all remaining potential candidates in the error message?
       return (Amb (ambs ++ extra))

resolveUniquely allowDisambiguate allowInfiniteChains chain ctx range (Found current) (((qname,info,_),_):_)
  | allowDisambiguate && iaScopeDepth current > iargScopeDepth qname info
  = -- if we can disambiguate, the inner scope is always preferred (assuming sorted candidates)
    do -- traceDefDoc $ \penv -> text "resolveUniquely: found innermost solution:" <+> prettyImplicitArg penv current
       return (Found current)

resolveUniquely allowDisambiguate allowInfiniteChains chain ctx range current (next@((qname,info,rho),ipars) : candidates)
  | not allowInfiniteChains && not (isDecreasingChain chain ctx qname rho)
  = -- if this might lead to an infinite derivation
    do -- traceDefDoc $ \penv -> text "resolveUniquely: infinite derivation:" <->
       --                      indent 2 (vcat (map (prettyTypedArg penv) (reverse (fst next:chain))))
       let iarg = toImplicitArg [(pname, emptyImplicitArg) | (pname,_) <- ipars] (fst next)
           sel  = Infty iarg
       resolveUniquely allowDisambiguate allowInfiniteChains chain ctx range (merge current sel) candidates

resolveUniquely allowDisambiguate allowInfiniteChains chain ctx range current candidates
  | length chain > resolveMaxChainDepth
  = do traceDefDoc $ \penv -> text "resolve implicit, cut off long chain:" <->
                               indent 2 (vcat (map (prettyTypedArg penv) (reverse chain)))
       let sels = map (Infty . toImplicitArg [] . fst) candidates
       return $! foldr merge None sels

resolveUniquely allowDisambiguate allowInfiniteChains chain ctx range current (next@((name,info,rho),ipars) : candidates)
  = do -- recursively resolve the required implicit parameters
       -- traceDefDoc $ \penv -> text "resolveUniquely: resolve next candidate:" <+> prettyTypedArg penv (fst next)
       --                          <-> indent 2 (text "current:" <+> prettySelect penv current)
       sel <- resolveImplicitParameters allowDisambiguate allowInfiniteChains chain range next
       resolveUniquely allowDisambiguate allowInfiniteChains chain ctx range (merge current sel) candidates


-- Resolve recursively any further required implicit parameters
resolveImplicitParameters :: Bool -> Bool -> [TypedArg] -> Range -> (TypedArg,[(Name,Type)]) -> Inf ImplicitSelect
resolveImplicitParameters allowDisambiguate allowInfiniteChains chain range (current@(name,info,rho),ipars)
  = resolve [] ipars
  where
    chainNew
      = current : chain

    resolve :: [(Name,ImplicitArg)] -> [(Name,Type)] -> Inf ImplicitSelect
    resolve acc []
      = return (Found (ImplicitArg name info rho (reverse acc)))
    resolve acc (par:pars)
      = do sel <- resolveImplicitParameter allowDisambiguate allowInfiniteChains chainNew range par
           case sel of
             Found iarg -> do -- keep resolving
                              resolve ((fst par,iarg):acc) pars
             _          -> do -- give up early if we cannot resolve a parameter
                              let makePars iarg   = reverse acc ++ [(fst par, iarg)] ++ [(pname, emptyImplicitArg) | (pname,_) <- pars]
                                  extendIarg iarg = ImplicitArg name info rho (makePars iarg)
                              return $ mapCandidates extendIarg sel

-- recursively resolve an implicit parameter
resolveImplicitParameter :: Bool -> Bool -> [TypedArg] -> Range -> (Name, Type) -> Inf ImplicitSelect
resolveImplicitParameter allowDisambiguate allowInfiniteChains chain range (pname,ptp)
  = -- recursively resolve an implicit parameter
    let (pnameName,pnameExpr) = splitImplicitParamName pname
        newctx = implicitTypeContext ptp
    in resolveImplicitArgEx allowDisambiguate True {- allow unit val -} allowInfiniteChains chain newctx
                            (endOfRange range) -- use end of range to deprioritize with hover info
                            [(isInfoValFunExt,pnameExpr)]


decreasingWithin :: Int
decreasingWithin = 4

-- Have a previously tried to derive this parameter?
isDecreasingChain :: [TypedArg] -> NameContext -> Name -> Type -> Bool
isDecreasingChain chain ctx qname tp
  = case filter (\(pname,_,_) -> pname == qname) chain of  -- find only matching definition in the chain
      []                  -> True                      -- never visited before
      prevtps  ->
        if length prevtps < decreasingWithin then True -- Not enough to decide yet (we want to be able to grow a bit at the beginning)
        else
          let limited = take decreasingWithin prevtps -- the last *k* elements in the same partition.
          in weight tp < (maximum $ map (\(_, _, tp) -> weight tp) limited) -- we want the instantiated type to "smaller" than any previous one (i.e. at least smaller than the maximum)
  where
    -- Note: pname and qname are fully qualified resolved names with their instantiated types
    --   pname=qname : forall as. t           e.g. list/show : (xs : list<a>, ?show : a -> string ) : string
    --
    -- This means ptp and tp are instantiations of the same type scheme (and only differ in substitution):
    --   ptp = t[as:=ts1]  tp = t[as:=ts2]
    --
    -- We can now put a decreasing measure on those types to see if the instantiation is getting smaller
    -- which ensures termination (but also prevents some programs to be accepted even though finite derivations exist (see `test/overload/wrong/blowup6.kk`))
    -- Our current measure is the number of constructors in the types of the implicit parameters.
    -- (and an even more precise one would be the size of each instantiated type variable used in implicits in a particular lexical order)

    weight :: Type -> Int
    weight tp
      = case splitFunScheme tp of
          Just (_,pars,eff,res) -> let (_,_,implicits) = splitOptionalImplicit pars
                                   in weightParams implicits
          _                     -> 0

    weightParams :: [(Name,Type)] -> Int
    weightParams pars
      = sum (map (weightType . snd) pars)

    weightType :: Type -> Int
    weightType tp
      = case tp of
          TForall tvars t      -> weightType t
          TFun tpars teff tres -> weightParams tpars + weightType teff + weightType tres
          TApp t targs         -> weightType t + sum (map weightType targs)
          TSyn _ _ t           -> weightType t
          TCon _               -> 1
          TVar _               -> 0



-- Find for an typed argument if it needs further implicits to be solved
implicitsToResolve :: NameContext -> TypedArg -> (TypedArg,[(Name,Type)])
implicitsToResolve ctx targ@(name,info,rho)
  = let iargs = case splitFunType rho of
                  Just (ipars,ieff,iresTp)  | any Op.isOptionalOrImplicit ipars
                    -- recursively resolve further required implicit parameters
                    -> implicitsOf ipars
                  _ -> []
    in (targ,iargs)
  where
    implicitsOf :: [(Name,Type)] -> [(Name,Type)]
    implicitsOf ipars
      = -- only return implicits that were not already given explicitly by the user (in `named`)
        let (fixed,optional,implicits)  = splitOptionalImplicit ipars
            alreadyGiven     = case ctx of
                                  CtxFunTypes partial fixedArgs named mbResTp
                                    -> let namedAsFixed = map fst (take (length fixedArgs - length fixed - length optional) implicits)
                                       in namedAsFixed ++ map fst named
                                  CtxFunArgs partial n named mbResTp
                                    -> let namedAsFixed = map fst (take (n - length fixed - length optional) implicits)
                                       in namedAsFixed ++ named
                                  _ -> []
            toResolve        = filter (\(name,_) -> let (pname,_) = splitImplicitParamName name
                                                    in not (pname `elem` alreadyGiven)) implicits
        in -- trace ("implicitsToResolve: " ++ show (map (fst . splitImplicitParamName . fst) toResolve) ++ ", " ++ show alreadyGiven) $
           toResolve


-----------------------------------------------------------------------
-- Looking up application names and implicit names
-----------------------------------------------------------------------

lookupImplicitArg :: Bool -> (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [TypedArg]
lookupImplicitArg allowUnitFunVal infoFilter name ctx range
  = do -- traceDefDoc $ \penv -> text "lookupImplicitArg:" <+> ppNameCtx penv (name,ctx) <+> text ", previous:" <+> list (map (ppNameCtx penv) previousCtxs)
       candidates0 <- lookupNames infoFilter name ctx range
       candidates  <- case ctx of
                        -- for implicits we also allow conversion unit functions for values
                        -- if `expect` is a type variable we may need to remove duplicate candidates here.
                        CtxType expect | allowUnitFunVal && not (isFun expect)
                           -> do candidates1 <- lookupNames infoFilter name (CtxFunTypes False [] [] (Just expect)) range
                                 return (nubBy (\(_,info1,_) (_,info2,_) -> infoCName info1 == infoCName info2)
                                               (candidates0 ++ candidates1))
                        _  -> return candidates0
       -- add implicit constraints
       iargs <- case ctx of
                  CtxType expect -> do mbiarg <- checkImplicitConstraint name expect range range
                                       case mbiarg of
                                         Just iarg -> return [iarg]
                                         _         -> return []
                  _ -> return []
       return (candidates ++ iargs)


----------------------------------------------------------------
-- Lookup names
----------------------------------------------------------------

lookupFunName :: HasCallStack => Name -> Maybe (Type,Range) -> Range -> Inf (Maybe (Name,Type,NameInfo))
lookupFunName name mbType range
  = do matches <- lookupNameCtx isInfoFun name (maybeRToContext mbType) range
       case matches of
        []   -> return Nothing
        [(name,info)]  -> return (Just (name,infoType info,info))
        _    -> do env <- getEnv
                   infError range (text "identifier" <+> Pretty.ppName (prettyEnv env) name <+> text "cannot be resolved"
                                     <.> ppAmbiguous env hintQualify matches)
  where
    hintQualify = "qualify the name to disambiguate it?"

lookupNameCtx :: HasCallStack => (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [(Name,NameInfo)]
lookupNameCtx infoFilter name ctx range
  = do candidates0 <- lookupNames infoFilter name ctx range
       let candidates = [(name,info) | (name,info,_) <- candidates0]
       -- traceDefDoc $ \penv -> text " lookupNameCtx:" <+> ppNameCtx penv (name,ctx) <+> colon
       --                       <+> list [Pretty.ppParam penv (name,rho) | (name,info,rho) <- candidates]
       case candidates of
         []  -> return candidates
         [_] -> return candidates
         _   -> case (filterInnerScopes candidates) of
                  [candidate] -> return [candidate]
                  _           -> return candidates
  where
    filterInnerScopes :: [(Name,NameInfo)] -> [(Name,NameInfo)]
    filterInnerScopes = filterInnerScopesEx compareScopeDepth

    filterInnerScopesEx cmpScope []  = []
    filterInnerScopesEx cmpScope (x:xs)
      = filter x [] xs
      where
        filter x acc []     = x : filterInnerScopesEx cmpScope (reverse acc)
        filter x acc (y:ys)
          = case cmpScope x y of
              LT -> filter y [] ys        -- continue with y, drop x and acc
              GT -> filter x acc ys       -- drop y
              EQ -> filter x (y:acc) ys

    compareScopeDepth (name1,info1) (name2,info2)
      = let sd1 = infoScopeDepth info1
            sd2 = infoScopeDepth info2
        in compare sd1 sd2


-- lookup names in the local and global scope that match the given name context
-- Returns also an instantiated rho required to match the name context
lookupNames :: HasCallStack => (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [(Name,NameInfo,Rho)]
lookupNames infoFilter name ctx range
  = do -- traceDefDoc $ \penv -> text " lookupNames:" <+> text (show name) <+> colon <+> ppNameContext penv ctx
       matches <- do lres <- lookupLocalName infoFilter name
                     case lres of
                      Right local -> return [local] -- a local name that matches exactly was found; use it always
                      Left locals -> -- otherwise consider globals as well besides the locally qualified locals that matched
                                     -- todo: should we prioritize local locally qualified names when disambiguating?
                                     do globals <- lookupGlobalName infoFilter name
                                        return (locals ++ globals)
       tmatches <- filterMatchNameContextEx range ctx matches
       -- traceDefDoc $ \penv -> text " lookupNames:" <+> text (show name) <+> colon <+> ppNameContext penv ctx <+> text ", matches:" <+> list [text (show name) <+> colon <+> Pretty.ppType penv tp | (name,info,tp) <- tmatches]
       return tmatches


lookupLocalName :: (NameInfo -> Bool) -> Name -> Inf (Either [(Name,NameInfo)] (Name,NameInfo))
lookupLocalName infoFilter name  | isImplicitConstraintEvidenceName name
  = do st <- getSt
       case infgammaLookup name (iconstraintsGamma st) of
         Right res -> return (Right res)
         left      -> return left
lookupLocalName infoFilter name
  = do env <- getEnv
       subst $ infgammaLookupEx infoFilter name (infgamma env)

lookupGlobalName :: (NameInfo -> Bool) -> Name -> Inf [(Name,NameInfo)]
lookupGlobalName infoFilter name
  = do env <- getEnv
       let findName = filterGammaNames (gamma env)
      --  traceDefDoc $ \penv -> text "lookupGlobalName:" <+> Pretty.ppName penv name
       case importsExpand name (imports env) of
        Right (name',_) -> do
          -- traceDefDoc $ \penv -> text "lookupGlobalName (imported):" <+> Pretty.ppName penv name'
          case findName name' of -- First look by aliased import name
            [] -> return $ findName name -- If not found, look by original import name
            ni -> return ni
        Left [] -> return $ findName name -- If no import alias, look by original name
        Left names -> do
          -- If multiple aliases match, ideally we look through all of them
          -- However, that runs into issue #719 where the original module name is not included in `names`
          -- traceDefDoc $ \penv -> text "lookupGlobalName (imported aliases):" <+> list (map (Pretty.ppName penv) names)
          return $ findName name
  where filterGammaNames gamma name = filter (infoFilter . snd) (gammaLookup name gamma)

filterMatchNameContext :: HasCallStack => Range -> NameContext -> [(Name,NameInfo)] -> Inf [(Name,NameInfo)]
filterMatchNameContext range ctx candidates
  = do xs <- filterMatchNameContextEx range ctx candidates
       return [(name,info) | (name,info,_) <- xs]

filterMatchNameContextEx :: HasCallStack => Range -> NameContext -> [(Name,NameInfo)] -> Inf [(Name,NameInfo,Rho)]
filterMatchNameContextEx range ctx candidates
  = case ctx of
      CtxNone         -> return [(name,info,infoType info) | (name,info) <- candidates]
      CtxType expect  -> do mss <- mapM (matchType expect) candidates
                            return (concat mss)
      CtxFunArgs partial n named mbResTp
                      -> do mss1 <- mapM (matchNamedArgs partial n named mbResTp) candidates
                            mss2 <- case mbResTp of
                                      Just tp | (partial && n==0) -> mapM (matchType tp) candidates -- for partial constructor like `Nil`
                                      _ -> return []
                            return (concat (mss1 ++ mss2))
      CtxFunTypes partial fixed named mbResTp
                      -> do mss <- mapM (matchArgs partial fixed named mbResTp) candidates
                            return (concat mss)
  where
    matchType :: HasCallStack => Type -> (Name,NameInfo) -> Inf [(Name,NameInfo,Rho)]
    matchType expect (name,info)
      = do free <- freeInGamma
           res <- do -- traceDefDoc $ \penv0 -> let penv = penv0{Pretty.showIds=True} in text "matchType:" <+> Pretty.ppName penv name <.> text "," <+> Pretty.ppType penv expect <+> text "~" <+> Pretty.ppType penv (infoType info)
                     runUnify (subsume range free expect (infoType info))
           case res of
             (Right (rho,_,_),_)  -> return [(name,info,rho)]
             (Left _,_)             -> return []

    matchNamedArgs :: Bool -> Int -> [Name] -> Maybe Type -> (Name,NameInfo) -> Inf [(Name,NameInfo,Rho)]
    matchNamedArgs matchSome n named mbResTp (name,info)
      = do free <- freeInGamma
           res <- runUnify (matchNamed matchSome range free (infoType info) n named mbResTp)
           case res of
             (Right rho,_)  -> return [(name,info,rho)]
             (Left _,_)     -> return []

    matchArgs :: Bool -> [Type] -> [(Name,Type)] -> Maybe Type -> (Name,NameInfo) -> Inf [(Name,NameInfo,Rho)]
    matchArgs matchSome fixed named mbResTp (name,info)
      = do free <- freeInGamma
          --  traceDefDoc $ \penv -> text "  match args fixed:" <+> list [Pretty.ppType penv fix | fix <- fixed]
          --                                 <+> text ", named" <+> list [Pretty.ppParam penv nametp | nametp <- named]
          --                                 <+> text "on" <+> Pretty.ppParam penv (name,infoType info)
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
  | CtxFunArgs Bool Int [Name] (Maybe Type)         -- ^ are only some arguments supplied?  @n@ fixed arguments and followed by the given named arguments and a possible result type.
  | CtxFunTypes Bool [Type] [(Name,Type)] (Maybe Type)  -- ^ are only some arguments supplied? fixed and named arguments, maybe a (propagated) result type
  deriving (Show)

ppNameContext :: Pretty.Env -> NameContext -> Doc
ppNameContext penv ctx
  = case ctx of
      CtxNone
        -> text "_"
      CtxType tp
        -> Pretty.ppType penv tp
      CtxFunArgs matchSome n names mbResTp
        -> -- text "CtxFunArgs" <+> pretty n <+> list [Pretty.ppName penv name | name <- names] <+> ppMbType penv mbResTp
           tupled ([text "?" | _ <- [1..n]] ++ [Pretty.ppName penv name <+> text ": ?" | name <- names]
                   ++ (if matchSome then [text "..."] else []))
           <+> text "->" <+> ppMaybeType mbResTp
      CtxFunTypes some fixed named mbResTp
        -> -- text "CtxFunTypes" <+> pretty some <+> list [Pretty.ppType penv atp | atp <- fixed]
           -- <+> list [Pretty.ppParam penv nt | nt <- named] <+> ppMbType penv mbResTp
           tupled ([Pretty.ppType penv ftp | ftp <- fixed] ++ [Pretty.ppParam penv nt | nt <- named]
                   ++ (if some then [text "..."] else [])) <+> text "->" <+> ppMaybeType mbResTp
  where
    ppMaybeType Nothing   = text "_"
    ppMaybeType (Just tp) = Pretty.ppType penv tp

ppNameCtx :: Pretty.Env -> (Name,NameContext) -> Doc
ppNameCtx penv (name,ctx) = Pretty.ppName penv name <+> text ":" <+> ppNameContext penv ctx


pureMatchShapeNameCtx :: NameContext -> NameContext -> Bool
pureMatchShapeNameCtx ctx1 ctx2
  = case (ctx1,ctx2) of
      (CtxType tp1, CtxType tp2)  -> pureMatchShape tp1 tp2
      (CtxFunTypes some1 fixed1 named1 mbResTp1, CtxFunTypes some2 fixed2 named2 mbResTp2 )
        -> (some1 == some2) && and (zipWith pureMatchShape fixed1 fixed2) && null named1 && null named2
           && (case (mbResTp1,mbResTp2) of
                 (Nothing,Nothing) -> True
                 (Just tp1, Just tp2) -> pureMatchShape tp1 tp2
                 _ -> False)
      _ -> False


-- Create a name context where the argument count is known (and perhaps some named arguments)
fixedCountContext :: Maybe (Type,Range) -> Int -> [Name] -> NameContext
fixedCountContext propagated fixedCount named
  = CtxFunArgs False fixedCount named (fmap fst propagated)

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
    tvars n  = mapM (\_ -> Op.freshStar) [1..n]

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
      = mapM (\name -> do { tv <- Op.freshStar; return (name,tv) }) named

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
  = vcat ([text ". Possible candidates: ",
           ppCandidates env infos]
          ++
          (if (null hint) then [] else [text "hint:" <+> text hint]))


ppCandidates :: Env -> [(Name,NameInfo)] -> Doc
ppCandidates env nameInfos
   = align $ table $ (if null rest
          then map (ppNameInfo env) defs
          else map (ppNameInfo env) (init defs) ++ [(text "...", text "or" <+> pretty (length rest + 1) <+> text "other definitions")])
  where
    penv = prettyEnv env
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

ppImplicitsHint :: Env -> [(Name,NameInfo)] -> [(Doc, Doc)]
ppImplicitsHint env nameInfos =
  case allRequiredMissingImplicits of
    [] -> []
    _  -> [(text "missing implicits" , hsep (map (Pretty.ppName penv) allRequiredMissingImplicits)),
            (text "hint", text "ensure implicit functions are defined prior to functions that require them")]
  where
    penv = prettyEnv env
    candidateMissingImplicits = map (map fst . requiresImplicits . infoType . snd) nameInfos
    allMissingImplicits = S.fromList (concat candidateMissingImplicits)
    allRequiredMissingImplicits = filter (\imp -> all (imp `elem`) candidateMissingImplicits) (S.toList allMissingImplicits)

ppNameInfo env (name,info)
  = (Pretty.ppName (prettyEnv env) (importsAlias name (imports env)), Pretty.ppType (prettyEnv env) (infoType info))

{--------------------------------------------------------------------------
  Implicit Constraints
--------------------------------------------------------------------------}

data ImplicitConstraint = ImplicitConstraint{ icName :: Name,     -- implicit constraint name (e.g. @hdiv)
                                              icType :: Type,
                                              icEvidence :: Name, -- fresh name for the implicit constraint evidence
                                              icContext :: Range,
                                              icRange :: Range,
                                              icCanSolve :: Tvs -> ImplicitConstraint -> Inf Bool,
                                              icSolve :: Tvs -> ImplicitConstraint -> Inf (Core.Expr,Type)
                                            }


instance HasTypeVar ImplicitConstraint where
  sub `substitute` ic
    = ic{ icType = sub `substitute` (icType ic) }
  ftv ic
    = ftv (icType ic)
  btv ic
    = btv (icType ic)
  ftc ic
    = ftc (icType ic)


instance Show ImplicitConstraint where
  show ic = show (icName ic)


ppConstraints :: Pretty.Env -> [ImplicitConstraint] -> Doc
ppConstraints penv ics
  = list (map (ppConstraint penv) ics)

ppConstraint :: Pretty.Env -> ImplicitConstraint -> Doc
ppConstraint penv ic
  = Pretty.ppName penv (icEvidence ic) <.> text "=" <+> Pretty.ppParam penv (icName ic, icType ic)

implicitConstraints :: [(Name,Name -> Type -> Maybe (Tvs -> ImplicitConstraint -> Inf Bool, Tvs -> ImplicitConstraint -> Inf (Core.Expr, Type)))]
implicitConstraints
  = [(nameHeapDiv, checkHeapDivConstraint)]

checkImplicitConstraint :: Name -> Type -> Range -> Range -> Inf (Maybe TypedArg)
checkImplicitConstraint name tp rangeContext range
  = case lookup name implicitConstraints of
      Just check
        -> case check name tp of
             Just (canResolve,resolve)
                -> do iarg <- addImplicitConstraint name tp canResolve resolve rangeContext range
                      return (Just iarg)
             Nothing -> return Nothing
      Nothing -> return Nothing

resolveImplicitConstraints :: Tvs -> [ImplicitConstraint] -> Inf (Core.Expr -> Core.Expr)
resolveImplicitConstraints free []  = return id
resolveImplicitConstraints free ics
  = do defs <- mapM resolve ics
       let fcore core = case core of
                          -- Core.Lam pars eff body -> Core.Lam pars eff (Core.makeDefsLet defs body)
                          -- Core.TypeLam tpars (Core.Lam pars eff body) -> Core.TypeLam tpars (Core.Lam pars eff (Core.makeDefsLet defs body))
                          _ -> Core.makeDefsLet defs core

       return fcore
  where
    resolve ic
      = do (evidence,tp) <- (icSolve ic) free ic
           solvedImplicitConstraint (icEvidence ic) tp
           return $ Core.makeTDef (Core.TName (icEvidence ic) tp) evidence


tryResolveImplicitConstraints :: Bool -> Tvs -> Inf (Core.Expr -> Core.Expr)
tryResolveImplicitConstraints close free
  = mapImplicitConstraints $ \ics -> tryResolve ([],[]) ics
  where
    tryResolve :: ([Core.Def],[ImplicitConstraint]) -> [ImplicitConstraint] -> Inf (Core.Expr -> Core.Expr,[ImplicitConstraint])
    tryResolve (defs,acc) []
      = do let fcore core = case core of
                              Core.Lam pars eff body -> Core.Lam pars eff (Core.makeDefsLet defs body)
                              Core.TypeLam tpars (Core.Lam pars eff body) -> Core.TypeLam tpars (Core.Lam pars eff (Core.makeDefsLet defs body))
                              _ -> Core.makeDefsLet defs core
           return (fcore, reverse acc)
    tryResolve (defs,acc) (ic:ics)
      = do determined <- (icCanSolve ic) free ic
           let force = -- let ftvs = fuv ic
                            --    generalized = tvsFilter (\tv -> not (tvsMember tv free)) ftvs
                            --in not (tvsIsEmpty generalized) || -- are any free variables about to be generalized?
                            --   tvsIsEmpty ftvs             -- or are no free variables left?
                            let freeTv = tvsList (fuv ic)
                            in null freeTv || not (all (\tv -> tvsMember tv free) freeTv)
           if determined || force || close
             then do (ev,tp) <- (icSolve ic) free ic
                     solvedImplicitConstraint (icEvidence ic) tp
                     let def = Core.makeTDef (Core.TName (icEvidence ic) tp) ev
                     tryResolve (def:defs, acc) ics
             else tryResolve (defs, ic:acc) ics



{--------------------------------------------------------------------------
  heap divergence constraints
--------------------------------------------------------------------------}

heapNeverContainedIn :: Tvs -> Type -> Type -> Bool
heapNeverContainedIn free hp0 tp
  = neverContainedIn tp
  where
    hp = expandSyn hp0
    neverContainedIn tp
      = case tp of
          TForall _ t           -> neverContainedIn t
          TFun tpars teff tres  -> all neverContainedIn (map snd tpars ++ [teff,tres])
          TApp t targs          -> all neverContainedIn (t:targs)
          TSyn _ targs t        -> all neverContainedIn (t:targs)
          TCon tcon             -> case hp of
                                     TCon hcon -> tcon /= hcon
                                     _ -> getKind tcon /= kindHeap
          TVar tvar             -> case hp of
                                     TVar htv -> -- if we are generalizing htv but not tvar, tvar can never contain htv
                                                 typevarFlavour tvar /= Meta ||
                                                 not (tvsMember htv free) && (tvsMember tvar free)
                                     _        -> -- but in all other case tvar might get a type containing htv
                                                  typevarFlavour tvar /= Meta


heapAlwaysContainedIn :: Tvs -> Type -> Type -> Bool
heapAlwaysContainedIn free hp0 tp
  = heapAlwaysContainedIn tp
  where
    hp = expandSyn hp0
    heapAlwaysContainedIn tp
      = case tp of
          TForall _ t           -> heapAlwaysContainedIn t
          TFun tpars teff tres  -> any heapAlwaysContainedIn (map snd tpars ++ [teff,tres])
          TApp t targs          -> any heapAlwaysContainedIn (t:targs)
          TSyn _ targs t        -> any heapAlwaysContainedIn (t:targs)
          TCon tcon             -> case hp of
                                     TCon hcon -> tcon == hcon
                                     _ -> False
          TVar tvar             -> case hp of
                                     TVar htv -> tvar == htv
                                     _        -> False


checkHeapDivConstraint :: Name -> Type -> Maybe (Tvs -> ImplicitConstraint -> Inf Bool, Tvs -> ImplicitConstraint -> Inf (Core.Expr, Type))
checkHeapDivConstraint name tp
  = case expandSyn tp of
      TApp (TCon tcon) [tpHeap,tpVal,tpEff]  | typeConName tcon == nameTypeHeapDiv
        -> Just (canResolveHeapDivConstraint,resolveHeapDivConstraint)
      _ -> Nothing

ppTvs :: Pretty.Env -> Tvs -> Doc
ppTvs penv tvs
  = list (map (Pretty.ppTypeVar penv) (tvsList tvs))


canResolveHeapDivConstraint :: Tvs -> ImplicitConstraint -> Inf Bool
canResolveHeapDivConstraint free ic
  = do icTp <- implicitConstraintType ic
       case expandSyn icTp of -- expand here again (should never fail!) so we get skolem substitutions
         TApp (TCon tcon) [tpHeap,tpVal,tpEff]
            -> do
              let never = heapNeverContainedIn free tpHeap tpVal
              let always = heapAlwaysContainedIn free tpHeap tpVal
              -- trace ("Check resolve\n" ++ show tpHeap ++ "\n" ++ show tpVal ++ "\n" ++ show never ++ " " ++ show always) $ return ()
              return (never || always)

implicitConstraintType :: HasCallStack => ImplicitConstraint -> Inf Type
implicitConstraintType ic
  = do ig <- iconstraintsGamma <$> getSt
       case infgammaLookup (icEvidence ic) ig of
         Right (_,nameInfo)
           -> subst (infoType nameInfo)  -- unlike icType, this may have substituted skolems
         _ -> failure "Type.InferMonad.implicitConstraintType" $ "unknown constraint: " ++ show (icEvidence ic)


resolveHeapDivConstraint :: Tvs -> ImplicitConstraint -> Inf (Core.Expr, Type)
resolveHeapDivConstraint free ic
  = do sic <- subst ic
       tp  <- implicitConstraintType ic
       case expandSyn tp of -- expand here again (should never fail!) so we get skolem substitutions
         TApp (TCon tcon) [tpHeap,tpVal,tpEff]
           -> do  -- traceDefDoc $ \penv -> text "resolveHeapDivConstraint:" <+> ppConstraint penv sic
                  --                        <-> text "  free:" <+> ppTvs penv free
                  --                        <-> text "  tvsHp:" <+> ppTvs penv tvsHp <.> text ", tvsTp:" <+> ppTvs penv tvsTp
                  maydiv <- if (heapNeverContainedIn free tpHeap tpVal)
                                -- not (tvsIsEmpty (ftv stp)))) -- conservative guess...
                              then return False
                              else do -- add div effect to tpEff
                                      tv <- Op.freshEffect
                                      let divEff = effectExtend typeDivergent tv
                                      inferUnify (Infer (icContext ic)) (icRange ic) tpEff divEff
                                      return True
                  (cname,ctype,cinfo) <- resolveNameEx isInfoCon Nothing
                                            (if maydiv then nameEvHeapDiv else nameEvHeapNoDiv) CtxNone (icContext ic) (icRange ic)
                                          -- resolveName nameEvHeapDiv Nothing (icRange ic)
                  seff <- subst tpEff
                  stp  <- subst tp
                  -- traceDefDoc $ \penv -> text "resolve @hdiv:" <+> Pretty.ppName penv (icEvidence ic) <.> colon <+> Pretty.ppType penv stp <+> text "as" <+> text (if maydiv then "divergent" else "non-divergent")
                  --                         <-> text "  , free: " <+> ppTvs penv free
                  let ev = Core.TypeApp (coreExprFromNameInfo cname cinfo) [tpHeap,tpVal,seff]
                  return (ev,stp)



{--------------------------------------------------------------------------
  Inference monad
--------------------------------------------------------------------------}

data Inf a  = Inf (Env -> St -> Res a)

data Res a  = Ok !a !St ![(Range,Doc)]
            | Err !(Range,Doc) ![(Range,Doc)]

data Env    = Env{ prettyEnv :: !Pretty.Env
                 , context  :: !Name  -- | current module name
                 , currentDefs :: ![Name]
                 , namedLam :: !Bool
                 , types :: !Newtypes
                 , synonyms :: !Synonyms
                 , gamma :: !Gamma
                 , infgamma :: !InfGamma
                 , imports :: !ImportMap
                 , returnAllowed :: !Bool
                 , inLhs :: !Bool
                 , hiddenTermDoc :: !(Maybe (Range,Doc))
                 , localDepth :: !Int   -- number of run-local scope's
                 , scopeNestingDepth :: !Int   -- nested scope level
                 , allowInfiniteChains :: !Bool
                 , niceNames :: !(NM.NameMap Doc)
                 }
data St     = St{ uniq :: !Int
                , sub :: !Sub                            -- current substitution
                , iconstraints :: ![ImplicitConstraint]  -- output
                , iconstraintsGamma :: !InfGamma         -- adding a constraint adds an implicit local evidence variable
                , holeAllowed :: !Bool                   -- is a hole allowed for a constructor context?
                , mbRangeMap :: !(Maybe RangeMap)         -- used for errors and IDE integration
                }


runInfer :: Pretty.Env -> Maybe RangeMap -> Synonyms -> Newtypes -> ImportMap -> Gamma -> Name -> Bool -> Int -> Inf a -> Error b (a,Int,Maybe RangeMap)
runInfer env mbrm syns newTypes imports assumption context allowInfiniteChains unique (Inf f)
  = case f (Env env context [] False newTypes syns assumption infgammaEmpty imports False False Nothing 0 0 allowInfiniteChains NM.empty)
           (St unique subNull [] infgammaEmpty False mbrm) of
      Err (rng,doc) warnings
        -> addWarnings (map (toWarning ErrType) warnings) (errorMsg (errorMessageKind ErrType rng doc))
      Ok x st warnings
        -> addWarnings (map (toWarning ErrType) warnings) (ok (x, uniq st, (sub st) |-> mbRangeMap st))


zapSubst :: HasCallStack => Inf ()
zapSubst
  = do env <- getEnv
       assertion "not an empty infgamma" (infgammaIsEmpty (infgamma env)) $
        do st <- getSt
           when (not (infgammaIsEmpty (iconstraintsGamma st))) $
            traceDefDoc $ \penv -> text "iconstraintsGamma:" <-> indent 2 (ppInfGamma penv (iconstraintsGamma st))
           updateSt (\st -> assertion "no empty iconstraints" (null (iconstraints st)) $
                            assertion "no empty iconstraints gamma" (infgammaIsEmpty (iconstraintsGamma st)) $
                            st{ sub = subNull, iconstraints = [], mbRangeMap = (sub st) |-> mbRangeMap st } ) -- this can be optimized further by splitting the rangemap into a 'substited part' and a part that needs to be done..
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


ignoreErrors :: Inf a -> Inf a -> Inf a
ignoreErrors (Inf onErrorRes) (Inf f)
  = Inf (\env st0 -> case f env st0 of
                       Err err ws -> case onErrorRes env st0 of
                                       Ok x st1 ws1 -> Ok x st1 ([err] ++ ws ++ ws1)  -- (st1{ mbRangeMap = mbMerge (mbRangeMap st0) (mbRangeMap st1)})
                                       Err err1 ws1 -> Err err1 ([err] ++ ws ++ ws1)
                       ok         -> ok)
  -- where
  --   mbMerge (Just rm1) (Just rm2) = Just $! (rangeMapAppend rm1 rm2)
  --   mbMerge _ mbRm2               = mbRm2

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
  = Inf (\env st -> Ok () (st{
          mbRangeMap = case (mbRangeMap st) of
                        Just rm -> Just (rangeMapInsert rng info rm)
                        rm      -> rm
        }) [])

withNoRangeInfo :: Inf a -> Inf a
withNoRangeInfo inf
  = do st0 <- updateSt (\st -> st{ mbRangeMap = Nothing })
       let rm0 = mbRangeMap st0
       x   <- inf
       updateSt ( \st -> st{ mbRangeMap = rm0 })
       return x

withLocalScope :: Inf a -> Inf a
withLocalScope inf
  = withEnv (\env -> env{ localDepth = localDepth env + 1 }) inf

withNoLocalScope :: Inf a -> Inf a
withNoLocalScope inf
  = withEnv (\env -> env{ localDepth = 0 }) inf

localScopeDepth :: Inf Int
localScopeDepth
  = do env <- getEnv
       return (localDepth env)

withScope :: Inf a -> Inf a
withScope inf
  = withEnv (\env -> env{ scopeNestingDepth = scopeNestingDepth env + 1 }) inf

getScopeDepth :: Inf Int
getScopeDepth
  = do env <- getEnv
       return (scopeNestingDepth env)

withNiceNames :: (Name -> Int -> Doc) -> [Name] -> ([Doc] -> Inf a) -> Inf a
withNiceNames create names finf
  = do env <- getEnv
       let n   = NM.size (niceNames env)
           nms = [(name, create name i) | (i,name) <- zip [n..] names]
           env'= env{ niceNames = NM.union (niceNames env) (NM.fromList nms) }
       withEnv (\_ -> env') $ finf (map snd nms)

lookupNiceName :: Name -> Inf (Maybe Doc)
lookupNiceName name
  = do env <- getEnv
       return (NM.lookup name (niceNames env))


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

inHiddenTermDoc :: Inf Bool
inHiddenTermDoc
  = do env <- getEnv
       case hiddenTermDoc env of
         Just _ -> return True
         _      -> return False

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
  = holeAllowed <$> updateSt (\st -> st{ holeAllowed = False } )

disallowHole :: Inf a -> Inf a
disallowHole action
  = do st0 <- updateSt (\st -> st{ holeAllowed = False })
       let prev = holeAllowed st0
       x <- action
       updateSt (\st -> st{ holeAllowed = prev })
       return x

allowHole :: Inf a -> Inf (a,Bool {- was the hole used? -})
allowHole action
  = do prev <- holeAllowed <$> updateSt (\st -> st{ holeAllowed = True })
       x <- action
       allowed <- holeAllowed <$> updateSt (\st -> st{ holeAllowed = prev })
       return (x,not allowed)


-- implicit constraint evidence name?
isImplicitConstraintEvidenceName :: Name -> Bool
isImplicitConstraintEvidenceName name
  = nameStartsWith name "iev@"

-- add a new implicit constraint with a fresh name (to be solved at generalization time)
addImplicitConstraint :: Name -> Type -> (Tvs -> ImplicitConstraint -> Inf Bool) -> (Tvs -> ImplicitConstraint -> Inf (Core.Expr, Type)) -> Range -> Range -> Inf TypedArg
addImplicitConstraint name tp canSolve solve context rng
  = do evName <- Core.freshName "iev"
       let ic       = ImplicitConstraint name tp evName context rng canSolve solve
           nameInfo = createNameInfoX Public evName 2 DefVal rng tp ""
           iarg     = (evName,nameInfo,tp)
       updateSt (\st -> st{ iconstraints = ic : iconstraints st,
                            iconstraintsGamma = infgammaExtend evName nameInfo (iconstraintsGamma st) })
       -- traceDefDoc $ \penv -> text "add implicit constraint:" <+> ppConstraint penv ic
       return iarg

solvedImplicitConstraint :: Name -> Type -> Inf ()
solvedImplicitConstraint evName evTp
  = do -- traceDefDoc $ \penv -> text "discharge implicit constraint:" <+> Pretty.ppParam penv (evName,evTp)
       updateSt (\st -> st{ iconstraintsGamma = infgammaDelete evName (iconstraintsGamma st) })
       return ()


getImplicitConstraints :: Inf [ImplicitConstraint]
getImplicitConstraints
  = do st <- getSt
       subst (iconstraints st)

mapImplicitConstraints :: ([ImplicitConstraint] -> Inf (a,[ImplicitConstraint])) -> Inf a
mapImplicitConstraints f
  = do ics0 <- iconstraints <$> updateSt (\st -> st{ iconstraints = [] })
       (x,ics1) <- f ics0
       updateSt (\st -> st{ iconstraints = ics1 ++ iconstraints st })
       return x

scopeImplicitConstraints :: Inf a -> Inf a
scopeImplicitConstraints inf
  = do ics0 <- iconstraints <$> updateSt (\st -> st{ iconstraints = [] })
       -- traceDefDoc $ \penv -> text "scope ics:" <+> ppConstraints penv ics0
       x    <- traceIndent $ inf
       ics1 <- getImplicitConstraints
       --traceDefDoc $ \penv -> text "end scope: new ics:" <+> ppConstraints penv ics1 <+> text "++" <+> ppConstraints penv ics0
       updateSt (\st -> st{ iconstraints = ics1 ++ ics0 })
       return x

-- apply skolem substitution to unresolved implicit constraints
substImplicitConstraints :: Sub -> Inf ()
substImplicitConstraints sksub
  = do updateSt (\st -> st{ iconstraintsGamma = (sksub |-> (sub st |-> iconstraintsGamma st)) })
       ig <- iconstraintsGamma <$> getSt
       -- traceDefDoc $ \penv -> text "subst ics:" <+> Pretty.ppSub penv sksub <-> indent 2 (ppInfGamma penv{Pretty.showIds=True} ig)
       return ()



getSub :: Inf Sub
getSub
  = do st <- getSt
       return (sub st)

subst :: (HasCallStack,HasTypeVar a) => a -> Inf a
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
  = do d <- getScopeDepth
       extendGamma isAlreadyCanonical (nameInfos d coreGroup) (extendGammaCore isAlreadyCanonical coreDefss inf)
  where
    nameInfos d (Core.DefRec defs)    = map (\def -> coreDefInfoX def d) defs
    nameInfos d (Core.DefNonRec def)
      = [coreDefInfoX def d]  -- used to be coreDefInfo

-- Specialized for recursive defs where we sometimes get InfoVal even though we want InfoFun? is this correct for the csharp backend?
coreDefInfoX def@(Core.Def name tp expr vis sort inl nameRng doc) scopeDepth
  = (name {- nonCanonicalName name -}, createNameInfoX Public name scopeDepth sort nameRng tp doc)

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


    checkNoOverlap :: Name -> Name -> NameInfo -> (Name,NameInfo) -> Inf ()
    checkNoOverlap ctx name info (name2,info2)
      = do checkCasingOverlap (infoRange info) name name2 info
           free <- freeInGamma
           res  <- runUnify (overlaps (infoRange info) free (infoType info) (infoType info2))
           case fst res of
            Right _ ->
              do env <- getEnv
                 let [nice1,nice2] = Pretty.niceTypes (prettyEnv env) [infoType info,infoType info2]
                     (_,rho1)      = splitTypeScheme (infoType info)
                     (_,rho2)      = splitTypeScheme (infoType info2)
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
  = do d <- getScopeDepth
       extendInfGammaEx topLevel [] (extracts d coreDefs) (extendInfGammaCore topLevel coreDefss inf)
  where
    extracts d (Core.DefRec defs) = map (extract d) defs
    extracts d (Core.DefNonRec def) = [extract d def]
    extract d def
      = coreDefInfo def d -- (Core.defName def,(Core.defNameRange def, Core.defType def, Core.defSort def))

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
              -> do case (infgammaLookup name infgamma) of
                      Right (cname,info2) | cname /= nameReturn  -- TODO: adapt to multiple matches?
                        -> do checkCasingOverlap range name cname info2
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
       name <- uniqueNameFrom defName
       d <- getScopeDepth
       extendInfGamma [(name,(InfoVal Public name tp d range False False ""))] inf

currentDefName :: Inf Name
currentDefName
  = do dnames <- currentDefNames
       case dnames of
         (dname:_) -> return dname
         _         -> return (newName "")

currentDefNames :: Inf [Name]
currentDefNames
  = do env <- getEnv
       return (currentDefs env)

withDefName :: Name -> Inf a -> Inf a
withDefName name inf
  = withEnv (\env -> env{ currentDefs = name : currentDefs env, namedLam = not (nameIsNil name || isWildcard name) }) inf

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
       case infgammaLookup (unqualify name) (infgamma env) of
         Right (name,info)  -> return (Just (name,infoType info))
         Left []            -> return Nothing
         Left infos -> do def <- currentDefName
                          failure ("InferMonad.lookupInfName: ambigous local? " ++ show def ++ ": " ++ show name ++ ":\n" ++ unlines (map show infos))


findDataInfo :: Name -> Inf DataInfo
findDataInfo typeName
  = do env <- getEnv
       case newtypesLookupAny typeName (types env) of
         Just info -> return info
         Nothing   -> failure ("Type.InferMonad.findDataInfo: unknown type: " ++ show typeName ++ "\n in: " ++ show (types env))

traceIndent :: Inf a -> Inf a
traceIndent inf
  = withEnv (\env -> env{ prettyEnv = (prettyEnv env){ Pretty.indentation = Pretty.indentation (prettyEnv env) + 2 } }) inf

traceDefDoc :: (Pretty.Env -> Doc) -> Inf ()
traceDefDoc f
  = do dnames <- currentDefNames
       traceDoc (\penv -> hcat (intersperse (text ".") (map (Pretty.ppName penv) dnames)) <+> text ":" <+> f penv)

traceDoc :: (Pretty.Env -> Doc) -> Inf ()
traceDoc f
  = do penv <- getPrettyEnv
       trace (show (indent (Pretty.indentation penv) $ f penv)) $ return ()

ppNameType penv (name,tp)
  = Pretty.ppName penv name <+> colon <+> Pretty.ppType penv tp

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = seqqList . concat <$> mapM f xs

-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Type.InferMonad( Inf, InfGamma
                      , runInfer, tryRun
                      
                      -- * substitutation
                      , zapSubst
                      , subst, extendSub
                      
                      -- * Environment
                      , getGamma
                      , extendGamma, extendGammaCore
                      , extendInfGamma, extendInfGammaCore
                      , withGammaType
                      
                      -- * Name resolution
                      , qualifyName
                      , resolveName, resolveNameEx
                      , resolveFunName
                      , resolveConName
                      , lookupConName
                      , lookupFunName
                      , lookupNameEx, NameContext(..), maybeToContext
                      , lookupInfName
                      , getModuleName
                      , lookupImportName
                      , lookupNameN
                      , findDataInfo
                      , withDefName
                      , currentDefName
                      
                      -- * Misc.
                      , allowReturn, isReturnAllowed
                      , withLhs, isLhs
                      , getPrettyEnv
                      , splitEffect
                      , occursInContext

                      -- * Operations
                      , generalize
                      , improve
                      , instantiate, instantiateNoEx
                      , checkEmptyPredicates
                      , checkCasing
                      , normalize
                      
                      -- * Unification
                      , Context(..)
                      , inferUnify, inferUnifies
                      , inferSubsume
                      , withSkolemized

                      , typeError
                      , contextError
                      , termError
                      , infError, infWarning

                      -- * Documentation, Intellisense
                      , addRangeInfo
                      ) where

import Data.List( partition, sortBy)
import Control.Applicative
import Control.Monad
import Lib.PPrint
import Common.Range
import Common.Unique
import Common.Failure
import Common.Error
import Common.Name
import Common.NamePrim(nameTpVoid,nameTpPure,nameTpIO,nameTpST,nameTpAsyncX,nameTpRead,nameTpWrite,namePredHeapDiv,nameReturn)
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

import Type.Operations hiding (instantiate, instantiateNoEx)
import qualified Type.Operations as Op
import Type.Assumption
import Type.InfGamma

import Type.Unify
import Common.Message( docFromRange, table, tablex) 

import Core.Pretty()

import Syntax.RangeMap( RangeMap, RangeInfo(..), rangeMapInsert )

import qualified Lib.Trace( trace ) 

trace s x =
  -- Lib.Trace.trace (" " ++ s)
   x

{--------------------------------------------------------------------------
  Generalization
--------------------------------------------------------------------------}
generalize :: Range -> Range -> Effect -> Rho -> Core.Expr -> Inf (Scheme,Core.Expr )
generalize contextRange range eff  tp@(TForall _ _ _)  core0
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
             do (rho,tvars,icore) <- instantiate range stp
                generalize contextRange range seff rho (icore core0)
     
generalize contextRange range eff0 rho0 core0
  = do seff <- subst eff0
       srho  <- subst rho0
       free0 <- freeInGamma
       let free = tvsUnion free0 (fuv seff)
       ps0  <- splitPredicates free
       score0 <- subst core0

       sub <- getSub
        -- trace ("generalize: " ++ show (pretty seff,pretty srho) ++ " with " ++ show ps0 
                  {- ++ " and free " ++ show (tvsList free) -} 
                  {- ++ "\n subst=" ++ show (take 10 $ subList sub) -} 
                  {- ++ "\ncore: " ++ show score0 -} 
            -- return ()
       -- simplify and improve predicates
       (ps1,(eff1,rho1),core1) <- simplifyAndResolve contextRange free ps0 (seff,srho) 
       -- trace (" improved to: " ++ show (eff1,rho1) ++ " with " ++ show ps1 ++ " and free " ++ show (tvsList free) {- ++ "\ncore: " ++ show score0 -}) $ return ()
       let -- generalized variables
           tvars0 = filter (\tv -> not (tvsMember tv free)) (ofuv (TForall [] (map evPred ps1) rho1))

       if (null tvars0) 
        then do addPredicates ps1 -- add them back to solve later (?)
                score <- subst (core1 core0)
                
                -- substitute more free variables in the core with ()
                let score1 = substFree free score
                nrho <- normalizeX free rho1
                trace ("generalized to (as rho type): " ++ show (pretty nrho)) $ return () 
                return (nrho,score1)

        else do -- check that the computation is total
                inferUnify (Check "Generalized values cannot have an effect" contextRange) range typeTotal eff1
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

                nrho <- normalizeX free rho5
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
                trace ("generalized to: " ++ show (pretty resTp)) $ return () 
                return (resTp, core6)

  where
    substFree free core
      = let fvars = tvsDiff (ftv core) free
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


improve :: Range -> Range -> Effect -> Rho -> Core.Expr -> Inf (Rho,Effect,Core.Expr )
improve contextRange range eff0 rho0 core0
  = do seff  <- subst eff0
       srho  <- subst rho0
       free  <- freeInGamma
       -- let free = tvsUnion free0 (fuv seff)
       sps    <- splitPredicates free
       score0 <- subst core0
       -- trace (" improve: " ++ show (Pretty.niceTypes Pretty.defaultEnv [seff,srho]) ++ " with " ++ show sps ++ " and free " ++ show (tvsList free) {- ++ "\ncore: " ++ show score0 -}) $ return ()
       
       -- isolate: do first to discharge certain hdiv predicates.
       -- todo: in general, we must to this after some improvement since that can lead to substitutions that may enable isolation..
       (ps0,eff0,coref0) <- isolate (tvsUnions [free,ftv srho]) sps seff

       -- simplify and improve predicates
       (ps1,(eff1,rho1),coref1) <- simplifyAndResolve contextRange free ps0 (eff0,srho) 
       addPredicates ps1  -- add unsolved ones back
       -- isolate
       -- (eff2,coref2) <- isolate (tvsUnions [free,ftv rho1,ftv ps1]) eff1 
       
       (nrho) <- normalizeX free rho1
       -- trace (" improve normalized: " ++ show (nrho) ++ " from " ++ show rho1) $ return ()
       -- trace (" improved to: " ++ show (eff1,nrho) ++ " with " ++ show ps1) $ return ()
       return (nrho,eff1,coref1 (coref0 core0))

instantiate :: Range -> Scheme -> Inf (Rho,[TypeVar],Core.Expr -> Core.Expr)
instantiate range tp | isRho tp
  = do (rho,coref) <- Op.extend tp
       return (rho,[],coref)
instantiate range tp 
  = do (tvars,ps,rho,coref) <- instantiateEx range tp
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
isolate :: Tvs -> [Evidence] -> Effect -> Inf ([Evidence],Effect, Core.Expr -> Core.Expr)
isolate free ps eff
  = -- trace ("isolate: " ++ show eff ++ " with free " ++ show (tvsList free)) $
    let (ls,tl) = extractOrderedEffect eff
    in case filter (\l -> labelName l `elem` [nameTpRead,nameTpWrite]) ls of
          (TApp _ [TVar h] : _) 
            -> -- has heap variable 'h' in its effect
               do (polyPs,ps1) <- splitHDiv h ps
                  if not (tvsMember h free || tvsMember h (ftv ps1))
                    then do -- yeah, we can isolate, and discharge the polyPs hdiv predicates
                            tv <- freshTVar kindEffect Meta
                            (Just syn) <- lookupSynonym nameTpST
                            let [bvar] = synInfoParams syn
                                st     = subNew [(bvar,TVar h)] |-> synInfoType syn
                            nofailUnify $ unify (effectExtend st tv) eff
                            neweff <- subst tv
                            sps    <- subst ps1
                            -- return (sps, neweff, id) -- TODO: supply evidence (i.e. apply the run function)
                            -- and try again
                            isolate free sps neweff
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

normalize :: Rho -> Inf Rho
normalize tp
  = do free <- freeInGamma
       normalizeX free tp

normalizeX :: Tvs -> Rho -> Inf Rho
normalizeX free tp
  = case tp of
      TForall [] [] t
        -> normalizeX free t
      TSyn syn targs t
        -> do t' <- normalizeX free t
              return (TSyn syn targs t')
      TFun args eff res
        -> do (ls,tl) <- nofailUnify $ extractNormalizeEffect eff
              eff'    <- case expandSyn tl of
                          -- remove tail variables in the result type 
                          (TVar tv) | isMeta tv && not (tvsMember tv free) && not (tvsMember tv (ftv (res:map snd args)))
                            -> trace ("close effect: " ++ show (pretty tp)) $
                                return (effectFixed ls)
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
        -> trace (" resolveHeapDiv: " ++ show (hp,tp,eff)) $
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
         Right (t,ps,coref) -> do addPredicates ps
                                  return (t,coref)
         Left err         -> do unifyError context range err sexp stp 
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
unifyError context range err xtp1 xtp2
  = do free <- freeInGamma
       tp1 <- subst xtp1 >>= normalizeX free
       tp2 <- subst xtp2 >>= normalizeX free
       env <- getEnv
       unifyError' (prettyEnv env) context range err tp1 tp2

unifyError' env context range err tp1 tp2
  = do infError range $
        text message <->
        table ([(text "context", docFromRange (Pretty.colors env) rangeContext)
               ,(text "term", docFromRange (Pretty.colors env) range)
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
          NoSubsume       -> [(text "is less polymorph as",nice1)]
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
          NoSubsume   -> ("type is not polymorph enough",[])
          NoEntail    -> ("predicates cannot be resolved",[])
          Infinite    -> ("types do not match (due to an infinite type)",[(text "hint",text "annotate the function definition?")])
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
  = do infError range $
        text message <->
        table  [(text "context", docFromRange (Pretty.colors env) contextRange)
               ,(text "origin", docFromRange (Pretty.colors env) range)
               ,(text "constraint", nicePred)
               ]
  where
    nicePred  = Pretty.ppPred env pred


typeError :: Range -> Range -> Doc -> Type -> [(Doc,Doc)] -> Inf ()
typeError contextRange range message xtp extra
  = do env  <- getEnv
       free <- freeInGamma
       tp   <- subst xtp >>= normalizeX free
       typeError' (prettyEnv env) contextRange range message tp extra

typeError' env contextRange range message tp extra
  = do infError range $
        message <->
        table ([(text "context", docFromRange (Pretty.colors env) contextRange)
              ,(text "term", docFromRange (Pretty.colors env) range)
              ,(text "inferred type", Pretty.niceType env tp)
              ] ++ extra)

contextError :: Range -> Range -> Doc -> [(Doc,Doc)] -> Inf ()
contextError contextRange range message extra
  = do env <- getEnv
       contextError' (prettyEnv env) contextRange range message extra

contextError' env contextRange range message extra
  = do infError range $
        message <->
        table  ([(text "context", docFromRange (Pretty.colors env) contextRange)
                ,(text "term", docFromRange (Pretty.colors env) range)
                ]
                ++ extra)

termError :: Range -> Doc -> Type -> [(Doc,Doc)] -> Inf ()
termError range message tp extra
  = do env <- getEnv
       termError' (prettyEnv env) range message tp extra

termError' env range message tp extra
  = do infError range $
        message <->
        table  ([(text "term", docFromRange (Pretty.colors env) range)
                ,(text "inferred type", Pretty.niceType env tp)
                ]
                ++ extra)
    

    
{--------------------------------------------------------------------------
  Inference monad
--------------------------------------------------------------------------}
data Inf a  = Inf (Env -> St -> Res a)
data Res a  = Ok !a !St ![(Range,Doc)]
            | Err !(Range,Doc) ![(Range,Doc)]
data Env    = Env{ prettyEnv :: !Pretty.Env
                 , context  :: !Name  -- | current module name
                 , currentDef :: !Name
                 , types :: !Newtypes
                 , synonyms :: !Synonyms
                 , gamma :: !Gamma
                 , infgamma :: !InfGamma 
                 , imports :: !ImportMap
                 , returnAllowed :: Bool
                 , inLhs :: Bool
                 }
data St     = St{ uniq :: !Int, sub :: !Sub, preds :: ![Evidence], mbRangeMap :: Maybe RangeMap }


runInfer :: Pretty.Env -> Maybe RangeMap -> Synonyms -> Newtypes -> ImportMap -> Gamma -> Name -> Int -> Inf a -> Error (a,Int,Maybe RangeMap)
runInfer env mbrm syns newTypes imports assumption context unique (Inf f)
  = case f (Env env context (newName "") newTypes syns assumption infgammaEmpty imports False False) (St unique subNull [] mbrm) of
      Err err warnings -> addWarnings warnings (errorMsg (ErrorType [err]))
      Ok x st warnings -> addWarnings warnings (ok (x,uniq st, (sub st) |-> mbRangeMap st))


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
  pure  = return
  (<*>) = ap

instance Monad Inf where
  return x        = Inf (\env st -> Ok x st [])
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

isLhs :: Inf Bool
isLhs
  = do env <- getEnv
       return (inLhs env)

isReturnAllowed :: Inf Bool
isReturnAllowed
  = do env <- getEnv
       return (returnAllowed env)

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
coreDefInfoX def@(Core.Def name tp expr vis sort nameRng doc)
  = (Core.nonCanonicalName name, createNameInfoX name sort nameRng tp)

extendGamma :: Bool -> [(Name,NameInfo)] -> Inf a -> Inf (a)
extendGamma isAlreadyCanonical defs inf
  = do env <- getEnv
       (gamma') <- extend (context env) defs (gamma env)
       withEnv (\env -> env{ gamma = gamma' }) inf
  where
    extend ctx [] (gamma)
      = return (gamma)
    extend ctx ((name,info):rest) (gamma)
      = do let matches = gammaLookup name gamma
               localMatches = [(qname,info) | (qname,info) <- matches, not (isInfoImport info), qualifier qname == ctx || qualifier qname == nameNil, isSameNamespace qname name ]
           mapM (checkNoOverlap ctx name info) localMatches
           -- trace (" extend gamma: " ++ show (name,info)) $
           let (cinfo) 
                   = -- if null localMatches then (info) else
                    if (isAlreadyCanonical) then info else
                       let cname = Core.canonicalName (length localMatches) (if isQualified name then name else qualify ctx name)
                       in case info of
                            InfoVal{} -> info{ infoCName = cname }  -- during recursive let's we use InfoVal sometimes for functions..
                            InfoFun{} -> info{ infoCName = cname }
                            InfoExternal{} -> info{ infoCName = cname }
                            _ -> info
           -- Lib.Trace.trace (" extend gamma: " ++ show (pretty name, pretty (infoType info), show cinfo) ++ " with " ++ show (infoCanonicalName name cinfo) ++ " (matches: " ++ show (length matches,ctx,map fst matches)) $ 
           extend ctx rest (gammaExtend name cinfo gamma)
           
     
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
  = extendInfGamma topLevel (extracts coreDefs) (extendInfGammaCore topLevel coreDefss inf)
  where
    extracts (Core.DefRec defs) = map extract defs
    extracts (Core.DefNonRec def) = [extract def]
    extract def
      = coreDefInfo def -- (Core.defName def,(Core.defNameRange def, Core.defType def, Core.defSort def))

extendInfGamma :: Bool -> [(Name,NameInfo)] -> Inf a -> Inf a
extendInfGamma topLevel tnames inf
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
                              infWarning range (Pretty.ppName (prettyEnv env) name <+> text "shadows an earlier local definition or parameter")
                      _ -> return ()
           extend ctx gamma (x:seen) rest (infgammaExtend qname (info{ infoCName =  if topLevel then createCanonicalName ctx gamma qname else qname}) infgamma)

createCanonicalName ctx gamma qname 
  = let matches = gammaLookup (unqualify qname) gamma
        localMatches = [(qname,info) | (qname,info) <- matches, not (isInfoImport info), qualifier qname == ctx || qualifier qname == nameNil ]
        cname = Core.canonicalName (length localMatches) qname
    in cname

withGammaType :: Range -> Type -> Inf a -> Inf a
withGammaType range tp inf
  = do defName <- currentDefName
       name <- uniqueName (show defName)
       extendInfGamma False [(name,(InfoVal name tp range False))] inf

currentDefName :: Inf Name
currentDefName
  = do env <- getEnv
       return (currentDef env)

withDefName :: Name -> Inf a -> Inf a
withDefName name inf
  = withEnv (\env -> env{ currentDef = name }) inf

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
       return (fuv (sub |-> (infgamma env)))

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

findDataInfo :: Name -> Inf DataInfo
findDataInfo typeName
  = do env <- getEnv
       case newtypesLookup typeName (types env) of
         Just info -> return info
         Nothing   -> failure ("Type.InferMonad.findDataInfo: unknown type: " ++ show typeName ++ "\n in: " ++ show (types env))

-- | Lookup a name with a certain type and return the fully qualified name and its type
resolveName :: Name -> Maybe(Type,Range) -> Range -> Inf (Name,Type,NameInfo)
resolveName name mbType range 
  = case mbType of
      Just (tp,ctxRange) -> resolveNameEx infoFilter (Just infoFilterAmb) name (CtxType tp) ctxRange range
      Nothing            -> resolveNameEx infoFilter (Just infoFilterAmb) name CtxNone range range
  where
    infoFilter = isInfoValFunExt 
    infoFilterAmb = not . isInfoImport

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
  = do matches <- lookupNameEx infoFilter name ctx range
       case matches of
        []   -> do amb <- case ctx of
                            CtxNone -> return []
                            _       -> lookupNameEx infoFilter name CtxNone range
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
                    (CtxFunArgs fixed named, (_:rest))
                      -> do let message = "takes " ++ show (fixed + length named) ++ " argument(s)" ++
                                          (if null named then "" else " with such parameter names")
                            infError range (text "no function" <+> Pretty.ppName penv name <+> text message <> ppAmbiguous env "" amb)
                    (CtxFunTypes partial fixed named, (_:rest))
                      -> do let docs = Pretty.niceTypes penv (fixed ++ map snd named)
                                fdocs = take (length fixed) docs
                                ndocs = [color (colorParameter (Pretty.colors penv)) (pretty n <+> text ":") <+> tpdoc |
                                           ((_,n),tpdoc) <- zip named (drop (length fixed) docs)]
                                pdocs = if partial then [text "..."] else []
                                argsDoc = color (colorType (Pretty.colors penv)) $
                                           parens (hsep (punctuate comma (fdocs ++ ndocs ++ pdocs))) <+>
                                           text "-> ..."
                            infError range (text "no function" <+> Pretty.ppName penv name <+> text "is defined that matches the argument types" <->
                                         table (ctxTerm rangeContext ++ 
                                                [(text "inferred type", argsDoc)
                                                ,(text "candidates", align (tablex 0 (ppCandidates env  "" amb)))]))

                    _ -> do amb2 <- case mbInfoFilterAmb of
                                      Just infoFilterAmb -> lookupNameEx infoFilterAmb name ctx range
                                      Nothing            -> return []
                            case amb2 of
                              (_:_)
                                -> infError range ((text "identifier" <+> Pretty.ppName penv name <+> text "is undefined") <->
                                                   (text "perhaps you meant: " <> ppOr penv (map fst amb2)))
                              _ -> infError range (text "identifier" <+> Pretty.ppName penv name <+> text "is undefined")
                    
        [(qname,info)]  
           -> do checkCasing range name qname info
                 return (qname,infoType info,info)
        _  -> do env <- getEnv
                 infError range (text "identifier" <+> Pretty.ppName (prettyEnv env) name <+> text "is ambiguous" <> ppAmbiguous env hintTypeSig matches)
  where
    hintTypeSig = "give a type annotation to the function parameters or arguments"
                                       
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
    in if (nameCaseOverlap ((if isQualified name then id else unqualify) (Core.nonCanonicalName qname1)) name) 
        then Just qname1
        else Nothing
    
ppOr :: Pretty.Env -> [Name] -> Doc
ppOr env []     = Lib.PPrint.empty
ppOr env [name] = Pretty.ppName env name
ppOr env names  = hcat (map (\name -> Pretty.ppName env name <> text ", ") (init names)) <+> text "or" <+> Pretty.ppName env (last names)


ppAmbiguous :: Env -> String -> [(Name,NameInfo)] -> Doc
ppAmbiguous env hint infos
  = text ". Possible candidates: " <-> table (ppCandidates env hint infos)

ppCandidates :: Env -> String -> [(Name,NameInfo)] -> [(Doc,Doc)]
ppCandidates env hint nameInfos
   = let penv = prettyEnv env
         modName = context env
         n = 6
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
  


lookupImportName :: Name -> Range -> Inf (Maybe (Name,NameInfo))
lookupImportName name range
  = do matches <- lookupNameEx (const True) name CtxNone range
       case matches of
        [] -> do env <- getPrettyEnv
                 infError range (text "identifier" <+> Pretty.ppName env name <+> text "is undefined")
                 return Nothing
        _  -> case filter (isInfoImport . snd) matches of
                []         -> return Nothing
                [(_,info)] -> return (Just (infoFullName info, info))
                _          -> failure ("Type.InferMonad.lookupImportName: " ++ show name ++ ": is ambiguous?")

lookupConName :: Name -> Maybe (Type) -> Range -> Inf (Maybe (Name,Type,NameInfo))
lookupConName name mbType range
  = do matches <- lookupNameEx isInfoCon name (maybeToContext mbType) range
       case matches of
        []   -> return Nothing
        [(name,info)]  -> return (Just (name,infoType info,info))
        _    -> do env <- getEnv
                   infError range (text "constructor" <+> Pretty.ppName (prettyEnv env) name <+> text "is ambiguous" <> ppAmbiguous env hintQualify matches)
  where
    hintQualify = "qualify the constructor name to disambiguate it"

lookupFunName :: Name -> Maybe (Type,Range) -> Range -> Inf (Maybe (Name,Type,NameInfo))
lookupFunName name mbType range
  = do matches <- lookupNameEx isInfoFun name (maybeRToContext mbType) range
       case matches of
        []   -> return Nothing
        [(name,info)]  -> return (Just (name,infoType info,info))
        _    -> do env <- getEnv
                   infError range (text "identifier" <+> Pretty.ppName (prettyEnv env) name <+> text "is ambiguous" <> ppAmbiguous env hintQualify matches)
  where
    hintQualify = "qualify the name to disambiguate it"

lookupNameN :: Name -> Int -> [Name] -> Range -> Inf [(Name,NameInfo)]
lookupNameN name fixed named range
  = lookupNameEx (const True) name (CtxFunArgs fixed named) range
  {-
    do matches <-
       case matches of
         []         -> do amb <- lookupNameEx isInfoFun name CtxNone range
                          env <- getEnv
                          if null amb 
                           then infError range (text "identifier" <+> Pretty.ppName (prettyEnv env) name <+> text "is undefined")
                           else infError range (text "no function" <+> Pretty.ppName (prettyEnv env) name 
                                                <+> text "accepts" <+> (pretty (fixed + length named)) <+> text "arguments" 
                                                <> ppAmbiguous env "" amb)
                          return []
         _          -> return matches
-}

lookupInfName :: Name -> Inf (Maybe (Name,Type))
lookupInfName name
  = do env <- getEnv
       return (infgammaLookup name (infgamma env))

maybeToContext :: Maybe Type -> NameContext
maybeToContext mbType
  = case mbType of 
      Just tp -> CtxType tp
      Nothing -> CtxNone

maybeRToContext :: Maybe (Type,Range) -> NameContext
maybeRToContext mbTypeRange
  = maybeToContext (fmap fst mbTypeRange)

data NameContext
  = CtxNone       -- ^ just a name
  | CtxType Type  -- ^ a name that can appear in a context with this type
  | CtxFunArgs  Int [Name]          -- ^ function name with @n@ fixed arguments and followed by the given named arguments
  | CtxFunTypes Bool [Type] [(Name,Type)]  -- ^ are only some arguments supplied?, function name, with fixed and named arguments
  deriving (Show)

lookupNameEx :: (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [(Name,NameInfo)]
lookupNameEx infoFilter name ctx range 
  = -- trace ("lookup: " ++ show name) $
    do env <- getEnv
       -- trace (" in infgamma: " ++ show (ppInfGamma (prettyEnv env) (infgamma env))) $ return ()
       case infgammaLookupX name (infgamma env) of
         Just info  | infoFilter info  
                  -> do sinfo <- subst info
                        return [(infoCanonicalName name info, sinfo)] -- TODO: what about local definitions without local type variables or variables?
         _        -> -- trace ("gamma: " ++ show (ppGamma (prettyEnv env) (gamma env))) $
                     -- lookup global candidates
                     do let candidates = filter (infoFilter . snd) (gammaLookup name (gamma env))
                        case candidates of
                           [(qname,info)] -> return candidates
                           [] -> return [] -- infError range (Pretty.ppName (prettyEnv env) name <+> text "is undefined")
                           _  -> do checkCasingOverlaps range name candidates
                                    -- lookup global candidates that match the expected type
                                    matches <- case ctx of
                                                 CtxNone         -> return candidates
                                                 CtxType expect  -> do mss <- mapM (matchType expect) candidates
                                                                       return (concat mss)
                                                 CtxFunArgs n named -> do mss <- mapM (matchNamedArgs n named) candidates
                                                                          return (concat mss)
                                                 CtxFunTypes partial fixed named -> do mss <- mapM (matchArgs partial fixed named) candidates                                                                                       
                                                                                       return (concat mss)
                                    case matches of
                                      [(qname,info)] -> return matches
                                      _  -> do -- lookup global names defined in the current module
                                               {-
                                               let localMatches = [(qname,info) | (qname,info) <- matches, qualifier qname == context env]
                                               case localMatches of
                                                 [(qname,info)] -> return localMatches
                                                 _  ->
                                               -}
                                                       return matches
                                                       {- do 
                                                          let localCands = [(qname,info) | (qname,info) <- candidates, qualifier qname == context env]
                                                          case localCands of
                                                            [(qname,info)] -> return (qname,info)
                                                            _            -> return localCands -- infError range (Pretty.ppName (prettyEnv env) name <+> text "is ambiguous")
                                                          
                                                        -}
  where   
    matchType :: Type -> (Name,NameInfo) -> Inf [(Name,NameInfo)]
    matchType expect (name,info)
      = do free <- freeInGamma
           res <- runUnify (subsume range free expect (infoType info))
           case res of
             (Right _,_)  -> return [(name,info)]
             (Left _,_)   -> return []

    matchNamedArgs :: Int -> [Name] -> (Name,NameInfo) -> Inf [(Name,NameInfo)]
    matchNamedArgs n named (name,info)
      = do res <- runUnify (matchNamed range (infoType info) n named) 
           case res of
             (Right _,_)  -> return [(name,info)]
             (Left _,_)   -> return []

    matchArgs :: Bool -> [Type] -> [(Name,Type)] -> (Name,NameInfo) -> Inf [(Name,NameInfo)]
    matchArgs matchSome fixed named (name,info)
      = -- trace ("match args: " ++ show matchSome ++ ", " ++ show fixed ++ ", " ++ show (length named) ++ " on " ++ show (infoType info)) $
        do free <- freeInGamma
           res <- runUnify (matchArguments matchSome range free (infoType info) fixed named) 
           case res of
             (Right _,_)  -> return [(name,info)]
             (Left _,_)   -> return []

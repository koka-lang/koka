-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Definition of higher-ranked types and utility functions over them.
-----------------------------------------------------------------------------
module Type.Operations( instantiate
                      , instantiateEx, instantiateNoEx, extend
                      , skolemize, skolemizeEx
                      , freshTVar, freshTVars, freshEffect, freshStar
                      , freshSub
                      , isOptionalOrImplicit, splitOptionalImplicit, requiresImplicits
                      , hasOptionalOrImplicits
                      , useCommonAliases, realiasEffect, realiasType
                      ) where


import Debug.Trace
import Common.Range
import Common.Unique
import Common.Failure
import Kind.Kind
import Type.Type
import Type.TypeVar
import Lib.PPrint
import Type.Pretty
import Core.Core as Core
import Core.CoreVar
import Type.Assumption
import Kind.Synonym
import Common.NamePrim ( nameTpIO, nameTpIOC, nameTpIOCTotal, nameTpST, nameTpPure, nameTpAsync )

requiresImplicits :: Type -> [(Name, Type)]
requiresImplicits tp
  = case splitFunScheme tp of
      Just (_ ,pars,_,_) -> filter (isImplicitParamName . fst) pars
      _               -> []

isOptionalOrImplicit :: (Name,Type) -> Bool
isOptionalOrImplicit (pname,ptype)
  = isImplicitParamName pname || isOptional ptype

splitOptionalImplicit :: [(Name,Type)] -> ([(Name,Type)],[(Name,Type)],[(Name,Type)])
splitOptionalImplicit pars
  = let (fixed,rest) = span (not . isOptionalOrImplicit) pars
        (opts,named) = span (isOptional . snd) rest
    in (fixed,opts,named)

hasOptionalOrImplicits :: [(Name,Type)] -> Bool
hasOptionalOrImplicits pars
  = any isOptionalOrImplicit pars

--------------------------------------------------------------------------
-- Instantiation
--------------------------------------------------------------------------

-- | Instantiate a type
instantiate :: (HasCallStack,HasUnique m) => Range -> Type -> m Rho
instantiate range tp
  = do (ids,rho,coref) <- instantiateNoEx range tp
       return rho

-- | Instantiate a type and return the instantiated quantifiers, name/predicate pairs for evidence,
-- the instantiated type, and a core transformer function (which applies type arguments and evidence)
instantiateEx :: (HasCallStack,HasUnique m) => Range -> Type -> m ([TypeVar],Rho,Core.Expr -> Core.Expr)
instantiateEx rng tp
  = do (ids,rho,coref) <- instantiateExFl Meta rng tp
       (erho,coreg) <- extend rho
       return (ids,erho, coreg . coref)

-- | Instantiate a type and return the instantiated quantifiers, name/predicate pairs for evidence,
-- the instantiated type, and a core transformer function (which applies type arguments and evidence)
instantiateNoEx :: (HasCallStack,HasUnique m) => Range -> Type -> m ([TypeVar],Rho,Core.Expr -> Core.Expr)
instantiateNoEx rng tp
  = do (ids,rho,coref) <- instantiateExFl Meta rng tp
       return (ids,rho,coref)

-- | Ensure the result of function always gets an extensible effect type
-- This is necessary to do on instantiation since we simplify such effect variables
-- away during generalization. Effectively, the set of accepted programs does not
-- change but the types look simpler to the user.
extend :: (HasCallStack,HasUnique m) => Rho -> m (Rho, Core.Expr -> Core.Expr)
extend tp
  = case expandSyn tp of
      TFun args eff res
        -> let (ls,tl) = extractOrderedEffect eff
           in if isEffectEmpty tl
               then do tv <- freshTVar kindEffect Meta
                       let openEff = effectExtends ls tv
                           openTp  = TFun args openEff res
                       -- return (openTp, id)
                       return (openTp, \core -> Core.openEffectExpr eff openEff tp openTp core)
               else return (tp,id)
      _ -> return (tp,id)


-- | Skolemize a type
skolemize :: (HasCallStack,HasUnique m) => Range -> Type -> m Rho
skolemize range tp
  = do (ids,rho,coref) <- skolemizeEx range tp
       return rho

-- | Skolemize a type and return the instantiated quantifiers, name/predicate pairs for evidence,
-- the instantiated type, and a core transformer function (which applies type arguments and evidence)
skolemizeEx :: (HasCallStack,HasUnique m) => Range -> Type -> m ([TypeVar],Rho,Core.Expr -> Core.Expr)
skolemizeEx rng tp
  = -- trace ("skolemizeEx: " ++ show tp) $
    instantiateExFl Skolem rng tp


-- | General instantiation for skolemize and instantiate
instantiateExFl :: (HasCallStack,HasUnique m) => Flavour -> Range -> Type -> m ([TypeVar],Rho,Core.Expr -> Core.Expr)
instantiateExFl flavour range tp
  = case splitTypeScheme tp of
      ([],rho) -> return ([],rho,id)
      (vars,rho)
        ->  do (tvars,sub) <- freshSubX TVar flavour vars (alltv rho)
               let srho   = sub |-> rho
               return $! (tvars, srho, addTypeApps tvars)


freshSub :: HasUnique m => Flavour -> [TypeVar] -> m ([TypeVar],Sub)
freshSub flavour vars
  = do tvars <- mapM (\tv -> freshTypeVar (typevarKind tv) flavour) vars
       let sub = subNew (zip vars (map TVar tvars))
       return (tvars,sub)


freshSubX :: HasUnique m => (TypeVar -> Type) -> Flavour -> [TypeVar] -> Tvs -> m ([TypeVar],Sub)
freshSubX makeType flavour vars exclude
  = -- trace ("freshSubX: " ++ show vars) $
    do tvars <- mapM (\tv -> freshTypeVarX (typevarKind tv) flavour exclude) vars
       let sub = subNew (zip vars (map makeType tvars))
       return (tvars,sub)

-- ensure we don't use a fresh type variable that already occurs in the type
freshTypeVarX :: HasUnique m => Kind -> Flavour -> Tvs -> m TypeVar
freshTypeVarX kind flavour exclude
  = do tvar <- freshTypeVar kind flavour
       if (tvsMember tvar exclude)
         then freshTypeVarX kind flavour exclude
         else return tvar

{-
-- | Instantiate the the "some" quantifiers of an annotation to fresh type variables
instantiateAnnot :: HasUnique m => Annot -> m Type
instantiateAnnot (Annot [] tp)
  = return tp
instantiateAnnot (Annot ids tp)
  = do tvs <- freshTypeVars Free (length ids)
       let stp  = subNew ids tvs |-> tp
       return stp
-}

--------------------------------------------------------------------------
-- Fresh type variables
--------------------------------------------------------------------------
-- | return fresh skolem variables
freshSkolems :: HasUnique m => Kind -> Int -> m [Type]
freshSkolems kind n
  = freshTVars kind Skolem  n

-- | return fresh type variables of a certain |Flavour|
freshTVars :: HasUnique m => Kind -> Flavour -> Int -> m [Type]
freshTVars kind fl n
  = mapM (\_ -> do freshTVar kind fl) [1..n]

freshTVar :: HasUnique m => Kind -> Flavour -> m Type
freshTVar kind flavour
  = do tv <- freshTypeVar kind flavour
       return (TVar tv)


freshEffect :: HasUnique m => m Effect
freshEffect
  = freshTVar kindEffect Meta

freshStar :: HasUnique m => m Tau
freshStar
  = freshTVar kindStar Meta

{--------------------------------------------------------------------------
   Synonym map
--------------------------------------------------------------------------}  

-- todo: check kind instead of only realiasing function effects?
-- todo: expand any synonyms, not just for common effects?
realiasType :: Synonyms -> Type -> Type
realiasType synonyms tp
  = realias tp
  where
    realias tp
      = case tp of
          TFun args eff res -> TFun [(argname,realias arg) | (argname,arg) <- args] (realiasEffect synonyms eff) (realias res)
          TForall vars t    -> TForall vars (realias t)
          TApp t args       -> TApp (realias t) (map realias args)
          TSyn syn args t   -> TSyn syn (map realias args) (realias t)
          _                 -> tp


realiasEffect :: Synonyms -> Effect -> Effect
realiasEffect synonyms eff
  = let (ls,tl) = extractOrderedEffect eff
        ls' = useCommonAliases synonyms ls           
    in (foldr (\l t -> TApp (TCon tconEffectExtend) [l,t]) tl ls') -- cannot use effectExtends since we want to keep synonyms


commonAliases
  = [nameTpIO, nameTpIOC, nameTpIOCTotal, nameTpST, nameTpPure, nameTpAsync]

useCommonAliases synonyms ls
  = useAliases synonyms commonAliases ls

useAliases :: Synonyms -> [Name] -> [Tau] -> [Tau]
useAliases synonyms names ls
  = case names of
      [] -> ls
      (name:ns)
        -> let (aliass,post) = tryAlias ls name
               ls' = useAliases synonyms ns post
           in (aliass ++ ls')
  where
    tryAlias :: [Tau] -> Name -> ([Tau],[Tau])
    tryAlias [] name
      = ([],[])
    tryAlias ls name
      = let mbsyn = synonymsLookup name synonyms
        in case mbsyn of
             Nothing -> -- trace ("* cannot find alias: " ++ show name) $ 
                        ([],ls)
             Just syn
              -> let (ls2,tl2) = extractOrderedEffect (synInfoType syn)
                 in if (null ls2 || not (isEffectEmpty tl2))
                     then -- trace ("* strange alias: " ++ show name ++ ": " ++ show (pretty tl2)) $
                          ([],ls)
                     else let params      = synInfoParams syn
                              (sls,insts) = findInsts params ls2 ls
                          in -- trace ("* try alias: " ++ show (synInfoName syn) ++ "\n  " ++ show (map pretty sls) ++ "\n  " ++ show (map pretty ls)) $
                             if (length sls > length ls) 
                              then ([],ls)
                              else case (isSubsetEq [] sls ls) of
                                    Just rest
                                      -> -- trace (" synonym replace: " ++ show (synInfoName syn, pretty rest)) $
                                         ([TSyn (TypeSyn name (synInfoKind syn) (synInfoRank syn) (Just syn)) insts (effectFixed sls)], rest)
                                    _ -> ([], ls)

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



isSubsetEq :: [Tau] -> [Tau] -> [Tau] -> Maybe [Tau]
isSubsetEq acc ls1 ls2
  = case (ls1,ls2) of
      ([],[])       -> Just (reverse acc)
      ([],(l2:ll2)) -> Just (reverse acc ++ ls2)
      (l1:ll1, [])  -> Nothing
      (l1:ll1,l2:ll2)
        -> if (labelName l1 < labelName l2)
            then Nothing
           else if (labelName l1 > labelName l2)
            then isSubsetEq (l2:acc) ls1 ll2
           else if (eqType l1 l2)
            then isSubsetEq acc ll1 ll2
            else Nothing

-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Definition of higher-ranked types and utility functions over them.
-----------------------------------------------------------------------------
module Type.Operations( instantiate
                      , instantiateEx, instantiateNoEx, extend
                      , skolemize, skolemizeEx
                      , freshTVar
                      , Evidence(..)
                      , freshSub
                      ) where


import Common.Range
import Common.Unique

import Kind.Kind
import Type.Type
import Type.TypeVar
import Core.Core as Core

--------------------------------------------------------------------------
-- Instantiation 
--------------------------------------------------------------------------
data Evidence = Ev{ evName :: Core.TName
                  , evPred :: Pred
                  , evRange :: Range
                  }

instance HasTypeVar Evidence where
  sub `substitute` ev
    = ev{ evPred = sub `substitute` (evPred ev) }
  ftv ev
    = ftv (evPred ev)
  btv ev
    = btv (evPred ev)

instance Show Evidence where
  show ev = show (evPred ev)

-- | Instantiate a type
instantiate :: HasUnique m => Range -> Type -> m Rho
instantiate range tp
  = do (ids,preds,rho,coref) <- instantiateEx range tp 
       return rho

-- | Instantiate a type and return the instantiated quantifiers, name/predicate pairs for evidence, 
-- the instantiated type, and a core transformer function (which applies type arguments and evidence)
instantiateEx :: HasUnique m => Range -> Type -> m ([TypeVar],[Evidence],Rho,Core.Expr -> Core.Expr)
instantiateEx rng tp
  = do (ids,preds,rho,coref) <- instantiateExFl Meta rng tp
       (erho,coreg) <- extend rho
       return (ids,preds,erho, coreg . coref)

-- | Instantiate a type and return the instantiated quantifiers, name/predicate pairs for evidence, 
-- the instantiated type, and a core transformer function (which applies type arguments and evidence)
instantiateNoEx :: HasUnique m => Range -> Type -> m ([TypeVar],[Evidence],Rho,Core.Expr -> Core.Expr)
instantiateNoEx rng tp
  = do (ids,preds,rho,coref) <- instantiateExFl Meta rng tp
       return (ids,preds,rho,coref)

-- | Ensure the result of function always gets an extensible effect type
-- This is necessary to do on instantiation since we simplify such effect variables
-- away during generalization. Effectively, the set of accepted programs does not 
-- change but the types look simpler to the user.
extend :: HasUnique m => Rho -> m (Rho, Core.Expr -> Core.Expr)
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
skolemize :: HasUnique m => Range -> Type -> m Rho
skolemize range tp
  = do (ids,preds,rho,coref) <- skolemizeEx range tp
       return rho

-- | Skolemize a type and return the instantiated quantifiers, name/predicate pairs for evidence, 
-- the instantiated type, and a core transformer function (which applies type arguments and evidence)
skolemizeEx :: HasUnique m => Range -> Type -> m ([TypeVar],[Evidence],Rho,Core.Expr -> Core.Expr)
skolemizeEx = instantiateExFl Skolem


-- | General instantiation for skolemize and instantiate
instantiateExFl :: HasUnique m => Flavour -> Range -> Type -> m ([TypeVar],[Evidence],Rho,Core.Expr -> Core.Expr)
instantiateExFl flavour range tp
  = case splitPredType tp of
      ([],[],rho) -> return ([],[],rho,id)
      (vars,preds,rho)
        ->  do (tvars,sub) <- freshSubX TVar flavour vars
               let srho   = sub |-> rho
                   spreds = sub |-> preds
               pnames <- mapM predName spreds
               let corevars = map (\name -> Core.Var name InfoNone) pnames
                   evidence = [Ev name pred range | (name,pred) <- zip pnames spreds]
               return (tvars, evidence, srho
                      ,(if null corevars then id else id {- Core.addApps corevars -}) . Core.addTypeApps tvars)

              
predName :: HasUnique m => Pred -> m Core.TName
predName pred
  = do name <- case pred of
                 PredSub _ _ -> Core.freshName "sub"
                 PredIFace iname _ -> Core.freshName (show iname)               
       return (Core.TName name (predType pred))


freshSub :: HasUnique m => Flavour -> [TypeVar] -> m ([TypeVar],Sub)
freshSub flavour vars
  = do tvars <- mapM (\tv -> freshTypeVar (typevarKind tv) flavour) vars
       let sub = subNew (zip vars (map TVar tvars))
       return (tvars,sub)


freshSubX :: HasUnique m => (TypeVar -> Type) -> Flavour -> [TypeVar] -> m ([TypeVar],Sub)
freshSubX makeTVar flavour vars
  = do tvars <- mapM (\tv -> freshTypeVar (typevarKind tv) flavour) vars
       let sub = subNew (zip vars (map makeTVar tvars))
       return (tvars,sub)

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


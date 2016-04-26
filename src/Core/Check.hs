-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Typechecker for Core-F 
-}
-----------------------------------------------------------------------------

module Core.Check (check) where

import Control.Monad
import Lib.Trace
import Lib.PPrint
import Common.Name
import Common.Unique
import Core.Core hiding (typeOf)
import qualified Core.Core as Core
import Core.Pretty
import Type.Type
import Type.TypeVar
-- import Type.Operations
import Type.Assumption
import Type.Pretty
import Type.Kind
import Kind.Kind
-- import Kind.KindVar

import qualified Lib.Set as S
check :: Int -> DefGroups -> Gamma -> Exception () 
check uniq defGroups gamma 
  = do checkDefGroups uniq defGroups gamma
       return ()

{--------------------------------------------------------------------------
  Definition groups 

  To check a recursive definition group, we assume the type for each 
  definition is correct, add all of those to the environment, then check 
  each definition
--------------------------------------------------------------------------}

checkDefGroups :: Int -> DefGroups -> (Gamma -> Exception Gamma)
checkDefGroups uniq (DefGroups defGroups)
  = runM $ map (checkDefGroup uniq) defGroups

checkDefGroup :: Int -> DefGroup -> (Gamma -> Exception Gamma) 
checkDefGroup uniq defGroup env 
  = do let defs = case defGroup of
                    DefRec (Defs defs) -> defs
                    DefNonRec def      -> [def]
           env' = gammaUnion env (gammaNew $ map annotation defs) 
       mapM (checkDef env' uniq) defs 
       return env'
  where
    annotation :: Def -> (Name, Type)
    annotation (Def name tp expr) = (name, tp)

checkDef :: Gamma -> Int -> Def -> Exception () 
checkDef env uniq d@(Def name tp expr) 
  = do tp' <- typeOfT env uniq expr 
       match "checking annotation on definition" (prettyDef defaultEnv d) uniq tp tp'

{--------------------------------------------------------------------------
  Expressions   
--------------------------------------------------------------------------}

typeOfT :: Gamma -> Int -> Expr -> Exception Type
typeOfT env uniq expr 
  = do tp <- typeOf env uniq expr
       -- trace ("(" ++ show expr ++ ") :: " ++ show tp) return ()
       return tp

typeOf :: Gamma -> Int -> Expr -> Exception Type 

-- Lambda abstraction
typeOf env uniq (Lam tname eff expr)
  = do body <- typeOfT (gammaExtend (getName tname) (Core.typeOf tname) env) uniq expr
       return (typeFun (Core.typeOf tname) eff body)

-- Variables
typeOf env uniq (Var tname)
  = find env (getName tname)

-- Constants 
typeOf env uniq (Con tname)
  = find env (getName tname)

-- Application
typeOf env uniq e@(App fun arg)
  = do tpFun            <- typeOfT env uniq fun
       (formal, result) <- splitFun tpFun
       actual           <- typeOfT env uniq arg
       match "comparing formal and actual argument" (prettyExpr defaultEnv e) uniq formal actual
       return result

-- Kind lambdas
typeOf env uniq (KindLam x tp)
  = do tp' <- typeOfT env uniq tp
       return (KForall x tp')

-- Kind application
typeOf env uniq (KindApp expr kind)
  = do tpKForall <- typeOfT env uniq expr
       (k, tp')  <- splitKForall tpKForall
       return (ksubSingle k kind |=> tp')

-- Type lambdas
typeOf env uniq (TypeLam x tp)
  = do tp' <- typeOfT env uniq tp
       return (TForall x tp')

-- Type application
typeOf env uniq e@(TypeApp expr tp)
  = do tpTForall <- typeOfT env uniq expr
       (t, tp')  <- splitTForall tpTForall 
       -- Kind check
       -- We can use actual equality for kinds, because any kind variables will have been
       -- substituted when doing kind application (above)
       when (getKind t /= getKind tp) $
         throw $ "kind error: " ++ show e
       return (subSingle t tp |-> tp') 

-- Literals
typeOf env uniq (Lit (LitInt _)) 
  = return typeInt
typeOf env uniq (Lit (LitFloat _)) 
  = return typeFloat
typeOf env uniq (Lit (LitChar _)) 
  = return typeChar
typeOf env uniq (Lit (LitString _)) 
  = return typeString

-- Let
typeOf env uniq (Let defGroups expr) 
  = do env' <- checkDefGroups uniq defGroups env
       typeOfT env' uniq expr 

-- Labels
typeOf env uniq (Label name)
  = return (TApp typeLabel (TCon (TypeCon name kindLabel)))

-- Case
typeOf env uniq e@(Case exprs branches)
  = do tpScrutinees <- mapM (typeOf env uniq) exprs
       tpBranches   <- mapM (typeOfBranch env uniq tpScrutinees) branches
       mapConseqM (match "verifying that all branches have the same type" (prettyExpr defaultEnv e) uniq) tpBranches
       return (head tpBranches)

{--------------------------------------------------------------------------
  Type of a branch 
--------------------------------------------------------------------------}

typeOfBranch :: Gamma -> Int -> [Type] -> Branch -> Exception Type
typeOfBranch env uniq tpScrutinees b@(Branch patterns guard expr) 
  = do mapM_ (typeOfPattern env uniq) (zip tpScrutinees patterns)
       let g = gammaUnion env (gammaNew [(getName tname, Core.typeOf tname) | tname <- S.toList (bv patterns)])
       tp <- typeOf g uniq guard
       match "verify that the guard is a boolean expression" (prettyExpr defaultEnv guard) uniq tp typeBool
       typeOf g uniq expr

typeOfPattern :: Gamma -> Int -> (Type,Pattern) -> Exception ()
typeOfPattern env uniq (tpScrutinee,pat)
  = case pat of
      PatCon tname args  -> do constrArgs <- findConstrArgs env uniq (prettyPattern defaultEnv pat) tpScrutinee (getName tname)
                               mapM_  (typeOfPattern env uniq) (zip constrArgs args)
      PatVar tname       -> match "comparing constructor argument to case annotation" (prettyPattern defaultEnv pat) uniq tpScrutinee (Core.typeOf tname)
      PatWild            -> return ()
                              

{--------------------------------------------------------------------------
  Util 
--------------------------------------------------------------------------}

-- Find the types of the arguments of a constructor,
-- given the type of the result of the constructor
-- Currently constructors can only have a single argument
findConstrArgs :: Gamma -> Int -> Doc -> Type -> Name -> Exception [Type]
findConstrArgs env uniq doc tpScrutinee con 
  = do tpCon <- find env con
       -- Until we add qualifiers to constructor types, the list of predicates
       -- returned by instantiate' must always be empty
       let (([], tpConInst, _), uniq') = runUnique uniq (instantiate' tpCon)
           (tpArgs, tpRes) = splitArgs tpConInst
       subst <- case runUnique uniq' (unify tpRes tpScrutinee) of
                 (Left error, _) -> showError "comparing scrutinee with branch type" "cannot unify" tpRes tpScrutinee doc
                 (Right subst, _) -> return subst 
       return $ (subst `substApply` tpArgs)

-- Find a variable in the environment
find :: Gamma -> Name -> Exception Type
find env name = case gammaInRange name env of
                  True  -> return (gammaFind name env) 
                  False -> throw $ "Unbound variable " ++ show name  

-- In Core, when we are comparing types, we are interested in exact 
-- matches only.
match :: String -> Doc -> Int -> Type -> Type -> Exception ()
match when e uniq a b = case runUnique uniq (unify a b) of
                   (Left error, _)  -> showError "cannot unify" when a b e 
                   (Right subst, _) -> if substIsNull subst 
                                       then return () 
                                       else showError "non-empty substitution" when a b e 

-- Print unification error
showError :: String -> String -> Type -> Type -> Doc -> Exception a 
showError err when a b e = throw . show $ align $ vcat [ text err 
                                                   , text "     " <> prettyType defaultEnv a
                                                   , text "  =~ " <> prettyType defaultEnv b
                                                   , text "when" <+> text when 
                                                   , indent 2 e
                                                   ]

{--------------------------------------------------------------------------
  Decompose types 
--------------------------------------------------------------------------}
splitArgs :: Type -> ([Type],Type)
splitArgs tp
  = case tp of
      (TApp (TApp con arg) rest) | con == typeArrow 
          -> let (args,res) = splitArgs rest
             in (arg:args, res)
      _   -> ([],tp)

splitFun :: Type -> Exception (Type, Type)
splitFun (TApp (TApp con arg) res) | con == typeArrow = return (arg, res)
splitFun _ = failure "Core.Check.splitFun: Expected function" 
  
splitKForall :: Type -> Exception (KindVar, Type)
splitKForall (KForall k tp) = return (k, tp)
splitKForall _ = failure "Core.Check.splitKForall: Expected kforall"

splitTForall :: Type -> Exception (TypeVar, Type)
splitTForall (TForall t tp) = return (t, tp)
splitTForall _ = throw $ "Expected forall"

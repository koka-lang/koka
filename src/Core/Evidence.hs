-----------------------------------------------------------------------------
-- Copyright 2019 Microsoft Corporation, Daan Leijen, Daniel Hillerström
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Evidence translation for effects
-----------------------------------------------------------------------------

module Core.Evidence( evidenceTransform
                    ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative

import Lib.PPrint

import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim( nameTpEv, nameConEv, nameEffectOpen, nameReturn, nameTpHandled, nameTpHandled1 )
import Common.Error
import Common.Syntax

import Kind.Kind( kindStar, isKindEffect, kindFun, kindEffect, kindHandled )

import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Type.Operations( freshTVar )

import Core.Core
import qualified Core.Core as Core
import Core.Pretty
import Core.CoreVar

import Control.Exception (assert)

trace s x =
  -- Lib.Trace.trace s
    x

evidenceTransform :: Pretty.Env -> DefGroups -> Error DefGroups
evidenceTransform penv defGroups
  = return defGroups

{--------------------------------------------------------------------------
  Transform definitions.
--------------------------------------------------------------------------}
evDefGroups :: Bool -> DefGroups -> Ev DefGroups
evDefGroups toplevel defGroups
  = mapM (evDefGroup toplevel) defGroups

evDefGroup :: Bool -> DefGroup -> Ev DefGroup
evDefGroup toplevel (DefRec defs)
  = do defs <- mapM (evDef toplevel) defs
       return (DefRec defs)
evDefGroup toplevel (DefNonRec def)
  = do def' <- evDef toplevel def
       return (DefNonRec def')

evDef :: Bool -> Def -> Ev Def
evDef toplevel def
  = do expr' <- evExpr (defExpr def)
       let ty' = evType (defType def)
       return $ def { defExpr = expr', defType = ty' }

{--------------------------------------------------------------------------
  Transform expressions.
--------------------------------------------------------------------------}
evExpr :: Expr -> Ev Expr
-- Effect row open.
evExpr (App (TypeApp (Var open _) [effFrom, effTo, _, _]) [f])
  | getName open == nameEffectOpen = undefined -- FIXME TODO.

-- Interesting cases.
evExpr (Lam params eff body)
  = do (body', p) <- runIsolated (evExpr body)  -- find needed evidence
       p' <- eff `entails` p                    -- bind the evidence in the right order
       let params' = map evParam params
       return (evAbstract p' (Lam params' eff body'))
         where evParam :: TName -> TName
               evParam (TName name typ) = TName name (evType typ)

evExpr (App f args)
  = do f' <- evExpr f
       return (App f' undefined)

--       App f args
--         -> do (p1, f') <- evExpr evCtx f
--               args' <- mapM (evExpr evCtx) args
--               let (p3, f'') = evClosureR pnil (fst . decomposeEvType $ typeOf f') f'
--                   p2        = foldr (<>) pnil (map fst args')
--                   (p6, args'') = transform (typeOf f') (map snd args')
--               return $ (p1 <> p2 <> p3, abstract p6 (App f'' args''))
--               where
--                 transform :: Type -> [Expr] -> (P, [Expr])
--                 transform sft args
--                   = let (q3, ft) = decomposeEvType sft
--                         Just(dom, _, cod) = splitFunType ft
--                         (p6, args')    = pointwise dom args
--                     in if not (isEmptyP p6) && not (isFun cod)
--                        then error "s' is not a function type."
--                        else (p6, args')
--                 pointwise :: [(Name, Type)] -> [Expr] -> (P, [Expr])
--                 pointwise [] [] = (pnil, [])
--                 pointwise ((_, s1) : ps) (arg : args)
--                   = if isFun s1
--                     then let (q4, s1'') = decomposeEvType s1
--                              (q5, s2'') = decomposeEvType (typeOf arg)
--                          in if s1'' /= s2'' then error "s1'' != s2''"
--                             else let p4 = toP q4
--                                      (p6, arg') = evClosureR p4 q5 arg
--                                      (p6', args') = pointwise ps args
--                                  in (p6 <> p6', (abstract p4 arg') : args')
--                     else let (p, args') = pointwise ps args
--                          in (p, arg : args')
--                 pointwise _ _ = error "Arity mismatch."


-- Regular cases.
evExpr (Let defgs body)
  = do defgs' <- evDefGroups False defgs
       body' <- evExpr body
       return $ makeLet defgs' body'


evExpr (Case exprs bs)
  = do exprs' <- mapM evExpr exprs
       bs' <- mapM branch bs
       return $ Case exprs' bs'
         where branch :: Branch -> Ev Branch
               branch (b@(Branch _ guards))
                 = do guards' <- mapM guard guards
                      return $ b { branchGuards = guards' }
               guard :: Guard -> Ev Guard
               guard (Guard test expr)
                 = do test' <- evExpr test
                      expr' <- evExpr expr
                      return $ Guard { guardTest = test'
                                     , guardExpr = expr' }

evExpr (Var (TName name typ) info)
  = let typ' = evType typ
        tname = TName name typ'
        info' = case info of
                  InfoArity n m mkind ->
                    let k = length (fst (decomposeEvType typ'))
                    in InfoArity n (m + k) mkind
                  _ -> info
    in return (Var tname info')

evExpr (TypeLam tvars body)
  = do body' <- evExpr body
       return (TypeLam tvars body')

evExpr (TypeApp body tps)
  = do body' <- evExpr body
       let tps' = map evType tps
       return (TypeApp body' tps')

evExpr expr = return expr -- leave unchanged.

{--------------------------------------------------------------------------
  Transform types.
--------------------------------------------------------------------------}
evType :: Type -> Type
evType (TFun params eff cod)
  =  let params' = map (mapSnd evType) params
         evs     = evidenceParameterTypesOf eff
         cod'    = evType cod
     in typeFun (evs ++ params') eff cod'

evType (TForall tyvars preds rho)
  = let preds' = map evPred preds
        rho'   = evType rho
    in TForall tyvars preds' rho'

evType (TApp t ts)
  = let t'  = evType t
        ts' = map evType ts
    in TApp t' ts'

evType (TSyn tysyn ts t)
  = let ts' = map evType ts
        t'  = evType t
    in TSyn tysyn ts' t'

evType typ = typ  -- base cases.

evPred :: Pred -> Pred
evPred (PredSub t0 t1)
  = let t0' = evType t0
        t1' = evType t1
    in PredSub t0' t1'
evPred (PredIFace name ts)
  = let ts' = map evType ts
    in PredIFace name ts'

{--------------------------------------------------------------------------
  Auxiliary functions on types.
--------------------------------------------------------------------------}
decomposeEvType :: Type -> (Q, Type)
decomposeEvType (TFun params eff cod)
  = (q, typeFun (dropWhile isEvParam params) eff cod)
  where isEvParam :: (Name, Type) -> Bool
        isEvParam (_, typ) = isEvType typ
        q = map (\(name, typ) -> (staticLabelNameOf typ, TName name typ)) (takeWhile isEvParam params)
decomposeEvType t = ([], t)

effectsOf :: Expr -> Effect
effectsOf e = case typeOf e of
               TFun _ eff _ -> eff
               _            -> effectEmpty

labelsOf :: Effect -> [Type]
labelsOf eff = fst (extractOrderedEffect eff)

extractLabelType :: Type -> Type
extractLabelType t
  = case expandSyn t of
      TApp (TCon (TypeCon name _)) [htp] | (name == nameTpHandled || name == nameTpHandled1)
        -> extractLabelType htp -- reach under the handled<htp> name to extract htp.
      t' -> t'

toEvidenceType :: Type -> Type
toEvidenceType label = makeEvType . extractLabelType $ label

evidenceRequiredBy :: Expr -> [Type]
evidenceRequiredBy e = map toEvidenceType (labelsOf (effectsOf e))

evidenceParameterTypesOf :: Effect -> [(Name, Type)]
evidenceParameterTypesOf eff
  = let labels = labelsOf eff
    in map (\label -> (nameNil, toEvidenceType label)) labels


-- evidenceTypesOf :: P -> [Type]
-- evidenceTypesOf p = foldr (\((_, TName _ typ)) types -> typ : types) [] p

{-----------------------------------------------------------------------------
  Evidence context.
-----------------------------------------------------------------------------}
type Evidence = TName
type P = [(Name, Evidence)]
type Q = P

pempty :: P
pempty = []

(<<=) :: P -> Effect -> Bool
p <<= eff = let present = map labelName (fst . extractOrderedEffect $ eff)
            in all (\(label, _) -> label `elem` present) p

evApply :: Expr -> P -> Expr
evApply e p = addApps (map toWitness p) e

evAbstract :: P -> Expr -> Expr
evAbstract p e = addLambdas (toFunParams p) (effectsOf e) e

toTFunParams :: Q -> [(Name, Type)]
toTFunParams q = map (\(_, TName name typ) -> (name, typ)) q

toFunParams :: P -> [(Name, Type)]
toFunParams = toTFunParams

toWitness :: (Name, TName) -> Expr
toWitness (_, tname) = Var { varName = tname, varInfo = InfoNone }

{-----------------------------------------------------------------------------
  Evidence monad.
-----------------------------------------------------------------------------}

data Result a = Result a State
data State    = State { uniq :: Int
                      , penv :: P }

newtype Ev a = Ev (State -> Result a)

instance HasUnique Ev where
  setUnique    i = Ev (\st -> Result () (st { uniq = i }) )
  updateUnique f = Ev (\st -> Result (uniq st) (st { uniq = (f (uniq st)) }))

instance Functor Ev where
  fmap f (Ev env) = Ev (\st -> case env st of
                                Result x st' -> Result (f x) st')

instance Applicative Ev where
  pure = return
  (<*>) = ap

instance Monad Ev where
  return x       = Ev (\st -> Result x st)
  (Ev env) >>= k = Ev (\st -> case env st of
                               Result x st' -> case k x of
                                                Ev env' -> env' st')

-- Monadic operations.
getEvContext :: Ev P
getEvContext = Ev (\st -> Result (penv st) st)

setEvContext :: P -> Ev ()
setEvContext p = Ev (\st -> let st' = st { penv = p }
                            in Result () st')

assertEmptyEvContext :: Ev ()
assertEmptyEvContext
  = do p <- getEvContext
       case p of
         [] -> return ()
         _  -> error "Assertion failure: the evidence context is non-empty."

runIsolated :: Ev a -> Ev (a, P)
runIsolated comp
 = do p <- getEvContext   -- backup context.
      setEvContext pempty -- reset context.
      result <- comp      -- run the computation under the empty context.
      p' <- getEvContext  -- get the final context.
      setEvContext p      -- restore the original context.
      return (result, p')

runIsolated' :: Ev a -> Ev a
runIsolated' comp
  = do result <- runIsolated comp
       return (fst result)

addEvidence :: Name -> Evidence -> Ev ()
addEvidence name ev
  = do p <- getEvContext
       setEvContext ((name, ev) : p)

bindEvidence :: Type -> Ev (Name, TName)
bindEvidence typ
  = let evtyp = makeEvType typ
        labelName = staticLabelNameOf typ
    in do runtimeLabelName <- freshRuntimeLabelName (toRuntimeLabelName labelName)
          let ev = TName runtimeLabelName evtyp
          addEvidence labelName ev
          return (labelName, ev)

-- Entailment.
entails :: Effect -> P -> Ev P
entails eff p
  = let delta = diff (labelsOf eff) p
    in do evs <- mapM bindEvidence delta
          return (evs ++ p)
            where diff :: [Type] -> P -> [Type]
                  diff ls p
                    = foldr
                      (\l delta -> case lookup (labelName l) p of
                                     Nothing -> l : delta
                                     _       -> delta)
                      [] ls


{-
assertMatchesEvidence :: Expr -> P -> String -> Ev ()
evidenceTypesOf :: P -> [Type]
evidenceNeeded :: Expr -> [Type]
effectsOf :: Expr -> Effect
-}

{--------------------------------------------------------------------------
  Evidence naming.
--------------------------------------------------------------------------}
toRuntimeLabelName :: Name -> Name
toRuntimeLabelName name = makeHiddenName "ev-" name -- FIXME TODO: use the names generated by the parser.

staticLabelNameOf :: Type -> Name
staticLabelNameOf typ = removePrefix ".ev-" (labelName typ)

removePrefix :: String -> Name -> Name
removePrefix prefix name
  = newQualified (nameModule name) (go prefix (nameId name))
  where go [] name = name
        go (c : prefix) (c' : name')
          | c == c' = go prefix name'
          | otherwise = nameId name -- short-cuit and return the original name.
        go _ _ = nameId name

freshRuntimeLabelName :: Name -> Ev Name
freshRuntimeLabelName name
  = uniqueName (nameId (toRuntimeLabelName name))

{--------------------------------------------------------------------------
  Miscellaneous.
--------------------------------------------------------------------------}
mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f p = mapPair id f p

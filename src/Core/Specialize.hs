-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Steven Fontanella, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Core.Specialize( specialize
                      , extractSpecializeDefs
                      ) where

import Data.List (transpose)
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((***))
import Data.Monoid((<>))
import Data.Maybe (mapMaybe, fromMaybe, catMaybes, isJust, fromJust)

import Lib.PPrint
import Common.Failure (failure)
import Common.File (splitOn)
import Common.Range (rangeNull)
import Common.Syntax
import Common.Unique (HasUnique)
import Common.Name
import Common.NameMap (NameMap)
import qualified Common.NameMap  as M
import Common.NameSet (NameSet)
import qualified Common.NameSet as S
import Core.Core
import Core.Pretty ()
import Type.Type (splitFunScheme, Effect, Type, TypeVar)
import Type.TypeVar
import Type.Pretty
import Lib.Trace
import Core.Inlines
import Core.Uniquefy

{--------------------------------------------------------------------------
  Specialization Monad
--------------------------------------------------------------------------}

type SpecM = Reader Inlines

runSpecM :: Inlines -> SpecM a -> a
runSpecM specEnv specM = runReader specM specEnv


{--------------------------------------------------------------------------
  Specialization
--------------------------------------------------------------------------}

specialize :: Inlines -> CorePhase ()
specialize specEnv 
  = liftCorePhase $ \defs ->
    uniquefyDefGroups $ runSpecM specEnv (mapM specOneDefGroup defs)

speclookupM :: Name -> SpecM (Maybe InlineDef)
speclookupM name 
  = asks $ \env -> 
    case inlinesLookup name env of
      Just idef | inlineDefIsSpecialize idef -> Just idef      
      _ -> Nothing 

specOneDefGroup :: DefGroup -> SpecM DefGroup
specOneDefGroup = mapMDefGroup specOneDef

specOneDef :: Def -> SpecM Def
specOneDef def 
  = do e <- specOneExpr (defName def) $ defExpr def
       pure def{ defExpr = e }

-- forceExpr :: Expr -> Expr
-- forceExpr e = foldExpr (const (+1)) 0 e `seq` e

specOneExpr :: Name -> Expr -> SpecM Expr
specOneExpr thisDefName 
  = rewriteBottomUpM $ \e -> 
    case e of
      App (Var (TName name _) _) args 
        -> go name e
      App (TypeApp (Var (TName name _) _) typVars) args 
        -> go name e
      e -> pure e
  where
    go name e 
       = do mbSpecDef <- speclookupM name
            case mbSpecDef of
              Nothing -> pure e
              Just specDef 
                | inlineName specDef /= thisDefName -> specOneCall specDef e   -- don't specialize ourselves
                | otherwise -> pure e

filterBools :: [Bool] -> [a] -> [a]
filterBools bools as 
  = catMaybes $ zipWith (\bool a -> guard bool >> Just a) bools as

-- (falses, trues)
partitionBools :: [Bool] -> [a] -> ([a], [a])
partitionBools bools as = foldr f ([], []) $ zip bools as
  where
    f (bool, a) (falses, trues)
      | bool = (falses, a : trues)
      | otherwise = (a : falses, trues)

specOneCall :: InlineDef -> Expr -> SpecM Expr
specOneCall (InlineDef{ inlineName=specName, inlineExpr=specExpr, specializeArgs=specArgs }) e 
  = case e of
      App (Var (TName name _) _) args  | goodArgs args
        -> replaceCall specName specExpr specArgs args Nothing
      App (TypeApp (Var (TName name ty) _) typeArgs) args  | goodArgs args
        -> replaceCall specName specExpr specArgs args $ Just typeArgs
      _ -> return e

  where
    goodArgs args  = all goodArg (map snd $ filter fst $ zip specArgs args)
    
    goodArg expr = case expr of
                    Lam{}                  -> True
                    TypeLam _ body         -> goodArg body
                    TypeApp body _         -> goodArg body
                    App fun _              -> goodArg fun  -- ??  for open(f) calls?
                    Var name info          -> case info of
                                                InfoNone -> False
                                                _        -> True
                    _                      -> False
                             

{-
restriction: all recursive calls are with the same parameters
  fun f = /\a1 ... an. \x1 ... xm.  <body...  f a1 ... an e1 ... em  ...>
and rule out:
  fun f = /\a1 ... an. \x1 ... xm.  <body...  f tree<a1> ... an e1 ... em  ...>   <-- polymorphic recursion

now when we specialize all types arguments will be there:

val x  = f<t1,...tn>(e1,...em) 

1) inline body of f
2) substiute all type arguments [a1->t1, ..., an -> tn], now we have function f'.
   and replace all calls in the body of thet form f<t1,...tn> to f'
   now we have a monomorpic function!


-}  

specInnerCalls :: TName -> TName -> [Bool] -> Expr -> Expr
specInnerCalls from to bools = rewriteBottomUp $ \e -> 
  case e of
    App (Var f info) xs
      | f == from -> App (Var to info) $ filterBools bools xs
    App (TypeApp (Var f info) _) xs
    -- the resulting specialized definition is always monomophic; when generating the Inlines we only allow functions whose TypeApps match in ALL recursive calls
    -- and we apply the TypeLam to the TypeApps from the call site
      | f == from -> App (Var to info) $ filterBools bools xs
    e -> e

replaceCall :: Name -> Expr -> [Bool] -> [Expr] -> Maybe [Type] -> SpecM Expr
replaceCall name expr bools args mybeTypeArgs -- trace ("specializing" <> show name) $
  = pure $ Let [DefRec [specDef]] (App (Var (defTName specDef) InfoNone) newArgs)
  where
    specDef = Def (getName specTName) (typeOf specTName) specBody Private DefFun InlineAuto rangeNull 
               $ "// specialized " <> show name <> " to parameters " <> show speccedParams
    -- there's some knot-tying here: we need to know the type of the specialized fn to replace recursive calls with the specialized version, and the new body also influences the specialized type
    specBody 
      = specInnerCalls (TName name (typeOf expr)) specTName (not <$> bools) specBody0
      
    specTName = TName (genSpecName name bools) specType
    specType = typeOf specBody0
    
    specBody0 =
      (\body -> case mybeTypeArgs of
        Nothing -> body
        Just typeArgs -> subNew (zip (fnTypeParams expr) typeArgs) |-> body)
      $ Lam newParams (fnEffect expr) 
      $ Let [DefNonRec $ Def param typ arg Private DefVal InlineAuto rangeNull "" 
            | (TName param typ, arg) <- zip speccedParams speccedArgs]
      $ fnBody expr

    ((newParams, newArgs), (speccedParams, speccedArgs)) =
      (unzip *** unzip)
      -- (\x@(new, spec) -> trace ("Specializing to newArgs " <> show new) $ x) $
      $ partitionBools bools
      $ zip (fnParams expr) args

genSpecName :: Name -> [Bool] -> Name
genSpecName name bools = newName $ "spec_" ++ show (unqualify name)

fnTypeParams :: Expr -> [TypeVar]
fnTypeParams (TypeLam typeParams _) = typeParams

fnParams :: Expr -> [TName]
fnParams (Lam params _ _) = params
fnParams (TypeLam _ (Lam params _ _)) = params

fnEffect :: Expr -> Effect
fnEffect (Lam _ effect _) = effect
fnEffect (TypeLam _ (Lam _ effect _)) = effect

fnBody :: Expr -> Expr
fnBody (Lam _ _ body) = body
fnBody (TypeLam _ (Lam _ _ body)) = body


{--------------------------------------------------------------------------
  Extract definitions that should be specialized
--------------------------------------------------------------------------}

extractSpecializeDefs :: DefGroups -> [InlineDef]
extractSpecializeDefs dgs 
  = mapMaybe makeSpecialize
  $ flattenDefGroups
  $ filter isRecursiveDefGroup dgs
  where
    isRecursiveDefGroup (DefRec [def]) = True
    isRecursiveDefGroup _ = False

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f Nothing = Nothing
filterMaybe f (Just a) = guard (f a) >> pure a

whenJust :: (Applicative f) => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

makeSpecialize :: Def -> Maybe InlineDef
makeSpecialize def 
  =do let (mbTypes, recArgs) = recursiveCalls def
      whenJust mbTypes $ (guard . allEq)

      let params = fnParams $ defExpr def
      let specializableParams =
              map   (filterMaybe ((`S.member` usedInThisDef def) . getName)
                  . (filterMaybe (isFun . tnameType)))
            $ allPassedInSameOrder params recArgs
      
      guard (any isJust specializableParams)
      pure $ -- SpecializeInfo (defName def) (defExpr def) $ map isJust specializableParams
             InlineDef (defName def) (defExpr def) True (InlineAuto) (costDef def) (map isJust specializableParams)

allPassedInSameOrder :: [TName] -> [[Expr]] -> [Maybe TName]
allPassedInSameOrder params calls 
  = -- head is safe because sublists in transpose can't be empty
    map (fmap head . sequence)
  $ transpose
  $ map (passedInSameOrder params) calls

passedInSameOrder :: [TName] -> [Expr] -> [Maybe TName]
passedInSameOrder params args 
  = zipWith f params args
  where 
    f param arg
     | Var tname _ <- arg
     , tname == param = Just tname
    f _ _ = Nothing

isFun = isJust . splitFunScheme

usedInThisDef :: Def -> S.NameSet
usedInThisDef def 
  = foldMapExpr go $ defExpr def
  where 
    go (Var (TName name _) _) = S.singleton name
    go _ = mempty

allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x:xs) = all (== x) xs

-- list of all params to recursive calls
-- include the type if the call is a TypeApp
-- recursiveCalls :: Def -> [(Maybe [Type], [Expr])]
recursiveCalls :: Def -> (Maybe [[Type]], [[Expr]]) -- [(Maybe [Type], [Expr])]
recursiveCalls Def{ defName=thisDefName, defExpr=expr } 
  = case expr of
      Lam params eff body 
        -> go body
      TypeLam types (Lam params eff body) 
        -> go body
      _ -> failure "recursiveCalls: not a function"
  where
    go body =
      let (types, args) = unzip $ foldMapExpr f body
      in (sequence types, args)

    f (App (Var (TName name _) _) args)
      | name == thisDefName = pure (Nothing, args)
    f (App (TypeApp (Var (TName name _) _) types) args)
      | name == thisDefName = pure (Just types, args)
    f _ = []

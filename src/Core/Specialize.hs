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

import Data.List (transpose )
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
    -- TODO: use uniqe int to generate names and remove call to uniquefyDefGroups?
    uniquefyDefGroups $ runSpecM specEnv (mapM specOneDefGroup defs)

speclookupM :: Name -> SpecM (Maybe InlineDef)
speclookupM name
  = asks $ \env ->
      filterMaybe inlineDefIsSpecialize $ inlinesLookup name env

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
  = map snd . filter fst $ zip bools as

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
    goodArgs args  = -- (\isgoodarg -> trace (show args ++ " is " ++ (if isgoodarg then "" else "not ") ++ "good") isgoodarg) $
      all goodArg $ filterBools specArgs args
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

comment :: String -> String
comment = unlines . map ("// " ++ ) . lines

replaceCall :: Name -> Expr -> [Bool] -> [Expr] -> Maybe [Type] -> SpecM Expr
replaceCall name expr bools args mybeTypeArgs = do

  specBody0 <-
        (\body -> case mybeTypeArgs of
          Nothing -> body
          Just typeArgs -> subNew (zip (fnTypeParams expr) typeArgs) |-> body)
        <$> Lam newParams (fnEffect expr)
        <$> specOneExpr name
        -- TODO do we still need the Let?
        (Let [DefNonRec $ Def param typ arg Private DefVal InlineAuto rangeNull ""
              | (TName param typ, arg) <- zip speccedParams speccedArgs]
        -- $ specInnerCalls (TName name (typeOf expr)) specTName (not <$> bools)
        $ fnBody expr)

  -- explain fixing the inner body here
  let specType = typeOf specBody0
      specTName = TName (genSpecName name bools) specType
  let specBody = case specBody0 of
        Lam args eff (Let specArgs body) -> Lam args eff
          (Let specArgs $ specInnerCalls (TName name (typeOf expr)) specTName (not <$> bools) body)
        _ -> failure "Specialize.replaceCall: Unexpected output from specialize pass"
  let
      specDef = Def (getName specTName) (typeOf specTName) specBody Private DefFun InlineAuto rangeNull
                $ "// specialized " <> show name <> " to parameters " <> show speccedParams <> " with args " <> comment (show speccedArgs)

  pure $ Let [DefRec [specDef]] (App (Var (defTName specDef) InfoNone) newArgs)

  where
    ((newParams, newArgs), (speccedParams, speccedArgs)) =
      (unzip *** unzip)
      -- $ (\x@(new, spec) -> trace ("Specializing to newArgs " <> show new) $ x)
      $ partitionBools bools
      $ zip (fnParams expr) args

-- we might shadow here but it will still be correct(?) and we uniquefy at the end
genSpecName :: Name -> [Bool] -> Name
genSpecName name bools = newName $ "spec_" ++ show (unqualify name)
-- genSpecName name bools = makeHiddenName "spec" name

fnTypeParams :: Expr -> [TypeVar]
fnTypeParams (TypeLam typeParams _) = typeParams
fnTypeParams e = failure $ "fnTypeParams: Not a function: " <> show e

fnParams :: Expr -> [TName]
fnParams (Lam params _ _) = params
fnParams (TypeLam _ (Lam params _ _)) = params
fnParams e = failure $ "fnParams: Not a function: " <> show e

fnEffect :: Expr -> Effect
fnEffect (Lam _ effect _) = effect
fnEffect (TypeLam _ (Lam _ effect _)) = effect
fnEffect e = failure $ "fnEffect: Not a function: " <> show e

fnBody :: Expr -> Expr
fnBody (Lam _ _ body) = body
fnBody (TypeLam _ (Lam _ _ body)) = body
fnBody e = failure $ "fnBody: Not a function: " <> show e


{--------------------------------------------------------------------------
  Extract definitions that should be specialized
--------------------------------------------------------------------------}

extractSpecializeDefs ::  DefGroups -> [InlineDef]
extractSpecializeDefs dgs =
    mapMaybe makeSpecialize
  $ filter (isFun . defType)
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
      pure $ InlineDef (defName def) (defExpr def) True (InlineAuto) (costDef def) (map isJust specializableParams)

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

isFun :: Type -> Bool
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
-- include the types if the call is a TypeApp
recursiveCalls :: Def -> (Maybe [[Type]], [[Expr]])
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
      -- assumption: all applications are TypeApps, or no applications are
      in (sequence types, args)

    f (App (Var (TName name _) _) args)
      | name == thisDefName = [(Nothing, args)]
    f (App (TypeApp (Var (TName name _) _) types) args)
      | name == thisDefName = [(Just types, args)]
    f _ = []

{-
multiStepInlines :: Inlines -> [Def] -> Inlines
multiStepInlines inlines = foldl' f inlines
  where
    -- seq here since we're using foldl'?
    f inlines def
    -- inlineCost ?
      | callsSpecializable inlines def = -- trace ("Add " ++ show (defName def) ++ " as multi-step specializable") $
          inlinesExtend (InlineDef (defName def) (defExpr def) False InlineAuto 0 []) inlines
    f inlines _ = inlines

    -- look for calls to specializable functions where we don't know the RHS of an argument
    -- references that aren't calls aren't eligible for multi-step specialization; we don't have the specializable args anyway
    -- callsSpecializable inlines def = getAny $ flip foldMapExpr (defExpr def) $ \e -> Any $ case e of
    callsSpecializable inlines def = flip anySubExpr (defExpr def) $ \e -> case e of
      App (Var (TName name _) _) args
        | Just inlineDef <- inlinesLookup name inlines
        , inlineDefIsSpecialize inlineDef || isTwoStepSpecialize inlineDef
        , name /= defName def -> trace ("Add " ++ show (defName def) ++ " as multi-step specializable because calls " ++ show name) $
            not $ null $ intersect params $ concatMap vars args
      App (TypeApp (Var (TName name _) _) _) args
        | Just inlineDef <- inlinesLookup name inlines
        , inlineDefIsSpecialize inlineDef || isTwoStepSpecialize inlineDef
        , name /= defName def -> trace ("Add " ++ show (defName def) ++ " as multi-step specializable because calls " ++ show name) $
            not $ null $ intersect params $ concatMap vars args
      e -> False

      where
        params = fnParams $ defExpr def
-}

vars :: Expr -> [TName]
vars = foldMapExpr $ \e -> case e of
  Var tname _ -> [tname]
  _ -> []

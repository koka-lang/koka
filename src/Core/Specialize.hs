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

import Data.List (transpose, foldl', intersect, intersperse)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((***))
import Data.Monoid((<>), Alt(..), Endo(..))
import Data.Maybe (mapMaybe, fromMaybe, catMaybes, isJust, fromJust)
import Data.Function

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
import Common.Unique
import Core.Core
import Core.CoreVar
import Core.Pretty ()
import Core.Simplify
import Type.Type (splitFunScheme, Effect, Type, TypeVar)
import Type.TypeVar
import Type.Pretty
import Lib.Trace
import Core.Inlines
import Core.Uniquefy

import Lib.Trace

{-
  Specialization
  Suppose a function is recursive and passes a function to itself unchanged e.g. map takes a function f which does not
  change during recursive calls.
  If we call map with a known function argument e.g. inc or fn(x) { x + x }, then generate a new
  'specialized' definition at this call site which always has the same value for this parameter
  e.g. map(inc, list(1, 10)) becomes

  val spec_map = fn(xs)
    val f = inc
    match(xs)
      Nil -> Nil
      Cons(x, xx) -> Cons(f(x), xx.spec_map())
  spec_map(list(1, 10))

  the simplifier will later inline the only occurence of f leaving

  val spec_map = fn(xs)
    match(xs)
      Nil -> Nil
      Cons(x, xx) -> Cons(inc(x), xx.spec_map())
  spec_map(list(1, 10))
-}

{--------------------------------------------------------------------------
  Specialization Monad
--------------------------------------------------------------------------}

data ReadState = ReadState
  { inlines :: Inlines
  , penv    :: Env
  }
type SpecM = UniqueT (Reader ReadState)

runSpecM :: Int -> ReadState -> SpecM a -> (a, Int)
runSpecM uniq readState specM =
    flip runReader readState
  $ runUniqueT uniq specM


{--------------------------------------------------------------------------
  Specialization
--------------------------------------------------------------------------}

specialize :: Inlines -> Env -> CorePhase b ()
specialize specEnv penv
  = liftCorePhaseUniq  $ \uniq defs ->
    -- TODO: use uniqe int to generate names and remove call to uniquefyDefGroups?
    let (defs', u') = runSpecM (uniq+100) (ReadState specEnv penv) (mapM specOneDefGroup defs)
    in (uniquefyDefGroups defs', u')

speclookup :: Name -> SpecM (Maybe InlineDef)
speclookup name
  = lift $ asks (\ReadState{inlines=inlines} ->
      filterMaybe inlineDefIsSpecialize (inlinesLookup name inlines))

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
  = rewriteTopDownM $ \e ->
    case e of
      App (Var (TName name _) _) args
        -> go name e
      App (TypeApp (Var (TName name _) _) typVars) args
        -> go name e
      e -> pure e
  where
    go name e
       = do mbSpecDef <- speclookup name
            case mbSpecDef of
              Nothing -> pure e
              Just specDef
                | inlineName specDef /= thisDefName -> -- trace ("specialize " <> show (inlineName specDef) <> " in " <> show thisDefName) $
                                                       specOneCall specDef e   -- don't specialize ourselves
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
specOneCall inlineDef@(InlineDef{ inlineName=specName, inlineExpr=specExpr, inlineParamSpecialize=specArgs, inlineSort=sort }) e
  = case e of
      App (Var (TName name _) _) args
       | gArgs <- goodArgs specArgs args
       , any isJust gArgs
        -> replaceCall specName specExpr sort specArgs (newArgs gArgs args) Nothing
      App (TypeApp (Var (TName name ty) _) typeArgs) args
       | gArgs <- goodArgs specArgs args
       , any isJust gArgs
        -> replaceCall specName specExpr sort specArgs (newArgs gArgs args) $ Just typeArgs
      _ -> return e

  where newArgs gArgs args = zipWith fromMaybe args gArgs

-- specOneCall :: InlineDef -> Expr -> SpecM Expr
-- specOneCall inlineDef@(InlineDef{ inlineName=specName, inlineExpr=specExpr, inlineParamSpecialize=specArgs }) e
--   = case e of
--       App (Var (TName name _) _) args
--         | gArgs <- goodArgs specArgs args
--         , any isJust gArgs
--         -> replaceCall specName specExpr (map isJust gArgs) (newArgs gArgs args) Nothing
--       App (TypeApp (Var (TName name ty) _) typeArgs) args
--         | gArgs <- goodArgs specArgs args
--         , any isJust gArgs
--         -> replaceCall specName specExpr (map isJust gArgs) (newArgs gArgs args) $ Just typeArgs
--       _ -> return e

--   where newArgs gArgs args = zipWith fromMaybe args gArgs -- map fromJust $ zipWith (<|>) gArgs (map Just args)

goodArgs :: [Bool] -> [Expr] -> [Maybe Expr]
-- goodArgs = zipWith (\b arg -> guard b >> goodArg arg)
goodArgs bools exprs = map (\(b, e) -> guard b >> goodArg e >> Just e) $ zip bools exprs

goodArg :: Expr -> Maybe Expr
goodArg expr = -- (\isgood -> trace ("expr: " ++ show expr ++ " is good? " ++ show (isJust $ isgood)) $ isgood) $
               case expr of
                Lam{}                  -> Just expr
                TypeLam _ body         -> goodArg body >> Just expr
                TypeApp body _         -> goodArg body >> Just expr
                App fun _              -> goodArg fun  >> Just expr-- ??  for open(f) calls?
                -- Var name info | show (getName name) == ".spec.1052" -> Just expr
                Var name info          -> case info of
                                            InfoNone -> Nothing
                                            _        -> Just expr
                _                      -> Nothing


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
{-
specInnerCalls :: TName -> TName -> [Bool] -> Expr -> Expr
specInnerCalls from to bools expr =
  -- substitute first to avoid name capture
  let arity = length $ filter id bools
      sexpr = [(from,Var to (InfoArity 0 arity))] |~> expr
  -- then adjust arguments
      rewrite e
        = case e of
            App (Var v info) xs
              | v == to -> App (Var v info) $ filterBools bools xs
            App (TypeApp (Var v info) _) xs
            -- the resulting specialized definition is always monomophic; when generating the Inlines we only allow functions whose TypeApps match in ALL recursive calls
            -- and we apply the TypeLam to the TypeApps from the call site
              | v == to -> App (Var v info) $ filterBools bools xs
            e -> e
  in rewriteBottomUp rewrite sexpr
-}

-- capture avoiding rewrite of all recursive calls with the given specialized parameter names
specInnerCalls :: TName -> TName -> [Bool] -> [TName] -> Expr -> Expr
specInnerCalls from to isSpecParam specParamNames expr
  = sicExpr expr
  where
    arityTo  = length $ filter id (map not isSpecParam)
    varTo    = Var to (InfoArity 0 arityTo)


    matchSpecParamNames args
      = (length args == length specParamNames) &&
        all eqVar (zip specParamNames args)

    eqVar (name,Var v _)  = name == v
    eqVar (name,arg)      = trace ("specialize: specInnerCalls: argument does not match: " ++ show name ++ " as " ++ show arg) $
                            False

    sicAppTo args
      = App varTo (map sicExpr (filterBools (map not isSpecParam) args))

    sicExpr expr
      = case expr of
          -- rewrite recursive calls
          App (Var v info) args  | v == from && matchSpecParamNames (filterBools isSpecParam args)
            -> sicAppTo args
          App (TypeApp (Var v info) targs) args  | v == from && matchSpecParamNames (filterBools isSpecParam args)
            -> sicAppTo args  -- always monomorph

          -- visitor
          Lam params eff body
            | any (== from) params -> expr  -- avoid capture
            | otherwise            -> Lam params eff (sicExpr body)
          App f args         -> App (sicExpr f) (map sicExpr args)
          TypeLam tpars e    -> TypeLam tpars (sicExpr e)
          TypeApp e targs    -> TypeApp (sicExpr e) targs
          Let defGroups body -> sicLet defGroups body
          Case exprs branches-> Case (map sicExpr exprs) (map sicBranch branches)
          Var tname info     -> expr
          Con tname repr     -> expr
          Lit lit            -> expr

    -- capture avoiding rewrite over let bindings
    sicLet [] body  = sicExpr body
    sicLet (DefNonRec def : defs) body
      | defTName def == from
        = makeLet (DefNonRec (sicDef def) : defs) body
      | otherwise
        = makeLet [DefNonRec (sicDef def)]  (sicLet defs body)
    sicLet (DefRec rdefs : defs) body
      | any (\d -> defTName d == from) rdefs
        = makeLet (DefRec rdefs : defs) body
      | otherwise
        = makeLet [DefRec (map sicDef rdefs)] (sicLet defs body)

    -- capture avoiding rewrite over branches
    sicBranch branch@(Branch patterns guards)
      | any (== from) (tnamesList (bv patterns)) = branch
      | otherwise = Branch patterns (map sicGuard guards)

    sicGuard (Guard test body) = Guard (sicExpr test) (sicExpr body)
    sicDef def = def{ defExpr = sicExpr (defExpr def) }




comment :: String -> String
comment = unlines . map ("// " ++) . lines

-- At this point we've identified a call to a specializable function with a 'known' argument passed for all specializable parameters
-- A couple steps here to avoid looping when getting the type of the specialized Def (e.g. in the spec_f example above)
-- 1. Find the body of spec_f e.g. val y = yexpr; ...body of f...
-- 2. Get the type of this body
-- 3. Only then, replace the recursive calls to f in the body (specInnerCalls)
-- The important thing is that we don't try to get the type of the body at the same time as replacing the recursive calls
-- since the type of the body depends on the type of the functions that it calls and vice versa
replaceCall :: Name -> Expr -> DefSort -> [Bool] -> [Expr] -> Maybe [Type] -> SpecM Expr
replaceCall name expr0 sort bools args mybeTypeArgs
  = do
      expr <- uniquefyExprU expr0

      -- extract the specialized parameters
      let ((newParams, newArgs), (speccedParams, speccedArgs))
            = (unzip *** unzip)
              -- $ (\x@(new, spec) -> trace ("Specializing to newArgs " <> show new) $ x)
              $ partitionBools bools
              $ zip (fnParams expr) args

      -- create a new (recursive) specialized body where the specialized parameters become local defitions
      let specBody0
            = (\body -> case mybeTypeArgs of
                Nothing -> body
                Just typeArgs -> subNew (zip (fnTypeParams expr) typeArgs) |-> body)
              $ Lam newParams (fnEffect expr)  -- fn <newparams>
              $ Let [DefNonRec $ Def param typ arg Private DefVal InlineAuto rangeNull ""  -- bind specialized parameters
                      | (TName param typ, arg) <- zip speccedParams speccedArgs]
              $ fnBody expr


      -- substitute self-recursive calls to call our new specialized definition (without the specialized arguments!)
      specName <- uniqueName "spec"
      let specType  = typeOf specBody0
          specTName = TName specName specType
          specBody  = case specBody0 of
                        Lam args eff (Let specArgs body)
                          -> -- uniquefyExpr $
                             Lam args eff $
                               (Let specArgs $
                                specInnerCalls (TName name (typeOf expr)) specTName bools speccedParams body)
                        _ -> failure "Specialize.replaceCall: Unexpected output from specialize pass"

      -- simplify so the new specialized arguments are potentially inlined unlocking potential further specialization
      sspecBody <- uniqueSimplify defaultEnv False False 1 10 specBody
      -- trace ("\n// ----start--------\n// specializing " <> show name <> " to parameters " <> show speccedParams <> " with args " <> comment (show speccedArgs) <> "\n// specTName: " <> show (getName specTName) <> ", specBody0: \n" <> show specBody <> "\n\n, sspecBody: \n" <> show sspecBody <> "\n// ---- start recurse---") $ return ()

      let specDef = Def specName specType sspecBody Private sort InlineAuto rangeNull
                     $ "// specialized: " <> show name <> ", on parameters " <> concat (intersperse ", " (map show speccedParams)) <> ", using:\n" <>
                       comment (unlines [show param <> " = " <> show arg | (param,arg) <- zip speccedParams speccedArgs])

      return $ Let [DefRec [specDef]] (App (Var (defTName specDef) InfoNone) newArgs)

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

extractSpecializeDefs :: Inlines -> DefGroups -> [InlineDef]
extractSpecializeDefs loadedInlines dgs =
    inlinesToList
  $ flip (multiStepInlines loadedInlines) (flattenDefGroups dgs)
  $ inlinesNew
  $ mapMaybe (\def -> makeSpecialize def)
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
            map (filterMaybe (isFun . tnameType))
            $ allPassedInSameOrder params recArgs

      guard (any isJust specializableParams)
      Just $ InlineDef (defName def) (defExpr def) True InlineAuto (costDef def) (defSort def) (map isJust specializableParams)

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
      -- _ -> (Nothing,[])
      _ -> failure ("Core.Specialize: recursiveCalls: not a function: " ++ show thisDefName ++ ": " ++ show expr)
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

multiStepInlines :: Inlines -> Inlines -> [Def] -> Inlines
multiStepInlines loadedInlines inlines = snd . foldl' f (inlines `inlinesMerge` loadedInlines, inlines)
  where
    -- seq here since we're using foldl'?
    -- f inlines def | trace ("checking " <> show (defName def)) False = undefined
    f (allInlines, newInlines) def
    -- inlineCost ?
      | not (defIsVal def) -- isFun (defType def)
      , defInline def /= InlineNever
      , Just specArgs <- callsSpecializable allInlines def =
          -- inlineCost = 1 here since kki complains about inline + specialize
          let new = InlineDef (defName def) (defExpr def) False InlineAuto 1 (defSort def) specArgs
          in ((,) `on` inlinesExtend new) allInlines newInlines
    f inlines _ = inlines

    -- look for calls to specializable functions where we don't know the RHS of an argument
    -- references that aren't calls aren't eligible for multi-step specialization; we don't have the specializable args anyway
    callsSpecializable inlines def = getAlt $ flip foldMapExpr (defExpr def) $ \e -> case e of
      App (Var (TName name _) _) args -> goCommon name args
      App (TypeApp (Var (TName name _) _) _) args -> goCommon name args
      e -> mempty

      where
        params = fnParams $ defExpr def

        goCommon :: Name -> [Expr] -> Alt Maybe [Bool]
        goCommon name args
          | Just InlineDef{ inlineParamSpecialize=specArgs } <- inlinesLookup name inlines
          , name /= defName def = do
              let spArgs = filterBools specArgs args
              let overlap = map (`elem` concatMap vars spArgs) params
              guard (or overlap)
              -- traceM ("Add " ++ show (defName def) ++ " as multi-step specializable for params " <> show overlap <> " because calls " ++ show name)
              pure overlap
        goCommon _ _ = mempty

vars :: Expr -> [TName]
vars = foldMapExpr $ \e -> case e of
  Var tname _ -> [tname]
  _ -> []

module Core.Specialize( SpecializeEnv
                      , specenvNew
                      , specenvEmpty
                      , specenvExtend, specenvExtends
                      , specenvLookup
                      , ppSpecializeEnv

                      , specialize

                      , extractSpecializeEnv
                      ) where

import Data.List (transpose)
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((***))
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

{--------------------------------------------------------------------------
  
--------------------------------------------------------------------------}

data SpecializeInfo = SpecializeInfo
  { specName :: Name
  , specExpr :: Expr
  , specArgs :: [Bool]
  } deriving (Show)


{--------------------------------------------------------------------------
  Specialization Monad
--------------------------------------------------------------------------}

type SpecM = Reader SpecializeEnv

runSpecM :: SpecializeEnv -> SpecM a -> a
runSpecM specEnv specM = runReader specM specEnv


{--------------------------------------------------------------------------
  Specialization
--------------------------------------------------------------------------}

specialize :: Env -> Int -> SpecializeEnv -> DefGroups -> (DefGroups, Int)
specialize env uniq specEnv groups = (runSpecM specEnv $ mapM specOneDefGroup groups, uniq)

speclookupM :: Name -> SpecM (Maybe SpecializeInfo)
speclookupM name = asks (specenvLookup name)

specOneDefGroup :: DefGroup -> SpecM DefGroup
specOneDefGroup = mapMDefGroup specOneDef

specOneDef :: Def -> SpecM Def
specOneDef def = do
  e <- specOneExpr (defName def) $ defExpr def
  pure def{ defExpr = e }

-- forceExpr :: Expr -> Expr
-- forceExpr e = foldExpr (const (+1)) 0 e `seq` e

specOneExpr :: Name -> Expr -> SpecM Expr
specOneExpr thisDefName = rewriteBottomUpM $ \e -> case e of
  App (Var (TName name _) _) args -> go name e
  App (TypeApp (Var (TName name _) _) typVars) args -> go name e
  e -> pure e
  where
    go name e = do
      specDef <- speclookupM name
      case specDef of
        Nothing -> pure e
        Just def 
          | specName def /= thisDefName -> specOneCall def e
          | otherwise -> pure e

filterBools :: [Bool] -> [a] -> [a]
filterBools bools as = catMaybes $ zipWith (\bool a -> guard bool >> Just a) bools as

-- (falses, trues)
partitionBools :: [Bool] -> [a] -> ([a], [a])
partitionBools bools as = foldr f ([], []) $ zip bools as
  where
    f (bool, a) (falses, trues)
      | bool = (falses, a : trues)
      | otherwise = (a : falses, trues)

specOneCall :: SpecializeInfo -> Expr -> SpecM Expr
specOneCall s@(SpecializeInfo{ specName=specName, specExpr=specExpr, specArgs=specArgs }) e = case e of
  App (Var (TName name _) _) args -> replaceCall specName specExpr specArgs args Nothing
  App (TypeApp (Var (TName name ty) _) typeArgs) args -> replaceCall specName specExpr specArgs args $ Just typeArgs
  _ -> error "specOneCall"

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
specInnerCalls from to bools = rewriteBottomUp $ \e -> case e of
  App (Var f info) xs
    | f == from -> App (Var to info) $ filterBools bools xs
  App (TypeApp (Var f info) _) xs
  -- the resulting specialized definition is always monomophic; when generating the SpecializeEnv we only allow functions whose TypeApps match in ALL recursive calls
  -- and we apply the TypeLam to the TypeApps from the call site
    | f == from -> App (Var to info) $ filterBools bools xs
  e -> e

replaceCall :: Name -> Expr -> [Bool] -> [Expr] -> Maybe [Type] -> SpecM Expr
replaceCall name expr bools args mybeTypeArgs = -- trace ("specializing" <> show name) $
  pure $ Let [DefRec [specDef]] (App (Var (defTName specDef) InfoNone) newArgs)
  where
    specDef = Def (genSpecName name bools) specType specBody Private DefFun InlineAuto rangeNull 
               $ "// specialized " <> show name <> " to parameters " <> show speccedParams
    -- there's some knot-tying here: we need to know the type of the specialized fn to replace recursive calls with the specialized version, and the new body also influences the specialized type
    specBody =
      (\body -> case mybeTypeArgs of
        Nothing -> body
        Just typeArgs -> subNew (zip (fnTypeParams expr) typeArgs) |-> body)
      $ Lam newParams (fnEffect expr) $ Let [DefNonRec $ Def param typ arg Private DefVal InlineAuto rangeNull "" | (TName param typ, arg) <- zip speccedParams speccedArgs]
      $ specInnerCalls (TName name $ typeOf expr) (defTName specDef) (not <$> bools)
      $ fnBody expr

    specType = typeOf specBody

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

extractSpecializeEnv :: DefGroups -> SpecializeEnv
extractSpecializeEnv = 
    specenvNew
  . mapMaybe getSpecInfo
  . flattenDefGroups
  . filter isRecursiveDefGroup

  where
    isRecursiveDefGroup (DefRec [def]) = True
    isRecursiveDefGroup _ = False

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f Nothing = Nothing
filterMaybe f (Just a) = guard (f a) >> pure a

whenJust :: (Applicative f) => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

getSpecInfo :: Def -> Maybe SpecializeInfo
getSpecInfo def = do
  let (mbTypes, recArgs) = recursiveCalls def
  whenJust mbTypes $ (guard . allEq)

  let params = fnParams $ defExpr def
  let specializableParams =
          map   (filterMaybe ((`S.member` usedInThisDef def) . getName)
              . (filterMaybe (isFun . tnameType)))
        $ allPassedInSameOrder params recArgs
  
  guard (any isJust specializableParams)
  pure $ SpecializeInfo (defName def) (defExpr def) $ map isJust specializableParams

allPassedInSameOrder :: [TName] -> [[Expr]] -> [Maybe TName]
allPassedInSameOrder params calls = 
    -- head is safe because sublists in transpose can't be empty
    map (fmap head . sequence)
  $ transpose
  $ map (passedInSameOrder params) calls

passedInSameOrder :: [TName] -> [Expr] -> [Maybe TName]
passedInSameOrder params args = zipWith f params args
  where 
    f param arg
     | Var tname _ <- arg
     , tname == param = Just tname
    f _ _ = Nothing

isFun = isJust . splitFunScheme

usedInThisDef :: Def -> S.NameSet
usedInThisDef def = foldMapExpr go $ defExpr def
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
recursiveCalls Def{ defName=thisDefName, defExpr=expr } = case expr of
  Lam params eff body -> go body
  TypeLam types (Lam params eff body) -> go body
  _ -> failure "recursiveCalls: not a function"
  where
    go body =
      let (types, args) = unzip $ foldMapExpr f body
      in (sequence types, args)

    f (App (Var (TName name _) _) args) = pure (Nothing, args)
    f (App (TypeApp (Var (TName name _) _) types) args) = pure (Just types, args)
    f _ = []


{--------------------------------------------------------------------------
  Specialize Environment
--------------------------------------------------------------------------}

-- | Environment mapping names to specialize definitions
newtype SpecializeEnv   = SpecializeEnv (M.NameMap SpecializeInfo)

-- | The intial SpecializeEnv
specenvEmpty :: SpecializeEnv
specenvEmpty
  = SpecializeEnv M.empty

specenvNew :: [SpecializeInfo] -> SpecializeEnv
specenvNew xs
  = specenvExtends xs specenvEmpty

specenvExtends :: [SpecializeInfo] -> SpecializeEnv -> SpecializeEnv
specenvExtends xs specenv
  = foldr specenvExtend specenv xs

specenvExtend :: SpecializeInfo -> SpecializeEnv -> SpecializeEnv
specenvExtend idef (SpecializeEnv specenv)
  = SpecializeEnv (M.insert (specName idef) idef specenv)

specenvLookup :: Name -> SpecializeEnv -> Maybe SpecializeInfo
specenvLookup name (SpecializeEnv specenv)
  = M.lookup name specenv


instance Show SpecializeEnv where
 show = show . pretty

instance Pretty SpecializeEnv where
 pretty g
   = ppSpecializeEnv defaultEnv g


ppSpecializeEnv :: Env -> SpecializeEnv -> Doc
ppSpecializeEnv env (SpecializeEnv specenv)
   = vcat [fill maxwidth (ppName env name) <+> list (map pretty (specArgs sdef))
          | (name,sdef) <- M.toList specenv]
   where
     maxwidth      = 12

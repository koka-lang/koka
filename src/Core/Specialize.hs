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
import Type.Type (splitFunScheme, Effect)
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

data SpecState = SpecState
  { _inScope :: NameMap Expr
  , _newDefs :: DefGroups
  } deriving (Show)

type SpecM = ReaderT SpecializeEnv (State SpecState)

runSpecM :: NameMap Expr -> SpecializeEnv -> SpecM a -> (a, DefGroups)
runSpecM scope specEnv specM = 
  let (result, SpecState _ newDefs) = flip runState (SpecState { _inScope = scope, _newDefs = [] }) $ flip runReaderT specEnv specM
  in (result, newDefs)

-- emitSpecializedDefGroup :: DefGroup -> SpecM ()
-- emitSpecializedDefGroup defGroup = modify (\state@SpecState { _newDefs = newDefs } -> state{ _newDefs = defGroup:newDefs})


{--------------------------------------------------------------------------
  Specialization
--------------------------------------------------------------------------}

specialize :: Env -> Int -> SpecializeEnv -> DefGroups -> (DefGroups, Int)
specialize env uniq specEnv groups =
  let 
    -- TODO: initial scope isn't empty
    (changedDefs, newDefs) = runSpecM M.empty specEnv $ mapM specOneDefGroup groups
  in (changedDefs ++ newDefs, uniq)

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
  App (TypeApp (Var (TName name _) _)_) args -> go name e
  e -> pure e
  where
    go name e = do
      specDef <- speclookupM name
      case specDef of
        Nothing -> pure e
        Just def 
          -- bandaid check to not try to spec the same def; in practice we should check if the RHS is known
          | specName def /= thisDefName -> specOneCall def e
          | otherwise  -> pure e

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
  App (Var (TName name _) _) args -> replaceCall specName specExpr specArgs args

  -- the result may no longer be a typeapp
  -- App (TypeApp (Var (TName name _) _) _) args -> undefined
  _ -> error "specOneCall"

specInnerCalls :: TName -> TName -> [Bool] -> Expr -> Expr
specInnerCalls from to bools = rewriteBottomUp $ \e -> case e of
  App (Var f info) xs
    | f == from -> App (Var to info) $ filterBools bools xs
  -- TODO TypeApp case
  e -> e

replaceCall :: Name -> Expr -> [Bool] -> [Expr] -> SpecM Expr
replaceCall name expr bools args = do
  -- ruExpr doesn't let me use DefRec here but it probably should be
  pure $ Let [DefNonRec specDef] (App (Var (defTName specDef) InfoNone) newArgs)
  where
    specDef = Def (genSpecName name bools) specType specBody Private DefFun InlineAuto rangeNull $ "// specialized " <> show name <> " to parameters " <> " TODO "
    -- there's some knot-tying here: we need to know the type of the specialized fn to replace recursive calls with the specialized version, and the new body also influences the specialized type
    specBody = Lam newParams (fnEffect expr) $ Let [DefNonRec $ Def param typ arg Private DefVal InlineAuto rangeNull "" | (TName param typ, arg) <- zip speccedParams speccedArgs]
      $ specInnerCalls (TName name $ typeOf expr) (defTName specDef) (not <$> bools)
      $ fnBody expr

    specType = typeOf specBody

    ((newParams, newArgs), (speccedParams, speccedArgs)) =
      (unzip *** unzip)
      -- (\x@(new, spec) -> trace ("Specializing to newArgs " <> show new) $ x) $
      $ partitionBools bools
      $ zip (fnParams expr) args

genSpecName :: Name -> [Bool] -> Name
genSpecName name bools = newName $ "spec_" ++ last (splitOn (== '/') $ show name)

fnParams :: Expr -> [TName]
fnParams (Lam params _ _) = params
fnParams (TypeLam _ (Lam params _ _)) = params
fnParams _ = error "fnParams"

fnEffect :: Expr -> Effect
fnEffect (Lam _ effect _) = effect
fnEffect (TypeLam _ (Lam _ effect _)) = effect
fnEffect _ = error "fnEffect"

fnBody :: Expr -> Expr
fnBody (Lam _ _ body) = body
fnBody (TypeLam _ (Lam _ _ body)) = body
fnBody _ = error "fnBody"


{--------------------------------------------------------------------------
  Extract definitions that should be specialized
--------------------------------------------------------------------------}

extractSpecializeEnv :: DefGroups -> SpecializeEnv
extractSpecializeEnv = 
    specenvNew
  . filter (or . specArgs)
  . map getSpecInfo
  . flattenDefGroups
  . filter isRecursiveDefGroup

  where
    isRecursiveDefGroup (DefRec [def]) = True
    isRecursiveDefGroup _ = False

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f Nothing = Nothing
filterMaybe f (Just a) = guard (f a) >> pure a

getSpecInfo :: Def -> SpecializeInfo
getSpecInfo def =
  SpecializeInfo (defName def) (defExpr def)
  $ map isJust
  $ map (filterMaybe (`S.member` usedInThisDef def))
  $ map (fmap getName)
  $ map (filterMaybe (isFun . tnameType))
  $ passedRecursivelyToThisDef def

isFun = isJust . splitFunScheme

usedInThisDef :: Def -> S.NameSet
usedInThisDef def = foldMapExpr go $ defExpr def
  where 
    go (Var (TName name _) _) = S.singleton name
    go _ = mempty
    -- go (App (Var (TName name _) _) xs)             = S.singleton name
    -- go (App (TypeApp (Var (TName name _) _) _) xs) = S.singleton name
    -- go _ = S.empty

-- return list of parameters that get called recursively to the same function in the same order
-- or Nothing if the parameter wasn't passed recursively in the same order
passedRecursivelyToThisDef :: Def -> [Maybe TName]
passedRecursivelyToThisDef def 
  -- TODO: FunDef type to avoid this check?
  = case defExpr def of
      Lam params effect body -> 
        go body params
      TypeLam _ (Lam params effect body) -> 
        go body params
      _ -> mempty
  where
    -- only keep params that are passed recursively in ALL calls
    go body params = 
      -- head is safe because sublists in transpose can't be empty
      map (fmap head . sequence)
      $ transpose 
      $ foldMapExpr ((:[]) . callsWith params) body

    dname = defName def

    callsWith params (App (Var (TName name _) _) args)
      | name == dname  = check args params
    callsWith params (App (TypeApp (Var (TName name _) _) _) args)
      | name == dname  = check args params
    callsWith params _ = mempty

    check args params =
      zipWith (\arg param ->
        case arg of
          Var tname _ | tname == param -> Just tname
          _ -> Nothing) args params


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

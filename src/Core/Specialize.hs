{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Core.Specialize( SpecializeEnv
                      , specenvNew
                      , specenvEmpty
                      , specenvExtend, specenvExtends
                      , specenvLookup
                      , ppSpecializeEnv

                      , specialize

                      , extractSpecializeDefs 
                      ) where

import Data.Bifunctor
import Control.Monad.State
import Control.Applicative
import Data.Maybe (mapMaybe, fromMaybe, catMaybes, isJust, fromJust)

import Lib.PPrint
import Common.Syntax
import Common.Name
import Common.NameMap (NameMap)
import qualified Common.NameMap  as M
import Common.NameSet (NameSet)
import qualified Common.NameSet as S
import Core.Core
import Core.Pretty ()
import Type.Pretty
import Lib.Trace

data SpecializeDefs = SpecializeDefs
  { targetFunc :: Name
  , argsToSpecialize :: [Bool]
  } deriving (Show)

data SpecState = SpecState
  { _inScope :: NameMap Expr
  , _newDefs :: DefGroups
  } deriving (Show)

type SpecM = State SpecState

runSpecM :: NameMap Expr -> SpecM a -> (a, DefGroups)
runSpecM scope specM = 
  let (result, SpecState _ newDefs) = flip runState (SpecState { _inScope = scope, _newDefs = [] }) specM
  in (result, newDefs)

inScope :: SpecM (NameMap Expr)
inScope = gets _inScope

queryScope :: Name -> SpecM (Maybe Expr)
queryScope name = gets (M.lookup name . _inScope)

addToScope :: Name -> Expr -> SpecM ()
addToScope name expr = modify (\state@SpecState{ _inScope = inScope } -> state{ _inScope = M.insert name expr inScope })

emitSpecializedDef :: DefGroup -> SpecM ()
emitSpecializedDef defGroup = modify (\state@SpecState { _newDefs = newDefs } -> state{ _newDefs = defGroup:newDefs})

specialize :: Env -> Int -> SpecializeEnv -> DefGroups -> (DefGroups, Int)
specialize env uniq specEnv groups =
  let 
    -- TODO: initial scope isn't empty
    (changedDefs, newDefs) = runSpecM M.empty $ mapM (specOneDefGroup specEnv) groups
  in (changedDefs ++ newDefs, uniq)

specOneDefGroup :: SpecializeEnv -> DefGroup -> SpecM DefGroup
specOneDefGroup env (DefRec [def]) = do
  specialized <- specOneDef env def
  pure $ DefRec [specialized]
specOneDefGroup _ group = pure group

specOneDef :: SpecializeEnv -> Def -> SpecM Def
specOneDef env def = 
  let 
    -- account for typelam
    Lam params eff body = defExpr def

    go = rewriteBottomUpM (\e -> case e of
      App (Var (TName name _) _) args 
        | Just (SpecializeDefs{argsToSpecialize=argsToSpecialize}) <- specenvLookup name env -> do
            -- we should keep this as a list of bool, no need to do this translation since we will need it again when removing the args
            -- and adding them to the new def
            candidates <- zipWithM (\isSpecializeCandidate arg -> if isSpecializeCandidate then argHasKnownRHS arg else pure Nothing) argsToSpecialize args
            specOneCall e candidates
      
      -- TODO:
      -- App (TypeApp (Var (TName name _) _) _) xs 
      --   | Just spec <- specenvLookup name env -> 
      --       maybe body (\rhs -> specOneCall body spec rhs) <$> queryScope name

      e -> pure e)

    argHasKnownRHS (Var (TName name _) _) = queryScope name
    -- double-check this
    argHasKnownRHS (TypeApp (Var (TName name _) _) _) = queryScope name
    argHasKnownRHS e = pure $ Just e
      
    specOneCall :: Expr -> [Maybe Expr] -> SpecM Expr
    specOneCall (App (Var (TName name _) _) args) argsToSpecialize = do
      -- name must be in scope since it's specializable
      defToSpecialize <- fromJust <$> queryScope name
      let params = case defToSpecialize of
            Lam params _ _ -> params
            TypeLam _ (Lam params _ _) -> params
            _ -> error $ show defToSpecialize
      let specName = newName "spec"
      let namesToReplace = zipWith (\name mybeArg -> (name,) <$> mybeArg) params argsToSpecialize
      let letDefGroups = mapMaybe ((\(TName name _, expr) -> DefNonRec $ Def name (error "type") expr Private DefVal InlineAuto (error "Range2") (error "doc")) <$>) namesToReplace
      let newParams = catMaybes $ zipWith (\spec param -> spec >> pure param) argsToSpecialize params
      let newBody = Lam newParams eff $ Let letDefGroups body
      let specializedDef = DefRec [Def name (error "type") newBody Private DefFun InlineNever (error "range") (error "doc")]

      let newArgs = catMaybes $ zipWith (\spec arg -> spec >> pure arg) argsToSpecialize args

      emitSpecializedDef specializedDef

      -- TODO info
      -- TODO TypeApp case ?
      pure $ App (Var (TName name $ error "error: type") InfoNone) newArgs

    -- TODO
    specOneCall e@(App (TypeApp (Var (TName name _) _) _) args) argsToSpecialize = pure e

    res = case defExpr def of 
      Lam params eff body -> Lam params eff <$> go body
      TypeLam types (Lam params eff body) -> TypeLam types <$> (Lam params eff <$> go body)
  in
    do 
      r <- res
      -- Type needs to change too
      pure def { defExpr = r }

extractSpecializeDefs :: DefGroups -> SpecializeEnv
extractSpecializeDefs = 
    specenvNew
  . filter (not . null . argsToSpecialize)
  . map getInline
  . flattenDefGroups
  . filter isRecursiveDefGroup

  where
    isRecursiveDefGroup (DefRec [def]) = True
    isRecursiveDefGroup _ = False

getInline :: Def -> SpecializeDefs
getInline def =
    SpecializeDefs (defName def)
  $ toBools
  $ map (\(_, argPosition) -> argPosition)
  $ filter (\(name, _) -> name `S.member` usedInThisDef def) 
  $ M.toList (passedRecursivelyToThisDef def)

type DistinctSorted a = a

-- list passed in should be sorted and not contain duplicates
-- >>> toBools [1, 3, 4, 7]
-- [False, True, False, True, True, False, False, True]
toBools :: [Int] -> [Bool]
toBools [] = []
toBools xs = foldr go (replicate (maximum xs + 1) False) xs
  where
    go i xs = at i (const True) xs

at :: Int -> (a -> a) -> [a] -> [a]
at i f xs =
  let (begin, x:end) = splitAt i xs
  in begin ++ f x : end

usedInThisDef :: Def -> S.NameSet
usedInThisDef def = foldMapExpr go $ defExpr def
  where 
    go (Var (TName name _) _) = S.singleton name
    go _ = mempty
    -- go (App (Var (TName name _) _) xs)             = S.singleton name
    -- go (App (TypeApp (Var (TName name _) _) _) xs) = S.singleton name
    -- go _ = S.empty

-- return list of (paramName, paramIndex) that get called recursively to the same function in the same order
passedRecursivelyToThisDef :: Def -> NameMap Int
passedRecursivelyToThisDef def 
  -- TODO: FunDef type to avoid this check?
  = case defExpr def of
      Lam params effect body 
        -> foldMapExpr (callsWith params) $ defExpr def
      TypeLam _ (Lam params effect body) 
        -> foldMapExpr (callsWith params) $ defExpr def
      _ -> mempty
  where
    dname = defName def

    callsWith params (App (Var (TName name _) _) args)
      | name == dname  = check args params
    callsWith params (App (TypeApp (Var (TName name _) _) _) args)
      | name == dname  = check args params
    callsWith params _ = mempty

    check args params =
      M.fromList $ flip mapMaybe (zip3 [0..] args params) $ \(i, arg, param) ->
        case arg of
          Var tname _ | tname == param -> Just (getName tname, i)
          _ -> Nothing


{--------------------------------------------------------------------------
  
--------------------------------------------------------------------------}

-- | Environment mapping names to specialize definitions
newtype SpecializeEnv   = SpecializeEnv (M.NameMap SpecializeDefs)

-- | The intial SpecializeEnv
specenvEmpty :: SpecializeEnv
specenvEmpty
  = SpecializeEnv M.empty

specenvNew :: [SpecializeDefs] -> SpecializeEnv
specenvNew xs
  = specenvExtends xs specenvEmpty

specenvExtends :: [SpecializeDefs] -> SpecializeEnv -> SpecializeEnv
specenvExtends xs specenv
  = foldr specenvExtend specenv xs

specenvExtend :: SpecializeDefs -> SpecializeEnv -> SpecializeEnv
specenvExtend idef (SpecializeEnv specenv)
  = SpecializeEnv (M.insert (targetFunc idef) idef specenv)

specenvLookup :: Name -> SpecializeEnv -> Maybe SpecializeDefs
specenvLookup name (SpecializeEnv specenv)
  = M.lookup name specenv


instance Show SpecializeEnv where
 show = show . pretty

instance Pretty SpecializeEnv where
 pretty g
   = ppSpecializeEnv defaultEnv g


ppSpecializeEnv :: Env -> SpecializeEnv -> Doc
ppSpecializeEnv env (SpecializeEnv specenv)
   = vcat [fill maxwidth (ppName env name) <+> list (map pretty (argsToSpecialize sdef))
          | (name,sdef) <- M.toList specenv]
   where
     maxwidth      = 12

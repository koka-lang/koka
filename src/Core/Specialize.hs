module Core.Specialize where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)

import Common.Name
import Common.NameMap (NameMap)
import Common.NameSet (NameSet)
import qualified Common.NameSet as S
import Core.Core
import Core.Pretty ()
import Lib.Trace

data SpecializeDefs = SpecializeDefs
  { targetFunc :: Name
  , argsToSpecialize :: Set Int
  } deriving (Show)

extractSpecializeDefs :: DefGroups -> NameMap SpecializeDefs
extractSpecializeDefs = 
    M.fromList
  . filter (not . S.null . argsToSpecialize . snd)
  . map ((\specDefs@(SpecializeDefs name _) -> (name, specDefs)) . getInline)
  . flattenDefGroups
  . filter isRecursiveDefGroup

  where
    isRecursiveDefGroup (DefRec [def]) = True
    isRecursiveDefGroup _ = False

getInline :: Def -> SpecializeDefs
getInline def =
    SpecializeDefs (defName def)
  $ S.map snd
  $ S.filter (\(name, _) -> name `S.member` calledInThisDef def) (passedRecursivelyToThisDef def)

calledInThisDef :: Def -> NameSet
calledInThisDef def = foldMapExpr go $ defExpr def
  where 
    go (App (Var (TName name _) _) xs) = S.singleton name

    -- this doesn't seems to make a difference?
    go (App (TypeApp (Var (TName name _) _) _) xs) = S.singleton name
    go _ = S.empty

-- return list of (paramName, paramIndex) that get called recursively to the same function in the same order
passedRecursivelyToThisDef :: Def -> Set (Name, Int)
passedRecursivelyToThisDef def 
  -- TODO: FunDef type to avoid this check?
  | Lam args effect body <- defExpr def = foldMapExpr (go args) $ defExpr def
  | TypeLam _ (Lam params effect body) <- defExpr def = foldMapExpr (go params) $ defExpr def
  | otherwise = mempty
  where
    go :: [TName] -> Expr -> Set (Name, Int)
    go params (App (Var (TName name _) _) args)
      | name == defName def = doWork args params
    go params (App (TypeApp (Var (TName name _) _) _) args)
      | name == defName def = doWork args params
    go params _ = mempty

    doWork :: [Expr] -> [TName] -> Set (Name, Int)
    doWork args params =
      S.fromList $ flip mapMaybe (zip3 [0..] args params) $ \(i, arg, param) ->
        case arg of
          Var tname _ | tname == param -> Just (getName tname, i)
          _ -> Nothing

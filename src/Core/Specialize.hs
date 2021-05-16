module Core.Specialize where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (find)
import Control.Monad

import Common.Name
import Common.NameMap (NameMap)
import Common.NameSet (NameSet)
import qualified Common.NameSet as S
import Core.Core
import Core.Pretty ()
import Lib.Trace

data SpecializeDef = SpecializeDef
  { targetFunc :: Name
  -- this works if we pass in a named func but how can we uniquely identify a lambda?
  -- could be the position of the parameter, although that only works at the call site
  , argToSpecialize :: Int
  } deriving (Show)

extractSpecializeDefs :: DefGroups -> NameMap SpecializeDef
extractSpecializeDefs = 
  M.fromList 
  . map (\specDef@SpecializeDef {targetFunc=target} -> (target, specDef)) 
  . concatMap getInline 
  . allDefs

calledInThisDef :: Def -> NameSet
calledInThisDef def = foldMapExpr go $ defExpr def
  where 
    go (App (Var (TName name _) _) xs) = S.singleton name
    go _ = S.empty

passedRecursivelyToThisDef :: Def -> NameMap Int
passedRecursivelyToThisDef def 
  -- TODO: FunDef type to avoid this check?
  | Lam args effect body <- defExpr def = foldMapExpr (go args) $ defExpr def
  | otherwise = M.empty
  where
    go args (App (Var (TName name _) _) xs)
      | name == defName def =
          M.fromList $ do
            (i, Var tname _) <- zip [0..] xs
            case fmap fst $ find ((== tname) . snd) $ zip [0..] args of
              Nothing -> []
              -- index should match i.e. we didn't pass it in a different order in the recursive call
              Just index | i == index -> [(getName tname, i)]
    go args _ = M.empty

getInline :: Def -> [SpecializeDef]
getInline def = map (\(k, v) -> SpecializeDef (defName def) v) $ M.toList $ M.filterWithKey (\k v -> k `S.member` calledInThisDef def) (passedRecursivelyToThisDef def)

allDefs :: DefGroups -> [Def]
allDefs = concatMap handleGroup
  where
    handleGroup (DefNonRec def) = [def]
    handleGroup (DefRec defs) = defs

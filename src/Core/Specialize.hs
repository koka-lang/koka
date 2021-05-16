module Core.Specialize where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

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
  , argToSpecialize :: Name
  } deriving (Show)

extractSpecializeDefs :: DefGroups -> NameMap SpecializeDef
extractSpecializeDefs = 
  M.fromList 
  . map (\specDef@SpecializeDef {targetFunc=target} -> (target, specDef)) 
  . concatMap getInline 
  . allDefs

traceEq :: (Show a) => String -> a -> a
traceEq name val = trace (name ++ " = " ++ show val) val

calledInThisDef :: Def -> NameSet
calledInThisDef def = foldMapExpr go $ defExpr def
  where 
    go (App (Var (TName name _) _) xs) = S.singleton name
    go _ = S.empty

passedRecursivelyToThisDef :: Def -> NameSet
passedRecursivelyToThisDef def 
  -- TODO: FunDef type to avoid this check?
  | Lam args effect body <- defExpr def = foldMapExpr go $ defExpr def
  | otherwise = S.empty
  where
    Lam args effect body = defExpr def
    go (App (Var (TName name _) _) xs) | name == defName def = S.fromList $ [getName tname | (Var tname _) <- xs, tname `elem` args] -- filter (`elem` xs) args
    go _ = S.empty

getInline :: Def -> [SpecializeDef]
getInline def = map (\name -> SpecializeDef name name) $ S.toList $ S.intersection (calledInThisDef def) (passedRecursivelyToThisDef def)

allDefs :: DefGroups -> [Def]
allDefs = concatMap handleGroup
  where
    handleGroup (DefNonRec def) = [def]
    handleGroup (DefRec defs) = defs

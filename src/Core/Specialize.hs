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
calledInThisDef def = traceShow def $ traceEq "CalledInThisDef" $ foldMapExpr go $ defExpr def
  where 
    go (App (Var (TName name _) _) xs) = S.singleton name
    go _ = S.empty

passedRecursivelyToThisDef :: Def -> NameSet
passedRecursivelyToThisDef def 
  -- TODO: FunDef type to avoid this check?
  | Lam args effect body <- defExpr def = traceEq "passedRecursively" $ foldMapExpr go $ defExpr def
  | otherwise = S.empty
  where
    Lam args effect body = defExpr def
    go (App (Var (TName name _) _) xs) | name == defName def = S.fromList $ [getName tname | (Var tname _) <- xs, tname `elem` args] -- filter (`elem` xs) args
    go _ = S.empty

getInline :: Def -> [SpecializeDef]
getInline def = (\result -> trace (show (defName def) ++ show result) result) $ map (\name -> SpecializeDef name name) $ S.toList $ S.intersection (calledInThisDef def) (passedRecursivelyToThisDef def)

-- getInline :: Def -> [SpecializeDef]
-- getInline def
--   | (True, True) <- folded = [SpecializeDef (defName def) (newName hardCodedName)]
--   | otherwise = []
--   where
--     folded = foldExpr go (False, False) $ defExpr def

--     thisDefName = traceEq "thisDefName" $ defName def
--     hardCodedName = "f"

--     go e (True, True) = (True, True)

--     -- we applied this function somewhere
--     go (App (Var (TName name _) _) xs) (_, isPassedRecursively) 
--       | traceEq "Applied function" name == thisDefName = (True, isPassedRecursively)

--     -- we passed this function to myself
--     go (App f xs) (isCalledInThisDef, _) 
--       | or [traceEq "Arg Name" (show name) == hardCodedName | Var name info <- xs] = (isCalledInThisDef, True)
--     go _ results = results

-- return list of names that are functions that are recursively passed
-- recursiveFuncCalls :: Def -> [Name]
-- recursiveFuncCalls def = undefined

knownDefs :: DefGroups -> NameMap Def
knownDefs = M.fromList . fmap (\def -> (defName def, def)) . allDefs

allDefs :: DefGroups -> [Def]
allDefs = concatMap handleGroup
  where
    handleGroup (DefNonRec def) = [def]
    handleGroup (DefRec defs) = defs

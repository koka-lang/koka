module Core.Specialize where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

import Common.Name
import Common.NameMap (NameMap)
import Core.Core

data SpecializeDef = SpecializeDef
  { targetFunc :: Name
  -- this works if we pass in a named func but how can we uniquely identify a lambda?
  , argToSpecialize :: Name
  } deriving (Show)

extractSpecializeDefs :: DefGroups -> NameMap SpecializeDef
extractSpecializeDefs = 
  M.fromList 
  . map (\specDef@SpecializeDef {targetFunc=target} -> (target, specDef)) 
  . concatMap getInline 
  . allDefs

getInline :: Def -> [SpecializeDef]
getInline = undefined

-- return list of names that are functions that are recursively passed
recursiveFuncCalls :: Def -> [Name]
recursiveFuncCalls def = undefined

knownDefs :: DefGroups -> NameMap Def
knownDefs = M.fromList . fmap (\def -> (defName def, def)) . allDefs

allDefs :: DefGroups -> [Def]
allDefs = concatMap handleGroup
  where
    handleGroup (DefNonRec def) = [def]
    handleGroup (DefRec defs) = defs

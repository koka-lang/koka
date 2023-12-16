-----------------------------------------------------------------------------
-- The LSP handler that provides ctrl-click definitions
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.Definition (definitionHandler) where

import Compiler.Module (Loaded (..), loadedModule, modRangeMap)
import Control.Lens ((^.))
import qualified Data.Map as M
import Common.Range as R
import Data.Foldable(maximumBy)
import Data.Maybe (maybeToList)
import Kind.Constructors (conInfoRange, constructorsLookup)
import Kind.Newtypes (dataInfoRange, newtypesLookupAny)
import Kind.Synonym (synInfoRange, synonymsLookup)
import Language.LSP.Server (Handlers, requestHandler)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import LanguageServer.Conversions (fromLspPos, toLspLocation, toLspLocationLink)
import LanguageServer.Monad (LSM, getLoaded)
import Syntax.RangeMap (RangeInfo (..), rangeMapFindAt, NameInfo (..))
import Type.Assumption (gammaLookupQ, infoRange)
import qualified Language.LSP.Protocol.Message as J

-- Finds the definitions of the element under the cursor.
definitionHandler :: Handlers LSM
definitionHandler = requestHandler J.SMethod_TextDocumentDefinition $ \req responder -> do
  let J.DefinitionParams doc pos _ _ = req ^. J.params
      uri = doc ^. J.uri
  loaded <- getLoaded uri
  let defs = do -- maybe monad
        l <- maybeToList loaded
        rmap <- maybeToList $ modRangeMap $ loadedModule l
        rm <- maybeToList $ rangeMapFindAt (fromLspPos uri pos) rmap
        case rangeMapBestDefinition rm of
          Just (r, rinfo) -> findDefinitions l rinfo
          Nothing -> []
  responder $ Right $ J.InR $ J.InL defs

-- Get best rangemap info for a given position
rangeMapBestDefinition rm = 
  case rm of
    [] -> Nothing
    [r] -> Just r
    xs -> Just $ maximumBy (\a b -> compare (rangeInfoPriority a) (rangeInfoPriority b)) xs

rangeInfoPriority :: (Range,RangeInfo) -> Int
rangeInfoPriority (r,ri) =
  case ri of
    Block _ -> -1
    Id _ (NICon{}) True -> 3
    Id _ _ True -> 2
    Id _ _ _ -> 0
    Decl "con" _ _ -> 3 -- Constructors are more important than other decls (such as automatically generated ones)
    Decl _ _ _ -> 1
    Warning _ -> 4
    Error _ -> 5

-- Finds the definition locations of the element
-- represented by the given range info.
findDefinitions :: Loaded -> RangeInfo -> [J.DefinitionLink]
findDefinitions loaded rinfo = case rinfo of
  Id qname _ _ ->
    let rngs =
          map infoRange (gammaLookupQ qname gamma)
            ++ map conInfoRange (maybeToList $ constructorsLookup qname constrs)
            ++ map synInfoRange (maybeToList $ synonymsLookup qname synonyms)
            ++ map dataInfoRange (maybeToList $ newtypesLookupAny qname newtypes)
     in map (J.DefinitionLink . toLspLocationLink rinfo) rngs
  _ -> []
  where
    gamma = loadedGamma loaded
    constrs = loadedConstructors loaded
    synonyms = loadedSynonyms loaded
    newtypes = loadedNewtypes loaded

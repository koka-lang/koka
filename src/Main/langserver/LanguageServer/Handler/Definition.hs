------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting, Fredrik Wieczerkowski
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The LSP handler that provides ctrl-click definitions
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.Definition (definitionHandler) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Foldable(maximumBy)
import Data.Maybe (maybeToList)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.Server (Handlers, requestHandler)
import Common.Range as R
import Kind.Constructors (conInfoRange, constructorsLookup)
import Kind.Newtypes (dataInfoRange, newtypesLookupAny)
import Kind.Synonym (synInfoRange, synonymsLookup)
import Type.Assumption (gammaLookupCanonical, gammaLookupQ, infoRange)
import Syntax.RangeMap (RangeInfo (..), rangeMapFindAt, NameInfo (..))
import Compiler.Module (Loaded (..), loadedModule, modRangeMap, modName, modSourcePath)
import LanguageServer.Conversions (fromLspPos, toLspLocation, toLspLocationLink)
import LanguageServer.Monad (LSM, getLoaded)
import Lib.Trace(trace)

-- Finds the definitions of the element under the cursor.
definitionHandler :: Handlers LSM
definitionHandler = requestHandler J.SMethod_TextDocumentDefinition $ \req responder -> do
  let J.DefinitionParams doc pos _ _ = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded normUri
  pos <- liftIO $ fromLspPos normUri pos
  let defs = do -- maybe monad
        l <- maybeToList loaded
        rmap <- maybeToList $ modRangeMap $ loadedModule l
        let rm = rangeMapFindAt pos rmap
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
    Id _ (NICon{}) _ True -> 3
    Id _ _ _ True -> 2
    Id _ _ _ _ -> 0
    Decl "con" _ _ -> 3 -- Constructors are more important than other decls (such as automatically generated ones)
    Decl _ _ _ -> 1
    Warning _ -> 4
    Error _ -> 5

-- Finds the definition locations of the element
-- represented by the given range info.
findDefinitions :: Loaded -> RangeInfo -> [J.DefinitionLink]
findDefinitions loaded rinfo
  = case rinfo of
      Id qname idInfo docs _
        -> -- trace ("find definition of id: " ++ show qname) $
           let ranges = case idInfo of
                          NIValue{}   -> map infoRange $ gammaLookupCanonical qname gamma
                          NICon{}     -> map conInfoRange $ maybeToList $ constructorsLookup qname constrs
                          NITypeCon _ -> (map synInfoRange $ maybeToList $ synonymsLookup qname synonyms)
                                        ++
                                        (map dataInfoRange $ maybeToList $ newtypesLookupAny qname newtypes)
                          NITypeVar _ -> []
                          NIModule    -> -- trace ("module definition: " ++ show qname) $
                                         [R.makeSourceRange (modSourcePath mod) 1 1 1 1 | mod <- loadedModules loaded, modName mod == qname]
                          NIKind      -> []
          in map (J.DefinitionLink . toLspLocationLink rinfo) ranges
      _ -> []
    where
      gamma = loadedGamma loaded
      constrs = loadedConstructors loaded
      synonyms = loadedSynonyms loaded
      newtypes = loadedNewtypes loaded

-----------------------------------------------------------------------------
-- The LSP handler that provides ctrl-click definitions
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.Definition (definitionHandler) where

import Compiler.Module (Loaded (..), loadedModule, modRangeMap)
import Control.Lens ((^.))
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Kind.Constructors (conInfoRange, constructorsLookup)
import Kind.Newtypes (dataInfoRange, newtypesLookupAny)
import Kind.Synonym (synInfoRange, synonymsLookup)
import Language.LSP.Server (Handlers, requestHandler)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import LanguageServer.Conversions (fromLspPos, toLspLocation, toLspLocationLink)
import LanguageServer.Monad (LSM, getLoaded)
import Syntax.RangeMap (RangeInfo (..), rangeMapFindAt)
import Type.Assumption (gammaLookupQ, infoRange)
import qualified Language.LSP.Protocol.Message as J

definitionHandler :: Handlers LSM
definitionHandler = requestHandler J.SMethod_TextDocumentDefinition $ \req responder -> do
  let J.DefinitionParams doc pos _ _ = req ^. J.params
      uri = doc ^. J.uri
  loaded <- getLoaded uri
  let defs = do
        l <- maybeToList loaded
        rmap <- maybeToList $ modRangeMap $ loadedModule l
        (_, rinfo) <- maybeToList $ rangeMapFindAt (fromLspPos uri pos) rmap
        findDefinitions l rinfo
  responder $ Right $ J.InR $ J.InL defs

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

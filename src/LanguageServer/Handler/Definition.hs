-----------------------------------------------------------------------------
-- The LSP handler that provides ctrl-click definitions
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.Definition (definitionHandler) where

import Compiler.Module (Loaded (..), loadedModule, modRangeMap)
import Compiler.Options (Flags)
import Control.Lens ((^.))
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Kind.Constructors (conInfoRange, constructorsLookup)
import Kind.Newtypes (dataInfoRange, newtypesLookupAny)
import Kind.Synonym (synInfoRange, synonymsLookup)
import Language.LSP.Server (Handlers, requestHandler)
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import LanguageServer.Conversions (fromLspPos, toLspLocation)
import LanguageServer.Monad (LSM, getLoaded)
import Syntax.RangeMap (RangeInfo (..), rangeMapFindAt)
import Type.Assumption (gammaLookupQ, infoRange)

definitionHandler :: Flags -> Handlers LSM
definitionHandler flags = requestHandler J.STextDocumentDefinition $ \req responder -> do
  let J.DefinitionParams doc pos _ _ = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded
  let defs = do
        l <- maybeToList $ M.lookup normUri loaded
        rmap <- maybeToList $ modRangeMap $ loadedModule l
        (_, rinfo) <- maybeToList $ rangeMapFindAt (fromLspPos uri pos) rmap
        findDefinitions l rinfo
  responder $ Right $ J.InR $ J.InL $ J.List defs

-- Finds the definition locations of the element
-- represented by the given range info.
findDefinitions :: Loaded -> RangeInfo -> [J.Location]
findDefinitions loaded rinfo = case rinfo of
  Id qname _ _ ->
    let rngs =
          map infoRange (gammaLookupQ qname gamma)
            ++ map conInfoRange (maybeToList $ constructorsLookup qname constrs)
            ++ map synInfoRange (maybeToList $ synonymsLookup qname synonyms)
            ++ map dataInfoRange (maybeToList $ newtypesLookupAny qname newtypes)
     in map toLspLocation rngs
  _ -> []
  where
    gamma = loadedGamma loaded
    constrs = loadedConstructors loaded
    synonyms = loadedSynonyms loaded
    newtypes = loadedNewtypes loaded

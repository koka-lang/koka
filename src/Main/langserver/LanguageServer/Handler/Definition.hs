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

import Debug.Trace(trace)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import qualified Data.Map as M
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.Server (Handlers, requestHandler)

import Common.Name
import Common.Range as R
import Kind.Constructors (conInfoRange, constructorsLookup)
import Kind.Newtypes (dataInfoRange, newtypesLookupAny)
import Kind.Synonym (synInfoRange, synonymsLookup)
import Type.Assumption (gammaLookupCanonical, gammaLookupQ, infoRange)
import Syntax.RangeMap (RangeInfo (..), rangeMapFindAt, NameInfo (..))

import LanguageServer.Conversions
import LanguageServer.Monad

-- Finds the definitions of the element under the cursor.
definitionHandler :: Handlers LSM
definitionHandler
  = requestHandler J.SMethod_TextDocumentDefinition $ \req responder ->
    do  let J.DefinitionParams doc pos0 _ _ = req ^. J.params
            uri  = J.toNormalizedUri (doc ^. J.uri)

            done :: LSM ()
            done = responder $ Right $ J.InR $ J.InR J.Null

            liftMaybe :: LSM (Maybe a) -> (a -> LSM ()) -> LSM ()
            liftMaybe action next = do res <- action
                                       case res of
                                         Nothing -> done
                                         Just x  -> next x

        pos <- liftIO $ fromLspPos uri pos0
        liftMaybe (lookupModuleName uri) $ \(fpath,modname) ->
          liftMaybe (lookupRangeMap modname) $ \(rmap,lexemes) ->
            liftMaybe (return (rangeMapFindAt lexemes pos rmap)) $ \(rng,rngInfo) ->
              do defs <- lookupVisibleDefinitions [modname]
                 mods <- lookupModulePaths
                 let defLinks = findDefLinks defs mods rngInfo
                 responder $ Right $ J.InR $ J.InL defLinks

findDefLinks :: Definitions -> [(ModuleName,FilePath)] -> RangeInfo -> [J.DefinitionLink]
findDefLinks defs mods rngInfo
  = case rngInfo of
      Id qname idInfo docs _
        -> -- trace ("find definition of id: " ++ show qname) $
           let ranges = case idInfo of
                          NIValue{}   -> map infoRange $ gammaLookupQ qname (defsGamma defs)
                          NICon{}     -> -- trace ("lookup con: " ++ show qname) $
                                         map infoRange $ gammaLookupQ qname (defsGamma defs)
                          NITypeCon{} -> (map synInfoRange $ maybeToList $ synonymsLookup qname (defsSynonyms defs))
                                        ++
                                         (map dataInfoRange $ maybeToList $ newtypesLookupAny qname (defsNewtypes defs))
                          NITypeVar _ -> []
                          NIModule    -> [R.makeSourceRange fpath 1 1 1 1 | (modname,fpath) <- mods, modname == qname]
                          NIKind      -> []
          in map (J.DefinitionLink . toLspLocationLink rngInfo) ranges
      _ -> []


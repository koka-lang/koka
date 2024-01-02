------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The LSP handler that provides hover tooltips
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.InlayHints (inlayHintsHandler) where

import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.))
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Protocol.Lens as J
import Common.Name (Name)
import Common.Range (Range (..), rangeEnd, Pos(..), rangeNull, posNull, extendRange)
import Type.Pretty (ppType, Env (..), defaultEnv, ppScheme)
import Kind.ImportMap (ImportMap)
import Compiler.Compile (Module(..), Loaded (..))
import Compiler.Options (prettyEnvFromFlags, Flags)
import Syntax.RangeMap (NameInfo (..), RangeInfo (..), rangeMapFindIn)
import Language.LSP.Server (Handlers, sendNotification, requestHandler)
import LanguageServer.Monad (LSM, getLoaded, getLoadedModule, getFlags, getInlayHintOptions, InlayHintOptions (..))
import LanguageServer.Conversions (fromLspPos, toLspRange, toLspPos, fromLspRange)
import LanguageServer.Handler.Hover (formatRangeInfoHover)
import Lib.PPrint (hcat, sep, text, punctuate)
-- import Debug.Trace (trace)

-- The LSP handler that provides inlay hints (inline type annotations etc)
inlayHintsHandler :: Handlers LSM
inlayHintsHandler = requestHandler J.SMethod_TextDocumentInlayHint $ \req responder -> do
  let J.InlayHintParams prog doc rng = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  options <- getInlayHintOptions
  newRng <- liftIO $ fromLspRange normUri rng
  loadedMod <- getLoadedModule normUri
  loaded <- getLoaded normUri
  flags <- getFlags
  let rsp = do -- maybe monad
        l <- loaded
        lm <- loadedMod
        rmap <- modRangeMap lm
        -- trace (show $ rangeMapFindIn newRng rmap) $ return ()
        let env = (prettyEnvFromFlags flags){ context = modName lm, importsMap = loadedImportMap l, showFlavours=False }
        let hints = concatMap (toInlayHint options env (modName lm)) $ rangeMapFindIn newRng rmap
        let hintsDistinct = Map.fromList $ map (\hint -> (hint ^. J.position, hint)) hints
        return $ Map.elems hintsDistinct
  case rsp of
    Nothing -> responder $ Right $ J.InR J.Null
    Just rsp -> responder $ Right $ J.InL rsp

-- Takes a range and range info and returns an inlay hint if it should be shown
toInlayHint :: InlayHintOptions -> Env -> Name -> (Range, RangeInfo) -> [J.InlayHint]
toInlayHint opts env modName (rng, rngInfo) = do
  let rngEnd = rangeEnd rng
      -- should show identifier hint if it's not annotated already
      shouldShow =
        (rngEnd /= posNull) &&
        case rngInfo of
          Id _ info docs _ -> case info of
            NIValue _ _ isAnnotated -> not isAnnotated || not (null docs)
            _ -> not (null docs)
          Implicits _ -> showImplicitArguments opts
          _ -> False
  if shouldShow then
    let info = formatInfo opts env modName rng rngInfo in
    mapMaybe (\(rng, str, kind) ->
        let rngEnd = rangeEnd rng in
        let position = toLspPos rngEnd{posColumn = posColumn rngEnd + 1} in
        let text = T.pack str in
        if T.null text then Nothing else
          Just $ J.InlayHint position (J.InL text) (Just kind) (Just [J.TextEdit (J.Range position position) text]) Nothing (Just True) (Just True) Nothing
    ) info
  else []

-- Pretty-prints type information for an inlay hint
formatInfo :: InlayHintOptions -> Env -> Name -> Range -> RangeInfo -> [(Range, String, J.InlayHintKind)]
formatInfo opts env modName rng rinfo = case rinfo of
  Id qname info docs isdef ->
    let implicitArguments = [(inlayRange, show (hcat $ punctuate (text ", ") docs), J.InlayHintKind_Parameter) | showImplicitArguments opts] in
    case info of
      NIValue tp _ isAnnotated ->
        let typeAnnotation = [(rng, " : " ++ show (ppScheme env tp), J.InlayHintKind_Type) | not isAnnotated && showInferredTypes opts]
            result = typeAnnotation ++ implicitArguments
        in if null result then [] else result
      _ -> if null implicitArguments then [] else implicitArguments
  Implicits implicits -> [(inlayRange, show implicits, J.InlayHintKind_Parameter) | showImplicitArguments opts]
  _ -> []
 where
  inlayRange = extendRange rng 1

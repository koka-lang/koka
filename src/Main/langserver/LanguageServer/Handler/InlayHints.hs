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
import Common.Name (Name, qualifier, showPlain, missingQualifier, splitLocalQualName)
import Common.Range (Range (..), rangeEnd, Pos(..), rangeNull, posNull, extendRange, minPos, makeRange, rangeStart)
import Type.Pretty (ppType, Env (..), defaultEnv, ppScheme)
import Kind.ImportMap (ImportMap)
import Compiler.Compile (Module(..), Loaded (..))
import Compiler.Module (modLexemes)
import Compiler.Options (prettyEnvFromFlags, Flags)
import Syntax.RangeMap (NameInfo (..), RangeInfo (..), rangeMapFindIn, lexemesFromPos)
import Syntax.Lexeme (Lexeme (..), Lex (..))
import Language.LSP.Server (Handlers, sendNotification, requestHandler)
import LanguageServer.Monad (LSM, getLoaded, getLoadedModule, getFlags, getInlayHintOptions, InlayHintOptions (..))
import LanguageServer.Conversions (fromLspPos, toLspRange, toLspPos, fromLspRange)
import LanguageServer.Handler.Hover (formatRangeInfoHover)
import Lib.PPrint (hcat, sep, text, punctuate, tupled, comma, (<.>), (<+>), isEmptyDoc)
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
        let env = (prettyEnvFromFlags flags){ context = modName lm, importsMap = loadedImportMap l, showFlavours=False, fullNames=showFullQualifiers options }
        let hints = concatMap (toInlayHint options env lm) $ rangeMapFindIn newRng rmap
        let hintsDistinct = Map.fromList $ map (\hint -> (hint ^. J.position, hint)) hints
        return $ Map.elems hintsDistinct
  case rsp of
    Nothing -> responder $ Right $ J.InR J.Null
    Just rsp -> responder $ Right $ J.InL rsp

-- Takes a range and range info and returns an inlay hint if it should be shown
toInlayHint :: InlayHintOptions -> Env -> Module -> (Range, RangeInfo) -> [J.InlayHint]
toInlayHint opts env mod (rng, rngInfo) = do
  let rngEnd = rangeEnd rng
      -- should show identifier hint if it's not annotated already
      shouldShow =
        (rngEnd /= posNull) &&
        case rngInfo of
          Id _ info docs _ -> case info of
            NIValue _ _ isAnnotated -> not isAnnotated || not (null docs) || showFullQualifiers opts
            _ -> not (null docs)
          Implicits _ -> showImplicitArguments opts
          _ -> False
  if shouldShow then
    let info = formatInfo opts env mod rng rngInfo in
    mapMaybe (\(rng, str, kind, padding) ->
        let rngEnd = rangeEnd rng in
        let position = toLspPos rngEnd{posColumn = posColumn rngEnd + 1} in
        let text = T.pack str in
        if T.null text then Nothing else
          Just $ J.InlayHint position (J.InL text) (Just kind) (Just [J.TextEdit (J.Range position position) text]) Nothing (Just padding) (Just padding) Nothing
    ) info
  else []

-- Pretty-prints type information for an inlay hint
formatInfo :: InlayHintOptions -> Env -> Module -> Range -> RangeInfo -> [(Range, String, J.InlayHintKind, Bool)]
formatInfo opts env mod rng rinfo = case rinfo of
  Id qname info docs False -> 
    case info of
      NIValue tp _ isAnnotated -> 
        let typeAnnotation = [(rng, " : " ++ show (ppScheme env tp), J.InlayHintKind_Type, True) | not isAnnotated && showInferredTypes opts]
            qualifiers = [(qualRng, getqualifier qname, J.InlayHintKind_Type, False) | showFullQualifiers opts && getqualifier qname /= ""]
            result = qualifiers ++ typeAnnotation ++ implicits docs
        in if null result then [] else result
      _ -> if null docs then [] else implicits docs
  Implicits imp -> implicit imp
  _ -> []
 where
  posPrev r = 
    let Pos src off l c = rangeStart r
        in Pos src off l (c - 1)
  lexes = lexemesFromPos (rangeEnd rng) (modLexemes mod)
  qualRng = 
    let rngS = posPrev rng
    in makeRange rngS rngS
  getqualifier qname = case lexes of
    (Lexeme rng (LexId name)):rst -> missingQualifier name qname
    _ -> ""
  isDotFunction = case lexes of
    _:(Lexeme rng (LexKeyword "." _)):_ -> True
    _:(Lexeme rng (LexSpecial ";")):_ -> True
    _:(Lexeme rng LexInsSemi):_ -> True
    _:(Lexeme rng LexInsLCurly):_ -> True
    _ -> False
  isEmptyFunction = case lexes of
    _:(Lexeme rng (LexSpecial "(")):(Lexeme rng1 (LexSpecial ")")):_ -> True
    _ -> False
  inlayRange = if isDotFunction then rng else finalParameterRange lexes 0
  finalParameterRange lexes match = case lexes of 
    Lexeme r (LexSpecial "("):rst -> finalParameterRange rst (match + 1)
    Lexeme r (LexSpecial ")"):rst -> 
      let newPos = posPrev r in
      if match == 1 then makeRange newPos newPos else finalParameterRange rst (match - 1)
    x:rst -> finalParameterRange rst match
    [] -> rng
  implicit imp = 
    [(inlayRange, if isDotFunction then show (tupled [imp]) else if isEmptyFunction then show imp else show (comma <+> imp), J.InlayHintKind_Parameter, False) | showImplicitArguments opts]
  implicits docs = 
    let doc = (hcat $ punctuate (text ", ") docs)
    in if isEmptyDoc doc then [] else [(inlayRange, show (if isDotFunction then tupled docs else if isEmptyFunction then doc else comma <+> doc), J.InlayHintKind_Parameter, False) | showImplicitArguments opts]
  
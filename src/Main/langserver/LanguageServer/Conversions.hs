------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting, Fredrik Wieczerkowski
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Conversions between LSP types and internal types, e.g. positions/ranges
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Conversions
  ( -- * Conversions to LSP types
    toLspPos,
    toLspRange,
    toLspLocation,
    toLspLocationLink,
    toLspDiagnostics,
    toLspErrorDiagnostics,
    toLspWarningDiagnostic,
    -- toLspUri,
    makeDiagnostic,
    filePathToUri,
    errorMessageToDiagnostic,

    -- * Conversions from LSP types
    fromLspPos,
    fromLspRange,
    fromLspLocation,
    fromLspUri
  )
where

import Debug.Trace(trace)
import Colog.Core
import Data.Map.Strict as M hiding (map)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as J

import Lib.PPrint (Doc)
import qualified Common.Error as E
import Common.File (normalize, realPath, startsWith)
import Common.Range (Source (sourceName), sourceNull, rangeSource)
import qualified Common.Range as R
import Syntax.RangeMap (RangeInfo)

toLspPos :: R.Pos -> J.Position
toLspPos p =
  J.Position (fromIntegral (max 0 (R.posLine p - 1))) (fromIntegral (max 0 (R.posColumn p - 1))) -- LSP positions are zero-based

toLspRange :: R.Range -> J.Range
toLspRange r =
  J.Range (J.Position l1 c1) (J.Position l2 (c2 + 1)) -- LSP range ends are exclusive
  where
    J.Position l1 c1 = toLspPos $ R.rangeStart r
    J.Position l2 c2 = toLspPos $ R.rangeEnd r

toLspLocation :: R.Range -> J.Location
toLspLocation r =
  J.Location uri (toLspRange r)
  where
    uri = J.filePathToUri $ R.sourceName $ R.rangeSource r

toLspLocationLink :: RangeInfo -> R.Range -> J.LocationLink
toLspLocationLink src r =
  J.LocationLink Nothing uri (toLspRange r) (toLspRange r)
  where
    uri = J.filePathToUri $ R.sourceName $ R.rangeSource r

toLspDiagnostics :: J.NormalizedUri -> Maybe T.Text -> E.Error b a -> M.Map J.NormalizedUri [J.Diagnostic]
toLspDiagnostics uri src err =
  case E.checkError err of
    Right (_, E.Errors ws) -> M.unions (map (toLspErrorDiagnostics uri src) ws)
        -- M.fromList $ map (\(r, doc) -> (uriFromRange r uri, [toLspWarningDiagnostic src r doc])) ws
    Left (E.Errors errs) -> M.unions (map (toLspErrorDiagnostics uri src) errs)


errorMessageToDiagnostic :: Maybe T.Text -> J.NormalizedUri -> E.ErrorMessage -> (J.NormalizedUri, [J.Diagnostic])
errorMessageToDiagnostic errSource defaultUri e
  = (uriFromRange (E.errRange e) defaultUri,
     [makeDiagnostic (toSeverity (E.errSeverity e)) errSource (E.errRange e) (E.errMessage e)] )
  where
    toSeverity sev
          = case sev of
              E.SevError   -> J.DiagnosticSeverity_Error
              E.SevWarning -> J.DiagnosticSeverity_Warning
              _            -> J.DiagnosticSeverity_Information

toLspErrorDiagnostics :: J.NormalizedUri -> Maybe T.Text -> E.ErrorMessage -> M.Map J.NormalizedUri [J.Diagnostic]
toLspErrorDiagnostics uri src e =
  M.singleton (uriFromRange (E.errRange e) uri)
    [makeDiagnostic (toSeverity (E.errSeverity e)) src (E.errRange e) (E.errMessage e)]
  where
    toSeverity sev
      = case sev of
          E.SevError   -> J.DiagnosticSeverity_Error
          E.SevWarning -> J.DiagnosticSeverity_Warning
          _            -> J.DiagnosticSeverity_Information

uriFromRange :: R.Range -> J.NormalizedUri -> J.NormalizedUri
uriFromRange r uri =
  if R.rangeSource r == sourceNull then uri else J.toNormalizedUri $ J.filePathToUri $ sourceName (R.rangeSource r)

toLspWarningDiagnostic :: Maybe T.Text -> R.Range -> Doc -> J.Diagnostic
toLspWarningDiagnostic diagsrc range doc
  = makeDiagnostic J.DiagnosticSeverity_Warning diagsrc range doc

makeDiagnostic :: J.DiagnosticSeverity -> Maybe T.Text -> R.Range -> Doc -> J.Diagnostic
makeDiagnostic s diagsrc r doc =
  J.Diagnostic range severity code codeDescription diagsrc message tags related dataX
  where
    range = toLspRange r
    severity = Just s
    code = Nothing
    codeDescription = Nothing
    message = T.pack $ show doc
    tags
      | "is unused" `T.isInfixOf` message = Just [J.DiagnosticTag_Unnecessary]
      | otherwise = Nothing
    related = Nothing
    dataX = Nothing

fromLspPos :: J.NormalizedUri -> J.Position -> IO R.Pos
fromLspPos uri (J.Position l c) = do
  filePath <- fromLspUri uri
  return $ R.makePos (src filePath) (-1) (fromIntegral l + 1) (fromIntegral c + 1)
  where
    src file = case file of
      Just filePath -> R.Source filePath R.bstringEmpty -- TODO: Read file here (and compute the offset correctly)
      Nothing -> R.sourceNull

fromLspRange :: J.NormalizedUri -> J.Range -> IO R.Range
fromLspRange uri (J.Range s e) = do
  start <- fromLspPos uri s
  end <- fromLspPos uri e
  return $ R.makeRange start end

fromLspLocation :: J.Location -> IO R.Range
fromLspLocation (J.Location uri rng) = fromLspRange (J.toNormalizedUri uri) rng

{-
toLspUri :: FilePath -> J.NormalizedUri
toLspUri = J.toNormalizedUri . J.filePathToUri . normalize
-}

fromLspUri :: J.NormalizedUri -> IO (Maybe FilePath)
fromLspUri uri =
  let uriText = T.unpack (J.getUri (J.fromNormalizedUri uri))
      -- work around lsp bugs :-(  (we should report this but it seems there are multiple wrong assumptions in both normalization and in uriToFilePath)
      -- on Windows, a file path like "\\wsl.localhost\Ubuntu\home\koka\samples\basic\fibonacci.kk"
      -- becomes a J.Uri as           "file://wsl.localhost/Ubuntu/home/koka/samples/basic/fibonacci.kk" (note the lost \\ start)
      -- and a J.NormalizedUri  as    "file:///wsl.localhostUbuntu/home/koka/samples/basic/fibonacci.kk" (note the lost / before Ubuntu, and added / start)
      --
      -- here we try to correct that for this specific case so we can at least open
      -- files in a WSL file system on Windows
      -- (which is important as build on wsl2 is very slow outside the wsl mounted file system)
      mbfpath = if uriText `startsWith` "file:///wsl.localhost"
                then Just ("//wsl.localhost/" ++ Prelude.drop (length ("file:///wsl.localhost" :: String)) uriText)
                else J.uriToFilePath (J.fromNormalizedUri uri)
  in case mbfpath of
      Just fpath  -> do p <- realPath fpath
                        -- trace ("LanguageServer.Conversions.fromLspUri: uri: " ++ uriText ++ ", realpath: " ++ p)
                        return $ Just p
      Nothing     -> return Nothing

filePathToUri :: FilePath -> J.Uri
filePathToUri fpath
  = if fpath `startsWith` "\\\\wsl.localhost\\"          -- again work around bugs
      then J.Uri (T.pack ("file://" ++ normalize (Prelude.drop 2 fpath)))
      else J.filePathToUri fpath

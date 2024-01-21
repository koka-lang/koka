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
    toLspUri,
    makeDiagnostic,

    -- * Conversions from LSP types
    fromLspPos,
    fromLspRange,
    fromLspLocation,
    fromLspUri,
  )
where

import Colog.Core
import qualified Common.Error as E
import Common.File (normalize, realPath)
import Common.Range (Source (sourceName), sourceNull)
import qualified Common.Range as R
import Compiler.Module (Loaded (..), Module (..))
import Data.Map.Strict as M hiding (map)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
--import GHC.Generics hiding (UInt)
--import Language.LSP.Protocol.Types (UInt)
import qualified Language.LSP.Protocol.Types as J
import Lib.PPrint (Doc)
import qualified Syntax.RangeMap as R

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

toLspLocationLink :: R.RangeInfo -> R.Range -> J.LocationLink
toLspLocationLink src r =
  J.LocationLink Nothing uri (toLspRange r) (toLspRange r)
  where
    uri = J.filePathToUri $ R.sourceName $ R.rangeSource r

toLspDiagnostics :: J.NormalizedUri -> T.Text -> E.Error b a -> M.Map J.NormalizedUri [J.Diagnostic]
toLspDiagnostics uri src err =
  case E.checkError err of
    Right (_, ws) -> M.fromList $ map (\(r, doc) -> (uriFromRange r uri, [toLspWarningDiagnostic src r doc])) ws
    Left e -> toLspErrorDiagnostics uri src e

toLspErrorDiagnostics :: J.NormalizedUri -> T.Text -> E.ErrorMessage -> M.Map J.NormalizedUri [J.Diagnostic]
toLspErrorDiagnostics uri src e =
  case e of
    E.ErrorGeneral r doc -> M.singleton (uriFromRange r uri) [makeDiagnostic J.DiagnosticSeverity_Error src r doc]
    E.ErrorParse r doc -> M.singleton (uriFromRange r uri) [makeDiagnostic J.DiagnosticSeverity_Error src r doc]
    E.ErrorStatic rds -> mapRangeDocs rds
    E.ErrorKind rds -> mapRangeDocs rds
    E.ErrorType rds -> mapRangeDocs rds
    E.ErrorWarning rds e' -> M.unionWith (++) (mapRangeDocs rds) (toLspErrorDiagnostics uri src e')
    E.ErrorIO doc -> M.singleton uri [makeDiagnostic J.DiagnosticSeverity_Error src R.rangeNull doc]
    E.ErrorZero -> M.empty
  where
    mapRangeDocs rds = M.fromList $ map (\(r, doc) -> (uriFromRange r uri, [makeDiagnostic J.DiagnosticSeverity_Error src r doc])) rds

uriFromRange :: R.Range -> J.NormalizedUri -> J.NormalizedUri
uriFromRange r uri =
  if R.rangeSource r == sourceNull then uri else J.toNormalizedUri $ J.filePathToUri $ sourceName (R.rangeSource r)

toLspWarningDiagnostic :: T.Text -> R.Range -> Doc -> J.Diagnostic
toLspWarningDiagnostic =
  makeDiagnostic J.DiagnosticSeverity_Warning

makeDiagnostic :: J.DiagnosticSeverity -> T.Text -> R.Range -> Doc -> J.Diagnostic
makeDiagnostic s src r doc =
  J.Diagnostic range severity code codeDescription source message tags related dataX
  where
    range = toLspRange r
    source = Just src
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

toLspUri :: FilePath -> J.NormalizedUri
toLspUri = J.toNormalizedUri . J.filePathToUri . normalize

fromLspUri :: J.NormalizedUri -> IO (Maybe FilePath)
fromLspUri uri = do
  let uri' = (J.uriToFilePath . J.fromNormalizedUri) uri
  case uri' of
    Just fpath -> do
      p <- realPath fpath
      return $ Just p
    Nothing -> return Nothing
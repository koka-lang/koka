-----------------------------------------------------------------------------
-- Conversions between LSP types and internal types, e.g. positions/ranges
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Conversions
  ( -- * Conversions to LSP types
    toLspPos,
    toLspRange,
    toLspLocation,
    toLspDiagnostics,
    toLspErrorDiagnostics,
    toLspWarningDiagnostic,

    -- * Conversions from LSP types
    fromLspPos,
    fromLspRange,
    fromLspLocation,
  )
where
import           GHC.Generics              hiding (UInt)
import qualified Common.Error as E
import qualified Common.Range as R
import qualified Data.Text as T
import qualified Language.LSP.Types as J
import Language.LSP.Types (UInt)
import Lib.PPrint (Doc)

toLspPos :: R.Pos -> J.Position
toLspPos p =
  J.Position (fromIntegral (max 0 (R.posLine p - 1))) (fromIntegral (max 0 (R.posColumn p - 1)))-- LSP positions are zero-based

toLspRange :: R.Range -> J.Range
toLspRange r =
  J.Range (J.Position l1 c1) (J.Position l2 $ c2 + 1) -- LSP range ends are exclusive
  where
    J.Position l1 c1 = toLspPos $ R.rangeStart r
    J.Position l2 c2 = toLspPos $ R.rangeEnd r

toLspLocation :: R.Range -> J.Location
toLspLocation r =
  J.Location uri (toLspRange r)
  where
    uri = J.filePathToUri $ R.sourceName $ R.rangeSource r

toLspDiagnostics :: J.DiagnosticSource -> E.Error a -> [J.Diagnostic]
toLspDiagnostics src err =
  case E.checkError err of
    Right (_, ws) -> map (uncurry $ toLspWarningDiagnostic src) ws
    Left e -> toLspErrorDiagnostics src e

toLspErrorDiagnostics :: J.DiagnosticSource -> E.ErrorMessage -> [J.Diagnostic]
toLspErrorDiagnostics src e =
  case e of
    E.ErrorGeneral r doc -> [makeDiagnostic J.DsError src r doc]
    E.ErrorParse r doc -> [makeDiagnostic J.DsError src r doc]
    E.ErrorStatic rds -> map (uncurry $ makeDiagnostic J.DsError src) rds
    E.ErrorKind rds -> map (uncurry $ makeDiagnostic J.DsError src) rds
    E.ErrorType rds -> map (uncurry $ makeDiagnostic J.DsError src) rds
    E.ErrorWarning rds e' -> map (uncurry $ makeDiagnostic J.DsError src) rds ++ toLspErrorDiagnostics src e'
    E.ErrorIO doc -> [makeDiagnostic J.DsError src R.rangeNull doc]
    E.ErrorZero -> []

toLspWarningDiagnostic :: J.DiagnosticSource -> R.Range -> Doc -> J.Diagnostic
toLspWarningDiagnostic =
  makeDiagnostic J.DsWarning

makeDiagnostic :: J.DiagnosticSeverity -> J.DiagnosticSource -> R.Range -> Doc -> J.Diagnostic
makeDiagnostic s src r doc =
  J.Diagnostic range severity code source message tags related
  where
    range = toLspRange r
    severity = Just s
    code = Nothing
    source = Just src
    message = T.pack $ show doc
    tags
      | "is unused" `T.isInfixOf` message = Just $ J.List [J.DtUnnecessary]
      | otherwise = Nothing
    related = Nothing

fromLspPos :: J.Uri -> J.Position -> R.Pos
fromLspPos uri (J.Position l c) =
  R.makePos src (-1) (fromIntegral l + 1) (fromIntegral c + 0)
  where
    src = case J.uriToFilePath uri of
      Just filePath -> R.Source filePath R.bstringEmpty -- TODO: Read file here (and compute the offset correctly)
      Nothing -> R.sourceNull

fromLspRange :: J.Uri -> J.Range -> R.Range
fromLspRange uri (J.Range s e) = R.makeRange (fromLspPos uri s) (fromLspPos uri e)

fromLspLocation :: J.Location -> R.Range
fromLspLocation (J.Location uri rng) = fromLspRange uri rng

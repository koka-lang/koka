-----------------------------------------------------------------------------
-- Conversions between LSP types and internal types, e.g. positions/ranges
-----------------------------------------------------------------------------
module LanguageServer.Conversions( toLspPos
                                 , toLspRange
                                 , toLspDiagnostics
                                 , toLspErrorDiagnostics
                                 , toLspWarningDiagnostic
                                 , fromLspPos ) where

import qualified Common.Error            as E
import qualified Common.Range            as R
import qualified Data.Text               as T
import qualified Language.LSP.Types      as J
import Lib.PPrint                        ( Doc )

toLspPos :: R.Pos -> J.Position
toLspPos p = J.Position (R.posLine p - 1) (R.posColumn p - 1) -- LSP positions are zero-based

toLspRange :: R.Range -> J.Range
toLspRange r = J.Range (J.Position l1 c1) (J.Position l2 $ c2 + 1) -- LSP range ends are exclusive
  where
    J.Position l1 c1 = toLspPos $ R.rangeStart r
    J.Position l2 c2 = toLspPos $ R.rangeEnd r

toLspDiagnostics :: E.Error a -> [J.Diagnostic]
toLspDiagnostics err = case E.checkError err of
  Right (_, ws) -> map (uncurry toLspWarningDiagnostic) ws
  Left  e ->       toLspErrorDiagnostics e

toLspErrorDiagnostics :: E.ErrorMessage -> [J.Diagnostic]
toLspErrorDiagnostics e = case e of
  E.ErrorGeneral r doc  -> [makeDiagnostic J.DsError r doc]
  E.ErrorParse   r doc  -> [makeDiagnostic J.DsError r doc]
  E.ErrorStatic  rds    -> map (uncurry $ makeDiagnostic J.DsError) rds
  E.ErrorKind    rds    -> map (uncurry $ makeDiagnostic J.DsError) rds
  E.ErrorType    rds    -> map (uncurry $ makeDiagnostic J.DsError) rds
  E.ErrorWarning rds e' -> map (uncurry $ makeDiagnostic J.DsError) rds ++ toLspErrorDiagnostics e'
  E.ErrorIO      doc    -> [makeDiagnostic J.DsError R.rangeNull doc]
  E.ErrorZero           -> []

toLspWarningDiagnostic :: R.Range -> Doc -> J.Diagnostic
toLspWarningDiagnostic = makeDiagnostic J.DsWarning

makeDiagnostic :: J.DiagnosticSeverity -> R.Range -> Doc -> J.Diagnostic
makeDiagnostic s r doc = J.Diagnostic (toLspRange r) (Just s) Nothing Nothing (T.pack $ show doc) Nothing Nothing

fromLspPos :: J.Uri -> J.Position -> R.Pos
fromLspPos uri (J.Position l c) = R.makePos src (-1) (l + 1) (c + 1)
  where
    src = case J.uriToFilePath uri of
      Just filePath -> R.Source filePath R.bstringEmpty -- TODO: Read file here?
      Nothing       -> R.sourceNull

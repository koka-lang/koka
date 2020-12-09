-----------------------------------------------------------------------------
-- Conversions between LSP types and internal types, e.g. positions/ranges
-----------------------------------------------------------------------------
module LanguageServer.Conversions( toLspPos
                                 , toLspRange
                                 , fromLspPos ) where

import qualified Common.Range            as R
import qualified Language.LSP.Types      as J

toLspPos :: R.Pos -> J.Position
toLspPos p = J.Position (R.posLine p - 1) (R.posColumn p - 1) -- LSP positions are zero-based

toLspRange :: R.Range -> J.Range
toLspRange r = J.Range (J.Position l1 c1) (J.Position l2 $ c2 + 1) -- LSP range ends are exclusive
  where
    J.Position l1 c1 = toLspPos $ R.rangeStart r
    J.Position l2 c2 = toLspPos $ R.rangeEnd r

fromLspPos :: J.Uri -> J.Position -> R.Pos
fromLspPos uri (J.Position l c) = R.makePos src (-1) (l + 1) (c + 1)
  where
    src = case J.uriToFilePath uri of
      Just filePath -> R.Source filePath R.bstringEmpty -- TODO: Read file here?
      Nothing       -> R.sourceNull

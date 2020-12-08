{-# LANGUAGE OverloadedStrings #-}
module LanguageServer.Handlers( handlers
                              ) where

import Control.Lens                      ( (^.) )
import Control.Monad.IO.Class            ( liftIO )
import Common.Error
import qualified Common.Range            as R
import Compiler.Compile                  ( compileModuleOrFile, Terminal (..) )
import Compiler.Options                  ( Flags )
import Compiler.Module                   ( loadedModule, modRangeMap )
import Data.Either                       ( fromRight )
import Data.Maybe                        ( listToMaybe, fromJust )
import qualified Data.Text               as T
import Language.LSP.Server
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import Syntax.RangeMap                   ( rangeMapFindAt )

handlers :: Flags -> Handlers (LspM ())
handlers flags = mconcat
  [ requestHandler J.STextDocumentHover $ \req responder -> do
      let J.HoverParams doc pos _ = req ^. J.params
          -- TODO: Use the VFS and recompile when the user types
          --       (instead of on every hover)
          uri = doc ^. J.uri
          -- TODO: Handle error
          filePath = fromJust $ J.uriToFilePath uri
      -- TODO: Generate diagnostics from compilation results,
      --       ideally in conjunction with the VFS (see above)
      loaded <- liftIO $ compileModuleOrFile terminal flags [] filePath False
      let rsp = do
                  -- TODO: Handle errors
                  let l = fst $ fromRight' $ checkError loaded
                  rmap <- modRangeMap $ loadedModule l
                  (r, rinfo) <- rangeMapFindAt (fromLspPos uri pos) rmap
                  -- TODO: Improve rendering of the range info
                  let hc = J.HoverContents $ J.markedUpContent "koka" $ T.pack $ show rinfo
                      hover = J.Hover hc $ Just $ toLspRange r
                  return hover
      responder $ Right rsp
  ]

fromRight' :: Show a => Either a b -> b
fromRight' (Right x) = x
fromRight' (Left e)  = error $ "fromRight' failed: " ++ show e

toLspPos :: R.Pos -> J.Position
toLspPos p = J.Position ((R.posLine p) - 1) ((R.posColumn p) - 1) -- LSP positions are zero-based

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

-- TODO: Emit messages via LSP's logging mechanism
terminal :: Terminal
terminal = Terminal
  { termError = const $ return ()
  , termPhase = const $ return ()
  , termPhaseDoc = const $ return ()
  , termType = const $ return ()
  , termDoc = const $ return ()
  }

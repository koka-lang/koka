-----------------------------------------------------------------------------
-- The hover request handler
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module LanguageServer.Hover( hoverHandler
                           ) where

import Control.Lens                      ( (^.) )
import Control.Monad.IO.Class            ( liftIO )
import Common.Error
import Compiler.Compile                  ( compileModuleOrFile, Terminal (..) )
import Compiler.Options                  ( Flags )
import Compiler.Module                   ( loadedModule, modRangeMap )
import Data.Maybe                        ( fromJust )
import qualified Data.Text               as T
import Language.LSP.Diagnostics          ( partitionBySource )
import Language.LSP.Server               ( requestHandler, publishDiagnostics, flushDiagnosticsBySource, Handlers, LspM )
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import LanguageServer.Conversions
import Lib.PPrint                        ( Pretty (..) )
import Syntax.RangeMap                   ( rangeMapFindAt, RangeInfo (..), NameInfo (..) )

hoverHandler :: Flags -> Handlers (LspM ())
hoverHandler flags = requestHandler J.STextDocumentHover $ \req responder -> do
  let J.HoverParams doc pos _ = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
      -- TODO: Handle error
      filePath = fromJust $ J.uriToFilePath uri
  -- TODO: Use the VFS and recompile (and generate diagnostics)
  --       when the user types (instead of on every hover)
  --       Also, this should happen in a separate module, not here.
  loaded <- liftIO $ compileModuleOrFile terminal flags [] filePath False
  let diagSrc = T.pack "koka"
      diags = toLspDiagnostics diagSrc loaded
      diagsBySrc = partitionBySource diags
      maxDiags = 100
      rsp = do
              l <- rightToMaybe $ fst <$> checkError loaded
              rmap <- modRangeMap $ loadedModule l
              (r, rinfo) <- rangeMapFindAt (fromLspPos uri pos) rmap
              let hc = J.HoverContents $ J.markedUpContent "koka" $ T.pack $ formatHoverContents rinfo
                  hover = J.Hover hc $ Just $ toLspRange r
              return hover
  if null diags
    then flushDiagnosticsBySource maxDiags (Just diagSrc)
    else publishDiagnostics maxDiags normUri (Just 0) diagsBySrc
  responder $ Right rsp

formatHoverContents :: RangeInfo -> String
formatHoverContents rinfo = case rinfo of
  Id qname info isdef -> show (pretty qname) ++ " : " ++ case info of
    NIValue tp  -> show $ pretty tp
    NICon tp    -> show $ pretty tp
    NITypeCon k -> show $ pretty k
    NITypeVar k -> show $ pretty k
    NIModule    -> "module"
    NIKind      -> "kind"
  Decl s name mname   -> s ++ " " ++ show (pretty name)
  Block s             -> s
  Error doc           -> "Error: " ++ show doc
  Warning doc         -> "Warning: " ++ show doc

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right x) = Just x
rightToMaybe (Left _)  = Nothing

-- TODO: Emit messages via LSP's logging mechanism
terminal :: Terminal
terminal = Terminal
  { termError = const $ return ()
  , termPhase = const $ return ()
  , termPhaseDoc = const $ return ()
  , termType = const $ return ()
  , termDoc = const $ return ()
  }

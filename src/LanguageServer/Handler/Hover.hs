-----------------------------------------------------------------------------
-- The LSP handler that provides hover tooltips
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.Hover (hoverHandler) where

import Compiler.Module (loadedModule, modRangeMap)
import Compiler.Options (Flags)
import Control.Lens ((^.))
import qualified Data.Map as M
import qualified Data.Text as T
import Language.LSP.Server (Handlers, requestHandler, sendNotification)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import LanguageServer.Conversions (fromLspPos, toLspRange)
import LanguageServer.Monad (LSM, getLoaded)
import Lib.PPrint (Pretty (..))
import Syntax.RangeMap (NameInfo (..), RangeInfo (..), rangeMapFindAt)
import qualified Language.LSP.Protocol.Message as J
import Data.Maybe (fromMaybe)

hoverHandler :: Flags -> Handlers LSM
hoverHandler flags = requestHandler J.SMethod_TextDocumentHover $ \req responder -> do
  let J.HoverParams doc pos _ = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded
  let rsp = do
        l <- M.lookup normUri loaded
        rmap <- modRangeMap $ loadedModule l
        (r, rinfo) <- rangeMapFindAt (fromLspPos uri pos) rmap
        let hc = J.InL $ J.mkMarkdown $ T.pack $ formatHoverContents rinfo
            hover = J.Hover hc $ Just $ toLspRange r
        return hover
  case rsp of
    Nothing -> responder $ Right $ J.InR J.Null
    Just rsp -> responder $ Right $ J.InL rsp

-- Pretty-prints type/kind information to a hover tooltip
formatHoverContents :: RangeInfo -> String
formatHoverContents rinfo = case rinfo of
  Id qname info isdef ->
    show (pretty qname) ++ " : " ++ case info of
      NIValue tp -> show $ pretty tp
      NICon tp -> show $ pretty tp
      NITypeCon k -> show $ pretty k
      NITypeVar k -> show $ pretty k
      NIModule -> "module"
      NIKind -> "kind"
  Decl s name mname -> s ++ " " ++ show (pretty name)
  Block s -> s
  Error doc -> "Error: " ++ show doc
  Warning doc -> "Warning: " ++ show doc

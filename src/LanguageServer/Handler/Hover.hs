-----------------------------------------------------------------------------
-- The LSP handler that provides hover tooltips
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module LanguageServer.Handler.Hover( hoverHandler
                                   ) where

import Compiler.Options                  ( Flags )
import Compiler.Module                   ( loadedModule, modRangeMap )
import Control.Lens                      ( (^.) )
import qualified Data.Map                as M
import qualified Data.Text               as T
import Language.LSP.Server               ( sendNotification, requestHandler, Handlers )
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import LanguageServer.Conversions        ( toLspRange, fromLspPos )
import LanguageServer.Monad              ( LSM, getLoaded )
import Lib.PPrint                        ( Pretty (..) )
import Syntax.RangeMap                   ( rangeMapFindAt, RangeInfo (..), NameInfo (..) )

hoverHandler :: Flags -> Handlers LSM
hoverHandler flags = requestHandler J.STextDocumentHover $ \req responder -> do
  let J.HoverParams doc pos _ = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded
  let rsp = do l <- M.lookup normUri loaded
               rmap <- modRangeMap $ loadedModule l
               (r, rinfo) <- rangeMapFindAt (fromLspPos uri pos) rmap
               let hc = J.HoverContents $ J.markedUpContent "koka" $ T.pack $ formatHoverContents rinfo
                   hover = J.Hover hc $ Just $ toLspRange r
               return hover
  responder $ Right rsp

-- Pretty-prints type/kind information to a hover tooltip
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

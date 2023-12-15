-----------------------------------------------------------------------------
-- The LSP handler that provides hover tooltips
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module LanguageServer.Handler.Hover (hoverHandler, formatRangeInfoHover) where

import Compiler.Module (loadedModule, modRangeMap, Loaded (loadedModules, loadedImportMap), Module (modPath, modSourcePath))
import Control.Lens ((^.))
import qualified Data.Text as T
import Language.LSP.Server (Handlers, sendNotification, requestHandler)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import LanguageServer.Conversions (fromLspPos, toLspRange)
import LanguageServer.Monad (LSM, getLoaded, getLoadedModule, getHtmlPrinter, getFlags)
import Lib.PPrint (Pretty (..), Doc, text, (<+>), color)
import Syntax.RangeMap (NameInfo (..), RangeInfo (..), rangeMapFindAt)
import qualified Language.LSP.Protocol.Message as J
import Control.Monad.Cont (liftIO)
import Type.Pretty (ppScheme, defaultEnv, Env(..), ppName)
import Common.ColorScheme (ColorScheme (colorNameQual, colorSource), Color (Gray))
import Kind.Pretty (prettyKind)
import Common.Name (nameNil)
import Kind.ImportMap (importsEmpty, ImportMap)
import Compiler.Options (Flags, colorSchemeFromFlags, prettyEnvFromFlags)
import Compiler.Compile (modName)
import Type.Type (Name)

-- Handles hover requests
hoverHandler :: Handlers LSM
hoverHandler = requestHandler J.SMethod_TextDocumentHover $ \req responder -> do
  let J.HoverParams doc pos _ = req ^. J.params
      uri = doc ^. J.uri
  loadedMod <- getLoadedModule uri
  loaded <- getLoaded uri
  let res = do -- maybe monad
        mod <- loadedMod
        l <- loaded
        rmap <- modRangeMap mod
        -- Find the range info at the given position
        (r, rinfo) <- rangeMapFindAt (fromLspPos uri pos) rmap
        return (modName mod, loadedImportMap l, r, rinfo)
  case res of
    Just (mName, imports, r, rinfo) -> do
      -- Get the html-printer and flags
      print <- getHtmlPrinter
      flags <- getFlags
      let env = (prettyEnvFromFlags flags){ context = mName, importsMap = imports }
          colors = colorSchemeFromFlags flags
      x <- liftIO $ print $ formatRangeInfoHover env colors rinfo
      let hc = J.InL $ J.mkMarkdown x
          rsp = J.Hover hc $ Just $ toLspRange r
      responder $ Right $ J.InL rsp
    Nothing -> responder $ Right $ J.InR J.Null

-- Pretty-prints type/kind information to a hover tooltip given a type pretty environment, color scheme
formatRangeInfoHover :: Env -> ColorScheme -> RangeInfo -> Doc
formatRangeInfoHover env colors rinfo =
  case rinfo of
  Id qname info isdef ->
    ppName env{colors=colors{colorSource = Gray}} qname <+> text " : " <+> case info of
      NIValue tp _ -> ppScheme env tp
      NICon tp ->  ppScheme env tp
      NITypeCon k -> prettyKind colors k
      NITypeVar k -> prettyKind colors k
      NIModule -> text "module"
      NIKind -> text "kind"
  Decl s name mname -> text s <+> text " " <+> pretty name
  Block s -> text s
  Error doc -> text "Error: " <+> doc
  Warning doc -> text "Warning: " <+> doc

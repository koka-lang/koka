-----------------------------------------------------------------------------
-- The LSP handler that provides hover tooltips
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module LanguageServer.Handler.Hover (hoverHandler, formatRangeInfoHover) where

import Compiler.Module (loadedModule, modRangeMap, Loaded (loadedModules), Module (modPath, modSourcePath))
import Control.Lens ((^.))
import qualified Data.Map as M
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
import Type.Pretty (ppScheme, defaultEnv, Env(..))
import Common.ColorScheme (ColorScheme (colorNameQual))
import Kind.Pretty (prettyKind)
import Common.Name (nameNil)
import Kind.ImportMap (importsEmpty)
import Compiler.Options (Flags, colorSchemeFromFlags, prettyEnvFromFlags)

hoverHandler :: Handlers LSM
hoverHandler = requestHandler J.SMethod_TextDocumentHover $ \req responder -> do
  let J.HoverParams doc pos _ = req ^. J.params
      uri = doc ^. J.uri
  loaded <- getLoadedModule uri
  flags <- getFlags
  let res = do
        l <- loaded
        rmap <- modRangeMap l
        rangeMapFindAt (fromLspPos uri pos) rmap
  case res of
    Just (r, rinfo) -> do
      print <- getHtmlPrinter
      x <- liftIO $ formatRangeInfoHover print flags rinfo
      let hc = J.InL $ J.mkMarkdown x
          rsp = J.Hover hc $ Just $ toLspRange r
      responder $ Right $ J.InL rsp
    Nothing -> responder $ Right $ J.InR J.Null

prettyEnv flags ctx imports = (prettyEnvFromFlags flags){ context = ctx, importsMap = imports }

-- Pretty-prints type/kind information to a hover tooltip
formatRangeInfoHover :: (Doc -> IO T.Text) -> Flags -> RangeInfo -> IO T.Text
formatRangeInfoHover print flags rinfo = 
  let colors = colorSchemeFromFlags flags in
  case rinfo of
  Id qname info isdef ->
    print $ (color (colorNameQual colors) $ pretty qname) <+> text " : " <+> case info of
      NIValue tp -> ppScheme (prettyEnv flags nameNil importsEmpty) tp
      NICon tp ->  ppScheme (prettyEnv flags nameNil importsEmpty) tp
      NITypeCon k -> prettyKind colors k
      NITypeVar k -> prettyKind colors k
      NIModule -> text "module"
      NIKind -> text "kind"
  Decl s name mname -> print $ text s <+> text " " <+> pretty name
  Block s -> return $ T.pack s
  Error doc -> print $ text "Error: " <+> doc
  Warning doc -> print $ text "Warning: " <+> doc

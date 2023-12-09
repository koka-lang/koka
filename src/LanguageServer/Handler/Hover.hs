-----------------------------------------------------------------------------
-- The LSP handler that provides hover tooltips
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module LanguageServer.Handler.Hover (hoverHandler, formatRangeInfoHover) where

import Compiler.Module (loadedModule, modRangeMap, Loaded (loadedModules, loadedImportMap), Module (modPath, modSourcePath))
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
import Type.Pretty (ppScheme, defaultEnv, Env(..), ppName)
import Common.ColorScheme (ColorScheme (colorNameQual, colorSource), Color (Gray))
import Kind.Pretty (prettyKind)
import Common.Name (nameNil)
import Kind.ImportMap (importsEmpty, ImportMap)
import Compiler.Options (Flags, colorSchemeFromFlags, prettyEnvFromFlags)
import Compiler.Compile (modName)
import Type.Type (Name)

hoverHandler :: Handlers LSM
hoverHandler = requestHandler J.SMethod_TextDocumentHover $ \req responder -> do
  let J.HoverParams doc pos _ = req ^. J.params
      uri = doc ^. J.uri
  loadedMod <- getLoadedModule uri
  loaded <- getLoaded
  flags <- getFlags
  let res = do
        mod <- loadedMod
        l <- loaded
        rmap <- modRangeMap mod
        (r, rinfo) <- rangeMapFindAt (fromLspPos uri pos) rmap
        return (modName mod, loadedImportMap l, r, rinfo)
  case res of
    Just (mName, imports, r, rinfo) -> do
      print <- getHtmlPrinter
      x <- liftIO $ formatRangeInfoHover print flags mName imports rinfo
      let hc = J.InL $ J.mkMarkdown x
          rsp = J.Hover hc $ Just $ toLspRange r
      responder $ Right $ J.InL rsp
    Nothing -> responder $ Right $ J.InR J.Null

prettyEnv flags ctx imports = (prettyEnvFromFlags flags){ context = ctx, importsMap = imports }

-- Pretty-prints type/kind information to a hover tooltip
formatRangeInfoHover :: (Doc -> IO T.Text) ->  Flags -> Name -> ImportMap ->RangeInfo -> IO T.Text
formatRangeInfoHover print flags mName imports rinfo =
  let colors = colorSchemeFromFlags flags
      env = prettyEnv flags mName imports in
  case rinfo of
  Id qname info isdef ->
    print $ (ppName env{colors=colors{colorSource = Gray}} qname) <+> text " : " <+> case info of
      NIValue tp -> ppScheme env tp
      NICon tp ->  ppScheme env tp
      NITypeCon k -> prettyKind colors k
      NITypeVar k -> prettyKind colors k
      NIModule -> text "module"
      NIKind -> text "kind"
  Decl s name mname -> print $ text s <+> text " " <+> pretty name
  Block s -> return $ T.pack s
  Error doc -> print $ text "Error: " <+> doc
  Warning doc -> print $ text "Warning: " <+> doc

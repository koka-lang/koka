------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting, Fredrik Wieczerkowski
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The LSP handler that provides hover tooltips
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module LanguageServer.Handler.Hover (hoverHandler, formatRangeInfoHover) where

import Data.Char(isSpace)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable(maximumBy)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.Server (Handlers, sendNotification, requestHandler)

import Common.Range as R
import Common.Name (nameNil)
import Common.ColorScheme (ColorScheme (colorNameQual, colorSource), Color (Gray))
import Kind.Kind(isKindEffect,isKindHandled,isKindHandled1,isKindLabel)
import Lib.PPrint
import Compiler.Module (loadedModule, modRangeMap, modLexemes, Loaded (loadedModules, loadedImportMap), Module (modPath, modSourcePath))
import Compiler.Options (Flags, colorSchemeFromFlags, prettyEnvFromFlags)
import Compiler.Compile (modName)
import Kind.Pretty (prettyKind)
import Kind.ImportMap (importsEmpty, ImportMap)
import Type.Pretty (ppScheme, defaultEnv, Env(..), ppName, keyword)
import Type.Type (Name)
import Syntax.RangeMap (NameInfo (..), RangeInfo (..), rangeMapFindAt)
import Syntax.Colorize( removeComment )
import LanguageServer.Conversions (fromLspPos, toLspRange)
import LanguageServer.Monad (LSM, getLoaded, getLoadedModule, getHtmlPrinter, getFlags, getLoadedSuccess)
import Debug.Trace (trace)
import LanguageServer.Handler.Pretty (ppComment, asKokaCode)


-- Handles hover requests
hoverHandler :: Handlers LSM
hoverHandler = requestHandler J.SMethod_TextDocumentHover $ \req responder -> do
  let J.HoverParams doc pos _ = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  -- outdated information is fine even if ranges are slightly different, we want to still be able to get hover info
  loaded <- getLoadedSuccess normUri
  pos <- liftIO $ fromLspPos normUri pos

  let res = trace ("loaded success: " ++ show loaded ++ ", pos: " ++ show pos ++ ", uri: " ++ show normUri) $
            do -- maybe monad
                l    <- loaded
                let mod  = loadedModule l
                rmap <- modRangeMap mod
                -- Find the range info at the given position
                {- let rm = rangeMapFindAt (modLexemes mod) pos rmap
                (r, rinfo) <- rangeMapBestHover rm
                -}
                (r,rinfo) <- trace ("hover lookup in rangemap") $
                            rangeMapFindAt (modLexemes mod) pos rmap
                return (modName mod, l, r, rinfo)
  case res of
    Just (mName, l, r, rinfo)
      -> -- trace ("hover found " ++ show rinfo) $
         do -- Get the html-printer and flags
            print <- getHtmlPrinter
            flags <- getFlags
            let env = (prettyEnvFromFlags flags){ context = mName, importsMap = loadedImportMap l }
                colors = colorSchemeFromFlags flags
            markdown <- liftIO $ print $ -- makeMarkdown $ -- makeMarkdown $
                        let doc = formatRangeInfoHover l env colors rinfo
                        in --trace ("formatted hover:\n" ++ show doc) $
                           doc
            let md = J.mkMarkdown markdown
                hc = J.InL md
                rsp = J.Hover hc $ Just $ toLspRange r
            -- trace ("hover markdown:\n" ++ show markdown) $
            responder $ Right $ J.InL rsp
    Nothing
      -> -- trace "no hover info" $
         responder $ Right $ J.InR J.Null



-- Get best rangemap info for a given position
rangeMapBestHover rm =
  case rm of
    [] -> Nothing
    [r] -> Just r
    r:rst -> Just r

-- Pretty-prints type/kind information to a hover tooltip given a type pretty environment, color scheme
formatRangeInfoHover :: Loaded -> Env -> ColorScheme -> RangeInfo -> Doc
formatRangeInfoHover loaded env colors rinfo
  = let kw s      = keyword env s
    in case rinfo of
      Decl s name mname mbType -> asKokaCode (kw s <+> pretty name <.>
                                              case mbType of
                                                Just tp -> text " :" <+> ppScheme env tp
                                                Nothing -> empty)
      Block s             -> asKokaCode (kw s)
      Error doc           -> text "error:" <+> doc
      Warning doc         -> text "warning:" <+> doc
      Implicits fdoc      -> text "implicits:" <+> fdoc False  -- should not occur

      Id qname info docs isdef ->
        let namedoc   = ppName env qname
            signature = case info of
                          NIValue sort tp doc _  -> (if null sort then empty else kw sort) <+> namedoc <+> text ":" <+> ppScheme env tp
                          NICon tp doc      -> kw "con" <+> namedoc <+> text ":" <+> ppScheme env tp
                          NITypeCon k doc   -> (if isKindEffect k || isKindHandled k || isKindLabel k
                                                  then kw "effect"
                                                  else if isKindHandled1 k
                                                    then kw "linear effect"
                                                    else kw "type")
                                                <+> namedoc <+> text "::" <+> prettyKind colors k
                          NITypeVar k       -> kw "type" <+> namedoc <+> text "::" <+> prettyKind colors k
                          NIModule -> kw "module" <+> namedoc <.>
                                      (case filter (\mod -> modName mod == qname) (loadedModules loaded) of
                                            [mod] | not (null (modSourcePath mod)) -> text (" (at \"" ++ modSourcePath mod ++ "\")")
                                            _     -> empty
                                        )
                          NIKind -> kw "kind" <+> namedoc
            comment = case info of
                        NIValue _ tp doc _ -> ppComment doc
                        NICon tp doc       -> ppComment doc
                        NITypeCon k doc    -> ppComment doc
                        _                  -> empty
        in asKokaCode (if null docs then signature else (signature <.> text "  " <-> color Gray (vcat docs)))
          <.> comment

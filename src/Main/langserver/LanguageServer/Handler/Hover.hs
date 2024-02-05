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

import Debug.Trace (trace)
import Data.Char(isSpace)
import Data.List(find)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

import Lib.PPrint
import Common.Name
import Common.Range as R
import Kind.Kind(isKindEffect,isKindHandled,isKindHandled1,isKindLabel)
import Kind.Pretty (prettyKind)
import Type.Pretty (ppScheme, defaultEnv, Env(..), ppName, keyword)
import Syntax.RangeMap (NameInfo (..), RangeInfo (..), rangeMapFindAt)
import Syntax.Colorize( removeComment )

import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.Server (Handlers, sendNotification, requestHandler)
import LanguageServer.Conversions (fromLspPos, toLspRange)
import LanguageServer.Handler.Pretty (ppComment, asKokaCode)
import LanguageServer.Monad



-- Handles hover requests
hoverHandler :: Handlers LSM
hoverHandler
  = requestHandler J.SMethod_TextDocumentHover $ \req responder ->
    do  let J.HoverParams doc pos0 _ = req ^. J.params
            uri  = J.toNormalizedUri (doc ^. J.uri)

            done :: LSM ()
            done = responder $ Right $ J.InR J.Null

            liftMaybe :: LSM (Maybe a) -> (a -> LSM ()) -> LSM ()
            liftMaybe action next = do res <- action
                                       case res of
                                         Nothing -> done
                                         Just x  -> next x

        pos <- liftIO $ fromLspPos uri pos0
        -- trace ("hover: lookup: " ++ show uri) $
        liftMaybe (lookupModuleName uri) $ \(fpath,modname) ->
          -- trace ("hover: found: " ++ show modname) $
           liftMaybe (lookupRangeMap modname) $ \(rmap,lexemes) ->
            -- trace ("hover: found rangemap: " ) $
             liftMaybe (return (rangeMapFindAt lexemes pos rmap)) $ \(rng,rngInfo) ->
              -- trace ("hover: found rng info: " ++ show rngInfo) $
              do penv <- getPrettyEnvFor modname
                 mods <- lookupModulePaths
                 let doc = formatRangeInfoHover penv mods rngInfo
                 markdown <- prettyMarkdown doc
                 let rsp = J.Hover (J.InL (J.mkMarkdown markdown)) (Just (toLspRange rng))
                 -- trace ("hover markdown:\n" ++ show markdown) $
                 responder $ Right $ J.InL rsp


-- Pretty-prints type/kind information to a hover tooltip given a type pretty environment, color scheme
formatRangeInfoHover :: Env -> [(ModuleName,FilePath)] -> RangeInfo -> Doc
formatRangeInfoHover env mods rinfo
  = let kw s = keyword env s
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
                                                <+> namedoc <+> text "::" <+> prettyKind (colors env) k
                          NITypeVar k       -> kw "type" <+> namedoc <+> text "::" <+> prettyKind (colors env) k
                          NIModule -> kw "module" <+> namedoc <.>
                                      (case find (\(modname,fpath) -> modname == qname) mods of
                                            Just (modname,fpath) -> text (" (at \"" ++ fpath ++ "\")")
                                            Nothing -> empty
                                        )
                          NIKind -> kw "kind" <+> namedoc
            comment = case info of
                        NIValue _ tp doc _ -> ppComment doc
                        NICon tp doc       -> ppComment doc
                        NITypeCon k doc    -> ppComment doc
                        _                  -> empty
        in asKokaCode (if null docs then signature else (signature <.> text "  " <-> color Gray (vcat docs)))
          <.> comment

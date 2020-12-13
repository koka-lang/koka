-----------------------------------------------------------------------------
-- The LSP handler that provides code completions
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module LanguageServer.Handler.Completion( completionHandler
                                        ) where

import Compiler.Options                  ( Flags )
import Compiler.Module                   ( loadedModule, modRangeMap )
import Control.Lens                      ( (^.) )
import qualified Data.Map                as M
import Data.Maybe                        ( maybeToList )
import qualified Data.Text               as T
import Language.LSP.Server               ( sendNotification, requestHandler, Handlers )
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import LanguageServer.Conversions        ( toLspRange, fromLspPos )
import LanguageServer.Monad              ( LSM, getLoaded )
import Lib.PPrint                        ( Pretty (..) )
import Syntax.RangeMap                   ( rangeMapFindAt, RangeInfo (..), NameInfo (..) )

completionHandler :: Flags -> Handlers LSM
completionHandler flags = requestHandler J.STextDocumentCompletion $ \req responder -> do
  let J.CompletionParams doc pos _ _ _ = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded
  let items = do l <- maybeToList $ M.lookup normUri loaded
                 []
  responder $ Right $ J.InL $ J.List items

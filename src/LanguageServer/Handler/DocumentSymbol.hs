-----------------------------------------------------------------------------
-- The LSP handler that provides a document symbol tree
-- (sometimes presented as 'outline' in the client's GUI)
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module LanguageServer.Handler.DocumentSymbol( documentSymbolHandler
                                            ) where

import Control.Lens                      ( (^.) )
import Compiler.Options                  ( Flags )
import Compiler.Module                   ( modProgram, loadedModule, Loaded (..) )
import qualified Data.Map                as M
import Data.Maybe                        ( maybeToList )
import Language.LSP.Server               ( requestHandler, Handlers )
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import LanguageServer.Monad              ( LSM, getLoaded )
import Syntax.Syntax                     ( UserProgram )

documentSymbolHandler :: Flags -> Handlers LSM
documentSymbolHandler flags = requestHandler J.STextDocumentDocumentSymbol $ \req responder -> do
  let J.DocumentSymbolParams _ _ doc = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded
  let symbols = findDocumentSymbols =<< maybeToList (M.lookup normUri loaded)
  responder $ Right $ J.InL $ J.List symbols

-- Traverses the syntax tree to find document symbols
findDocumentSymbols :: Loaded -> [J.DocumentSymbol]
findDocumentSymbols loaded = do
  prog <- maybeToList $ modProgram $ loadedModule loaded
  symbols prog

class HasSymbols a where
  symbols :: a -> [J.DocumentSymbol]

instance HasSymbols UserProgram where
  -- TODO
  symbols = const []

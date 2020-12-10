-----------------------------------------------------------------------------
-- The LSP handlers that handle changes to the document
-----------------------------------------------------------------------------
module LanguageServer.Handler.TextDocument( didOpenHandler
                                          , didChangeHandler
                                          , didSaveHandler
                                          , didCloseHandler
                                          ) where

import Compiler.Options                  ( Flags )
import Language.LSP.Server               ( notificationHandler, publishDiagnostics, flushDiagnosticsBySource, Handlers, LspM )
import qualified Language.LSP.Types      as J

didOpenHandler :: Flags -> Handlers (LspM ())
didOpenHandler flags = notificationHandler J.STextDocumentDidOpen $ \_not -> do
    return ()

didChangeHandler :: Flags -> Handlers (LspM ())
didChangeHandler flags = notificationHandler J.STextDocumentDidChange $ \_not -> do
    return ()

didSaveHandler :: Flags -> Handlers (LspM ())
didSaveHandler flags = notificationHandler J.STextDocumentDidSave $ \_not -> do
    return ()

didCloseHandler :: Flags -> Handlers (LspM ())
didCloseHandler flags = notificationHandler J.STextDocumentDidClose $ \_not -> do
    return ()

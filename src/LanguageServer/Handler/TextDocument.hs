-----------------------------------------------------------------------------
-- The LSP handlers that handle changes to the document
-----------------------------------------------------------------------------
module LanguageServer.Handler.TextDocument( didOpenHandler
                                          , didChangeHandler
                                          , didSaveHandler
                                          , didCloseHandler
                                          ) where

import Compiler.Options                  ( Flags )
import Language.LSP.Server               ( notificationHandler, publishDiagnostics, flushDiagnosticsBySource, Handlers )
import qualified Language.LSP.Types      as J
import LanguageServer.Monad              ( LSM )

didOpenHandler :: Flags -> Handlers LSM
didOpenHandler flags = notificationHandler J.STextDocumentDidOpen $ \_not -> do
    return ()

didChangeHandler :: Flags -> Handlers LSM
didChangeHandler flags = notificationHandler J.STextDocumentDidChange $ \_not -> do
    return ()

didSaveHandler :: Flags -> Handlers LSM
didSaveHandler flags = notificationHandler J.STextDocumentDidSave $ \_not -> do
    return ()

didCloseHandler :: Flags -> Handlers LSM
didCloseHandler flags = notificationHandler J.STextDocumentDidClose $ \_not -> do
    return ()

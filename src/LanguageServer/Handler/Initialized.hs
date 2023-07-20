-----------------------------------------------------------------------------
-- The LSP handlers that handles initialization
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.Initialized (initializedHandler) where

import Compiler.Options (Flags)
import Language.LSP.Server (Handlers, LspM, notificationHandler, sendNotification)
import qualified Language.LSP.Protocol.Types as J
import LanguageServer.Monad (LSM)
import qualified Language.LSP.Protocol.Message as J

initializedHandler :: Flags -> Handlers LSM
initializedHandler flags = notificationHandler J.SMethod_Initialized $ \_not -> do
  sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info "Initialized language server."

-----------------------------------------------------------------------------
-- The LSP handlers that handles initialization
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module LanguageServer.Handler.Initialized( initializedHandler
                                         ) where

import Compiler.Options                  ( Flags )
import Language.LSP.Server               ( notificationHandler, sendNotification, Handlers, LspM )
import qualified Language.LSP.Types      as J
import LanguageServer.Monad              ( LSM )

initializedHandler :: Flags -> Handlers LSM
initializedHandler flags = notificationHandler J.SInitialized $ \_not -> do
  sendNotification J.SWindowLogMessage $ J.LogMessageParams J.MtInfo "Initialized language server."

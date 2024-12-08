------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The request handlers used by the language server
-----------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module LanguageServer.Handlers (ReactorInput(..), lspHandlers) where


import GHC.Conc (atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM.TChan ( writeTChan )
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent (modifyMVar)
import Control.Concurrent.Async ( race )
import Control.Concurrent.STM
    ( atomically, retry, writeTChan, readTVar, readTVarIO, modifyTVar )
import Control.Lens ((^.))
import Control.Monad.Trans (lift)
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad (when, unless)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Language.LSP.Protocol.Lens hiding (color, text, retry)
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message (TRequestMessage(..), TNotificationMessage(..), Method, MessageDirection(..), MessageKind(..), SMethod (..), SomeLspId (SomeLspId), LspId (..), NotificationMessage (..), ResponseError (..))
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.Protocol.Types (DidChangeTextDocumentParams(..), VersionedTextDocumentIdentifier (..))
import Language.LSP.Server (Handlers, notificationHandler, sendNotification, Handler, mapHandlers, MonadLsp (..))

import Compile.Options (Flags,Terminal(..))
import Lib.PPrint (Doc, text, color)
import Common.ColorScheme (Color (..))

import LanguageServer.Handler.Completion (completionHandler)
import LanguageServer.Handler.Definition (definitionHandler)
import LanguageServer.Handler.DocumentSymbol (documentSymbolHandler)
import LanguageServer.Handler.Hover (hoverHandler)
import LanguageServer.Handler.InlayHints (inlayHintsHandler)
import LanguageServer.Handler.Commands (commandHandler)
import LanguageServer.Handler.Folding (foldingHandler)
import LanguageServer.Handler.TextDocument (didChangeHandler, didCloseHandler, didOpenHandler, didSaveHandler)
import LanguageServer.Handler.SignatureHelp (signatureHelpHandler)
import LanguageServer.Monad
import GHC.IO (catchAny)
import LanguageServer.Handler.CodeAction (codeActionHandler)

newtype ReactorInput = ReactorAction (IO ())

-- A list of all the handlers we support
handlers :: Handlers LSM
handlers =
  mconcat
    [ initializedHandler,
      didOpenHandler,
      didChangeHandler,
      didSaveHandler,
      didCloseHandler,
      hoverHandler,
      definitionHandler,
      documentSymbolHandler,
      completionHandler,
      cancelHandler,
      commandHandler,
      inlayHintsHandler,
      codeActionHandler,
      foldingHandler,
      configurationChangeHandler,
      signatureHelpHandler
    ]

configurationChangeHandler :: Handlers LSM
configurationChangeHandler = notificationHandler J.SMethod_WorkspaceDidChangeConfiguration $ \_not -> do
  let J.DidChangeConfigurationParams{_settings} = _not ^. params
  updateConfig _settings
  return ()

-- Handles the initialized notification
initializedHandler :: Handlers LSM
initializedHandler =
  notificationHandler J.SMethod_Initialized $ \_not -> do
    term <- getTerminal
    liftIO $ termInfo term $ color DarkGray (text "Initialized language server.")

-- Handles cancel requests
cancelHandler :: Handlers LSM
cancelHandler =
  notificationHandler SMethod_CancelRequest $ \msg ->
    do
      let id_ = msg ^. params ^. L.id
      state <- lift ask
      stateV <- liftIO $ readMVar state
      -- Add the request id to the cancelled requests set
      _ <- liftIO $ atomically $ modifyTVar (cancelledRequests stateV) $ \t -> S.insert (SomeLspId (toLspId id_)) t
      return ()
  where toLspId (J.InL x) = IdInt x
        toLspId (J.InR y) = IdString y

-- Wraps the normal handler with a function that runs a request in a separate thread and races it with a cancel
lspHandlers rin = mapHandlers goReq goNot handlers where
  -- For requests (which can be canceled, and the client expects a response)
  goReq :: forall (a :: Method ClientToServer Request). Handler LSM a -> Handler LSM a
  goReq f msg@TRequestMessage{_id} k = do
    env <- getLspEnv
    state <- lift ask
    let newId = SomeLspId _id
    stVal <- liftIO $ readMVar state
    -- Inserts the request into the pending requests set
    liftIO $ atomically $ modifyTVar (pendingRequests stVal) $ \t -> S.insert newId t

    -- Spins in a loop waiting for changes to the cancelled requests set and if the request is in there it finishes the thread
    let waitForCancel reqId = atomically $ do
          cancelled <- readTVar (cancelledRequests stVal)
          unless (reqId `S.member` cancelled) retry
    -- adds a task to the reactor to run the request in a separate thread
    liftIO $ atomically $ writeTChan rin $ -- check if canceled and if so don't run
       ReactorAction $ do
        -- To run it we race against the cancel
        cancelOrRes <- race (waitForCancel newId) $ do
            cancelled <- readTVarIO (cancelledRequests stVal)
            -- If the request is cancelled before we start we return a cancelled error
            if newId `S.member` cancelled then do
              
              runLSM (k $ Left $ J.TResponseError (J.InL J.LSPErrorCodes_RequestCancelled) (T.pack "") Nothing) state env
            -- Otherwise we run the request
            else 
              catchAny (runLSM (f msg k) state env)
                (\err -> runLSM (k $ Left $ J.TResponseError (J.InL J.LSPErrorCodes_RequestFailed) (T.pack (show err)) Nothing) state env)
        liftIO $ atomically $ do -- After it finishes we remove it from the pending requests set and canceled set
          modifyTVar (pendingRequests stVal) $ \t -> S.delete newId t
          modifyTVar (cancelledRequests stVal) $ \t -> S.delete newId t
        case cancelOrRes of -- Finally we return the result of the request
          Left () -> return ()
          Right res -> pure res

  -- For notifications from the client (which are just fire and forget for the client)
  goNot :: forall (a :: Method ClientToServer Notification). Handler LSM a -> Handler LSM a
  goNot f msg = do
    env <- getLspEnv
    state <- lift ask
    stVal <- liftIO $ readMVar state
    let mtd = msg ^. method
    let runCatchErrors = 
          catchAny (runLSM (f msg) state env)
            (\err -> runLSM (emitNotification (\env -> text (show err))) state env)
    case mtd of
      SMethod_TextDocumentDidChange -> do
      -- If text document change command, and a new change comes in with a newer version for the same file, cancel the old one
        let TNotificationMessage{_params=DidChangeTextDocumentParams{_textDocument=VersionedTextDocumentIdentifier{_uri, _version}}} = msg
        let normUri = J.toNormalizedUri _uri
        stateV <- liftIO $ readMVar state
        liftIO $ atomically $ modifyTVar (documentVersions stateV) $ \t -> M.insert normUri _version t
        liftIO $ atomically $ writeTChan rin $
          ReactorAction $ do
            -- When running the request we check if the version is the same as the latest version, if not we don't run the change handler
            versions <- readTVarIO (documentVersions stateV)
            -- runLSM (emitNotification (\env -> text ("Latest versions: " ++ show versions))) state env
            -- runLSM (emitNotification (\env -> text ("Doc Version: " ++ show normUri ++ ": " ++ show _version))) state env
            when (M.lookup normUri versions == Just _version) runCatchErrors
      _ ->
        liftIO $ atomically $ writeTChan rin $
          ReactorAction runCatchErrors

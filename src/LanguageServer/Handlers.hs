-----------------------------------------------------------------------------
-- The request handlers used by the language server
-----------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs                     #-}

module LanguageServer.Handlers (ReactorInput(..), lspHandlers) where

import Compiler.Options (Flags)
import Language.LSP.Server (Handlers, notificationHandler, Handler, mapHandlers, MonadLsp (..))
import LanguageServer.Handler.Completion (completionHandler)
import LanguageServer.Handler.Definition (definitionHandler)
import LanguageServer.Handler.DocumentSymbol (documentSymbolHandler)
import LanguageServer.Handler.Hover (hoverHandler)
import LanguageServer.Handler.InlayHints (inlayHintsHandler)
import LanguageServer.Handler.Commands (initializedHandler, commandHandler)
import LanguageServer.Handler.TextDocument (didChangeHandler, didCloseHandler, didOpenHandler, didSaveHandler)
import LanguageServer.Monad (LSM, runLSM, putLSState, LSState (..))
import Language.LSP.Protocol.Message (TRequestMessage(..), TNotificationMessage(..), Method, MessageDirection(..), MessageKind(..), SMethod (..), SomeLspId (SomeLspId), LspId (..), NotificationMessage (..), ResponseError (..))
import Control.Monad.Trans (lift)
import Control.Monad.Reader (MonadReader(ask))

import GHC.Conc (atomically)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar (readMVar)
import Control.Lens ((^.))
import Control.Concurrent (modifyMVar)
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Set as S
import Language.LSP.Protocol.Lens hiding (retry)
import Prelude hiding (id)
import qualified Language.LSP.Protocol.Types as J
import Language.LSP.Protocol.Types (DidChangeTextDocumentParams(..), VersionedTextDocumentIdentifier (..))
import Control.Monad (when, unless)
import qualified Data.Text as T

newtype ReactorInput = ReactorAction (IO ())

lspHandlers rin = mapHandlers goReq goNot handle where
  goReq :: forall (a :: Method ClientToServer Request). Handler LSM a -> Handler LSM a
  goReq f msg@TRequestMessage{_id} k = do
    env <- getLspEnv
    state <- lift ask
    let newId = SomeLspId _id
    stVal <- liftIO $ readMVar state
    liftIO $ atomically $ modifyTVar (pendingRequests stVal) $ \t -> S.insert newId t
  
    let waitForCancel reqId = atomically $ do
          cancelled <- readTVar (cancelledRequests stVal)
          unless (reqId `S.member` cancelled) retry
    liftIO $ atomically $ writeTChan rin $ -- check if canceled and if so don't run
       ReactorAction $ do
        cancelOrRes <- race (waitForCancel newId) $ do
            cancelled <- readTVarIO (cancelledRequests stVal)
            if newId `S.member` cancelled then runLSM (k $ Left $ ResponseError (J.InL J.LSPErrorCodes_RequestCancelled) (T.pack "") Nothing) state env else
              runLSM (f msg k) state env
        case cancelOrRes of
          Left () -> return ()
          Right res -> pure res
    liftIO $ atomically $ do
      modifyTVar (pendingRequests stVal) $ \t -> S.delete newId t
      modifyTVar (cancelledRequests stVal) $ \t -> S.delete newId t


  goNot :: forall (a :: Method ClientToServer Notification). Handler LSM a -> Handler LSM a
  goNot f msg = do
    env <- getLspEnv
    state <- lift ask
    stVal <- liftIO $ readMVar state
    let mtd = msg ^. method
    case mtd of
      SMethod_TextDocumentDidChange -> do
      -- If text document change command, and a new change comes in with a newer version for the same file, cancel the old one
        let TNotificationMessage{_params=DidChangeTextDocumentParams{_textDocument=VersionedTextDocumentIdentifier{_uri, _version}}} = msg
        stateV <- liftIO $ readMVar state
        liftIO $ atomically $ modifyTVar (documentVersions stateV) $ \t -> M.insert _uri _version t
        liftIO $ atomically $ writeTChan rin $
          ReactorAction $ do
            versions <- readTVarIO (documentVersions stVal)
            when (M.lookup _uri versions == Just _version) $ runLSM (f msg) state env
      _ ->
        liftIO $ atomically $ writeTChan rin $
          ReactorAction (runLSM (f msg) state env)

handle = handlers

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
      inlayHintsHandler
    ]

cancelHandler :: Handlers LSM
cancelHandler =
  notificationHandler SMethod_CancelRequest $ \msg ->
    do
      let id_ = msg ^. params ^. id
      state <- lift ask
      stateV <- liftIO $ readMVar state
      _ <- liftIO $ atomically $ modifyTVar (cancelledRequests stateV) $ \t -> S.insert (SomeLspId (toLspId id_)) t
      return ()
  where toLspId (J.InL x) = IdInt x
        toLspId (J.InR y) = IdString y
-----------------------------------------------------------------------------
-- The language server's main module
-----------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module LanguageServer.Run (runLanguageServer) where

import System.Exit            ( exitFailure )
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.Conc (atomically)
import GHC.IO.Handle (BufferMode(NoBuffering), hSetBuffering)
import GHC.IO.StdHandles (stdout, stderr)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM ( atomically )
import Control.Concurrent.STM.TChan ( newTChan, readTChan, TChan )
import Control.Concurrent ( readMVar, MVar, forkIO )
import Language.LSP.Server
import Colog.Core (LogAction, WithSeverity)
import qualified Colog.Core as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Server as J
import Language.LSP.Logging (defaultClientLogger)
import Network.Simple.TCP ( connect )
import Network.Socket ( socketToHandle )
import LanguageServer.Handlers ( lspHandlers, ReactorInput(..) )
import LanguageServer.Monad (newLSStateVar, runLSM, LSM, getLSState, LSState (messages, progress), getProgress)
import Compiler.Options (Flags (languageServerPort))
import Debug.Trace (trace)

runLanguageServer :: Flags -> [FilePath] -> IO ()
runLanguageServer flags files = do
  if languageServerPort flags == -1 then do
    putStr "No port specified for language server\n. Use --lsport=<port> to specify a port."
    exitFailure
  else return ()
  -- Have to set line buffering, otherwise the client doesn't receive data until buffers fill up
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  -- Connect to localhost on the port given by the client
  connect "127.0.0.1" (show $ languageServerPort flags) (\(socket, _) -> do
    -- Create a handle to the client from the socket
    handle <- socketToHandle socket ReadWriteMode
    -- Create a new language server state
    state <- newLSStateVar flags
    -- Get the message channel
    messageChan <- liftIO $ messages <$> readMVar state
    progressChan <- liftIO $ progress <$> readMVar state
    -- Create a new channel for the reactor to receive messages on
    rin <- atomically newTChan :: IO (TChan ReactorInput)
    void $
      runServerWithHandles
        ioLogger
        lspLogger
        handle
        handle
        $
        ServerDefinition
          { parseConfig = const $ const $ Right (),
            onConfigChange = const $ pure (),
            defaultConfig = (),
            configSection = T.pack "koka",
            -- Two threads, the request thread and the message thread (so we can send messages to the client, while the compilation is happening)
            doInitialize = \env _ -> forkIO (reactor rin) >> forkIO (messageHandler messageChan env state) >> forkIO (progressHandler progressChan env state) >> pure (Right env),
            staticHandlers = \_caps -> lspHandlers rin,
            interpretHandler = \env -> Iso (\lsm -> runLSM lsm state env) liftIO,
            options =
              defaultOptions
                { optTextDocumentSync = Just syncOptions,
                  optExecuteCommandCommands = Just [T.pack "koka/compile", T.pack "koka/compileFunction"],
                  optCompletionTriggerCharacters = Just ['.', ':', '/']
                -- TODO: ? https://www.stackage.org/haddock/lts-18.21/lsp-1.2.0.0/src/Language.LSP.Server.Core.html#Options
                }
          })
  where
    -- io logger, prints all log level messages to stdout
    ioLogger :: LogAction IO (WithSeverity LspServerLog)
    ioLogger = L.cmap show L.logStringStdout
    stderrLogger :: LogAction IO (WithSeverity T.Text)
    stderrLogger = L.cmap show L.logStringStderr
    -- lsp logger, prints all messages to stdout and to the client
    lspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
    lspLogger =
      let clientLogger = L.cmap (fmap (T.pack . show)) defaultClientLogger
      in clientLogger <> L.hoistLogAction liftIO ioLogger
    syncOptions =
      J.TextDocumentSyncOptions
        (Just True) -- open/close notifications
        (Just J.TextDocumentSyncKind_Incremental) -- changes
        (Just False) -- will save
        (Just False) -- will save (wait until requests are sent to server)
        (Just $ J.InR $ J.SaveOptions $ Just False) -- trigger on save, but dont send document

-- Handles messages to send to the client, just spins and sends
messageHandler :: TChan (String, J.MessageType) -> LanguageContextEnv () -> MVar LSState -> IO ()
messageHandler msgs env state = do
  forever $ do
    (msg, msgType) <- atomically $ readTChan msgs
    runLSM (sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams msgType $ T.pack msg) state env

-- Handles messages to send to the client, just spins and sends
progressHandler :: TChan String -> LanguageContextEnv () -> MVar LSState -> IO ()
progressHandler msgs env state = do
  forever $ do
    msg <- atomically $ readTChan msgs
    runLSM (do
        trace ("Progress: " <> msg) $ return ()
        report <- getProgress
        case report of
          Just report -> do
            trace ("Progress: " <> msg) $ return ()
            report (J.ProgressAmount (Just 1) (Just $ T.pack msg))
          Nothing -> return ()
      ) state env

-- Runs in a loop, getting the next queued request and executing it
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act
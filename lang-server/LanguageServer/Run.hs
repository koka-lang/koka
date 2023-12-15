-----------------------------------------------------------------------------
-- The language server's main module
-----------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module LanguageServer.Run (runLanguageServer) where

import System.Exit            ( exitFailure )
import Compiler.Options (Flags (languageServerPort))
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Language.LSP.Server
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Server as J
import LanguageServer.Handlers
import LanguageServer.Monad (newLSStateVar, runLSM, LSM, getLSState, LSState (messages))
import Colog.Core (LogAction, WithSeverity)
import qualified Colog.Core as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.LSP.Logging (defaultClientLogger)
import Network.Simple.TCP
import Network.Socket hiding (connect)
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.Conc (atomically)
import LanguageServer.Handler.TextDocument (persistModules)
import GHC.IO.Handle (BufferMode(LineBuffering), hSetBuffering)
import GHC.IO.StdHandles (stdout, stderr)

runLanguageServer :: Flags -> [FilePath] -> IO ()
runLanguageServer flags files = do
  if languageServerPort flags == -1 then do
    putStr "No port specified for language server\n. Use --lsport=<port> to specify a port."
    exitFailure 
  else return ()
  -- Have to set line buffering, otherwise the client doesn't receive data until buffers fill up
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  -- Connect to localhost on the port given by the client
  connect "127.0.0.1" (show $ languageServerPort flags) (\(socket, _) -> do
    -- Create a handle to the client from the socket
    handle <- socketToHandle socket ReadWriteMode
    -- Create a new language server state
    state <- newLSStateVar flags
    -- Get the message channel
    messageChan <- liftIO $ messages <$> readMVar state
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
            doInitialize = \env _ -> forkIO (reactor rin) >> forkIO (messageHandler messageChan env state) >> pure (Right env),
            staticHandlers = \_caps -> lspHandlers rin,
            interpretHandler = \env -> Iso (\lsm -> runLSM lsm state env) liftIO,
            options =
              defaultOptions
                { optTextDocumentSync = Just syncOptions,
                  optExecuteCommandCommands = Just [T.pack "koka/genCode", T.pack "koka/interpretExpression"]
                  -- optCompletionTriggerCharacters = Just ['.', ':', '/']
                -- TODO: ? https://www.stackage.org/haddock/lts-18.21/lsp-1.2.0.0/src/Language.LSP.Server.Core.html#Options
                }
          })
  where
    prettyMsg l = "[" <> show (L.getSeverity l) <> "] " <> show (L.getMsg l) <> "\n\n"
    -- io logger, prints all log level messages to stdout
    ioLogger :: LogAction IO (WithSeverity LspServerLog)
    ioLogger = L.cmap prettyMsg L.logStringStdout
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

-- Runs in a loop, getting the next queued request and executing it
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- TODO: Finish persisting modules in a separate thread
doPersist state env =
  forever $ do
    threadDelay 1000000
    runLSM persistModules state env

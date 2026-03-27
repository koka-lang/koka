------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting, Fredrik Wieczerkowski
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The language server's main module
-----------------------------------------------------------------------------
{-# OPTIONS -cpp #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module LanguageServer.Run (runLanguageServer) where

import System.Exit            ( exitFailure, die )
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.Conc (atomically)
import GHC.IO.Handle (Handle, BufferMode(NoBuffering), hSetBuffering)
import GHC.IO.StdHandles (stdin, stdout, stderr)
import System.IO (hPutStrLn)
import Control.Monad (void, forever, when, guard)
#if defined(KOKA_WASM)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Builder.Extra (defaultChunkSize)
import System.IO (hFlush, utf8, hSetEncoding)
#else
import Network.Simple.TCP ( connect )
import Network.Socket ( socketToHandle )
#endif
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM ( atomically )
import Control.Concurrent.STM.TChan ( newTChan, readTChan, TChan )
import Control.Concurrent ( readMVar, MVar, forkIO )
import Language.LSP.Server
import Colog.Core (LogAction, WithSeverity)
import qualified Colog.Core as L
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Server as J
import Language.LSP.Logging (defaultClientLogger)
import LanguageServer.Handlers ( lspHandlers, ReactorInput(..) )
import LanguageServer.Monad (newLSStateVar, runLSM, LSM, getLSState, LSState (messages, progress), getProgress, updateSignatureContext, SignatureContext(..))
import Compile.Options (Flags (languageServerPort, languageServerStdio))
import Control.Exception (catchJust)
import System.IO.Error (isDoesNotExistError)

-- ── Entry point ─────────────────────────────────────────────────────────

runLanguageServer :: Flags -> [FilePath] -> IO ()
runLanguageServer flags files = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hSetBuffering stdin NoBuffering
#if defined(KOKA_WASM)
  -- WASM/WASI: use stdio with a retrying stdin reader.
  -- On WASI, fd_read may return 0 bytes (no data yet) rather than blocking.
  -- The LSP parser (attoparsec) treats 0 bytes as end-of-input and fails.
  -- We retry with a short delay until real data is available.
  let wasiStdinRead = do
        bs <- BS.hGetSome stdin defaultChunkSize
        if BS.null bs
          then do threadDelay 10000  -- 10ms, yields to GHC scheduler
                  wasiStdinRead
          else return bs
  state <- newLSStateVar flags
  messageChan <- liftIO $ messages <$> readMVar state
  progressChan <- liftIO $ progress <$> readMVar state
  rin <- atomically newTChan :: IO (TChan ReactorInput)
  hSetEncoding stdout utf8
  let clientOut out = BSL.hPut stdout out >> hFlush stdout
  void $
    runServerWith
      (kokaIoLogger True)
      kokaLspLogger
      wasiStdinRead
      clientOut
      (serverDef flags rin state messageChan progressChan)
#else
  let useStdio = languageServerStdio flags
  when (not useStdio && languageServerPort flags == -1) $ do
    hPutStrLn stderr "No port specified for language server.\nUse --lsport=<port> to specify a port or --lsstdio to use stdio."
    exitFailure
  if useStdio
    then runWithHandles flags stdin stdout True
    else catchJust (guard . isDoesNotExistError)
           (connect "127.0.0.1" (show $ languageServerPort flags) (\(socket, _) -> do
              handle <- socketToHandle socket ReadWriteMode
              runWithHandles flags handle handle False))
           (\_ -> die $ "nothing was listening on port " ++ show (languageServerPort flags))

runWithHandles :: Flags -> Handle -> Handle -> Bool -> IO ()
runWithHandles flags inHandle outHandle useStdio = do
  state <- newLSStateVar flags
  messageChan <- liftIO $ messages <$> readMVar state
  progressChan <- liftIO $ progress <$> readMVar state
  rin <- atomically newTChan :: IO (TChan ReactorInput)
  void $
    runServerWithHandles
      (kokaIoLogger useStdio)
      kokaLspLogger
      inHandle
      outHandle
      (serverDef flags rin state messageChan progressChan)
#endif

-- ── Shared server definition ────────────────────────────────────────────

serverDef :: Flags -> TChan ReactorInput -> MVar LSState
          -> TChan (String, J.MessageType) -> TChan (Double, String)
          -> ServerDefinition ()
serverDef flags rin state messageChan progressChan =
  ServerDefinition
    { parseConfig = const $ const $ Right (),
      onConfigChange = const $ pure (),
      defaultConfig = (),
      configSection = T.pack "koka",
      doInitialize = \env _ ->
        forkIO (reactor rin) >>
        forkIO (messageHandler messageChan env state) >>
        forkIO (progressHandler progressChan env state) >>
        pure (Right env),
      staticHandlers = \_caps -> lspHandlers rin,
      interpretHandler = \env -> Iso (\lsm -> runLSM lsm state env) liftIO,
      options = defaultOptions
        { optTextDocumentSync = Just syncOptions,
          optExecuteCommandCommands = Just
            [ T.pack "koka/compile"
            , T.pack "koka/compileFunction"
            , T.pack "koka/signature-help/set-context"
            , T.pack "koka/set-colors"
            ],
          optCompletionTriggerCharacters = Just ['.', ':', '/', ' ', ']', '}'],
          optSignatureHelpTriggerCharacters = Just ['(', ',', ' '],
          optSignatureHelpRetriggerCharacters = Just [')'],
          optProgressStartDelay = 100000,  -- 100ms
          optProgressUpdateDelay = 20000   -- 20ms
        }
    }

-- ── Shared helpers ──────────────────────────────────────────────────────

syncOptions :: J.TextDocumentSyncOptions
syncOptions =
  J.TextDocumentSyncOptions
    (Just True)                                     -- open/close notifications
    (Just J.TextDocumentSyncKind_Incremental)       -- changes
    (Just False)                                    -- will save
    (Just False)                                    -- will save wait until
    (Just $ J.InR $ J.SaveOptions $ Just False)     -- trigger on save

kokaIoLogger :: Bool -> LogAction IO (WithSeverity LspServerLog)
kokaIoLogger useStdio = L.cmap show (if useStdio then L.logStringStderr else L.logStringStdout)

kokaLspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
kokaLspLogger =
  let clientLogger = L.cmap (fmap (T.pack . show)) defaultClientLogger
  in clientLogger <> L.hoistLogAction liftIO (kokaIoLogger True)

messageHandler :: TChan (String, J.MessageType) -> LanguageContextEnv () -> MVar LSState -> IO ()
messageHandler msgs env state =
  forever $ do
    (msg, msgType) <- atomically $ readTChan msgs
    runLSM (sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams msgType $ T.pack msg) state env

progressHandler :: TChan (Double, String) -> LanguageContextEnv () -> MVar LSState -> IO ()
progressHandler msgs env state =
  forever $ do
    (pct, msg) <- atomically $ readTChan msgs
    runLSM (do
        report <- getProgress
        case report of
          Just report -> report (J.ProgressAmount (Just (round pct)) (Just $ T.pack msg))
          Nothing     -> return ()
      ) state env

reactor :: TChan ReactorInput -> IO ()
reactor inp =
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

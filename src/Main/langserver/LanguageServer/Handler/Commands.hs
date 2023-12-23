------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The LSP handlers that handles initialization
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.Commands (commandHandler) where

import qualified Data.Text as T
import Data.Aeson as Json
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import GHC.Base (Type)
import qualified Language.LSP.Server as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import Common.Name (newName)
import Common.NamePrim (nameInteractiveModule)
import Common.Range (rangeNull)
import Language.LSP.Server (Handlers, LspM, notificationHandler, sendNotification, MonadLsp, getVirtualFiles, withProgress, requestHandler)
import LanguageServer.Monad (LSM, getFlags, getTerminal, getModules, getLoaded, setProgress)
import LanguageServer.Handler.TextDocument (recompileFile, compileEditorExpression)
import Compiler.Compile (CompileTarget(..), Terminal (..), compileExpression, Module (..))
import Compiler.Options (Flags (outFinalPath), targets, commandLineHelp, updateFlagsFromArgs)
import Compiler.Module (Loaded(..))
import Core.Core (Visibility(Private))
import Syntax.Syntax (programAddImports, programNull, Import (..))
import Lib.PPrint ((<+>), text, Color (..), color, (<-->))

-- Handles custom commands that we support clients to call
commandHandler :: Handlers LSM
commandHandler = requestHandler J.SMethod_WorkspaceExecuteCommand $ \req resp -> do
  let J.ExecuteCommandParams _ command commandParams = req ^. J.params
  flags <- getFlags
  if command == "koka/compile" then
    case commandParams of
      -- koka/compile filePath "...args to parse"
      Just [Json.String filePath, Json.String additionalArgs] -> do
        -- Update the flags with the specified arguments
        newFlags <- getNewFlags flags additionalArgs
        let forceRecompilation = flags /= newFlags
        -- Recompile the file, but with executable target
        withProgress (T.pack "Compiling " <> filePath) J.Cancellable $ \report -> do
          setProgress (Just report)
          res <- recompileFile (Executable (newName "main") ()) (J.filePathToUri $ T.unpack filePath) Nothing forceRecompilation newFlags
          term <- getTerminal
          liftIO $ termDoc term $ text "Finished generating code for main file" <+> color DarkGreen (text (T.unpack filePath)) <--> color DarkGreen (text (fromMaybe "No Compiled File" res))
          setProgress Nothing
          -- Send the executable file location back to the client in case it wants to run it
          resp $ Right $ case res of {Just filePath -> J.InL $ Json.String $ T.pack filePath; Nothing -> J.InR J.Null}
      _ -> do
        -- Client didn't send the right parameters for this command
        sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack "Invalid parameters for koka/compile"
        resp $ Right $ J.InR J.Null
  else if command == "koka/compileFunction" then
    case commandParams of
      -- The `filePath` where a top level function is defined by the name `functionName`, and any additional flags
      Just [Json.String filePath, Json.String functionName, Json.String additionalArgs] -> do
        -- Update the flags with the specified arguments
        newFlags <- getNewFlags flags additionalArgs
        let forceRecompilation = flags /= newFlags
        -- Compile the expression, but with the interpret target
        withProgress (T.pack "Compiling " <> functionName) J.Cancellable $ \report -> do
          setProgress (Just report)
          -- compile the expression
          res <- compileEditorExpression (J.filePathToUri $ T.unpack filePath) newFlags forceRecompilation (T.unpack filePath) (T.unpack functionName)
          term <- getTerminal
          liftIO $ termDoc term $ text "Finished generating code for function" <+> color DarkRed (text (T.unpack functionName)) <+>  color DarkGreen (text (T.unpack filePath)) <--> color DarkGreen (text (fromMaybe "No Compiled File" res))
          setProgress Nothing
          -- Send the executable file location back to the client in case it wants to run it
          resp $ Right $ case res of {Just filePath -> J.InL $ Json.String $ T.pack filePath; Nothing -> J.InR J.Null}
      _ -> do
        -- Client didn't send the right parameters for this command
        sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack "Invalid parameters for koka/compileFunction"
        resp $ Right $ J.InR J.Null
  else
    do
      sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack ("Unknown command" ++ show req)
      resp $ Right $ J.InR J.Null

getNewFlags :: Flags -> T.Text -> LSM Flags
getNewFlags flags args = do
  term <- getTerminal
  case updateFlagsFromArgs flags (T.unpack args) of
    Just flags' -> return flags'
    Nothing -> do
      doc <- liftIO (commandLineHelp flags)
      sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack "Invalid arguments " <> args
      liftIO $ termPhaseDoc term doc
      return flags
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

import Debug.Trace(trace)
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
import Language.LSP.Server (Handlers, LspM, notificationHandler, sendNotification, MonadLsp, getVirtualFiles, withProgress, requestHandler)

import Lib.PPrint
import Common.Name (newName)
import Common.NamePrim (nameInteractiveModule)
import Common.Range (rangeNull)

import LanguageServer.Monad
import LanguageServer.Conversions( filePathToUri )
import LanguageServer.Handler.TextDocument (rebuildUri)

import Compile.Options (Flags (outFinalPath), targets, commandLineHelp, updateFlagsFromArgs)



-- Handles custom commands that we support clients to call
commandHandler :: Handlers LSM
commandHandler = requestHandler J.SMethod_WorkspaceExecuteCommand $ \req resp -> do
  let J.ExecuteCommandParams _ command commandParams = req ^. J.params
  let parameterError = do
        sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack "Invalid parameters for " <> command
        resp $ Right $ J.InR J.Null
  flags <- getFlags
  if command == "koka/compile" then
    case commandParams of
      -- koka/compile filePath "...args to parse"
      Just [Json.String filePath, Json.String additionalArgs] -> do
        -- Update the flags with the specified arguments
        newFlags <- getNewFlags flags additionalArgs
        -- Recompile the file, but with executable target
        withProgress (T.pack "Compiling " <> filePath) Nothing J.Cancellable $ \report -> do
          setProgress (Just report)
          res <- -- trace ("koka/compile: " ++ T.unpack filePath ++ ", " ++ show (filePathToUri (T.unpack filePath))) $
                 rebuildUri (Just newFlags) (Just (newName "main")) (J.toNormalizedUri (filePathToUri (T.unpack filePath)))
          emitInfo $ \penv -> case res of
            Just file -> text "Finished generating code for main file" <+> color DarkGreen (text (T.unpack filePath)) 
                            <--> text "Executable:" <+> color DarkGreen (text file)
            Nothing -> color Red $ text "Compilation Failed"
          setProgress Nothing
          -- Send the executable file location back to the client in case it wants to run it
          resp $ Right $ case res of {Just exePath -> J.InL $ Json.String $ T.pack exePath; Nothing -> J.InR J.Null}
      _ -> parameterError
  else if command == "koka/compileFunction" then
    case commandParams of
      -- The `filePath` where a top level function is defined by the name `functionName`, and any additional flags
      Just [Json.String filePath, Json.String functionName, Json.String additionalArgs] -> do
        -- Update the flags with the specified arguments
        newFlags <- getNewFlags flags additionalArgs
        -- Compile the expression, but with the interpret target
        withProgress (T.pack "Compiling " <> functionName) Nothing J.Cancellable $ \report -> do
          setProgress (Just report)
          -- compile the expression
          res <- rebuildUri (Just newFlags) (Just (newName (T.unpack functionName)))
                            (J.toNormalizedUri (filePathToUri (T.unpack filePath)))
          emitInfo $ \penv -> case res of
            Just file -> text "Finished generating code for function" <+> color DarkRed (text (T.unpack functionName)) 
                            <+> text "in" <+> color DarkGreen (text (T.unpack filePath))
                            <--> text "Executable:" <+> color DarkGreen (text file)
            Nothing -> color Red $ text "Compilation Failed"
          setProgress Nothing
          -- Send the executable file location back to the client in case it wants to run it
          resp $ Right $ case res of {Just exePath -> J.InL $ Json.String $ T.pack exePath; Nothing -> J.InR J.Null}
      _ -> parameterError
  else if command == "koka/signature-help/set-context" then
    case commandParams of
      Just [a@(Json.Object _)] ->
        case fromJSON a of
          Json.Success context -> updateSignatureContext context
          _ -> parameterError
      Nothing -> parameterError
  else
    do
      sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack ("Unknown command: " ++ show req)
      resp $ Right $ J.InR J.Null

getNewFlags :: Flags -> T.Text -> LSM Flags
getNewFlags flags args = do
  term <- getTerminal
  case updateFlagsFromArgs flags (T.unpack args) of
    Just flags' -> return flags'
    Nothing -> do
      doc <- liftIO (commandLineHelp flags)
      emitNotification $  \penv -> text "Invalid arguments:" <+> text (T.unpack args)
      emitInfo $ \penv -> doc
      return flags
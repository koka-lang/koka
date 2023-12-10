-----------------------------------------------------------------------------
-- The LSP handlers that handles initialization
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.Commands (initializedHandler, commandHandler) where

import Compiler.Options (Flags (outFinalPath), targets, commandLineHelp, updateFlagsFromArgs)
import Language.LSP.Server (Handlers, LspM, notificationHandler, sendNotification, MonadLsp, getVirtualFiles, withIndefiniteProgress, requestHandler)
import qualified Language.LSP.Protocol.Types as J
import qualified Data.Text as T
import LanguageServer.Monad (LSM, getFlags, getTerminal, getModules, getLoaded)
import qualified Language.LSP.Protocol.Message as J
import Data.Aeson as Json
import qualified Language.LSP.Protocol.Lens as J
import Control.Lens ((^.))
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import GHC.Base (Type)
import LanguageServer.Handler.TextDocument (recompileFile, compileEditorExpression)
import Compiler.Compile (CompileTarget(..), Terminal (termError, termPhaseDoc), compileExpression, Module (..))
import Common.Name (newName)
import qualified Language.LSP.Server as J
import Control.Monad.Trans (liftIO)
import Syntax.Syntax (programAddImports, programNull, Import (..))
import Common.NamePrim (nameInteractiveModule)
import Compiler.Module (Loaded(..))
import Common.Range (rangeNull)
import Core.Core (Visibility(Private))

initializedHandler :: Handlers LSM
initializedHandler = notificationHandler J.SMethod_Initialized $ \_not -> sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info "Initialized language server."

targetFlag :: String -> Flags -> Flags
targetFlag t f
    = case lookup t targets of
        Just update -> update f
        Nothing     -> f

commandHandler :: Handlers LSM
commandHandler = requestHandler J.SMethod_WorkspaceExecuteCommand $ \req resp -> do
  flags <- getFlags
  let J.ExecuteCommandParams _ command commandParams = req ^. J.params
  if command == "koka/genCode" then
    case commandParams of
      Just [Json.String filePath, Json.String additionalArgs] -> do
        term <- getTerminal
        newFlags <-  case updateFlagsFromArgs flags (T.unpack additionalArgs) of
          Just flags' -> return flags'
          Nothing -> do
            doc <- liftIO (commandLineHelp flags)
            sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack "Invalid arguments " <> additionalArgs
            liftIO $ termPhaseDoc term doc
            return flags
        withIndefiniteProgress (T.pack "Compiling " <> filePath) J.NotCancellable $ do
          res <- recompileFile (Executable (newName "main") ()) (J.filePathToUri $ T.unpack filePath) Nothing False newFlags
          sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info $ T.pack ("Finished generating code for main file " ++ T.unpack filePath ++ " " ++ fromMaybe "No Compiled File" res)
          resp $ Right $ case res of {Just filePath -> J.InL $ Json.String $ T.pack filePath; Nothing -> J.InR J.Null}
      _ -> do
        sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack "Invalid parameters"
        resp $ Right $ J.InR J.Null
  else if command == "koka/interpretExpression" then
    case commandParams of
      Just [Json.String filePath, Json.String functionName, Json.String additionalArgs] -> do
        term <- getTerminal
        newFlags <-  case updateFlagsFromArgs flags (T.unpack additionalArgs) of
          Just flags' -> return flags'
          Nothing -> do
            doc <- liftIO (commandLineHelp flags)
            sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack "Invalid arguments " <> additionalArgs
            liftIO $ termPhaseDoc term doc
            return flags
        withIndefiniteProgress (T.pack "Interpreting " <> functionName) J.NotCancellable $ do
          -- term flags loaded compileTarget program line input
          res <- compileEditorExpression (J.filePathToUri $ T.unpack filePath) newFlags (T.unpack filePath) (T.unpack functionName)
          sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info $ T.pack ("Finished generating code for interpreting function " ++ T.unpack functionName ++ " in file " ++ T.unpack filePath ++ " Result: " ++ fromMaybe "No Compiled File" res)
          resp $ Right $ case res of {Just filePath -> J.InL $ Json.String $ T.pack filePath; Nothing -> J.InR J.Null}
      _ -> do
        sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack "Invalid parameters"
        resp $ Right $ J.InR J.Null
  else
    do
      sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack ("Unknown command" ++ show req)
      resp $ Right $ J.InR J.Null

liftMaybe:: Monad m => Maybe (m ()) -> m ()
liftMaybe Nothing = return ()
liftMaybe (Just m) = m

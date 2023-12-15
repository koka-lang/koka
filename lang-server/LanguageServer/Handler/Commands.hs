-----------------------------------------------------------------------------
-- The LSP handlers that handles initialization
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.Commands (commandHandler) where

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

-- Handles custom commands that we support clients to call
commandHandler :: Handlers LSM
commandHandler = requestHandler J.SMethod_WorkspaceExecuteCommand $ \req resp -> do
  let J.ExecuteCommandParams _ command commandParams = req ^. J.params
  flags <- getFlags
  if command == "koka/genCode" then
    case commandParams of
      -- koka/genCode filePath "...args to parse"
      Just [Json.String filePath, Json.String additionalArgs] -> do
        -- Update the flags with the specified arguments
        newFlags <- getNewFlags flags additionalArgs
        -- Recompile the file, but with executable target
        withIndefiniteProgress (T.pack "Compiling " <> filePath) J.NotCancellable $ do
          res <- recompileFile (Executable (newName "main") ()) (J.filePathToUri $ T.unpack filePath) Nothing False newFlags
          sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info $ T.pack ("Finished generating code for main file " ++ T.unpack filePath ++ " " ++ fromMaybe "No Compiled File" res)
          -- Send the executable file location back to the client in case it wants to run it
          resp $ Right $ case res of {Just filePath -> J.InL $ Json.String $ T.pack filePath; Nothing -> J.InR J.Null}
      _ -> do 
        -- Client didn't send the right parameters for this command
        sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack "Invalid parameters for koka/genCode"
        resp $ Right $ J.InR J.Null
  else if command == "koka/interpretExpression" then
    case commandParams of
      -- The `filePath` where a top level function is defined by the name `functionName`, and any additional flags
      Just [Json.String filePath, Json.String functionName, Json.String additionalArgs] -> do
        -- Update the flags with the specified arguments
        newFlags <- getNewFlags flags additionalArgs
        -- Compile the expression, but with the interpret target
        withIndefiniteProgress (T.pack "Interpreting " <> functionName) J.NotCancellable $ do
          -- compile the expression
          res <- compileEditorExpression (J.filePathToUri $ T.unpack filePath) newFlags (T.unpack filePath) (T.unpack functionName)
          sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info $ T.pack ("Finished generating code for interpreting function " ++ T.unpack functionName ++ " in file " ++ T.unpack filePath ++ " Result: " ++ fromMaybe "No Compiled File" res)
          -- Send the executable file location back to the client in case it wants to run it
          resp $ Right $ case res of {Just filePath -> J.InL $ Json.String $ T.pack filePath; Nothing -> J.InR J.Null}
      _ -> do
        -- Client didn't send the right parameters for this command
        sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack "Invalid parameters for koka/interpretExpression"
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
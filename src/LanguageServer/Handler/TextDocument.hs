-----------------------------------------------------------------------------
-- The LSP handlers that handle changes to the document
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.TextDocument
  ( didOpenHandler,
    didChangeHandler,
    didSaveHandler,
    didCloseHandler,
    recompileFile,
    compileEditorExpression,
    persistModules,
  )
where

import Common.Error (Error, checkPartial)
import Compiler.Compile (Terminal (..), compileModuleOrFile, Loaded (..), CompileTarget (..), compileFile, codeGen, compileExpression)
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server (Handlers, flushDiagnosticsBySource, publishDiagnostics, sendNotification, getVirtualFile, getVirtualFiles, notificationHandler)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import LanguageServer.Conversions (toLspDiagnostics)
import LanguageServer.Monad (LSM, getLoaded, putLoaded, getTerminal, getFlags, LSState (documentInfos), getLSState, modifyLSState, removeLoaded, getModules)
import Language.LSP.VFS (virtualFileText, VFS(..), VirtualFile, file_version, virtualFileVersion)
import qualified Data.Text.Encoding as T
import Data.Functor ((<&>))
import qualified Language.LSP.Protocol.Message as J
import Data.ByteString (ByteString)
import Data.Map (Map)
import Text.Read (readMaybe)
import Debug.Trace (trace)
import Control.Exception (try)
import qualified Control.Exception as Exc
import Compiler.Options (Flags)
import Common.File (getFileTime, FileTime, getFileTimeOrCurrent, getCurrentTime, normalize, realPath)
import GHC.IO (unsafePerformIO)
import Compiler.Module (Module(..))
import Control.Monad (when, foldM)
import Data.Time (addUTCTime, addLocalTime)
import qualified Data.ByteString as J
import Syntax.Syntax (programNull, programAddImports)
import Common.Range (rangeNull)
import Core.Core (Visibility(Private))
import Common.NamePrim (nameInteractiveModule, nameExpr, nameSystemCore)
import Common.Name (newName)
import Syntax.Syntax (Import(..))

didOpenHandler :: Handlers LSM
didOpenHandler = notificationHandler J.SMethod_TextDocumentDidOpen $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  let version = msg ^. J.params . J.textDocument . J.version
  flags <- getFlags
  _ <- recompileFile InMemory uri (Just version) False flags
  return ()

didChangeHandler :: Handlers LSM
didChangeHandler = notificationHandler J.SMethod_TextDocumentDidChange $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  let version = msg ^. J.params . J.textDocument . J.version
  flags <- getFlags
  _ <- recompileFile InMemory uri (Just version) False flags
  return ()

didSaveHandler :: Handlers LSM
didSaveHandler = notificationHandler J.SMethod_TextDocumentDidSave $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  flags <- getFlags
  _ <- recompileFile InMemory uri Nothing False flags
  return ()

didCloseHandler :: Handlers LSM
didCloseHandler = notificationHandler J.SMethod_TextDocumentDidClose $ \_msg -> do
  -- TODO: Remove file from LSM state?
  return ()

maybeContents :: Map FilePath (ByteString, FileTime, J.Int32) -> FilePath -> Maybe (ByteString, FileTime)
maybeContents vfs uri = do
  -- trace ("Maybe contents " ++ show uri ++ " " ++ show (M.keys vfs)) $ return ()
  (text, ftime, vers) <- M.lookup uri vfs
  return (text, ftime)

diffVFS :: Map FilePath (ByteString, FileTime, J.Int32) -> Map J.NormalizedUri VirtualFile -> LSM (Map FilePath (ByteString, FileTime, J.Int32))
diffVFS oldvfs vfs =
  foldM (\acc (k, v) ->
    let newK = normalize $ J.fromNormalizedFilePath $ fromJust $ J.uriToNormalizedFilePath k
        text = T.encodeUtf8 $ virtualFileText v
        vers = virtualFileVersion v
    in case M.lookup newK oldvfs of
      Just old@(_, _, vOld) -> 
        if vOld == vers then
          return $ M.insert newK old acc
        else do
          time <- liftIO getCurrentTime 
          return $ M.insert newK (text, time, vers) acc
      Nothing -> do
        time <- liftIO $ getFileTime newK
        -- trace ("New file " ++ show newK ++ " " ++ show time) $ return ()
        return $ M.insert newK (text, time, vers) acc)
    M.empty (M.toList vfs)

compileEditorExpression :: J.Uri -> Flags -> String -> String -> LSM (Maybe FilePath)
compileEditorExpression uri flags filePath functionName = do
  let normUri = J.toNormalizedUri uri
  term <- getTerminal
  loaded <- getLoaded uri
  case loaded of
    Just loaded -> do
      let mod = loadedModule loaded
      let imports = [Import nameSystemCore nameSystemCore rangeNull Private, Import (modName mod) (modName mod) rangeNull Private]
      let resultIO :: IO (Either Exc.SomeException (Error Loaded (Loaded, Maybe FilePath)))
          resultIO = try $ compileExpression term flags loaded (Executable nameExpr ()) (programAddImports (programNull nameInteractiveModule) imports) 0 (functionName ++ "()") 
      result <- liftIO resultIO
      case result of
        Right res -> do
          outFile <- case checkPartial res of
            Right ((l, outFile), _, _) -> do
              putLoaded l
              sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info $ "Successfully compiled " <> T.pack filePath
              return outFile
            Left (e, m) -> do
              case m of
                Nothing -> 
                  trace ("Error when compiling, no cached modules " ++ show e) $
                  return ()
                Just l -> do
                  trace ("Error when compiling have cached" ++ show (map modSourcePath $ loadedModules l)) $ return ()
                  putLoaded l
                  removeLoaded (loadedModule l)
              sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack ("Error when compiling " ++ show e) <> T.pack filePath
              return Nothing
          -- Emit the diagnostics (errors and warnings)
          let diagSrc = T.pack "koka"
              diags = toLspDiagnostics diagSrc res
              diagsBySrc = partitionBySource diags
              maxDiags = 100
          if null diags
            then flushDiagnosticsBySource maxDiags (Just diagSrc)
            else publishDiagnostics maxDiags normUri Nothing diagsBySrc
          return outFile
        Left e -> do
          sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ "When compiling file " <> T.pack filePath <> T.pack (" compiler threw exception " ++ show e)
          sendNotification J.SMethod_WindowShowMessage $ J.ShowMessageParams J.MessageType_Error $ "When compiling file " <> T.pack filePath <> T.pack (" compiler threw exception " ++ show e)
          return Nothing

-- Recompiles the given file, stores the compilation result in
-- LSM's state and emits diagnostics.
recompileFile :: CompileTarget () -> J.Uri -> Maybe J.Int32 -> Bool -> Flags -> LSM (Maybe FilePath)
recompileFile compileTarget uri version force flags =
  case J.uriToFilePath uri of
    Just filePath0 -> do
      path <- liftIO $ realPath filePath0
      let filePath = normalize path
      -- Recompile the file
      vFiles <- getVirtualFiles
      let vfs = _vfsMap vFiles
      oldvfs <- documentInfos <$> getLSState
      newvfs <- diffVFS oldvfs vfs
      modifyLSState (\old -> old{documentInfos = newvfs})
      let contents = fst <$> maybeContents newvfs filePath
      modules <- getModules
      loaded <- getLoaded uri
      term <- getTerminal
      sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info $ T.pack $ "Recompiling " ++ filePath

      let resultIO :: IO (Either Exc.SomeException (Error Loaded (Loaded, Maybe FilePath)))
          resultIO = try $ compileFile (maybeContents newvfs) contents term flags (maybe [] loadedModules loaded) (if force then [] else modules) compileTarget [] filePath
      result <- liftIO resultIO
      case result of
        Right res -> do
          outFile <- case checkPartial res of
            Right ((l, outFile), _, _) -> do
              putLoaded l
              sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info $ "Successfully compiled " <> T.pack filePath
              return outFile
            Left (e, m) -> do
              case m of
                Nothing -> 
                  trace ("Error when compiling, no cached modules " ++ show e) $
                  return ()
                Just l -> do
                  trace ("Error when compiling have cached" ++ show (map modSourcePath $ loadedModules l)) $ return ()
                  putLoaded l
                  removeLoaded (loadedModule l)
              sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack ("Error when compiling " ++ show e) <> T.pack filePath
              return Nothing
          -- Emit the diagnostics (errors and warnings)
          let diagSrc = T.pack "koka"
              diags = toLspDiagnostics diagSrc res
              diagsBySrc = partitionBySource diags
              maxDiags = 100
          if null diags
            then flushDiagnosticsBySource maxDiags (Just diagSrc)
            else publishDiagnostics maxDiags normUri version diagsBySrc
          return outFile
        Left e -> do
          sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ "When compiling file " <> T.pack filePath <> T.pack (" compiler threw exception " ++ show e)
          sendNotification J.SMethod_WindowShowMessage $ J.ShowMessageParams J.MessageType_Error $ "When compiling file " <> T.pack filePath <> T.pack (" compiler threw exception " ++ show e)
          return Nothing
    Nothing -> return Nothing
  where
    normUri = J.toNormalizedUri uri

persistModules :: LSM ()
persistModules = do
  mld <- getModules
  mapM_ persistModule mld -- TODO: Dependency ordering
  
persistModule :: Module -> LSM ()
persistModule m = do
  return ()
  -- TODO: This works, but needs to check that the dependencies are persisted first.
  -- let generate = do
  --       -- trace "Generating" $ return ()
  --       mld <- getLoaded
  --       case mld of
  --         Just loaded -> do
  --           term <- getTerminal
  --           flags <- getFlags
  --           (loaded, file) <- liftIO $ codeGen term flags Object loaded{loadedModule = m}
  --           putLoaded loaded
  --           return ()
  --         Nothing -> return ()
  -- -- trace ("Module " ++ show (modName m)) $ 
  -- case modOutputTime m of
  --   Nothing -> do
  --     -- trace "No output time" $ return ()
  --     generate
  --   -- If it has been 5 seconds since the last time the module was changed
  --   --  and it isn't updated on disk persist again.
  --   --  We don't do it all the time, because with virtual files and editor changes it would be too much
  --   Just t -> do
  --     ct <- liftIO getCurrentTime
  --     when ((ct > addUTCTime 5 (modTime m)) && (modTime m > t)) $ do
  --       -- trace ("Last output time" ++ show t) $ return ()
  --       generate
  -- return ()
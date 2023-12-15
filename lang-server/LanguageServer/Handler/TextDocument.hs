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
import LanguageServer.Conversions (toLspDiagnostics, makeDiagnostic)
import LanguageServer.Monad (LSM, getLoaded, putLoaded, getTerminal, getFlags, LSState (documentInfos), getLSState, modifyLSState, removeLoaded, getModules, putDiagnostics, getDiagnostics, clearDiagnostics, removeLoadedUri)
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
import Syntax.Syntax ( programNull, programAddImports, Import(..) )
import Common.Range (rangeNull)
import Core.Core (Visibility(Private))
import Common.NamePrim (nameInteractiveModule, nameExpr, nameSystemCore)
import Common.Name (newName)
import Lib.PPrint (text)

-- Compile the file on opening
didOpenHandler :: Handlers LSM
didOpenHandler = notificationHandler J.SMethod_TextDocumentDidOpen $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  let version = msg ^. J.params . J.textDocument . J.version
  flags <- getFlags
  _ <- recompileFile InMemory uri (Just version) False flags
  return ()

-- Recompile the file on changes
didChangeHandler :: Handlers LSM
didChangeHandler = notificationHandler J.SMethod_TextDocumentDidChange $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  let version = msg ^. J.params . J.textDocument . J.version
  flags <- getFlags
  _ <- recompileFile InMemory uri (Just version) False flags
  return ()

-- Saving a file just recompiles it
didSaveHandler :: Handlers LSM
didSaveHandler = notificationHandler J.SMethod_TextDocumentDidSave $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  flags <- getFlags
  _ <- recompileFile InMemory uri Nothing False flags
  return ()

-- Closing the file
didCloseHandler :: Handlers LSM
didCloseHandler = notificationHandler J.SMethod_TextDocumentDidClose $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  removeLoadedUri uri
  -- Don't remove diagnostics so the file stays red in the editor, and problems are shown, but do remove the compilation state
  return ()

-- Retreives a file from the virtual file system, returning the contents and the last modified time
maybeContents :: Map FilePath (ByteString, FileTime, J.Int32) -> FilePath -> Maybe (ByteString, FileTime)
maybeContents vfs uri = do
  -- trace ("Maybe contents " ++ show uri ++ " " ++ show (M.keys vfs)) $ return ()
  (text, ftime, vers) <- M.lookup uri vfs
  return (text, ftime)

-- Creates a diff of the virtual file system including keeping track of version numbers and last modified times
-- Modified times are not present in the LSP libraris's virtual file system, so we do it ourselves
diffVFS :: Map FilePath (ByteString, FileTime, J.Int32) -> Map J.NormalizedUri VirtualFile -> LSM (Map FilePath (ByteString, FileTime, J.Int32))
diffVFS oldvfs vfs =
  -- Fold over the new map, creating a new map that has the same keys as the new map
  foldM (\acc (k, v) -> do
    -- Get the key as a normalized file path
    path0 <- liftIO $ realPath $ J.fromNormalizedFilePath $ fromJust $ J.uriToNormalizedFilePath k
    let newK = normalize path0
    -- New file contents & verson
    let text = T.encodeUtf8 $ virtualFileText v
        vers = virtualFileVersion v
    case M.lookup newK oldvfs of
      Just old@(_, _, vOld) ->
        -- If the key is in the old map, and the version number is the same, keep the old value
        if vOld == vers then
          return $ M.insert newK old acc
        else do
          -- Otherwise update the value with a new timestamp
          time <- liftIO getCurrentTime
          return $ M.insert newK (text, time, vers) acc
      Nothing -> do
        -- If the key wasn't already present in the map, get it's file time from disk (since it was just opened / created)
        time <- liftIO $ getFileTimeOrCurrent newK
        -- trace ("New file " ++ show newK ++ " " ++ show time) $ return ()
        return $ M.insert newK (text, time, vers) acc)
    M.empty (M.toList vfs)

-- Updates the virtual file system in the LSM state
updateVFS :: LSM (Map FilePath (ByteString, FileTime, J.Int32))
updateVFS = do
  -- Get the virtual files
  vFiles <- getVirtualFiles
  -- Get the full map
  let vfs = _vfsMap vFiles
  -- Get the old virtual files we have stored
  oldvfs <- documentInfos <$> getLSState
  -- Diff the virtual files
  newvfs <- diffVFS oldvfs vfs
  -- Update the virtual files in the state
  modifyLSState (\old -> old{documentInfos = newvfs})
  return newvfs

-- Compiles a single expression (calling a top level function with no arguments) - such as a test method
compileEditorExpression :: J.Uri -> Flags -> String -> String -> LSM (Maybe FilePath)
compileEditorExpression uri flags filePath functionName = do
  loaded <- getLoaded uri
  case loaded of
    Just loaded -> do
      let mod = loadedModule loaded
      -- Get the virtual files
      vfs <- documentInfos <$> getLSState
      -- Set up the imports for the expression (core and the module)
      let imports = [Import nameSystemCore nameSystemCore rangeNull Private, Import (modName mod) (modName mod) rangeNull Private]
          program = programAddImports (programNull nameInteractiveModule) imports
      term <- getTerminal
      -- reusing interpreter compilation entry point
      let resultIO = compileExpression (maybeContents vfs) term flags loaded (Executable nameExpr ()) program 0 (functionName ++ "()")
      processCompilationResult normUri filePath False resultIO
    Nothing -> do
      return Nothing
  where normUri = J.toNormalizedUri uri

-- Recompiles the given file, stores the compilation result in
-- LSM's state and emits diagnostics
recompileFile :: CompileTarget () -> J.Uri -> Maybe J.Int32 -> Bool -> Flags -> LSM (Maybe FilePath)
recompileFile compileTarget uri version force flags =
  case J.uriToFilePath uri of
    Just filePath0 -> do
      -- Get the file path
      path <- liftIO $ realPath filePath0
      let filePath = normalize path
      -- Update the virtual file system
      newvfs <- updateVFS
      -- Get the file contents
      let contents = fst <$> maybeContents newvfs filePath
      modules <- getModules
      term <- getTerminal
      sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info $ T.pack $ "Recompiling " ++ filePath
      -- Don't use the cached modules as regular modules (they may be out of date, so we want to resolveImports fully over again)
      let resultIO = compileFile (maybeContents newvfs) contents term flags [] (if force then [] else modules) compileTarget [] filePath
      processCompilationResult normUri filePath True resultIO
    Nothing -> return Nothing
  where
    normUri = J.toNormalizedUri uri

-- Processes the result of a compilation, updating the loaded state and emitting diagnostics
-- Returns the executable file path if compilation succeeded
processCompilationResult :: J.NormalizedUri -> FilePath -> Bool -> IO (Error Loaded (Loaded, Maybe FilePath)) -> LSM (Maybe FilePath)
processCompilationResult normUri filePath update doIO = do
  let ioResult :: IO (Either Exc.SomeException (Error Loaded (Loaded, Maybe FilePath)))
      ioResult = try doIO
  result <- liftIO ioResult
  case result of
    Left e -> do
      -- Compilation threw an exception, put it in the log, as well as a notification
      sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ "When compiling file " <> T.pack filePath <> T.pack (" compiler threw exception " ++ show e)
      sendNotification J.SMethod_WindowShowMessage $ J.ShowMessageParams J.MessageType_Error $ "When compiling file " <> T.pack filePath <> T.pack (" compiler threw exception " ++ show e)
      let diagSrc = T.pack "koka"
          maxDiags = 100
          diags = M.fromList [(normUri, [makeDiagnostic J.DiagnosticSeverity_Error diagSrc rangeNull (text $ show e)])]
      putDiagnostics diags
      diags <- getDiagnostics
      let diagsBySrc = M.map partitionBySource diags
      flushDiagnosticsBySource maxDiags (Just diagSrc)
      mapM_ (\(uri, diags) -> publishDiagnostics maxDiags uri Nothing diags) (M.toList diagsBySrc)
      return Nothing
    Right res -> do
      -- No exception - so check the result of the compilation
      outFile <- case checkPartial res of
        Right ((l, outFile), _, _) -> do
          -- Compilation succeeded
          when update $ putLoaded l -- update the loaded state for this file
          sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Info $ "Successfully compiled " <> T.pack filePath
          return outFile -- return the executable file path
        Left (e, m) -> do
          -- Compilation failed
          case m of
            Nothing ->
              trace ("Error when compiling, no cached modules " ++ show e) $
              return ()
            Just l -> do
              trace ("Error when compiling have cached" ++ show (map modSourcePath $ loadedModules l)) $ return ()
              when update $ putLoaded l
              removeLoaded (loadedModule l)
          sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error $ T.pack ("Error when compiling " ++ show e) <> T.pack filePath
          return Nothing
      -- Emit the diagnostics (errors and warnings)
      let diagSrc = T.pack "koka"
          diags = toLspDiagnostics normUri diagSrc res
          maxDiags = 100
          -- Union with the current file mapped to an empty list, since we want to clear diagnostics for this file when it is an error in another file
          diags' = M.union diags (M.fromList [(normUri, [])])
      -- Clear diagnostics for this file if there are no errors / warnings
      if null diags then clearDiagnostics normUri else putDiagnostics diags'
      -- Get all the diagnostics for all files (language server doesn't support updating diagnostics for a single file)
      diags <- getDiagnostics
      -- Partition them by source (koka, typescript, linterx, etc.) -- we should only have koka for now
      let diagsBySrc = M.map partitionBySource diags
      if null diags
        -- If there are no diagnostics clear all koka diagnostics
        then flushDiagnosticsBySource maxDiags (Just diagSrc)
        -- Otherwise report all diagnostics
        else do
          flushDiagnosticsBySource maxDiags (Just diagSrc)
          mapM_ (\(uri, diags) -> publishDiagnostics maxDiags uri Nothing diags) (M.toList diagsBySrc)
      return outFile

-- Persists all modules to disk
persistModules :: LSM ()
persistModules = do
  mld <- getModules
  mapM_ persistModule mld -- TODO: Dependency ordering

-- Persist a single module to disk (not yet implemented)
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
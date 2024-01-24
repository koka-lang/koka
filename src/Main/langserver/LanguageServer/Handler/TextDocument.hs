------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting, Fredrik Wieczerkowski
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

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
    compileEditorExpression
  )
where


import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)
import Control.Exception (try)
import qualified Control.Exception as Exc
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Monad (when, foldM)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Functor ((<&>))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server (Handlers, flushDiagnosticsBySource, publishDiagnostics, sendNotification, getVirtualFile, getVirtualFiles, notificationHandler)
import Language.LSP.VFS (virtualFileText, VFS(..), VirtualFile, file_version, virtualFileVersion)
import Lib.PPrint (text, (<->), (<+>), color, Color (..))
import Common.Range (rangeNull)
import Common.NamePrim (nameInteractiveModule, nameExpr, nameSystemCore)
import Common.Name (newName)
import Common.File (getFileTime, FileTime, getFileTimeOrCurrent, getCurrentTime, isAbsolute, dirname)
import Common.ColorScheme(ColorScheme(..))
import Common.Error
import Core.Core (Visibility(Private))
import Compiler.Options (Flags, colorSchemeFromFlags, includePath)
import Compiler.Compile (Terminal (..), compileModuleOrFile, Loaded (..), CompileTarget (..), compileFile, codeGen, compileExpression)
import Compiler.Module (Module(..), initialLoaded)
import LanguageServer.Conversions (toLspDiagnostics, makeDiagnostic, fromLspUri)
import LanguageServer.Monad (LSM, getLoaded, putLoaded, getTerminal, getFlags, LSState (documentInfos), getLSState, modifyLSState, removeLoaded, getModules, putDiagnostics, getDiagnostics, clearDiagnostics, removeLoadedUri, getLastChangedFileLoaded, putLoadedSuccess, getLoadedLatest)


import Syntax.Syntax ( programNull, programAddImports, Import(..) )
-- Compile the file on opening
didOpenHandler :: Handlers LSM
didOpenHandler = notificationHandler J.SMethod_TextDocumentDidOpen $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  let version = msg ^. J.params . J.textDocument . J.version
  flags <- getFlags
  _ <- recompileFile Object uri (Just version) False flags
  return ()

-- Recompile the file on changes
didChangeHandler :: Handlers LSM
didChangeHandler = notificationHandler J.SMethod_TextDocumentDidChange $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  let version = msg ^. J.params . J.textDocument . J.version
  flags <- getFlags
  _ <- recompileFile Object uri (Just version) False flags
  return ()

-- Saving a file just recompiles it
didSaveHandler :: Handlers LSM
didSaveHandler = notificationHandler J.SMethod_TextDocumentDidSave $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  flags <- getFlags
  _ <- recompileFile Object uri Nothing False flags
  return ()

-- Closing the file
didCloseHandler :: Handlers LSM
didCloseHandler = notificationHandler J.SMethod_TextDocumentDidClose $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  removeLoadedUri (J.toNormalizedUri uri)
  -- Don't remove diagnostics so the file stays red in the editor, and problems are shown, but do remove the compilation state
  return ()

-- Retreives a file from the virtual file system, returning the contents and the last modified time
maybeContents :: Map J.NormalizedUri (ByteString, FileTime, J.Int32) -> FilePath -> Maybe (ByteString, FileTime)
maybeContents vfs path = do
  -- trace ("Maybe contents " ++ show uri ++ " " ++ show (M.keys vfs)) $ return ()
  let uri = J.toNormalizedUri $ J.filePathToUri path
  (text, ftime, vers) <- M.lookup uri vfs
  return (text, ftime)

-- Creates a diff of the virtual file system including keeping track of version numbers and last modified times
-- Modified times are not present in the LSP libraris's virtual file system, so we do it ourselves
diffVFS :: Map J.NormalizedUri (ByteString, FileTime, J.Int32) -> Map J.NormalizedUri VirtualFile -> LSM (Map J.NormalizedUri (ByteString, FileTime, J.Int32))
diffVFS oldvfs vfs =
  -- Fold over the new map, creating a new map that has the same keys as the new map
  foldM (\acc (k, v) -> do
    -- New file contents & verson
    let text = T.encodeUtf8 $ virtualFileText v
        vers = virtualFileVersion v
    case M.lookup k oldvfs of
      Just old@(_, _, vOld) ->
        -- If the key is in the old map, and the version number is the same, keep the old value
        if vOld == vers then
          return $ M.insert k old acc
        else do
          -- Otherwise update the value with a new timestamp
          time <- liftIO getCurrentTime
          return $ M.insert k (text, time, vers) acc
      Nothing -> do
        -- If the key wasn't already present in the map, get it's file time from disk (since it was just opened / created)
        path <- liftIO $ fromLspUri k
        time <- liftIO $ getFileTimeOrCurrent (fromMaybe "" path)
        -- trace ("New file " ++ show newK ++ " " ++ show time) $ return ()
        return $ M.insert k (text, time, vers) acc)
    M.empty (M.toList vfs)

-- Updates the virtual file system in the LSM state
updateVFS :: LSM (Map J.NormalizedUri (ByteString, FileTime, J.Int32))
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
compileEditorExpression :: J.Uri -> Flags -> Bool -> String -> String -> LSM (Maybe FilePath)
compileEditorExpression uri flags force filePath functionName = do
  loaded <- getLoadedLatest normUri
  case loaded of
    Just loaded -> do
      let mod = loadedModule loaded
      -- Get the virtual files
      vfs <- documentInfos <$> getLSState
      modules <- getLastChangedFileLoaded (normUri, flags)
      -- Set up the imports for the expression (core and the module)
      let imports = [-- Import nameSystemCore nameSystemCore rangeNull rangeNull rangeNull Private,
                     Import (modName mod) (modName mod) rangeNull rangeNull rangeNull Private]
          program = programAddImports (programNull nameInteractiveModule) imports
          flagsEx = if (isAbsolute filePath)
                      then flags{ includePath = includePath flags ++ [dirname filePath]} -- add include so it can be found by its basename
                      else flags
      term <- getTerminal
      -- reusing interpreter compilation entry point
      let resultIO = compileExpression (maybeContents vfs) term flagsEx (fromMaybe initialLoaded modules) (Executable nameExpr ()) program 0 (functionName ++ "()")
      processCompilationResult normUri filePath flagsEx False resultIO
    Nothing -> do
      sendNotification J.SMethod_WindowShowMessage $ J.ShowMessageParams J.MessageType_Error $ "Wait for initial type checking / compilation to finish prior to running a function " <> T.pack filePath
      return Nothing -- TODO: better error message
  where normUri = J.toNormalizedUri uri

-- Recompiles the given file, stores the compilation result in
-- LSM's state and emits diagnostics
recompileFile :: CompileTarget () -> J.Uri -> Maybe J.Int32 -> Bool -> Flags -> LSM (Maybe FilePath)
recompileFile compileTarget uri version force flags = do
  path <- liftIO $ fromLspUri normUri
  case path of
    Just path -> do
      -- Update the virtual file system
      newvfs <- updateVFS
      -- Get the file contents
      let contents = fst <$> maybeContents newvfs path
      modules <- fmap loadedModules <$> getLastChangedFileLoaded (normUri, flags)
      term <- getTerminal
      -- Don't use the cached modules as regular modules (they may be out of date, so we want to resolveImports fully over again)
      let resultIO = do res <- compileFile (maybeContents newvfs) contents term flags (fromMaybe [] modules) compileTarget [] path
                        liftIO $ -- trace ("koka/recompile: " ++ path ++ ", uri: " ++ show uri ++ ", normUri: " ++ show normUri) $
                                 termPhaseDoc term (color (colorInterpreter (colorSchemeFromFlags flags)) (text "done"))
                        return res
      processCompilationResult normUri path flags True resultIO
    Nothing -> return Nothing
  where
    normUri = J.toNormalizedUri uri

-- Processes the result of a compilation, updating the loaded state and emitting diagnostics
-- Returns the executable file path if compilation succeeded
processCompilationResult :: J.NormalizedUri -> FilePath -> Flags -> Bool -> IO (Error Loaded (Loaded, Maybe FilePath)) -> LSM (Maybe FilePath)
processCompilationResult normUri filePath flags update doIO = do
  let ioResult :: IO (Either Exc.SomeException (Error Loaded (Loaded, Maybe FilePath)))
      ioResult = try doIO
  result <- liftIO ioResult
  term <- getTerminal
  case result of
    Left e -> do
      -- Compilation threw an exception, put it in the log, as well as a notification
      liftIO $ termError term $ errorMessageKind ErrBuild rangeNull $ text ("When compiling file " ++ filePath) <-> text "\tcompiler threw exception:" <+> text (show e)
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
          when update $ putLoadedSuccess l normUri flags-- update the loaded state for this file
          -- liftIO $ termDoc term $ color Green $ text "success "
          return outFile -- return the executable file path
        Left (Errors errs, mbMod) -> do
          -- Compilation failed
          case mbMod of
            Nothing ->
              trace ("Error when compiling, no cached modules " ++ show errs) $
              return ()
            Just l -> do
              trace ("Error when compiling have cached" ++ show (map modSourcePath $ loadedModules l)) $ return ()
              when update $ putLoaded l normUri flags
              removeLoaded normUri (loadedModule l)
          liftIO $ mapM_ (termError term) errs
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
      -- Partition them by source (koka, koka-lints, etc.) -- we should only have koka (compiler diagnostics) for now
      let diagsBySrc = M.map partitionBySource diags
      if null diags
        -- If there are no diagnostics clear all koka diagnostics
        then flushDiagnosticsBySource maxDiags (Just diagSrc)
        -- Otherwise report all diagnostics
        else do
          flushDiagnosticsBySource maxDiags (Just diagSrc)
          mapM_ (\(uri, diags) -> publishDiagnostics maxDiags uri Nothing diags) (M.toList diagsBySrc)
      return outFile
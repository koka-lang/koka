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
    rebuildUri
  )
where


import Debug.Trace (trace)

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
import Language.LSP.Server (Handlers, flushDiagnosticsBySource, publishDiagnostics, sendNotification, getVirtualFile, getVirtualFiles, notificationHandler, withProgress, ProgressCancellable (..))
import Language.LSP.VFS (virtualFileText, VFS(..), VirtualFile, file_version, virtualFileVersion)
import Lib.PPrint (text, (<->), (<+>), color, Color (..))


import Common.Name (newName, ModuleName, Name, isQualified, qualify)
import Common.File (FileTime, getCurrentTime, getFileTimeOrCurrent, searchPathsCanonical)
import Common.Error
import Compile.Options( Flags (maxErrors, includePath) )
import Compile.BuildContext
import LanguageServer.Conversions
import LanguageServer.Monad
import qualified Language.LSP.Server as J
import Compile.Build (searchSourceFile)
import Platform.Config (sourceExtension)


-- Compile the file on opening
didOpenHandler :: Handlers LSM
didOpenHandler = notificationHandler J.SMethod_TextDocumentDidOpen $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  rebuildUri Nothing Nothing (J.toNormalizedUri uri)
  return ()

-- Recompile the file on changes
didChangeHandler :: Handlers LSM
didChangeHandler = notificationHandler J.SMethod_TextDocumentDidChange $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  rebuildUri Nothing Nothing (J.toNormalizedUri uri)
  return ()

-- Saving a file just recompiles it
didSaveHandler :: Handlers LSM
didSaveHandler = notificationHandler J.SMethod_TextDocumentDidSave $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  rebuildUri Nothing Nothing (J.toNormalizedUri uri)
  return ()

-- Closing the file
didCloseHandler :: Handlers LSM
didCloseHandler = notificationHandler J.SMethod_TextDocumentDidClose $ \msg -> do
  let uri = msg ^. J.params . J.textDocument . J.uri
  -- removeLoadedUri (J.toNormalizedUri uri)
  -- Don't remove diagnostics so the file stays red in the editor, and problems are shown, but do remove the compilation state
  -- note: don't remove from the roots in the build context
  -- TODO: maybe only keep the diagnostics? May not be worth the trouble?
  return ()


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


rebuildUri :: Maybe Flags -> Maybe Name -> J.NormalizedUri -> LSM (Maybe FilePath)
rebuildUri mbFlags mbRun uri
  = do mbfpath <- liftIO $ fromLspUri uri
       case mbfpath of
         Nothing    -> return Nothing
         Just fpath -> rebuildFile mbFlags mbRun uri fpath

fileNameFromPath :: FilePath -> LSM FilePath
fileNameFromPath fpath = do
  flags <- getFlags
  mbpath <- liftIO $ searchPathsCanonical "" (includePath flags) [sourceExtension,sourceExtension++".md"] [] fpath
  case mbpath of
    Nothing          -> return fpath
    Just (root,stem) -> return stem

rebuildFile :: Maybe Flags -> Maybe Name -> J.NormalizedUri -> FilePath -> LSM (Maybe FilePath)
rebuildFile mbFlags mbRun uri fpath
    = -- trace ("\nkoka: rebuild file: " ++ fpath) $
      do updateVFS
         fpath' <- fileNameFromPath fpath
         withProgress (T.pack $ "Koka: " ++ fpath') Nothing J.NotCancellable $ \report -> do
            setProgress (Just report)
            mbRes <- -- run build with diagnostics
                     liftBuildDiag mbFlags uri $ \buildc0 ->
                     -- get all errors from the returned build context
                     buildcLiftErrors fst $
                     -- we build with a focus so we only build what is needed for file, avoiding rebuilding non-dependencies
                     do (buildc1,[focus]) <- buildcAddRootSources [fpath] buildc0
                        buildcFocus [focus] buildc1 $ \focusMods buildcF ->
                            case mbRun of
                              -- just type check
                              Nothing    -> -- trace ("koka: rebuild: type check " ++ show focus) $
                                            do bc <- buildcTypeCheck [focus] buildcF  -- only force on "open" to build range maps etc.
                                                    -- buildcBuildEx False [focus] [] buildcF
                                               return (bc,Nothing)
                              -- full build and return the executable
                              Just entry -> do let qentry = if isQualified entry then entry else qualify focus entry
                                               (bc,res) <- buildcCompileEntry False qentry buildcF
                                               case res of
                                                  Just (tp, Just (exe,run)) -> return (bc,Just exe)
                                                  _                         -> return (bc,Nothing)
            setProgress Nothing
            case mbRes of
              Just mbPath -> return mbPath
              Nothing     -> return Nothing
            


-- Run a build monad and emit diagnostics if needed.
liftBuildDiag :: Maybe Flags -> J.NormalizedUri -> (BuildContext -> Build (BuildContext,a)) -> LSM (Maybe a)
liftBuildDiag mbflags defaultUri build
  = do flags <- getFlags
       flushDiagnosticsBySource (maxErrors flags) diagSourceKoka
       res <- liftBuildWith mbflags build
       case res of
         Right (x,errs) -> do diagnoseErrors defaultUri (errors errs)
                              return (Just x)
         Left errs      -> do diagnoseErrors defaultUri (errors errs)
                              return Nothing

-- A build retains all errors over all loaded modules, so we can always publish all
diagnoseErrors :: J.NormalizedUri -> [ErrorMessage] -> LSM ()
diagnoseErrors defaultUri errs
  = -- trace ("koka: diagnose errors: " ++ show errs) $
    do flags <- getFlags
       let diagSource = diagSourceKoka
           maxDiags   = maxErrors flags
           diagss     = M.toList $ M.map partitionBySource $ M.fromListWith (++) $  -- group all errors per file uri
                        map (errorMessageToDiagnostic diagSource defaultUri) errs
       -- flushDiagnosticsBySource maxDiags diagSource
       mapM_ (\(uri, diags) -> if null diags then return ()
                                else do mversion <- getVirtualFileVersion uri
                                        publishDiagnostics maxDiags uri mversion diags) diagss

diagSourceKoka :: Maybe T.Text
diagSourceKoka
  = Just (T.pack "koka")

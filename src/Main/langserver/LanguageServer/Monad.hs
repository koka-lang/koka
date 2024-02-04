------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting, Fredrik Wieczerkowski
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The language server's monad that holds state (e.g. loaded/compiled modules)
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Monad
  ( LSState (..),
    InlayHintOptions(..),
    SignatureContext(..),clearSignatureContext,updateSignatureContext,getSignatureContext,
    defaultLSState,
    newLSStateVar,
    LSM,
    getLastChangedFileLoaded,
    getTerminal,getFlags,getColorScheme,getHtmlPrinter,
    getLSState,modifyLSState,
    getLoaded,putLoaded,putLoadedSuccess,removeLoaded,removeLoadedUri,
    getLoadedModule,getLoadedSuccess,getLoadedLatest,
    getModules,
    updateConfig,
    getInlayHintOptions,
    getDiagnostics,putDiagnostics,clearDiagnostics,
    runLSM,
    getProgress,setProgress, maybeContents,
    liftBuild
  )
where

import Debug.Trace(trace)
import GHC.Conc (atomically)
import GHC.Base (Alternative (..))
import Platform.Var (newVar, takeVar)
import Platform.Filetime (FileTime)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift, liftIO)
import Control.Concurrent.Chan (readChan)
import Control.Concurrent.STM (newTVarIO, TVar)
import qualified Data.Set as Set
import Control.Concurrent.STM.TChan
import qualified Data.Aeson as A
import qualified Data.ByteString as D
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isJust)
import Data.List (find)
import Data.Aeson.Types
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT, sendNotification, Handlers)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Server as J

import Common.ColorScheme ( colorSource, ColorScheme, darkColorScheme, lightColorScheme )
import Common.Name (nameNil, Name, readQualifiedName)
import Common.File ( realPath, normalize, getCwd, realPath, normalize, getCurrentTime )
import Common.Error
import Lib.PPrint (Pretty(..), asString, writePrettyLn,  Doc, writePretty, writePrettyW, (<->), text)
import Lib.Printer (withColorPrinter, withColor, writeLn, ansiDefault, AnsiStringPrinter (AnsiString), Color (Red), ColorPrinter (PAnsiString, PHtmlText), withHtmlTextPrinter, HtmlTextPrinter (..))
import Type.Pretty (ppType, defaultEnv, Env (context, importsMap), ppScheme)
import Kind.ImportMap (importsEmpty)
import Compile.Options (Flags (..), prettyEnvFromFlags, verbose)
import Compile.BuildContext
import LanguageServer.Conversions ({-toLspUri,-} fromLspUri)

import Data.Map.Strict(Map)
import Data.ByteString (ByteString)
import Compiler.Compile (Terminal (..), Loaded (..), Module (..))
import Compiler.Module (Modules)

-- The language server's state, e.g. holding loaded/compiled modules.
data LSState = LSState {
  buildContext :: BuildContext,

  lsModules :: ![Module],
  lsLoaded :: !(M.Map J.NormalizedUri (Loaded, FileTime)),
  lsLoadedSuccess :: !(M.Map J.NormalizedUri (Loaded, FileTime)),

  messages :: !(TChan (String, J.MessageType)),
  progress :: !(TChan String),
  flags:: !Flags,
  terminal:: !Terminal,
  progressReport :: !(Maybe (J.ProgressAmount -> LSM ())),
  htmlPrinter :: Doc -> IO T.Text,
  pendingRequests :: !(TVar (Set.Set J.SomeLspId)),
  cancelledRequests :: !(TVar (Set.Set J.SomeLspId)),
  documentVersions :: !(TVar (M.Map J.NormalizedUri J.Int32)),
  documentInfos :: !(M.Map J.NormalizedUri (D.ByteString, FileTime, J.Int32)),
  -- If the file was changed last, we can reuse modules, since no dependencies have changed
  lastChangedFile :: Maybe (J.NormalizedUri, Flags, Loaded),
  diagnostics :: !(M.Map J.NormalizedUri [J.Diagnostic]),
  signatureContext :: !(Maybe SignatureContext),
  config :: Config
}

data SignatureContext = SignatureContext {
  sigFunctionName:: Name
}

instance FromJSON SignatureContext where
  parseJSON (A.Object v) = SignatureContext <$> readQualifiedName <$> v .: "sigFunctionName"
  parseJSON _ = return $ SignatureContext nameNil

instance ToJSON SignatureContext where
  toJSON (SignatureContext name) = object ["sigFunctionName" .= (show name)]

-- The monad holding (thread-safe) state used by the language server.
type LSM = LspT () (ReaderT (MVar LSState) IO)

-- Runs the language server's state monad.
runLSM :: LSM a -> MVar LSState -> LanguageContextEnv () -> IO a
runLSM lsm stVar cfg = runReaderT (runLspT cfg lsm) stVar

newLSStateVar :: Flags -> IO (MVar LSState)
newLSStateVar flags = defaultLSState flags >>= newMVar

-- Fetches the language server's state inside the LSM monad
getLSState :: LSM LSState
getLSState = do
  stVar <- lift ask
  liftIO $ readMVar stVar

-- Updates the language server's state inside the LSM monad
modifyLSState :: (LSState -> LSState) -> LSM ()
modifyLSState m = do
  stVar <- lift ask
  liftIO $ modifyMVar stVar $ \s -> return (m s, ())

defaultLSState :: Flags -> IO LSState
defaultLSState flags = do
  msgChan <- atomically newTChan :: IO (TChan (String, J.MessageType))
  progressChan <- atomically newTChan :: IO (TChan String)
  pendingRequests <- newTVarIO Set.empty
  cancelledRequests <- newTVarIO Set.empty
  fileVersions <- newTVarIO M.empty
  let withNewPrinter f = do
        ansiConsole <- newVar ansiDefault
        stringVar <- newVar ""
        let p = AnsiString ansiConsole stringVar
        tp <- (f . PAnsiString) p
        ansiString <- takeVar stringVar
        atomically $ writeTChan msgChan (ansiString, tp)
  let withNewProgressPrinter doc = do
        atomically $ writeTChan progressChan (show doc)
  cwd <- getCwd
  let cscheme = colorScheme flags
      prettyEnv flags ctx imports = (prettyEnvFromFlags flags){ context = ctx, importsMap = imports }
      term = Terminal (\err -> withNewPrinter $ \p -> do putErrorMessage p cwd (showSpan flags) cscheme err; return J.MessageType_Error)
                (if verbose flags > 1 then (\msg -> withNewPrinter $ \p -> do withColor p (colorSource cscheme) (writeLn p msg); return J.MessageType_Info)
                                         else (\_ -> return ()))
                 (if verbose flags > 0 then (\msg -> do
                    _ <- withNewPrinter $ \p -> do
                      writePrettyLn p msg
                      return J.MessageType_Info
                    withNewProgressPrinter msg
                    )
                  else (\_ -> return ()))
                 (\msg -> withNewPrinter $ \p -> do
                    writePrettyLn p msg
                    return J.MessageType_Info
                )
  return LSState {
    lsLoaded = M.empty, lsLoadedSuccess = M.empty, lsModules=[],
    buildContext = buildcEmpty flags,
    messages = msgChan, progress=progressChan, pendingRequests=pendingRequests, cancelledRequests=cancelledRequests,
    config = Config{colors=Colors{mode="dark"}, inlayHintOpts=InlayHintOptions{showImplicitArguments=True, showInferredTypes=True, showFullQualifiers=True}},
    terminal = term, htmlPrinter = htmlTextColorPrinter, flags = flags,
    lastChangedFile = Nothing, progressReport = Nothing,
    documentInfos = M.empty, documentVersions = fileVersions,
    signatureContext = Nothing,
    diagnostics = M.empty}

-- Prints a message to html spans
htmlTextColorPrinter :: Doc -> IO T.Text
htmlTextColorPrinter doc
  = do
    stringVar <- newVar (T.pack "")
    let printer = PHtmlText (HtmlTextPrinter stringVar)
    -- writePretty printer doc
    writePrettyW printer 1000 doc
    takeVar stringVar

putScheme p env tp
  = writePrettyLn p (ppScheme env tp)

putErrorMessage p cwd endToo cscheme err
  = writePrettyLn p (ppErrorMessage cwd endToo cscheme err)

data Config = Config {
  colors :: Colors,
  inlayHintOpts :: InlayHintOptions
}
data Colors = Colors {
  mode :: String
}

data InlayHintOptions  = InlayHintOptions {
  showImplicitArguments :: Bool,
  showInferredTypes :: Bool,
  showFullQualifiers :: Bool
}

instance FromJSON Colors where
  parseJSON (A.Object v) = Colors <$> v .: "mode"
  parseJSON _ = empty

instance FromJSON Config where
  parseJSON (A.Object v) = Config <$> v .: "colors" <*> v .: "inlayHints"
  parseJSON _ = empty

instance FromJSON InlayHintOptions where
  parseJSON (A.Object v) = InlayHintOptions <$> v .: "showImplicitArguments" <*> v .: "showInferredTypes" <*> v .: "showFullQualifiers"
  parseJSON _ = empty

setProgress :: Maybe (J.ProgressAmount -> LSM ()) -> LSM ()
setProgress report = do
  modifyLSState $ \s -> s {progressReport = report}

getProgress :: LSM (Maybe (J.ProgressAmount -> LSM ()))
getProgress = progressReport <$> getLSState

updateConfig :: A.Value -> LSM ()
updateConfig cfg =
  case fromJSON cfg of
    A.Success cfg -> do
      modifyLSState $ \s ->
        let s' = s{config=cfg} in
        if mode (colors cfg) == "dark" then
          s'{flags=(flags s'){colorScheme=darkColorScheme}}
        else
          s'{flags=(flags s'){colorScheme=lightColorScheme}}
    _ -> return ()

updateSignatureContext :: SignatureContext -> LSM ()
updateSignatureContext context =
  modifyLSState $ \s -> s{signatureContext=Just context}

clearSignatureContext :: LSM ()
clearSignatureContext =
  modifyLSState $ \s -> s{signatureContext=Nothing}

getSignatureContext :: LSM (Maybe SignatureContext)
getSignatureContext = signatureContext <$> getLSState

getInlayHintOptions :: LSM InlayHintOptions
getInlayHintOptions = inlayHintOpts . config <$> getLSState

getLastChangedFileLoaded :: (J.NormalizedUri, Flags) -> LSM (Maybe Loaded)
getLastChangedFileLoaded (path, flags) = do
  st <- lastChangedFile<$> getLSState
  case st of
    Nothing -> return Nothing
    Just (path', flags', loaded) -> do
      if path == path' && flags == flags' then
        return $ Just loaded
      else
        return Nothing

-- Fetches the terminal used for printing messages
getTerminal :: LSM Terminal
getTerminal = terminal <$> getLSState

-- Fetches the loaded state holding compiled modules
getFlags :: LSM Flags
getFlags = flags <$> getLSState

-- Fetches the html printer used for printing markdown compatible text
getHtmlPrinter :: LSM (Doc -> IO T.Text)
getHtmlPrinter = htmlPrinter <$> getLSState

-- Fetches the color scheme used for coloring markdown compatible text
getColorScheme :: LSM ColorScheme
getColorScheme = colorScheme <$> getFlags


-- Diagnostics
getDiagnostics :: LSM (M.Map J.NormalizedUri [J.Diagnostic])
getDiagnostics = diagnostics <$> getLSState

-- Clear diagnostics for a file
clearDiagnostics :: J.NormalizedUri -> LSM ()
clearDiagnostics uri = modifyLSState $ \s -> s {diagnostics = M.delete uri (diagnostics s)}

putDiagnostics :: M.Map J.NormalizedUri [J.Diagnostic] -> LSM ()
putDiagnostics diags = -- Left biased union prefers more recent diagnostics
  modifyLSState $ \s -> s {diagnostics = M.union diags (diagnostics s)}


-- Fetches all the most recent succesfully compiled modules (for incremental compilation)
getModules :: LSM Modules
getModules = lsModules <$> getLSState

mergeModules :: Modules -> Modules -> Modules
mergeModules newModules oldModules =
  let nModValid = filter modCompiled newModules -- only add modules that sucessfully compiled
      newModNames = map modName nModValid
  in nModValid ++ filter (\m -> modName m `notElem` newModNames) oldModules

-- Replaces the loaded state holding compiled modules
putLoaded :: Loaded -> J.NormalizedUri -> Flags -> LSM ()
putLoaded l fpathUri flags = do
  -- fpath <- liftIO $ realPath (modSourcePath $ loadedModule l)
  time <- liftIO getCurrentTime
  modifyLSState $ \s -> s {
    lastChangedFile = Just (fpathUri, flags, l),
    lsModules = mergeModules (loadedModule l:loadedModules l) (lsModules s),
    lsLoaded = M.insert {-(toLspUri fpath)-} fpathUri (l, time) (lsLoaded s)
    }

putLoadedSuccess :: Loaded -> J.NormalizedUri -> Flags -> LSM ()
putLoadedSuccess l fpathUri flags = do
  -- fpath <- liftIO $ realPath (modSourcePath $ loadedModule l)
  time <- liftIO getCurrentTime
  -- trace ("putLoadedSuccess: fpathUri: " ++ show fpathUri) $
  modifyLSState $ \s -> s {
    lastChangedFile = Just (fpathUri, flags, l),
    lsModules = mergeModules (loadedModule l:loadedModules l) (lsModules s),
    lsLoaded = M.insert {-(toLspUri fpath)-} fpathUri (l,time) (lsLoaded s),
    lsLoadedSuccess = M.insert {-(toLspUri fpath)-} fpathUri (l,time) (lsLoaded s)
    }

removeLoaded :: J.NormalizedUri -> Module -> LSM ()
removeLoaded fpathUri m = do
  -- fpath <- liftIO $ realPath (modSourcePath m)
  modifyLSState $ \s -> s {lsModules = filter (\m1 -> modName m1 /= modName m) (lsModules s),
                           lsLoaded = M.delete fpathUri {-(toLspUri fpath)-} (lsLoaded s)}

getLoadedModule :: J.NormalizedUri -> Maybe Loaded -> LSM (Maybe Module)
getLoadedModule uri loaded = do
  liftIO $ loadedModuleFromUri loaded uri

loadedModuleFromUri :: Maybe Loaded -> J.NormalizedUri -> IO (Maybe Module)
loadedModuleFromUri ml uri = do
  fpath <- liftIO $ fromLspUri uri
  return $ do
    l <- ml
    fpath <- fpath
    find (\m -> fpath == modSourcePath m) $ loadedModules l

-- Removes a loaded module from the loaded state holding compiled modules
removeLoadedUri :: J.NormalizedUri -> LSM ()
removeLoadedUri uri = do
  modifyLSState (\st -> st {lsLoaded = M.delete uri (lsLoaded st)})

-- Fetches the loaded state holding compiled modules
getLoadedLatest :: J.NormalizedUri -> LSM (Maybe Loaded)
getLoadedLatest uri
  = -- trace ("getLoadedLatest: " ++ show uri) $
    do ld <- M.lookup uri . lsLoaded <$> getLSState
       return $ fst <$> ld

-- Fetches the loaded state holding compiled modules, with a flag indicating if it is the latest version
getLoaded :: J.NormalizedUri -> LSM (Maybe (Loaded, Bool))
getLoaded uri = do
  lastSuccess <- M.lookup uri . lsLoadedSuccess <$> getLSState
  success <- M.lookup uri . lsLoaded <$> getLSState
  case (lastSuccess, success) of
    (Just (l1, t1), Just (l2, t2)) -> return $ Just (l1, t1 == t2)
    _ -> return Nothing

-- Fetches the last successful loaded state
getLoadedSuccess :: J.NormalizedUri -> LSM (Maybe Loaded)
getLoadedSuccess uri = do
  lastSuccess <- M.lookup uri . lsLoadedSuccess <$> getLSState
  return $ fst <$> lastSuccess

-- Retreives a file from the virtual file system, returning the contents and the last modified time
maybeContents :: Map J.NormalizedUri (ByteString, FileTime, J.Int32) -> FilePath -> Maybe (ByteString, FileTime)
maybeContents vfs path = do
  -- trace ("Maybe contents " ++ show uri ++ " " ++ show (M.keys vfs)) $ return ()
  let uri = J.toNormalizedUri $ J.filePathToUri path
  (text, ftime, vers) <- M.lookup uri vfs
  return (text, ftime)

-- Run a build monad with current terminal, flags, and virtual file system
liftBuild :: (BuildContext -> Build (BuildContext,a)) -> LSM (Either Errors (a,Errors))
liftBuild action
  = do ls <- getLSState
       let vfs = VFS (\fpath -> maybeContents (documentInfos ls) fpath)
       res <- liftIO $ runBuild (terminal ls) (flags ls) $ withVFS vfs $ action (buildContext ls)
       case res of
         Left errs               -> return (Left errs)
         Right ((buildc,x),errs) -> do modifyLSState (\ls -> ls{ buildContext = buildc })
                                       return (Right (x,errs))


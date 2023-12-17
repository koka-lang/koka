-----------------------------------------------------------------------------
-- The language server's monad that holds state (e.g. loaded/compiled modules)
-----------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Monad
  ( LSState (..),
    defaultLSState,
    newLSStateVar,
    LSM,
    getLastChangedFileLoaded,
    getTerminal,getFlags,getColorScheme,getHtmlPrinter,
    getLSState,modifyLSState,
    getLoaded,putLoaded,removeLoaded,removeLoadedUri,getLoadedModule,
    getModules,
    updateConfig,
    getDiagnostics,putDiagnostics,clearDiagnostics,
    runLSM,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, putMVar, readMVar, newEmptyMVar)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift, liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT, sendNotification, Handlers)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J

import Compiler.Compile (Terminal (..), Loaded (..), Module (..))
import Lib.PPrint (Pretty(..), asString, writePrettyLn, Doc)
import Control.Concurrent.Chan (readChan)
import Type.Pretty (ppType, defaultEnv, Env (context, importsMap), ppScheme)
import qualified Language.LSP.Server as J
import GHC.Base (Type, Alternative (..))
import Lib.Printer (withColorPrinter, withColor, writeLn, ansiDefault, AnsiStringPrinter (AnsiString), Color (Red), ColorPrinter (PAnsiString, PHtmlText), withHtmlTextPrinter, HtmlTextPrinter (..))
import Compiler.Options (Flags (..), prettyEnvFromFlags, verbose)
import Common.Error (ppErrorMessage)
import Common.ColorScheme (colorSource, ColorScheme)
import Common.Name (nameNil)
import Kind.ImportMap (importsEmpty)
import Platform.Var (newVar, takeVar)
import Debug.Trace (trace)

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent
import GHC.Conc (atomically)
import Control.Concurrent.STM (newTVarIO, TVar)
import qualified Data.Set as Set
import Control.Concurrent.STM.TMVar (TMVar)
import qualified Data.ByteString as D
import Platform.Filetime (FileTime)
import Common.File (realPath,normalize)
import Compiler.Module (Modules)
import Data.Maybe (fromMaybe, isJust)
import Data.List (find)
import qualified Data.Aeson as A
import Data.Aeson.Types
import Common.ColorScheme (darkColorScheme, lightColorScheme)

-- The language server's state, e.g. holding loaded/compiled modules.
data LSState = LSState {
  lsModules :: ![Module],
  lsLoaded :: !(M.Map FilePath Loaded),
  messages :: !(TChan (String, J.MessageType)),
  flags:: !Flags,
  terminal:: !Terminal,
  htmlPrinter :: Doc -> IO T.Text,
  pendingRequests :: !(TVar (Set.Set J.SomeLspId)),
  cancelledRequests :: !(TVar (Set.Set J.SomeLspId)),
  documentVersions :: !(TVar (M.Map J.Uri J.Int32)),
  documentInfos :: !(M.Map FilePath (D.ByteString, FileTime, J.Int32)),
  -- If the file was changed last, we can reuse modules, since no dependencies have changed
  lastChangedFile :: Maybe (FilePath, Flags, Loaded), 
  diagnostics :: !(M.Map J.NormalizedUri [J.Diagnostic]),
  config :: Config
}

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
  pendingRequests <- newTVarIO Set.empty
  cancelledRequests <- newTVarIO Set.empty
  fileVersions <- newTVarIO M.empty
  -- Trim trailing whitespace and newlines from the end of a string
  let trimnl :: [Char] -> [Char]
      trimnl str = reverse $ dropWhile (`T.elem` "\n\r\t ") $ reverse str
  let withNewPrinter f = do
        ansiConsole <- newVar ansiDefault
        stringVar <- newVar ""
        let p = AnsiString ansiConsole stringVar
        tp <- (f . PAnsiString) p
        ansiString <- takeVar stringVar
        atomically $ writeTChan msgChan (trimnl ansiString, tp)
  let cscheme = colorScheme flags
      prettyEnv flags ctx imports = (prettyEnvFromFlags flags){ context = ctx, importsMap = imports }
      term = Terminal (\err -> withNewPrinter $ \p -> do putErrorMessage p (showSpan flags) cscheme err; return J.MessageType_Error)
                (if verbose flags > 1 then (\msg -> withNewPrinter $ \p -> do withColor p (colorSource cscheme) (writeLn p msg); return J.MessageType_Info)
                                         else (\_ -> return ()))
                 (if verbose flags > 0 then (\msg -> withNewPrinter $ \p -> do writePrettyLn p msg; return J.MessageType_Info) else (\_ -> return ()))
                 (\tp -> withNewPrinter $ \p -> do putScheme p (prettyEnv flags nameNil importsEmpty) tp; return J.MessageType_Info)
                 (\msg -> withNewPrinter $ \p -> do writePrettyLn p msg; return J.MessageType_Info)
  return LSState {
    lsLoaded = M.empty, lsModules=[], 
    messages = msgChan, pendingRequests=pendingRequests, cancelledRequests=cancelledRequests, config=Config{colors=Colors{mode="dark"}},
    terminal = term, htmlPrinter = htmlTextColorPrinter, flags = flags, 
    lastChangedFile = Nothing,
    documentInfos = M.empty, documentVersions = fileVersions, diagnostics = M.empty}

htmlTextColorPrinter :: Doc -> IO T.Text
htmlTextColorPrinter doc
  = do
    stringVar <- newVar (T.pack "")
    let printer = PHtmlText (HtmlTextPrinter stringVar)
    writePrettyLn printer doc
    takeVar stringVar

putScheme p env tp
  = writePrettyLn p (ppScheme env tp)

putErrorMessage p endToo cscheme err
  = writePrettyLn p (ppErrorMessage endToo cscheme err)

data Config = Config {
  colors :: Colors
}
data Colors = Colors {
  mode :: String
}

instance FromJSON Colors where
  parseJSON (A.Object v) = Colors <$> v .: "mode"
  parseJSON _ = empty

instance FromJSON Config where
  parseJSON (A.Object v) = Config <$> v .: "colors"
  parseJSON _ = empty

updateConfig :: A.Value -> LSM ()
updateConfig cfg =
  case fromJSON cfg of
    A.Success cfg -> do 
      modifyLSState $ \s -> 
        let s' = s{config=cfg} in
        if mode (colors cfg) == "dark" then
          trace "setting color scheme to dark" $
            s'{flags=(flags s'){colorScheme=darkColorScheme}}
        else
          trace "setting color scheme to light" $
            s'{flags=(flags s'){colorScheme=lightColorScheme}}

getLastChangedFileLoaded :: (FilePath, Flags) -> LSM (Maybe Loaded)
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
putLoaded :: Loaded -> FilePath -> Flags -> LSM ()
putLoaded l f flags = 
  modifyLSState $ \s -> s {lastChangedFile = Just (f, flags, l), lsModules = mergeModules (loadedModule l:loadedModules l) (lsModules s), lsLoaded = M.insert (modSourcePath $ loadedModule l) l (lsLoaded s)}

removeLoaded :: Module -> LSM ()
removeLoaded m = modifyLSState $ \s -> s {lsModules = filter (\m1 -> modName m1 /= modName m) (lsModules s), lsLoaded = M.delete (modSourcePath m) (lsLoaded s)}

getLoadedModule :: J.Uri -> LSM (Maybe Module)
getLoadedModule uri = do
  lmaybe <- getLoaded uri
  liftIO $ loadedModuleFromUri lmaybe uri

loadedModuleFromUri :: Maybe Loaded -> J.Uri -> IO (Maybe Module)
loadedModuleFromUri l uri = 
  case l of
    Nothing -> return Nothing
    Just l -> 
      case J.uriToFilePath uri of 
        Nothing -> return Nothing
        Just uri -> do
          path <- realPath uri
          let p = normalize path
          return $ find (\m -> p == modSourcePath m) $ loadedModules l

-- Removes a loaded module from the loaded state holding compiled modules
removeLoadedUri :: J.Uri -> LSM ()
removeLoadedUri uri = do
  case J.uriToFilePath uri of
    Nothing -> return ()
    Just path -> do
      path0 <- liftIO $ realPath path
      let path = normalize path0
      modifyLSState (\st -> st {lsLoaded = M.delete path (lsLoaded st)})

-- Fetches the loaded state holding compiled modules
getLoaded :: J.Uri -> LSM (Maybe Loaded)
getLoaded uri = do
  st <- getLSState
  case J.uriToFilePath uri of 
    Nothing -> return Nothing
    Just uri -> do
      path <- liftIO $ realPath uri
      let p = normalize path
      return $ M.lookup p (lsLoaded st)
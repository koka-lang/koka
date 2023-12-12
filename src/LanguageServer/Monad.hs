-----------------------------------------------------------------------------
-- The language server's monad that holds state (e.g. loaded/compiled modules)
-----------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module LanguageServer.Monad
  ( LSState (..),
    defaultLSState,
    newLSStateVar,
    LSM,
    getLSState,
    getTerminal,
    getFlags,
    putLSState,
    modifyLSState,
    getLoaded,
    putLoaded,removeLoaded,
    getLoadedModule,
    getModules,
    getColorScheme,
    getHtmlPrinter,
    getDiagnostics,
    putDiagnostics,
    clearDiagnostics,
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
import GHC.Base (Type)
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
import LanguageServer.Conversions (loadedModuleFromUri)
import qualified Data.ByteString as D
import Platform.Filetime (FileTime)
import Common.File (realPath,normalize)
import Compiler.Module (Modules)
import Data.Maybe (fromMaybe)

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
  diagnostics :: !(M.Map J.NormalizedUri [J.Diagnostic])
}

trimnl :: [Char] -> [Char]
trimnl str = reverse $ dropWhile (`elem` "\n\r\t ") $ reverse str

htmlTextColorPrinter :: Doc -> IO T.Text
htmlTextColorPrinter doc
  = do
    stringVar <- newVar (T.pack "")
    let printer = PHtmlText (HtmlTextPrinter stringVar)
    writePrettyLn printer doc
    takeVar stringVar

defaultLSState :: Flags -> IO LSState
defaultLSState flags = do
  msgChan <- atomically newTChan :: IO (TChan (String, J.MessageType))
  pendingRequests <- newTVarIO Set.empty
  cancelledRequests <- newTVarIO Set.empty
  fileVersions <- newTVarIO M.empty
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
  return LSState {lsLoaded = M.empty,lsModules=[], messages = msgChan, terminal = term, htmlPrinter = htmlTextColorPrinter, flags = flags, pendingRequests=pendingRequests, cancelledRequests=cancelledRequests, documentInfos = M.empty, documentVersions = fileVersions, diagnostics = M.empty}

putScheme p env tp
  = writePrettyLn p (ppScheme env tp)

putErrorMessage p endToo cscheme err
  = writePrettyLn p (ppErrorMessage endToo cscheme err)

newLSStateVar :: Flags -> IO (MVar LSState)
newLSStateVar flags = defaultLSState flags >>= newMVar

-- The monad holding (thread-safe) state used by the language server.
type LSM = LspT () (ReaderT (MVar LSState) IO)

-- Fetches the language server's state inside the LSM monad
getLSState :: LSM LSState
getLSState = do
  stVar <- lift ask
  liftIO $ readMVar stVar

-- Replaces the language server's state inside the LSM monad
putLSState :: LSState -> LSM ()
putLSState s = do
  stVar <- lift ask
  liftIO $ putMVar stVar s

-- Updates the language server's state inside the LSM monad
modifyLSState :: (LSState -> LSState) -> LSM ()
modifyLSState m = do
  stVar <- lift ask
  liftIO $ modifyMVar stVar $ \s -> return (m s, ())

getModules :: LSM Modules
getModules = lsModules <$> getLSState

putDiagnostics :: M.Map J.NormalizedUri [J.Diagnostic] -> LSM ()
putDiagnostics diags = -- Left biased union prefers more recent diagnostics
  modifyLSState $ \s -> s {diagnostics = M.union diags (diagnostics s)}

getDiagnostics :: LSM (M.Map J.NormalizedUri [J.Diagnostic])
getDiagnostics = diagnostics <$> getLSState

clearDiagnostics :: J.NormalizedUri -> LSM ()
clearDiagnostics uri = modifyLSState $ \s -> s {diagnostics = M.delete uri (diagnostics s)}

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

-- Fetches the loaded state holding compiled modules
getFlags :: LSM Flags
getFlags = flags <$> getLSState

getHtmlPrinter :: LSM (Doc -> IO T.Text)
getHtmlPrinter = htmlPrinter <$> getLSState

getColorScheme :: LSM ColorScheme
getColorScheme = colorScheme <$> getFlags

-- Replaces the loaded state holding compiled modules
putLoaded :: Loaded -> LSM ()
putLoaded l = modifyLSState $ \s -> s {lsModules = mergeModules (loadedModule l:loadedModules l) (lsModules s), lsLoaded = M.insert (modSourcePath $ loadedModule l) l (lsLoaded s)}

removeLoaded :: Module -> LSM ()
removeLoaded m = modifyLSState $ \s -> s {lsModules = filter (\m1 -> modName m1 /= modName m) (lsModules s), lsLoaded = M.delete (modSourcePath m) (lsLoaded s)}

getLoadedModule :: J.Uri -> LSM (Maybe Module)
getLoadedModule uri = do
  lmaybe <- getLoaded uri
  liftIO $ loadedModuleFromUri lmaybe uri

-- Runs the language server's state monad.
runLSM :: LSM a -> MVar LSState -> LanguageContextEnv () -> IO a
runLSM lsm stVar cfg = runReaderT (runLspT cfg lsm) stVar

getTerminal :: LSM Terminal
getTerminal = terminal <$> getLSState

mergeModules :: Modules -> Modules -> Modules
mergeModules newModules oldModules =
  let nModValid = filter modCompiled newModules -- only add modules that sucessfully compiled
      newModNames = map modName nModValid
  in nModValid ++ filter (\m -> modName m `notElem` newModNames) oldModules

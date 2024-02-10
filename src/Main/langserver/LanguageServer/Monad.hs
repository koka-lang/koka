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
    getTerminal,getFlags,getColorScheme,getHtmlPrinter,
    getLSState,modifyLSState,
    updateConfig,
    getInlayHintOptions,
    runLSM,
    getProgress,setProgress, maybeContents,

    liftBuild, liftBuildWith,
    lookupModuleName, lookupRangeMap, lookupProgram, lookupLexemes,
    lookupDefinitions, lookupVisibleDefinitions, Definitions(..),
    lookupModulePaths,
    getPrettyEnv, getPrettyEnvFor, prettyMarkdown,
    emitInfo, emitNotification, getVirtualFileVersion

  )
where

import Debug.Trace(trace)
import GHC.Conc (atomically)
import GHC.Base (Alternative (..), when)
import Platform.Var (newVar, takeVar)
import Platform.Filetime (FileTime)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
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
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.List (find)
import Data.Aeson.Types
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT, sendNotification, Handlers)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Server as J

import Lib.PPrint hiding (empty)
import qualified Lib.PPrint as PP
import Lib.Printer
import Common.ColorScheme ( colorSource, ColorScheme, darkColorScheme, lightColorScheme )
import Common.Name (nameNil, Name, readQualifiedName, ModuleName)
import Common.File ( realPath, normalize, getCwd, realPath, normalize, getCurrentTime )
import Common.Error

import Syntax.Syntax( UserProgram )
import Syntax.RangeMap( RangeMap )
import Syntax.Lexeme( Lexeme )
import Kind.ImportMap (importsEmpty)
import qualified Type.Pretty as TP
import Compile.Options (Flags (..), prettyEnvFromFlags, verbose, Terminal(..))
import Compile.BuildContext
import LanguageServer.Conversions ({-toLspUri,-} fromLspUri)

import Data.Map.Strict(Map)
import Data.ByteString (ByteString)
import GHC.IO.Encoding (BufferCodec(getState))
import Numeric

-- The language server's state, e.g. holding loaded/compiled modules.
data LSState = LSState {
  buildContext      :: !BuildContext,

  messages          :: !(TChan (String, J.MessageType)),
  progress          :: !(TChan (Double, String)),
  flags             :: !Flags,
  terminal          :: !Terminal,
  progressReport    :: !(Maybe (J.ProgressAmount -> LSM ())),
  htmlPrinter       :: !(Doc -> IO T.Text),

  pendingRequests   :: !(TVar (Set.Set J.SomeLspId)),
  cancelledRequests :: !(TVar (Set.Set J.SomeLspId)),
  documentVersions  :: !(TVar (M.Map J.NormalizedUri J.Int32)),
  documentInfos     :: !(M.Map J.NormalizedUri (D.ByteString, FileTime, J.Int32)),
  signatureContext  :: !(Maybe SignatureContext),
  config            :: !Config
}

data SignatureContext = SignatureContext {
  sigFunctionName :: !Name
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
modifyLSState f = do
  stVar <- lift ask
  liftIO $ modifyMVar_ stVar (return . f)

defaultLSState :: Flags -> IO LSState
defaultLSState flags = do
  msgChan <- atomically newTChan :: IO (TChan (String, J.MessageType))
  progressChan <- atomically newTChan :: IO (TChan (Double, String))
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
  let withNewProgressPrinter percent mbdoc = do
        let perc = 100 * percent
        atomically $ writeTChan progressChan
          (perc, show (fill 7 (text ((showFFloat (Just 2) perc) "%")) <.>
                       case mbdoc of
                         Just doc -> space <+> doc
                         Nothing  -> space)
          )
  cwd <- getCwd
  let cscheme = colorScheme flags
      prettyEnv flags ctx imports = (prettyEnvFromFlags flags){ TP.context = ctx, TP.importsMap = imports }
      term = Terminal (\err -> withNewPrinter $ \p -> do putErrorMessage p cwd (showSpan flags) cscheme err; return J.MessageType_Error)
                (if verbose flags > 1 then (\msg -> withNewPrinter $ \p -> do withColor p (colorSource cscheme) (writeLn p msg); return J.MessageType_Info)
                                         else (\_ -> return ()))
                 (\(percent, mbdoc) -> withNewProgressPrinter percent mbdoc)
                 (if verbose flags > 0 then (\msg ->
                    withNewPrinter $ \p -> do
                      writePrettyLn p msg
                      return J.MessageType_Info
                    )
                  else (\_ -> return ()))
                 (\msg -> withNewPrinter $ \p -> do
                    writePrettyLn p msg
                    return J.MessageType_Info
                )
  return LSState {
    buildContext = buildcEmpty flags,
    messages = msgChan, progress=progressChan, pendingRequests=pendingRequests, cancelledRequests=cancelledRequests,
    terminal = term, htmlPrinter = htmlTextColorPrinter, flags = flags,
    documentInfos = M.empty, documentVersions = fileVersions,
    signatureContext = Nothing, progressReport = Nothing,
    config = Config{
        colors=Colors{mode="dark"},
        inlayHintOpts=InlayHintOptions{
                         showImplicitArguments=True,
                         showInferredTypes=True,
                         showFullQualifiers=True
        }
    }
  }

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
  = writePrettyLn p (TP.ppScheme env tp)

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

getVirtualFileVersion :: J.NormalizedUri -> LSM (Maybe J.Int32)
getVirtualFileVersion uri
  = do ls <- getLSState
       case M.lookup uri (documentInfos ls) of
         Just (_,_,ver) -> return (Just ver)
         _              -> return Nothing


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



-- Retreives a file from the virtual file system, returning the contents and the last modified time
maybeContents :: Map J.NormalizedUri (ByteString, FileTime, J.Int32) -> FilePath -> Maybe (ByteString, FileTime)
maybeContents vfs path = do
  -- trace ("Maybe contents " ++ show uri ++ " " ++ show (M.keys vfs)) $ return ()
  let uri = J.toNormalizedUri $ J.filePathToUri path
  (text, ftime, vers) <- M.lookup uri vfs
  return (text, ftime)


-- Run a build with optionally temporarily changed flags (this restores the original build context afterwards)
-- (when a build context is validated, it checks itself against the current flags and rebuilds accordingly)
liftBuildWith :: Maybe Flags -> (BuildContext -> Build (BuildContext,a)) -> LSM (Either Errors (a,Errors))
liftBuildWith mbFlags action
  = do ls <- getLSState
       let vfs  = VFS (\fpath -> maybeContents (documentInfos ls) fpath)
           flgs = case mbFlags of
                    Nothing    -> flags ls
                    Just flags -> flags
       res <- seq flgs $ seq VFS $ liftIO $ runBuild (terminal ls) flgs $ withVFS vfs $ action (buildContext ls)
       case res of
         Left errs               -> return (Left errs)
         Right ((buildc,x),errs) -> do when (isNothing mbFlags) $
                                          modifyLSState (\ls -> ls{ buildContext = buildc })
                                       return (Right (x,errs))


-- Run a build monad with current terminal, flags, and virtual file system
liftBuild :: (BuildContext -> Build (BuildContext,a)) -> LSM (Either Errors (a,Errors))
liftBuild action
  = liftBuildWith Nothing action

getBuildContext :: LSM BuildContext
getBuildContext
  = do ls <- getLSState
       return (buildContext ls)

-- Module name from URI
lookupModuleName :: J.NormalizedUri -> LSM (Maybe (FilePath,ModuleName))
lookupModuleName uri
  = do buildc <- getBuildContext
       mbfpath  <- liftIO $ fromLspUri uri
       return $ do fpath   <- mbfpath -- maybe monad
                   modname <- buildcLookupModuleName fpath buildc
                   return (fpath,modname)

-- Lexemes from module name
-- available even if the source cannot be parsed
lookupLexemes :: ModuleName -> LSM (Maybe [Lexeme])
lookupLexemes mname
  = do buildc <- getBuildContext
       return (buildcGetLexemes mname buildc)

-- RangeMap from module name
lookupRangeMap :: ModuleName -> LSM (Maybe (RangeMap,[Lexeme]))
lookupRangeMap mname
  = do buildc <- getBuildContext
       return (buildcGetRangeMap mname buildc)

-- Program from module name
lookupProgram :: ModuleName -> LSM (Maybe UserProgram)
lookupProgram mname
  = do buildc <- getBuildContext
       return (buildcLookupProgram mname buildc)


-- Pretty environment
getPrettyEnv :: LSM TP.Env
getPrettyEnv
  = do flags <- getFlags
       return (prettyEnvFromFlags flags)

-- Pretty environment
getPrettyEnvFor :: ModuleName -> LSM TP.Env
getPrettyEnvFor modname
  = do flags <- getFlags
       return (prettyEnvFromFlags flags){ TP.context = modname }


-- Format as markdown
prettyMarkdown :: Doc -> LSM T.Text
prettyMarkdown doc
  = do htmlPrinter <- getHtmlPrinter
       liftIO (htmlPrinter doc)

-- Sent text to the terminal info.
emitInfo :: (TP.Env -> Doc) -> LSM ()
emitInfo mkDoc
  = do penv <- getPrettyEnv
       term <- getTerminal
       liftIO $ termInfo term (mkDoc penv)

-- Emit an error notification
emitNotification :: (TP.Env -> Doc) -> LSM ()
emitNotification mkDoc
  = do penv     <- getPrettyEnv
       markdown <- prettyMarkdown (mkDoc penv)
       sendNotification J.SMethod_WindowLogMessage $ J.LogMessageParams J.MessageType_Error markdown

-- Return definitions (gamma etc) for a set of modules
lookupDefinitions :: [ModuleName] -> LSM Definitions
lookupDefinitions modnames
  = do buildc <- getBuildContext
       return (buildcGetDefinitions modnames buildc)

-- Return definitions (gamma etc) for a set of modules including the imports.
lookupVisibleDefinitions :: [ModuleName] -> LSM Definitions
lookupVisibleDefinitions modnames
  = do buildc <- getBuildContext
       return (buildcGetVisibleDefinitions modnames buildc)

-- Return all loaded module names and associated file paths.
lookupModulePaths :: LSM [(ModuleName,FilePath)]
lookupModulePaths
  = do buildc <- getBuildContext
       return (buildcModulePaths buildc)

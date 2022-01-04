-----------------------------------------------------------------------------
-- The language server's monad that holds state (e.g. loaded/compiled modules)
-----------------------------------------------------------------------------
module LanguageServer.Monad
  ( LSState (..),
    defaultLSState,
    newLSStateVar,
    LSM,
    getLSState,
    putLSState,
    modifyLSState,
    getLoaded,
    putLoaded,
    modifyLoaded,
    runLSM,
  )
where

import Compiler.Module (Loaded)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, putMVar, readMVar)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift, liftIO)
import qualified Data.Map as M
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT)
import qualified Language.LSP.Types as J

-- The language server's state, e.g. holding loaded/compiled modules.
newtype LSState = LSState {lsLoaded :: M.Map J.NormalizedUri Loaded}

defaultLSState :: LSState
defaultLSState = LSState {lsLoaded = M.empty}

newLSStateVar :: IO (MVar LSState)
newLSStateVar = newMVar defaultLSState

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

-- Fetches the loaded state holding compiled modules
getLoaded :: LSM (M.Map J.NormalizedUri Loaded)
getLoaded = lsLoaded <$> getLSState

-- Replaces the loaded state holding compiled modules
putLoaded :: M.Map J.NormalizedUri Loaded -> LSM ()
putLoaded l = modifyLSState $ \s -> s {lsLoaded = l}

-- Updates the loaded state holding compiled modules
modifyLoaded :: (M.Map J.NormalizedUri Loaded -> M.Map J.NormalizedUri Loaded) -> LSM ()
modifyLoaded m = modifyLSState $ \s -> s {lsLoaded = m $ lsLoaded s}

-- Runs the language server's state monad.
runLSM :: LSM a -> MVar LSState -> LanguageContextEnv () -> IO a
runLSM lsm stVar cfg = runReaderT (runLspT cfg lsm) stVar

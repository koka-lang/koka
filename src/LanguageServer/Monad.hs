-----------------------------------------------------------------------------
-- The language server's monad that holds state (e.g. loaded/compiled modules)
-----------------------------------------------------------------------------
module LanguageServer.Monad( LSState (..)
                           , LSM
                           , getLSState, putLSState, modifyLSState
                           , getLoaded, putLoaded, modifyLoaded
                           , runLSM
                           ) where

import Compiler.Module         ( Loaded )
import Control.Concurrent.MVar ( MVar, newMVar, takeMVar, putMVar, modifyMVar )
import Control.Monad.Reader    ( ReaderT, runReaderT, ask )
import Control.Monad.Trans     ( lift, liftIO )
import Language.LSP.Server

-- The language server's state, e.g. holding loaded/compiled modules.
data LSState = LSState { lsLoaded :: Maybe Loaded }

-- The monad holding (thread-safe) state used by the language server.
type LSM = LspT () (ReaderT (MVar LSState) IO)

-- Fetches the language server's state inside the LSM monad
getLSState :: LSM LSState
getLSState = do
  stVar <- lift ask
  liftIO $ takeMVar stVar

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
getLoaded :: LSM (Maybe Loaded)
getLoaded = lsLoaded <$> getLSState

-- Replaces the loaded state holding compiled modules
putLoaded :: Maybe Loaded -> LSM ()
putLoaded l = modifyLSState $ \s -> s { lsLoaded = l }

-- Updates the loaded state holding compiled modules
modifyLoaded :: (Maybe Loaded -> Maybe Loaded) -> LSM ()
modifyLoaded m = modifyLSState $ \s -> s { lsLoaded = m $ lsLoaded s }

-- Runs the language server's state monad.
runLSM :: LSM a -> LanguageContextEnv () -> IO a
runLSM lsm cfg = do
  stVar <- newMVar $ LSState { lsLoaded = Nothing }
  runReaderT (runLspT cfg lsm) stVar

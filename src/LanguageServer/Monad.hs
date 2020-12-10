-----------------------------------------------------------------------------
-- The language server's monad that holds state (e.g. loaded/compiled modules)
-----------------------------------------------------------------------------
module LanguageServer.Monad( LSState (..)
                           , LSM
                           , runLSM
                           ) where

import Compiler.Module         ( Loaded )
import Control.Concurrent.MVar ( MVar, newMVar )
import Control.Monad.Reader    ( ReaderT, runReaderT )
import Language.LSP.Server

-- The language server's state, e.g. holding loaded/compiled modules.
data LSState = LSState { lsLoaded :: Maybe Loaded }

-- The monad holding (thread-safe) state used by the language server.
type LSM = LspT () (ReaderT (MVar LSState) IO)

-- Runs the language server's state monad.
runLSM :: LSM a -> LanguageContextEnv () -> IO a
runLSM lsm cfg = do
  st <- newMVar $ LSState { lsLoaded = Nothing }
  runReaderT (runLspT cfg lsm) st

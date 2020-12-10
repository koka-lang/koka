-----------------------------------------------------------------------------
-- The language server's main module
-----------------------------------------------------------------------------
module LanguageServer.Run( runLanguageServer
                         ) where

import Compiler.Options                  ( Flags )
import Control.Monad                     ( void )
import Control.Monad.IO.Class            ( liftIO )
import Language.LSP.Server
import qualified Language.LSP.Types      as J
import LanguageServer.Handlers
import LanguageServer.Monad              ( runLSM, newLSStateVar )

runLanguageServer :: Flags -> [FilePath] -> IO ()
runLanguageServer flags files = do
  state <- newLSStateVar
  void $ runServer $ ServerDefinition
    { onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _ -> pure $ Right env
    , staticHandlers = handlers flags
    , interpretHandler = \env -> Iso (\lsm -> runLSM lsm state env) liftIO
    , options = defaultOptions
      { textDocumentSync = Just syncOptions
      }
    }
  where 
    syncOptions = J.TextDocumentSyncOptions
                        (Just True) -- open/close notifications
                        (Just J.TdSyncIncremental) -- changes
                        (Just False) -- will save
                        (Just False) -- will save (wait until requests are sent to server)
                        (Just $ J.InR $ J.SaveOptions $ Just False) -- save

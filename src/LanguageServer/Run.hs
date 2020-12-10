-----------------------------------------------------------------------------
-- The language server's main module
-----------------------------------------------------------------------------
module LanguageServer.Run( runLanguageServer
                         ) where

import Compiler.Options          ( Flags )
import Control.Monad             ( void )
import Control.Monad.IO.Class    ( liftIO )
import Language.LSP.Server
import LanguageServer.Handlers
import LanguageServer.Monad      ( runLSM )

runLanguageServer :: Flags -> [FilePath] -> IO ()
runLanguageServer flags files = void $ runServer $ ServerDefinition
  { onConfigurationChange = const $ pure $ Right ()
  , doInitialize = \env _ -> pure $ Right env
  , staticHandlers = handlers flags
  , interpretHandler = \env -> Iso (`runLSM` env) liftIO
  , options = defaultOptions
  }

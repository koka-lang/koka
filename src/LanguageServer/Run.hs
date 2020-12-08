module LanguageServer.Run( runLanguageServer
                         ) where

import Compiler.Options ( Flags )
import Control.Monad    ( void )
import Language.LSP.Server
import Language.LSP.Types
import LanguageServer.Handlers

runLanguageServer :: Flags -> [FilePath] -> IO ()
runLanguageServer flags files = void $ runServer $ ServerDefinition
  { onConfigurationChange = const $ pure $ Right ()
  , doInitialize = \env _ -> pure $ Right env
  , staticHandlers = handlers
  }

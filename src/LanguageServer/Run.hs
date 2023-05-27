-----------------------------------------------------------------------------
-- The language server's main module
-----------------------------------------------------------------------------
module LanguageServer.Run (runLanguageServer) where

import Compiler.Options (Flags)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Language.LSP.Server
import qualified Language.LSP.Types as LSPTypes
import LanguageServer.Handlers (handlers)
import LanguageServer.Monad (newLSStateVar, runLSM)

runLanguageServer :: Flags -> [FilePath] -> IO ()
runLanguageServer flags files = do
  state <- newLSStateVar
  void $
    runServer $
      ServerDefinition
        { onConfigurationChange = const $ pure $ Right (),
          doInitialize = \env _ -> pure $ Right env,
          staticHandlers = handlers flags,
          interpretHandler = \env -> Iso (\lsm -> runLSM lsm state env) liftIO,
          options =
            defaultOptions
              { textDocumentSync = Just syncOptions,
                completionTriggerCharacters = Just ['.', ':', '/']
              -- TODO: ? https://www.stackage.org/haddock/lts-18.21/lsp-1.2.0.0/src/Language.LSP.Server.Core.html#Options
              },
          defaultConfig = ()
        }
  where
    syncOptions =
      LSPTypes.TextDocumentSyncOptions
        (Just True) -- open/close notifications
        (Just LSPTypes.TdSyncIncremental) -- changes
        (Just False) -- will save
        (Just False) -- will save (wait until requests are sent to server)
        (Just $ LSPTypes.InR $ LSPTypes.SaveOptions $ Just False) -- trigger on save, but dont send document

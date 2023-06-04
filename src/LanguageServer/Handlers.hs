-----------------------------------------------------------------------------
-- The request handlers used by the language server
-----------------------------------------------------------------------------
module LanguageServer.Handlers (handlers) where

import Compiler.Options (Flags)
import Language.LSP.Server (Handlers, notificationHandler)
import LanguageServer.Handler.Completion (completionHandler)
import LanguageServer.Handler.Definition (definitionHandler)
import LanguageServer.Handler.DocumentSymbol (documentSymbolHandler)
import LanguageServer.Handler.Hover (hoverHandler)
import LanguageServer.Handler.Initialized (initializedHandler)
import LanguageServer.Handler.TextDocument (didChangeHandler, didCloseHandler, didOpenHandler, didSaveHandler)
import LanguageServer.Monad (LSM)
import qualified Language.LSP.Types as J

handlers :: Flags -> Handlers LSM
handlers flags =
  mconcat
    [ initializedHandler flags,
      didOpenHandler flags,
      didChangeHandler flags,
      didSaveHandler flags,
      didCloseHandler flags,
      hoverHandler flags,
      definitionHandler flags,
      documentSymbolHandler flags,
      completionHandler flags,
      cancelHandler flags
    ]

cancelHandler :: Flags -> Handlers LSM
cancelHandler flags =  
  notificationHandler J.SCancelRequest $ \msg -> 
    do
      return ()
-----------------------------------------------------------------------------
-- The request handlers used by the language server
-----------------------------------------------------------------------------
module LanguageServer.Handlers( handlers
                              ) where

import Compiler.Options                    ( Flags )
import Language.LSP.Server
import LanguageServer.Handler.Hover        ( hoverHandler )
import LanguageServer.Handler.Initialized  ( initializedHandler )
import LanguageServer.Handler.TextDocument ( didOpenHandler, didChangeHandler, didSaveHandler, didCloseHandler )

handlers :: Flags -> Handlers (LspM ())
handlers flags = mconcat
  [ initializedHandler flags
  , didOpenHandler flags
  , didChangeHandler flags
  , didSaveHandler flags
  , didCloseHandler flags
  , hoverHandler flags
  ]

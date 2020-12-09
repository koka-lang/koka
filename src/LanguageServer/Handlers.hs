-----------------------------------------------------------------------------
-- The request handlers used by the language server
-----------------------------------------------------------------------------
module LanguageServer.Handlers( handlers
                              ) where

import Compiler.Options                  ( Flags )
import Language.LSP.Server
import LanguageServer.Hover              ( hoverHandler )

handlers :: Flags -> Handlers (LspM ())
handlers flags = mconcat
  [ hoverHandler flags
  ]

{-# LANGUAGE OverloadedStrings #-}
module LanguageServer.Handlers( handlers
                              ) where

import Control.Lens                      ( (^.) )
import Language.LSP.Server
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J

handlers :: Handlers (LspM ())
handlers = mconcat
  [ requestHandler J.STextDocumentHover $ \req responder -> do
      let J.HoverParams _doc pos _ = req ^. J.params
          hc = J.HoverContents $ J.markedUpContent "koka" "It works!"
          rsp = J.Hover hc $ Just $ J.Range pos pos
      responder $ Right $ Just rsp
  ]

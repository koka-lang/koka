-----------------------------------------------------------------------------
-- The LSP handler that provides a document symbol tree
-- (sometimes presented as 'outline' in the client's GUI)
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module LanguageServer.Handler.DocumentSymbol( documentSymbolHandler
                                            ) where

import qualified Common.Range            as R
import Common.Name                       ( Name (..) )
import Compiler.Options                  ( Flags )
import Compiler.Module                   ( modProgram, loadedModule, Loaded (..) )
import Control.Lens                      ( (^.) )
import qualified Data.Map                as M
import Data.Maybe                        ( maybeToList )
import Data.Text                         as T
import Language.LSP.Server               ( requestHandler, Handlers )
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import LanguageServer.Conversions        ( toLspRange )
import LanguageServer.Monad              ( LSM, getLoaded )
import Syntax.Syntax

documentSymbolHandler :: Flags -> Handlers LSM
documentSymbolHandler flags = requestHandler J.STextDocumentDocumentSymbol $ \req responder -> do
  let J.DocumentSymbolParams _ _ doc = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded
  let symbols = findDocumentSymbols =<< maybeToList (M.lookup normUri loaded)
  responder $ Right $ J.InL $ J.List symbols

-- Traverses the syntax tree to find document symbols
findDocumentSymbols :: Loaded -> [J.DocumentSymbol]
findDocumentSymbols loaded = do
  prog <- maybeToList $ modProgram $ loadedModule loaded
  symbols prog

class HasSymbols a where
  symbols :: a -> [J.DocumentSymbol]

instance HasSymbols UserProgram where
  symbols prog = symbols =<< programDefs prog

instance HasSymbols UserDefGroup where
  symbols dg = case dg of
    DefRec    ds -> symbols =<< ds
    DefNonRec d  -> symbols d

instance HasSymbols UserDef where
  symbols d = symbols $ defBinder d

instance HasSymbols (ValueBinder () UserExpr) where
  symbols b = [makeSymbol n J.SkFunction r cs]
    where
      n = binderName b
      r = binderRange b
      cs = symbols $ binderExpr b

instance HasSymbols UserExpr where
  -- TODO
  symbols e = []

makeSymbol :: Name -> J.SymbolKind -> R.Range -> [J.DocumentSymbol] -> J.DocumentSymbol
makeSymbol n k r cs = J.DocumentSymbol name detail kind deprecated range selRange children
  where
    name = T.pack $ nameId n
    detail = Just $ T.pack $ nameModule n
    kind = k
    deprecated = Just False
    range = toLspRange r
    selRange = range
    children = Just $ J.List cs

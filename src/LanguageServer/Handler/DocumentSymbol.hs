-----------------------------------------------------------------------------
-- The LSP handler that provides a document symbol tree
-- (sometimes presented as 'outline' in the client's GUI)
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module LanguageServer.Handler.DocumentSymbol( documentSymbolHandler
                                            ) where

import qualified Common.Range            as R
import Common.Syntax                     ( DefSort (..) )
import Common.Name                       ( Name (..) )
import Compiler.Options                  ( Flags )
import Compiler.Module                   ( modProgram, loadedModule, Loaded (..) )
import Control.Lens                      ( (^.) )
import qualified Data.Map                as M
import Data.Maybe                        ( catMaybes,  maybeToList )
import qualified Data.Text               as T
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

instance HasSymbols a => HasSymbols (Maybe a) where
  symbols = maybe [] symbols

instance HasSymbols () where
  symbols = const []

instance HasSymbols UserDefGroup where
  symbols dg = case dg of
    DefRec    ds -> symbols =<< ds
    DefNonRec d  -> symbols d

instance HasSymbols UserDef where
  symbols d = [makeSymbol n k r cs]
    where
      b = defBinder d
      k = case defSort d of
        DefFun -> J.SkFunction
        DefVal -> J.SkConstant
        DefVar -> J.SkVariable
      n = binderName b
      r = binderRange b
      cs = symbols $ binderExpr b

instance HasSymbols e => HasSymbols (ValueBinder t e) where
  symbols b = [makeSymbol n k r cs]
    where
      k = J.SkConstant
      n = binderName b
      r = binderRange b
      cs = symbols $ binderExpr b

instance HasSymbols UserExpr where
  symbols ex = case ex of
    Lam bs e _                          -> (symbols =<< bs) ++ symbols e
    Let dg e _                          -> symbols dg ++ symbols e
    Bind d e _                          -> symbols d ++ symbols e
    App e nes _                         -> symbols e ++ (symbols . snd =<< nes)
    Ann e _ _                           -> symbols e
    Case e bs _                         -> symbols e ++ (symbols =<< bs)
    Parens e _ _                        -> symbols e
    Handler _ _ _ _ bs e1 e2 e3 hbs _ _ -> (symbols =<< bs) ++ symbols e1
                                                            ++ symbols e2
                                                            ++ symbols e3
                                                            ++ (symbols =<< hbs)
    Inject _ e _ _                      -> symbols e
    _                                   -> [] -- TODO: Handle other types of (nested) expressions

instance HasSymbols UserHandlerBranch where
  symbols hb = [makeSymbol n J.SkFunction r cs]
    where
      n = hbranchName hb
      r = hbranchNameRange hb
      e = hbranchExpr hb
      cs = (symbols =<< hbranchPars hb) ++ symbols e

instance HasSymbols UserBranch where
  symbols b = symbols p ++ (symbols =<< gs)
    where
      p = branchPattern b
      gs = branchGuards b

instance HasSymbols UserGuard where
  symbols g = symbols t ++ symbols e
    where
      t = guardTest g
      e = guardExpr g

instance HasSymbols UserPattern where
  symbols pat = case pat of
    PatVar b        -> let n = binderName b
                           r = binderRange b
                       in [makeSymbol n J.SkConstant r []]
    PatAnn p _ _    -> symbols p
    PatCon _ ps _ _ -> symbols . snd =<< ps
    PatParens p _   -> symbols p
    _ -> []

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

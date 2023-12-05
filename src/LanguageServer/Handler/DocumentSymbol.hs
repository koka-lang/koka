-----------------------------------------------------------------------------
-- The LSP handler that provides a document symbol tree
-- (sometimes presented as 'outline' in the client's GUI)
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleInstances, RecordWildCards #-}
module LanguageServer.Handler.DocumentSymbol( documentSymbolHandler
                                            ) where

import qualified Common.Range            as R
import Common.Syntax                     ( DefSort (..) )
import Common.Name                       ( Name (..) )
import Compiler.Module                   ( modProgram, loadedModule, Loaded (..) )
import Control.Lens                      ( (^.) )
import qualified Data.Map                as M
import Data.Maybe                        ( maybeToList )
import qualified Data.Text               as T
import Language.LSP.Server               ( Handlers, requestHandler )
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import LanguageServer.Conversions        ( toLspRange )
import LanguageServer.Monad              ( LSM, getLoaded )
import Syntax.Syntax
import qualified Language.LSP.Protocol.Message as J

documentSymbolHandler :: Handlers LSM
documentSymbolHandler = requestHandler J.SMethod_TextDocumentDocumentSymbol $ \req responder -> do
  let J.DocumentSymbolParams _ _ doc = req ^. J.params
      uri = doc ^. J.uri
  loaded <- getLoaded
  let symbols = findDocumentSymbols =<< maybeToList loaded
  responder $ Right $ J.InR $ J.InL symbols

-- Traverses the syntax tree to find document symbols
findDocumentSymbols :: Loaded -> [J.DocumentSymbol]
findDocumentSymbols loaded = do
  prog <- maybeToList $ modProgram $ loadedModule loaded
  symbols prog

class HasSymbols a where
  symbols :: a -> [J.DocumentSymbol]

instance HasSymbols a => HasSymbols (Maybe a) where
  symbols = maybe [] symbols

instance HasSymbols a => HasSymbols [a] where
  symbols = (symbols =<<)

instance HasSymbols () where
  symbols = const []

instance HasSymbols UserProgram where
  symbols prog = symbols (programTypeDefs prog) ++ symbols (programDefs prog)

-- Type definition instances

instance HasSymbols UserTypeDefGroup where
  symbols tdg = case tdg of
    TypeDefRec tds   -> symbols tds
    TypeDefNonRec td -> symbols td

instance HasSymbols UserTypeDef where
  symbols td = [makeSymbol n k r cs]
    where
      b = typeDefBinder td
      n = tbinderName b
      r = typeDefRange td
      k = case td of
        Synonym {..}                                       -> J.SymbolKind_Interface
        DataType {typeDefConstrs = ctrs} | length ctrs > 1 -> J.SymbolKind_Enum
                                         | otherwise       -> J.SymbolKind_Struct
      cs = case td of
        DataType {typeDefConstrs = ctrs} -> symbols ctrs
        _                                -> []

instance HasSymbols UserUserCon where
  symbols c = [makeSymbol n k r []]
    where
      n = userconName c
      ps = userconParams c
      k | not (null ps) = J.SymbolKind_Constructor
        | otherwise     = J.SymbolKind_EnumMember
      r = userconRange c

-- Value definition instances

instance HasSymbols UserDefGroup where
  symbols dg = case dg of
    DefRec    ds -> symbols ds
    DefNonRec d  -> symbols d

instance HasSymbols UserDef where
  symbols d = [makeSymbol n k r cs]
    where
      b = defBinder d
      k = case defSort d of
        DefFun _ _ -> J.SymbolKind_Function
        DefVal -> J.SymbolKind_Constant
        DefVar -> J.SymbolKind_Variable
      n = binderName b
      r = defRange d
      cs = symbols $ binderExpr b

instance HasSymbols e => HasSymbols (ValueBinder t e) where
  symbols b = [makeSymbol n k r cs]
    where
      k = J.SymbolKind_Constant
      n = binderName b
      r = binderRange b
      cs = symbols $ binderExpr b

instance HasSymbols UserExpr where
  symbols ex = case ex of
    Lam bs e _                            -> symbols bs ++ symbols e
    Let dg e _                            -> symbols dg ++ symbols e
    Bind d e _                            -> symbols d ++ symbols e
    App e nes _                           -> symbols e ++ symbols (map snd nes)
    Ann e _ _                             -> symbols e
    Case e bs _                           -> symbols e ++ symbols bs
    Parens e _ _                          -> symbols e
    Handler _ _ _ _ _ bs e1 e2 e3 hbs _ _ -> symbols bs ++ symbols e1
                                                        ++ symbols e2
                                                        ++ symbols e3
                                                        ++ symbols hbs
    Inject _ e _ _                        -> symbols e
    _                                     -> [] -- TODO: Handle other types of (nested) expressions

instance HasSymbols UserHandlerBranch where
  symbols hb = [makeSymbol n J.SymbolKind_Function r cs]
    where
      n = hbranchName hb
      r = hbranchNameRange hb
      e = hbranchExpr hb
      ps = hbranchPars hb
      cs = symbols ps ++ symbols e

instance HasSymbols UserBranch where
  symbols b = symbols p ++ symbols gs
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
                       in [makeSymbol n J.SymbolKind_Constant r []]
    PatAnn p _ _    -> symbols p
    PatCon _ ps _ _ -> symbols $ map snd ps
    PatParens p _   -> symbols p
    _ -> []

makeSymbol :: Name -> J.SymbolKind -> R.Range -> [J.DocumentSymbol] -> J.DocumentSymbol
makeSymbol n k r cs = J.DocumentSymbol name detail kind tags deprecated range selRange children
  where
    name = T.pack $ nameId n
    detail = Just $ T.pack $ nameModule n
    kind = k
    tags = Just []
    deprecated = Just False
    range = toLspRange r
    selRange = range
    children = Just cs

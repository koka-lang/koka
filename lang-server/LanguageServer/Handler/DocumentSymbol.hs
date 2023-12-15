-----------------------------------------------------------------------------
-- The LSP handler that provides a document symbol tree
-- (sometimes presented as 'outline' in the client's GUI)
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleInstances, RecordWildCards #-}
module LanguageServer.Handler.DocumentSymbol( documentSymbolHandler
                                            ) where

import qualified Common.Range            as R
import Common.Syntax                     ( DefSort (..), Visibility )
import Common.Name                       ( Name (..), isHiddenName )
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
import Common.NamePrim (nameNull, namePhantom)

-- The LSP handler that provides the symbol tree of a document
-- Symbols include
-- File / Module / Namespace / Package / Class / Method / Property / Field / Constructor / Enum / Interface / Function / Variable
-- Constant / String / Number / Boolean / Array / Object / Key / Null / EnumMember / Struct / Event / Operator / TypeParameter
-- 
-- Koka only reports a subset of these including
-- Interface (for synonyms)
-- Enum (types with more than one constructor) / EnumMember (constructors) / Field (fields) / TypeParameter (type parameters)
-- Struct (types with one constructor) / Constructor (constructor) / Field (fields) / TypeParameter (type parameters)
-- Constant / Function / Variable / Number / String
documentSymbolHandler :: Handlers LSM
documentSymbolHandler = requestHandler J.SMethod_TextDocumentDocumentSymbol $ \req responder -> do
  let J.DocumentSymbolParams _ _ doc = req ^. J.params
      uri = doc ^. J.uri
  loaded <- getLoaded uri
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
  symbols td = makeSymbolSelect n k r rngSelect cs
    where
      b = typeDefBinder td
      n = tbinderName b
      rngSelect = tbinderNameRange b
      r = typeDefRange td
      k = case td of
        Synonym {..}                                       -> J.SymbolKind_Interface
        DataType {typeDefConstrs = ctrs} | length ctrs > 1 -> J.SymbolKind_Enum
                                         | otherwise       -> J.SymbolKind_Struct
      cs = case td of
        DataType {typeDefConstrs = ctrs, typeDefParams=params} | length ctrs > 1 -> symbols ctrs ++ symbols params
        DataType {typeDefParams=params} -> symbols params
        Synonym {typeDefParams = params} -> symbols params

instance HasSymbols (TypeBinder k) where
  symbols b = [] -- makeSymbol n k r []
    where
      n = tbinderName b
      r = tbinderRange b
      k = J.SymbolKind_TypeParameter

instance HasSymbols UserUserCon where
  symbols c = makeSymbolSelect n k r rngSelect cs
    where
      n = userconName c
      rngSelect = userconNameRange c
      ps = userconParams c
      k | not (null ps) = J.SymbolKind_Constructor
        | otherwise     = J.SymbolKind_EnumMember
      r = userconRange c
      cs = symbols ps ++ symbols (userconExists c)

type UserConParam = (Visibility,ValueBinder UserType (Maybe (Expr UserType)))
instance HasSymbols UserConParam where
  symbols (v, b) = makeSymbolSelect n k r rngSelect cs
    where
      n = binderName b
      rngSelect = binderNameRange b
      r = binderRange b
      k = J.SymbolKind_Field
      cs = symbols (binderExpr b)

-- Value definition instances
instance HasSymbols UserDefGroup where
  symbols dg = case dg of
    DefRec    ds -> symbols ds
    DefNonRec d  -> symbols d

instance HasSymbols UserDef where
  symbols d = makeSymbolSelect n k r rngSelect cs
    where
      b = defBinder d
      k = case defSort d of
        DefFun _ _ -> J.SymbolKind_Function
        DefVal -> J.SymbolKind_Constant
        DefVar -> J.SymbolKind_Variable
      n = binderName b
      r = defRange d
      rngSelect = binderNameRange b
      cs = symbols $ binderExpr b

instance HasSymbols e => HasSymbols (ValueBinder t e) where
  symbols b = makeSymbolSelect n k r rngSelect cs
    where
      k = J.SymbolKind_Constant
      n = binderName b
      r = binderRange b
      rngSelect = binderNameRange b
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
    Var n _ r -> makeSymbol n J.SymbolKind_Variable r []
    Lit l -> symbols l

instance HasSymbols Lit where
  symbols l =
    let (s,k) = case l of
              LitChar c _ -> (show c, J.SymbolKind_Constant)
              LitInt i _ -> (show i, J.SymbolKind_Number)
              LitFloat f _ -> (show f, J.SymbolKind_Number)
              LitString s _ -> (show s, J.SymbolKind_String)
    in [J.DocumentSymbol (T.pack s) Nothing k Nothing Nothing rng rng Nothing ]
    where rng = toLspRange $ litRange l

instance HasSymbols UserHandlerBranch where
  symbols hb = makeSymbolSelect n J.SymbolKind_Function r rngSelect cs
    where
      n = hbranchName hb
      rngSelect = hbranchNameRange hb
      e = hbranchExpr hb
      r = rngSelect `R.combineRange` R.getRange e
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
                       in makeSymbol n J.SymbolKind_Constant r []
    PatAnn p _ _    -> symbols p
    PatCon _ ps _ _ -> symbols $ map snd ps
    PatParens p _   -> symbols p
    PatLit l -> symbols l
    PatWild _ -> []

makeSymbolSelect :: Name -> J.SymbolKind -> R.Range -> R.Range -> [J.DocumentSymbol] -> [J.DocumentSymbol]
makeSymbolSelect n k r rngSelect cs =
  [J.DocumentSymbol name detail kind tags deprecated range selRange children | nameId n /= "" && not (isHiddenName n) && n /= namePhantom]
  where
    name = T.pack $ nameId n
    detail = Just $ T.pack $ nameModule n
    kind = k
    tags = Just []
    deprecated = Just False
    range = toLspRange (r `R.combineRange` rngSelect)
    selRange = toLspRange rngSelect
    children = Just cs

makeSymbol :: Name -> J.SymbolKind -> R.Range -> [J.DocumentSymbol] -> [J.DocumentSymbol]
makeSymbol n k r cs = 
  [J.DocumentSymbol name detail kind tags deprecated range selRange children | nameId n /= "" && not (isHiddenName n) && n /= namePhantom]
  where
    name = T.pack $ nameId n
    detail = Just $ T.pack $ nameModule n
    kind = k
    tags = Just []
    deprecated = Just False
    range = toLspRange r
    selRange = range
    children = Just cs

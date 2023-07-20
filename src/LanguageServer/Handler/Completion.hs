-----------------------------------------------------------------------------
-- The LSP handler that provides code completions
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

module LanguageServer.Handler.Completion
  ( completionHandler,
  )
where

import Common.Name (Name (..))
import Compiler.Module (Loaded (..))
import Compiler.Options (Flags)
import Control.Lens ((^.))
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T
import Kind.Constructors (ConInfo (..), Constructors, constructorsList)
import Kind.Synonym (SynInfo (..), Synonyms, synonymsToList)
import Language.LSP.Server (Handlers, getVirtualFile, requestHandler)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.VFS (PosPrefixInfo (..), getCompletionPrefix)
import LanguageServer.Monad (LSM, getLoaded)
import Lib.PPrint (Pretty (..))
import Syntax.Lexer (reservedNames)
import Type.Assumption
  ( Gamma,
    NameInfo
      ( InfoCon,
        InfoExternal,
        InfoFun,
        InfoImport,
        InfoVal,
        infoAlias,
        infoArity,
        infoCName,
        infoCon,
        infoFormat,
        infoFullName,
        infoIsVar,
        infoRange,
        infoType,
        infoVis
      ),
    gammaList,
  )
import qualified Language.LSP.Protocol.Message as J

completionHandler :: Flags -> Handlers LSM
completionHandler flags = requestHandler J.SMethod_TextDocumentCompletion $ \req responder -> do
  let J.CompletionParams doc pos _ _ _ = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded
  vfile <- getVirtualFile normUri
  let items = do
        l <- maybeToList $ M.lookup normUri loaded
        vf <- maybeToList vfile
        pf <- maybeToList =<< getCompletionPrefix pos vf
        findCompletions l pf
  responder $ Right $ J.InL $ items

-- TODO: Make completions context-aware
-- TODO: Complete local variables
-- TODO: Show documentation comments in completion docs

findCompletions :: Loaded -> PosPrefixInfo -> [J.CompletionItem]
findCompletions loaded pfinfo = filter ((pf `T.isPrefixOf`) . (^. J.label)) completions
  where
    pf = prefixText pfinfo
    gamma = loadedGamma loaded
    constrs = loadedConstructors loaded
    syns = loadedSynonyms loaded
    completions =
      keywordCompletions
        ++ valueCompletions gamma
        ++ constructorCompletions constrs
        ++ synonymCompletions syns

-- TODO: Type completions, ideally only inside type expressions
-- ++ newtypeCompletions ntypes

valueCompletions :: Gamma -> [J.CompletionItem]
valueCompletions = map toItem . gammaList
  where
    toItem (n, ninfo) = makeCompletionItem n k d
      where
        k = case ninfo of
          InfoVal {..} -> J.CompletionItemKind_Constant
          InfoFun {..} -> J.CompletionItemKind_Function
          InfoExternal {..} -> J.CompletionItemKind_Reference
          InfoImport {..} -> J.CompletionItemKind_Module
          InfoCon {infoCon = ConInfo {conInfoParams = ps}}
            | not (null ps) -> J.CompletionItemKind_Constructor
            | otherwise -> J.CompletionItemKind_EnumMember
        d = show $ pretty $ infoType ninfo

constructorCompletions :: Constructors -> [J.CompletionItem]
constructorCompletions = map toItem . constructorsList
  where
    toItem (n, cinfo) = makeCompletionItem n k d
      where
        ps = conInfoParams cinfo
        k
          | not (null ps) = J.CompletionItemKind_Constructor
          | otherwise = J.CompletionItemKind_EnumMember
        d = show $ pretty $ conInfoType cinfo

synonymCompletions :: Synonyms -> [J.CompletionItem]
synonymCompletions = map toItem . synonymsToList
  where
    toItem sinfo = makeCompletionItem n J.CompletionItemKind_Interface d
      where
        n = synInfoName sinfo
        d = show $ pretty $ synInfoType sinfo

keywordCompletions :: [J.CompletionItem]
keywordCompletions = map toItem $ S.toList reservedNames
  where
    toItem s = makeSimpleCompletionItem s J.CompletionItemKind_Keyword

makeCompletionItem :: Name -> J.CompletionItemKind -> String -> J.CompletionItem
makeCompletionItem n k d =
  J.CompletionItem
    label
    labelDetails
    kind
    tags
    detail
    doc
    deprecated
    preselect
    sortText
    filterText
    insertText
    insertTextFormat
    insertTextMode
    textEdit
    textEditText
    additionalTextEdits
    commitChars
    command
    xdata
  where
    label = T.pack $ nameId n
    labelDetails = Nothing
    kind = Just k
    tags = Nothing
    detail = Just $  T.pack d
    doc = Just $ J.InL $ T.pack $ nameModule n
    deprecated = Just False
    preselect = Nothing
    sortText = Nothing
    filterText = Nothing
    insertText = Nothing
    insertTextFormat = Nothing
    insertTextMode = Nothing
    textEdit = Nothing
    textEditText = Nothing
    additionalTextEdits = Nothing
    commitChars = Nothing
    command = Nothing
    xdata = Nothing

makeSimpleCompletionItem :: String -> J.CompletionItemKind -> J.CompletionItem
makeSimpleCompletionItem l k =
  J.CompletionItem
    label
    labelDetails
    kind
    tags
    detail
    doc
    deprecated
    preselect
    sortText
    filterText
    insertText
    insertTextFormat
    insertTextMode
    textEdit
    textEditText
    additionalTextEdits
    commitChars
    command
    xdata
  where
    label = T.pack l
    labelDetails = Nothing
    kind = Just k
    tags = Nothing
    detail = Nothing
    doc = Nothing
    deprecated = Just False
    preselect = Nothing
    sortText = Nothing
    filterText = Nothing
    insertText = Nothing
    insertTextFormat = Nothing
    insertTextMode = Nothing
    textEdit = Nothing
    textEditText = Nothing
    additionalTextEdits = Nothing
    commitChars = Nothing
    command = Nothing
    xdata = Nothing

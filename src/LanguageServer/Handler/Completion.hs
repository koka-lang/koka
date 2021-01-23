-----------------------------------------------------------------------------
-- The LSP handler that provides code completions
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
module LanguageServer.Handler.Completion( completionHandler
                                        ) where

import Common.Name                       ( Name (..) )
import Compiler.Options                  ( Flags )
import Compiler.Module                   ( Loaded (..) )
import Control.Lens                      ( (^.) )
import qualified Data.Map                as M
import Data.Maybe                        ( maybeToList )
import qualified Data.Set                as S
import qualified Data.Text               as T
import Kind.Constructors                 ( Constructors, ConInfo (..), constructorsList )
import Kind.Synonym                      ( Synonyms, SynInfo (..), synonymsToList )
import Language.LSP.Server               ( getVirtualFile, requestHandler, Handlers )
import Language.LSP.VFS                  ( PosPrefixInfo (..), getCompletionPrefix )
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import LanguageServer.Monad              ( LSM, getLoaded )
import Lib.PPrint                        ( Pretty (..) )
import Syntax.Lexer                      ( reservedNames )
import Type.Assumption

completionHandler :: Flags -> Handlers LSM
completionHandler flags = requestHandler J.STextDocumentCompletion $ \req responder -> do
  let J.CompletionParams doc pos _ _ _ = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded
  vfile <- getVirtualFile normUri
  let items = do l <- maybeToList $ M.lookup normUri loaded
                 vf <- maybeToList vfile
                 pf <- maybeToList =<< getCompletionPrefix pos vf
                 findCompletions l pf
  responder $ Right $ J.InL $ J.List items

-- TODO: Make completions context-aware
-- TODO: Complete local variables
-- TODO: Show documentation comments in completion docs

findCompletions :: Loaded -> PosPrefixInfo -> [J.CompletionItem]
findCompletions loaded pfinfo = filter ((pf `T.isPrefixOf`) . (^. J.label)) completions
  where pf = prefixText pfinfo
        gamma = loadedGamma loaded
        constrs = loadedConstructors loaded
        syns = loadedSynonyms loaded
        completions = keywordCompletions
                   ++ valueCompletions gamma
                   ++ constructorCompletions constrs
                   ++ synonymCompletions syns
                -- TODO: Type completions, ideally only inside type expressions
                -- ++ newtypeCompletions ntypes

valueCompletions :: Gamma -> [J.CompletionItem]
valueCompletions = map toItem . gammaList
  where toItem (n, ninfo) = makeCompletionItem n k d
          where k = case ninfo of
                      InfoVal {..}      -> J.CiConstant
                      InfoFun {..}      -> J.CiFunction
                      InfoExternal {..} -> J.CiReference
                      InfoImport {..}   -> J.CiModule
                      InfoCon {infoCon=ConInfo {conInfoParams=ps}}| not (null ps) -> J.CiConstructor
                                                                  | otherwise     -> J.CiEnumMember
                d = show $ pretty $ infoType ninfo

constructorCompletions :: Constructors -> [J.CompletionItem]
constructorCompletions = map toItem . constructorsList
  where toItem (n, cinfo) = makeCompletionItem n k d
          where ps = conInfoParams cinfo
                k | not (null ps) = J.CiConstructor
                  | otherwise     = J.CiEnumMember
                d = show $ pretty $ conInfoType cinfo

synonymCompletions :: Synonyms -> [J.CompletionItem]
synonymCompletions = map toItem . synonymsToList
  where toItem sinfo = makeCompletionItem n J.CiInterface d
          where n = synInfoName sinfo
                d = show $ pretty $ synInfoType sinfo

keywordCompletions :: [J.CompletionItem]
keywordCompletions = map toItem $ S.toList reservedNames
  where toItem s = makeSimpleCompletionItem s J.CiKeyword

makeCompletionItem :: Name -> J.CompletionItemKind -> String -> J.CompletionItem
makeCompletionItem n k d = J.CompletionItem label kind tags detail doc deprecated
                                            preselect sortText filterText insertText
                                            insertTextFormat textEdit additionalTextEdits
                                            commitChars command xdata
  where label = T.pack $ nameId n
        kind = Just k
        tags = Nothing
        detail = Just $ T.pack d
        doc = Just $ J.CompletionDocString $ T.pack $ nameModule n
        deprecated = Just False
        preselect = Nothing
        sortText = Nothing
        filterText = Nothing
        insertText = Nothing
        insertTextFormat = Nothing
        textEdit = Nothing
        additionalTextEdits = Nothing
        commitChars = Nothing
        command = Nothing
        xdata = Nothing

makeSimpleCompletionItem :: String -> J.CompletionItemKind -> J.CompletionItem
makeSimpleCompletionItem l k = J.CompletionItem label kind tags detail doc deprecated
                                            preselect sortText filterText insertText
                                            insertTextFormat textEdit additionalTextEdits
                                            commitChars command xdata
  where label = T.pack l
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
        textEdit = Nothing
        additionalTextEdits = Nothing
        commitChars = Nothing
        command = Nothing
        xdata = Nothing

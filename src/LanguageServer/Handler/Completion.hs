-----------------------------------------------------------------------------
-- The LSP handler that provides code completions
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module LanguageServer.Handler.Completion( completionHandler
                                        ) where

import Common.Name                       ( Name (..) )
import Compiler.Options                  ( Flags )
import Compiler.Module                   ( Loaded (..) )
import Control.Lens                      ( (^.) )
import qualified Data.Map                as M
import Data.Maybe                        ( maybeToList )
import qualified Data.Text               as T
import Kind.Constructors                 ( Constructors )
import Language.LSP.Server               ( getVirtualFile, requestHandler, Handlers )
import Language.LSP.VFS                  ( PosPrefixInfo (..), getCompletionPrefix )
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import LanguageServer.Monad              ( LSM, getLoaded )
import Lib.PPrint                        ( Pretty (..) )
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

findCompletions :: Loaded -> PosPrefixInfo -> [J.CompletionItem]
findCompletions loaded pfinfo = filter ((pf `T.isPrefixOf`) . (^. J.label)) completions
  where pf = prefixText pfinfo
        gamma = loadedGamma loaded
        constrs = loadedConstructors loaded
        completions = valueCompletions gamma
                   ++ constructorCompletions constrs
                  -- TODO:
                  --  ++ synonymCompletions
                  --  ++ newtypeCompletions

valueCompletions :: Gamma -> [J.CompletionItem]
valueCompletions = map toItem . gammaList
  where toItem (n, _) = makeCompletionItem n J.CiFunction "" -- TODO: pretty-print type

constructorCompletions :: Constructors -> [J.CompletionItem]
constructorCompletions = const [] -- TODO

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

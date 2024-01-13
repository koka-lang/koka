-----------------------------------------------------------------------------
-- The LSP handler that provides signature help for functions
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}
module LanguageServer.Handler.SignatureHelp(signatureHelpHandler) where

import Control.Lens ((^.))
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Types (FoldingRangeKind(FoldingRangeKind_Region))
import Language.LSP.Server (Handlers, requestHandler, sendNotification, getVirtualFile)
import Data.Aeson
import Data.Aeson.Types as A
import Common.Name ( Name, readQualifiedName, nameNil )
import LanguageServer.Monad (LSM, getLoaded, getLoadedModule, SignatureContext(..), clearSignatureContext, getSignatureContext, getFlags, getHtmlPrinter, getLoadedSuccess)
import Debug.Trace (trace)
import Syntax.RangeMap (rangeMapFindAt, previousLexemesReversed, getFunctionIncompleteReverse, FnSyntax (..), getFunctionNameReverse)
import Compiler.Module (modRangeMap, modLexemes, loadedGamma, Module (..), Loaded (loadedImportMap, loadedModule))
import Control.Monad.IO.Class (liftIO)
import LanguageServer.Conversions (fromLspPos, fromLspUri)
import Syntax.Lexeme (Lexeme(..))
import Syntax.Lexer (Lex(..), extractLiterate, lexing)
import Type.Assumption (gammaLookup, NameInfo(..))
import Data.List (findIndex)
import Common.Range (Source(..))
import Common.File (isLiteralDoc)
import Syntax.Layout (layout)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as T
import Language.LSP.VFS (virtualFileText)
import Lib.PPrint (Pretty(..), Doc, (<.>), (<+>), text)
import Kind.Constructors (ConInfo(..))
import Type.Pretty (Env(..), ppScheme, ppName)
import Compiler.Options (prettyEnvFromFlags, colorSchemeFromFlags)
import Common.ColorScheme (ColorScheme)
import LanguageServer.Handler.Pretty (ppComment, asKokaCode)


firstId chain =
  case chain of
    FnValue (Lexeme _ (LexId id)) -> Just id
    FnChained (Lexeme _ (LexId id)) _ -> Just id
    FnIncomplete chain -> firstId chain
    _ -> Nothing

signatureHelpHandler :: Handlers LSM
signatureHelpHandler = requestHandler J.SMethod_TextDocumentSignatureHelp $ \req responder -> do
  let J.SignatureHelpParams doc pos prog context = req ^. J.params
      uri = J.toNormalizedUri $ doc ^. J.uri
  -- Get additional signature context given by the client (from completions)
  sig <- getSignatureContext
  -- Get the last successful typechecked version (so we can get gamma), it might be out of date a bit
  loaded <- getLoadedSuccess uri 
  pos <- liftIO $ fromLspPos uri pos
  vfile <- getVirtualFile uri
  filePath <- fromMaybe "" <$> liftIO (fromLspUri uri)
  let res = do -- maybe monad
        l    <- loaded
        vf   <- vfile
        return (l, vf)
  case res of
    Just (l, vf) -> do
      let text = T.encodeUtf8 $ virtualFileText vf

      let source = Source filePath text
          input  = if isLiteralDoc filePath then extractLiterate text else text
          xs = lexing source 1 input
          lexemes = layout False {-no at-} True {-semi insert-} xs
          prevLexes = previousLexemesReversed lexemes pos
      let fncontext = getFunctionNameReverse prevLexes
          incompleteContext = getFunctionIncompleteReverse prevLexes

      -- trace ("Signature help for " ++ show (take 5 prevLexes) ++ " context: " ++ show fncontext) $ return ()
      let id =
            case prevLexes of
              (Lexeme _ (LexId id)):_ -> id
              _ ->
                case firstId fncontext of
                  Just id -> id
                  Nothing -> fromMaybe nameNil (firstId incompleteContext)
      -- trace ("Signature help for " ++ show id) $ return ()
      if nameNil == id then
        responder $ Right $ J.InR J.Null
      else do
        -- Select the right index if the id == the sigContextId, default index to 0 or null
        -- TODO: Check if the pos is on a place with that name anyways
        print <- getHtmlPrinter
        flags <- getFlags
        let env = (prettyEnvFromFlags flags){ context = modName (loadedModule l), importsMap = loadedImportMap l }
            colors = colorSchemeFromFlags flags
        let gamma = loadedGamma l
        let results = gammaLookup id gamma
        let completionName = sigFunctionName <$> sig
        sigInfos <- liftIO $ mapM (getSignatureInformation print env colors) results 
        let sigInfo = concat sigInfos
        let mbIndex = findIndex (\(n, si) -> Just n == completionName) sigInfo
        -- trace ("Signature help for " ++ show completionName) $ return ()
        responder $ Right $ J.InL $ J.SignatureHelp
          (map snd sigInfo) (fmap fromIntegral mbIndex) (Just 0)
    _ ->
      -- trace ("No loaded module for " ++ show uri) $ do
      responder $ Right $ J.InL $ J.SignatureHelp [] (Just 0) (Just 0)

getSignatureInformation :: (Doc -> IO T.Text) -> Env -> ColorScheme -> (Name,NameInfo) -> IO [(Name, J.SignatureInformation)]
getSignatureInformation print env colors (n, ninfo) = do
  let signature name tp      = ppName env name <+> text ":" <+> ppScheme env tp
      toMarkdown name tp doc = asKokaCode (signature name tp) <.> ppComment doc
  case ninfo of
    InfoVal{ infoVis, infoCName , infoType , infoRange, infoIsVar, infoDoc } ->
      do
        markdown <- print $ toMarkdown infoCName infoType infoDoc
        return [(n, J.SignatureInformation (T.pack $ show $ pretty infoCName) (Just $ J.InR (J.mkMarkdown markdown)) Nothing Nothing)]
    InfoFun{ infoVis , infoCName , infoType , infoArity , infoFip, infoRange, infoDoc } ->
      do
        markdown <- print $ toMarkdown infoCName infoType infoDoc
        return [(n, J.SignatureInformation (T.pack $ show $ pretty infoCName) (Just $ J.InR (J.mkMarkdown markdown)) Nothing Nothing)]
    InfoCon{ infoVis, infoType , infoRepr  , infoCon , infoRange, infoDoc } -> 
      do
        markdown <- print $ toMarkdown (conInfoName infoCon) infoType infoDoc
        return [(n, J.SignatureInformation (T.pack $ show $ pretty $ conInfoName infoCon) (Just $ J.InR (J.mkMarkdown markdown)) Nothing Nothing)]
    InfoExternal{infoVis, infoCName , infoType , infoFormat , infoFip, infoRange} -> return []
    InfoImport{} -> return []

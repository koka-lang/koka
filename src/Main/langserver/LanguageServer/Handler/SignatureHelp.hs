-----------------------------------------------------------------------------
-- The LSP handler that provides signature help for functions
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}
module LanguageServer.Handler.SignatureHelp(signatureHelpHandler) where

import Debug.Trace (trace)
import Data.List (findIndex)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types as A
import qualified Data.Text.Encoding as T
import Control.Lens ((^.))
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Types (FoldingRangeKind(FoldingRangeKind_Region))
import Language.LSP.Server (Handlers, requestHandler, sendNotification, getVirtualFile)

import Lib.PPrint
import Common.Name ( Name, readQualifiedName, nameNil )
import Common.File (isLiteralDoc)
import Syntax.Lexeme (Lexeme(..), Lex(..))
import Syntax.RangeMap hiding (NameInfo)
import Kind.Constructors (ConInfo(..))
import Type.Assumption (gammaLookup, NameInfo(..))
import Type.Pretty (Env(..), ppScheme, ppName)

import Compile.Options (prettyEnvFromFlags, colorSchemeFromFlags)
import Compile.BuildContext

import Language.LSP.VFS (virtualFileText)
import LanguageServer.Conversions (fromLspPos, fromLspUri)
import LanguageServer.Handler.Pretty (ppComment, asKokaCode)
import LanguageServer.Monad

signatureHelpHandler :: Handlers LSM
signatureHelpHandler
  = requestHandler J.SMethod_TextDocumentSignatureHelp $ \req responder ->
    do let J.SignatureHelpParams doc pos0 prog context = req ^. J.params
           uri = J.toNormalizedUri $ doc ^. J.uri
           done :: LSM ()
           done = responder $ Right $ J.InR J.Null

           liftMaybe :: LSM (Maybe a) -> (a -> LSM ()) -> LSM ()
           liftMaybe action next = do res <- action
                                      case res of
                                        Nothing -> done
                                        Just x  -> next x

       -- Get additional signature context given by the client (from completions)
       sig <- getSignatureContext
       pos <- liftIO $ fromLspPos uri pos0
       filePath <- fromMaybe "" <$> liftIO (fromLspUri uri)

       liftMaybe (getVirtualFile uri) $ \vf ->
        liftMaybe (lookupModuleName uri) $ \(fpath,modname) ->
          liftMaybe (lookupLexemes modname) $ \lexemes ->
            liftMaybe (createSignatureHelp pos sig modname vf filePath lexemes) $ \sighelp ->
              do responder $ Right $ J.InL $ sighelp

-- _ ->
--        -- trace ("No loaded module for " ++ show uri) $ do
--        J.SignatureHelp [] (Just 0) (Just 0)

firstId chain =
  case chain of
    FnValue (Lexeme _ (LexId id)) -> Just id
    FnChained (Lexeme _ (LexId id)) _ -> Just id
    FnIncomplete chain -> firstId chain
    _ -> Nothing


createSignatureHelp pos sig modname vf filePath lexemes
  = do  let text = T.encodeUtf8 $ virtualFileText vf

        let prevLexes = previousLexemesReversed lexemes pos
        let fncontext = getFunctionNameReverse prevLexes
            incompleteContext = getFunctionIncompleteReverse prevLexes

        -- trace ("Signature help for " ++ show (take 5 prevLexes) ++ " context: " ++ show fncontext) $ return ()
        let name = case prevLexes of
                    (Lexeme _ (LexId id)):_ -> id
                    _ -> case firstId fncontext of
                           Just id -> id
                           Nothing -> fromMaybe nameNil (firstId incompleteContext)
        -- trace ("Signature help for " ++ show id) $ return ()
        if nameNil == name then
          return Nothing
        else do
          -- Select the right index if the id == the sigContextId, default index to 0 or null
          -- TODO: Check if the pos is on a place with that name anyways
          penv <- getPrettyEnvFor modname
          defs <- lookupVisibleDefinitions [modname]
          let gamma   = defsGamma defs
          let results = gammaLookup name gamma
          let completionName = sigFunctionName <$> sig
          sigInfos <- concat <$> mapM (getSignatureInformation penv) results
          let mbIndex = findIndex (\(n, si) -> Just n == completionName) sigInfos
          -- trace ("Signature help for " ++ show completionName) $ return ()
          return $! Just $! J.SignatureHelp (map snd sigInfos) (fmap fromIntegral mbIndex) (Just 0)

getSignatureInformation :: Env -> (Name,NameInfo) -> LSM [(Name, J.SignatureInformation)]
getSignatureInformation penv (n, ninfo)
  = do  let signature name tp      = ppName penv name <+> text ":" <+> ppScheme penv tp
            toMarkdown name tp doc = asKokaCode (signature name tp) <.> ppComment doc
        case ninfo of
          InfoVal{ infoVis, infoCName , infoType , infoRange, infoIsVar, infoDoc } ->
            do
              markdown <- prettyMarkdown $ toMarkdown infoCName infoType infoDoc
              return [(n, J.SignatureInformation (T.pack $ show $ pretty infoCName) (Just $ J.InR (J.mkMarkdown markdown)) Nothing Nothing)]
          InfoFun{ infoVis , infoCName , infoType , infoArity , infoFip, infoRange, infoDoc } ->
            do
              markdown <- prettyMarkdown $ toMarkdown infoCName infoType infoDoc
              return [(n, J.SignatureInformation (T.pack $ show $ pretty infoCName) (Just $ J.InR (J.mkMarkdown markdown)) Nothing Nothing)]
          InfoCon{ infoVis, infoType , infoRepr  , infoCon , infoRange, infoDoc } ->
            do
              markdown <- prettyMarkdown $ toMarkdown (conInfoName infoCon) infoType infoDoc
              return [(n, J.SignatureInformation (T.pack $ show $ pretty $ conInfoName infoCon) (Just $ J.InR (J.mkMarkdown markdown)) Nothing Nothing)]
          InfoExternal{infoVis, infoCName , infoType , infoFormat , infoFip, infoRange} -> return []
          InfoImport{} -> return []

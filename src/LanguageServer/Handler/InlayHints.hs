-----------------------------------------------------------------------------
-- The LSP handler that provides hover tooltips
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.InlayHints (inlayHintsHandler) where

import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Server (Handlers, sendNotification, requestHandler)
import LanguageServer.Monad (LSM, getLoaded, getLoadedModule, getFlags)
import LanguageServer.Conversions (fromLspPos, toLspRange, toLspPos, fromLspRange)
import LanguageServer.Handler.Hover (formatRangeInfoHover)
import qualified Data.Text as T
import Common.Range (Range (..), rangeEnd, Pos(..), rangeNull, posNull)
import Syntax.RangeMap (NameInfo (..), RangeInfo (..), rangeMapFindIn)
import Control.Lens ((^.))
import Data.Maybe (mapMaybe)
import Type.Pretty (ppType, Env (..), defaultEnv, ppScheme)
import Common.Name (Name)
import Compiler.Compile (Module(..), Loaded (..))
import Kind.ImportMap (ImportMap)
import Compiler.Options (prettyEnvFromFlags)
-- import Debug.Trace (trace)

inlayHintsHandler :: Handlers LSM
inlayHintsHandler = requestHandler J.SMethod_TextDocumentInlayHint $ \req responder -> do
  let J.InlayHintParams prog doc rng = req ^. J.params
      uri = doc ^. J.uri
      newRng = fromLspRange uri rng
  loadedMod <- getLoadedModule uri
  loaded <- getLoaded uri
  flags <- getFlags
  let toInlayHint :: ImportMap -> Name -> (Range, RangeInfo) -> Maybe J.InlayHint
      toInlayHint imports modName (rng, rngInfo) =
        let env = (prettyEnvFromFlags flags){ context = modName, importsMap = imports } in
        let rngEnd = rangeEnd rng
            shouldShow =
              (rngEnd /= posNull) &&
              case rngInfo of
              Id _ info _ -> case info of
                NIValue _ isAnnotated -> not isAnnotated
                NICon _ -> False
                NITypeCon _ -> False
                NITypeVar _ -> False
                NIModule -> False
                NIKind -> False
              Decl{} -> False
              Block _ -> False
              Error _ -> False
              Warning _ -> False
        in
        if shouldShow then
          Just $ J.InlayHint (toLspPos rngEnd{posColumn = posColumn rngEnd + 1}) (J.InL $ T.pack $ formatInfo env modName rngInfo) (Just J.InlayHintKind_Type) Nothing Nothing (Just True) (Just True) Nothing
        else Nothing
      rsp = do
        l <- loaded
        lm <- loadedMod
        rmap <- modRangeMap lm
        -- trace (show $ rangeMapFindIn newRng rmap) $ return ()
        return $ mapMaybe (toInlayHint (loadedImportMap l) (modName lm)) $ rangeMapFindIn newRng rmap
  case rsp of
    Nothing -> responder $ Right $ J.InR J.Null
    Just rsp -> responder $ Right $ J.InL rsp

-- Pretty-prints type/kind information to a hover tooltip
formatInfo :: Env -> Name -> RangeInfo -> String
formatInfo env modName rinfo = case rinfo of
  Id qname info isdef ->
    case info of
      NIValue tp _ -> " : " ++ show (ppScheme env tp)
      NICon tp -> ""
      NITypeCon k -> ""
      NITypeVar k -> ""
      NIModule -> ""
      NIKind -> ""
  Decl s name mname -> ""
  Block s -> ""
  Error doc -> "Error: " ++ show doc
  Warning doc -> "Warning: " ++ show doc
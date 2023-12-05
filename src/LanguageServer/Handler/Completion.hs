-----------------------------------------------------------------------------
-- The LSP handler that provides code completions
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LanguageServer.Handler.Completion
  ( completionHandler,
  )
where

import Common.Name (Name (..))
import Compiler.Module (Loaded (..))
import Control.Lens ((^.))
import qualified Data.Map as M
import Data.Maybe (maybeToList, fromMaybe, fromJust)
import qualified Data.Text.Utf16.Rope as Rope
import qualified Data.Set as S
import qualified Data.Text as T
import Kind.Constructors (ConInfo (..), Constructors, constructorsList)
import Kind.Synonym (SynInfo (..), Synonyms, synonymsToList)
import Language.LSP.Server (Handlers, getVirtualFile, requestHandler)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.VFS (VirtualFile (VirtualFile))
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
import Data.Char (isUpper, isAlphaNum)
import Compiler.Compile (Module (..))
import Type.Type (Type(..), splitFunType, splitFunScheme)
import Syntax.RangeMap (rangeMapFindAt, rangeInfoType)
import LanguageServer.Conversions (fromLspPos, loadedModuleFromUri)
import Common.Range (makePos, posNull, Range, rangeNull)
import LanguageServer.Handler.Hover (formatRangeInfoHover)
import Type.Unify (runUnify, unify, runUnifyEx, matchArguments)
import Data.Either (isRight)
import Lib.Trace (trace)
import Type.InferMonad (subst, instantiate)
import Type.TypeVar (tvsEmpty)
import Data.ByteString (intercalate)
import Control.Monad.ST (runST)
import Language.LSP.Protocol.Types (InsertTextFormat(InsertTextFormat_Snippet))

completionHandler :: Handlers LSM
completionHandler = requestHandler J.SMethod_TextDocumentCompletion $ \req responder -> do
  let J.CompletionParams doc pos _ _ context = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded
  vfile <- getVirtualFile normUri
  let items = do
        l <- maybeToList loaded
        lm <- maybeToList $ loadedModuleFromUri loaded uri
        vf <- maybeToList vfile
        pi <- maybeToList =<< getCompletionInfo pos vf lm uri
        findCompletions l lm pi
  responder $ Right $ J.InL items

-- | Describes the line at the current cursor position
data PositionInfo = PositionInfo
  { fullLine :: !T.Text
    -- ^ The full contents of the line the cursor is at
  , argument :: !T.Text
  , searchTerm :: !T.Text
  , cursorPos :: !J.Position
    -- ^ The cursor position
  , argumentType :: Maybe Type
  , isFunctionCompletion :: Bool
  } deriving (Show,Eq)

getCompletionInfo :: Monad m => J.Position -> VirtualFile -> Module -> J.Uri -> m (Maybe PositionInfo)
getCompletionInfo pos@(J.Position l c) (VirtualFile _ _ ropetext) mod uri =
      let rm = (fromJust $ modRangeMap mod) in
      let result = Just $ fromMaybe (PositionInfo "" "" "" pos Nothing False) $ do -- Maybe monad
            let lastMaybe [] = Nothing
                lastMaybe xs = Just $ last xs

            let currentRope = fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine (fromIntegral l) ropetext
            beforePos <- Rope.toText . fst <$> Rope.splitAt (fromIntegral c + 1) currentRope
            currentWord <-
                if | T.null beforePos -> Just ""
                   | T.last beforePos == ' ' -> Just "" -- Up to whitespace but not including it
                   | otherwise -> lastMaybe (T.words beforePos)

            let parts = T.split (=='.') -- The () are for operators and / is for qualified names otherwise everything must be adjacent
                          $ T.takeWhileEnd (\x -> isAlphaNum x || x `elem` ("()-_./'"::String)) currentWord

            case reverse parts of
              [] -> Nothing
              (x:xs) -> do
                trace ("parts: " ++ show parts) $ return ()
                let modName = case filter (not .T.null) xs of {x:xs -> x; [] -> ""}
                argumentText <- Rope.toText . fst <$> Rope.splitAt (fromIntegral c) currentRope
                let isFunctionCompletion = if | T.null argumentText -> False
                                              | T.findIndex (== '.') argumentText > T.findIndex (== ' ') argumentText -> True
                                              | otherwise -> False
                    newC = c - fromIntegral (T.length x + (if isFunctionCompletion then 1 else 0))
                let currentType =
                      if isFunctionCompletion then
                          let currentRange = fromLspPos uri (J.Position l newC) in
                          do
                            (range, rangeInfo) <- rangeMapFindAt currentRange rm
                            t <- rangeInfoType rangeInfo
                            case splitFunType t of
                              Just (pars,eff,res) -> Just res
                              Nothing             -> Just t
                      else Nothing
                -- currentRope is already a single line, but it may include an enclosing '\n'
                let curLine = T.dropWhileEnd (== '\n') $ Rope.toText currentRope
                let pi = PositionInfo curLine modName x pos currentType isFunctionCompletion
                return $ trace (show pi) pi
            in
      trace (show result) $ return result

-- TODO: Complete local variables
-- TODO: Show documentation comments in completion docs

filterInfix :: PositionInfo -> T.Text -> Bool
filterInfix pinfo n = (searchTerm pinfo `T.isInfixOf` n) && (('.' /= T.head n) || ".Hnd-" `T.isPrefixOf` n)

filterInfixConstructors :: PositionInfo -> T.Text -> Bool
filterInfixConstructors pinfo n = (searchTerm pinfo `T.isInfixOf` n) && (('.' /= T.head n) || ".Hnd-" `T.isPrefixOf` n)

findCompletions :: Loaded -> Module -> PositionInfo -> [J.CompletionItem]
findCompletions loaded mod pinfo@PositionInfo{isFunctionCompletion = fcomplete} = filter (filterInfix pinfo . (^. J.label)) completions
  where
    curModName = modName mod
    search = searchTerm pinfo
    gamma = loadedGamma loaded
    constrs = loadedConstructors loaded
    syns = loadedSynonyms loaded
    completions =
      if fcomplete then valueCompletions curModName gamma pinfo else keywordCompletions curModName
        ++ valueCompletions curModName gamma pinfo
        ++ constructorCompletions curModName constrs
        ++ synonymCompletions curModName syns

-- TODO: Type completions, ideally only inside type expressions
-- ++ newtypeCompletions ntypes

typeUnifies :: Type -> Maybe Type -> Bool
typeUnifies t1 t2 =
  case t2 of
    Nothing -> True
    Just t2 ->  let (res, _, _) = (runUnifyEx 0 $ matchArguments True rangeNull tvsEmpty t1 [t2] [] Nothing) in  isRight res

valueCompletions :: Name -> Gamma -> PositionInfo -> [J.CompletionItem]
valueCompletions curModName gamma pinfo@PositionInfo{argumentType=tp, searchTerm=search, isFunctionCompletion} = map toItem . filter matchInfo $ filter (\(n, ni) -> filterInfix pinfo $ T.pack $ nameId n) $ gammaList gamma
  where
    isHandler n = '.' == T.head n
    matchInfo :: (Name, NameInfo) -> Bool
    matchInfo (n, ninfo) = case ninfo of
        InfoVal {infoType} -> typeUnifies infoType tp
        InfoFun {infoType} -> typeUnifies infoType tp
        InfoExternal {infoType} -> typeUnifies infoType tp
        InfoImport {infoType} -> typeUnifies infoType tp
        InfoCon {infoType } -> typeUnifies infoType tp
    toItem (n, ninfo) = case ninfo of
        InfoCon {infoCon} | isHandler $ T.pack (nameId n) -> makeHandlerCompletionItem curModName infoCon d rng (fullLine pinfo)
        InfoFun {infoType} -> makeFunctionCompletionItem curModName n d infoType isFunctionCompletion rng (fullLine pinfo)
        InfoVal {infoType} -> case splitFunScheme infoType of
          Just (tvars, tpreds, pars, eff, res) -> makeFunctionCompletionItem curModName n d infoType isFunctionCompletion rng (fullLine pinfo)
          Nothing -> makeCompletionItem curModName n k d
        _ -> makeCompletionItem curModName n k d
      where
        pos@(J.Position l c) = cursorPos pinfo
        rng = J.Range (J.Position l $ c - fromIntegral (T.length search)) pos
        k = case ninfo of
          InfoVal {..} -> J.CompletionItemKind_Constant
          InfoFun {..} -> J.CompletionItemKind_Function
          InfoExternal {..} -> J.CompletionItemKind_Reference
          InfoImport {..} -> J.CompletionItemKind_Module
          InfoCon {infoCon = ConInfo {conInfoParams = ps}}
            | not (null ps) -> J.CompletionItemKind_Constructor
            | otherwise -> J.CompletionItemKind_EnumMember
        d = show $ pretty $ infoType ninfo

constructorCompletions :: Name -> Constructors -> [J.CompletionItem]
constructorCompletions curModName cstrs = map toItem $ filter (\(n,ci) -> '.' /= T.head (T.pack $ nameId n)) (constructorsList cstrs)
  where
    toItem (n, cinfo) = makeCompletionItem curModName n k d
      where
        ps = conInfoParams cinfo
        k
          | not (null ps) = J.CompletionItemKind_Constructor
          | otherwise = J.CompletionItemKind_EnumMember
        d = show $ pretty $ conInfoType cinfo

synonymCompletions :: Name -> Synonyms -> [J.CompletionItem]
synonymCompletions curModName = map toItem . synonymsToList
  where
    toItem sinfo = makeCompletionItem curModName n J.CompletionItemKind_Interface d
      where
        n = synInfoName sinfo
        d = show $ pretty $ synInfoType sinfo

keywordCompletions :: Name -> [J.CompletionItem]
keywordCompletions curModName  = map toItem $ S.toList reservedNames
  where
    toItem s = makeSimpleCompletionItem curModName s J.CompletionItemKind_Keyword

makeCompletionItem :: Name -> Name -> J.CompletionItemKind -> String -> J.CompletionItem
makeCompletionItem curModName n k d =
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
    sortText = Just $ if nameId curModName == nameModule n then T.pack $ "0" ++ nameId n else T.pack $ "2" ++ nameId n
    filterText = Nothing
    insertText = Nothing
    insertTextFormat = Nothing
    insertTextMode = Nothing
    textEdit = Nothing
    textEditText = Nothing
    additionalTextEdits = Nothing
    commitChars = Just [T.pack "\t"]
    command = Nothing
    xdata = Nothing

makeFunctionCompletionItem :: Name -> Name -> String -> Type -> Bool -> J.Range -> T.Text-> J.CompletionItem
makeFunctionCompletionItem curModName funName d funType accessor rng line =
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
      label = T.pack $ nameId funName
      indentation = T.length $ T.takeWhile (== ' ') line
      trailingFunIndentation = T.replicate indentation " "
      labelDetails = Nothing
      kind = Just J.CompletionItemKind_Snippet
      tags = Nothing
      detail = Just $  T.pack d
      doc = Just $ J.InL $ T.pack $ nameModule funName
      deprecated = Just False
      preselect = Nothing
      sortText = Just $ if nameId curModName == nameModule funName then "0" <> label else "2" <> label
      filterText = Just label
      insertText = Nothing
      insertTextFormat = Just InsertTextFormat_Snippet
      insertTextMode = Nothing
      arguments = case splitFunScheme funType
        of Just (tvars, tpreds, pars, eff, res) -> pars
           Nothing -> []
      numArgs = length arguments - (if accessor then 1 else 0)
      trailingFunArgTp = case arguments
        of [] -> Nothing
           xs -> let arg = last xs
            in case splitFunScheme (snd arg) of
              Nothing -> Nothing
              Just (_, _, args, _, _) -> Just args
      argumentsText =
        if numArgs == 0 then trace ("No function arguments for " ++ show label) $ T.pack ""
        else case trailingFunArgTp of
          Nothing -> "(" <> T.intercalate "," (map (\i -> T.pack $ "$" ++ show i) [1..numArgs]) <> ")"
          Just tp ->
            let mainArgs = "(" <> T.intercalate "," (map (\i -> T.pack $ "$" ++ show i) [1..numArgs-1]) <> ")"
            in mainArgs <> " fn(" <> T.intercalate "," (map (\i -> T.pack $ "$" ++ show i) [numArgs..numArgs+length tp-1]) <> ")\n" <> trailingFunIndentation <> "()"
      textEdit = Just $ J.InL $ J.TextEdit rng $ label <> argumentsText
      textEditText = Nothing
      additionalTextEdits = Nothing
      commitChars = Just [T.pack "\t"]
      command = Nothing
      xdata = Nothing

makeHandlerCompletionItem :: Name -> ConInfo -> String -> J.Range -> T.Text -> J.CompletionItem
makeHandlerCompletionItem curModName conInfo d r line =
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
    indentation = T.length $ T.takeWhile (== ' ') line
    clauseIndentation = T.replicate indentation " "
    clauseBodyIndentation = T.replicate (indentation + 2) " "
    typeName = conInfoTypeName conInfo
    typeNameId = T.replace ".hnd-" "" $ T.pack $ nameId typeName
    label = "handler for " <> typeNameId
    labelDetails = Nothing
    kind = Just J.CompletionItemKind_Snippet
    tags = Nothing
    detail = Just $  T.pack d
    doc = Just $ J.InL $ T.pack $ nameModule typeName
    deprecated = Just False
    preselect = Nothing
    sortText = Just $ if nameId curModName == nameModule typeName then "0" <> typeNameId else "2" <> typeNameId
    filterText = Just typeNameId
    insertText = Nothing
    insertTextFormat = Just InsertTextFormat_Snippet
    insertTextMode = Nothing
    handlerClause :: (Int, [T.Text]) -> (Name, Type) -> (Int, [T.Text])
    handlerClause (i, acc) (name, tp) =
      -- TODO: Consider adding snippet locations for the body of the handlers as well
      if T.isPrefixOf "val" newName then
        (i + 1, acc ++ [clauseIndentation <> newName <> " = $" <> T.pack (show (i + 1))])
      else (if not (null funArgs) then fst (last funArgs) + 1 else 1, acc ++ [clauseIndentation <> newName <> "(" <> T.intercalate "," (map snd funArgs) <> ")\n" <> clauseBodyIndentation <> "()"])
      where
        funArgs = zipWith (\i s -> (i, T.pack $ "$" ++ show (i + 1))) [i..] (handlerArgs newName tp)
        newName = T.replace "brk" "final ctl" $ T.replace "-" " " (T.pack (show name))
    textEdit = Just $ J.InL $ J.TextEdit r $ "handler\n" <> T.intercalate "\n" (snd (foldl handlerClause (1, []) (conInfoParams conInfo)))
    textEditText = Nothing
    additionalTextEdits = Nothing
    commitChars = Just [T.pack "\t"]
    command = Nothing
    xdata = Nothing

handlerArgs :: T.Text -> Type -> [Type]
handlerArgs name tp =
  case tp of
    TApp _ args -> if T.isPrefixOf "val" name then take (length args - 3) args else take (length args - 4) args
    _ -> []

makeSimpleCompletionItem :: Name -> String -> J.CompletionItemKind -> J.CompletionItem
makeSimpleCompletionItem curModName l k =
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
    sortText = Just $ T.pack $ "1" ++ l
    filterText = Nothing
    insertText = Nothing
    insertTextFormat = Nothing
    insertTextMode = Nothing
    textEdit = Nothing
    textEditText = Nothing
    additionalTextEdits = Nothing
    commitChars = Just [T.pack "\t"]
    command = Nothing
    xdata = Nothing

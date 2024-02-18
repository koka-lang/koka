------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting, Fredrik Wieczerkowski
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The LSP handler that provides code completions
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module LanguageServer.Handler.Completion
  ( completionHandler,
  )
where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Map as M
import Data.ByteString ()
import Data.Char (isUpper, isAlphaNum)
import Data.Maybe (maybeToList, fromMaybe, fromJust)
import Data.Either (isRight)
import qualified Data.Text.Utf16.Rope as Rope
import qualified Data.Set as S
import qualified Data.Text as T
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Language.LSP.Server (Handlers, getVirtualFile, requestHandler)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.VFS (VirtualFile (VirtualFile), virtualFileText)
import Common.Name
    ( Name(..), ModuleName,
      isHiddenName,
      nameIsNil,
      showPlain,
      nameNil,
      qualify,
      newName )

import Debug.Trace(trace)
import qualified Data.Text.Encoding as T
import Data.Aeson (fromJSON, ToJSON (toJSON))
import Lib.PPrint (Pretty (..), Doc)
import Common.File (isLiteralDoc)
import Common.NamePrim (nameSystemCore)
import Common.Range
import Kind.Newtypes (Newtypes, DataInfo (..), newtypesTypeDefs)
import Kind.Kind (kindStar)
import Kind.Constructors (ConInfo (..), Constructors, constructorsList)
import Kind.Synonym      (SynInfo (..), Synonyms, synonymsToList)
import Type.Assumption   (Gamma, NameInfo(..), gammaList )
import Type.TypeVar (tvsEmpty)
import Type.Type
import Type.Unify (runUnify, unify, runUnifyEx, matchArguments)

import Syntax.Lexer (reservedNames)
import Syntax.Lexeme
import Syntax.RangeMap hiding (NameInfo)

import Language.LSP.Protocol.Types (InsertTextFormat(InsertTextFormat_Snippet))
import LanguageServer.Conversions (fromLspPos, fromLspUri, toLspPos, toLspRange)
import LanguageServer.Handler.Hover (formatRangeInfoHover)

import Compile.BuildContext
import LanguageServer.Monad

-- Gets tab completion results for a document location
-- This is a pretty complicated handler because it has to do a lot of work
completionHandler :: Handlers LSM
completionHandler
  = requestHandler J.SMethod_TextDocumentCompletion $ \req responder ->
    do  let J.CompletionParams doc pos _ _ context = req ^. J.params
            uri = J.toNormalizedUri (doc ^. J.uri)

            done :: LSM ()
            done = responder $ Right $ J.InR $ J.InR J.Null

            liftMaybe :: LSM (Maybe a) -> (a -> LSM ()) -> LSM ()
            liftMaybe action next = do res <- action
                                       case res of
                                          Nothing -> done
                                          Just x  -> next x

        liftMaybe (getVirtualFile uri) $ \vfile ->
          liftMaybe (lookupModuleName uri) $ \(fpath,modname) ->
            liftMaybe (lookupRangeMap modname) $ \(rmap,lexemes) ->
              liftMaybe (liftIO $ getCompletionInfo pos vfile rmap uri lexemes) $ \completionInfo ->
                do defs <- lookupVisibleDefinitions [modname]
                   let completions = findCompletions defs modname completionInfo
                       completionList = J.CompletionList False Nothing completions
                   responder $ Right $ J.InR $ J.InL $ completionList

-- | Describes the information gained from lexing needed to suggest completions
data CompletionInfo = CompletionInfo
  {
    fullLine :: !T.Text
  , cursorPos :: !J.Position
    -- ^ The cursor position
  , searchTerm :: !Name
  , searchRange :: !Range
    -- ^ The cursor position
  , argumentType :: Maybe Type
  -- Determines if it is a function completion (. is just prior to the cursor)
  , completionKind :: CompletionKind
  } deriving (Show,Eq)

data CompletionKind = CompletionKindFunction | CompletionKindValue | CompletionKindType | CompletionKindEffectLabel | CompletionKindTypeOrEffect deriving (Show,Eq)

isTypeCompletion :: CompletionKind -> Bool
isTypeCompletion CompletionKindType = True
isTypeCompletion CompletionKindTypeOrEffect = True
isTypeCompletion _ = False

getCompletionInfo :: MonadIO m => J.Position -> VirtualFile -> RangeMap -> J.NormalizedUri -> [Lexeme] -> m (Maybe CompletionInfo)
getCompletionInfo pos vf rmap uri lexemes = do
  let text = T.encodeUtf8 $ virtualFileText vf
  filePath <- fromMaybe "" <$> liftIO (fromLspUri uri)
  pos' <- liftIO $ fromLspPos uri pos
  let !prior = previousLexemesReversed lexemes pos'
      fncontext = getFunctionNameReverse prior
      tpcontext = dropAutoGenClosing prior -- TODO: Test this out, should it be opening?
      lines = T.lines (virtualFileText vf)
      row = case prior of
        [] -> 0
        (Lexeme rng tkn):_ -> fromIntegral $ posLine (rangeStart rng)
      line = if length lines < row then "" else lines !! (row - 1) -- rows are 1 indexed in koka
      endRng = rngEnd prior
  -- trace ("Prior: " ++ intercalate "\n" (map show (take 4 prior)) ++ " context " ++ show fncontext ++ " row" ++ show row ++ " pos: " ++ show pos' ++ "\n") $ return ()

  -- Matches all of the kinds of completions we support. Both without any partial name and with a partial name
  return $! case fncontext of
    -- Names followed by . (use the type of the name as a filter)
    FnIncomplete (FnValue (Lexeme rng (LexId nm))) -> completeFunction line nameNil endRng rng False
    FnChained (Lexeme rng0 (LexId partial)) (FnValue (Lexeme rng2 (LexId nm))) -> completeFunction line partial rng0 rng2 False
    FnIncomplete (FnChained (Lexeme rng (LexId nm)) _) -> completeFunction line nameNil endRng rng True
    FnChained (Lexeme rng0 (LexId partial)) (FnChained (Lexeme rng2 (LexId nm)) _) -> completeFunction line partial rng0 rng2 True
    FnIncomplete (FnValue (Lexeme rng (LexString _))) -> completeString line nameNil endRng
    FnChained (Lexeme rng0 (LexId partial)) (FnValue (Lexeme rng2 (LexString _))) -> completeString line partial rng0
    FnIncomplete (FnValue (Lexeme rng (LexInt _ _))) -> completeInt line nameNil endRng
    FnChained (Lexeme rng0 (LexId partial)) (FnValue (Lexeme rng2 (LexInt _ _))) -> completeInt line partial rng0
    FnIncomplete (FnValue (Lexeme rng (LexFloat _ _))) -> completeFloat line nameNil endRng
    FnChained (Lexeme rng0 (LexId partial)) (FnValue (Lexeme rng2 (LexFloat _ _))) -> completeFloat line partial rng0
    FnIncomplete (FnValue (Lexeme rng (LexChar _))) -> completeChar line nameNil endRng
    FnChained (Lexeme rng0 (LexId partial)) (FnValue (Lexeme rng2 (LexChar _))) -> completeChar line partial rng0
    FnIncomplete (FnValue (Lexeme rng (LexSpecial "]"))) -> completeList line nameNil endRng
    FnChained (Lexeme rng0 (LexId partial)) (FnValue (Lexeme rng2 (LexSpecial "]"))) -> completeList line partial rng0
    FnValue (Lexeme rng (LexId partial)) -> completeIdentifier line partial rng
    FnChained (Lexeme rng (LexId partial)) _ -> completeIdentifier line partial rng
    _ ->
      case tpcontext of
        -- closing paren followed by : (type or effect)
        (Lexeme rng1 (LexKeyword ":" _)):(Lexeme rng2 (LexSpecial ")")):_ -> completeTypeOrEffect line nameNil endRng
        (Lexeme rng0 (LexId partial)):(Lexeme rng1 (LexKeyword ":" _)):(Lexeme rng2 (LexSpecial ")")):_ -> completeTypeOrEffect line partial rng0
        -- : (type)
        (Lexeme rng1 (LexKeyword ":" _)):_ -> completeType line nameNil endRng
        (Lexeme rng0 (LexId partial)):(Lexeme rng1 (LexKeyword ":" _)):_ -> completeType line partial rng0
        _ -> Nothing
  where
    -- Range where to insert if there is no partial name to replace
    rngEnd prior = case prior of
      [] -> rangeNull
      (Lexeme rng _):_ ->
        -- Move to after the .
        let adjust = extendRange rng 1 in
        makeRange (rangeEnd adjust) (rangeEnd adjust)
    completeString line partial rng =
      return (CompletionInfo line pos partial rng (Just typeString) CompletionKindFunction)
    completeInt line partial rng =
      return (CompletionInfo line pos partial rng (Just typeInt) CompletionKindFunction)
    completeFloat line partial rng =
      return (CompletionInfo line pos partial rng (Just typeFloat) CompletionKindFunction)
    completeChar line partial rng =
      return (CompletionInfo line pos partial rng (Just typeChar) CompletionKindFunction)
    completeList line partial rng =
      let tyvar = TypeVar (-1) kindStar Skolem
          tvar  = TVar tyvar in
      return (CompletionInfo line pos partial rng (Just (TForall [tyvar] [] (TApp typeList [tvar]))) CompletionKindFunction)
    completeFunction line partial rnginsert rng resultOfFunction =
      let rm = rangeMapFind rng rmap
      in completeRangeInfo line partial rm rnginsert resultOfFunction
    completeType line partial rng =
      return (CompletionInfo line pos partial rng Nothing CompletionKindType)
    completeTypeOrEffect line partial rng =
      return (CompletionInfo line pos partial rng Nothing CompletionKindTypeOrEffect)
    completeRangeInfo line partial rm rnginsert resultOfFunction =
      case rm of
        [] -> return (CompletionInfo line pos partial rnginsert Nothing CompletionKindFunction)
        (r, rangeInfo):rst ->
          case rangeInfoType rangeInfo of
            Just t ->
              if not resultOfFunction then return (CompletionInfo line pos partial rnginsert (Just t) CompletionKindFunction)
              else
                case splitFunScheme t of
                  Just (_, _, _, _,res) ->
                    -- trace (" res: " ++ show res) $
                    return (CompletionInfo line pos partial rnginsert (Just res) CompletionKindFunction)
                  Nothing             -> return (CompletionInfo line pos partial rnginsert (Just t) CompletionKindFunction)
            Nothing -> completeRangeInfo line partial rst rnginsert resultOfFunction
    completeIdentifier line partial rnginsert = return (CompletionInfo line pos partial rnginsert Nothing CompletionKindValue)

-- TODO: Complete arguments
-- TODO: Complete local variables
-- TODO: Show documentation comments in completion docs

filterInfix :: (Name,CompletionInfo) -> Bool
filterInfix (n, cinfo) = (showPlain (searchTerm cinfo) `isInfixOf` showPlain n) && (nameIsNil n || not (isHiddenName n) || "@Hnd-" `isInfixOf` showPlain n)

findCompletions ::  Definitions -> ModuleName -> CompletionInfo -> [J.CompletionItem]
findCompletions defs curModName cinfo@CompletionInfo{completionKind = kind} = result
  where
    search = searchTerm cinfo
    gamma = defsGamma defs
    constrs = defsConstructors defs
    syns = defsSynonyms defs
    datatps = defsNewtypes defs
    completions =
      if kind == CompletionKindValue then valueCompletions curModName gamma cinfo else
        valueCompletions curModName gamma cinfo
        ++ constructorCompletions curModName constrs
        ++ synonymCompletions cinfo curModName syns
        ++ datatypeCompletions cinfo curModName datatps
    filtered = map snd $ filter (\(n, i) -> filterInfix (n, cinfo)) completions
    result = if kind == CompletionKindFunction then filtered else keywordCompletions cinfo curModName ++ filtered

typeUnifies :: Type -> Maybe Type -> Name -> Bool
typeUnifies t1 t2 name =
  case t2 of
    Nothing -> True
    Just t2 ->
      let (res, _, _) = (runUnifyEx 0 $ matchArguments True rangeNull tvsEmpty t1 [t2] [] Nothing)
          typeMatches = isRight res in
        -- if name == qualify nameSystemCore (newName "join") then trace ("t1: " ++ show t1 ++ " t2: " ++ show t2 ++ " " ++ show typeMatches) typeMatches
        -- else
          typeMatches

valueCompletions :: Name -> Gamma -> CompletionInfo -> [(Name, J.CompletionItem)]
valueCompletions curModName gamma cinfo@CompletionInfo{argumentType=tp, searchTerm=search, completionKind, searchRange=searchRange}
  = let lspRng = toLspRange searchRange in
    if CompletionKindFunction == completionKind || CompletionKindValue == completionKind then
      map (toItem lspRng) . filter matchInfo $ filter (\(n, ni) -> filterInfix (n, cinfo)) $ gammaList gamma
    else []
  where
    matchInfo :: (Name, NameInfo) -> Bool
    matchInfo (n, ninfo) = case ninfo of
        InfoVal {infoType} -> typeUnifies infoType tp n
        InfoFun {infoType} -> typeUnifies infoType tp n
        InfoExternal {infoType} -> typeUnifies infoType tp n
        InfoImport {infoType} -> typeUnifies infoType tp n
        InfoCon {infoType } -> typeUnifies infoType tp n
    toItem lspRng (n, ninfo) = case ninfo of
        -- We only let hidden names get to this point if they are handlers
        InfoCon {infoCon} | isHiddenName n -> (n, makeHandlerCompletionItem curModName infoCon typeDoc lspRng (fullLine cinfo))
        InfoFun {infoType} -> (n, makeFunctionCompletionItem curModName n typeDoc infoType (completionKind == CompletionKindFunction) lspRng (fullLine cinfo))
        InfoVal {infoType} -> case splitFunScheme infoType of
          Just (tvars, tpreds, pars, eff, res) -> (n, makeFunctionCompletionItem curModName n typeDoc infoType (completionKind == CompletionKindFunction) lspRng (fullLine cinfo))
          Nothing -> (n, makeCompletionItem curModName n kind typeDoc)
        _ -> (n, makeCompletionItem curModName n kind typeDoc)
      where
        kind = case ninfo of
          InfoVal {..} -> J.CompletionItemKind_Constant
          InfoFun {..} -> J.CompletionItemKind_Function
          InfoExternal {..} -> J.CompletionItemKind_Reference
          InfoImport {..} -> J.CompletionItemKind_Module
          InfoCon {infoCon = ConInfo {conInfoParams = ps}}
            | not (null ps) -> J.CompletionItemKind_Constructor
            | otherwise -> J.CompletionItemKind_EnumMember
        typeDoc = show $ pretty $ infoType ninfo

constructorCompletions :: Name -> Constructors -> [(Name, J.CompletionItem)]
constructorCompletions curModName cstrs = map toItem $ filter (\(n,ci) -> not (isHiddenName n)) (constructorsList cstrs)
  where
    toItem (n, cinfo) = (n, makeCompletionItem curModName n k d)
      where
        ps = conInfoParams cinfo
        k
          | not (null ps) = J.CompletionItemKind_Constructor
          | otherwise = J.CompletionItemKind_EnumMember
        d = show $ pretty $ conInfoType cinfo

synonymCompletions :: CompletionInfo -> Name -> Synonyms -> [(Name, J.CompletionItem)]
synonymCompletions cinfo curModName syns = if isTypeCompletion (completionKind cinfo) then map toItem (synonymsToList syns) else []
  where
    toItem sinfo = (name, makeCompletionItem curModName name J.CompletionItemKind_Interface details)
      where
        name = synInfoName sinfo
        details = show $ pretty $ synInfoType sinfo

datatypeCompletions :: CompletionInfo -> Name -> Newtypes -> [(Name, J.CompletionItem)]
datatypeCompletions cinfo curModName ntps = if isTypeCompletion (completionKind cinfo) then map (toItem . snd) (M.toList (newtypesTypeDefs ntps)) else []
  where
    toItem dinfo = (name, makeCompletionItem curModName name J.CompletionItemKind_Interface details)
      where
        name = dataInfoName dinfo
        details = showPlain name

keywordCompletions :: CompletionInfo -> Name -> [J.CompletionItem]
keywordCompletions cinfo curModName  = if completionKind cinfo == CompletionKindValue then map toItem $ S.toList reservedNames else []
  where
    toItem s = makeSimpleCompletionItem curModName s J.CompletionItemKind_Keyword

makeCompletionItem :: Name -> Name -> J.CompletionItemKind -> String -> J.CompletionItem
makeCompletionItem curModName name0 kind0 details =
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
    label = T.pack $ nameStem name0
    labelDetails = Nothing
    kind = Just kind0
    tags = Nothing
    detail = Just $  T.pack details
    doc = Just $ J.InL $ T.pack $ nameModule name0
    deprecated = Just False
    preselect = Nothing
    sortText = Just $ if nameModule curModName == nameModule name0 then T.pack "0" <> label else T.pack "2" <> label
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
makeFunctionCompletionItem curModName funName typeDoc funType hasDotPrefix rng line =
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
      label = T.pack $ nameStem funName
      indentation = T.length $ T.takeWhile (== ' ') line
      trailingFunIndentation = T.replicate indentation " "
      labelDetails = Nothing
      kind = Just J.CompletionItemKind_Snippet
      tags = Nothing
      detail = Just $  T.pack typeDoc
      doc = Just $ J.InR ""
      deprecated = Just False
      preselect = Nothing
      sortText = Just $ if nameModule curModName == nameModule funName then "0" <> label else "2" <> label
      filterText = Just label
      insertText = Nothing
      insertTextFormat = Just InsertTextFormat_Snippet
      insertTextMode = Nothing
      arguments = case splitFunScheme funType
        of Just (tvars, tpreds, pars, eff, res) -> pars
           Nothing -> []
      numArgs = length arguments - (if hasDotPrefix then 1 else 0)
      -- trailingFunArgTp = case arguments
      --   of [] -> Nothing
      --      xs -> let arg = last xs
      --       in case splitFunScheme (snd arg) of
      --         Nothing -> Nothing
      --         Just (_, _, args, _, _) -> Just args
      argumentsText =
        if numArgs == 0 then -- trace ("No function arguments for " ++ show label ++ " " ++ show (pretty funType)) $
          T.pack ""
        -- else case trailingFunArgTp of
        else "(" <> T.intercalate "," (map (\i -> T.pack $ "$" ++ show i) [1..numArgs]) <> ")"
          -- Just tp ->
          --   let mainArgs = "(" <> T.intercalate "," (map (\i -> T.pack $ "$" ++ show i) [1..numArgs-1]) <> ")"
          --   in mainArgs <> " fn(" <> T.intercalate "," (map (\i -> T.pack $ "$" ++ show i) [numArgs..numArgs+length tp-1]) <> ")\n" <> trailingFunIndentation <> "()"
      textEdit = Just $ J.InL $ J.TextEdit rng $ label <> argumentsText
      textEditText = Nothing
      additionalTextEdits = Nothing
      commitChars = Just [T.pack "\t"]
      command = Just (J.Command "Signature Help" "koka/signature-help/set-context" (Just [toJSON (SignatureContext funName)]))
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
    typeNameId = T.replace "@hnd-" "" $ T.pack $ nameStem typeName
    label = "handler for " <> typeNameId
    labelDetails = Nothing
    kind = Just J.CompletionItemKind_Snippet
    tags = Nothing
    detail = Just $  T.pack d
    doc = Just $ J.InL $ T.pack $ nameModule typeName
    deprecated = Just False
    preselect = Nothing
    sortText = Just $ if nameModule curModName == nameModule typeName then "0" <> typeNameId else "1" <> typeNameId
    filterText = Just typeNameId
    insertText = Nothing
    insertTextFormat = Just InsertTextFormat_Snippet
    insertTextMode = Nothing
    handlerClause :: (Int, [T.Text]) -> (Name, Type) -> (Int, [T.Text])
    handlerClause (i, acc) (name, tp) =
      -- trace ("Handler clause: " ++ show name ++ " " ++ show tp ++ " args: " ++ show (handlerArgs newName tp)) $
      -- TODO: Consider adding snippet locations for the body of the handlers as well
      if T.isPrefixOf "val" newName then
        (i + 1, acc ++ [clauseIndentation <> newName <> " = $" <> T.pack (show (i + 1))])
      else
        (if not (null funArgs) then fst (last funArgs) + 1 else 1,
          acc ++ [clauseIndentation <> newName <> "(" <> T.intercalate "," (map snd funArgs) <> ")\n" <> clauseBodyIndentation <> "()"])
      where
        funArgs = zipWith (\i s -> (i, T.pack $ "$" ++ show (i + 1))) [i..] (handlerArgs newName tp)
        newNameList = T.splitOn "-" $ T.replace "brk" "final ctl" $ T.pack (show name)
        newName = case newNameList of
          [] -> T.pack ""
          x:tl -> x <> T.pack " " <> T.intercalate (T.pack "-") tl
    textEdit = Just $ J.InL $ J.TextEdit r $ "handler\n" <> T.intercalate "\n" (snd (foldl handlerClause (1, []) (conInfoParams conInfo)))
    textEditText = Nothing
    additionalTextEdits = Nothing
    commitChars = Just [T.pack "\t"]
    command = Nothing
    xdata = Nothing

handlerArgs :: T.Text -> Type -> [Type]
handlerArgs name tp =
  case splitPredType tp of
    (_,_,TApp _ args) ->
      if T.isPrefixOf "val" name then take (length args - 3) args else take (length args - 4) args
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

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
    ( Name(..),
      nameLocal,
      isHiddenName,
      nameIsNil,
      showPlain,
      nameNil, qualify )
import Common.Range (makePos, posNull, Range(..), rangeEnd, rangeStart, rangeNull, Source (..), extractLiterate, Pos(..), rangeLength, makeRange, extendRange)
import Lib.PPrint (Pretty (..))
import Kind.Constructors (ConInfo (..), Constructors, constructorsList)
import Kind.Synonym (SynInfo (..), Synonyms, synonymsToList)
import Type.Assumption
import Type.InferMonad (subst, instantiate)
import Type.TypeVar (tvsEmpty)
import Type.Type (Type(..), splitFunType, splitFunScheme, typeString, typeInt, typeFloat, typeChar, typeList, TypeVar (..), Flavour (..))
import Type.Unify (runUnify, unify, runUnifyEx, matchArguments)
import Compiler.Compile (Module (..))
import Compiler.Module ( Loaded(..), modLexemes )
import Syntax.Lexer (reservedNames, lexing, Lexeme (..), Lex (..))
import Syntax.Lexeme
import Syntax.RangeMap (rangeMapFind, rangeInfoType)
import Syntax.Parse (parseProgramFromFile, parseProgramFromString)
import Syntax.Layout (layout)
import Language.LSP.Protocol.Types (InsertTextFormat(InsertTextFormat_Snippet))
import LanguageServer.Conversions (fromLspPos, fromLspUri, toLspPos, toLspRange)
import LanguageServer.Monad (LSM, getLoaded, getLoadedModule)
import qualified Data.Text.Encoding as T
import Common.File (isLiteralDoc)
import Lib.Trace (trace)
import Kind.Newtypes (Newtypes, DataInfo (..), newtypesTypeDefs)
import Kind.Kind (kindStar)
import Common.NamePrim (nameSystemCore)
import Common.Name (newName)

-- Gets tab completion results for a document location
-- This is a pretty complicated handler because it has to do a lot of work
completionHandler :: Handlers LSM
completionHandler = requestHandler J.SMethod_TextDocumentCompletion $ \req responder -> do
  let J.CompletionParams doc pos _ _ context = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  loaded <- getLoaded normUri
  loadedM <- getLoadedModule normUri
  vfile <- getVirtualFile normUri
  let maybeRes = do -- maybeMonad
        l <- loaded
        lm <- loadedM
        vf <- vfile
        return (l, lm, vf)
  items <- case maybeRes of
    Just (l, lm, vf) -> do
      completionInfo <- liftIO $ getCompletionInfo pos vf lm normUri
      -- trace ("Completion info: " ++ show completionInfo) $ return ()
      case completionInfo of
        Just info ->
          let completions = findCompletions l lm info
          in-- trace (show completions) $P
            return completions
        _ -> -- trace ("No completion infos for position ")
          return []
    _ -> return []
  responder $ Right $ J.InL items

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

previousLexemes :: [Lexeme] -> [Lexeme] -> Pos -> [Lexeme]
previousLexemes !lexemes !acc !pos =
  case lexemes of
    [] -> acc
    (lex@(Lexeme rng _):rst) ->
      if rangeEnd rng >= pos && rangeStart rng <= pos then
        lex:acc
      else if rangeEnd rng < pos then
        previousLexemes rst (lex:acc) pos
      else
        acc

-- Drop matchings () pairs, also drop to ;
-- e.g.
--   a.b(x, y, fn() {z}). drops to
--   a.b.
-- this way we know that we are completing a function whose first argument is the result of b
-- If we were doing this instead
--   a. and a is a function type we know that we are completing a function whose first argument is actually the function type a
-- Working with lambda literals is a TODO:
dropMatched :: [Lexeme] -> [Lexeme]
dropMatched xs =
  case xs of
    [] -> []
    (Lexeme x' LexInsSemi):xs -> []
    (Lexeme x' (LexSpecial ";"):xs) -> []
    (Lexeme x' LexInsLCurly):xs -> []
    (Lexeme x' (LexSpecial "{"):xs) -> []
    (Lexeme x' (LexSpecial ")")):xs -> dropToLex (LexSpecial "(") xs

    x:xs -> x:dropMatched xs
  where
    dropToLex x xs =
      case xs of
        [] -> []
        (Lexeme x' l):xs | l == x -> dropMatched xs
        (Lexeme x' l):xs -> dropToLex x xs

dropAutoGen :: [Lexeme] -> [Lexeme]
dropAutoGen lexes =
  case lexes of
    [] -> []
    (Lexeme x' LexInsSemi):xs -> dropAutoGen xs
    (Lexeme x' LexInsRCurly):xs -> dropAutoGen xs
    _ -> lexes

data CompletionKind = CompletionKindFunction | CompletionKindValue | CompletionKindType | CompletionKindEffectLabel | CompletionKindTypeOrEffect deriving (Show,Eq)

isTypeCompletion :: CompletionKind -> Bool
isTypeCompletion CompletionKindType = True
isTypeCompletion CompletionKindTypeOrEffect = True
isTypeCompletion _ = False

getCompletionInfo :: MonadIO m => J.Position -> VirtualFile -> Module -> J.NormalizedUri -> m (Maybe CompletionInfo)
getCompletionInfo pos vf mod uri = do
  let text = T.encodeUtf8 $ virtualFileText vf
  filePath <- fromMaybe "" <$> liftIO (fromLspUri uri)
  pos' <- liftIO $ fromLspPos uri pos
  let source = Source filePath text
      input  = if isLiteralDoc filePath then extractLiterate text else text
      xs = lexing source 1 input
      lexemes = layout False {-no at-} True {-semi insert-} xs
      !prior = previousLexemes lexemes [] pos'
      context = dropMatched (dropAutoGen prior)
      lines = T.lines (virtualFileText vf)
      row = case prior of
        [] -> 0
        (Lexeme rng tkn):_ -> fromIntegral $ posLine (rangeStart rng)
      line = if length lines < row then "" else lines !! (row - 1) -- rows are 1 indexed in koka
      endRng = rngEnd prior
  -- trace ("Prior: " ++ intercalate "\n" (map show (take 4 prior)) ++ " context " ++ intercalate "\n" (map show context)  ++ " row" ++ show row ++ " pos: " ++ show pos' ++ "\n") $ return ()
  
  -- Matches all of the kinds of completions we support. Both without any partial name and with a partial name
  return $! case context of
    -- Names followed by . (use the type of the name as a filter)
    [(Lexeme rng1 (LexKeyword "." _)), (Lexeme rng2 (LexId nm))] -> completeFunction line nameNil endRng rng2 False
    [(Lexeme rng0 (LexId partial)), (Lexeme rng1 (LexKeyword "." _)), (Lexeme rng2 (LexId nm))] -> completeFunction line partial rng0 rng2 False
    -- Names followed by . (but with a chain of prior names) i.e. a.b.c (use the result type of the name as a filter)
    (Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexId nm)):_ -> completeFunction line nameNil endRng rng2 True
    (Lexeme rng0 (LexId partial)):(Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexId nm)):_-> completeFunction line partial rng0 rng2 True
    -- Strings followed by .
    (Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexString _)):_ -> completeString line nameNil endRng
    (Lexeme rng0 (LexId partial)):(Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexString _)):_ -> completeString line partial rng0
    -- Chars followed by .
    (Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexChar _)):_ -> completeChar line nameNil endRng
    (Lexeme rng0 (LexId partial)):(Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexChar _)):_ -> completeChar line partial rng0
    -- Ints followed by .
    (Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexInt _ _)):_ -> completeInt line nameNil endRng
    (Lexeme rng0 (LexId partial)):(Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexInt _ _)):_ -> completeInt line partial rng0
    -- Floats followed by .
    (Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexFloat _ _)):_ -> completeFloat line nameNil endRng
    (Lexeme rng0 (LexId partial)):(Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexFloat _ _)):_ -> completeFloat line partial rng0
    -- Lists followed by .
    (Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexSpecial "]")):_ -> completeList line nameNil endRng
    (Lexeme rng0 (LexId partial)):(Lexeme rng1 (LexKeyword "." _)):(Lexeme rng2 (LexSpecial "]")):_ -> completeList line partial rng0
    -- closing paren followed by : (type or effect)
    (Lexeme rng1 (LexKeyword ":" _)):(Lexeme rng2 (LexSpecial ")")):_ -> completeTypeOrEffect line nameNil endRng
    (Lexeme rng0 (LexId partial)):(Lexeme rng1 (LexKeyword ":" _)):(Lexeme rng2 (LexSpecial ")")):_ -> completeTypeOrEffect line partial rng0
    -- : (type)
    (Lexeme rng1 (LexKeyword ":" _)):_ -> completeType line nameNil endRng
    (Lexeme rng0 (LexId partial)):(Lexeme rng1 (LexKeyword ":" _)):_ -> completeType line partial rng0
    -- plain identifier - suggest all values that contain identifier regardless of context type
    (Lexeme rng (LexId partial)):_ -> completeIdentifier line partial rng
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
      let rm = rangeMapFind rng (fromJust $ modRangeMap mod)
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

findCompletions :: Loaded -> Module -> CompletionInfo -> [J.CompletionItem]
findCompletions loaded mod cinfo@CompletionInfo{completionKind = kind} = result
  where
    curModName = modName mod
    search = searchTerm cinfo
    gamma = loadedGamma loaded
    constrs = loadedConstructors loaded
    syns = loadedSynonyms loaded
    datatps = loadedNewtypes loaded
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
        InfoCon {infoCon} | isHiddenName n -> (n, makeHandlerCompletionItem curModName infoCon d lspRng (fullLine cinfo))
        InfoFun {infoType} -> (n, makeFunctionCompletionItem curModName n d infoType (completionKind == CompletionKindFunction) lspRng (fullLine cinfo))
        InfoVal {infoType} -> case splitFunScheme infoType of
          Just (tvars, tpreds, pars, eff, res) -> (n, makeFunctionCompletionItem curModName n d infoType (completionKind == CompletionKindFunction) lspRng (fullLine cinfo))
          Nothing -> (n, makeCompletionItem curModName n k d)
        _ -> (n, makeCompletionItem curModName n k d)
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
    toItem sinfo = (n, makeCompletionItem curModName n J.CompletionItemKind_Interface d)
      where
        n = synInfoName sinfo
        d = show $ pretty $ synInfoType sinfo

datatypeCompletions :: CompletionInfo -> Name -> Newtypes -> [(Name, J.CompletionItem)]
datatypeCompletions cinfo curModName ntps = if isTypeCompletion (completionKind cinfo) then map (toItem . snd) (M.toList (newtypesTypeDefs ntps)) else []
  where
    toItem dinfo = (n, makeCompletionItem curModName n J.CompletionItemKind_Interface d)
      where
        n = dataInfoName dinfo
        d = show $ pretty n

keywordCompletions :: CompletionInfo -> Name -> [J.CompletionItem]
keywordCompletions cinfo curModName  = if completionKind cinfo == CompletionKindValue then map toItem $ S.toList reservedNames else []
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
    label = T.pack $ nameLocal n
    labelDetails = Nothing
    kind = Just k
    tags = Nothing
    detail = Just $  T.pack d
    doc = Just $ J.InL $ T.pack $ nameModule n
    deprecated = Just False
    preselect = Nothing
    sortText = Just $ if nameModule curModName == nameModule n then T.pack $ "0" ++ nameLocal n else T.pack $ "2" ++ nameLocal n
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
      label = T.pack $ nameLocal funName
      indentation = T.length $ T.takeWhile (== ' ') line
      trailingFunIndentation = T.replicate indentation " "
      labelDetails = Nothing
      kind = Just J.CompletionItemKind_Snippet
      tags = Nothing
      detail = Just $  T.pack d
      doc = Just $ J.InL $ T.pack $ nameModule funName
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
      numArgs = length arguments - (if accessor then 1 else 0)
      trailingFunArgTp = case arguments
        of [] -> Nothing
           xs -> let arg = last xs
            in case splitFunScheme (snd arg) of
              Nothing -> Nothing
              Just (_, _, args, _, _) -> Just args
      argumentsText =
        if numArgs == 0 then -- trace ("No function arguments for " ++ show label ++ " " ++ show (pretty funType)) $
          T.pack ""
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
    typeNameId = T.replace "@hnd-" "" $ T.pack $ nameLocal typeName
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
  case tp of 
    TApp _ args -> 
      -- trace ("Hey these are the args" ++ show args) $
      if T.isPrefixOf "val" name then take (length args - 3) args else take (length args - 4) args
    TForall _ _ t -> handlerArgs name t


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

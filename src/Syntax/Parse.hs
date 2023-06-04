------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Parse concrete syntax.
-}
-----------------------------------------------------------------------------
module Syntax.Parse( parseProgramFromFile, parseProgramFromString
                   , parseValueDef
                   , parseTypeDef
                   , parseExpression
                   , parseType

                   -- used by the core parser
                   , lexParse, parseLex, LexParser, parseLexemes, parseInline, ignoreSyntaxWarnings

                   , visibility, modulepath, importAlias, parseFip
                   , tbinderId, constructorId, funid, paramid
                   , braced, semiBraces, semis, semiColons1, semiBraced
                   , angles, anglesCommas, parensCommas, parens, curlies
                   , semiColon, lparen, rparen, langle, rangle, comma, lapp, lidx, bar
                   , qtypeid, qvarid, qconid, qidop, identifier, qoperator, varid, idop
                   , integer, charLit, floatLit, stringLit
                   , special, specialId, specialOp, specialConId, wildcard
                   , keyword, dockeyword
                   , typeDeclKind
                   , paramInfo
                   ) where

import Lib.Trace
import Data.List (intersperse,unzip4,sortBy)
import Data.Maybe (isJust,isNothing,catMaybes)
import Data.Either (partitionEithers)
import Lib.PPrint hiding (string,parens,integer,semiBraces,lparen,comma,angles,rparen,rangle,langle)
import qualified Lib.PPrint as PP (string)

import Control.Monad (mzero,when)
import Data.Monoid (Endo(..))
import Text.Parsec hiding (space,tab,lower,upper,alphaNum,sourceName,optional)
import Text.Parsec.Error
import Text.Parsec.Pos           (newPos)

import Common.Error as Err
import Common.Name
import Common.NamePrim
import Common.Range hiding (after)
import Common.File
import Platform.Config
import Platform.Runtime( unsafePerformIO, exCatch )
import Common.Error
import Common.Failure (failure)
import Common.Syntax
import Common.ResumeKind

import Syntax.Syntax
import Syntax.Lexeme
import Syntax.Lexer   ( lexing )
import Syntax.Layout  ( layout )
import Syntax.Promote ( promote, promoteType, quantify, promoteFree )
import Common.ColorScheme (defaultColorScheme)

-----------------------------------------------------------
-- Parser on token stream
-----------------------------------------------------------

type LexParser a  = Parsec [Lexeme] [(String, Range)] a -- GenParser Lexeme () a

parseLex :: Lex -> LexParser Lexeme
parseLex lex
  = token showTok posFromTok testTok
  where
    showTok (Lexeme _ lex)       = show lex
    posFromTok (Lexeme range _)  = newPos "" (posLine (rangeStart range)) (posColumn (rangeStart range))
    testTok l@(Lexeme _ lex')    | sameLex lex lex'  = Just l
                                 | otherwise         = Nothing


optional p  = do { p; return True } <|> return False

-----------------------------------------------------------
-- Parse varieties
-----------------------------------------------------------
parseProgramFromFile :: Bool -> FilePath -> IO (Error UserProgram)
parseProgramFromFile semiInsert fname
  = do input <- readInput fname
       let result = parseProgramFromString semiInsert input fname
       case checkError result of
          Right (a, warnings) -> 
            do 
              logSyntaxWarnings warnings
              return result
          Left err            -> return result

logSyntaxWarnings :: [(Range, Doc)] -> IO ()
logSyntaxWarnings warnings
  = putPretty (prettyWarnings True defaultColorScheme warnings) 

parseProgramFromString :: Bool -> BString -> FilePath -> Error UserProgram
parseProgramFromString semiInsert input fname
  = do (result, syntaxWarnings) <- lexParse semiInsert id program fname 1 input
       addWarnings (map (\(s, r) -> (r, text s)) syntaxWarnings) $ return result

parseValueDef :: Bool -> FilePath -> Int -> String -> Error UserDef
parseValueDef semiInsert sourceName line input
  = lexParseS semiInsert (const valueDefinition)  sourceName line input

parseTypeDef :: Bool -> FilePath -> Int -> String -> Error (UserTypeDef,[UserDef])
parseTypeDef semiInsert sourceName line input
  = lexParseS semiInsert (const typeDefinition)  sourceName line input

parseType :: Bool -> FilePath -> Int -> Name -> String -> Error UserTypeDef
parseType semiInsert sourceName line name input
  = lexParseS semiInsert (const (userType name))  sourceName line input

parseExpression :: Bool -> FilePath -> Int -> Name -> String -> Error UserDef
parseExpression semiInsert sourceName line name input
  = lexParseS semiInsert (const (expression name))  sourceName line input

ignoreSyntaxWarnings :: Error (a, [(String, Range)]) -> Error a
ignoreSyntaxWarnings result =
  do (x, syntaxWarnings) <- result
     return x

lexParseS :: Bool -> (Source -> LexParser b) -> FilePath -> Int -> String -> Error b
lexParseS semiInsert p sourceName line str
  = do
      (result, syntaxWarnings) <- (lexParse semiInsert id p sourceName line (stringToBString str))
      return $ trace (concat (intersperse "\n" (map fst syntaxWarnings))) $ result

runStateParser :: LexParser a -> SourceName -> [Lexeme] -> Either ParseError (a, [(String, Range)])
runStateParser p sourceName lex = 
  runParser (pp p) [] sourceName lex
  where 
    pp p =
      do r <- p
         s <- getState
         return (r, s)

lexParse :: Bool -> ([Lexeme]-> [Lexeme]) -> (Source -> LexParser a) -> FilePath -> Int -> BString -> Error (a, [(String, Range)])
lexParse semiInsert preprocess p sourceName line rawinput
  = let source = Source sourceName rawinput
        input  = if (isLiteralDoc sourceName) then extractLiterate rawinput else rawinput
        xs = lexing source line input
        lexemes = preprocess $ layout semiInsert xs
    in  -- trace  (unlines (map show lexemes)) $
        case (runStateParser (p source) sourceName lexemes) of
          Left err -> makeParseError (errorRangeLexeme xs source) err
          Right x  -> return x

parseLexemes :: LexParser a -> Source -> [Lexeme] -> Error (a, [(String, Range)])
parseLexemes p source@(Source sourceName _) lexemes
  = case (runStateParser p sourceName lexemes) of
      Left err -> makeParseError (errorRangeLexeme lexemes source) err
      Right x  -> return x


makeParseError :: (ParseError -> Range) -> ParseError -> Error a
makeParseError toRange perr
  = errorMsg (ErrorParse (toRange perr) errorDoc)
  where
    errorDoc
      = PP.string ("invalid syntax" ++ (drop 1 $ dropWhile (/=':') $ show perr))


errorRangeLexeme :: [Lexeme] -> Source -> ParseError -> Range
errorRangeLexeme lexemes source perr
  = case dropWhile (\r -> r < range) (map getRange lexemes) of
      (lrange : _) | rangeStart lrange == rangeStart range  -> lrange
      _            -> range
  where
    range = makeRange pos pos
    pos = makePos source (-1) (sourceLine (errorPos perr)) (sourceColumn (errorPos perr))


{--------------------------------------------------------------------------
  Interactive
--------------------------------------------------------------------------}
interactive :: LexParser a -> LexParser a
interactive p
  = do x <- p
       many semiColon
       eof
       return x

valueDefinition :: LexParser UserDef
valueDefinition
  = interactive (pureDecl Private)


typeDefinition :: LexParser (UserTypeDef,[UserDef])
typeDefinition
  = interactive (do tdef <- aliasDecl Public
                    return (tdef,[])
                 <|>
                 typeDecl Public)

expression :: Name -> LexParser (UserDef) -- ,UserDef)
expression name
  = interactive $
    do e <- aexpr
       let r = getRange e
       return (Def (ValueBinder name () (Lam [] e r) r r)  r Public (DefFun [] noFip) InlineNever ""
              -- ,Def (ValueBinder (prepend ".eval" name) () (Lam [] (App (Var nameGPrint False r) [Var name False r] r)))
              )

userType :: Name -> LexParser UserTypeDef
userType name
  = interactive $
    do tp <- ptype
       let rng = getRange tp
       return (Synonym (TypeBinder name KindNone rangeNull rangeNull) [] tp rng Public "")


-----------------------------------------------------------
-- Program
-----------------------------------------------------------
program :: Source -> LexParser UserProgram
program source
  = do many semiColon
       p <- pmodule source
       eof
       return p


pmodule :: Source -> LexParser UserProgram
pmodule source
  = do (vis,rng,doc) <- try $ do (vis,_) <- visibility Private
                                 -- if (vis == Public) then pwarningMessage "using 'public module' is deprecated" else return ()
                                 (rng,doc) <- dockeyword "module"
                                 return (vis,rng,doc)
       -- (rng,doc) <- dockeyword "module"
       (name,rng) <- modulepath
       programBody vis source name rng doc
  <|>
    programBody Public source (pathToModuleName (noexts (basename (sourceName source)))) (rangeNull) ""

programBody vis source modName nameRange doc
  = do many semiColon
       (imports, fixDefss, topDefss)
          <- braced (do imps <- semis importDecl
                        fixs <- semis fixDecl
                        tdefs <- semis (topdef vis)
                        return (imps,fixs,tdefs))
       many semiColon
       let (defs,typeDefs,externals) = splitTopDefs (concat topDefss)
       return (Program source modName nameRange [TypeDefRec typeDefs] [DefRec defs]
                 (prelude ++ imports) externals (concat fixDefss) doc)
  where
    prelude = if (show modName `startsWith` "std/core")
               then []
               else [Import nameSystemCore nameSystemCore rangeNull Private]

braced p
  = do lcurly
       many semiColon
       x <- p
       many semiColon
       rcurly
       return x
  <|>
    do parseLex LexInsLCurly
       many semiColon
       x <- p
       many semiColon
       parseLex LexInsRCurly
       return x
  <|>
    p

-- collect definitions
data TopDef
  = DefValue   UserDef
  | DefType    UserTypeDef
  | DefExtern  External

splitTopDefs ds
  = fold ([],[],[]) ds
  where
    fold (defs,tdefs,edefs) []  = (reverse defs, reverse tdefs, reverse edefs)
    fold (defs,tdefs,edefs) (d:ds)
      = case d of
          DefValue def  -> fold (def:defs,  tdefs, edefs) ds
          DefType tdef  -> fold (defs, tdef:tdefs, edefs) ds
          DefExtern edef-> fold (defs, tdefs, edef:edefs) ds


topdef :: Visibility -> LexParser [TopDef]
topdef vis
  = do def <- pureDecl vis
       return [DefValue def]
  <|>
    do tdef <- aliasDecl vis
       return [DefType tdef]
  <|>
    do (tdef,cdefs) <- typeDecl vis
       return ([DefType tdef] ++ map DefValue cdefs)
  <|>
    do effectDecl vis
  <|>
    do externDecl vis

{---------------------------------------------------------------
  Import declaration
---------------------------------------------------------------}
importDecl :: LexParser Import
importDecl
  = do (vis,vrng,rng0) <- try $ do (vis,vrng) <- visibility Private
                                   rng0  <- keyword "import"
                                   return (vis,vrng,rng0)
       (asname,name,rng) <- importAlias
       return (Import asname name (combineRanges [vrng,rng0,rng]) vis)

importAlias :: LexParser (Name,Name,Range)
importAlias
  = do (name1,rng1) <- modulepath
       (do keyword "="
           (name2,rng2) <- modulepath
           return (name1,name2,rng2)
        <|> return (name1,name1,rng1))



visibility :: Visibility -> LexParser (Visibility,Range)
visibility vis
  =   do rng <- keywordOr "pub" ["public"]
         return (Public,rng)
  <|> do rng <- keyword "private" 
         pwarningMessage "using 'private' is deprecated, only use 'pub' to make declarations public" rng
         return (Private,rng)
  <|> return (vis,rangeNull)


parseInline :: LexParser DefInline
parseInline
  =   do{ specialId "inline"; return InlineAlways }
  <|> do{ specialId "noinline"; return InlineNever }
  <|> return InlineAuto

{--------------------------------------------------------------------------
  External
--------------------------------------------------------------------------}
externDecl :: Visibility -> LexParser [TopDef]
externDecl dvis
  = do lr <- try ( do (krng,_) <- dockeyword "extern"
                      keyword "import"
                      return (Left (externalImport krng)))
            <|>
             try ( do (krng,_) <- dockeyword "extern"
                      specialId "include"
                      warnDeprecated "include" "import" krng
                      return (Left (externalImport krng)))
            <|>
             try ( do (vis,vrng) <- visibility dvis
                      inline     <- parseInline
                      fip        <- parseFip
                      (krng,doc) <- dockeyword "extern"
                      return (Right (combineRange vrng krng, vis, doc, inline, fip)))
       case lr of
         Left p -> do extern <- p
                      return [DefExtern extern]
         Right (krng,vis,doc,inline,fip)
           -> do (name,nameRng) <- funid
                 (pars,pinfos,args,tp,annotate)
                   <- do keyword ":"
                         tp <- ptype  -- no "some" allowed
                         (pars,args) <- genParArgs (promoteType tp)
                         return (pars,[]{-all owned-},args,tp,\body -> Ann body tp (getRange tp))
                      <|>
                      do tpars <- typeparams
                         (pars, pinfos, parRng) <- declParams True {-allowBorrow-} (inline /= InlineAlways) -- allow defaults? 
                         (teff,tres)   <- annotResult
                         let tp = typeFromPars nameRng pars teff tres
                             lift :: ValueBinder UserType (Maybe UserExpr) -> ValueBinder (Maybe UserType) (Maybe UserExpr)
                             lift (ValueBinder name tp expr rng1 rng2) = ValueBinder name (Just tp) expr rng1 rng2
                         genParArgs tp -- checks the type
                         return (map lift pars,pinfos,genArgs pars,tp,\body -> promote [] tpars [] (Just (Just teff, tres)) body)
                 (exprs,rng) <- externalBody
                 if (inline == InlineAlways)
                  then return [DefExtern (External name tp pinfos nameRng (combineRanges [krng,rng]) exprs vis fip doc)]
                  else do let  externName = newHiddenExternalName name
                               fullRng    = combineRanges [krng,rng]
                               extern     = External externName tp pinfos (before nameRng) (before fullRng) exprs Private fip doc
                               body       = annotate (Lam pars (App (Var externName False rangeNull) args fullRng) fullRng)
                               binder     = ValueBinder name () body nameRng fullRng
                               extfun     = Def binder fullRng vis (defFunEx pinfos fip) InlineNever doc
                          return [DefExtern extern, DefValue extfun]
  where
    typeFromPars :: Range -> [ValueBinder UserType (Maybe UserExpr)] -> UserType -> UserType -> UserType
    typeFromPars rng pars teff tres
      = promoteType $ TpFun [(binderName p, binderType p) | p <- pars] teff tres rng

    genArgs pars
      = [(Nothing,Var (binderName p) False (before (getRange p))) | p <- pars]

    genParArgs tp
      = case tp of
          TpQuan QSome _ _ _ -> fail "external types cannot contain unspecified ('_') types"
          TpQuan QExists _ _ _ -> fail "external types cannot contain existential types"
          TpQuan _ _ t _ -> genParArgs t
          TpQual _ t     -> genParArgs t
          TpParens t _   -> genParArgs t
          TpAnn t _      -> genParArgs t
          TpFun pars _ _ _ -> return $ genFunParArgs pars
          _                -> fail "external declarations must have a function type"

    genFunParArgs pars
      = unzip (map genParArg (zip pars [1..]))

    genParArg ((name,tp),idx)
      = let fullName = if name == nameNil then newHiddenName ("arg" ++ show idx) else name
            rng = rangeNull -- before (getRange tp)
        in (ValueBinder fullName Nothing Nothing rng rng
           ,(Nothing,Var fullName False rng))

externalImport :: Range -> LexParser External
externalImport rng1
  = do keyword "="
       (entry) <- externalImportEntry
       return (ExternalImport [entry] rng1)
  <|>
    do (entries,rng2) <- semiBracesRanged externalImportEntry
       return (ExternalImport entries (combineRange rng1 rng2))
  where
    externalImportEntry
      = do target        <- externalTarget
           (keyvals,rng) <- do key <- externalImportKey
                               (val,rng)   <- stringLit
                               return ([(key,val)],rng)
                            <|> semiBracesRanged externalImportKeyVal
           keyvalss <- mapM (externalIncludes target rng) keyvals
           return (target,concat keyvalss)
           
    externalImportKeyVal 
      = do key <- externalImportKey
           keyword "="
           (val,_) <- stringLit
           return (key,val)

    externalImportKey
      = do (id,_) <- varid
           return (show id) 

    externalIncludes target rng (key,fname)  | key == "file" || key == "header-file" || key == "header-end-file"
     = do let currentFile = (Common.Range.sourceName (rangeSource rng))
              fpath       = joinPath (dirname currentFile) fname
          if (isTargetC target && null (extname fpath) && key=="file")
            then do contentH <- preadFile (fpath ++ ".h")
                    contentC <- preadFile (fpath ++ ".c")
                    return [("header-include-inline",contentH),("include-inline",contentC)]
            else if (isTargetC target  && key=="header-file")
                  then do content <- preadFile fpath
                          return [("header-include-inline",content)]
                 else if (isTargetC target && key=="header-end-file")
                  then do content <- preadFile fpath
                          return [("header-end-include-inline",content)]                 
                  else if (key == "file") 
                         then do content <- preadFile fpath
                                 return [("include-inline",content)]
                         else return [(key,fpath)]
    externalIncludes target rng (key,val) 
      = return [(key,val)]

    preadFile :: FilePath -> LexParser String
    preadFile fpath
      = do mbContent <- ptryReadFile fpath
           case mbContent of
             Just content -> return content
             Nothing      -> fail ("unable to read external file: " ++ fpath)

    ptryReadFile :: FilePath -> LexParser (Maybe String)
    ptryReadFile fpath
      = do pos <- getPosition
           let mbContent  = unsafePerformIO $ exCatch (do -- putStrLn ("reading: " ++ fpath);
                                                          content <- readFile fpath
                                                          return (seq (last content) $ Just content)
                                                      ) (\exn -> return Nothing)
           case mbContent of
             Just content -> seq content $ return (Just content)
             Nothing      -> return Nothing


externalBody :: LexParser ([(Target,ExternalCall)],Range)
externalBody
  = semiBracesRanged externalEntry

externalEntry
  = do (target,inline,_) <- externalEntryRanged
       return (target,inline)

externalEntryRanged
  = do target <- externalTarget
       (call,rng) <- externalCall
       return (target,call,rng)

externalCall
  = do f <- do specialId "inline"
               return ExternalInline
            <|>
            do return ExternalCall
       (s,rng) <- stringLit
       return (f s,rng)


externalTarget
  = do specialId "c"
       return (C CDefault)
  <|>
    do specialId "cs"
       return CS
  <|>
    do specialId "js"
       return (JS JsDefault)
  <|>
    return Default



{--------------------------------------------------------------------------
  Fixity declaration
--------------------------------------------------------------------------}
fixDecl :: LexParser FixDefs
fixDecl
  = do (vis,vrng,assoc) <- try $ do (vis,vrng) <- visibility Private
                                    assoc <- assocDef
                                    return (vis,vrng,assoc)
       (n,_) <- integer
       -- convenient to check here, but it really should be done during static analysis.
       if (n < 0 || n > 100)
        then fail "The precedence must be between 0 and 100"
        else return ()
       let prec = fromInteger n
       names <- sepBy1 identifier comma
       return [FixDef name (FixInfix prec assoc) (combineRange vrng rng) vis | (name,rng) <- names]
{-
  <|>
    do fix   <- do{ keyword "prefix"; return FixPrefix }
                <|>
                do{ keyword "postfix"; return FixPostfix }
       names <- sepBy1 identifier comma
       return [FixDef name fix rng | (name,rng) <- names]
-}
assocDef
  =   do keyword "infixl"; return AssocLeft
  <|> do keyword "infixr"; return AssocRight
  <|> do keyword "infix"; return AssocNone

{--------------------------------------------------------------------------
  Type definitions
--------------------------------------------------------------------------}
aliasDecl :: Visibility -> LexParser UserTypeDef
aliasDecl dvis
  = do (vis,vrng,trng,doc) <- try$ do (vis,vrng) <- visibility dvis
                                      (trng,doc) <- dockeyword "alias"
                                      return (vis,vrng,trng,doc)
       tbind <- tbinderDef
       (tpars,kind,krng) <- typeKindParams
       keyword "="
       tp <- ptype
       let range = combineRanges [vrng,trng,krng,getRange tp]
       return (Synonym (tbind kind) tpars tp range vis doc)

typeDecl,dataTypeDecl,structDecl :: Visibility -> LexParser (UserTypeDef, [UserDef])
typeDecl dvis
  = dataTypeDecl dvis <|> structDecl dvis

dataTypeDecl dvis =
   do (vis,defvis,vrng,(typeSort,trng,doc,ddef,isExtend)) <-
          (try $
            do rng <- keyword "abstract"
               x   <- typeDeclKind
               return (Public,Private,rng,x)
            <|>
            do (vis,vrng) <- visibility dvis
               x <- typeDeclKind
               return (vis,vis,vrng,x))
      tbind <- if isExtend
                then do (qid,rng) <- qtypeid
                        return (\kind -> TypeBinder qid kind rng rng)
                else tbinderDef
      (tpars,kind,prng) <- typeKindParams
      let name = tbind kind
          resTp = TpApp (tpCon name) (map tpVar tpars) (combineRanged name tpars)
      (cs,crng)    <- semiBracesRanged (constructor defvis tpars resTp) <|> return ([],rangeNull)
      let (constrs,creatorss) = unzip cs
          range   = combineRanges [vrng,trng, getRange (tbind kind),prng,crng]
      return (DataType name tpars constrs range vis typeSort ddef isExtend doc, concat creatorss)
   where
    tpVar tb = TpVar (tbinderName tb) (tbinderRange tb)
    tpCon tb = TpCon (tbinderName tb) (tbinderRange tb)

structDecl dvis =
   do (vis,defvis,ddef,vrng,trng,doc) <-
        (try $
          do (vis,dvis,rng) <-     do{ rng <- keyword "abstract"; return (Public,Private,rng) }
                               <|> do{ (vis,rng) <- visibility dvis; return (vis,vis,rng) }
             ddef           <-     do { specialId "value"; return (DataDefValue valueReprZero) }
                               <|> do { specialIdOr "ref" ["reference"]; 
                                        -- pwarningMessage "using 'reference' is deprecated and is always the default now";
                                        return DataDefNormal }
                               <|> do { return DataDefAuto }
             (trng,doc) <- dockeyword "struct"
             return (vis,dvis,ddef,rng,trng,doc))

      tbind <- tbinderDef
      tpars <- angles tbinders <|> return []
      let name = tbind KindNone
          resTp = TpApp (tpCon name) (map tpVar tpars) (combineRanged name tpars)

      (pars,prng)  <- conPars defvis
      let (tid,rng) = getRName name
          conId     = toConstructorName tid
          (usercon,creators) = makeUserCon conId tpars resTp [] pars rng (combineRange rng prng) defvis doc
      return (DataType name tpars [usercon] (combineRanges [vrng,trng,rng,prng]) vis Inductive ddef False doc, creators)

tpVar tb = TpVar (tbinderName tb) (tbinderRange tb)
tpCon tb = TpCon (tbinderName tb) (tbinderRange tb)

  {-
  <|>
    do trng <- keyword "enum"
       tbind <- tbinderDef
       es <- semiBraces enum <|> return []
       return (DataType (tbind KindNone) [] es (combineRanged trng es))

enum
  = do (con,rng) <- constructorId
       return (UserCon con [] [] rng rng)
  -}

typeDeclKind :: LexParser (DataKind,Range,String,DataDef, Bool)
typeDeclKind
  = try(
    do (rng1,kind) <-     do{ rng <- specialId "rec"; return (rng,Retractive) }
                      <|> do{ rng <- specialId "co"; return (rng,CoInductive) }
       (rng2,doc)  <- dockeyword "type"
       return (kind,combineRanges [rng1,rng2],doc,DataDefNormal,False)
    )
  <|>
    try(
    do (ddef,isExtend) <-     do { specialId "open"; return (DataDefOpen, False) }
                          <|> do { specialId "extend"; return (DataDefOpen, True) }
                          <|> do { specialId "value"; return (DataDefValue valueReprZero, False) }
                          <|> do { specialIdOr "ref" ["reference"]; 
                                   return (DataDefNormal, False) }
                          <|> return (DataDefAuto, False)
       (rng,doc) <- dockeyword "type"
       return (Inductive,rng,doc,ddef,isExtend))


typeKindParams
   = do (tpars,rng) <- anglesRanged tbinders
        kind  <- kindAnnot
        return (tpars,kind,combineRanged rng kind)
     <|>
     do kind <- kindAnnot
        return ([],kind,getRange kind)
     <|>
        return ([],KindNone,rangeNull)


constructor :: Visibility -> [UserTypeBinder] -> UserType -> LexParser (UserCon UserType UserType UserKind, [UserDef])
constructor defvis foralls resTp
  = do ((vis,vrng),(rng0,doc),(con,rng)) <- try $ do v <- visibility defvis
                                                     k <- dockeyword "con" <|> return (rangeNull,"")
                                                     c <- constructorId
                                                     return (v,k,c)
       exists    <- typeparams
       (pars,prng) <- conPars vis
       return (makeUserCon con foralls resTp exists pars rng (combineRanges [vrng,rng0,rng,getRange exists,prng]) vis doc)

makeUserCon :: Name -> [UserTypeBinder] -> UserType -> [UserTypeBinder] -> [(Visibility,ValueBinder UserType (Maybe UserExpr))] -> Range -> Range -> Visibility -> String -> (UserCon UserType UserType UserKind, [UserDef])
makeUserCon con foralls resTp exists pars nameRng rng vis doc
  = (UserCon con exists conParams Nothing nameRng rng vis doc
    ,if (any (isJust . binderExpr . snd) pars) then [creator] else [])
  where
    conParams
      = [(vis,par{ binderExpr = Nothing }) | (vis,par) <- pars]
    creator
      = let name = newCreatorName con
            def  = Def binder rng vis (defFun []) InlineAlways doc
            binder    = ValueBinder name () body nameRng nameRng
            body      = Ann (Lam lparams (App (Var con False nameRng) arguments rng) rng) tpFull rng
            params    = [par{ binderType = (if (isJust (binderExpr par)) then makeOptional (binderType par) else binderType par) }  | (_,par) <- pars]
            lparams   = [par{ binderType = Nothing} | par <- params]
            arguments = [(Nothing,Var (binderName par) False (binderNameRange par)) | par <- params]
            tpParams  = [(binderName par, binderType par) | par <- params]
            tpFull    = quantify QForall foralls (TpFun tpParams (makeTpTotal nameRng) resTp rng)
            makeOptional tp = TpApp (TpCon nameTpOptional (getRange tp)) [tp] (getRange tp)
        in def

    isJust (Just{}) = True
    isJust _        = False

conPars defVis
  =   semiBracesRanged (conBinder defVis)
  <|> parensCommasRng (conBinder defVis)   -- deprecated
  <|> return ([],rangeNull)

conBinder defVis
  = do (vis,vrng)    <- visibility defVis
       (name,rng,tp) <- paramType
       (opt,drng)    <- defaultExpr
       return (vis, ValueBinder name tp opt rng (combineRanges [vrng,rng,getRange tp,drng]))
{-
    do (name,rng) <- try (do{ (Var name _ rng) <- variable; keyword ":"; return (name,rng) })
       tp <- ptype <?> "field type"
       return (ValueBinder name tp Nothing rng)
  <|>
    do tp <- ptype <?> "field type"
       return (ValueBinder nameNil tp Nothing (getRange tp))
-}
  <?>
    "constructor field"

constructorId
  = try ttuple
  <|>
    tlist
  <|>
    conid
  <?> "constructor"

-----------------------------------------------------------
-- Implicit Parameters
-----------------------------------------------------------

type Param = (Visibility, ValueBinder UserType (Maybe UserExpr))

-- given a name and an expression, this function generates
-- - a binder for a fresh name (let's say `val x$name$3 = expr; body`), binding the expression
-- - a tail resuming expression i.e. `resume(x$name$3, params...)`
-- TODO add parameters to resume (replace UserExpr by [ValueBinder t e] -> UserExpr)

bindExprToVal :: Name -> Range -> UserExpr -> (UserExpr -> UserExpr, [ValueBinder t e] -> UserExpr)
bindExprToVal opname oprange expr
  =  let fresh    = makeFreshHiddenName "value" opname oprange
         freshVar = (Var fresh False oprange)
         erange   = (getRange expr)
         binder   = (Def (ValueBinder fresh () expr oprange erange) oprange Private DefVal InlineAuto "")
     in (\body -> Bind binder body erange, \params -> freshVar {- \params -> resumeCall freshVar params erange -})


-- Effect definitions
--
-- We don't return a syntactic construction for effects
-- but immediately build the underlying data structures.
-----------------------------------------------------------
-- the following newtypes are used to represent intermediate syntactic
-- structures

-- OpDecl (doc,id,kwdrng,idrng,exists0,pars,prng,mbteff,tres)
newtype OpDecl = OpDecl (String, Name, Range, Range, Bool {-linear-}, OperationSort, [TypeBinder UserKind],
                               [(ValueBinder UserType (Maybe UserExpr))],
                               Range, (Maybe UserType), UserType)

-- EffectDeclHeader
newtype EffectDecl = EffectDecl (Visibility, Visibility, Range, Range,
                                 String, DataKind, Bool {-linear-}, Bool {-instance?-}, Bool {-scoped?-},
                                 Name, Range, [TypeBinder UserKind],
                                 UserKind, Range, Maybe [UserType] {- instance umbrella -}, [OpDecl])

parseEffectDecl :: Visibility -> LexParser EffectDecl
parseEffectDecl dvis =
  do (vis,defvis,vrng,erng,doc,singleShot,sort,isInstance,isScoped) <-
        (try $
          do (vis,defVis,vrng) <-     do{ (v,vr) <- visibility dvis; return (v,v,vr) }
                                  <|> do{ vr <- keyword "abstract"; return (Public,Private,vr) }
             isInstance <- do{ keyword "named"; return True } <|> return False
             isScoped   <- do{ specialId "scoped"; return True } <|> return False
             (rng1,singleShot) <- do{ rng <- specialId "linear"; return (rng,True) } <|> return (rangeNull,False)
             sort              <- do{ specialId "rec"; return Retractive } <|> return Inductive
             (rng2,doc)        <- dockeyword "effect"
             let erng = combineRange rng1 rng2
             return (vis,vis,vrng,erng,doc,singleShot,sort,isInstance,isScoped))
     (do (effectId,irng) <- typeid
         (tpars,kind,prng) <- typeKindParams
         mbInstanceUmb <- if (not isInstance) then return Nothing
                            else do keyword "in"
                                    tp <- ptype
                                    return (Just [tp,TpCon nameTpPartial irng])
                                 <|>
                                    return (if (isScoped) then Just []
                                                          else Just [TpCon nameTpPartial irng])
                                    -- todo: still need to add TpNamed for the JavaScript backend?
                                    -- return (Just (TpCon nameTpNamed irng))  -- todo: needed only if not using exn?
         (operations, xrng) <- semiBracesRanged (parseOpDecl singleShot defvis)
         return $ -- trace ("parsed effect decl " ++ show effectId ++ " " ++ show sort ++ " " ++ show singleShot ++ " " ++ show isInstance ++ " " ++ show tpars ++ " " ++ show kind ++ " " ++ show mbInstance) $
          EffectDecl (vis, defvis, vrng, erng, doc, sort, singleShot, isInstance, isScoped, effectId, irng,
                           tpars, kind, prng, mbInstanceUmb, operations)
      <|>
      do (tpars,kind,prng) <- typeKindParams
         op@(OpDecl (doc,opId,krng,idrng,linear,opSort,exists0,pars,prng,mbteff,tres)) <- parseOpDecl singleShot vis
         let mbInstance = Nothing
             effectId   = if isValueOperationName opId then fromValueOperationsName opId else opId
         return $ -- trace ("parsed effect decl " ++ show opId ++ " " ++ show sort ++ " " ++ show singleShot ++ " " ++ show linear ) $
          EffectDecl (vis, defvis, vrng, erng, doc, sort, singleShot||linear, False, isScoped, effectId, extendRange idrng (-1),
                           tpars, kind, prng, mbInstance, [op])
      )

dockeywordEffect
  = dockeywordOr "effect" ["context", "ambient"]

keywordFun
  = keyword "fun"

dockeywordFun
  = dockeyword "fun"

keywordInject
  = keywordOr "mask" ["inject"]

makeEffectDecl :: EffectDecl -> [TopDef]
makeEffectDecl decl =
  let (EffectDecl (vis, defvis, vrng, erng, doc, sort, singleShot, isInstance, isScoped,
                    id, irng, tpars, kind, prng, mbInstanceUmb, operations)) = decl

                             
      rng     = combineRanges [vrng,erng,irng]  

      krng    = rangeNull -- for generated code
      grng    = krng

      (tparsScoped, tparsNonScoped)
         = if (isScoped)
            then (take 1 tpars, drop 1 tpars)
            else ([], tpars)

      infkind = case kind of
                 KindNone -> foldr KindArrow
                               (KindCon (if isInstance then nameKindStar else
                                         if singleShot then nameKindHandled1
                                                       else nameKindHandled) krng)
                               (map tbinderKind tpars)
                 _ -> kind
      ename   = TypeBinder id infkind irng irng
      effTpH  = TpApp (TpCon (tbinderName ename) (tbinderRange ename)) (map tpVar tpars) krng
      effTp   = if (isInstance)
                 then effTpH
                 else TpApp (TpCon (if singleShot then nameTpHandled1 else nameTpHandled) (tbinderRange ename))
                       [effTpH] krng


      -- declare the effect type (for resources, generate a hidden constructor to check the types)
      docEffect  = "`:" ++ show id ++ "` effect"
      docx       = (if (doc/="") then doc else "// " ++ docEffect)

      (effTpDecl,wrapAction)
                = if isInstance
                    then -- Synonym ename tpars (makeTpApp (TpCon nameTpEv rng) [makeTpApp (tpCon hndTpName) (map tpVar tpars) rng] rng) rng vis docx
                         let evTp  = makeTpApp (TpCon nameTpEv rng) [makeTpApp (tpCon hndTpName) (map tpVar tparsNonScoped) rng] rng
                             evName  = newName "ev"
                             evFld = ValueBinder evName evTp Nothing irng rng
                             evCon = UserCon (toConstructorName id) [] [(Private,evFld)] Nothing irng rng Private ""
                         in (DataType ename tpars [evCon] rng vis Inductive (DataDefNormal {-DataDefValue 0 0-}) False docx
                            ,(\action -> Lam [ValueBinder evName Nothing Nothing irng rng]
                                                  (App (action) [(Nothing,App (Var (toConstructorName id) False rng) [(Nothing,Var evName False rng)] rng)] rng)
                                                  rng))
                    else let -- add a private constructor that refers to the handler type to get a proper recursion check
                             hndfld = ValueBinder nameNil hndTp Nothing irng irng
                             hndcon = UserCon (toConstructorName id) [hndEffTp,hndResTp] [(Private,hndfld)] Nothing irng irng Private ""
                         in (DataType ename tpars [hndcon] rng vis Inductive DataDefNormal False docx, \action -> action)

      -- declare the effect handler type
      kindEffect = KindCon nameKindEffect krng
      kindStar   = KindCon nameKindStar krng
      hndName    = toHandlerName id
      hndEffTp   = TypeBinder (newHiddenName "e") (KindCon nameKindEffect krng) krng krng
      hndResTp   = TypeBinder (newHiddenName "r") kindStar krng krng
      hndTpName  = TypeBinder hndName KindNone krng krng
      hndTp      = makeTpApp (tpCon hndTpName) (map tpVar (tparsNonScoped ++ [hndEffTp,hndResTp])) grng

      -- declare the effect tag
      tagName    = toEffectTagName id
      tagDef     = Def (ValueBinder tagName ()
                         (Ann (App (Var nameHTag False krng)
                               -- todo: this needs to be prefixed with the actual module name
                               [(Nothing,Lit (LitString (show id ++ "." ++ basename (sourceName (rangeSource irng))) krng))]
                               krng)
                          (quantify QForall tpars
                            (makeTpApp (TpCon nameTpHTag krng) [makeTpApp (TpCon hndName krng) (map tpVar tparsNonScoped) krng] krng))
                         krng)
                        krng krng) krng vis DefVal InlineNever ("// runtime tag for the " ++ docEffect)


      --extendConName = toEffectConName (tbinderName ename)
      scopeEff    = TpApp (TpCon nameTpScope krng) [TpVar (tbinderName tb) krng | tb <- tparsScoped] krng
      extraEffects = (if (isScoped && isInstance) then [scopeEff] else [])
                     ++
                     (if (sort==Retractive) then [TpCon nameTpDiv krng] else [])

      -- parse the operations and return the constructor fields and function definitions
      opCount = length operations
      (opFields,opSelects,opDefs,opValDefs)
          = unzip4 $ map (operationDecl opCount vis tparsScoped tparsNonScoped docEffect hndName
                                                 id mbInstanceUmb effTp (tpCon hndTpName)
                                                 ([hndEffTp,hndResTp]) extraEffects)
                                                 (zip [0..opCount-1] (sortBy cmpName operations))
      cmpName op1 op2 = compare (getOpName op1) (getOpName op2)
      getOpName (OpDecl (doc,opId,_,idrng,linear,opSort,exists0,pars,prng,mbteff,tres)) = show (unqualify opId)

      hndCon     = UserCon (toConstructorName hndName) [] [(Public,fld) | fld <- opFields] Nothing krng grng vis ""
      hndTpDecl  = DataType hndTpName (tparsNonScoped ++ [hndEffTp,hndResTp]) [hndCon] grng vis sort DataDefNormal False ("// handlers for the " ++ docEffect)

      -- declare the handle function

      handleRetTp= TypeBinder (newHiddenName "b") kindStar krng krng
      handleName = toHandleName id
      handleEff  = if isInstance
                    then if (isScoped)
                           then makeEffectExtend krng scopeEff (tpVar hndEffTp)
                           else tpVar hndEffTp
                    else makeEffectExtend krng effTp (tpVar hndEffTp) :: UserType
      actionTp   = makeTpFun actionArgTp handleEff (tpVar handleRetTp) grng
      handleTp   = quantify QForall (tparsNonScoped ++ [handleRetTp,hndEffTp,hndResTp]) $
                   makeTpFun [
                    (newName "cfc", TpCon nameTpInt32 krng),
                    (newName "hnd", TpApp (TpCon hndName grng) (map tpVar (tparsNonScoped ++ [hndEffTp,hndResTp])) grng),
                    (newName "ret", makeTpFun [(newName "res",tpVar handleRetTp)] (tpVar hndEffTp) (tpVar hndResTp) grng),
                    (newName "action",
                        if (isScoped)
                          then quantify QForall tparsScoped actionTp
                          else actionTp)
                    ] (tpVar hndEffTp) (tpVar hndResTp) grng
      actionArgTp= if isInstance
                    then [(newName "hname",effTp)] -- makeTpApp effTp (map tpVar tpars) rng)]
                    else []
      handleBody = Ann (Lam params handleInner grng) handleTp grng
      handleInner= App (Var (if isInstance then nameNamedHandle else nameHandle) False grng) arguments grng
      params     = [ValueBinder (newName "cfc") Nothing Nothing krng grng,
                    ValueBinder (newName "hnd") Nothing Nothing krng grng,
                    ValueBinder (newName "ret") Nothing Nothing krng grng,
                    ValueBinder (newName "action") Nothing Nothing krng grng]
      arguments  = [(Nothing, Var tagName False krng),
                    (Nothing, Var (newName "cfc") False krng),
                    (Nothing, Var (newName "hnd") False krng),
                    (Nothing, Var (newName "ret") False krng),
                    (Nothing, wrapAction (Var (newName "action") False krng))]
      handleDef  =  Def (ValueBinder handleName () handleBody irng rng)
                        grng vis (defFun []) InlineNever ("// handler for the " ++ docEffect)

   in [DefType effTpDecl, DefValue tagDef, DefType hndTpDecl, DefValue handleDef]
         ++ map DefValue opSelects
         ++ map DefValue opDefs
         ++ map DefValue (catMaybes opValDefs)

           -- effInstanceDecls ++
           -- map DefType opTpDecls ++
           -- map DefValue opDefs ++ map DefValue (catMaybes opsValDefs)

effectDecl :: Visibility -> LexParser [TopDef]
effectDecl dvis = do
  decl <- parseEffectDecl dvis
  return $ makeEffectDecl decl

parseOpDecl :: Bool -> Visibility -> LexParser OpDecl
parseOpDecl linear vis = parseValOpDecl vis <|> parseFunOpDecl linear vis

-- effect NAME { val op = ... }
-- TODO annotate the operation as "value operation" to
-- (a) also constrain the definition in the handler to use `val`
-- (b) constrain the use site to use it as a value
parseValOpDecl :: Visibility -> LexParser OpDecl
parseValOpDecl vis =
  do (rng0,doc)   <- (dockeyword "val")
     (id,idrng)   <- identifier
     keyword ":"
     (mbteff,tres) <- tresult
     _ <- case mbteff of
       Nothing  -> return ()
       Just etp -> fail "an explicit effect in result type of an operation is not allowed (yet)"
     return $ OpDecl (doc, toValueOperationName id, rng0, idrng,True,OpVal,[],[],idrng,mbteff,tres)

parseFunOpDecl :: Bool -> Visibility -> LexParser OpDecl
parseFunOpDecl linear vis =
  do ((rng0,doc),opSort) <- do rdoc <- dockeywordFun
                               return (rdoc,OpFun)
                           <|>
                            do rdoc <- dockeyword "ctl" <|> dockeyword "control"
                               if (linear)
                                then fail "'ctl' operations are invalid for a linear effect"
                                else return (rdoc,OpControl)
                           <|>
                            do rdoc <- dockeyword "final"
                               keyword "ctl"
                               if (linear)
                                then fail "'final ctl' operations are invalid for a linear effect"
                                else return (rdoc,OpExcept)
                           -- deprecated:
                           <|>
                            do rdoc <- dockeyword "except" <|> dockeyword "brk"
                               if (linear)
                                then fail "'brk' (or 'final ctl') operations are invalid for a linear effect"
                                else return (rdoc,OpExcept)
                           
     (id,idrng)   <- identifier
     exists0      <- typeparams
     (pars,_,prng)  <- declParams False {-allowBorrow-} True {-allowDefaults-}
     keyword ":"
     (mbteff,tres) <- tresult
     _ <- case mbteff of
        Nothing  -> return ()
        Just etp -> -- TODO: check if declared effect is part of the effect type
                    -- return etp
                    fail "an explicit effect in result type of an operation is not allowed (yet)"
     return $ -- trace ("parsed operation " ++ show id ++ " : (" ++ show tres ++ ") " ++ show exists0 ++ " " ++ show pars ++ " " ++ show mbteff) $
              OpDecl (doc,id,rng0,idrng,False{-linear-},opSort,exists0,pars,prng,mbteff,tres)


declParams :: Bool -> Bool -> LexParser ([ValueBinder UserType (Maybe UserExpr)],[ParamInfo],Range)
declParams allowBorrow allowDefaults
  = do (ipars,rng) <- parensCommasRng paramBinder
       let (pars,pinfos) = unzip ipars
       return (pars,pinfos,rng)
  where
    paramBinder 
       = do pinfo <- if allowBorrow then paramInfo else return Own
            (name,rng,tp) <- paramType
            (opt,drng)    <- if allowDefaults then defaultExpr else return (Nothing,rangeNull)
            return (ValueBinder name tp opt rng (combineRanges [rng,getRange tp,drng]), pinfo)
      <?> "parameter"

paramInfo :: LexParser ParamInfo
paramInfo 
  = do specialOp "^"
       return Borrow
  <|>
    return Own

-- smart constructor for operations
operationDecl :: Int -> Visibility -> [UserTypeBinder] -> [UserTypeBinder] ->
                 String -> Name -> Name -> Maybe [UserType] -> UserType -> UserType -> [UserTypeBinder] ->
                 [UserType] -> (Int,OpDecl) ->
                 (ValueBinder UserType (Maybe UserExpr), UserDef, UserDef, Maybe UserDef)
operationDecl opCount vis forallsScoped forallsNonScoped docEffect hndName effName mbInstanceUmb effTp hndTp hndTpVars extraEffects (opIndex,op)
  = let -- teff     = makeEffectExtend rangeNull effTp (makeEffectEmpty rangeNull)
           foralls  = forallsScoped ++ forallsNonScoped
           OpDecl (doc,id,kwrng,idrng,linear,opSort,exists0,pars,prng,mbteff,tres) = op

           rng      = idrng -- combineRanges [idrng,prng,getRange tres]

           krng     = rangeNull
           grng     = krng  -- for generated code

           opEffTps = case mbInstanceUmb of
                        Nothing   -> [effTp]
                        Just rtps -> rtps
           teff0    = foldr (makeEffectExtend krng) (makeEffectEmpty krng) (opEffTps ++ extraEffects)
           
           
           nameA    = newName ".a"
           tpVarA   = TpVar nameA krng
           isInstance = isJust mbInstanceUmb

           --nameE    = newName ".e"
           --tpBindE  = TypeBinder nameE (KindCon nameKindLabel idrng) idrng idrng

           -- Create the constructor
           -- opName   = toOpTypeName id
           -- opBinder = TypeBinder opName KindNone idrng idrng


           exists   = if (not (null exists0)) then exists0
                       else promoteFree foralls (map (binderType) pars ++ [teff0,tres])
           -- for now add a divergence effect to named effects/resources when there are type variables...
           -- this is too conservative though; we should generate the `ediv` constraint instead but
           -- that is a TODO for now
           teff     = if (not (null (forallsNonScoped ++ exists)) && isInstance && all notDiv extraEffects)
                       then makeEffectExtend krng (TpCon nameTpDiv krng) teff0
                       else teff0
                    where
                      notDiv (TpCon name _) = name /= nameTpDiv
                      notDiv _              = True

           -- create a constructor field for the operation as `clauseId : clauseN<a1,..,aN,b,e,r>`
           forallParams= [TpVar (tbinderName par) krng | par <- forallsNonScoped]
           tpParams    = forallParams ++ [TpVar (tbinderName par) krng | par <- exists]


           makeClauseFieldName :: OperationSort -> Name -> Name
           makeClauseFieldName opSort name
             = prepend (show opSort ++ "-") (if (isValueOperationName name) then fromValueOperationsName name else name)

           clauseId    = makeClauseFieldName opSort id
           (clauseName,clauseParsTp)
                       = if (length pars <= 2) -- set by std/core/hnd
                          then (nameTpClause (length pars), [binderType par | (par) <- pars])
                          else (nameTpClause 1,
                                [makeTpApp (TpCon (nameTuple (length pars)) krng)    -- as tuple on clause1
                                           [binderType par | (par) <- pars] krng])

           clauseRhoTp = makeTpApp (TpCon clauseName krng)
                                   (clauseParsTp ++ [tres]
                                     ++ [makeTpApp hndTp (map tpVar forallsNonScoped) krng]
                                     ++ map tpVar hndTpVars)
                                   krng
           clauseTp    = quantify QForall (exists ++ forallsScoped) $ clauseRhoTp

           conField    = -- trace ("con field: " ++ show clauseId) $
                         ValueBinder clauseId clauseTp Nothing  krng krng

           -- create an operation selector explicitly so we can hide the handler constructor
           selectId    = toOpSelectorName id
           opSelect = let def       = Def binder krng vis (defFun [Borrow]) InlineAlways ("// select `" ++ show id ++ "` operation out of the " ++ docEffect ++ " handler")
                          nameRng   = krng
                          binder    = ValueBinder selectId () body nameRng nameRng
                          body      = Ann (Lam [hndParam] innerBody grng) fullTp grng
                          fullTp    = quantify QForall (foralls ++ exists ++ hndTpVars) $
                                      makeTpFun [(hndArg,makeTpApp hndTp (map tpVar (forallsNonScoped ++ hndTpVars)) grng)]
                                                 (makeTpTotal grng) clauseRhoTp grng

                          hndArg    = newName "hnd"
                          hndParam  = ValueBinder hndArg Nothing Nothing krng grng

                          innerBody = Case (Var hndArg False grng) [branch] grng
                          branch    = Branch (PatCon (toConstructorName hndName) patterns grng grng)
                                             [Guard guardTrue (Var clauseId False grng)]
                          i          = opIndex
                          fieldCount = opCount
                          patterns  = [(Nothing,PatWild grng) | _ <- [0..i-1]]
                                      ++ [(Nothing,PatVar (ValueBinder clauseId Nothing (PatWild grng) grng grng))]
                                      ++ [(Nothing,PatWild grng) | _ <- [i+1..fieldCount-1]]
                      in def


           -- create a typed perform wrapper: fun op(x1:a1,..,xN:aN) : <l> b { performN(evv-at(0),clause-op,x1,..,xN) }
           opDef  = let def      = Def binder idrng vis (defFun []) InlineAlways ("// call `" ++ show id ++ "` operation of the " ++ docEffect)
                        nameRng   = idrng
                        binder    = ValueBinder id () body nameRng nameRng
                        body      = Ann (Lam lparams innerBody rng) tpFull rng

                        hasExists = (length exists==0)
                        innerBody
                          = App perform (
                               [(Nothing, if isInstance
                                           then Case (Var resourceName False krng)
                                                 [Branch (PatCon (toConstructorName effName)
                                                                 [(Nothing,PatVar (ValueBinder (newName "ev") Nothing (PatWild krng) krng grng))]
                                                                 krng grng)
                                                         [Guard guardTrue (Var (newName "ev") False krng)]
                                                 ] grng
                                           else App (Var nameEvvAt False krng) [(Nothing,zeroIdx)] krng),
                                (Nothing, Var selectId False krng)]
                               ++ arguments) grng


                        zeroIdx        = App (Var nameSSizeT False krng) [(Nothing,Lit (LitInt 0 krng))] krng
                        resourceName   = newHiddenName "hname"
                        resourceBinder = ValueBinder resourceName effTp  Nothing krng grng
                        perform        = Var (namePerform (length pars)) False krng

                        params0   = [par{ binderType = (if (isJust (binderExpr par)) then makeOptional (binderType par) else binderType par) }  | par <- pars] -- TODO: visibility?
                        params    = (if (isInstance) then [resourceBinder] else []) ++ params0
                        arguments = [(Nothing,Var (binderName par) False (binderNameRange par)) | par <- params0]

                        lparams   = [par{ binderType = Nothing} | par <- params]
                        tplparams = [(binderName par, binderType par) | par <- params]
                        tpFull    = quantify QForall (foralls ++ exists) (TpFun tplparams teff tres grng)

                        makeOptional tp = TpApp (TpCon nameTpOptional (getRange tp)) [tp] (getRange tp)
                        isJust (Just{}) = True
                        isJust _        = False
                    in def

           -- create a temporary value definition for type checking
           opValDef = if isValueOperationName id then
                         let opName  = fromValueOperationsName id
                             qualTpe = promoteType (TpApp (TpCon nameTpValueOp krng) [tres] krng)
                             phantom = App (Var namePhantom False krng) [] krng
                             annot   = Ann phantom qualTpe krng
                         in Just $ Def (ValueBinder opName () annot idrng krng)
                                        krng vis DefVal InlineNever "// phantom definition for value operations"

                       else Nothing

           in (conField,opSelect,opDef,opValDef) -- (opsConDef,opTpDecl,opDef,opValDef)



-----------------------------------------------------------
-- Value definitions
-----------------------------------------------------------

pureDecl :: Visibility -> LexParser UserDef
pureDecl dvis
  = do pdecl
          <- try $ do (vis,vrng) <- visibility dvis
                      inline <- parseInline
                      (do (rng,doc) <- dockeyword "val" -- return (vis,vrng,rng,doc,inline,True)
                          return (valDecl (combineRange vrng rng) doc vis inline)
                       <|>
                       do fip    <- parseFip
                          (rng,doc) <- dockeywordFun  -- return (vis,vrng,rng,doc,inline,False)
                          return (funDecl (combineRange vrng rng) doc vis inline fip)
                       <|>
                       do keyword "fn"
                          fail "hint: use 'fun' to start a named function definition (and 'fn' for anonymous functions)")
       -- (if isVal then valDecl else funDecl) (combineRange vrng rng) doc vis inline
       -- valueDecl vrng vis <|> functionDecl vrng vis
       pdecl

parseFipAlloc :: LexParser FipAlloc
parseFipAlloc
  = parens (  (do (num,_) <- integer
                  return (AllocAtMost (fromInteger num)))
           <|> do _ <- specialId "n"
                  return AllocFinitely)
      <|> return (AllocAtMost 0)

parseFip :: LexParser Fip
parseFip 
  = do isTail   <- do specialId "tail"
                      return True
                  <|> return False
       ( do specialId "fip"
            alloc <- parseFipAlloc
            when isTail $ pwarningMessage "a 'fip' function implies already 'tail'"
            return (Fip alloc)
         <|> 
         do specialId "fbip"
            alloc <- parseFipAlloc
            return (Fbip alloc isTail)
         <|> return (NoFip isTail))

functionDecl vrng vis
  = do pdecl <- try $ do inline <- parseInline
                         fip    <- parseFip
                         (rng,doc) <- dockeywordFun
                         return (funDecl (combineRange vrng rng) doc vis inline fip)
       pdecl

varDecl
  = do (vrng,doc) <- dockeyword "var"
       bind <- binder vrng
       keyword ":="
       body <- blockexpr
       return (Def (bind body) (combineRanged vrng body) Private DefVar InlineNever doc)


valDecl rng doc vis inline
  = do bind <- binder rng
       keyword "="
       body <- blockexpr
       return (Def (bind body) (combineRanged rng body) vis DefVal inline doc)

funDecl rng doc vis inline fip
  = do spars <- squantifier
       -- tpars <- aquantifier  -- todo: store somewhere
       (name,nameRng) <- funid
       (tpars,pars,pinfos,parsRng,mbtres,preds,ann) <- funDef True {-allowBorrow-}
       body   <- bodyexpr
       let fun = promote spars tpars preds mbtres
                  (Lam pars body (combineRanged rng body))
       return (Def (ValueBinder name () (ann fun) nameRng nameRng) (combineRanged rng fun) vis 
                       (defFunEx pinfos fip) inline doc)

-- fundef: forall parameters, parameters, (effecttp, resulttp), annotation
funDef :: Bool -> LexParser ([TypeBinder UserKind],[ValueBinder (Maybe UserType) (Maybe UserExpr)], [ParamInfo], Range, Maybe (Maybe UserType, UserType),[UserType], UserExpr -> UserExpr)
funDef allowBorrow
  = do tpars  <- typeparams
       (pars, pinfos, transform, rng) <- parameters allowBorrow True {-allowDefault-}
       resultTp <- annotRes
       preds <- do keyword "with"
                   parens (many1 predicate)
                <|> return []
       return (tpars,pars,pinfos,rng,resultTp,preds,transform)

annotRes :: LexParser (Maybe (Maybe UserType,UserType))
annotRes
  = do (teff,tres) <- annotResult
       return (Just (Just teff, tres))
  <|>
    return Nothing

annotResult :: LexParser (UserType,UserType)
annotResult
  = do keyword ":"
       tresultTotal

typeparams
  = do tbinds <- angles tbinders
       return tbinds
  <|>
    do return []

parameters :: Bool -> Bool -> LexParser ([(ValueBinder (Maybe UserType) (Maybe UserExpr))], [ParamInfo], UserExpr -> UserExpr, Range)
parameters allowBorrow allowDefaults = do
  (results, rng) <- parensCommasRng (parameter allowBorrow allowDefaults)
  let (binders, pinfos, transforms) = unzip3 results
      transform = appEndo $ foldMap Endo transforms  -- right-to-left so the left-most parameter matches first
  pure (binders, pinfos, transform, rng)

parameter :: Bool -> Bool -> LexParser (ValueBinder (Maybe UserType) (Maybe UserExpr), ParamInfo, UserExpr -> UserExpr)
parameter allowBorrow allowDefaults = do
  pinfo <- if allowBorrow then paramInfo else return Own
  pat <- patAtom
  tp  <- optionMaybe typeAnnotPar
  (opt,drng) <- if allowDefaults then defaultExpr else return (Nothing,rangeNull)

  let rng = case pat of
              PatVar binder -> getRange (binderExpr binder)
              _ -> getRange pat
      binder name nameRng = ValueBinder name tp opt nameRng (combineRanges [rng, getRange tp, drng])
  case pat of
    -- treat PatVar and PatWild as special cases to avoid unnecessary match expressions
    PatVar (ValueBinder name Nothing (PatWild _) nameRng rng) -- binder   | PatWild nameRng <- binderExpr binder  -> 
      -> return (binder name nameRng, pinfo, id)
    PatWild nameRng 
      -> do let name = uniqueRngHiddenName nameRng "_wildcard"
            return (binder name nameRng, pinfo, id)
    pat 
      -> do -- transform (fun (pattern) { body }) --> fun(.pat_X_Y) { match(.pat_X_Y) { pattern -> body }}
            let name = uniqueRngHiddenName rng "pat"
                transform (Lam binders body lambdaRng) = Lam binders (Case (Var name False rng) 
                                                                        [Branch pat [Guard guardTrue body]] rng) lambdaRng
                transform (Ann body tp rng) = Ann (transform body) tp rng
                transform _ = failure "Syntax.Parse.parameter: unexpected function expression in parameter match transform"
            return (binder name rng, pinfo, transform)

paramid = identifier <|> wildcard

defaultExpr
  = do krng <- keyword "="
       e <- expr
       return (Just e, combineRanged krng e)
  <|>
    return (Nothing,rangeNull)


{--------------------------------------------------------------------------
  Statements
--------------------------------------------------------------------------}

block :: LexParser UserExpr
block
  = do rng1 <- lcurly
       many semiColon
       stmts1 <- semis statement
       stmts2 <- do rng2 <- keyword "return"
                    e <- expr
                    many semiColon
                    return [StatExpr (makeReturn rng2 e)]
                 <|>
                    return []
       rng2 <- rcurly
       let localize = case anyStatVar stmts1 of 
                        Just def -> [StatFun (localScope def)] 
                        _        -> []
           stats = localize ++ stmts1 ++ stmts2
       case (reverse stats) of
         (StatExpr exp:_) -> return (Parens (foldr combine exp (init stats)) nameNil (combineRange rng1 rng2))
         []               -> return (Var nameUnit False (combineRange rng1 rng2))
         _                -> fail "Last statement in a block must be an expression"
  where
    anyStatVar (StatVar def:_) = Just def
    anyStatVar (_:rest)        = anyStatVar rest
    anyStatVar _               = Nothing


    localScope :: UserDef -> UserExpr -> UserExpr
    localScope vdef exp = let erng = getRange exp
                              drng = getRange vdef
                              nrng = binderNameRange (defBinder vdef)
                          in App (Var nameRunLocal False nrng)
                                  [(Nothing,Lam [] exp erng)]
                                  drng

    combine :: Statement -> UserExpr -> UserExpr
    combine (StatFun f) exp   = f exp
    combine (StatExpr e) exp  = let r = getRange e
                                in Bind (Def (ValueBinder (newName "_") () e r r) r Private DefVal InlineAuto "") exp r
    combine (StatVar def) exp = let (ValueBinder name () expr nameRng rng) = defBinder def
                                in  App (Var nameLocal False rng)
                                        -- put parens over the lambda so it comes later during type inference (so the type of expr can be propagated in)
                                        -- see test/ambient/ambient3
                                        [(Nothing, expr),
                                         (Nothing, Parens (Lam [ValueBinder name Nothing Nothing nameRng nameRng] exp (combineRanged def exp)) name rng)]
                                         (combineRanged rng exp)

makeReturn r0 e
  = let r = getRange e
    in App (Var nameReturn False r0) [(Nothing,e)] (combineRange r0 r)

data Statement = StatFun (UserExpr -> UserExpr)
               | StatExpr UserExpr
               | StatVar UserDef

statement :: LexParser Statement
statement
  = do funs <- many1 (functionDecl rangeNull Private)
       return (StatFun (\body -> Let (DefRec funs) body (combineRanged funs body)))
  <|>
    do fun <- localValueDecl -- <|> localUseDecl <|> localUsingDecl
       return (StatFun fun) -- (\body -> -- Let (DefNonRec val) body (combineRanged val body)
                            --              Bind val body (combineRanged val body)  ))
  <|>
    do var <- varDecl
       return (StatVar var) -- (StatFun (\body -> Bind var body (combineRanged var body)))
  <|>
    do f <- withstat
       (do keyword "in"
           e <- blockexpr
           return (StatExpr (f e))
        <|>
           return (StatFun f))
  <|>
    do exp <- basicexpr <|> returnexpr
       return (StatExpr exp)
       {-
       case exp of
         Var name _ rng -> do ann <- typeAnnotation
                              keyword "="
                              e <- expr
                              let val = Def (ValueBinder name () (ann e) rng (combineRanged rng e)) (combineRanged rng e) Private DefVal ""
                              return (StatFun (\body -> Bind val body (combineRanged rng body)))
                           <|>
                           return (StatExpr exp)
         _              -> return (StatExpr exp)
      -}

localValueDecl
  = do krng <- keyword "val"
       pat  <- pattern
       keyword "="
       e    <- blockexpr
       let bindVar binder mbTp rng
            = let annexpr = case mbTp of
                              Just tp -> Ann e (promoteType tp) rng
                              Nothing -> e
                  vbinder = ValueBinder (binderName binder) () annexpr (binderNameRange binder) (binderRange binder)
              in \body -> Bind (Def vbinder rng Private DefVal InlineAuto "") body (combineRanged krng body)
       case unParens(pat) of
         PatVar (binder@ValueBinder{ binderExpr = PatWild _ })
           -> return $ bindVar binder (binderType binder) (binderRange binder)
         PatAnn (PatVar (binder@ValueBinder{ binderExpr = PatWild _})) tp rng
           -> return $ bindVar binder (Just tp) rng
         _ -> return $ \body -> Case e [Branch pat [Guard guardTrue body]] (combineRanged krng body)

  where
    unParens (PatParens p _) = unParens(p)
    unParens p               = p

{-
localUseDecl
  = do krng <- keyword "use"
       warnDeprecated "use" "with"
       par  <- parameter False
       keyword "="
       e    <- blockexpr
       return $ applyToContinuation krng [promoteValueBinder par] e
  where
    promoteValueBinder binder
      = case binderType binder of
          Just tp -> binder{ binderType = Just (promoteType tp)}
          _ -> binder

localUsingDecl
  = do krng <- keyword "using"
       warnDeprecated "using" "with"
       e    <- blockexpr
       return $ applyToContinuation krng [] e
-}
withstat :: LexParser (UserExpr -> UserExpr)
withstat
  = do krng <- keyword "with"
       (do (par, _, transform) <- try $ parameter False{-allowBorrow-} False{-allowDefault-} <*  (keyword "=" <|> keyword "<-")
           e <- basicexpr <|> handlerExprStat krng HandlerInstance
           pure $ applyToContinuation krng [promoteValueBinder par] $ transform e
        <|>
        do e <- basicexpr <|> handlerExprStat krng HandlerNormal
           return (applyToContinuation krng [] e)
        )
  where
     promoteValueBinder binder
       = case binderType binder of
           Just tp -> binder{ binderType = Just (promoteType tp)}
           _ -> binder

applyToContinuation wrng params expr body
  = let fun = Parens (Lam params body (combineRanged wrng body)) nameNil (getRange body) -- Parens makes it last in type inference so types can better propagate (ambients/heap1)
        funarg = [(Nothing,fun)]
        fullrange = combineRanged wrng fun
    in case unParens expr of
      App f args range -> App f (args ++ funarg) fullrange
      atom             -> App atom funarg fullrange
  where
    unParens (Parens p _ _) = unParens(p)
    unParens p               = p

typeAnnotation :: LexParser (UserExpr -> UserExpr)
typeAnnotation
  = do tp <- typeAnnot
       return (\e -> Ann e tp (combineRanged e tp))
  <|>
    return id

{--------------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------------}
bodyexpr :: LexParser UserExpr
bodyexpr
  = blockexpr
  <|>
    do keyword "->" -- <|> keyword "="
       -- pwarningMessage "using '->' is deprecated, it can be left out."
       blockexpr    

blockexpr :: LexParser UserExpr   -- like expr but a block `{..}` is interpreted as statements
blockexpr
  = withexpr <|> bfunexpr <|> returnexpr <|> valexpr <|> basicexpr
  <?> "expression"

expr :: LexParser UserExpr
expr
  = withexpr <|> funexpr <|> returnexpr <|> valexpr <|> basicexpr
  <?> "expression"

basicexpr :: LexParser UserExpr
basicexpr
  = ifexpr <|> fnexpr <|> matchexpr <|> handlerExpr <|> opexpr
  <?> "(basic) expression"

withexpr :: LexParser UserExpr
withexpr
  = do f <- withstat
       keyword "in"
       e <- blockexpr
       return (f e)

valexpr :: LexParser UserExpr
valexpr
  = do f <- localValueDecl
       keyword "in"
       e <- expr
       return (f e)


bfunexpr
  = block <|> lambda ["fun"]

funexpr
  = funblock <|> lambda ["fun"]

fnexpr
  = lambda []

funblock
  = do exp <- block
       return (Lam [] exp (getRange exp))

lambda alts
  = do rng <- keywordOr "fn" alts
       spars <- squantifier
       (tpars,pars,_,parsRng,mbtres,preds,ann) <- funDef False {-allowBorrow-}
       body <- bodyexpr
       let fun = promote spars tpars preds mbtres
                  (Lam pars body (combineRanged rng body))
       return (ann fun)

ifexpr
  = do rng <- do keyword "if"
       tst <- ntlexpr
       (texpr,eexprs,eexpr) <- 
           do texpr <- returnexpr
              return (texpr, [], Var nameUnit False (after (getRange texpr)))
           <|>
           do texpr   <- thenexpr rng
              eexprs  <- many elif
              eexpr   <- do keyword "else"
                            blockexpr
                          <|>
                            return (Var nameUnit False (after (combineRanged texpr (map snd eexprs))))
              return (texpr,eexprs,eexpr)
           
            
       let fullMatch = foldr match eexpr ((tst,texpr):eexprs)
                     where
                       match (tst,texpr) eexpr
                        = let r = rangeNull
                          in  Case tst [Branch (PatCon nameTrue [] r r) [Guard guardTrue texpr]
                                       ,Branch (PatCon nameFalse [] r r) [Guard guardTrue eexpr]]
                                       (combineRanged tst eexpr)

       return fullMatch
  where
    elif
      = do rng <- keyword "elif"
           tst <- ntlexpr -- parens expr
           texpr <- thenexpr rng
           return (tst,texpr)

    thenexpr rng
      = do keyword "then"
           blockexpr 
        <|>
        do pos <- getPosition
           expr <- blockexpr
           pwarning ("warning " ++ show pos ++ ": using an 'if' without 'then' is deprecated.\n  hint: add the 'then' keyword.") rng                  
           return expr

returnexpr
  = do rng <- keyword "return"
       exp <- expr
       return (makeReturn rng exp)


matchexpr
  = do rng <- keyword "match"
       tst <- ntlexpr  -- allows tuples for multi pattern match
       (branches,rng2) <- semiBracesRanged1 branch
       return (Case tst branches (combineRange rng rng2))
  <|> handlerExpr

-- TODO: fix parsing of handlers to match the grammar precisely
handlerExpr
  = do (rng0,hsort) <- do { rng <- keyword "named"; return (rng,HandlerInstance) }
                       <|> return (rangeNull,HandlerNormal)
       (do rng1 <- keyword "handle"
           let rng = combineRange rng0 rng1
           scoped  <- do{ specialId "scoped"; return HandlerScoped } <|> return HandlerNoScope
           (override,mbEff) <- handlerOverride hsort
           arg  <- ntlexpr -- parens argument
           expr <- handlerClauses rng mbEff scoped override hsort
           return (App expr [(Nothing,arg)] (combineRanged rng expr))
        <|>
        do rng1 <- keyword "handler"
           let rng = combineRange rng0 rng1
           handlerExprX rng hsort)

handlerExprStat rng HandlerInstance
  = do keyword "named"
       optional (keyword "handler")
       handlerExprX rng HandlerInstance

handlerExprStat rng HandlerNormal
  = do handlerExprX rng HandlerNormal

handlerExprX rng hsort
  = do scoped <- do{ keyword "scoped"; return HandlerScoped } <|> return HandlerNoScope
       (override,mbEff) <- handlerOverride hsort
       handlerClauses rng mbEff scoped override hsort

handlerOverride hsort
  = do override <- if (hsort == HandlerNormal)
                     then do{ keyword "override"; return HandlerOverride } <|> return HandlerNoOverride
                     else return HandlerNoOverride
       mbEff    <- do{ eff <- angles ptype; return (Just (promoteType eff)) } <|> return Nothing
       return (override,mbEff)

handlerClauses :: Range -> Maybe UserType -> HandlerScope -> HandlerOverride -> HandlerSort -> LexParser UserExpr
handlerClauses rng mbEff scoped override hsort
  = do (clausesAndBinders,rng2) <- opClauses
       let fullrange = combineRanges [rng,rng2]
       let (clauses, binders) = extractBinders clausesAndBinders
       (reinit,ret,final,ops) <- partitionClauses clauses rng
       handler <- case (mbEff,ops) of
                   (Nothing,[]) -- no ops, and no annotation: this is not a handler; just apply return
                     -> do -- TODO: error on override/scoped/instance?
                           let handlerExpr f = Lam [ValueBinder (newHiddenName "action") Nothing Nothing rng rng]
                                                   (f (Var (newHiddenName "action") False rng)) fullrange
                               retExpr = case ret of
                                           Nothing -> id
                                           Just f  -> \actionExpr -> App f [(Nothing,App actionExpr [] fullrange)] fullrange
                           return (binders $ handlerExpr retExpr)
                   _ -> do let handlerExpr = Handler hsort scoped override Nothing mbEff [] reinit ret final ops rng fullrange
                           return (binders handlerExpr)
       return $ applyMaybe fullrange reinit final handler

applyMaybe :: Range -> Maybe UserExpr -> Maybe UserExpr -> UserExpr -> UserExpr
applyMaybe rng Nothing Nothing f  = f
applyMaybe rng reinit final f
  = Lam [ValueBinder (newHiddenName "act") Nothing Nothing rng rng] bodyI rng
  where
    bodyI = case reinit of
              Nothing  -> bodyF
              Just ini -> App (Var nameInitially False rng) [(Nothing,ini),(Nothing,Lam [] bodyF rng)] rng

    bodyF = case final of
              Nothing  -> applyH
              Just fin -> App (Var nameFinally False rng) [(Nothing,fin),(Nothing,Lam [] applyH rng)] rng

    applyH = App f [(Nothing,Var (newHiddenName "act") False rng)] rng


data Clause = ClauseRet UserExpr
            | ClauseFinally UserExpr
            | ClauseInitially UserExpr
            | ClauseBranch UserHandlerBranch

instance Ranged Clause where
  getRange (ClauseRet e) = getRange e
  getRange (ClauseFinally e) = getRange e
  getRange (ClauseInitially e) = getRange e
  getRange (ClauseBranch e) = getRange e

extractBinders :: [(Clause, Maybe (UserExpr -> UserExpr))] -> ([Clause], UserExpr -> UserExpr)
extractBinders = foldr extractBinder ([], id) where
  extractBinder (clause, Nothing) (cs, binders) = (clause : cs, binders)
  extractBinder (clause, Just binder) (cs, binders) = (clause : cs, binders . binder)

partitionClauses ::  [Clause] -> Range -> LexParser (Maybe UserExpr,Maybe UserExpr,Maybe UserExpr,[UserHandlerBranch])
partitionClauses clauses rng
  = do let (reinits,rets,finals,ops) = separate ([],[],[],[]) clauses
       ret <- case rets of
                [r] -> return (Just r)
                []  -> return Nothing
                _   -> fail "There can be be at most one 'return' clause in a handler body"
       final <- case finals of
                [f] -> return (Just f)
                []  -> return Nothing
                _   -> fail "There can be be at most one 'finally' clause in a handler body"
       reinit <- case reinits of
                   [i] -> return (Just i)
                   []  -> return Nothing
                   _   -> fail "There can be at most one 'initially' clause in a handler body"
       return (reinit,ret,final,reverse ops)
  where
    separate acc [] = acc
    separate (reinits,rets,finals,ops) (clause:clauses)
      = case clause of
          ClauseRet r -> separate (reinits,r:rets,finals,ops) clauses
          ClauseFinally f -> separate (reinits,rets,f:finals,ops) clauses
          ClauseInitially i -> separate (i:reinits,rets,finals,ops) clauses
          ClauseBranch op   -> separate (reinits,rets,finals,op:ops) clauses

-- either a single op without braces, or multiple ops within braces
opClauses :: LexParser ([(Clause, Maybe (UserExpr -> UserExpr))],Range)
opClauses
  =   semiBracesRanged handlerOpX
  <|> singleOp
  <|> do lparen
         fail "unexpected '(': local parameters are no longer supported, use a local 'var' instead"
  <|> return ([],rangeNull)
  where
    singleOp
      = do (op, bind) <- handlerOp
           return ([(op, bind)], getRange op)

handlerOpX :: LexParser (Clause, Maybe (UserExpr -> UserExpr))
handlerOpX
  = do rng <- specialId "finally"
       optional( parens (return ()) )
       expr <- bodyexpr
       return (ClauseFinally (Lam [] expr (combineRanged rng expr)), Nothing)
  <|>
    do rng <- specialId "initially"
       (name,prng,tp) <- (parens $
                          do (name,prng) <- paramid
                             tp         <- optionMaybe typeAnnotPar
                             return (name,prng,tp))
                         <|> return (newName "_",rng,Nothing)
       expr <- bodyexpr
       return (ClauseInitially (Lam [ValueBinder name tp Nothing prng (combineRanged rng tp)] expr (combineRanged rng expr)), Nothing)
  <|>
    handlerOp


-- returns a clause and potentially a binder as transformation on the handler
handlerOp :: LexParser (Clause, Maybe (UserExpr -> UserExpr))
handlerOp
  = do rng <- keyword "return"
       (name,prng,tp) <- do (name,prng) <- paramid
                            pwarningMessage "'return x' is deprecated; use 'return(x)' instead." prng
                            tp         <- optionMaybe typeAnnotPar
                            return (name,prng,tp)
                        <|>
                        (parens $
                         do (name,prng) <- paramid
                            tp         <- optionMaybe typeAnnotPar
                            return (name,prng,tp))
       expr <- bodyexpr
       return (ClauseRet (Lam [ValueBinder name tp Nothing prng (combineRanged prng tp)] expr (combineRanged rng expr)), Nothing)
  -- TODO is "raw" needed for value definitions?
  <|>
    do keyword "val"
       (name, nameRng) <- qidentifier
       keyword "="
       expr <- blockexpr
       let (binder,resumeExpr) = bindExprToVal name nameRng expr
       return (ClauseBranch (HandlerBranch (toValueOperationName name) [] (resumeExpr []) OpVal nameRng nameRng), Just binder)
  <|>
    do opSort <- do keyword "fun"
                    return OpFun
                 <|>
                 do keywordOr "ctl" ["control"]
                    return OpControl
                 <|>
                 do keyword "final"
                    keyword "ctl"
                    return OpExcept
                 <|>
                 do keyword "raw"
                    keyword "ctl"
                    return OpControlRaw
                 <|>
                 -- deprecated
                 do keyword "except" <|> keyword "brk"
                    return OpExcept
                 <|>
                 do keyword "rcontrol" <|> keyword "rawctl"
                    return OpControlRaw
                 <|>
                 -- deprecated
                 do lookAhead qidentifier
                    return OpControlErr 
       (name, nameRng) <- qidentifier
       if opSort == OpControlErr then 
        pwarningMessage "using a bare operation is deprecated.\n  hint: start with 'val', 'fun', 'brk', or 'ctl' instead." nameRng
       else return ()
       (oppars,prng) <- opParams
       expr <- bodyexpr
       let rexpr  = expr -- if (resumeKind /= ResumeTail) then expr else resumeCall expr pars nameRng
       return (ClauseBranch (HandlerBranch name oppars rexpr opSort nameRng (combineRanges [nameRng,prng])), Nothing)

opParams :: LexParser ([ValueBinder (Maybe UserType) ()],Range)
opParams
  = parensCommasRng opParam <|> return ([],rangeNull)

opParam :: LexParser (ValueBinder (Maybe UserType) ())
opParam
  = do (name,rng) <- paramid
       tp <- optionMaybe typeAnnot
       return (ValueBinder name tp () rng (combineRanged rng tp))


handlerPar :: LexParser (ValueBinder (Maybe UserType) ())
handlerPar
  = do (name,rng) <- identifier
       tp <- optionMaybe typeAnnot
       return (ValueBinder name tp () rng (combineRanged rng tp))

-- default return clause: return x -> x
handlerReturnDefault :: Range -> UserExpr
handlerReturnDefault rng
  = let xname = newHiddenName "x"
        xbind = ValueBinder xname Nothing Nothing rng rng
        xvar  = Var xname False rng
    in Lam [xbind] xvar rng


{--------------------------------------------------------------------------
  Branches
--------------------------------------------------------------------------}
branch
  = do pat  <- pattern
       grds <- guards
       return (Branch pat grds)
  <?> "pattern match"

guards :: LexParser [UserGuard]
guards
  = many1 guardBar
  <|>
    do keyword "->"
       exp <- blockexpr
       return [Guard guardTrue exp]
  <|>
    do exp <- block
       pwarningMessage "use '->' for pattern matches" (getRange exp)
       return [Guard guardTrue exp]

guardBar
  = do bar  <?> "guard condition \"|\""
       grd  <- pguardTest
       keyword "->"
       exp  <- blockexpr
       return (Guard grd exp)

pguardTest
  = do specialId "otherwise"  -- Haskell help
       fail "hint: use \"_\" instead of \"otherwise\" for the guard condition"
  <|>
    do expr
  <|>
    do wildcard
       return guardTrue
  <?> "guard condition or '_'"


{--------------------------------------------------------------------------
  Op expr
--------------------------------------------------------------------------}
ntlexpr :: LexParser UserExpr -- non-trailing-lambda expression
ntlexpr 
  = opexprx False

opexpr :: LexParser UserExpr
opexpr = opexprx True


opexprx :: Bool -> LexParser UserExpr
opexprx allowTrailingLam
  = do e1 <- prefixexpr allowTrailingLam
       (do ess <- many1(do{ op <- operatorVar; e2 <- prefixexpr allowTrailingLam; return [op,e2]; })
           return (App (Var nameOpExpr True rangeNull)
                    [(Nothing,e) | e <- e1 : concat ess] (combineRanged e1 (concat ess)))
        <|>
           return e1)

operatorVar
  = do (name,rng) <- qoperator
       return (Var name True rng)
    <|>
    do rng <- keyword ":="
       return (Var nameAssign True rng)


prefixexpr :: Bool -> LexParser UserExpr
prefixexpr allowTrailingLam
  = do ops  <- many prefixOp
       aexp <- appexpr allowTrailingLam
       return (foldr (\op e -> App op [(Nothing,e)] (combineRanged op e)) aexp ops)

appexpr :: Bool -> LexParser UserExpr
appexpr allowTrailingLam
  = do e0 <- atom
       fs <- many (dotexpr <|> applier <|> indexer <|> funapps)
       return (foldl (\e f -> f e) e0 fs)
  where
    
    dotexpr, indexer, applier, funapps :: LexParser (UserExpr -> UserExpr)
    dotexpr
      = do keyword "."
           e <- atom
           (do rng0 <- lapp
               args <- sepBy argument (comma)
               rng1 <- rparen
               return (\arg0 -> App e ((Nothing,arg0):args) (combineRanged arg0 rng1))
            <|>
               return (\arg0 -> App e [(Nothing,arg0)] (combineRanged arg0 e)))

    indexer
      = do rng0 <- lidx
           (do crng <- keyword "ctx"
               ctx  <- ccontext crng
               rng1 <- special "]"
               return (\exp -> let rng = combineRanged exp rng1
                               in App (Var nameCCtxComposeExtend False rng) [(Nothing,exp),(Nothing,ctx)] rng)
            <|>
            do idxs <- sepBy1 expr comma
               rng1 <- special "]"
               return (\exp -> App (Var nameIndex False (combineRange rng0 rng1)) (map (\a -> (Nothing,a)) (exp:idxs)) (combineRange rng0 rng1))
               )

    applier
      = do rng0 <- lapp
           args <- sepBy argument (comma)
           rng1 <- rparen
           return (\exp -> App exp (args) (combineRanged exp rng1))

    funapp | allowTrailingLam = funblock <|> lambda []
           | otherwise        = lambda []

    funapps
      = do fs <- many1 funapp
           return (\arg0 -> injectApply arg0 fs)
      where
        injectApply expr []
          = expr
        injectApply expr fargs
          = case expr of
              App fun args rng -> App fun (args ++ nfargs) rng
              _                -> App expr nfargs (combineRanged expr fargs)
          where
            nfargs = [(Nothing,f) | f <- fargs]

argument :: LexParser (Maybe (Name,Range),UserExpr)
argument
  = do exp <- aexpr
       case exp of
         Var name _ rng -> do keyword "="
                              exp2 <- expr
                              return (Just (name,rng),exp2)
                           <|>
                              return (Nothing,exp)
         _              -> return (Nothing,exp)


{--------------------------------------------------------------------------
  Atomic expression
--------------------------------------------------------------------------}
atom :: LexParser UserExpr
atom
  = do (name,rng) <- qidentifier <|> qconstructor
       return $ Var name False rng
  <|>
    do tupleExpr -- must be second due to '(' operator ')'
  <|>
    do listExpr
  <|>
    do lit <- literal
       return (Lit lit)
  <|> 
    do cctxHole
  <|>
    do cctxExpr
  <|>
    do injectExpr
  <?> "(simple) expression"

literal
  = do (i,rng) <- integer
       return (LitInt i rng)
  <|>
    do (f,rng) <- floatLit
       return (LitFloat f rng)
  <|>
    do (s,rng) <- stringLit
       return (LitString s rng)
  <|>
    do (c,rng) <- charLit
       return (LitChar c rng)
  <?> "constant"


aexpr
  = do e <- expr
       (do keyword ":"
           tp <- ptypescheme
           return (Ann e tp (combineRanged e tp))
        <|>
           return e)



tupleExpr :: LexParser UserExpr
tupleExpr
  = do rng1 <- lparen <?> ""
       es <- sepEndBy aexpr comma
       rng2 <- rparen
       case es of
         []  -> return (Var nameUnit False (combineRange rng1 rng2))
         [e] -> return (Parens e nameNil (combineRanged rng1 rng2))
         _   -> return (App (Var (nameTuple (length es)) False (combineRange rng1 rng2)) [(Nothing,e) | e <- es] (combineRange rng1 rng2))


listExpr :: LexParser UserExpr
listExpr
  = do rng1 <- special "[" <?> ""
       es <- sepEndBy aexpr comma
       rng2 <- special "]"
       if null es
        then return (makeNil (combineRange rng1 rng2))
        else return (adjustRange (combineRange rng1 rng2) (foldr (makeCons rng1) (makeNil rng2) (es)))

makeNil rng   = Var nameNull False rng
makeCons rng x xs = makeApp (Var nameCons False rng) [x,xs]

cctxExpr :: LexParser UserExpr
cctxExpr
  = do rng <- keyword "ctx"
       ccontext rng

ccontext :: Range -> LexParser UserExpr
ccontext rng
  = do ctx <- ntlexpr
       return (makeApp (Var nameCCtxCreate False rng) [ctx])
       
cctxHole :: LexParser UserExpr
cctxHole 
  = do rng <- keyword "hole" <|> do { (_,r) <- wildcard; return r }
       return (makeApp (Var nameCCtxHoleCreate False rng) [])


injectExpr :: LexParser UserExpr
injectExpr
  = do (rng, mkInj) <- injectType
       (do exp <- parens expr <|> funblock      -- need apply or the escape check may fail if it becomes a separate lambda
           return (mkInj exp)
        <|>
        do let name = newHiddenName "mask-action"
           return $ Lam [ValueBinder name Nothing Nothing rng rng] (mkInj (Var name False rng)) rng)

injectType :: LexParser (Range, UserExpr -> UserExpr)
injectType
 = do rng1 <- keywordInject
      behind <- do { specialId "behind" <|> specialId "other"; return True } <|> return False
      langle
      tp <- ptype
      rangle
      return (rng1, \exp -> Inject (promoteType tp) exp behind (combineRanged rng1 exp))

-----------------------------------------------------------
-- Patterns (and binders)
-----------------------------------------------------------
binder :: Range -> LexParser (UserExpr -> ValueBinder () UserExpr)
binder preRange
  = do (name,range) <- identifier
       ann <- typeAnnotation
       return (\expr -> ValueBinder name () (ann expr) range (combineRange preRange range))

funid
  = identifier
  <|>
    do rng1 <- special "["
       rng2 <- special "]"
       return (nameIndex,combineRange rng1 rng2)
  -- secretly allow definition of any name
  <|>
    do (s,rng) <- stringLit
       return (newName s, rng)

pattern :: LexParser UserPattern
pattern
  = patAnn

patAnn
  = do p <- patAtom
       maybeTypeAnnot p (\tp0 -> let tp = promoteType tp0
                                 in case p of
                                      PatVar (ValueBinder name Nothing npat rng1 rng2)
                                        -> PatVar (ValueBinder name (Just tp) npat rng1 rng2)
                                      _ -> PatAnn p tp (combineRanged p tp))


patAtom :: LexParser UserPattern
patAtom
  = do (name,rng) <- qconstructor
       (ps,r) <- parensCommasRng namedPattern <|> return ([],rangeNull)
       return (PatCon name ps rng (combineRanged rng r))
  <|>
    do (name,rng) <- identifier
       (do keyword "as" 
           p <- pattern
           return (PatVar (ValueBinder name Nothing p rng (combineRanged rng p)))
        <|>
        return (PatVar (ValueBinder name Nothing (PatWild rng) rng rng)) 
        )
  <|>
    do (_,range) <- wildcard
       return (PatWild range)
  <|>
    do lit <- literal
       return (PatLit lit)
  <|>
    do listPattern
  <|>
    do (ps,rng) <- parensCommasRng namedPattern
       case ps of
         [p] -> return (PatParens (snd p) rng)
         _   -> return (PatCon (nameTuple (length ps)) ps rng rng)

namedPattern :: LexParser (Maybe (Name,Range),UserPattern)
namedPattern
  = do (name,rng) <- try (do{ x <- identifier; keyword "="; return x})
       pat <- pattern
       return (Just (name,rng),pat)
  <|>
    do pat <- pattern
       return (Nothing,pat)

maybeTypeAnnot :: a -> (UserType -> a) -> LexParser a
maybeTypeAnnot def f
  = do tp <- typeAnnot
       return (f tp)
  <|>
    return def


listPattern :: LexParser UserPattern
listPattern
  = do rng1 <- special "[" <?> ""
       es <- sepEndBy pattern comma
       rng2 <- special "]"
       if null es
        then return (makeNilPat (combineRange rng1 rng2))
        else let pat = (foldr makeConsPat (makeNilPat (after rng2)) (es)) :: UserPattern
             in return (PatParens pat (combineRange rng1 rng2))

makeNilPat :: Range -> UserPattern
makeNilPat rng   = PatCon nameNull [] rng rng

makeConsPat :: UserPattern -> UserPattern -> UserPattern
makeConsPat x xs = PatCon nameCons [(Nothing,x),(Nothing,xs)] (getRange x) (getRange x)



{--------------------------------------------------------------------------
  Types
--------------------------------------------------------------------------}
typeAnnot :: LexParser UserType
typeAnnot
  = do keyword ":"
       ptype

typeAnnotPar :: LexParser UserType
typeAnnotPar
  = do keyword ":"
       (do rng <- specialOp "?"
           tp <- ptype
           return (TpApp (TpCon nameTpOptional rng) [tp] (combineRanged rng tp))
        <|>
        do rng <- specialOp "$"
           (eff,res) <- tresultTotal -- todo: use proper result
           return (TpApp (TpCon nameTpDelay rng) [eff,res] (combineRanged rng res))
        <|>
        ptype)

ptypescheme :: LexParser UserType
ptypescheme
 = do tp <- pquanSome <|> ptype
      return (promoteType tp)   -- add quantifiers for free type variables
   <?> "type signature"

ptype :: LexParser UserType
ptype
  = pquanForall
  <|>
    pquanSome
  <|>
    do tqual
  <?> "type"


aquantifier
  = do keyword "forall"
       angles tbinders
  <|> return []

squantifier
  = do keyword "some"
       parens tbinders
  <|> return []


pquanSome
  = pquantifier QSome ptype

pquanForall
  = pquantifier QForall tqual

pquanExists
  = pquantifier QExists ptype

pquantifier quan next
  = do rng <- keyword (case quan of QSome -> "some"; QForall -> "forall"; QExists -> "exists")
       params <- angles tbinders
       -- keyword "."
       tp <- next
       let makeQuan = \tname tp -> TpQuan quan tname tp (combineRanged rng tp)
       return (foldr makeQuan tp params)


tqual
  = do tp  <- tarrow
       pqualifier tp

pqualifier tp
  = do keyword "with"
       ps <- parens (many1 predicate)
       return (TpQual ps tp)
  <|>
    return tp

predicate
  = do tp <- tid
       typeApp tp
  <?> "predicate" -- fail "predicates are not allowed for now"

tarrow :: LexParser UserType
tarrow
  = do (tps,rng1) <- tatom
       (do keyword "->"
           (teff,tres) <- tresultTotal
           return (makeTpFun tps teff tres (combineRanged rng1 tres))
        <|>
        return (tuple (tps,rng1)))


teffect
  = do rng1   <- langle
       labels <- sepBy tlabel comma
       (ext,brng)    <- textend
       rng2   <- rangle
       let rng = combineRange rng1 rng2
       return (foldr (makeEffectExtend brng) (ext rng) labels)

textend :: LexParser (Range -> UserType, Range {- "|" -})
textend
  = do brng <- bar
       tp <- teffect <|> tid
       return (const tp,brng)
  <|>
    return (makeEffectEmpty, rangeNull)


tlabel
  = do tp1 <- tid
       tp2 <- typeApp tp1
       return tp2


tresultTotal :: LexParser (UserType,UserType)
tresultTotal
  = do (mbeff,tres) <- tresult
       let teff = case mbeff of Just tp -> tp
                                Nothing -> makeTpTotal (before (getRange tres))
       return (teff,tres)

tresult :: LexParser (Maybe UserType,UserType)
tresult
  = do (tps1,rng1)  <- tatom
       (teff,tres) <- do (tps,rng) <- tatom
                         return (Just (tuple (tps1,rng)), tuple (tps,rng))
                      <|>
                         return (Nothing {-makeTpTotal (getRange (map snd tps2))-}, tuple(tps1,rng1)) -- TODO: range
       return (teff,tres)
  where
    merge :: [([(Name,UserType)],Range)] -> [(Name,UserType)]
    merge ts  = concat (map fst ts)

tatomic :: LexParser UserType
tatomic
  = do (ts,rng) <- tatom
       return (tuple (ts,rng))

tuple :: ([(Name,UserType)],Range) -> UserType
tuple ([tp],rng) = snd tp
tuple (tps,rng) = TpApp (TpCon (nameTuple (length tps)) rng) (map snd tps) rng


tatom :: LexParser ([(Name,UserType)],Range)
tatom
  = {- do tp <- listType
       return (single tp)
  <|>
    -}
    do tp1 <- tid
       tp2 <- typeApp tp1
       return (single tp2)
  <|>
    do rng1 <- special "("
       (do tps  <- sepBy paramTypeX comma
           rng2 <- rparen
           {- case tps of
            []  -> return (single (TpCon nameUnit (combineRange rng1 rng2)))
            _   -> -}
           return ([(name,tp) | (name,rng,tp) <- tps], combineRange rng1 rng2)
        <|>
        do cs <- many1 comma
           rng2 <- rparen
           tp <- typeApp (mktuple (length cs + 1) (combineRange rng1 rng2))
           return (single tp)
        )
  <|>
    do tp <- teffect
       return (single tp)
  where
    single tp
      = ([(nameNil,tp)],getRange tp)

    mktuple n rng
      = TpCon (unqualify (nameTuple n)) rng   -- unqualify: means regular lookup

typeApp tp
  = do rng1  <- langle -- liparen
       targs <- sepBy anntypek comma
       rng2  <- rangle
       return (TpApp tp (targs) (combineRanged tp rng2))
  <|>
    do return tp

paramType :: LexParser (Name,Range,UserType)
paramType
  = do (id,rng) <- varid <|> wildcard <|> return (nameNil, rangeNull)
       keyword ":"
       tp <- parameterType rng
       return (id,rng,tp)

paramTypeX
  = do (id,rng) <- try (do v <- varid <|> wildcard; keyword ":"; return v)
       tp <- parameterType rng
       return (id,rng,tp)
  <|>
    do tp <- parameterType rangeNull
       return (nameNil,getRange tp,tp)


parameterType rng
  = do rng2 <- specialOp "?"
       tp <- ptype
       return (TpApp (TpCon nameTpOptional rng) [tp] (combineRanged rng2 tp))
    <|>
    do ptype

{-      <|>
        do rng2 <- specialOp "$"
           ([],eff,res) <- tresultTotal -- todo: use proper result
           return (id, rng, TpApp (TpCon nameTpDelay rng) [eff,res] (combineRanged rng2 res))
-}


anntypek
  = do tp <- ptype
       (do specialOp "::"
           kind <- pkind
           return (TpAnn tp kind)
        <|>
           return tp)

tid
  = do (id,rng) <- qvarid <|> typeidCtx
       return (if isTypeVar id then TpVar id rng else TpCon id rng)
  <|>
    do (id,rng) <- wildcard <?> ""
       return (TpVar id rng)

listType
  = do rng1 <- special "["
       ( do tp   <- ptype
            rng2 <- special "]"
            return (TpApp (TpCon nameTpList rng1) [tp] (combineRange rng1 rng2))
        <|>
         do rng2 <- special "]"
            return (TpCon nameTpList (combineRange rng1 rng2))
        )


-- Just before or after a token.
before range
  = makeRange (rangeStart range) (rangeStart range)

after range
  = makeRange (rangeEnd range) (rangeEnd range)


makeTpFun args effect res
  = -- TpApp (TpApp (makeTpApp (TpCon (nameTpFun (length args)) (combineRanged args res)) args) effect) res
    TpFun args effect res


makeTpApp tp args
  = TpApp tp args

makeApp expr args
  = App expr [(Nothing,a) | a <- args] (combineRanged expr args)

makeTpPure rng
  = TpCon nameTpPure rng

makeTpTotal rng
  = TpCon nameEffectEmpty rng

makeEffectEmpty rng
  = TpCon nameEffectEmpty rng

makeEffectExtend rng (label) ext
  = TpApp (TpCon nameEffectExtend rng) [label,ext] (combineRanged (getRange label) ext)

{--------------------------------------------------------------------------
  Type binder
--------------------------------------------------------------------------}
tbinderDef :: LexParser (UserKind -> TypeBinder UserKind)
tbinderDef
  = do (id,rng) <- tbinderId
       return (\kind -> TypeBinder id kind rng rng)

tbinderId
  = typeid <|> tlist <|> ttuple
--  <|> toptional <|> tdelay
  <|> temptyOrExtend

tlist
  = do rng1 <- special "["
       rng2 <- special "]"
       return (unqualify nameTpList,combineRange rng1 rng2)  -- unqualify: local lookup?

ttuple
  = do rng1 <- lparen
       cs   <- many (comma)
       rng2 <- rparen
       return (unqualify (nameTuple (length cs+1)), combineRange rng1 rng2) -- unqualify: local lookup?


temptyOrExtend
  = do rng1 <- langle
       (do bar <?> "extend bar"
           rng2 <- rangle
           return (nameEffectExtend, combineRange rng1 rng2)
        <|>
        do rng2 <- rangle
           return (nameEffectEmpty, combineRange rng1 rng2))


tbinders :: LexParser [TypeBinder UserKind]
tbinders
  = sepBy tbinder (comma)

tbinder :: LexParser (TypeBinder UserKind)
tbinder
  = do (id,rng) <- varid <?> "type parameter"
       kind     <- kindAnnot
       return (TypeBinder id kind rng rng)

kindAnnot :: LexParser UserKind
kindAnnot
  = do specialOp "::"
       kind <- pkind
       return kind
  <|>
    return KindNone



{--------------------------------------------------------------------------
  Kinds
--------------------------------------------------------------------------}
pkind :: LexParser UserKind
pkind
  = do params <- parensCommas pkind
       keyword "->"
       res    <- pkind
       return (foldr KindArrow res params)
  <|>
    do k <- katom
       (do keyword "->"
           res <- pkind
           return (KindArrow k res)
        <|>
        return k)
  <?> "kind"

katom
  = do parensx lparen KindParens pkind
  <|>
    do rng <- specialConId "V"
       return (KindCon nameKindStar rng)
  <|>
    do rng <- specialConId "X"
       return (KindCon nameKindLabel rng)
  <|>
    do rng <- specialConId "E"
       return (KindCon nameKindEffect rng)
  <|>
    do rng <- specialConId "H"
       return (KindCon nameKindHeap rng)
  <|>
    do rng <- specialConId "S"
       return (KindCon nameKindScope rng)
  <|>
    do rng <- specialConId "P"
       return (KindCon nameKindPred rng)
  <|>
    do rng <- specialConId "HX"
       return (KindCon nameKindHandled rng)
  <|>
    do rng <- specialConId "HX1"
       return (KindCon nameKindHandled1 rng)
  <?> "kind constant (V,E,H,S,X,HX,HX1, or P)"

-----------------------------------------------------------
-- Braces and parenthesis
-----------------------------------------------------------
semiBraces :: LexParser a -> LexParser [a]
semiBraces p
  = do (xs,rng) <- semiBracesRanged p
       return xs

semiBracesRanged :: LexParser a -> LexParser ([a],Range)
semiBracesRanged p
  = do rng1 <- lcurly
       many semiColon
       xs <- sepEndBy p semiColons1
       rng2 <- rcurly
       return (xs,combineRange rng1 rng2)

semiBracesRanged1 :: LexParser a -> LexParser ([a],Range)
semiBracesRanged1 p
  = do rng1 <- lcurly
       many semiColon
       xs <- sepEndBy1 p semiColons1
       rng2 <- rcurly
       return (xs,combineRange rng1 rng2)

semiBraced :: LexParser a -> LexParser a
semiBraced p
  = do rng1 <- lcurly
       many semiColon
       x <- p
       many semiColon
       rng2 <- rcurly
       return x

semis p
  = sepEndBy p semiColons1

semiColons1
  = many1 semiColon

anglesRanged p
  = bracketed langle rangle (,) p


anglesCommas p
  = angles (sepBy p comma)

angles p
  = bracketed langle rangle const p

parensCommas p
  = parensx lparen const (sepBy p comma)

parensRng p
  = parensx lparen (,) p

parens p
  = parensx lparen const p

parensCommasRng p
  = parensx lparen (,) (sepBy p comma)

parensx lpar f p
  = bracketed lpar rparen f p

curlies p
  = curliesx const p

curliesx f p
  = bracketed lcurly rcurly f p

bracketed open close f p
  = do rng1 <- open
       x <- p
       rng2 <- close
       return (f x (combineRanged rng1 rng2))


-----------------------------------------------------------
-- Lexical tokens
-----------------------------------------------------------
lapp     = lparen      -- special "(.apply" <?> show "("
lidx     = special "[" -- special "[.index" <?> show "["
lparen   = special "(" -- <|> liparen
rparen   = special ")"
langle   = specialOp "<"
rangle   = specialOp ">"

--lcurly   = special "{" <|> 
--rcurly   = special "}"

bar      = keyword "|" -- specialOp "|"
comma    = special ","


{-
liparen :: LexParser Range
liparen
  = do (Lexeme rng _) <- parseLex LexLIParen
       return rng
-}

semiColon :: LexParser Range
semiColon
  = do (Lexeme rng _) <- parseLex LexInsSemi <|> parseLex (LexSpecial ";")
       return rng
  <?> show ";"

lcurly :: LexParser Range
lcurly
  = do (Lexeme rng _) <- parseLex LexInsLCurly <|> parseLex (LexSpecial "{")
       return rng
  <?> show "{"

rcurly :: LexParser Range
rcurly
  = do (Lexeme rng _) <- parseLex LexInsRCurly <|> parseLex (LexSpecial "}")
       return rng
  <?> show "}"


-----------------------------------------------------------
-- Identifiers & Operators
-----------------------------------------------------------

identifier
  = ensureUnqualified "identifier" qidentifier


qidentifier :: LexParser (Name,Range)
qidentifier
  = qvarid <|> qidop

qconstructor :: LexParser (Name,Range)
qconstructor
  = qconid

qoperator :: LexParser (Name,Range)
qoperator
  = qop
  {-
  <|>
    do rng1 <- special "`"
       (name,rng) <- (qidentifier <|> qconstructor)
       rng2 <- special "`"
       return (name,rng {- combineRange rng1 rng2 -})
  -}

-----------------------------------------------------------
-- Unqualified Identifiers
-----------------------------------------------------------
varid :: LexParser (Name,Range)
varid
  = ensureUnqualified "identifier" qvarid

idop :: LexParser (Name,Range)
idop
  = ensureUnqualified "operator" qidop

conid :: LexParser (Name,Range)
conid
  = ensureUnqualified "constructor" qconid
  -- secretly allow any name
  <|>
    do (s,rng) <- stringLit
       return (newName s,rng)

op :: LexParser (Name,Range)
op = ensureUnqualified "operator" qop

typeid ::  LexParser (Name,Range)
typeid
  = do (name,rng) <- qtypeid
       if (isQualified name)
        then fail "qualified type variable"
        else return (name,rng)

ensureUnqualified :: String -> LexParser (Name,Range) -> LexParser (Name,Range)
ensureUnqualified entity p
  = do (name,rng) <- p
       if (isQualified name)
        then fail ("qualified " ++ entity)
        else return (name,rng)

-----------------------------------------------------------
-- Lexical tokens
-----------------------------------------------------------
qtypeid, typeidCtx :: LexParser (Name,Range)
qtypeid
  = try $
    do pos <- getPosition
       (name,range) <- qvarid <|> typeidCtx      
       if (not (isTypeVar name))
        then return (name,range)
        else -- trace ("not a qtype: " ++ show name) $
             do setPosition pos
                mzero <?> "type name (and not type variable)"

typeidCtx
  = do r <- keyword "ctx"
       return (newName "ctx",r) 

qop :: LexParser (Name,Range)
qop
  = do (Lexeme rng (LexOp id)) <- parseLex (LexOp nameNil)
       return (id,rng)
  <?> "operator"

prefixOp :: LexParser UserExpr
prefixOp
  = do (Lexeme rng (LexPrefix id)) <- parseLex (LexPrefix nameNil)
       return (Var id True rng)
  <?> ""

-- is really qvarid, varid, from the spec
qvarid :: LexParser (Name,Range)
qvarid
  = do (Lexeme rng (LexId id)) <- parseLex (LexId nameNil)
       return (id,rng)
  <?> "identifier"

-- is really qidop and idop from the spec
qidop :: LexParser (Name,Range)
qidop
  = do (Lexeme rng (LexIdOp id)) <- parseLex (LexIdOp nameNull)
       return (id,rng)
  <?> ""

-- is really qconid and conid in the spec
qconid :: LexParser (Name,Range)
qconid
  = do (Lexeme rng (LexCons id)) <- parseLex (LexCons nameNil)
       return (id,rng)
  <?> "constructor"

modulepath :: LexParser (Name,Range)
modulepath
  = do (id,rng) <- qvarid
       return (newName (showPlain id), rng) -- return the entire module path as one identifier
  <?> "module path"

wildcard :: LexParser (Name,Range)
wildcard
  = do (Lexeme rng (LexWildCard id)) <- parseLex (LexWildCard nameNil)
       if (showPlain id == "_")
        then let p = rangeStart rng
             in return (uniqueRngName rng "_w", rng)
        else return (id,rng)
  <?> "wildcard"


integer :: LexParser (Integer,Range)
integer
  = do (Lexeme rng (LexInt i _)) <- parseLex (LexInt 0 "0")
       return (i,rng)
  <?> "integer"

floatLit :: LexParser (Double,Range)
floatLit
  = do (Lexeme rng (LexFloat f _)) <- parseLex (LexFloat 0.0 "0.0")
       return (f,rng)
  <?> "float"

charLit :: LexParser (Char,Range)
charLit
  = do (Lexeme rng (LexChar c)) <- parseLex (LexChar ' ')
       return (c,rng)
  <?> "character"

stringLit :: LexParser (String,Range)
stringLit
  = do (Lexeme rng (LexString s)) <- parseLex (LexString "")
       return (s,rng)
  <?> "string"


specialOp :: String -> LexParser Range
specialOp s
  = try (
      do (Lexeme rng (LexOp op)) <- parseLex (LexOp nameNil)
         if (showPlain op == s)
          then return rng
          else fail s
      <?> show s
    )

specialId :: String -> LexParser Range
specialId s
  = try (
      do (Lexeme rng (LexId id)) <- parseLex (LexId nameNil)
         if (showPlain id == s)
          then return rng
          else fail s
      <?> show s
    )

specialConId :: String -> LexParser Range
specialConId s
  = try (
      do (Lexeme rng (LexCons id)) <- parseLex (LexCons nameNil)
         if (showPlain id == s)
          then return rng
          else fail s
      <?> show s
    )



special :: String -> LexParser Range
special s
  = do (Lexeme rng _) <- parseLex (LexSpecial s)
       return rng
  <?> show s


specialIdOr :: String -> [String] -> LexParser Range
specialIdOr kw [] = specialId kw
specialIdOr kw deprecated
  = choice (specialId kw : map deprecate deprecated)
  where
    deprecate  k = do rng <- specialId k
                      warnDeprecated k kw rng
                      return rng


keywordOr :: String -> [String] -> LexParser Range
keywordOr kw [] = keyword kw
keywordOr kw deprecated
  = choice (keyword kw : map deprecate deprecated)
  where
    deprecate  k = do rng <- keyword k
                      warnDeprecated k kw rng
                      return rng

dockeywordOr :: String -> [String] -> LexParser (Range,String)
dockeywordOr kw [] = dockeyword kw
dockeywordOr kw deprecated
  = choice (dockeyword kw : map deprecate deprecated)
  where
    deprecate k  = do x <- dockeyword k
                      warnDeprecated k kw (fst x)
                      return x


keyword :: String -> LexParser Range
keyword s
  = do (Lexeme rng _) <- parseLex (LexKeyword s "")
       return rng
  <?> show (LexKeyword s "")

dockeyword :: String -> LexParser (Range,String)
dockeyword s
  = do (Lexeme rng (LexKeyword _ doc)) <- parseLex (LexKeyword s "")
       return (rng,doc)
  <?> show s


warnDeprecated dep new rng
  = do pos <- getPosition
       pwarning ("warning " ++ show pos ++ ": keyword \"" ++ dep ++ "\" is deprecated. Consider using \"" ++ new ++ "\" instead.") rng


pwarningMessage msg rng
  = do pos <- getPosition
       pwarning ("warning " ++ show pos ++ ": " ++ msg) rng

pwarning :: String -> Range -> LexParser ()
pwarning msg rng = modifyState (\prev -> prev ++ [(msg, rng)])


uniqueRngHiddenName :: Range -> String -> Name
uniqueRngHiddenName rng prefix =
  let pos  = rangeStart rng
      uniq = show (posLine pos) ++ "_" ++ show (posColumn pos)  
  in newHiddenName (prefix ++ "_" ++ uniq)

uniqueRngName :: Range -> String -> Name
uniqueRngName rng prefix =
  let pos  = rangeStart rng
      uniq = "-l" ++ show (posLine pos) ++ "-c" ++ show (posColumn pos)  
  in newName (prefix ++ uniq)



{--------------------------------------------------------------------------
  Adjust the range of an expression
--------------------------------------------------------------------------}
adjustRange :: Range -> UserExpr -> UserExpr
adjustRange rng expr
  = Parens expr nameNil rng


adjustTpRange :: Range -> UserType -> UserType
adjustTpRange rng tp
  = TpParens tp rng

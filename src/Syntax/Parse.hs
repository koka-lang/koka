------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Parse concrete syntax.
-}
-----------------------------------------------------------------------------
module Syntax.Parse( parseProgramFromFile
                   , parseValueDef
                   , parseTypeDef
                   , parseExpression
                   , parseType

                   -- used by the core parser
                   , lexParse, parseLex, LexParser

                   , visibility, modulepath, importAlias
                   , tbinderId, constructorId, funid, paramid
                   , braced, semiBraces, semis, semiColons
                   , angles, anglesCommas, parensCommas, parens
                   , semiColon, lparen, rparen, langle, rangle, comma, lapp, lidx
                   , qtypeid, qvarid, qconid, qidop, identifier, qoperator, varid
                   , integer, charLit, floatLit, stringLit
                   , special, specialId, specialOp, specialConId
                   , keyword, dockeyword
                   , parseOpenExtend
                   ) where

import Lib.Trace
import Data.List (intersperse)
import Data.Maybe( isJust )
import Data.Either (partitionEithers)
import Lib.PPrint hiding (string,parens,integer,semiBraces,lparen,comma,angles,rparen,rangle,langle)
import qualified Lib.PPrint as PP (string)

import Control.Monad (mzero)
import Text.Parsec hiding (space,tab,lower,upper,alphaNum,sourceName)
import Text.Parsec.Error
import Text.Parsec.Pos           (newPos)

import Common.Name
import Common.NamePrim
import Common.Range hiding (after)
import Common.File
import Platform.Config
import Platform.Runtime( unsafePerformIO, exCatch )
import Common.Error
import Common.Syntax

import Syntax.Syntax
import Syntax.Lexeme
import Syntax.Lexer   ( lexing )
import Syntax.Layout  ( layout )
import Syntax.Promote ( promote, promoteType, quantify, promoteFree )


-----------------------------------------------------------
-- Parser on token stream
-----------------------------------------------------------

type LexParser a  = Parsec [Lexeme] () a -- GenParser Lexeme () a

parseLex :: Lex -> LexParser Lexeme
parseLex lex
  = token showTok posFromTok testTok
  where
    showTok (Lexeme _ lex)       = show lex
    posFromTok (Lexeme range _)  = newPos "" (posLine (rangeStart range)) (posColumn (rangeStart range))
    testTok l@(Lexeme _ lex')    | sameLex lex lex'  = Just l
                                 | otherwise         = Nothing

-----------------------------------------------------------
-- Parse varieties
-----------------------------------------------------------
parseProgramFromFile :: Bool -> FilePath -> IO (Error UserProgram)
parseProgramFromFile semiInsert fname
  = do input <- readInput fname
       return (lexParse semiInsert program fname 1 input)


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

lexParseS semiInsert p sourceName line str
  = lexParse semiInsert p sourceName line (stringToBString str)

lexParse :: Bool -> (Source -> LexParser a) -> FilePath -> Int -> BString -> Error a
lexParse semiInsert p sourceName line rawinput
  = let source = Source sourceName rawinput
        input  = if (isLiteralDoc sourceName) then extractLiterate rawinput else rawinput
        xs = lexing source line input
        lexemes = layout semiInsert xs
    in  -- trace  (unlines (map show lexemes)) $
        case (parse (p source) sourceName lexemes) of
          Left err -> makeParseError (errorRangeLexeme xs source) err
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
       return (Def (ValueBinder name () (Lam [] e r) r r)  r Public defFun ""
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
                                 (rng,doc) <- dockeyword "module"
                                 return (vis,rng,doc)
       -- (rng,doc) <- dockeyword "module"
       (name,rng) <- modulepath
       programBody vis source name rng doc
  <|>
    programBody Public source (newName (noexts (basename (sourceName source)))) (rangeNull) ""

programBody vis source modName nameRange doc
  = do many semiColon
       (imports, fixDefss, topDefss)
          <- braced (do imps <- semis importDecl
                        fixs <- semis fixDecl
                        tdefs <- semis (topdef vis)
                        return (imps,fixs,tdefs))
       many semiColon
       let (defs,typeDefs,externals) = splitTopDefs (concat topDefss)
       return (Program source modName nameRange [TypeDefRec typeDefs] [DefRec defs] (prelude ++ imports) externals (concat fixDefss) doc)
  where
    prelude = if (modName == nameSystemCore)
               then []
               else [Import nameSystemCore nameSystemCore rangeNull Private]

braced p
  = do lcurly
       many semiColon
       x <- p
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
  =   do rng <- keyword "public"; return (Public,rng)
  <|> do rng <- keyword "private"; return (Private,rng)
  <|> return (vis,rangeNull)



{--------------------------------------------------------------------------
  External
--------------------------------------------------------------------------}
externDecl :: Visibility -> LexParser [TopDef]
externDecl dvis
  = do (vis,vrng)  <- visibility dvis
       (krng,doc) <- dockeyword "external" <|> dockeyword "extern"
       (do specialId "include"
           extern <- externalInclude (combineRange vrng krng)
           return [DefExtern extern]
        <|>
        do keyword "import"
           extern <- externalImport (combineRange vrng krng)
           return [DefExtern extern]
        <|>
        do isInline <- do{ specialId "inline"; return True } <|> return False
           (name,nameRng) <- funid
           (pars,args,tp,annotate)
             <- do keyword ":"
                   tp <- ptype  -- no "some" allowed
                   (pars,args) <- genParArgs (promoteType tp)
                   return (pars,args,tp,\body -> Ann body tp (getRange tp))
                <|>
                do tpars <- typeparams
                   (pars,parRng) <- parameters (not isInline) {- allow defaults? -}
                   (teff,tres)   <- annotResult
                   let tp = typeFromPars nameRng pars teff tres
                   genParArgs tp -- checks the type
                   return (pars,genArgs pars,tp,\body -> promote [] tpars [] (Just (Just teff, tres)) body)
           (exprs,rng) <- externalBody
           if (isInline)
            then return [DefExtern (External name tp nameRng (combineRanges [vrng,krng,rng]) exprs vis doc)]
            else do let  externName = newHiddenExternalName name
                         fullRng    = combineRanges [vrng,krng,rng]
                         extern     = External externName tp (before nameRng) (before fullRng) exprs Private doc

                         body       = annotate (Lam pars (App (Var externName False rangeNull) args fullRng) fullRng)
                         binder     = ValueBinder name () body nameRng fullRng
                         extfun     = Def binder fullRng vis defFun doc
                    return [DefExtern extern, DefValue extfun]
        )
  where
    typeFromPars :: Range -> [ValueBinder (Maybe UserType) (Maybe UserExpr)] -> UserType -> UserType -> UserType
    typeFromPars rng pars teff tres
      = promoteType $ TpFun [(binderName p, tp) | p <- pars, let Just tp = binderType p] teff tres rng

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
      = do target  <- externalTarget
           mbId    <- optionMaybe identifier
           (s,rng) <- stringLit
           let id = case mbId of
                      Just(nm,_) -> nm
                      Nothing    -> newName s
           return (target,(id,s))


externalInclude :: Range -> LexParser External
externalInclude rng1
  = do keyword "="
       (entry) <- externalIncludeEntry
       return (ExternalInclude [entry] rng1)
  <|>
    do (entries,rng2) <- semiBracesRanged externalIncludeEntry
       return (ExternalInclude entries (combineRange rng1 rng2))

externalIncludeEntry
  = do target <- externalTarget
       (do specialId "file"
           (fname,rng) <- stringLit
           content <- preadFile fname (Common.Range.sourceName (rangeSource rng))
           return (target,content)
        <|>
        do (s,rng) <- stringLit
           return (target,s)
        )
  where
    preadFile :: FilePath -> FilePath -> LexParser String
    preadFile fname currentFile
      = do pos <- getPosition
           let fpath      = joinPath (dirname currentFile) fname
               mbContent  = unsafePerformIO $ exCatch (do{ -- putStrLn ("reading: " ++ fpath);
                                                           content <- readFile fpath; return (Just content) }) (\exn -> return Nothing)
           case mbContent of
             Just content -> return content
             Nothing      -> fail ("unable to read external file: " ++ fpath)


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
  = do specialId "cs"
       return CS
  <|>
    do specialId "js"
       return JS
  <|>
    return Default



{--------------------------------------------------------------------------
  Fixity declaration
--------------------------------------------------------------------------}
fixDecl :: LexParser FixDefs
fixDecl
  = do assoc <- assocDef
       (n,_) <- integer
       -- convenient to check here, but it really should be done during static analysis.
       if (n < 0 || n > 100)
        then fail "The precedence must be between 0 and 100"
        else return ()
       let prec = fromInteger n
       names <- sepBy1 identifier comma
       return [FixDef name (FixInfix prec assoc) rng | (name,rng) <- names]
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
   do (vis,defvis,vrng,trng,doc) <-
        (try $
          do rng     <- keyword "abstract"
             (trng,doc) <- dockeyword "struct"
             return (Public,Private,rng,trng,doc)
          <|>
          do (vis,vrng) <- visibility dvis
             (trng,doc) <- dockeyword "struct"
             return (vis,vis,vrng,trng,doc))

      tbind <- tbinderDef
      tpars <- angles tbinders <|> return []
      let name = tbind KindNone
          resTp = TpApp (tpCon name) (map tpVar tpars) (combineRanged name tpars)

      (pars,prng)  <- conPars defvis
      let (tid,rng) = getRName name
          conId     = toConstructorName tid
          (usercon,creators) = makeUserCon conId tpars resTp [] pars rng (combineRange rng prng) defvis doc
      return (DataType name tpars [usercon] (combineRanges [vrng,trng,rng,prng]) vis Inductive DataDefNormal False doc, creators)

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
  = do let f kw  allowRec defsort
              =do (rng,doc) <- dockeyword kw
                  sort <- if (allowRec) then do{ keyword "rec"; return Retractive} <|> return defsort
                           else return defsort
                  (ddef,isExtend) <- parseOpenExtend
                  return (sort,rng,doc,ddef,isExtend)
       (f "type" True (Inductive)
        <|>
        f "cotype" False (CoInductive)
        <|>
        f "rectype" False (Retractive))

parseOpenExtend :: LexParser (DataDef,Bool)
parseOpenExtend
  =   do{ specialId "open"; return (DataDefOpen, False) }
  <|> do{ specialId "extend"; return (DataDefOpen, True) }
  <|> return (DataDefNormal, False)
  <?> ""

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
            def  = Def binder rng vis defFun doc
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
  = parensCommasRng (lparen <|> lapp) (conBinder defVis)
  <|>
    return ([],rangeNull)

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
-- Effect definitions
--
-- We don't return a syntactic construction for effects
-- but immediately build the underlying data structures.
-----------------------------------------------------------
effectDecl :: Visibility -> LexParser [TopDef]
effectDecl dvis
  = do (vis,defvis,vrng,erng,doc) <-
          (try $
            do rng     <- keyword "abstract"
               (trng,doc) <- dockeyword "effect"
               return (Public,Private,rng,trng,doc)
            <|>
            do (vis,vrng) <- visibility dvis
               (erng,doc) <- dockeyword "effect"
               return (vis,vis,vrng,erng,doc))
       sort <- do{ keyword"rec"; return Retractive} <|> return Inductive
       singleShot <- do{ specialId "linear"; return True} <|> return False
       isResource <- do{ specialId "resource"; return True} <|> return False
       (id,irng) <- typeid
       (tpars,kind,prng) <- typeKindParams
       mbResource <- if (not isResource) then return Nothing
                      else do specialId "in"
                              tp <- ptype
                              return (Just tp)
       let infkind = case kind of
                      KindNone -> foldr KindArrow
                                    (KindCon (if isResource then nameKindStar else if singleShot then nameKindHandled1 else nameKindHandled) irng)
                                    (map tbinderKind tpars)
                      _ -> kind
           ename   = TypeBinder id infkind irng irng
           effTpH  = TpApp (TpCon (tbinderName ename) (tbinderRange ename)) (map tpVar tpars) irng
           effTp   = if (isResource) then effTpH
                      else TpApp (TpCon (if singleShot then nameTpHandled1 else nameTpHandled) (tbinderRange ename))
                            [effTpH] irng
           rng     = combineRanges [vrng,erng,irng]

           opEffTp = case mbResource of
                       Nothing -> effTp
                       Just rtp -> rtp

           -- declare the effect tag
           effTagName = toOpenTagName id
           effTagDef  = Def (ValueBinder effTagName ()
                              (Lit (LitString (show id ++ "-" ++ basename (sourceName (rangeSource irng))) irng))
                              irng irng) irng vis DefVal ""

           -- declare the effect type (for resources, generate a hidden constructor to check the types)
           effConName = (makeHiddenName "Con" (toConstructorName id))
           resName = newHiddenName "res"


           effTpCons = case mbResource of
                          Nothing -> []
                          Just tp ->
                            let resourceTp = TpApp (TpCon (newQualified "std/core" "resource") irng) [tp] irng
                                cons = [UserCon effConName [] [(Public,ValueBinder nameNil resourceTp Nothing irng irng)] Nothing irng irng vis ""]
                            in cons

           effTpDecl = DataType ename tpars effTpCons rng vis Inductive DataDefNormal False doc

           -- define resource wrapper
           (effResourceDecls, mbResourceInt)
            = case mbResource of
                Nothing -> ([], Nothing)
                Just labelTp ->
                  let createDef =
                          let createName = makeHiddenName "create" id
                              nameCreateResource = newQualified "std/core" ".Resource"
                              body= App (Var effConName False irng)
                                        [(Nothing,App (Var nameCreateResource False irng )
                                                      [(Nothing,Var resName False irng)] irng)]
                                        irng

                              fun = Lam [ValueBinder resName (Just (TpCon nameTpInt irng)) Nothing irng irng]
                                        body irng
                              def = Def (ValueBinder createName () fun irng irng) irng vis (DefFun NoMon) ""
                          in def

                      (resourceDef,resourceBinder,resourceLamBinder,resourceGet) =
                          let resourceGetName = makeHiddenName "resource" id
                              valName = newHiddenName "resource"
                              binder = ValueBinder valName effTp Nothing irng irng
                              rbinder = ValueBinder valName Nothing Nothing irng irng
                              patvar = ValueBinder resName (Nothing) (PatWild irng) irng irng
                              rmatch = Case (Var valName False irng)
                                                [Branch (PatCon effConName [(Nothing,PatCon (newQualified "std/core" ".Resource")
                                                                                   [(Nothing,PatVar patvar)] irng irng)] irng irng)
                                                        (guardTrue)
                                                        (Var resName False irng)] irng
                              fun = Lam [rbinder] rmatch irng
                              def = Def (ValueBinder resourceGetName () fun irng irng) irng vis (DefFun NoMon) ""
                          in (def, binder, rbinder, App (Var resourceGetName False irng) [(Nothing,Var valName False irng)] irng)

                      injectDef =
                          let injectName = newName "inject-resource"
                              actionName = newName "action"
                              body= App (Var nameInjectResource False irng)
                                        [(Nothing,Var effTagName False irng),
                                         (Nothing,resourceGet),
                                         (Nothing,Var actionName False irng)] irng
                              tpVarA    = TpVar (newHiddenName "a") irng
                              tpVarE    = TpVar (newHiddenName "e") irng
                              typeUnit  = TpCon nameUnit rng
                              actionEff = makeEffectExtend irng labelTp tpVarE
                              actionTp  = makeTpFun [] actionEff tpVarA irng
                              fullTp    = promoteType $
                                           makeTpFun [(newName "resource",effTp),
                                                      (newName "action",actionTp)] actionEff tpVarA irng
                              fun = Ann (
                                    Lam [resourceLamBinder,
                                         ValueBinder actionName Nothing Nothing irng irng]
                                        body irng)
                                        fullTp rng

                              def = Def (ValueBinder injectName () fun irng irng) irng vis (DefFun NoMon) ""
                          in def

                  in ([DefValue createDef, DefValue resourceDef, DefValue injectDef], Just (labelTp, resourceBinder, resourceGet))

           -- define the effect operations type (to be used by the type checker
           -- to find all operation definitions belonging to an effect)
           opsName   = TypeBinder (toOperationsName id) KindNone irng irng
           opsTp    = tpCon opsName
           opsResTpVar = TypeBinder (newHiddenName "r") (KindCon nameKindStar irng) irng irng
           -- opsTpApp = TpApp (opsTp) (map tpVar tpars) (combineRanged irng prng)
                      --TpApp (tpCon opsName) (map tpVar tpars) (combineRanged irng prng)
           --extendConName = toEffectConName (tbinderName ename)
           extraEffects = (case mbResourceInt of
                             Just _  -> [TpCon nameTpPartial irng]
                             Nothing -> []) ++
                          (if (sort==Retractive) then [TpCon nameTpDiv irng] else [])


       -- parse the operations and return the constructors and function definitions
       (ops,xrng) <- semiBracesRanged (operation singleShot vis tpars effTagName opEffTp opsTp mbResourceInt extraEffects)

       let (opsConDefs,opTpDecls,mkOpDefs) = unzip3 ops
           opDefs = map (\(mkOpDef,idx) -> mkOpDef idx) (zip mkOpDefs [0..])

           -- declare operations data type (for the type checker)
           opsTpDecl = DataType opsName (tpars++[opsResTpVar]) opsConDefs
                           rng vis sort DataDefNormal False "// internal data type to group operations belonging to one effect"

       return $ [DefType effTpDecl, DefValue effTagDef, DefType opsTpDecl] ++
                  effResourceDecls ++
                  map DefType opTpDecls ++
                  map DefValue opDefs


operation :: Bool -> Visibility -> [UserTypeBinder] -> Name -> UserType -> UserType -> Maybe (UserType, ValueBinder UserType (Maybe UserExpr),UserExpr) -> [UserType] -> LexParser (UserUserCon, UserTypeDef, Integer -> UserDef)
operation singleShot vis foralls effTagName effTp opsTp mbResourceInt extraEffects
  = do (rng0,doc)   <- (dockeyword "function" <|> dockeyword "fun")
       (id,idrng)   <- identifier
       exists0      <- typeparams
       (pars,prng)  <- conPars vis
       keyword ":"
       (mbteff,tres) <- tresult
       teff0 <- case mbteff of
                 Nothing  -> return $
                              foldr (makeEffectExtend idrng) (makeEffectEmpty idrng) (effTp:extraEffects)
                 Just etp -> -- TODO: check if declared effect is part of the effect type
                             -- return etp
                             fail "an explicit effect in result type of an operation is not allowed (yet)"
       let -- teff     = makeEffectExtend rangeNull effTp (makeEffectEmpty rangeNull)
           rng      = combineRanges [idrng,prng,getRange tres]
           nameA    = newName ".a"
           tpVarA   = TpVar nameA idrng


           --nameE    = newName ".e"
           --tpBindE  = TypeBinder nameE (KindCon nameKindLabel idrng) idrng idrng

           -- Create the constructor
           opName   = toOpTypeName id
           opBinder = TypeBinder opName KindNone idrng idrng


           exists   = if (not (null exists0)) then exists0
                       else promoteFree foralls (map (binderType . snd) pars ++ [teff0,tres])
           -- for now add a divergence effect to named effects/resources when there are type variables...
           -- this is too conservative though; we should generate the `ediv` constraint instead but
           -- that is a TODO for now
           teff     = if (not (null (foralls ++ exists)) && isJust mbResourceInt && all notDiv extraEffects)
                       then makeEffectExtend idrng (TpCon nameTpDiv idrng) teff0
                       else teff0
                    where
                      notDiv (TpCon name _) = name /= nameTpDiv
                      notDiv _              = True

           conName  = toOpConName id
           conParams= pars -- [(pvis,par{ binderExpr = Nothing }) | (pvis,par) <- pars]
           conDef   = UserCon conName [] conParams Nothing idrng rng vis ""

           -- Declare the operation as a struct type with one constructor
           opTpDecl = -- trace ("declare op type: " ++ show opName) $
                      DataType opBinder ({-tpBindE:-}foralls ++ exists) [conDef] rng vis Inductive DataDefNormal False doc

           -- Declare the operation constructor for part of the full operations data type
           forallParams= [TpVar (tbinderName par) idrng | par <- foralls]
           tpParams    = forallParams ++ [TpVar (tbinderName par) idrng | par <- exists]
           opsConTpRes = makeTpApp opsTp (forallParams ++ [tres]) rng
           opsConTpArg = makeTpApp (tpCon opBinder) ({-effTp:-}tpParams) rng
           opsConArg   = ValueBinder id opsConTpArg Nothing idrng idrng
           opsConDef = UserCon (toOpsConName id) exists [(Private,opsConArg)] (Just opsConTpRes) idrng rng vis ""

           -- Declare the operation tag name
           opTagName    = toOpenTagName opName
           opTagDef     = Def (ValueBinder opTagName () (Lit (LitString (show id) idrng)) idrng idrng)
                              idrng vis DefVal ""

           -- Declare the yield operation
           opDef tagIdx = -- trace ("create op def: " ++ show id) $
                    let def  = Def binder rng vis defFun ""
                        nameRng   = idrng
                        tag       = Var opTagName False idrng
                        binder    = ValueBinder id () body nameRng nameRng
                        body      = Ann (Lam lparams innerBody rng) tpFull rng
                        conNameVar = Var conName False idrng

                        hasExists = (length exists==0)
                        innerBody
                          = App yieldOp
                                     ([(Nothing, Var effTagName False idrng),
                                       (Nothing, Lit (LitString (show id) idrng)),
                                       (Nothing, case mbResourceInt of
                                                   Nothing -> Lit (LitInt 0 idrng)
                                                   Just (_,binder,expr) -> expr),
                                       (Nothing, Lit (LitInt tagIdx idrng)),
                                       (Nothing, opCon)
                                      ]
                                       ++ [(Nothing, Var nameNothing False idrng) | _ <- exists]) rng

                        yieldOp   = Ann (Var (nameYieldOp (length exists)) False nameRng)
                                        (yieldOpTp) nameRng
                        yieldOpTp = quantify QSome (foralls ++ exists) (TpFun yieldOpTpParams teff tres rng)
                        yieldOpTpParams = [(nameNil,typeString),(nameNil,typeString),(nameNil,TpCon nameTpInt tprng),(nameNil,TpCon nameTpInt tprng),
                                           (nameNil,opsConTpArg)]
                                           ++ [(nameNil,tp) | tp <- typesMaybeX]
                        typesMaybeX  = [TpApp (TpCon nameTpMaybe tprng) [TpVar (tbinderName evar) tprng] tprng | evar <- exists]
                        typeString = TpCon nameTpString tprng
                        tprng      = idrng

                        params0   = [par{ binderType = (if (isJust (binderExpr par)) then makeOptional (binderType par) else binderType par) }  | (_,par) <- pars] -- TODO: visibility?
                        params    = (case mbResourceInt of
                                       Nothing -> []
                                       Just (_,binder,expr)  -> [binder]) ++ params0
                        arguments = [(Nothing,Var (binderName par) False (binderNameRange par)) | par <- params0]
                        opCon     = if null arguments then conNameVar else App conNameVar arguments rng

                        lparams   = [par{ binderType = Nothing} | par <- params] -- ++ [ValueBinder dname Nothing (Just dvalue) nameRng nameRng | (dname,dtype,dvalue) <- defaults]
                        tpParams  = [(binderName par, binderType par) | par <- params] -- ++ [(dname,makeOptional dtype) | (dname,dtype,dvalue) <- defaults]
                        tpFull    = quantify QForall (foralls ++ exists) (TpFun tpParams teff tres rng)

                        makeOptional tp = TpApp (TpCon nameTpOptional (getRange tp)) [tp] (getRange tp)
                        isJust (Just{}) = True
                        isJust _        = False


                    in def
       return (opsConDef,opTpDecl,opDef)



-----------------------------------------------------------
-- Value definitions
-----------------------------------------------------------

pureDecl :: Visibility -> LexParser UserDef
pureDecl dvis
  = do (vis,vrng,rng,doc,isVal) <- try $ do (vis,vrng) <- visibility dvis
                                            (do (rng,doc) <- (dockeyword "function" <|> dockeyword "fun"); return (vis,vrng,rng,doc,False)
                                             <|>
                                             do (rng,doc) <- dockeyword "val"; return (vis,vrng,rng,doc,True))
       (if isVal then valDecl else funDecl) (combineRange vrng rng) doc vis
       -- valueDecl vrng vis <|> functionDecl vrng vis

valueDecl vrng vis
  = do (rng,doc) <- dockeyword "val"
       valDecl (combineRange vrng rng) doc vis

functionDecl vrng vis
  = do (rng,doc) <- dockeyword "fun" <|> dockeyword "function"
       funDecl (combineRange vrng rng) doc vis

varDecl
  = do (vrng,doc) <- dockeyword "var"
       bind <- binder
       keyword ":="
       body <- blockexpr
       return (Def (bind body) (combineRanged vrng body) Private DefVar doc)

valDecl rng doc vis
  = do bind <- binder
       keyword "="
       body <- blockexpr
       return (Def (bind body) (combineRanged rng body) vis DefVal doc)

funDecl rng doc vis
  = do spars <- squantifier
       -- tpars <- aquantifier  -- todo: store somewhere
       (name,nameRng) <- funid
       (tpars,pars,parsRng,mbtres,preds,ann) <- funDef
       body   <- bodyexpr
       let fun = promote spars tpars preds mbtres
                  (Lam pars body (combineRanged rng body))
       return (Def (ValueBinder name () (ann fun) nameRng nameRng) (combineRanged rng fun) vis defFun doc)

-- fundef: forall parameters, parameters, (effecttp, resulttp), annotation
funDef :: LexParser ([TypeBinder UserKind],[ValueBinder (Maybe UserType) (Maybe UserExpr)], Range, Maybe (Maybe UserType, UserType),[UserType], UserExpr -> UserExpr)
funDef
  = do tpars  <- typeparams
       (pars,rng) <- parameters True
       resultTp <- annotRes
       preds <- do keyword "with"
                   parens (many1 predicate)
                <|> return []
       return (tpars,pars,rng,resultTp,preds,id)


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


parameters :: Bool -> LexParser ([ValueBinder (Maybe UserType) (Maybe UserExpr)],Range)
parameters allowDefaults
  = parensCommasRng (lparen <|> lapp) (parameter allowDefaults)

parameter :: Bool -> LexParser (ValueBinder (Maybe UserType) (Maybe UserExpr))
parameter allowDefaults
  = do (name,rng) <- paramid
       tp         <- optionMaybe typeAnnotPar
       (opt,drng) <- if allowDefaults then defaultExpr else return (Nothing,rangeNull)
       return (ValueBinder name tp opt rng (combineRanges [rng,getRange tp,drng]))

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
                    semiColons
                    return [StatExpr (makeReturn rng2 e)]
                 <|>
                    return []
       rng2 <- rcurly
       let stats = stmts1 ++ stmts2
       case (reverse stats) of
         (StatExpr exp:_) -> return (Parens (foldr combine exp (init stats)) (combineRange rng1 rng2))
         []               -> return (Var nameUnit False (combineRange rng1 rng2))
         _                -> fail "Last statement in a block must be an expression"
  where
    combine :: Statement -> UserExpr -> UserExpr
    combine (StatFun f) exp   = f exp
    combine (StatExpr e) exp  = let r = getRange e
                                in Bind (Def (ValueBinder (newName "_") () e r r) r Private DefVal "") exp r

makeReturn r0 e
  = let r = getRange e
    in App (Var nameReturn False r0) [(Nothing,e)] (combineRange r0 r)

data Statement = StatFun (UserExpr -> UserExpr)
               | StatExpr UserExpr

statement :: LexParser Statement
statement
  = do funs <- many1 (functionDecl rangeNull Private)
       return (StatFun (\body -> Let (DefRec funs) body (combineRanged funs body)))
  <|>
    do fun <- localValueDecl <|> localUseDecl <|> localUsingDecl
       return (StatFun fun) -- (\body -> -- Let (DefNonRec val) body (combineRanged val body)
                            --              Bind val body (combineRanged val body)  ))
  <|>
    do var <- varDecl
       return (StatFun (\body -> Bind var body (combineRanged var body)))
  <|>
    do exp <- nofunexpr
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
              in \body -> Bind (Def vbinder rng Private DefVal "") body (combineRanged krng body)
       case unParens(pat) of
         PatVar (binder@ValueBinder{ binderExpr = PatWild _ })
           -> return $ bindVar binder (binderType binder) (binderRange binder)
         PatAnn (PatVar (binder@ValueBinder{ binderExpr = PatWild _})) tp rng
           -> return $ bindVar binder (Just tp) rng
         _ -> return $ \body -> Case e [Branch pat guardTrue body] (combineRanged krng body)

  where
    unParens (PatParens p _) = unParens(p)
    unParens p               = p

localUseDecl
  = do krng <- keyword "use"
       par  <- parameter False
       keyword "="
       e    <- blockexpr
       let bindVar body
            = let fun = Lam [promoteValueBinder par] body (combineRanged krng body)
                  funarg = [(Nothing,fun)]
              in case unParens e of
                App f args range -> App f (args ++ funarg) (combineRanged krng e)
                atom             -> App atom funarg (combineRanged krng e)
       return bindVar
  where
    unParens (Parens p _) = unParens(p)
    unParens p               = p

    promoteValueBinder binder
      = case binderType binder of
          Just tp -> binder{ binderType = Just (promoteType tp)}
          _ -> binder

localUsingDecl
  = do krng <- keyword "using"
       e    <- blockexpr
       let bindVar body
            = let fun = Lam [] body (combineRanged krng body)
                  funarg = [(Nothing,fun)]
              in case unParens e of
                App f args range -> App f (args ++ funarg) (combineRanged krng e)
                atom             -> App atom funarg (combineRanged krng e)
       return bindVar
  where
    unParens (Parens p _) = unParens(p)
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
  = do keyword "->"
       blockexpr
  <|>
    block

expr :: LexParser UserExpr
expr
  = ifexpr <|> matchexpr <|> funexpr <|> funblock <|> opexpr
  <?> "expression"

blockexpr :: LexParser UserExpr
blockexpr
  = ifexpr <|> noifexpr
  <?> "expression"

noifexpr :: LexParser UserExpr
noifexpr
  = returnexpr <|> matchexpr <|> funexpr <|> block <|> opexpr

nofunexpr :: LexParser UserExpr
nofunexpr
  = ifexpr <|> returnexpr <|> matchexpr <|> opexpr
  <?> "expression"


ifexpr
  = do rng <- keyword "if"
       tst <- parens expr
       optional (keyword "then")
       texpr   <- noifexpr
       eexprs  <- many elif
       eexpr   <- do keyword "else"
                     noifexpr
                  <|>
                     return (Var nameUnit False (after (combineRanged texpr (map snd eexprs))))
       let fullMatch = foldr match eexpr ((tst,texpr):eexprs)
                     where
                       match (tst,texpr) eexpr
                        = let trng = getRange tst
                          in  Case tst [Branch (PatCon nameTrue [] trng trng) guardTrue texpr
                                       ,Branch (PatCon nameFalse [] trng trng) guardTrue eexpr]
                                       (combineRanged tst eexpr)

       return fullMatch
  where
    elif
      = do keyword "elif"
           tst <- parens expr
           optional (keyword "then")
           texpr <- noifexpr
           return (tst,texpr)

returnexpr
  = do rng <- keyword "return"
       exp <- expr
       return (makeReturn rng exp)


matchexpr
  = do rng <- keyword "match"
       tst <- parens expr  -- todo: multiple patterns
       (branches,rng2) <- semiBracesRanged1 branch
       return (Case tst branches (combineRange rng rng2))
  <|> handlerExpr

handlerExpr
  = do rng <- keyword "handler"
       mbEff <- do{ eff <- angles ptype; return (Just (promoteType eff)) } <|> return Nothing
       scoped  <- do{ specialId "scoped"; return HandlerScoped } <|> return HandlerNoScope
       hsort   <- handlerSort
       handlerExprX rng mbEff scoped hsort
  <|>
    do rng <- keyword "handle"
       mbEff <- do{ eff <- angles ptype; return (Just (promoteType eff)) } <|> return Nothing
       scoped  <- do{ specialId "scoped"; return HandlerScoped } <|> return HandlerNoScope
       hsort   <- handlerSort
       args <- parensCommas lparen argument
       expr <- handlerExprX rng mbEff scoped hsort
       return (App expr args (combineRanged rng expr))
  where
    handlerSort =     do specialId "resource"
                         res <- do (name,rng) <- qidentifier
                                   return (Just (Var name False rng))
                                <|> return Nothing
                         return (HandlerResource res)
                  <|> do specialId "shallow"; return HandlerShallow
                  <|> return HandlerDeep



handlerExprX rng mbEff scoped hsort
  = do (pars,dpars,rng1) <- handlerParams  -- parensCommas lp handlerPar <|> return []
       let xpars = [par{binderExpr = Nothing} | par <- pars]
       (clauses,rng2)  <- semiBracesRanged (handlerOp xpars)
       (mbReinit,ret,final,ops) <- partitionClauses clauses pars rng
       let reinitFun = case mbReinit of
                         Nothing -> constNull rng
                         Just reinit -> makeNull $
                           let argName = newHiddenName "local"
                               rng = getRange reinit
                               app = App (Var (getName reinit) False rng)
                                        [(Nothing,App (Var nameJust False rng)
                                                      [(Nothing,Var argName False rng)] rng)] rng
                           in Lam [ValueBinder argName Nothing Nothing rng rng] app rng
           handler = Handler hsort scoped mbEff pars reinitFun ret final ops
                       (combineRanged rng pars) (combineRanges [rng,rng1,rng2])
           hasDefaults = any (isJust.binderExpr) dpars
       case mbReinit of
         Nothing
          | hasDefaults -> return $ handlerAddDefaults dpars handler
          | otherwise   -> return $ handler
         Just reinit
          | hasDefaults -> fail "A handler with an 'initially' clause cannot have default values for the local parameters"
          | otherwise   -> case pars of
                             [par] -> return $ handlerAddReinit reinit par handler
                             []    -> fail "A handler with an 'initially' clause must have local parameters"
                             _     -> fail "A handler with an 'initially' clause can only have one local paramter (for now)"

handlerParams :: LexParser ([ValueBinder (Maybe UserType) ()],[ValueBinder (Maybe UserType) (Maybe UserExpr)],Range)
handlerParams
  = do optional (specialId "local")
       (pars,rng) <- parameters True {-allow defaults-} <|> return ([],rangeNull)
       let hpars  = [p{ binderExpr = () } | p <- pars]
       return (hpars, pars, rng)

handlerAddReinit :: UserDef -> (ValueBinder (Maybe UserType) ()) -> UserExpr -> UserExpr
handlerAddReinit reinit par handler
  =let rng  = getRange par
       pinit = App (Var (getName reinit) False rng) [(Nothing,Var nameNothing False rng)] rng
       dpars = [par{ binderExpr = Just(pinit) }]
       --pdef = Def (ValueBinder (binderName par) () pinit rng rng) rng Private DefVal ""
       aname = newHiddenName "action"
   in Let (DefNonRec reinit)
        (Lam [ValueBinder aname Nothing Nothing rng rng]
             (App handler [(Nothing, pinit),(Nothing,Var aname False rng)] rng)
             rng) rng

handlerAddDefaults :: [ValueBinder (Maybe UserType) (Maybe UserExpr)] -> UserExpr -> UserExpr
handlerAddDefaults dpars handler
  =  let rng    = getRange (head dpars) -- safe as dpars is non-empty
         apar   = ValueBinder (newHiddenName "action") Nothing Nothing rng rng
         xpars  = [case binderExpr p of
                     Nothing -> p{binderName = makeHiddenName "par" (binderName p)}
                     Just _  -> p
                  | p <- dpars]
         xargs  = [(Nothing,
                    case binderExpr p of
                      Nothing -> Var (binderName p) False rng
                      Just e  -> e) |  p <- xpars]
                  ++
                  [(Nothing, (Var (binderName apar) False rng) )]
         xxpars = [p | p <- xpars, not (isJust (binderExpr p))]
         hname  = newHiddenName "handler"
         hdef   = Def (ValueBinder hname () handler rng rng) rng Private DefVal ""
         hlam   = Let (DefNonRec hdef) (Lam (xxpars ++ [apar]) (App (Var hname False rng) xargs rng) rng) rng
     in hlam


makeNull expr
  = let rng = getRange expr
    in App (Var nameMakeNull False rng) [(Nothing,expr)] rng

constNull rng
  = Var nameConstNull False rng


data Clause = ClauseRet UserExpr
            | ClauseFinally UserExpr
            | ClauseInitially UserDef
            | ClauseBranch UserHandlerBranch

partitionClauses ::  [Clause] -> [ValueBinder (Maybe UserType) ()] -> Range -> LexParser (Maybe UserDef,UserExpr,UserExpr,[UserHandlerBranch])
partitionClauses clauses pars rng
  = do let (reinits,rets,finals,ops) = separate ([],[],[],[]) clauses
       ret <- case rets of
                [r] -> return (makeNull r)
                []  -> return (Var (if null pars then nameReturnNull else nameReturnNull1) False rng)
                _   -> fail "There can be be at most one 'return' clause in a handler body"
       final <- case finals of
                [f] -> return (makeNull f)
                []  -> return (constNull rng)
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


handlerOp :: [ValueBinder (Maybe UserType) (Maybe UserExpr)] -> LexParser Clause
handlerOp pars
  = do rng <- keyword "return"
       (name,prng) <- paramid
       tp         <- optionMaybe typeAnnotPar
       expr <- bodyexpr
       return (ClauseRet (Lam [ValueBinder name tp Nothing prng (combineRanged prng tp)] expr (combineRanged rng expr)))
  <|>
    do rng <- specialId "finally"
       expr <- bodyexpr
       return (ClauseFinally (Lam pars expr (combineRanged rng expr)))
  <|>
    do rng <- specialId "initially"
       expr <- bodyexpr
       let -- locals are passed as a maybe value to initially clauses
           mpars = [p{binderType = case (binderType p) of
                                     Nothing -> Nothing
                                     Just tp -> Just (TpApp (TpCon nameTpMaybe (getRange tp)) [tp] (getRange tp))
                     }
                    | p <- pars]
           drng = combineRanged rng expr
           lam = Lam mpars expr drng
           name = newHiddenName "reinit"
           def  = Def (ValueBinder name () lam drng drng) drng Private (DefFun NoMon) ""
       return (ClauseInitially def)
  <|>
    do isRaw <-  (do keyword "fun"
                     (do specialId "raw"
                         return True
                      <|> return False)
                  <|> return False)
       (name,nameRng) <- qidentifier
       (pars,prng) <- opParams
       expr <- bodyexpr
       return (ClauseBranch (HandlerBranch name pars expr isRaw nameRng (combineRanges [nameRng,prng])))

opParams :: LexParser ([ValueBinder (Maybe UserType) ()],Range)
opParams
  = parensCommasRng (lparen <|> lapp) opParam <|> return ([],rangeNull)

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
       (grd,exp) <- guard
       return (Branch pat grd exp)
  <?> "pattern match"

guard
  = do bar
       grd <- expr <?> "guard expression"
       keyword "->"
       exp <- blockexpr
       return (grd,exp)
  <|>
    do exp <- bodyexpr
       return (guardTrue, exp)


{--------------------------------------------------------------------------
  Op expr
--------------------------------------------------------------------------}
opexpr :: LexParser UserExpr
opexpr
  = do e1 <- prefixexpr
       (do ess <- many1(do{ op <- operatorVar; e2 <- prefixexpr; return [op,e2]; })
           return (App (Var nameOpExpr True rangeNull) [(Nothing,e) | e <- e1 : concat ess] (combineRanged e1 (concat ess)))
        <|>
           return e1)

operatorVar
  = do (name,rng) <- qoperator
       return (Var name True rng)
    <|>
    do rng <- keyword ":="
       return (Var nameAssign True rng)


prefixexpr :: LexParser UserExpr
prefixexpr
  = do ops  <- many prefixOp
       aexp <- appexpr
       return (foldr (\op e -> App op [(Nothing,e)] (combineRanged op e)) aexp ops)

appexpr :: LexParser UserExpr
appexpr
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
           idxs <- sepBy1 expr comma
           rng1 <- special "]"
           return (\exp -> App (Var nameIndex False (combineRange rng0 rng1)) (map (\a -> (Nothing,a)) (exp:idxs)) (combineRange rng0 rng1))

    applier
      = do rng0 <- lapp
           args <- sepBy argument (comma)
           rng1 <- rparen
           return (\exp -> App exp (args) (combineRanged exp rng1))

    funapps
      = do fs <- many1 bfunexpr
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

bfunexpr
  = funblock <|> funexpr

funblock
  = do exp <- block
       return (Lam [] exp (getRange exp))

funexpr
  = do rng <- keyword "fun.anon" <|> keyword "function.anon"
       spars <- squantifier
       (tpars,pars,parsRng,mbtres,preds,ann) <- funDef
       body <- block
       let fun = promote spars tpars preds mbtres
                  (Lam pars body (combineRanged rng body))
       return (ann fun)


{--------------------------------------------------------------------------
  Atomic expression
--------------------------------------------------------------------------}
atom :: LexParser UserExpr
atom
  = do (name,rng) <- qidentifier <|> qconstructor
       return (Var name False rng)
  <|>
    do tupleExpr -- must be second due to '(' operator ')'
  <|>
    do listExpr
  <|>
    do lit <- literal
       return (Lit lit)
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
         [e] -> return (Parens e (combineRanged rng1 rng2))
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


injectExpr :: LexParser UserExpr
injectExpr
  = do rng1 <- keyword "inject"
       langle
       tp <- ptype
       rangle
       exp <- parens expr <|> funblock
       return (Inject (promoteType tp) exp (combineRanged rng1 exp))

-----------------------------------------------------------
-- Patterns (and binders)
-----------------------------------------------------------
binder :: LexParser (UserExpr -> ValueBinder () UserExpr)
binder
  = do (name,range) <- identifier
       ann <- typeAnnotation
       return (\expr -> ValueBinder name () (ann expr) range range)

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
  = do p <- patAs
       maybeTypeAnnot p (\tp -> PatAnn p tp (combineRanged p tp))

patAs
  = do p <- patAtom
       (do keyword "as"
           (id,rng) <- identifier
           return (PatVar (ValueBinder id Nothing p rng rng))
        <|>
           return p)

patAtom :: LexParser UserPattern
patAtom
  = do (name,rng) <- qconstructor
       (ps,r) <- parensCommasRng (lparen <|> lapp) namedPattern <|> return ([],rangeNull)
       return (PatCon name ps rng (combineRanged rng r))
  <|>
    do (name,rng) <- identifier
       tp <- optionMaybe typeAnnot
       return (PatVar (ValueBinder name tp (PatWild rng) rng (combineRanged rng tp)))  -- could still be singleton constructor
  <|>
    do (_,range) <- wildcard
       return (PatWild range)
  <|>
    do lit <- literal
       return (PatLit lit)
  <|>
    do listPattern
  <|>
    do (ps,rng) <- parensCommasRng lparen namedPattern
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
  = do (id,rng) <- qvarid
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
  = do params <- parensCommas lparen pkind
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
       xs <- sepEndBy p semiColons
       rng2 <- rcurly
       return (xs,combineRange rng1 rng2)

semiBracesRanged1 :: LexParser a -> LexParser ([a],Range)
semiBracesRanged1 p
  = do rng1 <- lcurly
       many semiColon
       xs <- sepEndBy1 p semiColons
       rng2 <- rcurly
       return (xs,combineRange rng1 rng2)

semis p
  = sepEndBy p semiColons

semiColons
  = many semiColon

anglesRanged p
  = bracketed langle rangle (,) p


anglesCommas p
  = angles (sepBy p comma)

angles p
  = bracketed langle rangle const p

parensCommas lpar p
  = parensx lpar const (sepBy p comma)

parensRng p
  = parensx lparen (,) p

parens p
  = parensx lparen const p

parensCommasRng lpar p
  = parensx lpar (,) (sepBy p comma)

parensx lpar f p
  = bracketed lpar rparen f p

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
lapp     = special "(.(" <?> show "("
lidx     = special "[.[" <?> show "["
lparen   = special "(" -- <|> liparen
rparen   = special ")"
langle   = specialOp "<"
rangle   = specialOp ">"
lcurly   = special "{"
rcurly   = special "}"

bar      = specialOp "|"
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
qtypeid
  = try $
    do pos <- getPosition
       (name,range) <- qvarid
       if (not (isTypeVar name))
        then return (name,range)
        else do setPosition pos
                mzero <?> "type name"

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
       return (newName (show id), rng) -- return the entire module path as one identifier
  <?> "module path"

wildcard:: LexParser (Name,Range)
wildcard
  = do (Lexeme rng (LexWildCard id)) <- parseLex (LexWildCard nameNil)
       if (show id == "_")
        then return (newName ("_" ++ show (rangeStart rng)), rng)
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
         if (show op == s)
          then return rng
          else fail s
      <?> show s
    )

specialId :: String -> LexParser Range
specialId s
  = try (
      do (Lexeme rng (LexId id)) <- parseLex (LexId nameNil)
         if (show id == s)
          then return rng
          else fail s
      <?> show s
    )

specialConId :: String -> LexParser Range
specialConId s
  = try (
      do (Lexeme rng (LexCons id)) <- parseLex (LexCons nameNil)
         if (show id == s)
          then return rng
          else fail s
      <?> show s
    )



special :: String -> LexParser Range
special s
  = do (Lexeme rng _) <- parseLex (LexSpecial s)
       return rng
  <?> show s



keyword :: String -> LexParser Range
keyword s
  = do (Lexeme rng _) <- parseLex (LexKeyword s "")
       return rng
  <?> show s

dockeyword :: String -> LexParser (Range,String)
dockeyword s
  = do (Lexeme rng (LexKeyword _ doc)) <- parseLex (LexKeyword s "")
       return (rng,doc)
  <?> show s


{--------------------------------------------------------------------------
  Adjust the range of an expression
--------------------------------------------------------------------------}
adjustRange :: Range -> UserExpr -> UserExpr
adjustRange rng expr
  = Parens expr rng


adjustTpRange :: Range -> UserType -> UserType
adjustTpRange rng tp
  = TpParens tp rng

-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
-- Parse .kki interface files
-----------------------------------------------------------------------------

module Core.Parse( parseCore ) where

import Text.Parsec hiding (space,tab,lower,upper,alphaNum)
import Text.Parsec.Prim( getInput, setInput )

import Common.Failure( failure, assertion, HasCallStack )
import Common.Id
import Common.NamePrim
import Common.Name
import Common.Range hiding (after)

import Common.Error
import Common.Syntax
import qualified Common.NameMap as M


import Syntax.Lexeme
import Syntax.Parse

import Kind.Kind
import Kind.Synonym
import Kind.ImportMap

import Type.Type
import Type.TypeVar
import Type.Assumption
import Core.Core

import Lib.Trace

{--------------------------------------------------------------------------
  Parse core interface files
--------------------------------------------------------------------------}
type ParseInlines = Maybe (Gamma -> Error () [InlineDef])

parseCore :: HasCallStack => FilePath -> FilePath -> IO (Error b (Core, ParseInlines))
parseCore fname sourceName
  = do input <- readInput fname
       return $
          -- Error monad
          do ((res,warn),lexemes) <- lexParse True {-allow @-} False  {- no semi-colon insertion -} id (program sourceName) fname 1 input
             return res


parseInlines :: Core -> Source -> Env -> [Lexeme] -> ParseInlines
parseInlines prog source env [] = Nothing
parseInlines prog source env inlines
  = Just (\gamma -> ignoreSyntaxWarnings $ parseLexemes (pInlines env{ gamma = gamma }) source inlines)

pInlines :: Env -> LexParser [InlineDef]
pInlines env
  = do idefs <- many (inlineDef env)
       eof
       return idefs

program :: FilePath -> Source -> LexParser (Core,ParseInlines)
program srcName source
  = do many semiColon
       (prog,env,inlines) <- pmodule srcName
       eof
       return (prog, parseInlines prog source env inlines)


pmodule :: FilePath -> LexParser (Core,Env,[Lexeme])
pmodule srcName
  = do (rng,doc) <- dockeyword "module"
       keyword "interface"
       (name,_)<- modulepath
       many semiColon
       braced (do (imps,impAliases) <- fmap unzip $ semis importDecl
                  let impMap = foldr (\(asname,name) imp -> case importsExtend asname name imp of { Just imp' -> imp'; Nothing -> imp }) importsEmpty impAliases

                  externImports <- semis externImportDecl
                  fixs <- semis fixDecl
                  (impsyns,env1) <- semisEnv (envInitial name srcName impMap) localAlias
                  (tdefs,env2)   <- semisEnv env1 typeDecl
                  -- add synonyms
                  let syns = concatMap (\td -> case td of
                                                 Synonym info  -> [info]
                                                 _             -> []) tdefs
                      env2 = env1{ syns = synonymsNew (impsyns ++ syns) }

                  defs      <- semis (defDecl env2)
                  externals <- semis (externDecl env2)
                  inlines   <- do special "//.inline-section" <?> ""
                                  lexemes <- getInput
                                  setInput []
                                  return lexemes
                               <|> return []
                  let tdefGroups = map (\tdef -> TypeDefGroup [tdef]) tdefs
                      defGroups  = map DefNonRec defs
                  return (Core name imps (concat fixs) tdefGroups defGroups (externImports ++ externals) doc, env2, inlines)
              )

localAlias :: Env -> LexParser (SynInfo, Env)
localAlias env
  = do try $ do { specialId "local"; keyword "alias" }
       (qname,_) <- qtypeid  -- can be qualified
       range    <- prange env
       let name = envQualify env qname
       (env,params) <- typeParams env
       kind     <- kindAnnotFull
       keyword "="
       tp       <- ptype env
       (rank,_)  <- do{ keyword "="; integer } <|> return (0::Integer,rangeNull)
       let synInfo = SynInfo name kind params tp (fromInteger rank) range Private ""
       return (synInfo, envExtendSynonym env synInfo)

semisEnv :: Env -> (Env -> LexParser (a,Env)) -> LexParser ([a],Env)
semisEnv env p
  = accum [] env
  where
    accum xs env0
      = do (x,env1) <- p env0
           many1 semiColon
           accum (x:xs) env1
      <|>
        return (reverse xs,env0)

vispub :: LexParser Visibility
vispub
  = do keyword "pub"
       return Public
  <|>
    return Private

{--------------------------------------------------------------------------
  Top Declarations
--------------------------------------------------------------------------}
importDecl :: LexParser (Import,(Name,Name))
importDecl
  = do (vis,doc) <- try $ do vis <- vispub
                             (_,doc) <- dockeyword "import"
                             return (vis,doc)
       (asname,name,_,_) <- importAlias
       prov <- pimportProvenance
       pkg <- (do{ keyword "="; (s,_) <- stringLit; return s } <|> return "")
       return (Import name pkg prov vis doc, (asname, name))

pimportProvenance :: LexParser ImportProvenance
pimportProvenance
  = do keyword "pub"
       return ImportPub
  <|>
    do keyword "type"
       return ImportTypes
  <|>
    do specialId "inline"
       return ImportCompiler
  <|>
    return ImportUser

fixDecl :: LexParser [FixDef]
fixDecl
  = do fix <- pfixity
       (op,_) <- identifier
       return [FixDef op fix]

pfixity :: LexParser Fixity
pfixity
  =   {-
      do keyword "prefix"; return FixPrefix
  <|> do keyword "postfix"; return FixPostfix
  <|> -}
      do assoc <- (   do keyword "infix"; return AssocNone
                  <|> do keyword "infixr"; return AssocRight
                  <|> do keyword "infixl"; return AssocLeft)
         (prec,_) <- integer
         return (FixInfix (fromInteger prec) assoc)

{--------------------------------------------------------------------------
  Type definitions
--------------------------------------------------------------------------}
typeDecl :: Env -> LexParser (TypeDef,Env)
typeDecl env
  = do (vis,(ddef,isExtend,dataEff,sort,doc))   <- try $ do vis <- vispub
                                                            info <- typeSort
                                                            return (vis,info)
       tname <- if (isExtend)
                 then do (name,_) <- qtypeid
                         return name
                 else do (name,_) <- tbinderId
                         return (qualify (modName env) name)
                      <|>
                      do (name,_) <- idop -- (<>), (<|>)
                         return (qualify (modName env) name)
       range    <- prange env
       -- trace ("core type: " ++ show tname) $ return ()
       (env,params) <- typeParams env
       kind       <- kindAnnotFull
       cons       <- semiBraces (conDecl tname params sort env) <|> return []
       let cons1    = case cons of
                        [con] -> [con{ conInfoSingleton = True }]
                        _     -> cons
           dataInfo = DataInfo sort tname kind params cons1 range ddef dataEff vis doc
       return (Data dataInfo isExtend, env)
  <|>
    do (vis,doc) <- try $ do vis <- vispub
                             (_,doc) <- dockeyword "alias"
                             return (vis,doc)
       (name,_) <- tbinderId
       range    <- prange env
       --trace ("core alias: " ++ show name) $ return ()
       (env,params) <- typeParams env
       kind     <- kindAnnotFull
       keyword "="
       tp       <- ptype env
       (rank,_)  <- do{ keyword "="; integer } <|> return (0::Integer,rangeNull)
       let qname   = qualify (modName env) name
       let synInfo = SynInfo qname kind params tp (fromInteger rank) range vis doc
       return (Synonym synInfo, envExtendSynonym env synInfo)

conDecl tname foralls sort env
  = do vis <- try $ do vis <- vispub
                       keyword "con"
                       return vis
       (name,(_,doc)) <- docconid
       range    <- prange env
       -- trace ("core con: " ++ show name) $ return ()
       (env1,existss) <- typeParams env
       (env2,params)  <- parameters env1
       vrepr <- parseValueRepr
       tp     <- typeAnnot env2
       let params2 = [(if nameIsNil name then newFieldName i else name, tp) | ((name,tp),i) <- zip params [1..]]
           orderedFields = []  -- no need to reconstruct as it is only used during codegen?
       let con = (ConInfo (qualify (modName env) name) tname foralls existss params2 tp sort range (map (const range) params2) (map (const Public) params2) False
                             orderedFields vrepr vis doc)
       -- trace (show con ++ ": " ++ show params2) $
       return con


typeSort :: LexParser (DataDef, Bool, DataEffect, DataKind,String)
typeSort
  = do isRecursive <- do{ specialId "recursive"; return True } <|> return False
       (ddef0,isExtend,sort,dataEff) <- parseTypeMod
       (_,doc) <- dockeyword "type"
       let ddef = case (isRecursive, ddef0) of
                    (True,ddef) | dataDefIsNormal ddef -> DataDefRec
                    _ -> ddef0
       return (ddef,isExtend,dataEff,sort,doc)

parseTypeMod :: LexParser (DataDef,Bool,DataKind,DataEffect)
parseTypeMod
 =   do{ specialId "open"; return (DataDefOpen, False, Inductive, DataNoEffect) }
 <|> do{ specialId "extend"; return (DataDefOpen, True, Inductive, DataNoEffect) }
 <|> do specialId "value"
        vrepr <- parseValueRepr
        return (DataDefValue vrepr, False, Inductive, DataNoEffect)
 <|> do dataEff <- parseDataEffect
        (
              do{ specialId "co"; return (DataDefNormal, False, CoInductive, dataEff) }
          <|> do{ specialId "rec"; return (DataDefNormal, False, Retractive, dataEff) }
          <|> return (DataDefNormal, False, Inductive, dataEff))
 <?> ""

parseDataEffect :: LexParser DataEffect
parseDataEffect
  = do named <- do{ keyword "named"; return True } <|> return False
       lin   <- do{ specialId "linear"; return True } <|> return False
       keyword "effect"
       return (DataEffect named lin)
    <|>
       return DataNoEffect

parseValueRepr :: LexParser ValueRepr
parseValueRepr
  = braced $ do (raw,_) <- integer
                comma
                (scan,_) <- integer
                comma
                (align,_) <- integer
                return (ValueRepr (fromInteger raw) (fromInteger scan) (fromInteger align))


{--------------------------------------------------------------------------
  Value definitions
--------------------------------------------------------------------------}

defDecl :: Env -> LexParser Def
defDecl env
  = do (vis,sort0,inl,doc) <- try $ do vis <- vispub
                                       (sort,inl,isRec,doc) <- pdefSort
                                       return (vis,sort,inl,doc)
       (name,_) <- funid True <|> idop
       range    <- prange env
       -- inl      <- parseInline
       -- trace ("core def: " ++ show name) $ return ()
       keyword ":"
       (tp,pinfos) <- pdeftype env
       let sort = case sort0 of
                    DefFun _ fip -> DefFun pinfos fip
                    _            -> sort0
       -- trace ("parse def: " ++ show name ++ ": " ++ show tp) $ return ()
       return (Def (qualify (modName env) name) tp
                    -- (error ("Core.Parse: " ++ show name ++ ": cannot get the expression from an interface core file"))
                    exprUnit
                   vis sort inl range doc)

pdefSort
  = do isRec <- do{ specialId "recursive"; return True } <|> return False
       inl <- parseInline
       try $
        (do fip <- parseFip
            (_,doc) <- dockeyword "fun"
            _       <- do { specialOp "**"; return ()}
                       <|>
                       do { specialOp "*"; return () }
                       <|>
                       return ()
            return (defFunEx [] fip,inl,isRec,doc)  -- borrow info comes from type
        <|>
        do (_,doc) <- dockeyword "val"
           return (DefVal,inl,False,doc))

{--------------------------------------------------------------------------
  External definitions
--------------------------------------------------------------------------}
externDecl :: Env -> LexParser External
externDecl env
  = do (vis,fip,doc)  <- try $ do vis <- vispub
                                  fip <- parseFip
                                  (_,doc) <- dockeyword "extern"
                                  return (vis,fip,doc)
       (name,_) <- funid True
       range    <- prange env
       -- trace ("core def: " ++ show name) $ return ()
       keyword ":"
       (tp,pinfos) <- pdeftype env
       formats <- externalBody
       return (External (qualify (modName env) name) tp pinfos formats vis fip range doc)


externalBody :: LexParser [(Target,String)]
externalBody
  = do keyword "="
       call <- externalEntry
       return [call]
  <|>
    do semiBraces externalEntry

externalEntry
  = do target <- externalTarget
       optional (specialId "inline")
       (s,_)  <- stringLit
       return (target,s)

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
    do specialId "vm"
       return VM
  <|>
    return Default


{--------------------------------------------------------------------------
  External imports
--------------------------------------------------------------------------}
externImportDecl ::  LexParser External
externImportDecl
  = do try $ do keyword "extern"
                keyword "import"
       entries <- externalImportBody
       return (ExternalImport entries rangeNull)

externalImportBody :: LexParser [(Target, [(String,String)])]
externalImportBody
  = do keyword "="
       entry <- externalImportEntry
       return [entry]
  <|>
    do semiBraces externalImportEntry
  where
    externalImportEntry
      = do target  <- externalTarget
           keyvals <- semiBraces externalImportKeyVal
           return (target,keyvals)

    externalImportKeyVal
      = do key <- do{ (s,_) <- stringLit; return s }
           keyword "="
           (val,_) <- stringLit
           return (key,val)

{--------------------------------------------------------------------------
  Inline defs
--------------------------------------------------------------------------}
inlineDef :: Env -> LexParser InlineDef
inlineDef env
  = do (sort,inl,isRec,specArgs,doc) <- inlineDefSort
       -- inl        <- parseInline
       -- trace ("core inline def: " ++ show name) $ return ()
       (name,_) <- qfunid
       expr <- parseBody env
       return (InlineDef (envQualify env name) expr isRec inl (if (inl==InlineAlways) then 0 else costExpr expr) sort specArgs)


inlineDefSort
  = do isRec <- do{ specialId "recursive"; return True } <|> return False
       inl <- parseInline
       spec <- do specialId "specialize"
                  (s,_) <- stringLit
                  return [c == '*' | c <- s]
               <|> return []
       pinfos <- do specialId "borrow"
                    (s,_) <- stringLit
                    return [if c == '^' then Borrow else Own | c <- s]
                 <|> return []
       (do fip <- try parseFip
           (_,doc) <- dockeyword "fun"
           return (DefFun pinfos fip,inl,isRec,spec,doc)
        <|>
        do (_,doc) <- dockeyword "val"
           return (DefVal,inl,False,spec,doc))

parseBody env
  = do keyword "="
       expr <- parseExpr env
       semiColon
       return expr

parseExpr :: Env -> LexParser Expr
parseExpr env
  =     parseFun env
    <|> parseForall env
    <|> parseMatch env
    <|> parseLet env
    <|> parseApp env
    <?> "expression"

parseApp env
  = do expr <- parseAtom env
       parseApplies expr
  where
    parseApplies expr
      = do args <- parensCommas (parseExpr env)
           parseApplies (App expr args)
        <|>
        do tps  <- angles (ptype env `sepBy` comma)
           parseApplies (makeTypeApp expr tps)
        <|> return expr

parseAtom env
  =   parseCon env
  <|> parseVar env
  <|> do lit <- parseLit
         return (Lit lit)
  <|> parens (parseExpr env)

parseLet :: Env -> LexParser Expr
parseLet env
  = {-
    do specialId "rec"
       (env',dgs) <- semiBraced (parseDefGroups env)
       let defs = [def | DefNonRec def <- dgs]
       expr <- parseExpr env'
       return (Let [DefRec defs] expr)
  <|>
    -}
    do (env',dgs) <- parseDefGroups env
       expr <- parseExpr env'
       return (Let dgs expr)


parseForall :: Env -> LexParser Expr
parseForall env
  = do keyword "forall"
       (env',tvars) <- typeParams1 env
       expr <- parseExpr env'
       return (TypeLam tvars expr)

parseFun :: Env -> LexParser Expr
parseFun env
  = do keyword "fn"
       eff    <- angles (ptype env) <|> return typeTotal
       (env1,params) <- parameters env
       body   <- semiBraced (parseExpr env1)
       return (Lam [TName name tp | (name,tp) <- params] eff body)

parseMatch :: Env -> LexParser Expr
parseMatch env
  = do keyword "match"
       args <- parensCommas (parseExpr env)
       branches <- semiBraces (parseBranch env)
       return (Case args branches)


parseCon :: Env -> LexParser Expr
parseCon env
  = do name <- qualifiedConId
       con  <- envLookupCon env name
       return $ Con (TName name (infoType con)) (infoRepr con)

parseVar :: Env -> LexParser Expr
parseVar env
  = do (name,_) <- qvarid <|> qidop
       if (isQualified name)
        then envLookupVar env name
        else do tp <- envLookupLocal env name
                return (Var (TName name tp) InfoNone)

parseLit :: LexParser Lit
parseLit
  =   do (i,rng) <- integer
         return (LitInt i)
    <|>
      do (f,rng) <- floatLit
         return (LitFloat f)
    <|>
      do (s,rng) <- stringLit
         return (LitString s)
    <|>
      do (c,rng) <- charLit
         return (LitChar c)
    <?> "literal"


{--------------------------------------------------------------------------
  Binding groups
--------------------------------------------------------------------------}
parseDefGroups :: Env -> LexParser (Env,[DefGroup])
parseDefGroups env
  = do (env1,dg) <- parseDefGroup env
       (env2,dgs) <- parseDefGroups0 env1
       return (env2,dg:dgs)

parseDefGroups0 env
  = parseDefGroups env <|> return (env,[])

parseDefGroup :: Env -> LexParser (Env,DefGroup)
parseDefGroup env
  = do (sort,inl,isRec,doc) <- pdefSort
       (name,_)   <- funid False <|> do{ wildcard; return (nameNil,rangeNull) }
       range      <- prange env
       -- inl        <- parseInline
       tp         <- typeAnnot env
       expr       <- parseBody env
       return (envExtendLocal env (name,tp), DefNonRec (Def name tp expr Private sort inl range doc))

prange :: Env -> LexParser Range
prange env
  = do special "["
       l1 <- linecol
       special ","
       c1 <- linecol
       special ","
       l2 <- linecol
       special ","
       c2 <- linecol
       special "]"
       return (envRange env l1 c1 l2 c2)
  where
    linecol :: LexParser Int
    linecol = do (i,_) <- integer
                 return (fromInteger i)



{--------------------------------------------------------------------------
  Match
--------------------------------------------------------------------------}

parseBranch :: Env -> LexParser Branch
parseBranch env
  = do (env',patterns) <- parsePatterns1 env
       guards <- many1 (parseGuard env')
       return (Branch patterns guards)

parsePatterns1 :: Env -> LexParser (Env, [Pattern])
parsePatterns1 env
  = do (env1,pattern) <- parsePattern env
       (do comma
           (envN,patterns) <- parsePatterns1 env1
           return (envN,pattern:patterns)
        <|> return (env1,[pattern]))

parseGuard :: Env -> LexParser Guard
parseGuard env
  = do grd <- do bar
                 parseExpr env <?> "guard"
              <|>
              return exprTrue
       keyword "->"
       expr <- parseExpr env
       return (Guard grd expr)


type PatBinders = [(Name,Type)]

parsePattern  :: Env -> LexParser (Env,Pattern)
parsePattern env
  = do (env',pat) <- parsePatternBasic env
       (do keyword "as"
           parsePatVar env' pat
        <|>
           return (env',pat))

parsePatternBasic  :: Env -> LexParser (Env,Pattern)
parsePatternBasic env
  = parsePatCon env <|> parsePatVar env PatWild <|> parsePatLit env <|> parsePatWild env
    <|> parens (parsePattern env)

parsePatCon  :: Env -> LexParser (Env,Pattern)
parsePatCon env
  = do skip  <- do specialId "@skip"
                   return True
                <|> return False
       cname <- qualifiedConId
       (env1,exists) <- typeParams env
       (env2,args)  <- do (lparen <|> lapp)
                          x <- parsePatternArgs0 env1
                          rparen
                          return x
       let (patArgs,argTypes)  = unzip args
       resTp <- typeAnnot env2
       con <- envLookupCon env2 cname
       return $ (env2,PatCon (TName cname (infoType con)) patArgs (infoRepr con) argTypes exists resTp (infoCon con) skip)


parsePatternArgs0 :: Env -> LexParser (Env,[(Pattern,Type)])
parsePatternArgs0 env
  = parsePatternArgs1 env <|> return (env,[])

parsePatternArgs1 :: Env -> LexParser (Env,[(Pattern,Type)])
parsePatternArgs1 env
  = do (env1,pattp) <- parsePatternArg env
       (do comma
           (envN,patTps) <- parsePatternArgs1 env1
           return (envN, pattp:patTps)
        <|>
           return (env1,[pattp]))

parsePatternArg :: Env -> LexParser (Env,(Pattern,Type))
parsePatternArg env
  = do (env1,pat) <- parsePattern env
       tp  <- typeAnnot env
       return (env1,(pat,tp))

parsePatVar  :: Env -> Pattern -> LexParser (Env,Pattern)
parsePatVar env pat
  = do (name,_) <- varid
       tp <- typeAnnot env
       let env1 = envExtendLocal env (name,tp)
       return (env1,PatVar (TName name tp) pat)

parsePatLit env
  = do lit <- parseLit
       return (env,PatLit lit)

parsePatWild env
  = do wildcard
       return (env,PatWild)

qualifiedConId
   = do n <-  try $ do modulepath
                       specialOp "/"
                       special "("
                       cs <- many comma
                       special ")"
                       return (length cs)
        return (nameTuple (n+1)) -- (("(" ++ concat (replicate (length cs) ",") ++ ")"))
   <|>
     do (name,_) <- qconid
        return name


qfunid :: LexParser (Name,Range)
qfunid
  = do (name,range) <- funid True  -- allow qualified identifier (for a definition)
       return (requalifyLocally name, range)

{--------------------------------------------------------------------------
  Type signatures, parameters, kind annotations etc
--------------------------------------------------------------------------}


parameters :: Env -> LexParser (Env, [(Name,Type)])
parameters env
  = do iparams <- parensCommas (parameter env False)
                   <|> return []
       let (params,pinfos) = unzip iparams
           env' = foldl envExtendLocal env params
       return (env',params)

parameter :: Env -> Bool -> LexParser ((Name,Type),ParamInfo)
parameter env allowBorrow
  = do (name,pinfo) <-  try (do pinfo <- if allowBorrow then paramInfo else return Own
                                name <- parameterName
                                keyword ":"
                                return (name,pinfo))
                        <|> return (nameNil,Own)
       ({-
        do specialOp "?"
           tp <- ptype env
           return ((name, makeOptionalType tp), pinfo)
        <|>
        -}
        do tp <- ptype env
           return ((name, tp), pinfo))

parameterName :: LexParser Name
parameterName
  = parameterId

parameterId :: LexParser Name
parameterId
  = do (qname,_) <- qvarid <|> qidop  -- implicit/p
       return (requalifyLocally qname)
    <|>
    do (name,_) <- paramid
       return name

typeAnnot :: Env -> LexParser Type
typeAnnot env
  = do keyword ":"
       ptype env

typeAnnotFull :: Env -> LexParser Type
typeAnnotFull env
  = do specialOp "::"
       ptype env

typeParams env
  = typeParams1 env <|> return (env,[])

typeParams1 env
  = angles (tbinders env)

tbinders :: Env -> LexParser (Env,[TypeVar])
tbinders env
  = do bs <- tbinder `sepBy` comma
       let env1 = foldl envExtend env bs
           tvs  = [tv | TVar tv <- [envType env1 name kind | (name,kind) <- bs]]
       return (env1,tvs)

tbinder :: LexParser (Name,Kind)
tbinder
  = do id     <- do (id,_) <- varid <|> wildcard
                    return id
                <?> "type parameter"
       kind   <- kindAnnotFull <|> return kindStar
       return (id,kind)
  <|>
    parens tbinder
  <?>
    "quantifier"


kindAnnotFull :: LexParser Kind
kindAnnotFull
  = do specialOp "::"
       pkind
  <|>
    return kindStar

kindAnnot
  = do specialOp "::"
       pkind
  <|>
    return kindStar

{--------------------------------------------------------------------------
  Types
--------------------------------------------------------------------------}
ptype :: Env -> LexParser Type
ptype env
  = do (tp,_) <- ptypex env False
       return tp

pdeftype :: Env -> LexParser (Type, [ParamInfo])
pdeftype env
  = do (tp,pinfos) <- ptypex env True
       return (tp, if all (==Own) pinfos then [] else pinfos)

ptypex :: Env -> Bool -> LexParser (Type, [ParamInfo])
ptypex env allowBorrow
  = do (quantify,env1) <- pforall env
       (tp,pinfos) <- tarrow env1 allowBorrow
       preds <- pqualifier env1
       return (quantify preds tp, pinfos)
  <?> "type"

pforall :: Env -> LexParser ([Pred] -> Rho -> Type, Env)
pforall env
  = do keyword "forall"
       (env1,params) <- typeParams1 env
       return (\ps rho -> TForall params ps rho, env1)
  <|>
    return (\ps rho -> if null ps then rho else TForall [] ps rho, env)

pqualifier :: Env -> LexParser [Pred]
pqualifier env
  = do keyword "with"
       many1 (predicate env)
  <|>
    return []

predicate env
  = do (name,_) <- qvarid
       tps <- angles (ptype env `sepBy` comma) <|> return []
       return (PredIFace (envQualify env name) tps)
  <?> "predicate"

tarrow :: Env -> Bool -> LexParser (Type, [ParamInfo])
tarrow env allowBorrow
  = do etp <- tatomParams env allowBorrow
       case etp of
         Left (params,pinfos)
          -> do keyword "->"
                tp <- tresult env params
                return (tp, pinfos)
             <|>
             do tp <- extract params "unexpected parameters not followed by an ->"
                t <- ptypeApp env tp
                return (t, pinfos)
         Right tp
          -> return (tp, [])

tresult :: Env -> [(Name,Type)] -> LexParser Type
tresult env params
  = do tp1 <- tatom env
       (do tp2 <- tatom env
           return (TFun params tp1 tp2)
        <|>
           return (TFun params typeTotal tp1))


tatom :: Env -> LexParser Type
tatom env
  = do etp <- tatomParamsEx False env False {-allowBorrow-}
       case etp of
         Left (params,_) -> do tp <- extract params "expecting single type"
                               ptypeApp env tp
         Right tp    -> return tp

extract params msg
  = case params of
      [] -> return typeUnit
      [(name,tp)] | name == nameNil -> return tp
      _  -> if all (\(name,_) -> name == nameNil) params
             then return (TApp (typeTuple (length params)) (map snd params))
             else fail msg

tatomParams :: Env -> Bool -> LexParser (Either ([(Name,Type)], [ParamInfo]) Type)
tatomParams env allowBorrow
  = tatomParamsEx True env allowBorrow

tatomParamsEx allowParams env allowBorrow
  = do special "("
       (do iparams <- parameter env allowBorrow `sepBy` comma
           special ")"
           let (params,pinfos) = unzip iparams
           return (Left (params,pinfos))
        <|>
        do cs <- many1 comma
           special ")"
           tp <- ptypeApp env (typeTuple (length cs+1))
           return (single tp)
        )
    <|>
     do tp1 <- tid env           -- note: must come after '(' match
        tp2 <- ptypeApp env tp1
        return (single tp2)
    <|>
     do tp <- teffect env
        return (single tp)
    <|>
     do special "?"
        tp <- tatom env
        return (single (makeOptionalType tp))
    <?>
     "type atom"
  where
    single tp   = Right tp

ptypeApp env tp
  = do tps <- angles (ptype env `sepBy` comma) <|> return []
       psynonym env tp tps

psynonym env tp tps
  = do specialOp "=="
       (rank,_) <- integer <|> return (0,rangeNull)
       body <- ptype env
       case tp of
         TCon (TypeCon name kind)
           -> -- trace ("make type syn: " ++ show name) $
              case synonymsLookup name (syns env) of
                Just info@(SynInfo synname kind params syntp rank range vis doc)
                  -> return (TSyn (TypeSyn name kind rank (Just info)) tps body)
                _ -> return (TSyn (TypeSyn name kind (fromInteger rank) Nothing) tps body)
         TSyn _ _ _ | null tps
           -> return tp
         _ -> fail ("illegal type alias expression: " ++ show tp)
  <|> return (envTypeApp env tp tps)

teffect env
  = do langle
       labels <- tlabel env `sepBy` comma
       ext    <- textend env
       rangle
       return (foldr shallowEffectExtend ext labels)

textend env
  = do bar
       tp <- tatom env
       return tp
  <|>
    return effectEmpty


tlabel env
  = do tp1 <- tatom env
       ptypeApp env tp1


tid :: Env -> LexParser Type
tid env
  = do (name,_) <- qvarid <|> qidop {- std/core/types/(<>) -} <|> wildcard {- __c -}
       kind <- kindAnnotFull <|> return kindStar
       return (envType env name kind)



{--------------------------------------------------------------------------
  Kinds
--------------------------------------------------------------------------}

pkind :: LexParser Kind
pkind
  = do params <- parensCommas pkind
       keyword "->"
       res    <- pkind
       return (foldr kindFun res params)
  <|>
    do k <- katom
       (do keyword "->"
           res <- pkind
           return (kindFun k res)
        <|>
        return k)
  <?> "kind"

katom
  = do parens pkind
  <|>
    do specialConId "V"
       return kindStar
  <|>
    do specialConId "X"
       return kindLabel
  <|>
    do specialConId "E"
       return kindEffect
  <|>
    do specialConId "H"
       return kindHeap
 <|>
    do specialConId "S"
       return kindScope
   <|>
    do specialConId "HX"
       return kindHandled
  <|>
    do specialConId "HX1"
       return kindHandled1
  <|>
    do specialConId "P"
       return kindPred
  <?> "kind"

{--------------------------------------------------------------------------
  Environment to create type variables
--------------------------------------------------------------------------}
data Env = Env{ bound :: !(M.NameMap TypeVar)
              , syns  :: !Synonyms
              , modName :: !Name
              , srcPath :: !FilePath
              , imports :: !ImportMap
              , unique  :: !Int
              , gamma  ::  !Gamma            -- only used for inline definitions
              , locals :: !(M.NameMap Type) -- only used for inline definitions
              }

envRange :: Env -> Int -> Int -> Int -> Int -> Range
envRange env l1 c1 l2 c2
  = makeSourceRange (srcPath env) l1 c1 l2 c2

envInitial :: Name -> FilePath -> ImportMap -> Env
envInitial modName srcPath imports
  = Env M.empty synonymsEmpty modName srcPath imports 1000 gammaEmpty M.empty

envExtend :: Env -> (Name,Kind) -> Env
envExtend (Env env syns mname srcpath imports unique gamma locals) (name,kind)
  = let id = newId unique
        tv = TypeVar id kind Bound
    in Env (M.insert name tv env) syns mname srcpath imports (unique+1) gamma locals

envType :: Env -> Name -> Kind -> Type
envType env@(Env bound syns mname _ _ _ _ _) name kind
  = case M.lookup name bound of
      Nothing -> let qname = envQualify env name
                 in case synonymsLookup qname syns of
                      Just info@(SynInfo name kind params tp rank range vis doc) | null params
                        -> -- trace ("type synonym1: " ++ show info) $
                           TSyn (TypeSyn name kind rank (Just info)) [] tp
                      _ -> {- (if (qname == nameTpST)
                            then trace ("st as con in " ++ show mname ++ ": " ++ show qname ++ ": " ++ show syns)
                            else id) $  -}
                           TCon (TypeCon qname kind)
      Just tv -> TVar tv

envQualify :: Env -> Name -> Name
envQualify (Env _ _ mname _ imports _ _ _) name
  = if isQualified name
     then case (importsExpand name imports) of
            Right (qname,_) -> qname
            Left amb        -> trace ("Core.Parse.envQualify: unable to expand name: " ++ show name ++ ": " ++ show amb) $
                               name
     else qualify mname name

envExtendSynonym :: Env -> SynInfo -> Env
envExtendSynonym env synInfo
  = -- trace ("core extend syns: " ++ show synInfo) $
    env{ syns = synonymsExtend synInfo (syns env) }

envTypeApp :: Env -> Type -> [Type] -> Type
envTypeApp env tp tps
  = case tp of
      TCon (TypeCon name0 kind0)
        -> case synonymsLookup name0 (syns env) of
            Just synInfo@(SynInfo name kind params syntp rank range vis doc) | length params == length tps
              -> assertion ("Core.Parse.envTypeApp: kind/name does not match in type synonym: " ++ show (tp,tps,name0,kind0,synInfo) )
                           (name==name0 && kind==kind0) $
                 -- trace ("core: fix synonym: " ++ show name) $
                 TSyn (TypeSyn name kind rank (Just synInfo)) tps (subNew (zip params tps) |-> syntp)
            _ -> typeApp tp tps
      _ -> typeApp tp tps


envExtendLocal :: Env -> (Name,Type) -> Env
envExtendLocal (Env env syns mname srcpath imports unique gamma locals) (name,tp)
  = Env env syns mname srcpath imports (unique+1) gamma (M.insert name tp locals)


envLookupLocal :: Env -> Name -> LexParser Type
envLookupLocal env name
  = case M.lookup name (locals env) of
      Just tp -> return tp
      Nothing -> fail $ "unbound local: " ++ show name


envLookupCon :: Env -> Name -> LexParser NameInfo
envLookupCon env name
  = case gammaLookupExactCon name (gamma env) of
     [con@(InfoCon{})] -> return con
     res               -> fail $ "when parsing " ++ show (modName env) ++ " unknown constructor: " ++ show name ++ ": " ++ show res -- ++ ":\n" ++ show (gamma env)

envLookupVar :: Env -> Name -> LexParser Expr
envLookupVar env name
 = let lqname = requalifyLocally name
   in case M.lookup lqname (locals env) of
     Just tp -> return (Var (TName lqname tp) InfoNone)   -- implicit/par
     _ -> case gammaLookupCanonical name (gamma env) of
            [fun@(InfoFun{})] -> return $ coreExprFromNameInfo name fun
            [val@(InfoVal{})] -> return $ coreExprFromNameInfo name val
            [extern@(Type.Assumption.InfoExternal{})] -> return $ coreExprFromNameInfo name extern
            res               -> fail $ "unknown identifier: " ++ show name ++ ": " ++ show res --  ++ ":\n" ++ show (gamma env)

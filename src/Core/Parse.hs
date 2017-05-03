-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Pretty-printer for core-F 
-}
-----------------------------------------------------------------------------

module Core.Parse( parseCore ) where

import Text.Parsec hiding (space,tab,lower,upper,alphaNum)


import Common.Failure( failure, assertion )
import Common.Id
import Common.NamePrim
import Common.Name
import Common.Range hiding (after)

import Common.Error
import Common.Syntax
import qualified Common.NameMap as M

import Syntax.Parse

import Kind.Kind
import Kind.Synonym
import Kind.ImportMap

import Type.Type
import Type.TypeVar
import Core.Core

import Lib.Trace

{--------------------------------------------------------------------------
  Parse core interface files
--------------------------------------------------------------------------}

parseCore :: FilePath -> IO (Error Core)
parseCore fname
  = do input <- readInput fname
       return (lexParse True program fname 1 input)


program :: Source -> LexParser Core
program source
  = do many semiColon
       p <- pmodule 
       eof
       return p


pmodule :: LexParser Core
pmodule 
  = do (rng,doc) <- dockeyword "module"
       keyword "interface"
       (name,_)<- modulepath
       many semiColon
       braced (do (imps,impAliases) <- fmap unzip $ semis importDecl
                  let impMap = foldr (\(asname,name) imp -> case importsExtend asname name imp of { Just imp' -> imp'; Nothing -> imp }) importsEmpty impAliases
                  
                  fixs <- semis fixDecl
                  (impsyns,env1) <- semisEnv (envInitial name impMap) localAlias 
                  (tdefs,env2)   <- semisEnv env1 typeDecl 
                  {-
                  let syns = concatMap (\td -> case td of
                                                 Synonym info vis -> [info]
                                                 _                -> []) tdefs
                      env2 = env1{ syns = synonymsNew (impsyns ++ syns) }
                  -}
                  defs      <- semis (defDecl env2)
                  externals <- semis (externDecl env2)

                  let tdefGroups = map (\tdef -> TypeDefGroup [tdef]) tdefs
                      defGroups  = map DefNonRec defs
                  return (Core name imps (concat fixs) tdefGroups defGroups externals doc)
              )

localAlias :: Env -> LexParser (SynInfo, Env)
localAlias env
  = do try $ do { keyword "private"; keyword "alias" }
       (qname) <- qualifiedTypeId  -- can be qualified
       let name = envQualify env qname 
       (env,params) <- typeParams env
       kind     <- kindAnnotFull
       keyword "="
       tp       <- ptype env
       (rank,_)  <- do{ keyword "="; integer } <|> return (0::Integer,rangeNull)
       let synInfo = SynInfo name kind params tp (fromInteger rank) rangeNull ""
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


{--------------------------------------------------------------------------
  Top Declarations
--------------------------------------------------------------------------}
importDecl :: LexParser (Import,(Name,Name))
importDecl
  = do (vis,doc) <- try $ do (vis,_) <- visibility Private
                             (_,doc) <- dockeyword "import"
                             return (vis,doc)
       (asname,name,_) <- importAlias
       pkg <- (do{ keyword "="; (s,_) <- stringLit; return s } <|> return "")
       return (Import name pkg vis doc, (asname, name))

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
  = do (ddef0,isExtend,sort,doc) <- typeSort
       ddef       <- do keyword "rec"
                        return (case ddef0 of 
                                  DataDefNormal -> DataDefRec
                                  _ -> ddef0)                         
                     <|> return ddef0
       tname <- if (isExtend)
                 then qualifiedTypeId
                 else do (name,_)   <- tbinderId <|> tbinderDot
                         return (qualify (modName env) name)

       -- trace ("core type: " ++ show name) $ return ()
       (env,params) <- typeParams env
       kind       <- kindAnnotFull
       cons       <- semiBraces (conDecl tname params sort env) <|> return []
       let cons1    = case cons of
                        [con] -> [con{ conInfoSingleton = True }]
                        _     -> cons
           dataInfo = DataInfo sort tname kind params cons1 rangeNull ddef doc
       return (Data dataInfo Public (map (const Public) cons) isExtend, env)
  <|>
    do (_,doc) <- dockeyword "alias"
       (name,_) <- tbinderId <|> tbinderDot
       --trace ("core alias: " ++ show name) $ return ()
       (env,params) <- typeParams env
       kind     <- kindAnnotFull
       keyword "="
       tp       <- ptype env
       (rank,_)  <- do{ keyword "="; integer } <|> return (0::Integer,rangeNull)
       let qname   = qualify (modName env) name
       let synInfo = SynInfo qname kind params tp (fromInteger rank) rangeNull doc       
       return (Synonym synInfo Public, envExtendSynonym env synInfo)

conDecl tname foralls sort env
  = do (_,doc) <- dockeyword "con"
       (name,_)  <- constructorId <|> constructorDot
       -- trace ("core con: " ++ show name) $ return ()
       (env1,existss) <- typeParams env
       params <- parameters env1 
       tp     <- typeAnnot env
       let params2 = [(if nameIsNil name then newFieldName i else name, tp) | ((name,tp),i) <- zip params [1..]]
       return (ConInfo (qualify (modName env) name) tname foralls existss params2 tp sort rangeNull (map (const rangeNull) params2) (map (const Public) params2) False doc)


typeSort :: LexParser (DataDef, Bool, DataKind,String)
typeSort
  = do let f kw sort = do (_,doc) <- dockeyword kw
                          (ddef,isExtend) <- parseOpenExtend
                          return (ddef,isExtend,sort,doc)
       (f "type" Inductive <|> f "cotype" CoInductive <|> f "rectype" Retractive)


{--------------------------------------------------------------------------
  Value definitions 
--------------------------------------------------------------------------}

defDecl :: Env -> LexParser Def
defDecl env
  = do (sort,doc) <- pdefSort
       (name) <- canonical (funid <|> binderDot)
       -- trace ("core def: " ++ show name) $ return ()
       keyword ":"
       tp       <- ptype env
       -- trace ("parse def: " ++ show name ++ ": " ++ show tp) $ return ()
       return (Def (qualify (modName env) name) tp (error ("Core.Parse: " ++ show name ++ ": cannot get the expression from an interface core file")) 
                   Public sort rangeNull doc)

canonical p
  = do (name,_) <- p
       (do keyword "."
           (n,_) <- integer
           return (canonicalName (fromInteger n) name)
        <|> 
           return name)

pdefSort
  = do (_,doc) <- dockeyword "fun" 
       return (DefFun,doc)
  <|>
    do (_,doc) <- dockeyword "val"
       return (DefVal,doc)

       
binderDot
  = parens $
    do keyword "."
       (name,rng) <- identifier
       return (prepend "." name,rng)

constructorDot
  = -- parens $
    do keyword "."
       (name,rng) <- constructorId
       return (prepend "." name,rng)

tbinderDot
  = do keyword "."
       (name,rng) <- tbinderId
       return (prepend "." name,rng)

{--------------------------------------------------------------------------
  External definitions 
--------------------------------------------------------------------------}
externDecl :: Env -> LexParser External
externDecl env
  = do (_,doc) <- dockeyword "external"
       (name) <- canonical (funid  <|> binderDot)
       -- trace ("core def: " ++ show name) $ return ()
       keyword ":"
       tp <- ptype env
       formats <- externalBody
       return (External (qualify (modName env) name) tp formats Public rangeNull doc)


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
  = do specialId "cs"
       return CS
  <|>
    do specialId "js"
       return JS
  <|>
    return Default


{--------------------------------------------------------------------------
  Type signatures, parameters, kind annotations etc
--------------------------------------------------------------------------}

parameters :: Env -> LexParser [(Name,Type)]
parameters env
  = parensCommas (lparen <|> lapp) (parameter env)
    <|>
    return []

parameter :: Env -> LexParser (Name,Type)
parameter env
  = do name <- try (do{ (name,_) <- paramid; keyword ":"; return name}) <|> return nameNil
       (do specialOp "?"
           tp <- ptype env
           return (name, makeOptional tp)
        <|>
        do tp <- ptype env
           return (name, tp))
 

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
           tvs  = [tv | TVar tv <- [envType env1 name kindStar | (name,_) <- bs]]
       return (env1,tvs)

tbinder :: LexParser (Name,Kind)
tbinder
  = do (id,_) <- varid <?> "type parameter"
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
  = do (quantify,env1) <- pforall env
       tp <- tarrow env1
       preds <- pqualifier env1
       return (quantify preds tp)
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

tarrow :: Env -> LexParser Type
tarrow env
  = do etp <- tatomParams env
       case etp of
         Left params
          -> do keyword "->"
                tresult env params
             <|>
             do tp <- extract params "unexpected parameters not followed by an ->"
                ptypeApp env tp
         Right tp
          -> return tp

tresult :: Env -> [(Name,Type)] -> LexParser Type
tresult env params
  = do tp1 <- tatom env
       (do tp2 <- tatom env
           return (TFun params tp1 tp2)
        <|>
           return (TFun params typeTotal tp1))
        

tatom :: Env -> LexParser Type
tatom env
  = do etp <- tatomParamsEx False env
       case etp of
         Left params -> do tp <- extract params "expecting single type"
                           ptypeApp env tp
         Right tp    -> return tp
       
extract params msg
  = case params of
      [] -> return typeUnit
      [(name,tp)] | name == nameNil -> return tp
      _  -> if all (\(name,_) -> name == nameNil) params
             then return (TApp (typeTuple (length params)) (map snd params))
             else fail msg

tatomParams :: Env -> LexParser (Either [(Name,Type)] Type)
tatomParams env
  = tatomParamsEx True env

tatomParamsEx allowParams env
  = do special "("
       (do params <- parameter env `sepBy` comma
           special ")"
           return (Left params)
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
     do specialOp "?"
        tp <- tatom env
        return (single (makeOptional tp))
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
                Just info@(SynInfo synname kind params syntp rank range doc) 
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
  = do specialOp "|"
       tp <- tatom env
       return tp
  <|>
    return effectEmpty


tlabel env
  = do tp1 <- tatom env
       ptypeApp env tp1

tid :: Env -> LexParser Type
tid env
  = do (name) <- qualifiedTypeId 
       kind <- kindAnnotFull <|> return kindStar
       return (envType env name kind)
    
qualifiedTypeId 
  = do (name,_) <- qvarid
       return name
  <|>
    do (name,_) <- qidop  -- for things like std/core/<>
       return name
  <|> 
    do special "("
       cs <- many comma
       special ")"
       return (nameTuple (length cs+1)) -- (("(" ++ concat (replicate (length cs) ",") ++ ")"))
  <|>
    do keyword "."
       (name,_) <- qvarid
       return (prepend "." name)     


{--------------------------------------------------------------------------
  Kinds
--------------------------------------------------------------------------}

pkind :: LexParser Kind
pkind
  = do params <- parensCommas lparen pkind 
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
data Env = Env{ bound :: M.NameMap TypeVar
              , syns  :: Synonyms
              , modName :: Name
              , imports :: ImportMap
              , unique  :: Int
              }

envInitial :: Name -> ImportMap -> Env
envInitial modName imports
  = Env M.empty synonymsEmpty modName imports 0

envExtend :: Env -> (Name,Kind) -> Env
envExtend (Env env syns mname imports unique) (name,kind)
  = let id = newId unique
        tv = TypeVar id kind Bound
    in Env (M.insert name tv env) syns mname imports (unique+1)

envType :: Env -> Name -> Kind -> Type
envType env@(Env bound syns mname _ _) name kind
  = case M.lookup name bound of
      Nothing -> let qname = envQualify env name
                 in case synonymsLookup qname syns of
                      Just info@(SynInfo name kind params tp rank range doc) | null params
                        -> -- trace ("type synonym1: " ++ show info) $
                           TSyn (TypeSyn name kind rank (Just info)) [] tp
                      _ -> {- (if (qname == nameTpST)
                            then trace ("st as con in " ++ show mname ++ ": " ++ show qname ++ ": " ++ show syns) 
                            else id) $  -}
                           TCon (TypeCon qname kind)
      Just tv -> TVar tv

envQualify :: Env -> Name -> Name
envQualify (Env _ _ mname imports _) name
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
            Just synInfo@(SynInfo name kind params syntp rank range doc) | length params == length tps
              -> assertion ("Core.Parse.envTypeApp: kind/name does not match in type synonym: " ++ show (tp,tps,name0,kind0,synInfo) )
                           (name==name0 && kind==kind0) $              
                 -- trace ("core: fix synonym: " ++ show name) $
                 TSyn (TypeSyn name kind rank (Just synInfo)) tps (subNew (zip params tps) |-> syntp)
            _ -> typeApp tp tps
      _ -> typeApp tp tps

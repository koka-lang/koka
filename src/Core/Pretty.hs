-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-    Pretty-printer for core-F
-}
-----------------------------------------------------------------------------

module Core.Pretty( prettyCore, prettyExpr, prettyPattern, prettyDef, prettyDefs, prettyDefGroup ) where

import Lib.Trace
import Data.Char( isAlphaNum )
import qualified Data.Set as S
import Common.Name
import Common.ColorScheme
import Common.Syntax
import qualified Common.NameSet as S
import Lib.PPrint
import Core.Core
import Kind.Kind
import Kind.Pretty hiding (prettyKind)
import Kind.Synonym
import Kind.ImportMap
import Type.Type
import Type.TypeVar
import Type.Pretty

{--------------------------------------------------------------------------
  Show pretty names (rather than numbers)
--------------------------------------------------------------------------}

prettyNames :: Bool
prettyNames = True

keyword env s
  = color (colorKeyword (colors env)) (text s)

{--------------------------------------------------------------------------
  Show instance declarations
--------------------------------------------------------------------------}

instance Show Core      where show = show . prettyCore      defaultEnv Default []
instance Show External  where show = show . prettyExternal  defaultEnv
instance Show TypeDef   where show = show . prettyTypeDef   defaultEnv
instance Show DefGroup  where show = show . prettyDefGroup  defaultEnv
instance Show Def       where show = show . prettyDef       defaultEnv
instance Show Expr      where show = show . prettyExpr      defaultEnv{showKinds=True,coreShowTypes=True}
instance Show Lit       where show = show . prettyLit       defaultEnv
instance Show Branch    where show = show . prettyBranch    defaultEnv
instance Show Pattern   where show = show . snd . prettyPattern   defaultEnv

{--------------------------------------------------------------------------
  Pretty-printers proper
--------------------------------------------------------------------------}

prettyCore :: Env -> Target -> [InlineDef] -> Core -> Doc
prettyCore env0 target inlineDefs core@(Core name imports fixDefs typeDefGroups defGroups externals doc)
  = prettyComment env doc $
    keyword env "module" <+>
    (if (coreIface env) then text "interface " else empty) <.>
    prettyDefName env name <->
    (vcat $ concat $
      [ separator "import declarations"
      , map (prettyImport envX) (imports)
      , separator "external imports"
      , map (prettyExternalImport envX target) externals
      , separator "fixity declarations"
      , map (prettyFixDef envX) fixDefs
      , separator "local imported aliases"
      , map (prettyImportedSyn envX) importedSyns
      , separator "type declarations"
      , map (prettyTypeDef envX) allTypeDefs
      , separator "declarations"
      , map (prettyDef env) allDefs
      , separator "external declarations"
      , map (prettyExternal env) externals
      , separator "inline definitions"
      , if (not (coreIface env0) || null inlineDefs)
         then []
         else [text "//.inline-section"] ++
               map (prettyInlineDef env1) inlineDefs
      ]
      -- ,
      {-
      , map (prettyTypeDefGroup envX) typeDefGroups
      , map (prettyDefGroup env) defGroups
      , map (prettyExternal env) externals
      -}
    )
  where
    separator msg = if (not (coreIface env0)) then []
                     else [text " ", text "//------------------------------", text ("//#kki: " ++ msg), text " "]

    --realImports = let deps = dependencies inlineDefs core
    --              in filter (\imp -> S.member (importName imp) deps) imports

    allDefs     = flattenDefGroups defGroups
    allTypeDefs = flattenTypeDefGroups typeDefGroups

    env  = env1{ expandSynonyms = False }
    envX = env1{ showKinds = True, expandSynonyms = True }

    importedSyns = extractImportedSynonyms core
    extraImports = extractImportsFromSynonyms imports importedSyns

    env1         = env0{ importsMap =  extendImportMap extraImports (importsMap env0),
                         coreShowTypes = (coreShowTypes env0 || coreIface env0),
                         showKinds = (showKinds env0 || coreIface env0),
                         coreShowDef = not (coreIface env0) }

prettyImport env imp
  = -- prettyComment env (importModDoc imp) $
    prettyVis env (importVis imp) $
    keyword env "import"
      <+> pretty (importsAlias (importName imp) (importsMap env)) <+> text "="
      <+> prettyName env (importName imp)
      <+> text "=" <+> prettyLit env (LitString (importPackage imp))
      <.> semi


prettyExternalImport env target (ExternalImport imports _)
  = -- prettyComment env (importModDoc imp) $
    -- trace ("external imports: target: " ++ show target ++ ": " ++ show imports) $
    case lookupTarget target imports of
      Nothing -> empty
      Just keyvals0
        -> case filter (\(key,_) -> key /= "include-inline" && key /= "header-include-inline") keyvals0 of
             [] -> empty
             keyvals -> keyword env "extern import" <+> text "{" 
                          <-> tab (ppTarget env target <+> text "{" <-> tab (vcat (map prettyKeyval keyvals)) <-> text "};")
                          <-> text "};"
  where
    prettyKeyval (key,val)
      = prettyLit env (LitString key) <.> text "=" <.> prettyLit env (LitString val) <.> semi
      
prettyExternalImport env target _ = empty



prettyFixDef env (FixDef name fixity)
  = (case fixity of
       FixInfix fix assoc -> ppAssoc assoc <+> pretty fix
       FixPrefix          -> text "prefix"
       FixPostfix         -> text "postfix")
    <+> prettyDefName env name <.> semi
  where
    ppAssoc AssocLeft    = text "infixl"
    ppAssoc AssocRight   = text "infixr"
    ppAssoc AssocNone    = text "infix"

prettyImportedSyn :: Env -> SynInfo -> Doc
prettyImportedSyn env synInfo
  = ppSynInfo env True False True synInfo <.> semi

prettyExternal :: Env -> External -> Doc
prettyExternal env (External name tp pinfos body vis fip nameRng doc) | coreIface env && isHiddenExternalName name
  = empty
prettyExternal env (External name tp pinfos body vis fip nameRng doc)
  = prettyComment env doc $
    prettyVis env vis $
    keyword env (show fip ++ "extern") <+> prettyDefName env name <+> text ":" <+> prettyDefFunType env pinfos tp <+> prettyEntries body
  where
    prettyEntries [(Default,content)] = keyword env "= inline" <+> prettyLit env (LitString content) <.> semi
    prettyEntries entries             = text "{" <-> tab (vcat (map prettyEntry entries)) <-> text "};"
    prettyEntry (target,content)      = ppTarget env target <.> keyword env "inline" <+> prettyLit env (LitString content) <.> semi

prettyExternal env (ExternalImport imports range)
  = empty

  {-
    keyword env "external inline" <+> text "{" <->
    tab (vcat (map prettyInclude includes)) <->
    text "}"
  where
    prettyInclude (target,content)
        = ppTarget env target <.> prettyLit env (LitString content)
  -}


ppTarget env target
  = case target of
      Default -> empty
      CS      -> text "cs "
      C _     -> text "c "
      JS _    -> text "js "
      -- _       -> keyword env (show target) <.> space
    

prettyTypeDefGroup :: Env -> TypeDefGroup -> Doc
prettyTypeDefGroup env (TypeDefGroup defs)
  = -- (if (length defs==1) then id else (\ds -> text "rec {" <-> tab ds <-> text "}")) $
    vcat (map (prettyTypeDef env) defs)

prettyTypeDef :: Env -> TypeDef -> Doc
prettyTypeDef env (Synonym synInfo  )
  = ppSynInfo env False True True synInfo <.> semi

prettyTypeDef env (Data dataInfo isExtend)
  = -- keyword env "type" <+> prettyVis env vis <.> ppDataInfo env True dataInfo
    prettyDataInfo env True False {-public only?-} isExtend dataInfo <.> semi

prettyDefGroup :: Env -> DefGroup -> Doc
prettyDefGroup env (DefRec defs)
  = -- (\ds -> text "rec {" <-> tab ds <-> text "}") $
    text "rec {" <+> align (prettyDefs env defs) </> text "}"
prettyDefGroup env (DefNonRec def)
  = prettyDefX env False def

prettyDefs :: Env -> Defs -> Doc
prettyDefs env (defs)
  = vcat (map (prettyDefX env True) defs)

{-
prettyInlineDefGroup :: Env -> DefGroup -> Doc
prettyInlineDefGroup env (DefRec defs)
  = -- (\ds -> text "rec {" <-> tab ds <-> text "}") $
    -- text "rec {" <+> align (prettyDefs env defs) </> text "}"
    vcat (map (prettyInlineDef env True) defs)
prettyInlineDefGroup env (DefNonRec def)
  = prettyInlineDef env False def


prettyInlineDef :: Env -> Bool -> Def -> Doc
prettyInlineDef env isRec def | (not (coreIface env) || defInline def == InlineNever || (defInline def == InlineAuto && not (isInlineable (coreInlineMax env) def)))
  = empty
prettyInlineDef env isRec def@(Def name scheme expr vis sort inl nameRng doc)
  = keyword env (show sort)
    <.> (if (inl==InlineAlways) then (space <.> keyword env "inline") else empty)
    <.> (if isRec then (space <.> keyword env "rec") else empty)
    <+> (if nameIsNil name then text "_" else prettyDefName env name)
    -- <+> text ":" <+> prettyType env scheme
    <+> text ("// inline size: " ++ show (costDef def))
    <.> linebreak <.> indent 2 (text "=" <+> prettyExpr env{coreShowVis=False,coreShowDef=True} expr) <.> semi
-}

prettyInlineDef :: Env ->  InlineDef -> Doc
prettyInlineDef env (InlineDef name expr isRec inlkind cost sort specArgs)
  =     (if isRec then (keyword env "recursive ") else empty)
    <.> (if (null specArgs) then empty else (keyword env "specialize " <.> prettySpecArgs <.> text " "))
    <.> (if (cost <= 0 || inlkind == InlineAlways) then (keyword env "inline ") else empty)
    <.> prettyParamInfos sort
    <.> keyword env (defSortShowFull sort)
    <+> (if nameIsNil name then text "_" else prettyDefName env name)
    -- <+> text ":" <+> prettyType env scheme
    <+> text ("// inline size: " ++ show cost)
    <.> linebreak <.> indent 2 (text "=" <+> prettyExpr env{coreShowVis=False,coreShowDef=True} expr) <.> semi
  where
    isFun = case expr of
              TypeLam _ (Lam _ _ _) -> True
              Lam _ _ _             -> True
              _                     -> False

    prettySpecArgs 
      = dquotes (text [if spec then '*' else '_' | spec <- specArgs])

    prettyParamInfos (DefFun{defFunParamInfos=pinfos}) | Borrow `elem` pinfos
      = keyword env "borrow" <+> dquotes (text [if info == Borrow then '^' else '_' | info <- pinfos]) <.> text " "
    prettyParamInfos _
      = empty

prettyDef :: Env-> Def -> Doc
prettyDef env def = prettyDefX env True def

prettyDefX env isRec def@(Def name scheme expr vis sort inl nameRng doc)
  = prettyComment env doc $
    {- if (nameIsNil name && not isRec && coreShowDef env && not (coreShowVis env))
      then ppBody <.> semi
      else -}
           prettyVis env vis $
            keyword env (show sort)
            <+> (if nameIsNil name && coreShowDef env
                  then text "_" 
                  else prettyDefName env name) 
            <+> text ":" <+> (case sort of 
                  DefFun pinfos _ -> prettyDefFunType env pinfos scheme
                  _               -> prettyType env scheme
                )
            <.> (if (not (coreShowDef env)) -- && (sizeDef def >= coreInlineMax env)
                  then empty
                  else linebreak <.> indent 2 (text "=" <+> ppBody)) <.> semi
  where
    ppBody = prettyExpr env{coreShowVis=False} expr 

prettyVis env vis doc
  = if (not (coreShowVis env)) then doc else
    case vis of
      Public  -> keyword env "pub" <+> doc
      Private -> doc -- keyword env "private" <+> doc --if (coreIface env) then (keyword env "private" <+> doc) else doc

prettyType env tp
  = head (prettyTypes env [tp])

prettyTypes env tps
  = niceTypes env tps

prettyKind env prefix kind
  = if (kind == kindStar)
     then empty
     else text prefix <+> ppKind (colors env) precTop kind


{--------------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------------}
tab doc
  = indent 2 doc


prettyExpr :: Env -> Expr -> Doc

-- Core lambda calculus
prettyExpr env lam@(Lam tnames eff expr)
  = pparens (prec env) precArrow $
    keyword env "fn" <.>
      (if isTypeTotal eff then empty else color (colorType (colors env)) (text "<" <.> prettyType env' eff <.> text ">")) <.>
      tupled [prettyTName env' tname | tname <- tnames] <.> text "{" <-->
      tab (prettyExpr env expr <.> semi) <-->
      text "}"
  where
    env'  = env { prec = precTop }
    env'' = env { prec = precArrow }

prettyExpr env (Var tname varInfo)
  = prettyVar env tname <.> prettyInfo
  where
    prettyInfo
      = case varInfo of
          InfoArity m n  -> empty -- braces (pretty m <.> comma <.> pretty n)
          InfoExternal f -> empty -- braces (text"@")
          _ -> empty

prettyExpr env (App a args)
  = pparens (prec env) precApp $
    prettyExpr (decPrec env') a <.> tupled [prettyExpr env' a | a <- args]
  where
    env' = env { prec = precApp }

-- Type abstraction/application
prettyExpr env (TypeLam tvs expr)
  = pparens (prec env) precArrow $
    keyword env "forall" <.> angled [prettyTypeVar env' tv | tv <- tvs] <+> prettyExpr env' expr
  where
    env' = env { prec = precTop
               , nice = (if prettyNames then niceTypeExtendVars tvs else id) $ nice env
               }

prettyExpr env (TypeApp expr tps)
  = if (not (coreShowTypes env)) then prettyExpr env expr
     else pparens (prec env) precApp $
          prettyExpr (decPrec env') expr <.> angled [prettyType env'' tp | tp <- tps]
  where
    env' = env { prec = precApp }
    env'' = env { prec = precTop }

-- Literals and constants
prettyExpr env (Con tname repr)
  = -- prettyTName env tname
    prettyVar env tname

prettyExpr env (Lit lit)
  = prettyLit env lit

--  
prettyExpr env (Let ([DefNonRec (Def x tp e vis isVal inl nameRng doc)]) e')
  = vcat [ let exprDoc = prettyExpr env e <.> semi
           in {- if (nameIsNil x) then exprDoc
               else -}
              (keyword env "val" <+> hang 2 (
                (if nameIsNil x then text "_" else prettyDefName env x) <+> text ":" <+> prettyType env tp 
                <-> text "=" <+> exprDoc))
         , prettyExpr env e'
         ]
prettyExpr env (Let defGroups expr)
  = -- parens $
    vcat [ align $ vcat (map (\dg -> prettyDefGroup env dg) defGroups)
         , prettyExpr env expr
         ]


-- Case expressions
prettyExpr env (Case exprs branches)
  = pparens (prec env) precTop $
    keyword env "match" <+> tupled (map (prettyExpr env{ prec = precAtom }) exprs) <+> text "{" <-->
    tab (prettyBranches env branches) <--> text "}"

prettyVar env tname
  = prettyName env (getName tname)
    -- <.> braces (ppType env{ prec = precTop } (typeOf tname))

{--------------------------------------------------------------------------
  Case branches
--------------------------------------------------------------------------}

prettyBranches :: Env -> [Branch] -> Doc
prettyBranches env (branches)
  = vcat (map (prettyBranch env) branches)

prettyBranch :: Env -> Branch -> Doc
prettyBranch env (Branch patterns guards)
  = let (env', patDocs) = (prettyPatterns env{ prec = precApp } patterns)
    in hsep (punctuate comma patDocs)
        <.> linebreak <.> indent 2 (vcat (map (prettyGuard env') guards)) <.> semi

prettyGuard   :: Env -> Guard -> Doc
prettyGuard env (Guard test expr)
  = ( if (isExprTrue test)
       then empty
       else text " |" <+> prettyExpr env{ prec = precTop } test
    )   <+> text "->" <+> prettyExpr env{ prec = precTop } expr


prettyPatterns :: Env -> [Pattern] -> (Env,[Doc])
prettyPatterns env pats
  = foldl f (env,[]) pats
  where
    f (env,docs) pat = let (env',doc) = prettyPattern env pat
                       in (env',doc:docs)

prettyPatternType (pat,tp) (env,docs)
  = let (env',doc) = prettyPattern (decPrec env) pat
    in (env', (doc <.> (if (coreShowTypes env') then text " :" <+> prettyType env' tp else empty)) : docs)

prettyPattern :: Env -> Pattern -> (Env,Doc)
prettyPattern env pat
  = case pat of
      PatCon tname args repr targs exists resTp info skip
                        -> -- pparens (prec env) precApp $
                           -- prettyName env (getName tname)
                           let env' = env { nice = niceTypeExtendVars exists (nice env) }
                               (env'',docs) = foldr prettyPatternType (env',[]) (zip args targs)
                           in (env'',
                               parens $
                                (if skip then keyword env ".skip " else empty) <.>
                                prettyConName env tname <.>
                                 (if (null exists) then empty
                                   else angled (map (ppTypeVar env'') exists)) <.>
                                  tupled docs <+> colon <+> prettyType env'' resTp <.> space)

      PatVar tname PatWild  -> (env, parens  $
                               prettyTName env tname) -- prettyName env (getName tname))
      PatVar tname pat      -> let (env',doc) = prettyPattern (decPrec env) pat
                               in (env', parens (doc <+> keyword env "as" <+> prettyTName env (tname)))

      PatWild               -> (env,text "_")
      PatLit lit            -> (env,prettyLit env lit)
  where
    commaSep :: [Doc] -> Doc
    commaSep = hcat . punctuate comma
    prettyArg :: TName -> Doc
    prettyArg tname = parens (prettyName env (getName tname) <+> text "::" <+> prettyType env (typeOf tname))

    prettyConName env tname
      = pretty (getName tname)
        --  if (coreShowTypes env) then prettyTName env tname else pretty (getName tname)

{--------------------------------------------------------------------------
  Literals
--------------------------------------------------------------------------}

prettyLit :: Env -> Lit -> Doc
prettyLit env lit
  = case lit of
      LitInt    i -> color (colorNumber (colors env)) (text (show i))
      LitFloat  d -> color (colorNumber (colors env)) (text (show d)) -- TODO: use showHex
      LitChar   c -> color (colorString (colors env)) (text ("'" ++ showXChar c ++ "'"))
      LitString s -> color (colorString (colors env)) (text ("\"" ++ concatMap showXChar s ++ "\""))


showXChar c
  = if (c >= ' ' && c <= '~' && c /= '\"' && c /= '\'' && c /= '\\') then [c]
    else let n = fromEnum c
         in if (n < 256) then "\\x" ++ (showHex 2 n)
            else if (n < 65536) then "\\u" ++ (showHex 4 n)
                                else "\\U" ++ (showHex 6 n)

{--------------------------------------------------------------------------
  Pretty-printers for non-core terms
--------------------------------------------------------------------------}

prettyTName :: Env -> TName -> Doc
prettyTName env (TName name tp)
  = prettyName env name <.> text ":" <+> ppType env tp

prettyName :: Env -> Name -> Doc
prettyName env name
  = if (isQualified name) 
      then color (colorNameQual (colors env)) (text (nameModule name ++ "/")) <.> 
           color (colorSource (colors env)) (pretty (unqualify name)) 
      else color (colorSource (colors env)) $ pretty name
  {-
    let (nm,post) = canonicalSplit name
    in if (post=="") then pretty name else pretty nm <.> text post
    -}

prettyDefName :: Env -> Name -> Doc
prettyDefName env name
  = color (colorSource (colors env)) $
    pretty (unqualify name)
  where
    fmtName cname
      = let (name,postfix) = canonicalSplit cname
            s = show name
            pre = case s of
                   ""  -> empty
                   (c:cs) -> if (isAlphaNum c || c == '_' || c == '(' || c == '[') then text s else parens (text s)
        in (if null postfix then pre else (pre <.> text postfix))

ppOperatorName env name
  = color (colorSource (colors env)) $
    fmtName (unqualify name)
  where
    fmtName name
      = let s = show name
        in case s of
             "" -> empty
             (c:cs) -> if (isAlphaNum c) then text ("`" ++ s ++ "`") else text s


prettyTypeVar :: Env -> TypeVar -> Doc
prettyTypeVar
  = ppTypeVar


{--------------------------------------------------------------------------
  Precedence
--------------------------------------------------------------------------}

type Prec = Int

decPrec :: Env -> Env
decPrec env = env { prec = prec env - 1 }


-- Extend an import map with nice aliases for a set of new imports
extendImportMap :: [Import] -> ImportMap -> ImportMap
extendImportMap imports impMap
  = foldr extend impMap imports
  where
    extend imp impMap
      = let fullName = importName imp in
        case importsExtend fullName fullName impMap of
         Just newMap -> newMap
         Nothing     -> impMap -- already imported ?

-- extract all qualifiers in synonyms: it can be the case that a type synonym
-- refers to a type defined in a non-imported module and we need to add it to the
-- imports of the .kki file. (no need to add it to the generated code since
-- javascript does not use types, while c-sharp does not use import declarations but fully
-- qualified names all the time)
extractImportsFromSynonyms :: [Import] -> [SynInfo] -> [Import]
extractImportsFromSynonyms imps syns
  = let quals = filter (\nm -> not (S.member nm impNames)) $
                concatMap extractSyn syns
        extraImports = map (\nm -> Import nm "" Private "") quals -- TODO: import path ?
    in extraImports
  where
    impNames        = S.fromList (map importName imps)

    extractSyn syn  = qualifier (synInfoName syn) : extractType (synInfoType syn)
    extractType tp
      = case tp of
          TSyn syn args body -> qualifier (typesynName syn) : extractTypes (body:args)
          TApp con args      -> extractTypes (con:args)
          TFun args eff res  -> extractTypes (res:eff:map snd args)
          TForall _ _ body   -> extractType body
          TCon tcon          -> [qualifier (typeConName tcon)]
          TVar _             -> []
    extractTypes tps
      = concatMap extractType tps


-- extract from type signatures the synonyms so we can compress .kki files
-- by locally defining imported synonyms
extractImportedSynonyms :: Core -> [SynInfo]
extractImportedSynonyms core
  = let syns = filter (\info -> coreProgName core /= qualifier (synInfoName info)) $
               synonymsToList $ extractSynonyms (extractSignatures core)
    in -- trace ("extracted synonyms: " ++ show (map (show . synInfoName) syns)) $
       syns
  where
    extractSynonym :: Type -> Synonyms
    extractSynonym tp
      = case tp of
          TSyn syn args body  -> let syns = extractSynonyms (body:args) in
                                 case typesynInfo syn of
                                   Just info -> synonymsExtend info syns
                                   Nothing   -> syns
          TApp con args       -> extractSynonyms (con:args)
          TFun args eff res   -> extractSynonyms (res:eff:map snd args)
          TForall _ _ body    -> extractSynonym body
          _                   -> synonymsEmpty

    extractSynonyms :: [Type] -> Synonyms
    extractSynonyms xs = foldr synonymsCompose synonymsEmpty (map extractSynonym xs)




instance HasTypeVar DefGroup where
  sub `substitute` defGroup
    = case defGroup of
        DefRec defs   -> DefRec (sub `substitute` defs)
        DefNonRec def -> DefNonRec (sub `substitute` def)

  ftv defGroup
    = case defGroup of
        DefRec defs   -> ftv defs
        DefNonRec def -> ftv def

  btv defGroup
    = case defGroup of
        DefRec defs   -> btv defs
        DefNonRec def -> btv def


instance HasTypeVar Def where
  sub `substitute` (Def name scheme expr vis isVal inl nameRng doc)
    = Def name (sub `substitute` scheme) (sub `substitute` expr) vis isVal inl nameRng doc

  ftv (Def name scheme expr vis isVal inl nameRng doc)
    = ftv scheme `tvsUnion` ftv expr

  btv (Def name scheme expr vis isVal inl nameRng doc)
    = btv scheme `tvsUnion` btv expr

instance HasTypeVar Expr where
  sub `substitute` expr
    = case expr of
        Lam tnames eff expr -> Lam (sub `substitute` tnames) (sub `substitute` eff) (sub `substitute` expr)
        Var tname info    -> Var (sub `substitute` tname) info
        App f args        -> App (sub `substitute` f) (sub `substitute` args)
        TypeLam tvs expr  -> let sub' = subRemove tvs sub
                              in TypeLam tvs (sub' |-> expr)
        TypeApp expr tps   -> TypeApp (sub `substitute` expr) (sub `substitute` tps)
        Con tname repr     -> Con (sub `substitute` tname) repr
        Lit lit            -> Lit lit
        Let defGroups expr -> Let (sub `substitute` defGroups) (sub `substitute` expr)
        Case exprs branches -> Case (sub `substitute` exprs) (sub `substitute` branches)

  ftv expr
    = let tvs = case expr of
                  Lam tname eff expr -> tvsUnions [ftv tname, ftv eff, ftv expr]
                  Var tname info     -> ftv tname
                  App a b            -> ftv a `tvsUnion` ftv b
                  TypeLam tvs expr   -> tvsRemove tvs (ftv expr)
                  TypeApp expr tp    -> ftv expr `tvsUnion` ftv tp
                  Con tname repr     -> ftv tname
                  Lit lit            -> tvsEmpty
                  Let defGroups expr -> ftv defGroups `tvsUnion` ftv expr
                  Case exprs branches -> ftv exprs `tvsUnion` ftv branches
      in -- trace ("ftv :" ++ show (tvsList (tvs)) ++ ", in expr: " ++ show expr) $
         tvs

  btv expr
    = case expr of
        Lam tname eff expr -> tvsUnions [btv tname, btv eff, btv expr]
        Var tname info     -> btv tname
        App a b            -> btv a `tvsUnion` btv b
        TypeLam tvs expr   -> tvsInsertAll tvs (btv expr)
        TypeApp expr tp    -> btv expr `tvsUnion` btv tp
        Con tname repr     -> btv tname
        Lit lit            -> tvsEmpty
        Let defGroups expr -> btv defGroups `tvsUnion` btv expr
        Case exprs branches -> btv exprs `tvsUnion` btv branches


instance HasTypeVar Branch where
  sub `substitute` (Branch patterns guards)
    = let sub' = subRemove (tvsList (btv patterns)) sub
      in Branch (map ((sub `substitute`)) patterns) (map (sub' `substitute`) guards)

  ftv (Branch patterns guards)
    = ftv patterns `tvsUnion` (tvsDiff (ftv guards) (btv patterns))

  btv (Branch patterns guards)
    = btv patterns `tvsUnion` btv guards


instance HasTypeVar Guard where
  sub `substitute` (Guard test expr)
    = Guard (sub `substitute` test) (sub `substitute` expr)
  ftv (Guard test expr)
    = let tvs = ftv test `tvsUnion` ftv expr
      in -- trace ("ftv:" ++ show (tvsList (tvs)) ++ ", in guard: " ++ show expr) $
         tvs
  btv (Guard test expr)
    = btv test `tvsUnion` btv expr

instance HasTypeVar Pattern where
  sub `substitute` pat
    = case pat of
        PatVar tname pat   -> PatVar (sub `substitute` tname) (sub `substitute` pat)
        PatCon tname args repr tps exists restp info skip
          -> let sub' = subRemove exists sub
             in PatCon (sub `substitute` tname) (sub' `substitute` args) repr (sub' `substitute` tps) exists (sub' `substitute` restp) info skip
        PatWild           -> PatWild
        PatLit lit        -> pat


  ftv pat
    = let tvs = case pat of
                  PatVar tname pat    -> tvsUnion (ftv tname) (ftv pat)
                  PatCon tname args _ targs exists tres _ _ -> tvsRemove exists (tvsUnions [ftv tname,ftv args,ftv targs,ftv tres])
                  PatWild             -> tvsEmpty
                  PatLit lit          -> tvsEmpty
      in -- trace ("ftv :" ++ show (tvsList (tvs)) ++ ", in pattern: " ++ show pat) $
         tvs

  btv pat
    = case pat of
        PatVar tname pat           -> tvsUnion (btv tname) (btv pat)
        PatCon tname args _ targs exists tres _ _  -> tvsUnions [btv tname,btv args,btv targs,btv tres,tvsNew exists]
        PatWild                 -> tvsEmpty
        PatLit lit              -> tvsEmpty


instance HasTypeVar TName where
  sub `substitute` (TName name tp)
    = TName name (sub `substitute` tp)
  ftv (TName name tp)
    = ftv tp
  btv (TName name tp)
    = btv tp

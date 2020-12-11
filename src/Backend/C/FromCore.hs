-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Backend.C.FromCore ( cFromCore ) where

import Numeric( showHFloat )
import Platform.Config(version)
import Lib.Trace
import Control.Applicative hiding (empty)
import Control.Monad
import Data.List ( intersperse, partition )
import Data.Char
-- import Data.Maybe
-- import Data.Monoid ( mappend )
import qualified Data.Set as S

import Common.File( normalizeWith, startsWith, endsWith  )
import Kind.Kind
import Kind.Newtypes
import Type.Type
import Type.TypeVar
import Type.Kind( getKind )
-- import Type.Assumption( getArity )
import qualified Type.Pretty as Pretty

import Lib.PPrint
-- import qualified Lib.PPrint
import Common.Name
-- import Common.Range
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty
import Core.CoreVar

import Backend.C.Parc
import Backend.C.ParcReuse
import Backend.C.ParcReuseSpec
import Backend.C.Box

type CommentDoc   = Doc
type ConditionDoc = Doc

type ModuleName = Name

debug :: Bool
debug  = False

externalNames :: [(TName, Doc)]
externalNames
  = [ (conName exprTrue,  text "true")
    , (conName exprFalse, text "false")
    -- , (TName nameOptionalNone typeOptional, text "undefined")  -- ugly but has real performance benefit
    ]

--------------------------------------------------------------------------
-- Generate C code from System-F core language
--------------------------------------------------------------------------

cFromCore :: FilePath -> Pretty.Env -> Platform -> Newtypes -> Int -> Bool -> Bool -> Bool -> Maybe (Name,Bool) -> Core -> (Doc,Doc,Core)
cFromCore sourceDir penv0 platform newtypes uniq enableReuse enableSpecialize enableReuseSpecialize mbMain core
  = case runAsm uniq (Env moduleName moduleName False penv externalNames newtypes platform False)
           (genModule sourceDir penv platform newtypes enableReuse enableSpecialize enableReuseSpecialize mbMain core) of
      (bcore,cdoc,hdoc) -> (cdoc,hdoc,bcore)
  where
    moduleName = coreProgName core
    penv       = penv0{ Pretty.context = moduleName, Pretty.fullNames = False }

contextDoc :: Doc
contextDoc = text "_ctx"

contextParam :: Doc
contextParam = text "kk_context_t* _ctx"

genModule :: FilePath -> Pretty.Env -> Platform -> Newtypes -> Bool -> Bool -> Bool -> Maybe (Name,Bool) -> Core -> Asm Core
genModule sourceDir penv platform newtypes enableReuse enableSpecialize enableReuseSpecialize mbMain core0
  =  do core <- liftUnique (do bcore <- boxCore core0            -- box/unbox transform
                               ucore <- if (enableReuse)
                                         then parcReuseCore penv platform newtypes bcore -- constructor reuse analysis
                                         else return bcore
                               pcore <- parcCore penv platform newtypes enableSpecialize ucore -- precise automatic reference counting
                               score <- if (enableReuse && enableReuseSpecialize)
                                         then parcReuseSpecialize penv pcore -- selective reuse
                                         else return pcore
                               return score
                           )

        let headComment   = text "// Koka generated module:" <+> string (showName (coreProgName core)) <.> text ", koka version:" <+> string version
            initSignature = text "void" <+> ppName (qualify (coreProgName core) (newName ".init")) <.> parameters []

        emitToInit $ vcat $ [text "static bool _initialized = false;"
                            ,text "if (_initialized) return;"
                            ,text "_initialized = true;"]
                            ++ map initImport (coreProgImports core)

        emitToC $ vcat $ [headComment
                         ,text "#include" <+> dquotes (text (moduleNameToPath (coreProgName core)) <.> text ".h")]
                         ++ externalImports
                         ++ externalIncludesC

        emitToH $ vcat $ [ text "#pragma once"
                         , text "#ifndef " <.> modName <.> text "_H"
                         , text "#define " <.> modName <.> text "_H"
                         , headComment
                         , text "#include <kklib.h>" ]
                         ++ map moduleImport (coreProgImports core)
                         ++ externalIncludesH

        emitToH (linebreak <.> text "// type declarations")
        genTypeDefs (coreProgTypeDefs core)
        emitToH (linebreak <.> text "// value declarations")
        genTopGroups (coreProgDefs core)

        genMain (coreProgName core) mbMain

        init <- getInit
        emitToC $ linebreak
                  <.> text "// initialization"
                  <-> initSignature
                  <.> block init
        emitToH $ vcat [ linebreak <.> initSignature <.> semi <.> linebreak
                       , text "#endif // header"]
        return core -- box/unboxed core
  where
    modName         = ppModName (coreProgName core0)

    externalIncludesC :: [Doc]
    externalIncludesC
      = concatMap includeExternalC (coreProgExternals core0)

    externalIncludesH :: [Doc]
    externalIncludesH
      = concatMap includeExternalH (coreProgExternals core0)


    externalImports :: [Doc]
    externalImports
      = map fst (concatMap (importExternal sourceDir) (coreProgExternals core0))

    initImport :: Import -> Doc
    initImport imp
      = ppName (qualify (importName imp) (newName ".init")) <.> arguments [] <.> semi



moduleImport :: Import -> Doc
moduleImport imp
  = text "#include" <+>
    (if null (importPackage imp)
      then dquotes (text (moduleNameToPath  (importName imp)) <.> text ".h")
      else brackets (text (importPackage imp) <.> text "/" <.> text (moduleNameToPath  (importName imp))) <.> text ".h")

includeExternalC :: External -> [Doc]
includeExternalC (ExternalInclude includes range)
  = let content = case lookup C includes of
                    Just s -> s
                    Nothing -> case lookup Default includes of
                                 Just s -> s
                                 Nothing -> ""
    in [text (dropWhile isSpace content)] -- [align $ vcat $! map text (lines content)]
includeExternalC _  = []

includeExternalH :: External -> [Doc]
includeExternalH (ExternalInclude includes range)
  = let content = case lookup CHeader includes of
                    Just s -> s
                    Nothing -> ""
    in [text (dropWhile isSpace content)] -- [align $ vcat $! map text (lines content)]
includeExternalH _  = []


importExternal :: FilePath -> External -> [(Doc,Doc)]
importExternal sourceDir (ExternalImport imports range)
  = let xs = case lookup C imports of
                    Just s -> [s]
                    Nothing -> case lookup Default imports of
                                 Just s -> [s]
                                 Nothing -> [] -- failure ("C backend does not support external import at " ++ show range)
    in [(text "#include" <+>
           (if (head s == '<')
             then text s
             else dquotes (if (null sourceDir) then text s
                            else text (normalizeWith '/' sourceDir ++ "/" ++ s)))
           , pretty nm)
       | (nm,s) <- xs, not (null s)]
importExternal _ _
  = []

genMain :: Name -> Maybe (Name,Bool) -> Asm ()
genMain progName Nothing = return ()
genMain progName (Just (name,_))
  = emitToC $
    text "\n// main entry\nint main(int argc, char** argv)" <+> block (vcat [
      text "kk_context_t* _ctx = kk_main_start(argc, argv);"
    , ppName (qualify progName (newName ".init")) <.> parens (text "_ctx") <.> semi
    , ppName name <.> parens (text "_ctx") <.> semi
    , text "kk_main_end(_ctx);"
    , text "return 0;"
    ]);

---------------------------------------------------------------------------------
-- Generate C statements for value definitions
---------------------------------------------------------------------------------

genLocalGroups :: [DefGroup] -> Asm [Doc]
genLocalGroups dgs
  = mapM genLocalGroup dgs

genLocalGroup :: DefGroup -> Asm Doc
genLocalGroup (DefRec _) = error "Backend.C.FromCore.genLocalGroup: local resursive function definitions are not allowed"
genLocalGroup (DefNonRec def)
  = genLocalDef def


genLocalDef :: Def -> Asm Doc
genLocalDef def@(Def name tp expr vis sort inl rng comm)
  = do penv <- getPrettyEnv
       let resDoc = typeComment (Pretty.ppType penv tp)
       defDoc <- genStat (ResultAssign (defTName def) Nothing) expr
       let fdoc = vcat ([ if null comm
                           then empty
                           else align (vcat (space : map text (lines (trimComment comm)))) {- already a valid C comment -}]
                        ++
                        if (not (nameIsNil name) && dstartsWith defDoc (show (ppName name) ++ " ="))
                          then --single assignment without declarations
                               [ ppType tp <+> defDoc <.> semi ]
                          else [ if (nameIsNil name) then empty else ppVarDecl (defTName def) <.> unitSemi tp
                               , if (isDiscardExpr expr) then empty else defDoc]
                       )
       return (fdoc)
  where
    isDiscardExpr (Con con _)              = (getName con == nameUnit)
    isDiscardExpr (App (Var name _) [])    = (getName name == nameReuseNull)
    isDiscardExpr _                        = False

-- remove final newlines and whitespace and line continuations (\\)
trimComment comm
  = unlines (map trimLine (lines comm))
  where
    trimLine s = case reverse s of
                   '\\':xs -> trimRest xs
                   xs      -> trimRest xs
    trimRest xs = reverse (dropWhile (`elem` " \n\r\t") xs)


---------------------------------------------------------------------------------
-- Generate C declaration for top level definitions
---------------------------------------------------------------------------------

genTopGroups :: [DefGroup] -> Asm ()
genTopGroups groups
  = localUnique $
    mapM_ genTopGroup groups

genTopGroup :: DefGroup -> Asm ()
genTopGroup group
  = do case group of
        DefRec defs   -> do mapM_ genFunTopDefSig defs
                            mapM_ (genTopDef False False) defs
        DefNonRec def -> do let inlineC =  (defInline def == InlineAlways || isInlineable 5 def)
                            genTopDef True inlineC def

genFunTopDefSig :: Def -> Asm ()
genFunTopDefSig def@(Def name tp defExpr vis sort inl rng comm)
  = do penv <- getPrettyEnv
       let tpDoc = typeComment (Pretty.ppType penv tp)
           sig   = (genFunDefSig False def)
       -- (if (isPublic vis) then emitToH else emitToC)
       emitToH (linebreak <.> sig <.> semi <+> tpDoc)

genFunDefSig :: Bool -> Def -> Doc
genFunDefSig inlineC def@(Def name tp defExpr vis sort inl rng comm)
  = genFunSig inlineC vis name defExpr

genFunSig :: Bool -> Visibility -> Name -> Expr -> Doc
genFunSig inlineC vis name defExpr
  = let tryFun expr = case expr of
                        TypeApp e _   -> tryFun e
                        TypeLam _ e   -> tryFun e
                        Lam params eff body  -> genLamSig inlineC vis name params body
                        _             -> error ("Backend.C.FromCore.genFunDefSig: not a function: " ++ show (name,defExpr))
    in tryFun defExpr

genLamSig :: Bool -> Visibility -> Name -> [TName] -> Expr -> Doc
genLamSig inlineC vis name params body
  = (if (inlineC) then text "static inline "
       -- else if (not (isPublic vis)) then text "static "
       else empty) <.>
    ppType (typeOf body) <+> ppName name <.> tparameters params


genTopDef :: Bool -> Bool -> Def -> Asm ()
genTopDef genSig inlineC def@(Def name tp expr vis sort inl rng comm)
  = do when (not (null comm)) $
         (if inlineC then emitToH else emitToC) (align (vcat (space : map text (lines (trimComment comm))))) {- already a valid C comment -}
       genTopDefDecl genSig inlineC def

genTopDefDecl :: Bool -> Bool -> Def -> Asm ()
genTopDefDecl genSig inlineC def@(Def name tp defBody vis sort inl rng comm)
  = let tryFun expr = case expr of
                        TypeApp e _   -> tryFun e
                        TypeLam _ e   -> tryFun e
                        Lam params eff body | isDefFun sort -> genFunDef params body
                        _ | isDefFun sort
                          -> -- some optimization turned a toplevel lambda into
                             -- a value; wrap it back into a lambda again as all occurrences
                             -- will have InfoArity and call it directly (so it cannot be a function_t)
                             case splitFunScheme tp of
                               Nothing -> failure ("Backend.C.FromCore.getTopDefDecl: function def has not a function type: " ++ show (name,tp))
                               Just (_,_,argTps,_,resTp)
                                 -> do names <- mapM newVarName ["x" ++ show i | i <- [1..length argTps]]
                                       let tnames = [TName name tp | (name,(_,tp)) <- zip names argTps]
                                           app    = App expr [Var tname InfoNone | tname <- tnames]
                                       genFunDef tnames app
                        -- special case string literals
                        Lit (LitString s)
                          -> do let (cstr,clen) = cstring s
                                    decl = if (isPublic vis) then empty else text "static"
                                if (clen > 0)
                                 then emitToC (text "kk_define_string_literal" <.> tupled [decl,ppName name,pretty clen,cstr] {- <.> semi -})
                                 else emitToC (text "kk_define_string_literal_empty" <.> tupled [decl, ppName name])
                                when (isPublic vis) $
                                 emitToH (linebreak <.> text "extern" <+> ppType typeString <+> ppName name <.> semi)
                        -- special case for doubles
                        Lit lit@(LitFloat f)
                          -> do let flt  = ppLit lit
                                emitToH (text "#define" <+> ppName name <+> parens (text "(double)" <.> parens flt))
                        _ -> do doc <- genStat (ResultAssign (TName name tp) Nothing) (defBody)
                                emitToInit (block doc)  -- must be scoped to avoid name clashes
                                let decl = ppType tp <+> ppName name <.> unitSemi tp
                                -- if (isPublic vis) -- then do
                                -- always public since inlined definitions can refer to it (sin16 in std/num/ddouble)
                                emitToH (linebreak <.> text "extern" <+> decl)
                                emitToC (linebreak <.> decl)
                                -- else do emitToC (linebreak <.> text "static" <+> decl)
    in withDef name inlineC (tryFun defBody)
  where
    emit = if inlineC then emitToH else emitToC

    resTp = case splitFunScheme tp of
                    Nothing -> tp
                    Just (_,_,argTps,_,resTp0) -> resTp0

    genFunDef :: [TName] -> Expr -> Asm ()
    genFunDef params body
      = do let args = map ( ppName . getName ) params
               isTailCall = body `isTailCalling` name
           bodyDoc <- (if isTailCall then withStatement else id)
                      (genStat (ResultReturn (Just (TName name resTp)) params) body)
           penv <- getPrettyEnv
           let tpDoc = typeComment (Pretty.ppType penv tp)
           let sig = genLamSig inlineC vis name params body
           when (genSig && not inlineC {-&& isPublic (defVis def)-}) $ emitToH (linebreak <.> sig <.> semi <+> tpDoc)
           top <- getTop -- get top level decls generated by body (for functions etc)
           emit $ linebreak
                  <.> top
                  <.> sig
                  <+> ( if isTailCall
                          then tcoBlock tpDoc bodyDoc
                          else debugComment ("genFunDef: no tail calls to " ++ showName name ++ " found")
                            <.> tblock tpDoc bodyDoc
                      )

unitSemi :: Type -> Doc
unitSemi tp  = if (isTypeUnit tp) then text " = kk_Unit;" else semi

---------------------------------------------------------------------------------
-- Generate value constructors for each defined type
---------------------------------------------------------------------------------

genTypeDefs :: TypeDefGroups -> Asm ()
genTypeDefs groups
  = mapM_ (genTypeDefGroup) groups


genTypeDefGroup :: TypeDefGroup -> Asm ()
genTypeDefGroup (TypeDefGroup tds)
  = mapM_ (genTypeDef) tds

genTypeDef :: TypeDef -> Asm ()
genTypeDef (Synonym synInfo)
  = return ()
genTypeDef (Data info isExtend)
  = do -- generate the type constructor
       emitToH $ linebreak <.> text ("// " ++ if (dataInfoIsValue info) then "value type" else "type") <+> pretty (dataInfoName info)
       let (dataRepr,conReprs) = getDataRepr info
           noCons = null conReprs
           name   = (dataInfoName info)

       -- trace ("type " ++ show name ++ ": " ++ show dataRepr ++ ": " ++ show conReprs) $ return ()
       -- if (isExtend) then return ()
       -- generate the type
       if (dataRepr == DataEnum)
        then let enumIntTp = case (dataInfoDef info) of
                               DataDefValue 1 0 -> "uint8_t"
                               DataDefValue 2 0 -> "uint16_t"
                               _                -> "uint32_t"
             in  emitToH $ ppVis (dataInfoVis info) <.> text "enum" <+> ppName (typeClassName (dataInfoName info)) <.> text "_e" <+>
                           block (if (null conReprs)
                                   then ppName (dataInfoName info) <.> text "_empty"
                                   else vcat (punctuate comma (map ppEnumCon (zip (dataInfoConstrs info) conReprs)))) <.> semi <->
                           text "typedef" <+> text enumIntTp <+> ppName (typeClassName (dataInfoName info)) <.> semi <.> linebreak
        else if (dataReprIsValue dataRepr || isExtend)
          then return ()
              --  else emitToH $ text "struct" <+> ppName name <.> text "_s" <+> text "{" <+> text "datatype_tag_t _tag;" <+> text "};"
          else emitToH $ ppVis (dataInfoVis info) <.> text "struct" <+> ppName (typeClassName name) <.> text "_s" <+>
                         block (vcat ([text "kk_block_t _block;"] ++
                                      (if (dataRepr /= DataOpen) then [] else [text "kk_string_t _tag;"])
                               )) <.> semi
                         <->
                         (if dataReprMayHaveSingletons dataRepr
                           then (text "typedef kk_datatype_t" <+> ppName (typeClassName name) <.> semi)
                           else (text "typedef struct" <+> ppName (typeClassName name) <.> text "_s*" <+> ppName (typeClassName name) <.> semi))

       -- order fields of constructors to have their scan fields first
       let conInfoReprs = zip (dataInfoConstrs info) conReprs
       conInfos <- mapM (\(conInfo,conRepr) -> do -- should never fail as mixed raw/scan is checked in kindInfer
                                                  newtypes <- getNewtypes
                                                  platform <- getPlatform
                                                  let (fields,size,scanCount) = orderConFieldsEx platform newtypes (dataRepr == DataOpen) (conInfoParams conInfo)
                                                  return (conInfo,conRepr,fields,scanCount)) conInfoReprs
       let maxScanCount = maxScanCountOf conInfos
           minScanCount = minScanCountOf conInfos

       -- generate types for constructors
       if (dataRepr == DataEnum)
        then return ()
        else mapM_ (genConstructorType info dataRepr) conInfos

       -- wrap up the type definition
       if (dataRepr == DataOpen && not isExtend)
        then {- do let openTag = text "tag_t" <+> openTagName name
                emitToH $ text "extern" <+> openTag <.> semi
                emitToC $ openTag <+> text "= 0;" -}
             return ()
        else if (dataRepr == DataEnum || not (dataReprIsValue dataRepr))
          then return ()
          else emitToH $ if (isDataStructLike dataRepr)
                  then ppVis (dataInfoVis info) <.> text "struct" <+> ppName name <.> text "_s"
                       <+> block (text "kk_value_tag_t _tag;" <-> text "union"
                                  <+> block (vcat (
                                         map ppStructConField (dataInfoConstrs info)
                                         ++ (if (maxScanCount > 0 && minScanCount /= maxScanCount)
                                              then [text "kk_box_t _fields[" <.> pretty maxScanCount <.> text "];"]
                                              else [])
                                      )) <+> text "_cons;") <.> semi
                       <-> ppVis (dataInfoVis info) <.> text "typedef struct" <+> ppName name <.> text "_s" <+> ppName (typeClassName name) <.> semi
                  else ppVis (dataInfoVis info) <.> text "typedef struct"
                       <+> (case (dataRepr,dataInfoConstrs info) of
                              (DataIso,[con])          -> ppName ((conInfoName con))
                              (DataSingleStruct,[con]) -> ppName ((conInfoName con))
                              _                        -> ppName name <.> text "_s")
                       <+> ppName (typeClassName name) <.> semi

       -- generate functions for constructors
       mapM_ (genConstructor info dataRepr maxScanCount) conInfos
       mapM_ (genConstructorTest info dataRepr) conInfos
       genDupDrop (typeClassName name) info dataRepr conInfos

       -- generate functions for the datatype (box/unbox)
       genBoxUnbox name info dataRepr
  where
    ppEnumCon (con,conRepr)
      = ppName (conInfoName con)  -- <+> text "= datatype_enum(" <.> pretty (conTag conRepr) <.> text ")"

    ppStructConField con
      = text "struct" <+> ppName ((conInfoName con)) <+> ppName (unqualify (conInfoName con)) <.> semi

maxScanCountOf :: [(ConInfo,ConRepr,[(Name,Type)],Int)] -> Int
maxScanCountOf conInfos
  = foldr (\(_,_,_,sc) n -> max sc n) 0 conInfos

minScanCountOf :: [(ConInfo,ConRepr,[(Name,Type)],Int)] -> Int
minScanCountOf [] = 0
minScanCountOf conInfos
  = foldr (\(_,_,_,sc) n -> min sc n) (maxScanCountOf conInfos) conInfos

genConstructorType :: DataInfo -> DataRepr -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Asm ()
genConstructorType info dataRepr (con,conRepr,conFields,scanCount) =
  case conRepr of
    ConEnum{}
       -> return () -- already in enum declaration
    ConSingleton{}  | not (dataReprIsValue dataRepr)
       -> return () -- represented as an enum
    -- _ | null conFields && (dataRepr < DataNormal && not (isDataStructLike dataRepr))
    --   -> return ()
    _  -> do emitToH $ ppVis (conInfoVis con) <.> text "struct" <+> ppName ((conInfoName con)) <+>
                       block (let fields = (typeField ++ map ppConField conFields)
                              in if (null fields) then text "kk_box_t _unused;"  -- avoid empty struct
                                                  else vcat fields) <.> semi
  where
    typeField  = if (dataReprIsValue dataRepr) then []
                 else [text "struct" <+> ppName (typeClassName (dataInfoName info)) <.> text "_s" <+> text "_base;"]

ppConField :: (Name,Type) -> Doc
ppConField (name,tp)
  = ppType tp <+> ppName (unqualify name) <.> semi

genConstructor :: DataInfo -> DataRepr -> Int -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Asm ()
genConstructor info dataRepr maxScanCount (con,conRepr,conFields,scanCount)
  = do genConstructorBaseCast info dataRepr con conRepr
       genConstructorCreate info dataRepr con conRepr conFields scanCount maxScanCount
       genConstructorAccess info dataRepr con conRepr

genConstructorTest :: DataInfo -> DataRepr -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Asm ()
genConstructorTest info dataRepr (con,conRepr,conFields,scanCount)
  = do genConstructorTestX info dataRepr con conRepr

genConstructorTestX :: DataInfo -> DataRepr -> ConInfo -> ConRepr -> Asm ()
genConstructorTestX info dataRepr con conRepr
  = do emitToH  $ text "static inline bool" <+> (conTestName con) <.> tupled [ppName (typeClassName (dataInfoName info)) <+> text "x"]
                  <+> block( text "return (" <.> (
                  let nameDoc = ppName (conInfoName con)
                      -- tagDoc  = text "datatype_enum(" <.> pretty (conTag conRepr) <.> text ")"
                      dataTypeTagDoc = text "kk_datatype_tag" <.> tupled [text "x"]
                      valueTagEq     = text "kk_integer_small_eq(x._tag," <+> ppConTag con conRepr dataRepr <.> text ")"
                  in case conRepr of
                    ConEnum{}      -> text "x ==" <+> ppConTag con conRepr dataRepr
                    ConIso{}       -> text "true"
                    ConSingleton{} | dataRepr == DataAsList -> text "kk_datatype_is_singleton(x)" -- text "x ==" <+> conSingletonName con
                                   | dataReprIsValue dataRepr -> valueTagEq
                                   | otherwise -> text "kk_datatype_has_tag" <.> tupled [text "x", ppConTag con conRepr dataRepr]
                    ConSingle{}    -> text "true"
                    ConAsJust{}    -> valueTagEq
                    ConStruct{}    -> valueTagEq
                    ConAsCons{}    -> text "kk_datatype_is_ptr(x)" --  <+> conSingletonNameX (conAsNil conRepr)
                    ConNormal{}
                                   -- | dataRepr == DataSingleNormal -> text "datatype_is_ptr(x)"
                                   -- | otherwise -> text "datatype_is_ptr(x) && datatype_tag_fast(x) ==" <+> ppConTag con conRepr dataRepr
                                   -- -> text "datatype_tag(x) ==" <+> ppConTag con conRepr dataRepr
                                   -> text (if (dataReprMayHaveSingletons dataRepr)
                                             then "kk_datatype_has_tag" else "kk_basetype_has_tag")
                                      <.> tupled [text "x", ppConTag con conRepr dataRepr]
                    ConOpen{}      -> text "kk_string_ptr_eq_borrow" <.> tupled [text "x->_tag",ppConTag con conRepr dataRepr]
                  ) <.> text ");")

conTestName con
  = conTestNameX (conInfoName con)
conTestNameX name
  = ppName (makeHiddenName "is" name)

conTagName con
  = ppName (makeHiddenName "tag" (conInfoName con))

ppConTag con conRepr dataRepr
  = case conRepr of
      ConOpen{} ->  ppName (makeHiddenName "tag" (conInfoName con))
      ConEnum{} ->  ppName (conInfoName con)
      -- ConSingleton{}  | dataRepr == DataAsList -> text "datatype_from_enum(" <.> pretty (conTag conRepr) <.> text ")" -- ppName ((conInfoName con))
      _         | isDataStructLike dataRepr -> text "kk_value_tag(" <.> pretty (conTag conRepr) <.> text ")"
      _         ->  text "(kk_tag_t)" <.> parens (pretty (conTag conRepr))


genConstructorCreate :: DataInfo -> DataRepr -> ConInfo -> ConRepr -> [(Name,Type)] -> Int -> Int -> Asm ()
genConstructorCreate info dataRepr con conRepr conFields scanCount maxScanCount
  = do {-
       if (null conFields && not (dataReprIsValue dataRepr))
         then do let structTp = text "struct" <+> ppName (typeClassName (dataInfoName info)) <.> text "_s"
                     declTpName = structTp <+> conSingletonName con
                     open = if (dataRepr == DataOpen) then "open_" else ""
                 emitToH $ text "extern" <+> ppName (typeClassName (dataInfoName info)) <+> conSingletonName con <.> semi
                 emitToC $ text ("define_static_" ++ open ++ "datatype(,")
                             <+> structTp <.> text ","
                             <+> conSingletonName con <.> text ","
                             <+> ppConTag con conRepr dataRepr <.> text ");"
                 when (dataRepr == DataOpen) $
                   emitToInit $ text "_static_" <.> conSingletonName con <.> text "._tag =" <+> ppConTag con conRepr dataRepr <.> semi -- assign open tag
         else return ()
       -}
       when (dataRepr == DataOpen) $ emitToH $ text "extern kk_string_t" <+> conTagName con <.> semi
       let at = newHiddenName "at"
       emitToH $
          text "static inline" <+> ppName (typeClassName (dataInfoName info)) <+> conCreateNameInfo con
          <.> ntparameters ((if (dataReprIsValue dataRepr || (null conFields)) then [] else [(at,typeReuse)])
                             ++ conInfoParams con)
          <.> block (
            let nameDoc = ppName (conInfoName con)
                -- tagDoc  = text "datatype_enum(" <.> pretty (conTag conRepr) <.> text ")"
            in case conRepr of
              ConEnum{}      -> text "return" <+> ppConTag con conRepr dataRepr <.> semi
              ConSingleton{} | not (dataReprIsValue dataRepr)-> text "return kk_datatype_from_tag" <.> parens (ppConTag con conRepr dataRepr) <.> semi
              ConIso{}
                -> let tmp = text "_con"
                   in vcat [ppName (typeClassName (dataInfoName info)) <+> tmp <+> text "= {" <+> ppDefName (fst (head conFields)) <+> text "};"  -- struct init
                           ,text "return" <+> tmp <.> semi]
              _ -> let tmp = text "_con"
                       assignField f (name,tp) = f (ppDefName name) <+> text "=" <+> ppDefName name <.> semi
                   in if (dataReprIsValue dataRepr)
                    then vcat(--[ppName (typeClassName (dataInfoName info)) <+> tmp <.> semi]
                               (if (hasTagField dataRepr)
                                 then [ ppName (typeClassName (dataInfoName info)) <+> tmp <.> semi
                                      , tmp <.> text "._tag =" <+> ppConTag con conRepr dataRepr  <.> semi]
                                      ++ map (assignField (\fld -> tmp <.> text "._cons." <.> ppDefName (conInfoName con) <.> text "." <.> fld)) conFields
                                      ++ [tmp <.> text "._cons._fields[" <.> pretty i <.> text "] = kk_box_null;"
                                          | i <- [scanCount..(maxScanCount-1)]]
                                 else [ ppName (typeClassName (dataInfoName info)) <+> tmp <.> semi {- <+> text "= {0}; // zero initializes all fields" -} ]
                                      ++ map (assignField (\fld -> tmp <.> text "." <.> fld)) conFields
                               )
                               ++ [text "return" <+> tmp <.> semi])
                    else {- if (null conFields)
                     then text "return dup_datatype_as" <.> tupled [ppName (typeClassName (dataInfoName info)),  (conSingletonName con) {-, ppConTag con conRepr dataRepr <+> text "/* _tag */"-}] <.> semi
                     else -}
                          vcat([text "struct" <+> nameDoc <.> text "*" <+> tmp <+> text "="
                               <+> text "kk_block_alloc_at_as"
                                       <.> arguments [ text "struct" <+> nameDoc,
                                                       (if (null conFields {- open singleton -}) then text "kk_reuse_null" else ppName at),
                                                       pretty scanCount <+> text "/* scan count */",
                                                       if (dataRepr /= DataOpen)
                                                        then ppConTag con conRepr dataRepr
                                                        else text "KK_TAG_OPEN"]
                               <.> semi]
                              ++ (if (dataRepr /= DataOpen) then [] else [tmp <.> text "->_base._tag = kk_string_dup" <.> parens(ppConTag con conRepr dataRepr) <.> semi ])
                              ++ map (assignField (\fld -> tmp <.> text "->" <.> fld)) conFields
                              ++ {- [let base = text "&" <.> tmp <.> text "->_base"
                                    in if (dataReprMayHaveSingletons dataRepr)
                                        then text "return kk_datatype_from_base" <.> parens base <.> semi
                                        else text "return" <+> base <.> semi])
                                 -}
                                 [text "return" <+> conBaseCastNameInfo con <.> parens tmp <.> semi])
          )

genConstructorBaseCast :: DataInfo -> DataRepr -> ConInfo -> ConRepr -> Asm ()
genConstructorBaseCast info dataRepr con conRepr
  = case conRepr of
      ConEnum{}      -> return ()
      ConSingleton{} -> return ()
      ConIso{}       -> return ()
      _ | dataReprIsValue dataRepr -> return ()
      _ -> emitToH $
            text "static inline" <+> ppName (typeClassName (dataInfoName info)) <+> conBaseCastNameInfo con
            <.> tupled [text "struct" <+> ppName (conInfoName con) <.> text "* _x"]
            <.> block (
                  let base = text "&_x->_base"
                  in if (dataReprMayHaveSingletons dataRepr)
                      then text "return kk_datatype_from_base" <.> parens base <.> semi
                      else text "return" <+> base <.> semi
                )


genConstructorAccess :: DataInfo -> DataRepr -> ConInfo -> ConRepr -> Asm ()
genConstructorAccess info dataRepr con conRepr
  = if (dataReprIsValue dataRepr || isConSingleton conRepr)
     then return ()
     else gen
  where
    gen = emitToH $ text "static inline struct" <+> ppName (conInfoName con) <.> text "*" <+> conAsName con
                    <.> tupled [ppName (typeClassName (dataInfoName info)) <+> text "x"]
                    <+> block( vcat $
                          [-- text "assert(" <.> conTestName con <.> tupled [text "x"] <.> text ");",
                           text "return" <+>
                           text (if dataReprMayHaveSingletons dataRepr then "kk_datatype_as_assert" else "kk_basetype_as_assert") <.>
                           tupled [text "struct"  <+> ppName (conInfoName con) <.> text "*", text "x",
                               (if (dataRepr == DataOpen) then text "KK_TAG_OPEN" else ppConTag con conRepr dataRepr <+> text "/* _tag */")] <.> semi]
                        )


genBoxUnbox :: Name -> DataInfo -> DataRepr -> Asm ()
genBoxUnbox name info dataRepr
  = do let tname = typeClassName name
       genBox tname info dataRepr
       genUnbox  tname info dataRepr


genBoxCall prim asBorrowed tp arg
  = case cType tp of
      CFun _ _   -> primName_t prim "function_t" <.> parens arg
      CPrim val  | val == "kk_unit_t" || val == "kk_integer_t" || val == "bool" || val == "kk_string_t"
                 -> primName_t prim val <.> parens arg  -- no context
      --CPrim val  | val == "int32_t" || val == "double" || val == "unit_t"
      --           -> text val <.> arguments [arg]
      CData name -> primName prim (ppName name) <.> tupled [arg,ctx]
      _          -> primName_t prim (show (ppType tp)) <.> tupled [arg,ctx]
  where
    ctx          = if asBorrowed then text "NULL" else contextDoc


primName_t prim s = primName prim $ text $
                     (if (s `startsWith` "kk_") then "" else "kk_") ++
                     (if (s `endsWith` "_t") then reverse (drop 2 (reverse s)) else s)
primName prim d   = d <.> text "_" <.> text prim


genBox name info dataRepr
  = emitToH $
    text "static inline kk_box_t " <.> ppName name <.> text "_box" <.> parameters [ppName name <+> text "_x"] <+> block (
      case dataRepr of
        DataEnum -> text "return" <+> text "kk_enum_box" <.> tupled [text "_x"] <.> semi
        DataIso  -> let conInfo = head (dataInfoConstrs info)
                        (isoName,isoTp)   = (head (conInfoParams conInfo))
                    in text "return" <+> genBoxCall "box" False isoTp (text "_x." <.> ppName (unqualify isoName)) <.> semi
        _ -> case dataInfoDef info of
               DataDefValue raw scancount
                  -> let -- extra = if (isDataStructLike dataRepr) then 1 else 0  -- adjust scan count for added "tag_t" members in structs with multiple constructors
                         docScanCount = if (hasTagField dataRepr)
                                         then ppName name <.> text "_scan_count" <.> parens (text "_x")
                                         else pretty scancount <+> text "/* scan count */"
                     in vcat [ text "kk_box_t _box;"
                             , text "kk_valuetype_box" <.> arguments [ppName name, text "_box", text "_x",
                                                                      docScanCount
                                                                     ] <.> semi
                             , text "return _box;" ]
               _  -> text "return" <+> text (if dataReprMayHaveSingletons dataRepr then "kk_datatype_box" else "kk_basetype_box") <.> tupled [text "_x"] <.> semi
    )

genUnbox name info dataRepr
  = emitToH $
    text "static inline" <+> ppName name <+> ppName name <.> text "_unbox" <.> parameters [text "kk_box_t _x"] <+> block (
      (case dataRepr of
        DataEnum -> text "return" <+> parens (ppName name) <.> text "kk_enum_unbox" <.> tupled [text "_x"]
        DataIso  -> let conInfo = head (dataInfoConstrs info)
                        isoTp   = snd (head (conInfoParams conInfo))
                    in text "return" <+> conCreateNameInfo conInfo <.> arguments [genBoxCall "unbox" False isoTp (text "_x")]
        _ | dataReprIsValue dataRepr
          -> vcat [ text "kk_boxed_value_t _p;"
                  , ppName name <+> text "_unbox;"
                  , text "kk_valuetype_unbox_" <.> arguments [ppName name, text "_p", text "_unbox", text "_x"] <.> semi  -- borrowing
                  , text "if (_ctx!=NULL && _p!=NULL)" <+> block (
                      text "if (kk_basetype_is_unique(_p)) { kk_basetype_free(_p); } else" <+> block (
                        vcat [ppName name <.> text "_dup(_unbox);"
                             ,text "kk_basetype_decref" <.> arguments [text "_p"] <.> semi]
                      )
                    )
                  , text "return _unbox" ]
             -- text "unbox_valuetype" <.> arguments [ppName name, text "x"]
        _ -> text "return"
               <+> (if dataReprMayHaveSingletons dataRepr
                     then text "kk_datatype_unbox(_x)"
                     else text "kk_basetype_unbox_as" <.> tupled [ppName name, text "_x"])
    ) <.> semi)


genDupDrop :: Name -> DataInfo -> DataRepr -> [(ConInfo,ConRepr,[(Name,Type)],Int)] -> Asm ()
genDupDrop name info dataRepr conInfos
  = do genScanFields name info dataRepr conInfos
       genDupDropX True name info dataRepr conInfos
       genDupDropX False name info dataRepr conInfos
       if (dataReprIsValue dataRepr)
        then return ()
        else do  genIsUnique name info dataRepr
                 genFree name info dataRepr          -- free the block
                 genDecRef name info dataRepr        -- decrement the ref count (if > 0)
                 genDropReuseFun name info dataRepr     -- drop, but if refcount==0 return the address of the block instead of freeing
                 genDropNFun name info dataRepr
                 genReuse name info dataRepr         -- return the address of the block
                 genHole name info dataRepr


genIsUnique :: Name -> DataInfo -> DataRepr -> Asm ()
genIsUnique name info dataRepr
  = emitToH $
    text "static inline bool" <+> ppName name <.> text "_is_unique" <.> tupled [ppName name <+> text "_x"] <+> block (
      text "return" <+>
      (if (dataReprMayHaveSingletons dataRepr)
        then text "kk_datatype_is_unique(_x)"
        else text "kk_basetype_is_unique(_x)"
      ) <.> semi)

genFree :: Name -> DataInfo -> DataRepr -> Asm ()
genFree name info dataRepr
  = emitToH $
    text "static inline void" <+> ppName name <.> text "_free" <.> tupled [ppName name <+> text "_x"] <+> block (
      (if (dataReprMayHaveSingletons dataRepr)
        then text "kk_datatype_free(_x)"
        else text "kk_basetype_free(_x)"
      ) <.> semi)

genDecRef :: Name -> DataInfo -> DataRepr -> Asm ()
genDecRef name info dataRepr
  = emitToH $
    text "static inline void" <+> ppName name <.> text "_decref" <.> parameters [ppName name <+> text "_x"] <+> block (
      (if (dataReprMayHaveSingletons dataRepr)
        then text "kk_datatype_decref"
        else text "kk_basetype_decref"
      ) <.> arguments [text "_x"] <.> semi)


genDropReuseFun :: Name -> DataInfo -> DataRepr -> Asm ()
genDropReuseFun name info dataRepr
  = emitToH $
    text "static inline kk_reuse_t" <+> ppName name <.> text "_dropn_reuse" <.> parameters [ppName name <+> text "_x", text "size_t _scan_fsize"] <+> block (
      text "return" <+>
      (if (dataReprMayHaveSingletons dataRepr)
        then text "kk_datatype_dropn_reuse"
        else text "kk_basetype_dropn_reuse"
      ) <.> arguments [text "_x", text "_scan_fsize"] <.> semi)


genDropNFun :: Name -> DataInfo -> DataRepr -> Asm ()
genDropNFun name info dataRepr
  = emitToH $
    text "static inline void" <+> ppName name <.> text "_dropn" <.> parameters [ppName name <+> text "_x", text "size_t _scan_fsize"] <+> block (
      (if (dataReprMayHaveSingletons dataRepr)
        then text "kk_datatype_dropn"
        else text "kk_basetype_dropn"
      ) <.> arguments [text "_x", text "_scan_fsize"] <.> semi)

genReuse :: Name -> DataInfo -> DataRepr -> Asm ()
genReuse name info dataRepr
  = emitToH $
    text "static inline kk_reuse_t" <+> ppName name <.> text "_reuse" <.> tupled [ppName name <+> text "_x"] <+> block (
      text "return" <+>
      (if (dataReprMayHaveSingletons dataRepr)
        then text "kk_datatype_reuse(_x)"
        else text "kk_basetype_reuse(_x)"
      ) <.> semi)

genHole :: Name -> DataInfo -> DataRepr -> Asm ()
genHole name info dataRepr
  = emitToH $
    text "static inline" <+> ppName name <+> ppName name <.> text "_hole()" <+> block (
      text "return" <+>
      -- holes must be trace-able and look like values (least-significant-bit==1)
      (if (dataReprMayHaveSingletons dataRepr)
        then text "kk_datatype_from_tag((kk_tag_t)0)"
        else parens (ppName name) <.> text "(1)"
      ) <.> semi)


genScanFields :: Name -> DataInfo -> DataRepr -> [(ConInfo,ConRepr,[(Name,Type)],Int)] -> Asm ()
genScanFields name info dataRepr conInfos | not (hasTagField dataRepr)
 = return ()
genScanFields name info dataRepr conInfos
 = emitToH $
    text "static inline size_t" <+> ppName name <.> text "_scan_count" <.> tupled [ppName name <+> text "_x"]
    <+> block (vcat (map (genScanFieldTests (length conInfos)) (zip conInfos [1..])))

genScanFieldTests :: Int -> ((ConInfo,ConRepr,[(Name,Type)],Int),Int) -> Doc
genScanFieldTests lastIdx ((con,conRepr,conFields,scanCount),idx)
  = if (lastIdx == idx)
      then (text "else" <+> stat)
      else (text (if (idx==1) then "if" else "else if") <+> parens (conTestName con <.> tupled [text "_x"]))
            <+> stat
  where
    stat = text ("return " ++ show (1 {-tag-} + scanCount) ++ ";")

genDupDropX :: Bool -> Name -> DataInfo -> DataRepr -> [(ConInfo,ConRepr,[(Name,Type)],Int)] -> Asm ()
genDupDropX isDup name info dataRepr conInfos
  = emitToH $
     text "static inline"
     <+> (if isDup then ppName name <+> ppName name <.> text "_dup" else text "void" <+> ppName name <.> text "_drop")
     <.> (if isDup then tupled else parameters) [ppName name <+> text "_x"]
     <+> block (vcat (dupDropTests))
  where
    ret = (if isDup then [text "return _x;"] else [])
    dupDropTests
      | dataRepr == DataEnum   = ret
      | dataRepr == DataIso    = [genDupDropIso isDup (head conInfos)] ++ ret
      | dataRepr <= DataStruct = map (genDupDropTests isDup dataRepr (length conInfos)) (zip conInfos [1..]) ++ ret
      | otherwise = if (isDup) then [text "return"
                                      <+> (if dataReprMayHaveSingletons dataRepr
                                            then text "kk_datatype_dup(_x)"
                                            else text "kk_basetype_dup_as" <.> tupled [ppName name, text "_x"])
                                       <.> semi]
                               else [text (if dataReprMayHaveSingletons dataRepr then "kk_datatype_drop" else "kk_basetype_drop")
                                       <.> arguments [text "_x"] <.> semi]

genDupDropIso :: Bool -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Doc
genDupDropIso isDup (con,conRepr,[(name,tp)],scanCount)
  = hcat $ map (<.>semi) (genDupDropCall isDup tp (text "_x." <.> ppName name))
genDupDropIso _ _
  = failure $ "Backend.C.genDupDropIso: ivalid arguments"

genDupDropTests :: Bool -> DataRepr -> Int -> ((ConInfo,ConRepr,[(Name,Type)],Int),Int) -> Doc
genDupDropTests isDup dataRepr lastIdx ((con,conRepr,conFields,scanCount),idx)
  = let stats = genDupDropFields isDup dataRepr con conFields
    in if (lastIdx == idx)
        then (if null stats
               then empty
              else if (lastIdx == 1)
               then vcat stats
               else text "else" <+> block (vcat stats))
        else (text (if (idx==1) then "if" else "else if") <+> parens (conTestName con <.> tupled [text "_x"]))
             <+> (if null stats then text "{ }" else block (vcat stats))

genDupDropFields :: Bool -> DataRepr -> ConInfo -> [(Name,Type)] -> [Doc]
genDupDropFields isDup dataRepr con conFields
  = map (\doc -> doc <.> semi) $ concat $
    [genDupDropCall isDup tp
      ((if (hasTagField dataRepr) then text "_x._cons." <.> ppDefName (conInfoName con) else text "_x")
       <.> dot <.> ppName name) | (name,tp) <- conFields]


genDupDropCallX prim tp args
  = case cType tp of
      CFun _ _   -> [(primName_t prim "function_t") <.> args]
      CBox       -> [(primName_t prim "box_t") <.> args]
      CPrim val   | val == "kk_integer_t" || val == "kk_string_t" || val == "kk_vector_t" || val == "kk_evv_t" || val == "kk_ref_t" || val == "kk_reuse_t" || val == "kk_box_t"
                  -> [(primName_t prim val) <.> args]
                  | otherwise
                  -> -- trace ("** skip dup/drop call: " ++ pre val ++ ": " ++ show args) $
                     []-- text "value" <.> args
      CData name -> [primName prim (ppName name) <.> args]

genDupCall tp arg  = hcat $ genDupDropCall True tp arg
genDropCall tp arg = hcat $ genDupDropCall False tp arg

genDupDropCall :: Bool -> Type -> Doc -> [Doc]
genDupDropCall isDup tp arg = if (isDup) then genDupDropCallX "dup" tp (parens arg)
                                         else genDupDropCallX "drop" tp (arguments [arg])

genIsUniqueCall :: Type -> Doc -> [Doc]
genIsUniqueCall tp arg  = case genDupDropCallX "is_unique" tp (parens arg) of
                            -- [call] -> [text "kk_likely" <.> parens call]
                            cs     -> cs

genFreeCall :: Type -> Doc -> [Doc]
genFreeCall tp arg  = genDupDropCallX "free" tp (parens arg)

genDecRefCall :: Type -> Doc -> [Doc]
genDecRefCall tp arg  = genDupDropCallX "decref" tp (arguments [arg])

genDropReuseCall :: Type -> [Doc] -> [Doc]
genDropReuseCall tp args  = genDupDropCallX "dropn_reuse" tp (arguments args)

genReuseCall :: Type -> Doc -> [Doc]
genReuseCall tp arg  = genDupDropCallX "reuse" tp (parens arg)

genDropNCall :: Type -> [Doc] -> [Doc]
genDropNCall tp args  = genDupDropCallX "dropn" tp (arguments args)

conBaseCastNameInfo :: ConInfo -> Doc
conBaseCastNameInfo con = conBaseCastName (conInfoName con)

conBaseCastName :: Name -> Doc
conBaseCastName conName = ppName (makeHiddenName "base" conName)

conCreateNameInfo :: ConInfo -> Doc
conCreateNameInfo con = conCreateName (conInfoName con)

conCreateName :: Name -> Doc
conCreateName conName  = ppName (makeHiddenName "new" conName)

conSingletonName :: ConInfo -> Doc
conSingletonName con = conSingletonNameX (conInfoName con)
conSingletonNameX conName = ppName (makeHiddenName "singleton" conName)

conAsName :: ConInfo -> Doc
conAsName con   = conAsNameX (conInfoName con)

conAsNameX cname = ppName (makeHiddenName "as" cname)

openTagName :: Name -> Doc
openTagName name = ppName (makeHiddenName "tag" name)

ntparameters :: [(Name,Type)] -> Doc
ntparameters pars
  = parameters (map param pars)
  where
    param (name,tp) = ppType tp <+> ppName (unqualify name)

parameters :: [Doc] -> Doc
parameters pars
  = tupled (pars ++ [contextParam])


arguments :: [Doc] -> Doc
arguments args
  = tupled (args ++ [contextDoc])


ppVis :: Visibility -> Doc
ppVis _       = empty
-- ppVis Public  = text "decl_public "
-- ppVis Private = text "decl_private "

-- | Returns the type constructor class name, for "List" it would be ".List"
typeConClassName :: Name -> Name
typeConClassName name
  = name -- postpend "." (prepend "." name)

typeClassName :: Name -> Name
typeClassName name
  = (prepend "." name)  -- prepend . to create separate namespace

ppDefName :: Name -> Doc
ppDefName name
  = ppName (unqualify name)

vcatBreak []  = empty
vcatBreak xs  = linebreak <.> vcat xs

-- explicit tag field?
hasTagField :: DataRepr -> Bool
hasTagField DataAsMaybe = True
hasTagField DataStruct  = True
hasTagField rep         = False

dataReprMayHaveSingletons :: DataRepr -> Bool
dataReprMayHaveSingletons dataRepr
  = case dataRepr of
      DataAsList        -> True
      DataSingleNormal  -> True
      (DataSingle hasSingletons) -> hasSingletons
      (DataNormal hasSingletons) -> hasSingletons
      -- DataOpen          -> True
      _                 -> False



genLambda :: [TName] -> Effect -> Expr -> Asm Doc
genLambda params eff body
  = do funName <- newDefVarName "fun"
       toH     <- getDefToHeader
       let newName   = prepend "new-" funName
           funTpName = postpend "_t" funName
           structDoc = text "struct" <+> ppName funTpName
           freeVars  = [(nm,tp) | (TName nm tp) <- tnamesList (freeLocals (Lam params eff body))]
       newtypes <- getNewtypes
       platform <- getPlatform
       let (fields,_,scanCount) = orderConFieldsEx platform newtypes False freeVars
           fieldDocs = [ppType tp <+> ppName name | (name,tp) <- fields]
           tpDecl  = text "struct" <+> ppName funTpName <+> block (
                       vcat ([text "struct kk_function_s _base;"] ++
                             [ppType tp <+> ppName name <.> semi | (name,tp) <- fields])
                     ) <.> semi

           funSig  = text (if toH then "extern" else "static") <+> ppType (typeOf body)
                     <+> ppName funName <.> parameters ([text "kk_function_t _fself"] ++
                                                        [ppType tp <+> ppName name | (TName name tp) <- params])

           newDef  = funSig <.> semi
                     <-> text (if toH then "static inline" else "static")
                     <+> text "kk_function_t" <+> ppName newName <.> ntparameters fields <+> block ( vcat (
                       (if (null fields)
                         then [text "kk_define_static_function" <.> arguments [text "_fself", ppName funName] -- <.> semi
                               --text "static" <+> structDoc <+> text "_self ="
                              --  <+> braces (braces (text "static_header(1, TAG_FUNCTION), box_cptr(&" <.> ppName funName <.> text ")")) <.> semi
                              ,text "return kk_function_dup(_fself);"]
                         else [structDoc <.> text "* _self = kk_function_alloc_as" <.> arguments [structDoc, pretty (scanCount + 1) -- +1 for the _base.fun
                                                                                              ] <.> semi
                              ,text "_self->_base.fun = kk_cfun_ptr_box(&" <.> ppName funName <.> text ", kk_context());"]
                              ++ [text "_self->" <.> ppName name <+> text "=" <+> ppName name <.> semi | (name,_) <- fields]
                              ++ [text "return &_self->_base;"])
                     ))


       emitToCurrentDef (vcat [linebreak,text "// lift anonymous function", tpDecl, newDef] <.> linebreak)

       bodyDoc <- genStat (ResultReturn Nothing params) body
       let funDef = funSig <+> block (
                      (if (null fields) then text "KK_UNUSED(_fself);"
                        else let dups = braces (hcat [genDupCall tp (ppName name) <.> semi | (name,tp) <- fields])
                             in vcat ([structDoc <.> text "* _self = kk_function_as" <.> tupled [structDoc <.> text "*",text "_fself"] <.> semi]
                                   ++ [ppType tp <+> ppName name <+> text "= _self->" <.> ppName name <.> semi <+> text "/*" <+> pretty tp <+> text "*/"  | (name,tp) <- fields]
                                   ++ [text "kk_drop_match" <.> arguments [text "_self",dups,text "{}"]]
                                   ))
                      <-> bodyDoc
                    )
       emitToC funDef  -- TODO: make  static if for a Private definition

       let funNew = ppName newName <.> arguments [ppName name | (name,_) <- fields]
       return funNew

---------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------


ppType :: Type -> Doc
--ppType (TApp (TCon c) [t])  | typeConName c == nameTpCTail
--  = ppType t <.> text "*"
ppType tp
  = case cType tp of
      CBox -> text "kk_box_t"
      CFun _ _ -> text "kk_function_t"
      CData name -> ppName name
      CPrim prim -> text prim

data CType
  = CBox
  | CFun [CType] CType
  | CData Name
  | CPrim String
  deriving (Eq,Show)

cType :: Type -> CType
cType tp
  = case tp of
      TForall vars preds t
        -> cType t
      TFun pars eff res
        -> CFun (map (cType . snd) pars) (cType res)
      TApp t ts
        -> cType t
      TCon c
        -> cTypeCon c
      TVar v
        -> CBox
      TSyn syn args t
        -> cType t

cTypeCon c
   = let name = typeConName c
     in if (name == nameTpInt)
         then CPrim "kk_integer_t"
        else if (name == nameTpString)
         then CPrim "kk_string_t"
        else if (name == nameTpVector)
         then CPrim "kk_vector_t"
        else if (name ==  nameTpEvv)
         then CPrim "kk_evv_t"
        else if (name == nameTpChar)
         then CPrim "kk_char_t"  -- 32-bit unicode point
        else if (name == nameTpInt32)
         then CPrim "int32_t"
        else if (name == nameTpSizeT)
         then CPrim "size_t"
        else if (name == nameTpFloat)
         then CPrim "double"
        else if (name == nameTpBool)
         then CPrim "bool"
        else if (name == nameTpUnit)
         then CPrim "kk_unit_t"
        else if (name == nameTpInt64)
         then CPrim "int64_t"
        else if (name == nameTpInt16)
         then CPrim "int16_t"
        else if (name == nameTpInt8)
         then CPrim "int8_t"
        else if (name == nameTpByte)
         then CPrim "uint8_t"
        else if (name == nameTpFloat32)
         then CPrim "float"
        else if (name == nameTpRef || name == nameTpLocalVar)
         then CPrim "kk_ref_t"
        else if (name == nameTpBox)
         then CPrim "kk_box_t"
        else if (name == nameTpReuse)
         then CPrim "kk_reuse_t"
        else if (name == nameTpCField)
         then CPrim "kk_box_t*"
        else CData (typeClassName name)



---------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------

-- | Applies a return context
getResult :: Result -> Doc -> Doc
getResult result doc
  = if isEmptyDoc doc
      then text ""
      else getResultX result doc

getResultX result (retDoc)
  = case result of
     ResultReturn (Just n) _  | (isTypeUnit (typeOf n))
                              -> retDoc <.> text "; return kk_Unit;"
     ResultReturn _ _  -> text "return" <+> retDoc <.> semi
     ResultAssign n ml -> ( if --isWildcard (getName n) ||
                               nameNil == (getName n) || isTypeUnit (typeOf n)
                              then retDoc <.> semi
                              else ppName (getName n) <+> text "=" <+> retDoc <.> semi <+> text "/*" <.> pretty (typeOf n) <.> text "*/"
                          ) <-> case ml of
                                  Nothing -> empty
                                  Just l  -> text "goto" <+> ppName l <.> semi

ppVarDecl (TName name tp) = ppType tp <+> ppName name

tryTailCall :: Result -> Expr -> Asm (Maybe Doc)
tryTailCall result expr
  = case expr of
     -- Tailcall case 1
     App (Var n info) args  | ( case result of
                                  ResultReturn (Just m) _ -> m == n && infoArity info == (length args)
                                  _                       -> False
                              )
       -> do let (ResultReturn _ params) = result
             stmts <- genOverride params args
             return $ Just $ tailblock $ stmts <-> tailcall

     -- Tailcall case 2
     App (TypeApp (Var n info) _) args | ( case result of
                                            ResultReturn (Just m) _ -> m == n && infoArity info == (length args)
                                            _                       -> False
                                          )
       -> do let (ResultReturn _ params) = result
             stmts <- genOverride params args
             return $ Just $ tailblock $ stmts <-> tailcall

     _ -> return Nothing
  where
    tailblock doc = hang 2 (text "{ // tailcall" <-> doc) <-> text "}"
    -- overriding function arguments carefully
    genOverride :: [TName] -> [Expr] -> Asm Doc
    genOverride params args
      = fmap (debugWrap "genOverride") $
        do (stmts, varNames) <- do -- args' <- mapM tailCallArg args
                                   let args' = args
                                   bs    <- mapM genVarBinding args'
                                   return (unzip bs)
           docs1             <- mapM genDefName params
           docs2             <- mapM genDefName varNames
           let assigns    = map (\(p,a)-> if p == a
                                            then debugComment ("genOverride: skipped overriding `" ++ (show p) ++ "` with itself")
                                            else debugComment ("genOverride: preparing tailcall") <.> p <+> text "=" <+> a <.> semi
                                ) (zip docs1 docs2)
           return $ vcat (stmts ++ assigns)


-- | Generates a statement from an expression by applying a return context (deeply) inside
genStat :: Result -> Expr -> Asm Doc
genStat result expr
  = fmap (debugWrap "genStat") $
    {-
    case extractExternal expr of
      Just (tn,fs,es)
        -> do (statDoc, exprDoc) <- genExternalExpr tn fs es
              return (statDoc <-> getResult result exprDoc)
      Nothing
        -> -}
           do mdoc <- tryTailCall result expr
              case mdoc of
                Just doc
                  -> return doc
                Nothing
                  -> genExprStat result expr


genExprStat :: Result -> Expr -> Asm Doc
genExprStat result expr
  = case expr of
      -- If expression is inlineable, inline it
      _  | isInlineableExpr expr
        -> do exprDoc <- genInline expr
              return (getResult result exprDoc)

      Case exprs branches
         -> do (docs, scrutinees)
                   <- fmap unzip $
                      mapM (\e-> if isInlineableExpr e && isTypeBool (typeOf e)
                                   then do d       <- genInline e
                                           return (text "", d)
                                   else do (sd,vn) <- genVarBinding e
                                           vd      <- genDefName vn
                                           return (sd, vd)
                      ) exprs
               doc <- genMatch result scrutinees branches
               return (vcat docs <-> doc)

      Let groups body
        -> case (reverse groups, body) of
             (DefNonRec (Def name tp expr Private DefVal _ _ _):rgroups, (Case [Var vname _] branches))
               | name == getName vname && not (S.member vname (freeLocals branches)) && isInlineableExpr expr
               -> genExprStat result (makeLet (reverse rgroups) (Case [expr] branches))
             _ -> do docs1 <- genLocalGroups groups
                     doc2  <- genStat result body
                     return (vcat docs1 <-> doc2)

      -- Handling all other cases
      _ -> do (statDocs,exprDoc) <- genExpr expr
              return (vcat statDocs <-> getResult result exprDoc)

---------------------------------------------------------------------------------
-- Match
---------------------------------------------------------------------------------

-- | Generates a statement for a match expression regarding a given return context
genMatch :: Result -> [Doc] -> [Branch] -> Asm Doc
genMatch result0 exprDocs branches
  = do -- mbTagDocs <- mapM genTag (zip exprDocs (transpose (map branchPatterns branches)))
       (result,genLabel)
          <- case result0 of
               ResultAssign name Nothing | length branches > 1 && not (isSingleTestBranch)
                 -> do label <- newVarName "match"
                       return (ResultAssign name (Just label),[ppName label <.> colon <+> semi])
               _ -> return (result0,[])
       docsInit <- mapM (genBranch result exprDocs True) (init branches)
       docLast0  <- genBranch result0 exprDocs False (last branches)
       let docLast = if (isSingleTestBranch && not (isJumpResult result))
                      then (text "else" <+> block docLast0) else docLast0
           doc = vcat (docsInit ++ [docLast] ++ genLabel)
       return doc
  where
    isJumpResult res
      = case res of
          ResultReturn _ _ -> True
          ResultAssign _ (Just _) -> True
          _ -> False

    isSingleTestBranch
      = case branches of
          -- [_,Branch [pat] _] | testIsSkipped pat
          --  -> True
          [Branch [pat] [Guard test expr],_]
            -> isExprTrue test && isSingleTestPat pat
          _ -> False

    testIsSkipped pat
      = case pat of
          PatWild    -> True
          PatVar _ p -> testIsSkipped p
          PatCon{patConSkip = skip} -> skip
          _ -> False

    isSingleTestPat pat
      = case pat of
          PatWild    -> True
          PatLit _   -> True
          PatVar _ p -> isSingleTestPat p
          PatCon{patConPatterns = ps} -> all isZeroTestPat ps

    isZeroTestPat pat
      = case pat of
          PatWild    -> True
          PatVar _ p -> isZeroTestPat p
          _          -> False

genBranch :: Result -> [Doc] -> Bool -> Branch -> Asm Doc
genBranch result exprDocs doTest branch@(Branch patterns guards)
  = genPattern doTest (freeLocals guards)  (zip exprDocs patterns) (genGuards result guards)

genGuards :: Result -> [Guard] -> Asm Doc
genGuards result guards
  = do docs <- mapM (genGuard result) guards
       return (vcat docs)

genGuard :: Result -> Guard-> Asm Doc
genGuard result (Guard guard expr)
  = case guard of
      Con tname repr | getName tname == nameTrue
        -> genStat result expr
      _ -> do (gddoc,gdoc) <- genExpr guard
              sdoc <- genStat result expr
              return (vcat gddoc <-> text "if" <+> parensIf gdoc <+> block (sdoc))

parensIf :: Doc -> Doc -- avoid parens if already parenthesized
parensIf d
  = if (dstartsWith d "(" && dendsWith d ")") then d else parens d


genPattern :: Bool -> TNames -> [(Doc,Pattern)] -> Asm Doc -> Asm Doc
genPattern doTest gfree [] genBody
  = genBody
genPattern doTest gfree dpatterns genBody
  = do (testss,localss,nextPatternss) <- fmap (unzip3 . concat) $
                                           mapM (genPatternTest doTest gfree) dpatterns
       let tests  = concat testss
           locals = concat localss
           nextPatterns = concat nextPatternss

       ndoc <- genPattern doTest gfree nextPatterns genBody
       if (null tests)
        then return (vcat (locals ++ [ndoc]))
        else return (text "if" <+> parensIf (hcat (punctuate (text " && ") tests))
                      <+> block (vcat (locals ++ [ndoc])))

genPatternTest :: Bool -> TNames -> (Doc,Pattern) -> Asm [([Doc],[Doc],[(Doc,Pattern)])]
genPatternTest doTest gfree (exprDoc,pattern)
  = let test xs = if doTest then xs else [] in
    case pattern of
      PatWild -> return []
      {-
      PatVar tname pattern | hiddenNameStartsWith (getName tname) "unbox"
        -> do let after = ppType (typeOf tname) <+> ppDefName (getName tname) <+> text "="
                              <+> genBoxCall "unbox" True (typeOf tname) exprDoc <.> semi
                  next  = genNextPatterns (\self fld -> self) (ppDefName (getName tname)) (typeOf tname) [pattern]
              return [([],[after],next)]
      -}
      {-
      PatVar tname(PatCon bname [pattern] repr [targ] exists tres info skip)  | getName bname == nameBoxCon
        -> do let tp    = targ
                  after = ppType tp <+> ppDefName (getName tname) <+> text "="
                          <+> genBoxCall "unbox" True tp exprDoc <.> semi
                  next  = genNextPatterns (\self fld -> self) (ppDefName (getName tname)) tp [pattern]
              return [([],[after],next)]
      -}
      PatCon bname [pattern] repr [targ] exists tres info skip  | getName bname == nameBoxCon
        -> do local <- newVarName "unbox"
              let unbox   = genBoxCall "unbox" True targ exprDoc
                  next    = genNextPatterns (\self fld -> self) {-(ppDefName local)-} unbox targ [pattern]
                  -- assign  = ppType targ <+> ppDefName local <+> text "=" <+> unbox <.> semi
              return [([],[{-assign-}],next)]
      PatVar tname pattern
        -> do let after = if (patternVarFree pattern && not (tnamesMember tname gfree)) then []
                           else [ppType (typeOf tname) <+> ppDefName (getName tname) <+> text "=" <+> exprDoc <.> semi]
                  next  = genNextPatterns (\self fld -> self) (ppDefName (getName tname)) (typeOf tname) [pattern]
              return [([],after,next)]
      PatLit (LitString s)
        -> return [(test [text "kk_string_cmp_cstr_borrow" <.> tupled [exprDoc,fst (cstring s)] <+> text "== 0"],[],[])]
      PatLit lit@(LitInt _)
        -> return [(test [text "kk_integer_eq" <.> arguments [exprDoc,ppLit lit]],[],[])]
      PatLit lit
        -> return [(test [exprDoc <+> text "==" <+> ppLit lit],[],[])]
      PatCon tname patterns repr targs exists tres info skip
        -> -- trace ("patCon: " ++ show info ++ ","  ++ show tname ++ ", " ++ show repr) $
           case repr of
                 ConEnum{}  | conInfoName info == nameTrue
                    -> return [(xtest [exprDoc],[],[])]
                 ConEnum{} | conInfoName info == nameFalse
                    -> return [(xtest [text "!" <.> parens exprDoc],[],[])]
                 _  -> let dataRepr = conDataRepr repr
                       in if (dataReprIsValue dataRepr || isConSingleton repr)
                           then valTest tname info dataRepr
                           else conTest info
        where
          xtest xs = if skip then [] else test xs

          valTest :: TName -> ConInfo -> DataRepr -> Asm [([Doc],[Doc],[(Doc,Pattern)])]
          valTest conName conInfo dataRepr
            = --do let next = genNextPatterns (exprDoc) (typeOf tname) patterns
              --   return [(test [conTestName conInfo <.> parens exprDoc],[assign],next)]
              do let selectOp = if (isDataStructLike dataRepr)
                                 then "._cons." ++ show (ppDefName (getName conName)) ++ "."
                                 else "."
                     next = genNextPatterns (\self fld -> self <.> text selectOp <.> fld) exprDoc (typeOf tname) patterns
                 return [(xtest [conTestName conInfo <.> tupled [exprDoc]],[],next)]

          conTest conInfo
            = do local <- newVarName "con"
                 let next    = genNextPatterns (\self fld -> self <.> text "->" <.> fld) (ppDefName local) (typeOf tname) patterns
                     typeDoc = text "struct" <+> ppName (conInfoName conInfo) <.> text "*"
                     assign  = typeDoc <+> ppDefName local <+> text "=" <+> conAsName conInfo <.> tupled [exprDoc] <.> semi
                 return [(xtest [conTestName conInfo <.> parens exprDoc],[assign],next)]

patternVarFree  pat
  = case pat of
      PatWild              -> True
      PatLit (LitFloat _)  -> True
      PatLit (LitChar _)   -> True
      _ -> False

genNextPatterns :: (Doc -> Doc -> Doc) -> Doc -> Type -> [Pattern] -> [(Doc,Pattern)]
genNextPatterns select exprDoc tp []
  = []
genNextPatterns select exprDoc tp patterns
  = let (vars,preds,rho) = splitPredType tp
    in case expandSyn rho of
         TFun args eff res
          -> case patterns of
               [PatWild]  | length args > 1 -> []
               [pat]      | length args == 0 || length args > 1 -> [(exprDoc, pat)]
               _          -> assertion ("C.FromCore.genNextPatterns: args != patterns " ++ show (length args, length patterns) ++ show (args,patterns) ++ ":\n expr: " ++ show exprDoc ++ "\n type: " ++ show tp) (length args == length patterns) $
                             concatMap genNextPattern
                                          (zip [if nameIsNil name then newFieldName i else name  | (name,i) <- zip (map fst args) [1..]]
                                           patterns)
         _ -> case patterns of
                [PatWild] -> []
                [pat]     -> [(exprDoc,pat)]
                _         -> failure "C.FromCore.genNextPatterns: patterns but not a function"
  where
    genNextPattern (name,pattern)
      = case pattern of
          PatWild -> []
          _       -> let patDoc = select exprDoc (ppDefName name)
                     in [(patDoc, pattern)]



---------------------------------------------------------------------------------
-- Expressions that produce statements on their way
---------------------------------------------------------------------------------

-- | Generates javascript statements and a javascript expression from core expression
genExpr :: Expr -> Asm ([Doc],Doc)
genExpr expr  | isInlineableExpr expr
  = do doc <- genInline expr
       return ([],doc)
genExpr expr
  = genExprPrim expr

genExprPrim expr
  = -- trace ("genExpr: " ++ show expr) $
    case expr of
     TypeApp e _ -> genExpr e
     TypeLam _ e -> genExpr e

     App f args
       -> genApp f args

     Let groups body
       -> do decls1       <- genLocalGroups groups
             (decls2,doc) <- genExpr body
             return (decls1 ++ decls2, doc)

     Case _ _
       -> do (doc, tname) <- genVarBinding expr
             nameDoc <- genDefName tname
             return ([doc], nameDoc)

     Lit (LitString s)
       -> do name <- newVarName "s"
             if (s=="")
              then return ([],text "kk_string_empty()")
              else do let (cstr,clen) = cstring s
                      return ([text "kk_define_string_literal" <.> tupled [empty,ppName name,pretty clen,cstr]]
                             ,text "kk_string_dup" <.> parens (ppName name));

     Var vname (InfoExternal formats)
       -> case splitFunScheme (typeOf vname) of
            Just(_,_,tpars,teff,tres)
              -> do names <- newVarNames (length tpars)
                    let tnames = [TName name tp | (name,(_,tp)) <- zip names tpars]
                    genExpr $ Lam tnames teff (App expr [Var tname InfoNone | tname <- tnames])
            _ -> failure ("Backend.C.FromCore.genExpr: invalid partially applied external:\n" ++ show expr)
     _ -> failure ("Backend.C.FromCore.genExpr: invalid expression:\n" ++ show expr)


genExprs :: [Expr] -> Asm ([Doc],[Doc])
genExprs exprs
  = do xs <- mapM genExpr exprs
       let (declss,docs) = unzip xs
       return (concat declss, docs)

genInlineableExprs :: [Expr] -> Asm ([Doc],[Doc])
genInlineableExprs exprs
  = do xs <- mapM genInlineableExpr exprs
       let (declss,docs) = unzip xs
       return (concat declss, docs)

genInlineableExpr :: Expr -> Asm ([Doc],Doc)
genInlineableExpr expr  | isInlineableExpr expr
  = do doc <- genInline expr
       return ([],doc)
genInlineableExpr expr
  = do (doc,var) <- genVarBinding expr
       return ([doc],ppName (getName var))


-- | Introduces an additional let binding in core if necessary
--   The expression in the result is guaranteed to be a Var afterwards
genVarBinding :: Expr -> Asm (Doc, TName)
genVarBinding expr
  = case expr of
      Var tn _ | not (isQualified (getName tn))-> return $ (empty, tn)
      _        -> do name <- newVarName "x"
                     let tp = typeOf expr
                         tname = TName name tp
                     doc <- genStat (ResultAssign tname Nothing) expr
                     if (dstartsWith doc (show (ppName name) ++ " ="))
                       then return (ppType tp <+> doc, tname)
                       else return (ppVarDecl tname <.> unitSemi tp  <-> doc, tname)


---------------------------------------------------------------------------------
-- Pure expressions
---------------------------------------------------------------------------------

genPure   :: Expr -> Asm Doc
genPure expr
  = case expr of
     TypeApp e _ -> genPure e
     TypeLam _ e -> genPure e
     -- Var name (InfoExternal formats)
     --   -> genWrapExternal name formats  -- unapplied inlined external: wrap as function
     Var name info
       -> case splitFunScheme (typeOf name) of
            Just (_,_,argTps,eff,resTp) | isQualified (getName name) && isInfoArity info -- wrap bare top-level functions
              -> do argNames <- mapM newVarName ["x" ++ show i | i <- [1..length argTps]]
                    let tnames = [TName name tp | (name,(_,tp)) <- zip argNames argTps]
                        body   = (App expr [Var name InfoNone | name <- tnames])
                    genLambda tnames eff body
            _ -> case info of
                   InfoExternal formats -> genInlineExternal name formats []
                   _ -> return (ppName (getName name))
     Con name info
       | getName name == nameTrue -> return (text "true")
       | getName name == nameFalse -> return (text "false")
       | getName name == nameUnit  -> return (text "kk_Unit")
       | otherwise -> return (conCreateName (getName name) <.> arguments [])
     Lit l
       -> return $ ppLit l
     Lam params eff body
       -> {-
          do args    <- mapM genCommentTName params
             bodyDoc <- genStat (ResultReturn Nothing params) body
             return (text "function" <.> tupled args <+> block bodyDoc)
          -}
          genLambda params eff body
     _ -> failure ("Backend.C.FromCore.genPure: invalid expression:\n" ++ show expr)

{-
genLambda :: Expr -> ([TypeVar],[Pred],[(Name,Type)],Effect,Type) -> Asm Doc
genLambda expr (_,_,argTps,eff,resTp)
  = do argNames <- genVarNames (length argTps)
       let tnames = [TName name tp | (name,tp) <- zip argNames argTps]
           lam = Lam tnames eff $
                 App expr [Var tname InfoNone | tname <- tnames]
       in genInline
-}

isPat :: Bool -> Pattern -> Bool
isPat b q
  = case q of
      PatWild     -> False
      PatLit _    -> False
      PatVar _ q' -> isPat b q'
      PatCon {}   -> getName (patConName q) == if b then nameTrue else nameFalse

-- | Generates an effect-free expression
--   NOTE: Throws an error if expression is not guaranteed to be effectfree
genInline :: Expr -> Asm Doc
genInline expr | isPureExpr expr
  = genPure expr
genInline expr
  = do (decls,doc) <- genExprPrim expr
       when (not (null decls)) $
         failure ("Backend.C.FromCore.genInline: not an inlineable expression? " ++ show expr)
       return doc
    {-
    case expr of
      _  | isPureExpr expr -> genPure expr

      TypeLam _ e -> genInline e
      TypeApp e _ -> genInline e
      App f args
        -> do argDocs <- mapM genInline args
              case extractExtern f of
                Just (tname,formats)
                  -> case args of
                       [Lit (LitInt i)] | getName tname == nameInt32 && isSmallInt32 i
                         -> return (pretty i)
                       _ -> genInlineExternal tname formats argDocs
                Nothing
                  -> case (f,args) of
                       ((Var tname _),[Lit (LitInt i)]) | getName tname == nameInt32 && isSmallInt i
                         -> return (pretty i)
                       _ -> do fdoc <- genInline f
                               return (fdoc <.> tupled argDocs)
      _ -> failure ("Backend.C.FromCore.genInline: invalid expression:\n" ++ show expr)
-}


---------------------------------------------------------------------------------
-- Applications
---------------------------------------------------------------------------------

genApp :: Expr -> [Expr] -> Asm ([Doc],Doc)
genApp f args
  = do sapp <- genAppSpecial f args
       case sapp of
         Just app -> return ([],app)
         Nothing  -> -- trace ("genAppNormal: " ++ show (f,args)) $
                     genAppNormal f args


genAppNormal :: Expr -> [Expr] -> Asm ([Doc],Doc)
-- special: allocat
genAppNormal (Var allocAt _) [Var at _, App (Con tname repr) args]  | getName allocAt == nameAllocAt
  = do (decls,argDocs) <- genInlineableExprs args
       let atDoc = ppName (getName at)
       return (decls,conCreateName (getName tname) <.> arguments ([atDoc] ++ argDocs))
genAppNormal (Var allocAt _) [Var at _, App (TypeApp (Con tname repr) targs) args]  | getName allocAt == nameAllocAt
  = do (decls,argDocs) <- genInlineableExprs args
       let atDoc = ppName (getName at)
       return (decls,conCreateName (getName tname) <.> arguments ([atDoc] ++ argDocs))
genAppNormal v@(Var allocAt _) [at, Let dgs expr]  | getName allocAt == nameAllocAt  -- can happen due to box operations
  = genExpr (Let dgs (App v [at,expr]))

-- special: conAssignFields
genAppNormal (Var (TName conFieldsAssign typeAssign) _) (Var reuseName (InfoConField conName nameNil):fieldValues) | conFieldsAssign == nameConFieldsAssign
  = do (decls,fieldDocs) <- genExprs fieldValues
       tmp <- genVarName "con"
       let conTp    = text "struct" <+> ppName (getName conName) <.> text "*"
           tmpDecl  = conTp <+> tmp <+> text "=" <+> parens conTp <.> ppName (getName reuseName) <.> semi
           fieldNames = case splitFunScheme typeAssign of
                          Just (_,_,args,_,_) -> tail (map fst args)
                          _ -> failure ("Backend.C.FromCore: illegal conAssignFields type: " ++ show (pretty typeAssign))
           assigns  = [tmp <.> text "->" <.> ppName fname <+> text "=" <+> fval <.> semi
                      | (fname,fval) <- zip fieldNames fieldDocs]
           result   = conBaseCastName (getName conName) <.> parens tmp
       return (decls ++ [tmpDecl] ++ assigns, result)

-- special: cfield-hole
genAppNormal (Var unbox _) [App (Var cfieldHole _) []] | getName cfieldHole == nameCFieldHole && getName unbox == nameUnbox
  = return ([],ppType (resultType (typeOf unbox)) <.> text "_hole()")

-- special: cfield-of
genAppNormal (Var cfieldOf _) [App (Var box _) [App (Var dup _) [Var con _]], Lit (LitString conName), Lit (LitString fieldName)]  | getName cfieldOf == nameCFieldOf && getName dup == nameDup
  = do let doc = genFieldAddress con (readQualified conName) (readQualified fieldName)
       return ([],text "(kk_box_t*)" <.> parens doc)

genAppNormal (Var cfieldOf _) [App (Var box _) [Var con _], Lit (LitString conName), Lit (LitString fieldName)]  | getName cfieldOf == nameCFieldOf
 = do let drop = map (<.> semi) (genDupDropCall False (typeOf con) (ppName (getName con)))
          doc = genFieldAddress con (readQualified conName) (readQualified fieldName)
      return (drop,text "(kk_box_t*)" <.> parens doc)


-- normal
genAppNormal f args
  = do (decls,argDocs) <- genInlineableExprs args
       case extractExtern f of
         -- known external
         Just (tname,formats)
           -> do (edecls,doc) <- genExprExternal tname formats argDocs
                 return ((edecls ++ decls), doc)
         Nothing
           -> case f of
               -- constructor
               Con tname repr
                 -> let at = if (dataReprIsValue (conDataRepr repr)) then [] else [text "kk_reuse_null"]
                    in return (decls,conCreateName (getName tname) <.> arguments (at ++ argDocs))
               -- call to known function
               Var tname _ | getName tname == nameAllocAt
                 -> failure ("Backend.C.genApp.Var.allocat: " ++ show (f,args))
               Var tname (InfoArity m n) | isQualified (getName tname)
                 -> return (decls,ppName (getName tname) <.> arguments argDocs)
               -- call unknown function_t
               _ -> do (fdecls,fdoc) <- case f of
                                          Var tname info -> return ([], ppName (getName tname)) -- prevent lambda wrapping recursively
                                          _ -> do (fdecl,fname) <- genVarBinding f
                                                  return ([fdecl],ppName (getName fname))
                       let (cresTp,cargTps) = case splitFunScheme (typeOf f) of
                                               Just (_,_,argTps,_,resTp)
                                                 -> (ppType resTp, tupled ([text "kk_function_t"] ++
                                                                           (map (ppType . snd) argTps) ++
                                                                           [text "kk_context_t*"]))
                                               _ -> failure $ ("Backend.C.genAppNormal: expecting function type: " ++ show (pretty (typeOf f)))
                       return (fdecls ++ decls, text "kk_function_call" <.> tupled [cresTp,cargTps,fdoc,arguments (fdoc:argDocs)])



genFieldAddress :: TName -> Name -> Name -> Doc
genFieldAddress conVar conName fieldName
  = parens (text "&" <.> conAsNameX (conName) <.> parens (ppName (getName conVar)) <.> text "->" <.> ppName (unqualify fieldName))


genAppSpecial :: Expr -> [Expr] -> Asm (Maybe Doc)
genAppSpecial f args
  = do platform <- getPlatform
       case (f,args) of
        (Var tname _, [Lit (LitInt i)]) | getName tname == nameInt32 && isSmallInt32 i
          -> return (Just (genLitInt32 i))
        (Var tname _, [Lit (LitInt i)]) | getName tname == nameSizeT && isSmallSizeT platform i
          -> return (Just (genLitSizeT i))
        _ -> case extractExtern f of
               Just (tname,formats)
                 -- inline external
                 -> case args of
                     [Lit (LitInt i)] | getName tname == nameInt32 && isSmallInt32 i
                       -> return (Just (parens (text "(int32_t)" <.> pretty i)))
                     [Lit (LitInt i)] | getName tname == nameSizeT && isSmallSizeT platform i
                       -> return (Just (parens (text "(size_t)" <.> pretty i)))
                     _ -> return Nothing
               _ -> return Nothing

{-
genAppInline :: Expr -> [Expr] -> Asm Doc
genAppInline f args
  = do sapp <- genAppSpecial f args
       case sapp of
         Just app ->  return app
         Nothing  ->  do argDocs <- mapM genInline args
                         case f of
                           Con tname repr
                             -> return (conCreateName (getName tname) <.> tupled argDocs)
                           _ -> case extractExtern f of
                                  Just (tname,formats)
                                    -> genInlineExternal tname formats argDocs
                                  Nothing
                                    -> do fdoc <- genInline f
                                          return (fdoc <.> tupled argDocs)
-}

---------------------------------------------------------------------------------
-- Externals
---------------------------------------------------------------------------------

extractExtern :: Expr -> Maybe (TName,[(Target,String)])
extractExtern expr
  = case expr of
      TypeApp (Var tname (InfoExternal formats)) targs -> Just (tname,formats)
      Var tname (InfoExternal formats) -> Just (tname,formats)
      _ -> Nothing

-- inlined external sometimes  needs wrapping in a applied function block
genInlineExternal :: TName -> [(Target,String)] -> [Doc] -> Asm Doc
genInlineExternal tname formats argDocs
  = do (decls,doc) <- genExprExternal tname formats argDocs
       if (null decls)
        then return doc
        else error ("Backend.C.FromCore.genInlineExternal: TODO: inline external declarations: " ++ show (vcat (decls++[doc])))

-- generate external: needs to add try blocks for primitives that can throw exceptions
genExprExternal :: TName -> [(Target,String)] -> [Doc] -> Asm ([Doc],Doc)

-- special case box/unbox
genExprExternal tname formats [argDoc] | getName tname == nameBox || getName tname == nameUnbox
  = let isBox = (getName tname == nameBox)
        tp    = case typeOf tname of
                  TFun [(_,fromTp)] _ toTp -> if (isBox) then fromTp else toTp
                  _ -> failure $ ("Backend.C.genExprExternal.unbox: expecting function type: " ++ show tname ++ ": " ++ show (pretty (typeOf tname)))
        call  = genBoxCall (if (isBox) then "box" else "unbox") False tp argDoc
    in return ([], call)


-- special case dropn
genExprExternal tname formats [argDoc,scanDoc] | getName tname == nameDrop
  = let isDup = (getName tname == nameDup)
        tp    = case typeOf tname of
                  TFun [(_,fromTp),(_,_)] _ toTp -> fromTp
                  _ -> failure $ ("Backend.C.genExprExternal.dropn: expecting function type: " ++ show tname ++ ": " ++ show (pretty (typeOf tname)))
        call  = hcat (genDropNCall tp [argDoc,scanDoc])
    in return ([], call)


-- special case drop_reuse
genExprExternal tname formats [argDoc,scanDoc] | getName tname == nameDropReuse
  = let tp    = case typeOf tname of
                  TFun [(_,fromTp),(_,_)] _ toTp -> fromTp
                  _ -> failure $ ("Backend.C.genExprExternal.drop_reuse: expecting function type: " ++ show tname ++ ": " ++ show (pretty (typeOf tname)))
        call  = hcat (genDropReuseCall tp [argDoc,scanDoc])
    in return ([], call)

-- special case dup/drop
genExprExternal tname formats [argDoc] | getName tname == nameDup || getName tname == nameDrop
  = let isDup = (getName tname == nameDup)
        tp    = case typeOf tname of
                  TFun [(_,fromTp)] _ toTp -> fromTp
                  _ -> failure $ ("Backend.C.genExprExternal.drop: expecting function type: " ++ show tname ++ ": " ++ show (pretty (typeOf tname)))
        call  = hcat (genDupDropCall isDup tp argDoc)   -- if empty, pass dup argument along?
    in return ([], call)

-- special case is-unique
genExprExternal tname formats [argDoc] | getName tname == nameIsUnique
  = let tp    = case typeOf tname of
                  TFun [(_,fromTp)] _ toTp -> fromTp
                  _ -> failure $ ("Backend.C.genExprExternal.is_unique: expecting function type: " ++ show tname ++ ": " ++ show (pretty (typeOf tname)))
        call  = hcat (genIsUniqueCall tp argDoc)
    in return ([], call)

-- special case free
genExprExternal tname formats [argDoc] | getName tname == nameFree
  = let tp    = case typeOf tname of
                  TFun [(_,fromTp)] _ toTp -> fromTp
                  _ -> failure $ ("Backend.C.genExprExternal.free: expecting function type: " ++ show tname ++ ": " ++ show (pretty (typeOf tname)))
        call  = hcat (genFreeCall tp argDoc)
    in return ([], call)

-- special case decref
genExprExternal tname formats [argDoc] | getName tname == nameDecRef
  = let tp    = case typeOf tname of
                  TFun [(_,fromTp)] _ toTp -> fromTp
                  _ -> failure $ ("Backend.C.genExprExternal.decref: expecting function type: " ++ show tname ++ ": " ++ show (pretty (typeOf tname)))
        call  = hcat (genDecRefCall tp argDoc)
    in return ([], call)

-- special case reuse
genExprExternal tname formats [argDoc] | getName tname == nameReuse
  = let tp    = case typeOf tname of
                  TFun [(_,fromTp)] _ toTp -> fromTp
                  _ -> failure $ ("Backend.C.genExprExternal.reuse: expecting function type: " ++ show tname ++ ": " ++ show (pretty (typeOf tname)))
        call  = hcat (genReuseCall tp argDoc)
    in return ([], call)

-- special case: cfield hole
genExprExternal tname formats [] | getName tname == nameCFieldHole
  = return ([],ppType (resultType (typeOf tname)) <.> text "_hole()")

-- special case: cfield set
genExprExternal tname formats [fieldDoc,argDoc] | getName tname == nameCFieldSet
  = return ([],text "*" <.> parens fieldDoc <+> text "=" <+> argDoc)

-- normal external
genExprExternal tname formats argDocs0
  = let name = getName tname
        format = getFormat tname formats
        argDocs = map (\argDoc -> if (all (\c -> isAlphaNum c || c == '_') (asString argDoc)) then argDoc else parens argDoc) argDocs0
    in return $ case map (\fmt -> ppExternalF name fmt argDocs) $ lines format of
         [] -> ([],empty)
         ds -> (init ds, last ds)
  where
    ppExternalF :: Name -> String -> [Doc] -> Doc
    ppExternalF name []  args
     = empty
    ppExternalF name k@('\\':'#':xs) args
     = char '#' <.> ppExternalF name xs args
    ppExternalF name k@('#':'#':xs) args
     = failure ("Backend.C.FromCore: type arguments in C external in: " ++ show tname)
    ppExternalF name k@('#':y:xs)  args
     = if  y `elem` ['1'..'9']
        then (let n = length args
                  i = fromEnum y - fromEnum '1'
              in assertion ("illegal index in external: " ++ show tname ++ ":" ++ show (pretty (typeOf tname)) ++ "("++k++"): index: " ++ show i ++ ", arguments: " ++ show args) (i < n) $
                 (args!!i) <.> ppExternalF name xs args)
        else char y <.> ppExternalF name xs args
    ppExternalF name (x:xs)  args
     = char x <.> ppExternalF name xs args

getFormat :: TName -> [(Target,String)] -> String
getFormat tname formats
  = case lookup C formats of
      Nothing -> case lookup Default formats of
         Just s  -> s
         Nothing -> -- failure ("backend does not support external in " ++ show tname ++ ": " ++ show formats)
                    trace( "warning: C backend does not support external in " ++ show tname ) $
                      ("kk_unsupported_external(\"" ++ (show tname) ++ "\")")
      Just s -> s

genDefName :: TName -> Asm Doc
genDefName tname
  = return (ppName (unqualify (getName tname)))


genVarName :: String -> Asm Doc
genVarName s = do n <- newVarName s
                  return $ ppName n

-- | Generates `i` fresh variables and delivers them as `Doc` right away
genVarNames :: Int -> Asm [Doc]
genVarNames i = do ns <- newVarNames i
                   return $ map ppName ns

-- | Generate a name with its type in comments
genCommentTName :: TName -> Asm Doc
genCommentTName (TName n t)
  = do env <- getPrettyEnv
       return $ ppName n <+> comment (Pretty.ppType env t )


---------------------------------------------------------------------------------
-- Classification
---------------------------------------------------------------------------------

extractExternal  :: Expr -> Maybe (TName, String, [Expr])
extractExternal expr
  = case expr of
      App (TypeApp (Var tname (InfoExternal formats)) targs) args
        -> Just (tname, format tname formats, args)
      App var@(Var tname (InfoExternal formats)) args
        -> Just (tname, format tname formats, args)
      _ -> Nothing
  where
    format tn fs
      = case lookup C fs of
          Nothing -> case lookup Default fs of
                       Nothing -> failure ("backend does not support external in " ++ show tn ++ show fs)
                       Just s  -> s
          Just s -> s

isFunExpr :: Expr -> Bool
isFunExpr expr
  = case expr of
      TypeApp e _   -> isFunExpr e
      TypeLam _ e   -> isFunExpr e
      Lam args eff body -> True
      _                 -> False

isInlineableExpr :: Expr -> Bool
isInlineableExpr expr
  = case expr of
      TypeApp expr _   -> isInlineableExpr expr
      TypeLam _ expr   -> isInlineableExpr expr
      Lit (LitString _)-> False
      -- C has no guarantee on argument evaluation so we only allow a select few operations to be inlined
      App (Var v (InfoExternal _)) [] -> getName v `elem` [nameYielding,nameReuseNull,nameCFieldHole]
      -- App (Var v (InfoExternal _)) [arg] | getName v `elem` [nameBox,nameDup,nameInt32] -> isInlineableExpr arg
      App (Var v _) [arg] | getName v `elem` [nameBox,nameInt32,nameReuse,nameIsUnique] -> isInlineableExpr arg

      --App (Var _ (InfoExternal _)) args -> all isPureExpr args  -- yielding() etc.

      -- App (Var v _) [arg] | getName v `elem` [nameBox,nameUnbox] -> isInlineableExpr arg
      {-
      -- TODO: comment out for now as it may prevent a tailcall if inlined
      App f args       -> -- trace ("isInlineable f: " ++ show f) $
                          isPureExpr f && all isPureExpr args
                          -- all isInlineableExpr (f:args)
                          && not (isFunExpr f) -- avoid `fun() {}(a,b,c)` !
                          -- && getParamArityExpr f == length args
      -}
      _                -> isPureExpr expr

isPureExpr :: Expr -> Bool
isPureExpr expr
  = case expr of
      TypeApp expr _  -> isPureExpr expr
      TypeLam _ expr  -> isPureExpr expr
      Var _ (InfoExternal{}) -> False
      Var _ _ -> True
      Con _ _ -> True
      Lit (LitString _) -> False  -- for our purposes, it's not pure (as it needs a declaration)
      Lit _   -> True
      Lam _ _ _ -> True
      _       -> False


isTailCalling :: Expr -> Name -> Bool
isTailCalling expr n
  = case expr of
      TypeApp expr _    -> expr `isTailCalling` n     -- trivial
      TypeLam _ expr    -> expr `isTailCalling` n     -- trivial
      Lam _ _ _           -> False                      -- lambda body is a new context, can't tailcall
      Var _ _           -> False                      -- a variable is not a call
      Con _ _           -> False                      -- a constructor is not a call
      Lit _             -> False                      -- a literal is not a call
      App (Var tn info) args   | getName tn == n            -- direct application can be a tail call
                        -> infoArity info == length args
      App (TypeApp (Var tn info) _) args | getName tn == n  -- tailcalled function might be polymorphic and is applied to types before
                        -> infoArity info == length args
      App (Var tn _) [e] | getName tn == nameReturn   -- a return statement is transparent in terms of tail calling
                        -> e `isTailCalling` n
      App _ _           -> False                      -- other applications don't apply
      Let _ e           -> e `isTailCalling` n        -- tail calls can only happen in the actual body
      Case _ bs         -> any f1 bs                  -- match statement get analyzed in depth
  where
    f1 (Branch _ gs) = any f2 gs                      -- does any of the guards tailcall?
    f2 (Guard _ e)   = e `isTailCalling` n            -- does the guarded expression tailcall?

---------------------------------------------------------------------------------
-- The assembly monad
---------------------------------------------------------------------------------

newtype Asm a = Asm { unAsm :: Env -> St -> (a, St)}

instance Functor Asm where
  fmap f (Asm a) = Asm (\env st -> case a env st of
                                     (x,st') -> (f x, st'))

instance Applicative Asm where
  pure  = return
  (<*>) = ap

instance Monad Asm where
  return x      = Asm (\env st -> (x,st))
  (Asm a) >>= f = Asm (\env st -> case a env st of
                                    (x,st1) -> case f x of
                                                 Asm b -> b env st1)

runAsm :: Int -> Env -> Asm a -> (a,Doc,Doc)
runAsm uniq initEnv (Asm asm)
  = case asm initEnv (initSt uniq) of
      (x,st) -> (x, vcat (reverse (cdoc st)), vcat (reverse (hdoc st)))

data St  = St  { uniq :: Int
               , hdoc :: [Doc]  -- h file in reverse
               , cdoc :: [Doc]  -- c file in reverse
               , idoc :: [Doc]  -- initialization expressions
               , tdoc :: [Doc]  -- toplevel (goes to either H or C)
               }

data Env = Env { moduleName        :: Name                    -- | current module
               , cdefName          :: Name                    -- | current definition
               , cdefToHeader      :: Bool                    -- | emit current def to header?
               , prettyEnv         :: Pretty.Env              -- | for printing nice types
               , substEnv          :: [(TName, Doc)]          -- | substituting names
               , newtypes          :: Newtypes
               , platform          :: Platform
               , inStatement       :: Bool                    -- | for generating correct function declarations in strict mode
               }

data Result = ResultReturn (Maybe TName) [TName] -- first field carries function name if not anonymous and second the arguments which are always known
            | ResultAssign TName (Maybe Name)    -- variable name and optional label to break

initSt uniq = St uniq [] [] [] []

instance HasUnique Asm where
  updateUnique f
    = Asm (\env st -> (uniq st, st{ uniq = f (uniq st)}))

updateSt f
  = Asm (\env st -> ((),f st))

getSt
  = updateSt id

setSt st
  = updateSt (const st)


emitToH doc
  = updateSt (\st -> st{hdoc = doc : hdoc st })
emitToC doc
  = updateSt (\st -> st{cdoc = doc : cdoc st })
emitToInit doc
  = updateSt (\st -> st{idoc = doc : idoc st })
emitToTop doc
  = updateSt (\st -> st{tdoc = doc : tdoc st })

emitToCurrentDef doc
  = do env <- getEnv
       if (cdefToHeader env) then emitToH doc else emitToC doc

getInit :: Asm Doc
getInit
  = Asm (\env st -> (vcat (reverse (idoc st)), st{ idoc = [] }))

getTop :: Asm Doc
getTop
  = Asm (\env st -> (vcat (reverse (tdoc st)), st{ tdoc = [] }))

getEnv
  = Asm (\env st -> (env, st))

withEnv f (Asm asm)
  = Asm (\env st -> asm (f env) st)

localUnique asm
  = do u <- updateUnique id
       x <- asm
       setUnique u
       return x

getDefToHeader :: Asm Bool
getDefToHeader
  = do env <- getEnv
       return (cdefToHeader env)

withDef :: Name -> Bool -> Asm a -> Asm a
withDef name toHeader asm
  = withEnv (\env -> env{ cdefName = name, cdefToHeader = toHeader })  asm

newVarName :: String -> Asm Name
newVarName s
  = do u <- unique
       return (newName ("." ++ s ++ show u))

newVarNames :: Int -> Asm [Name]
newVarNames 0 = return []
newVarNames i
  = do n  <- newVarName "x"
       ns <- newVarNames (i - 1)
       return (n:ns)

getModule :: Asm Name
getModule
  = do env <- getEnv
       return (moduleName env)

newDefVarName :: String -> Asm Name
newDefVarName s
  = do env <- getEnv
       u <- unique
       return $ postpend ("-" ++ s ++ show u) (cdefName env)

getPrettyEnv :: Asm Pretty.Env
getPrettyEnv
  = do env <- getEnv
       return (prettyEnv env)

withTypeVars :: [TypeVar] -> Asm a -> Asm a
withTypeVars vars asm
  = withEnv (\env -> env{ prettyEnv = Pretty.niceEnv (prettyEnv env) vars }) asm

withNameSubstitutions :: [(TName, Doc)] -> Asm a -> Asm a
withNameSubstitutions subs asm
  = withEnv (\env -> env{ substEnv = subs ++ substEnv env }) asm

withStatement :: Asm a -> Asm a
withStatement asm
  = withEnv (\env -> env{ inStatement = True }) asm

getInStatement :: Asm Bool
getInStatement
  = do env <- getEnv
       return (inStatement env)

getNewtypes :: Asm Newtypes
getNewtypes
  = do env <- getEnv
       return (newtypes env)

getPlatform :: Asm Platform
getPlatform
 = do env <- getEnv
      return (platform env)

---------------------------------------------------------------------------------
-- Pretty printing
---------------------------------------------------------------------------------

ppLit :: Lit -> Doc
ppLit lit
    = case lit of
      LitInt i    -> if (isSmallInt(i))
                      then text "kk_integer_from_small" <.> parens (pretty i)
                     else if (isSmallInt32(i))
                      then text "kk_integer_from_int" <.> arguments [pretty i]
                      else text "kk_integer_from_str" <.> arguments [dquotes (pretty i)]
      LitChar c   -> let i = fromEnum c
                     in if (c >= ' ' && c <= '~')
                         then text (show c)
                         else text ("0x" ++ showHex 4 (fromEnum c))
      LitFloat d  -> text (showHFloat d "")
      LitString s -> failure ("Backend.C.FromCore: ppLit: cannot inline string literal: " ++ show s)

cstring :: String -> (Doc,Int)
cstring s
  = let (cstr,ccnt) = unzip (map escape s)
    in (dquotes (hcat cstr), sum ccnt)
  where
    bytes bs
      = text ("\" \"" ++ concat ["\\x" ++ showHex 2 b | b <- bs] ++ "\" \"")
    escape c
      = if (c=='\0')
         then (bytes [0xC0,0x80],2) -- embedded zero character
        else if (c < ' ')
         then (if (c=='\n') then text "\\n"
               else if (c == '\r') then text "\\r"
               else if (c == '\t') then text "\\t"
               else bytes [fromEnum c], 1)
        else if (c <= '\x7F')
         then (if (c == '\"') then text "\\\""
               else if (c=='\'') then text "\\'"
               else if (c=='\\') then text "\\\\"
               else if (c=='?')  then text "\\?"  -- to avoid accidental trigraphs
               else char c, 1)
        else let x = fromEnum c
             in if (x <= 0x07FF)
                 then (bytes [0xC0 + (x`div`64), 0x80 + (x`mod`64)], 2)
                else if (x <= 0xFFFF)
                 then (bytes [0xE0 + (x`div`4096), 0x80 + ((x`div`64)`mod`64), 0x80 + (x`mod`64)], 3)
                else if (x <= 0x10FFFF)
                 then (bytes [0xF0 + (x`div`262144), 0x80 + ((x`div`4096)`mod`64), 0x80 + ((x`div`64)`mod`64), 0x80 + (x`mod`64)], 4)
                 else escape (toEnum 0xFFFD)


genLitInt32 :: Integer -> Doc
genLitInt32 i
  = parens (text "(int32_t)" <.> pretty i)

genLitSizeT :: Integer -> Doc
genLitSizeT i
  = parens (text "(size_t)" <.> pretty i)

isSmallLitInt expr
  = case expr of
      Lit (LitInt i)  -> isSmallInt i
      _ -> False

isSmallInt i = (i >= minSmallInt && i <= maxSmallInt)
maxSmallInt, minSmallInt :: Integer
maxSmallInt = 2047  -- 2^13 - 1   (conservative: 14 bits on 32-bits platform)
minSmallInt = -maxSmallInt - 1

isSmallInt32 i = (i >= minSmallInt32 && i <= maxSmallInt32)
maxSmallInt32, minSmallInt32 :: Integer
maxSmallInt32 = 2147483647  -- 2^31 - 1
minSmallInt32 = -maxSmallInt32 - 1

isSmallInt64 i = (i >= minSmallInt64 && i <= maxSmallInt64)
maxSmallInt64, minSmallInt64 :: Integer
maxSmallInt64 = 9223372036854775807  -- 2^63 - 1
minSmallInt64 = -maxSmallInt64 - 1

isSmallSizeT platform i
  | sizeSize platform == 4 = (i >= 0 && i <= 4294967295)
  | sizeSize platform == 8 = (i >= 0 && i <= 18446744073709551615)
  | otherwise = failure $ "Backend.C.isSmallSizeT: unknown platform size_t: " ++ show platform

ppName :: Name -> Doc
ppName name
  = if isQualified name
     then ppModName (qualifier name) <.> text "_" <.> text (asciiEncode False (show (unqualify name)))-- encode False (unqualify name)
     else encode False name

ppQName :: Name -> Name -> Doc
ppQName modName name
  = if (modName == qualifier name)   -- We need to qualify always since otherwise we may clash with local variables. i.e. fun f( x : int ) { Main.x( x ) }
     then ppName (unqualify name)
     else ppName name

ppModName :: Name -> Doc
ppModName name
  = text "kk_" <.> encode True (name)

encode :: Bool -> Name -> Doc
encode isModule name
  = let s = asciiEncode isModule (show name)
    in if (isReserved s)
         then text ("kkloc_" ++ s)
         else text s

isReserved :: String -> Bool
isReserved s
  = if (s `startsWith` "kk_")
      then True
      else s `S.member` reserved

reserved :: S.Set String
reserved
  = S.fromList $ -- C pseudo-keywords
    [ "bool"
    , "toString"
    , "arguments"
    , "eval"
    ]
    ++ -- C types
    [ "char"
    , "int"
    , "intptr_t"
    , "long"
    , "short"
    , "signed"
    , "size_t"
    , "ssize_t"
    , "uintptr_t"
    , "unsigned"
    ]
    ++ -- C keywords
    [ "async"
    , "await"
    , "break"
    , "case"
    , "catch"
    , "continue"
    , "const"
    , "debugger"
    , "default"
    , "delete"
    , "do"
    , "else"
    , "finally"
    , "for"
    , "function"
    , "if"
    , "in"
    , "instanceof"
    , "new"
    , "return"
    , "switch"
    , "this"
    , "throw"
    , "try"
    , "typeof"
    , "var"
    , "void"
    , "while"
    , "with"
    , "yield"
    ]
    ++ -- reserved for future use
    [ "class"
    , "enum"
    , "export"
    , "extends"
    , "import"
    , "super"
    ]
    ++ -- special macros
    [ "errno"
    , "exception_info"
    ]

block :: Doc -> Doc
block doc
  = text "{" <--> tab doc <--> text "}"

tblock :: Doc -> Doc -> Doc
tblock tpDoc doc
  = text "{" <+> tpDoc <--> tab doc <--> text "}"


tcoBlock :: Doc -> Doc -> Doc
tcoBlock tpDoc doc
  = tblock tpDoc (text "kk__tailcall: ;" <-> doc)

tailcall :: Doc
tailcall  = text "goto kk__tailcall;"

object :: [(Doc, Doc)] -> Doc
object xs
  = text "{" <+> hcat ( punctuate (comma <.> space) (map f xs) ) <+> text "}"
  where
    f (d1, d2) = d1 <.> colon <+> d2

tab :: Doc -> Doc
tab doc
  = indent 2 doc

typeComment = comment

comment :: Doc -> Doc
comment d
  = text "/*" <+> d <+> text "*/ "

linecomment :: Doc -> Doc
linecomment d
  = text "//" <+> d

debugComment :: String -> Doc
debugComment s
  = if debug
      then comment (text s)
      else empty

debugWrap     :: String -> Doc -> Doc
debugWrap s d
  = if debug
      then debugComment ("<" ++ s ++ ">") <-> tab d <-> debugComment ("</" ++ s ++ ">")
      else d

tagField :: Doc
tagField  = text "_tag"

constdecl :: Doc
constdecl = text "const"

tparameters :: [TName] -> Doc
tparameters tnames
  = ntparameters [(name,tp) | TName name tp <- tnames]

resultType :: Type -> Type
resultType tp
  = case splitFunScheme tp of
      Just (_,_,_,_,resTp) -> resTp
      _ -> failure ("Backend.C.FromCore.resultType: not a function type: " ++ show (pretty tp))

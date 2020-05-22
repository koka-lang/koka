-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Backend.C.FromCore ( cFromCore ) where

import Platform.Config(version)
import Lib.Trace
import Control.Applicative hiding (empty)
import Control.Monad
import Data.List ( intersperse, partition )
import Data.Char
-- import Data.Maybe
-- import Data.Monoid ( mappend )
import qualified Data.Set as S

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

cFromCore :: Newtypes -> Int -> Maybe (Name,Bool) -> Core -> (Doc,Doc)
cFromCore newtypes uniq mbMain core
  = case runAsm uniq (Env moduleName moduleName False penv externalNames newtypes False) (genModule mbMain core) of
      ((),cdoc,hdoc) -> (cdoc,hdoc)
  where
    moduleName = coreProgName core
    penv       = Pretty.defaultEnv{ Pretty.context = moduleName, Pretty.fullNames = False }

genModule :: Maybe (Name,Bool) -> Core -> Asm ()
genModule mbMain core0
  =  do core <- liftUnique (boxCore core0)  -- box/unbox transform
        let externs       = vcat (concatMap includeExternal (coreProgExternals core))
            headComment   = text "// Koka generated module:" <+> string (showName (coreProgName core)) <.> text ", koka version:" <+> string version
            initSignature = text "void" <+> ppName (qualify (coreProgName core) (newName ".init")) <.> text "(void)"

        emitToInit $ vcat $ [text "static bool _initialized = false;"
                            ,text "if (_initialized) return;"
                            ,text "_initialized = true;"]
                            ++ map initImport (coreProgImports core)

        emitToC $ vcat $ [headComment
                         ,text "#include" <+> dquotes (text (moduleNameToPath (coreProgName core)) <.> text ".h")]
                         ++ externalIncludes

        emitToH $ vcat $ [ text "#pragma once"
                         , text "#ifndef __" <.> modName <.> text "_H"
                         , text "#define __" <.> modName <.> text "_H"
                         , headComment ]
                         ++ externalImports
                         ++ map moduleImport (coreProgImports core)

        emitToH (linebreak <.> text "// type declarations")
        genTypeDefs (coreProgTypeDefs core)
        emitToH (linebreak <.> text "// value declarations")
        genTopGroups (coreProgDefs core)

        init <- getInit
        emitToC $ linebreak
                  <.> text "// initialization"
                  <-> initSignature
                  <.> block init
        emitToH $ vcat [ linebreak <.> initSignature <.> semi <.> linebreak
                       , text "#endif // header"]
        return ()
  where
    modName         = ppModName (coreProgName core0)

    externalIncludes :: [Doc]
    externalIncludes
      = concatMap includeExternal (coreProgExternals core0)

    externalImports :: [Doc]
    externalImports
      = map fst (concatMap importExternal (coreProgExternals core0))

    initImport :: Import -> Doc
    initImport imp
      = ppName (qualify (importName imp) (newName ".init")) <.> text "();"



moduleImport :: Import -> Doc
moduleImport imp
  = text "#include" <+>
    (if null (importPackage imp)
      then dquotes (text (moduleNameToPath  (importName imp)) <.> text ".h")
      else brackets (text (importPackage imp) <.> text "/" <.> text (moduleNameToPath  (importName imp))) <.> text ".h")

includeExternal :: External -> [Doc]
includeExternal (ExternalInclude includes range)
  = let content = case lookup C includes of
                    Just s -> s
                    Nothing -> case lookup Default includes of
                                 Just s -> s
                                 Nothing -> ""
    in [align $ vcat $! map text (lines content)]
includeExternal _  = []


importExternal :: External -> [(Doc,Doc)]
importExternal (ExternalImport imports range)
  = let xs = case lookup C imports of
                    Just s -> [s]
                    Nothing -> case lookup Default imports of
                                 Just s -> [s]
                                 Nothing -> [] -- failure ("C backend does not support external import at " ++ show range)
    in [(text "#include" <+> (if (head s == '<') then text s else dquotes (text s)), pretty nm) | (nm,s) <- xs, not (null s)]
importExternal _
  = []

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
       let fdoc = vcat [ if null comm
                           then empty
                           else align (vcat (space : map text (lines (trim comm)))) {- already a valid C comment -}
                       , ppVarDecl (defTName def) <.> semi
                       , defDoc
                       ]
       return (fdoc)
  where
    -- remove final newlines and whitespace
    trim s = reverse (dropWhile (`elem` " \n\r\t") (reverse s))


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
       (if (isPublic vis) then emitToH else emitToC) (linebreak <.> sig <.> semi <+> tpDoc)

genFunDefSig :: Bool -> Def -> Doc
genFunDefSig inlineC def@(Def name tp defExpr vis sort inl rng comm)
  = let tryFun expr = case expr of
                        TypeApp e _   -> tryFun e
                        TypeLam _ e   -> tryFun e
                        Lam params eff body  -> genSig params body
                        _             -> error ("Backend.C.FromCore.genFunDefSig: not a function: " ++ show def)
    in tryFun defExpr
  where
    genSig params body
      = (if (inlineC) then text "static inline "
           else if (not (isPublic vis)) then text "static "
           else empty) <.>
        ppType (typeOf body) <+> ppName name <.> tparameters params


genTopDef :: Bool -> Bool -> Def -> Asm ()
genTopDef genSig inlineC def@(Def name tp expr vis sort inl rng comm)
  = do when (not (null comm)) $
         (if inlineC then emitToH else emitToC) (align (vcat (space : map text (lines (trim comm))))) {- already a valid C comment -}
       genTopDefDecl genSig inlineC def
  where
    -- remove final newlines and whitespace
    trim s = reverse (dropWhile (`elem` " \n\r\t") (reverse s))

genTopDefDecl :: Bool -> Bool -> Def -> Asm ()
genTopDefDecl genSig inlineC def@(Def name tp defBody vis sort inl rng comm)
  = let tryFun expr = case expr of
                        TypeApp e _   -> tryFun e
                        TypeLam _ e   -> tryFun e
                        Lam params eff body  -> genFunDef params body
                        _ -> do doc <- genStat (ResultAssign (TName name tp) Nothing) (defBody)
                                emitToInit doc
                                let decl = ppType tp <+> ppName name <.> semi
                                if (isPublic vis)
                                 then do emitToH (linebreak <.> text "extern" <+> decl)
                                         emitToC (linebreak <.> decl)
                                 else do emitToC (linebreak <.> text "static" <+> decl)
    in withDef name inlineC (tryFun defBody)
  where
    emit = if inlineC then emitToH else emitToC

    genFunDef :: [TName] -> Expr -> Asm ()
    genFunDef params body
      = do let args = map ( ppName . getName ) params
               isTailCall = body `isTailCalling` name
           bodyDoc <- (if isTailCall then withStatement else id)
                      (genStat (ResultReturn (Just name) params) body)
           penv <- getPrettyEnv
           let tpDoc = typeComment (Pretty.ppType penv tp)
           let sig = genFunDefSig inlineC def
           when (genSig && not inlineC && isPublic (defVis def)) $ emitToH (linebreak <.> sig <.> semi <+> tpDoc)
           top <- getTop -- get top level decls generated by body (for functions etc)
           emit $ linebreak
                  <.> top
                  <.> sig
                  <+> ( if isTailCall
                          then tcoBlock tpDoc bodyDoc
                          else debugComment ("genFunDef: no tail calls to " ++ showName name ++ " found")
                            <.> tblock tpDoc bodyDoc
                      )

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

       trace ("type " ++ show name ++ ": " ++ show dataRepr ++ ": " ++ show conReprs) $ return ()
       -- if (isExtend) then return ()
       -- generate the type
       if (dataRepr == DataEnum)
        then emitToH $ ppVis (dataInfoVis info) <.> text "typedef enum" <+> ppName (typeClassName (dataInfoName info)) <.> text "_e" <+>
                       block (vcat (punctuate comma (map ppEnumCon (zip (dataInfoConstrs info) conReprs)))) <+> ppName (typeClassName (dataInfoName info)) <.> semi <.> linebreak
        else if (dataReprIsValue dataRepr || isExtend)
          then return ()
              --  else emitToH $ text "struct" <+> ppName name <.> text "_s" <+> text "{" <+> text "datatype_tag_t _tag;" <+> text "};"
          else emitToH $ ppVis (dataInfoVis info) <.> text "typedef datatype_t"
                         <+> ppName (typeClassName name) <.> semi
                         <-> if (noCons) then empty
                              else text "struct" <+> ppName (typeClassName name) <.> text "_s" <+> text "{"
                                    <+> text "header_t _header;"
                                    <.> (if (dataRepr /= DataOpen) then empty else text " string_t _tag;")
                                    <+> text "};"

       -- order fields of constructors to have their scan fields first
       let conInfoReprs = zip (dataInfoConstrs info) conReprs
       conInfos <- mapM (\(conInfo,conRepr) -> do -- should never fail as mixed raw/scan is checked in kindInfer
                                                  mbOrd <- orderConFields (dataInfoDef info) (conInfoParams conInfo)
                                                  let (fields,scanCount0) = case mbOrd of
                                                                              Nothing -> failure ("Backend.C.FromCore.orderConFields: multiple mixed scan/raw fields" ++ show name)
                                                                              Just res -> res
                                                  let scanCount = if (dataRepr == DataOpen)
                                                                   then scanCount0 + 1  -- tag field
                                                                   else scanCount0
                                                  return (conInfo,conRepr,fields,scanCount)) conInfoReprs
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
          else emitToH $ if (isDataStruct dataRepr)
                  then ppVis (dataInfoVis info) <.> text "struct" <+> ppName name <.> text "_s"
                       <-> block (text "datatype_tag_t _tag;" <-> text "union"
                                  <+> block (vcat (map ppStructConField (dataInfoConstrs info))) <+> text "_cons;") <.> semi
                       <-> ppVis (dataInfoVis info) <.> text "typedef struct" <+> ppName name <.> text "_s" <+> ppName (typeClassName name) <.> semi
                  else ppVis (dataInfoVis info) <.> text "typedef struct"
                       <+> (case (dataRepr,dataInfoConstrs info) of
                              (DataIso,[con])          -> ppName ((conInfoName con))
                              (DataSingleStruct,[con]) -> ppName ((conInfoName con))
                              _                        -> ppName name <.> text "_s")
                       <+> ppName (typeClassName name) <.> semi

       -- generate functions for constructors
       mapM_ (genConstructor info dataRepr) conInfos

       -- generate functions for the datatype (box/unbox)
       genBoxUnbox name info dataRepr
  where
    ppEnumCon (con,conRepr)
      = ppName (conInfoName con)  -- <+> text "= datatype_enum(" <.> pretty (conTag conRepr) <.> text ")"

    ppStructConField con
      = text "struct" <+> ppName ((conInfoName con)) <+> ppName (unqualify (conInfoName con)) <.> semi



genBoxUnbox :: Name -> DataInfo -> DataRepr -> Asm ()
genBoxUnbox name info dataRepr
  = do genBox name info dataRepr
       genUnbox  name info dataRepr

genUnbox name info dataRepr
  = emitToH $
    text "static inline box_t box_" <.> ppName name <.> parens (ppName name <+> text "x") <+> block (
      text "return" <+> (
      case dataRepr of
        DataEnum -> parens (ppName name) <.> text "box_enum(x)"
        DataIso  -> let conInfo = head (dataInfoConstrs info)
                        (isoName,isoTp)   = (head (conInfoParams conInfo))
                    in text "box_" <.> ppType isoTp <.> parens (text "x." <.> ppName (unqualify isoName))
        _ -> case dataInfoDef info of
               DataDefValue raw scancount
                  -> let extra = if (dataRepr == DataStruct) then 1 else 0  -- adjust scan count for added "tag_t" members in structs with multiple constructors
                     in text "box_valuetype" <.> tupled [ppName name, text "x", pretty (scancount + extra) <+> text "/* scan fields */"]
               _  -> text "box_datatype(x)"
    ) <.> semi)

genBox name info dataRepr
  = emitToH $
    text "static inline" <+> ppName name <+> text "unbox_" <.> ppName name <.> parens (text "box_t x") <+> block (
      text "return" <+> (
      case dataRepr of
        DataEnum -> parens (ppName name) <.> text "unbox_enum(x)"
        DataIso  -> let conInfo = head (dataInfoConstrs info)
                        isoTp   = snd (head (conInfoParams conInfo))
                    in conCreateNameInfo conInfo <.> parens (text "unbox_" <.> ppType isoTp <.> text "(x)")
        _ | dataReprIsValue dataRepr
          -> text "unbox_valuetype" <.> tupled [ppName name, text "x"]
        _ -> text "unbox_datatype(x)"
    ) <.> semi)


genConstructorType :: DataInfo -> DataRepr -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Asm ()
genConstructorType info dataRepr (con,conRepr,conFields,scanCount) =
  case conRepr of
    ConEnum _ _ _
       -> return () -- already in enum declaration
    _ | null conFields && (dataRepr < DataNormal && not (isDataStruct dataRepr))
       -> return ()
    _  -> do emitToH $ ppVis (conInfoVis con) <.> text "struct" <+> ppName ((conInfoName con)) <+>
                       block (vcat (typeField ++ map ppConField conFields)) <.> semi
  where
    typeField  = if (dataReprIsValue dataRepr) then []
                  else [text "struct" <+> ppName (typeClassName (dataInfoName info)) <.> text "_s" <+> text "_type;"]

ppConField :: (Name,Type) -> Doc
ppConField (name,tp)
  = ppType tp <+> ppName (unqualify name) <.> semi

genConstructor :: DataInfo -> DataRepr -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Asm ()
genConstructor info dataRepr (con,conRepr,conFields,scanCount)
  = do genConstructorTest info dataRepr con conRepr
       genConstructorCreate info dataRepr con conRepr conFields scanCount
       genConstructorAccess info dataRepr con conRepr


genConstructorTest :: DataInfo -> DataRepr -> ConInfo -> ConRepr -> Asm ()
genConstructorTest info dataRepr con conRepr
  = do if (dataRepr/=DataOpen)
          then return ()
          else do emitToH $ text "extern string_t" <+> conTagName con <.> semi  -- real def already generated
                  -- emitToC $ text "tag_t" <+> conTagName con <.> semi
                  -- emitToInit $ conTagName con <+> text "=" <+> openTagName (dataInfoName info) <.> text "++;"
       emitToH  $ text "static inline bool" <+> (conTestName con) <.> tupled [ppName (typeClassName (dataInfoName info)) <+> text "x"]
                  <+> block( text "return (" <.> (
                  let nameDoc = ppName (conInfoName con)
                      -- tagDoc  = text "datatype_enum(" <.> pretty (conTag conRepr) <.> text ")"
                      dataTypeTagDoc = text "datatype_tag(x)"
                  in case conRepr of
                    ConEnum{}      -> text "x ==" <+> ppConTag con conRepr dataRepr
                    ConIso{}       -> text "true"
                    ConSingleton{} | dataRepr == DataAsList -> text "!datatype_is_ptr(x)"
                                   | otherwise -> text "x ==" <+> ppConTag con conRepr dataRepr
                    ConSingle{}    -> text "true"
                    ConStruct{}    -> text "x._tag ==" <+> ppConTag con conRepr dataRepr
                    ConAsCons{}    -> text "datatype_is_ptr(x)"
                    ConNormal{}    | dataRepr == DataSingleNormal -> text "datatype_is_ptr(x)"
                                   | otherwise -> dataTypeTagDoc <+> text "==" <+> ppConTag con conRepr dataRepr
                    ConOpen{}      -> text "((struct" <+> ppName (typeClassName (dataInfoName info)) <.> text "_s*)(x))->_type._tag" <+> text "==" <+> ppConTag con conRepr dataRepr
                  ) <.> text ");")

conTestName con
  = ppName (makeHiddenName "is" (conInfoName con))

conTagName con
  = ppName (makeHiddenName "tag" (conInfoName con))

ppConTag con conRepr dataRepr
  = case conRepr of
      ConOpen{} ->  ppName (makeHiddenName "tag" (conInfoName con))
      ConEnum{} ->  ppName (conInfoName con)
      ConSingleton{} | dataRepr == DataAsList -> text "datatype_enum(" <.> pretty (conTag conRepr) <.> text ")" -- ppName ((conInfoName con))
      _         | dataRepr == DataStruct -> text "datatype_enum(" <.> pretty (conTag conRepr) <.> text ")"
      _         ->  pretty (conTag conRepr)

genConstructorCreate :: DataInfo -> DataRepr -> ConInfo -> ConRepr -> [(Name,Type)] -> Int -> Asm ()
genConstructorCreate info dataRepr con conRepr conFields scanCount
  = do if (null conFields && dataRepr >= DataNormal)
         then do let declTpName = text "struct" <+> ppName (typeClassName (dataInfoName info)) <.> text "_s" <+> conSingletonName con
                 emitToH $ text "extern" <+> declTpName <.> semi
                 emitToC $ declTpName <+> text "= { header_static( 0, " <.> ppConTag con conRepr dataRepr <.> text ") };"
         else return ()
       emitToH $
          text "static inline" <+> ppName (typeClassName (dataInfoName info)) <+> conCreateNameInfo con
          <.> parameters (conInfoParams con)
          <.> block (
            let nameDoc = ppName (conInfoName con)
                -- tagDoc  = text "datatype_enum(" <.> pretty (conTag conRepr) <.> text ")"
            in case conRepr of
              ConEnum{}      -> text "return" <+> ppConTag con conRepr dataRepr <.> semi
              ConIso{}       -> text "return {" <.> ppDefName (fst (head conFields)) <.> text "};"  -- return as wrapped struct
              ConSingleton{} | dataRepr == DataAsList
                             -> text "return" <+> ppConTag con conRepr dataRepr <.> semi
              _ -> let tmp = text "_con"
                       assignField f (name,tp) = f (ppDefName name) <+> text "=" <+> ppDefName name <.> semi
                   in if (dataReprIsValue dataRepr)
                    then vcat(--[ppName (typeClassName (dataInfoName info)) <+> tmp <.> semi]
                               (if (hasTagField dataRepr)
                                 then [ ppName (typeClassName (dataInfoName info)) <+> tmp <+> text "=" <+>
                                        text "{" <+> ppConTag con conRepr dataRepr <+> text "/* _tag */ }; // zero initializes remaining fields"]
                                      ++ map (assignField (\fld -> tmp <.> text "._cons." <.> ppDefName (conInfoName con) <.> text "." <.> fld)) conFields
                                 else [ ppName (typeClassName (dataInfoName info)) <+> tmp <+> text "= {0}; // zero initializes all fields" ]
                                      ++ map (assignField (\fld -> tmp <.> text "." <.> fld)) conFields
                               )
                               ++ [text "return" <+> tmp <.> semi])
                    else if (null conFields)
                     then text "return cptr_to_datatype(&" <.> conSingletonName con <.> text ");"
                     else vcat([text "struct" <+> nameDoc <.> text "*" <+> tmp <+> text "="
                               <+> text "alloc_tp" <.> tupled [text "struct" <+> nameDoc, pretty scanCount <+> text "/* scan fields */",
                                                               if (dataRepr /= DataOpen)
                                                                then ppConTag con conRepr dataRepr <+> text "/* tag */"
                                                                else text "TAG_OPEN"]
                               <.> semi]
                              ++ (if (dataRepr /= DataOpen) then [] else [tmp <.> text "->_type._tag =" <+> ppConTag con conRepr dataRepr <.> semi ])
                              ++ map (assignField (\fld -> tmp <.> text "->" <.> fld)) conFields
                              ++ [text "return cptr_to_datatype(" <.> tmp <.> text ");"])
          )

genConstructorAccess :: DataInfo -> DataRepr -> ConInfo -> ConRepr -> Asm ()
genConstructorAccess info dataRepr con conRepr
  = if (dataReprIsValue dataRepr)
     then return ()
     else gen
  where
    gen = emitToH $ text "static inline struct" <+> ppName (conInfoName con) <.> text "*" <+> conAsName con
                    <.> parens( ppName (typeClassName (dataInfoName info)) <+> text "x" )
                    <+> block( vcat $
                          [text "assert(" <.> conTestName con <.> text "(x)" <.> text ");"
                          ,text "return" <+> parens (text "struct"  <+> ppName (conInfoName con) <.> text "*") <.> text "datatype_to_cptr(x);"]
                        )




conCreateNameInfo :: ConInfo -> Doc
conCreateNameInfo con = conCreateName (conInfoName con)

conCreateName :: Name -> Doc
conCreateName conName  = ppName (makeHiddenName "new" conName)

conSingletonName :: ConInfo -> Doc
conSingletonName con = ppName (makeHiddenName "singleton" (conInfoName con))

conAsName :: ConInfo -> Doc
conAsName con = ppName (makeHiddenName "as" (conInfoName con))

openTagName :: Name -> Doc
openTagName name = ppName (makeHiddenName "tag" name)

parameters :: [(Name,Type)] -> Doc
parameters []
  = text "(void)"
parameters pars
  = tupled (map param pars)
  where
    param (name,tp) = ppType tp <+> ppName (unqualify name)

-- order constructor fields of constructors with raw field so the regular fields
-- come first to be scanned.
orderConFields :: DataDef -> [(Name,Type)] -> Asm (Maybe ([(Name,Type)],Int))
orderConFields ddef fields
  = visit ([],[],0,0) fields
  where
    visit (rraw, rscan, scanCount, mixCount) []
      = do case ddef of
             DataDefValue raw scan | scanCount > scan
               -> failure $ "Backend.C.FromCore.orderConFields: scan count seems wrong: " ++ show scanCount ++ " vs " ++ show (raw,scan) ++ ", in " ++ show fields
             _ -> if (mixCount > 1)
                   then return Nothing -- multiple fields with mixed raw/scan fields itself
                   else return (Just (reverse rscan ++ reverse rraw, scanCount))
    visit (rraw,rscan,scanCount,mixCount) (field@(name,tp) : fs)
      = do (dd,dataRepr) <- getDataDefRepr tp
           case dd of
             DataDefValue raw scan
               -> let extra = if (dataRepr == DataStruct) then 1 else 0 in -- adjust scan count for added "tag_t" members in structs with multiple constructors
                  if (raw > 0 && scan > 0)
                   then -- mixed raw/scan: put it at the head of the raw fields (there should be only one of these as checked in Kind/Infer)
                        -- but we count them to be sure (and for function data)
                        visit (rraw ++ [field], rscan, scanCount + scan  + extra, mixCount + 1) fs
                   else if (raw > 0)
                         then visit (field:rraw, rscan, scanCount, mixCount) fs
                         else visit (rraw, field:rscan, scanCount + scan + extra, mixCount) fs
             _ -> visit (rraw, field:rscan, scanCount + 1, mixCount) fs


ppVis :: Visibility -> Doc
ppVis _       = empty
ppVis Public  = text "decl_public "
ppVis Private = text "decl_private "

-- | Returns the type constructor class name, for "List" it would be ".List"
typeConClassName :: Name -> Name
typeConClassName name
  = postpend "." (prepend "." name)

conClassName, typeClassName :: Name -> Name
typeClassName name
  = prepend "." name
conClassName name
  = postpend "-ct" name


ppDefName :: Name -> Doc
ppDefName name
  = ppName (unqualify name)

vcatBreak []  = empty
vcatBreak xs  = linebreak <.> vcat xs


hasTagField :: DataRepr -> Bool
hasTagField DataStruct = True
hasTagField rep        = False

-- Value data is not heap allocated and needs no header
dataReprIsValue :: DataRepr -> Bool
dataReprIsValue DataEnum         = True
dataReprIsValue DataIso          = True
dataReprIsValue DataSingleStruct = True
dataReprIsValue DataStruct       = True   -- structs have a tag field though
dataReprIsValue _                = False


genLambda :: [TName] -> Effect -> Expr -> Asm Doc
genLambda params eff body
  = do funName <- newDefVarName "fun"
       toH     <- getDefToHeader
       let newName   = prepend "new-" funName
           funTpName = postpend "_t" funName
           structDoc = text "struct" <+> ppName funTpName
           freeVars  = [(nm,tp) | (TName nm tp) <- tnamesList (freeLocals (Lam params eff body))]
       mbOrd <- orderConFields DataDefNormal freeVars
       let (fields,scanCount) = case mbOrd of
                                 Nothing -> failure ("Backend.C.FromCore.genLambda: free value type variables have mixed raw/scan fields: report this issue please. " ++ show (funTpName))
                                 Just res -> res
           fieldDocs = [ppType tp <+> ppName name | (name,tp) <- fields]
           tpDecl  = text "struct" <+> ppName funTpName <+> block (
                       vcat ([text "struct function_s _fun;"] ++ [ppType tp <+> ppName name <.> semi | (name,tp) <- fields])
                     ) <.> semi

           funSig  = text (if toH then "extern" else "static") <+> ppType (typeOf body)
                     <+> ppName funName <.> tupled ([structDoc <.> text "* _self"] ++ [ppType tp <+> ppName name | (TName name tp) <- params])

           newDef  = funSig <.> semi
                     <-> text (if toH then "static inline" else "static")
                     <+> text "function_t" <+> ppName newName <.> parameters fields <+> block ( vcat (
                       (if (null fields)
                         then [text "static" <+> structDoc <+> text "_self ="
                                <+> braces (braces (text "static_header(1, TAG_FUNCTION), box_cptr(&" <.> ppName funName <.> text ")")) <.> semi
                              ,text "return (&_self._fun);"]
                         else [structDoc <.> text "* _self = alloc_tp" <.> tupled [structDoc, pretty (scanCount + 1) -- +1 for the _fun
                                                                                 , text "TAG_FUNCTION" ] <.> semi
                              ,text "_self->_fun._fun = box_cptr(&" <.> ppName funName <.> text ");"]
                              ++ [text "_self->" <.> ppName name <+> text "=" <+> ppName name <.> semi | (name,_) <- fields]
                              ++ [text "return (&_self->_fun);"])
                     ))


       emitToCurrentDef (vcat [linebreak,text "// lift anonymous function", tpDecl, newDef] <.> linebreak)

       bodyDoc <- genStat (ResultReturn Nothing params) body
       let funDef = funSig <+> block (
                      (if (null fields) then text "UNUSED(_self);"
                        else vcat [ppType tp <+> ppName name <+> text "= _self->" <.> ppName name <.> semi <+> text "/*" <+> pretty tp <+> text "*/"  | (name,tp) <- fields])
                      <-> bodyDoc
                    )
       emitToC funDef  -- TODO: make  static if for a Private definition

       let funNew = ppName newName <.> tupled [ppName name | (name,_) <- fields]
       return funNew

---------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------


ppType :: Type -> Doc
ppType tp
  = case cType tp of
      CBox -> text "box_t"
      CFun _ _ -> text "function_t"
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
         then CPrim "integer_t"
        else if (name == nameTpString)
         then CPrim "string_t"
        else if (name == nameTpChar)
         then CPrim "int32_t"
        else if (name == nameTpInt32)
         then CPrim "int32_t"
        else if (name == nameTpFloat)
         then CPrim "double"
        else if (name == nameTpBool)
         then CPrim "bool"
        else CData (typeClassName name)



---------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------

-- | Applies a return context
getResult :: Result -> Doc -> Doc
getResult result doc
  = if isEmptyDoc doc
      then text ""
      else getResultX result (doc,doc)

getResultX result (puredoc,retdoc)
  = case result of
     ResultReturn _ _  -> text "return" <+> retdoc <.> semi
     ResultAssign n ml -> ( if isWildcard (getName n)
                              then (if (isEmptyDoc puredoc) then puredoc else puredoc <.> semi)
                              else ppName (getName n) <+> text "=" <+> retdoc <.> semi <+> text "/*" <.> pretty (typeOf n) <.> text "*/"
                          ) <-> case ml of
                                  Nothing -> empty
                                  Just l  -> text "goto" <+> ppName l <.> semi

ppVarDecl (TName name tp) = ppType tp <+> ppName name

tryTailCall :: Result -> Expr -> Asm (Maybe Doc)
tryTailCall result expr
  = case expr of
     -- Tailcall case 1
     App (Var n info) args  | ( case result of
                                  ResultReturn (Just m) _ -> m == getName n && infoArity info == (length args)
                                  _                       -> False
                              )
       -> do let (ResultReturn _ params) = result
             stmts <- genOverride params args
             return $ Just $ block $ stmts <-> tailcall

     -- Tailcall case 2
     App (TypeApp (Var n info) _) args | ( case result of
                                            ResultReturn (Just m) _ -> m == getName n && infoArity info == (length args)
                                            _                       -> False
                                          )
       -> do let (ResultReturn _ params) = result
             stmts <- genOverride params args
             return $ Just $ block $ stmts <-> tailcall

     _ -> return Nothing
  where
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
           return $
             linecomment (text "tail call") <-> vcat stmts <-> vcat assigns


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
        -> do docs1 <- genLocalGroups groups
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
                       return (ResultAssign name (Just label),[ppName label <.> colon])
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
          [Branch [pat] [Guard test expr],_]
            -> isExprTrue test && isSingleTestPat pat
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
  = genPattern doTest (zip exprDocs patterns) (genGuards result guards)

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
              return (vcat gddoc <-> text "if" <+> parens gdoc <+> block (sdoc))


genPattern :: Bool -> [(Doc,Pattern)] -> Asm Doc -> Asm Doc
genPattern doTest [] genBody
  = genBody
genPattern doTest dpatterns genBody
  = do (testss,localss,nextPatternss) <- fmap (unzip3 . concat) $
                                           mapM (genPatternTest doTest) dpatterns
       let tests  = concat testss
           locals = concat localss
           nextPatterns = concat nextPatternss

       ndoc <- genPattern doTest nextPatterns genBody
       if (null tests)
        then return (vcat (locals ++ [ndoc]))
        else return (text "if" <+> parens (hcat (punctuate (text "&&") tests))
                      <+> block (vcat (locals ++ [ndoc])))

genPatternTest :: Bool -> (Doc,Pattern) -> Asm [([Doc],[Doc],[(Doc,Pattern)])]
genPatternTest doTest (exprDoc,pattern)
  = let test xs = if doTest then xs else [] in
    case pattern of
      PatWild -> return []
      PatVar tname pattern | hiddenNameStartsWith (getName tname) "unbox"
        -> do let after = ppType (typeOf tname) <+> ppDefName (getName tname) <+> text "=" <+> text "unbox_" <.> ppType (typeOf tname) <.> parens exprDoc <.> semi
                  next  = genNextPatterns (\self fld -> self) (ppDefName (getName tname)) (typeOf tname) [pattern]
              return [([],[after],next)]
      PatVar tname pattern
        -> do let after = ppType (typeOf tname) <+> ppDefName (getName tname) <+> text "=" <+> exprDoc <.> semi
                  next  = genNextPatterns (\self fld -> self) (ppDefName (getName tname)) (typeOf tname) [pattern]
              return [([],[after],next)]
      PatLit lit
        -> return [(test [exprDoc <+> text "==" <+> ppLit lit],[],[])]
      PatCon tname patterns repr targs exists tres info
        -> trace ("patCon: " ++ show info ++ ","  ++ show tname ++ ", " ++ show repr) $
           case repr of
                 ConEnum{}  | conInfoName info == nameTrue
                    -> return [(test [exprDoc],[],[])]
                 ConEnum{} | conInfoName info == nameFalse
                    -> return [(test [text "!" <.> parens exprDoc],[],[])]
                 _  -> let dataRepr = conDataRepr repr
                       in if (dataReprIsValue dataRepr)
                           then valTest tname info dataRepr
                           else conTest info
        where
          valTest :: TName -> ConInfo -> DataRepr -> Asm [([Doc],[Doc],[(Doc,Pattern)])]
          valTest conName conInfo dataRepr
            = --do let next = genNextPatterns (exprDoc) (typeOf tname) patterns
              --   return [(test [conTestName conInfo <.> parens exprDoc],[assign],next)]
              do let selectOp = case dataRepr of
                                  DataStruct -> "._cons." ++ show (ppDefName (getName conName)) ++ "."
                                  _          -> "."
                     next = genNextPatterns (\self fld -> self <.> text selectOp <.> fld) exprDoc (typeOf tname) patterns
                 return [(test [conTestName conInfo <.> parens exprDoc],[],next)]

          conTest conInfo
            = do local <- newVarName "con"
                 let next    = genNextPatterns (\self fld -> self <.> text "->" <.> fld) (ppDefName local) (typeOf tname) patterns
                     typeDoc = text "struct" <+> ppName (conInfoName conInfo) <.> text "*"
                     assign  = typeDoc <+> ppDefName local <+> text "=" <+> conAsName conInfo <.> parens exprDoc <.> semi
                 return [(test [conTestName conInfo <.> parens exprDoc],[assign],next)]


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
                             concatMap genNextPattern (zip [if nameIsNil name then newFieldName i else name  | (name,i) <- zip (map fst args) [1..]]
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

     _ -> failure ("Backend.C.FromCore.genExpr: invalid expression:\n" ++ show expr)


genExprs :: [Expr] -> Asm ([Doc],[Doc])
genExprs exprs
  = do xs <- mapM genExpr exprs
       let (declss,docs) = unzip xs
       return (concat declss, docs)

-- | Introduces an additional let binding in core if necessary
--   The expression in the result is guaranteed to be a Var afterwards
genVarBinding :: Expr -> Asm (Doc, TName)
genVarBinding expr
  = case expr of
      Var tn _ -> return $ (empty, tn)
      _        -> do name <- newVarName "x"
                     let tname = TName name (typeOf expr)
                     doc  <- genStat (ResultAssign tname Nothing) expr
                     return (ppVarDecl tname <.> semi <-> doc, tname)


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
            Just (_,_,argTps,eff,resTp) 
              -> do argNames <- mapM newVarName ["x" ++ show i | i <- [1..length argTps]]
                    let tnames = [TName name tp | (name,(_,tp)) <- zip argNames argTps]
                        body   = (App expr [Var name InfoNone | name <- tnames])
                    genLambda tnames eff body
            Nothing   
              -> case info of
                   InfoExternal formats -> genInlineExternal name formats []
                   _ -> return (ppName (getName name))
     Con name info
       -> return (conCreateName (getName name) <.> text "()")
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
         Nothing  -> genAppNormal f args
         
         
genAppNormal :: Expr -> [Expr] -> Asm ([Doc],Doc)
genAppNormal f args
  = do (decls,argDocs) <- genExprs args
       case extractExtern f of
         -- known external
         Just (tname,formats)
           -> do (edecls,doc) <- genExprExternal tname formats argDocs
                 return ((edecls ++ decls), doc)
         Nothing
           -> case f of 
               -- constructor
               Con tname repr
                 -> return (decls,conCreateName (getName tname) <.> tupled argDocs)
               -- call to known function
               Var tname info | isQualified (getName tname)
                 -> return (decls,ppName (getName tname) <.> tupled argDocs)         
               -- call unknown function_t
               _ -> do (fdecls,fdoc) <- case f of 
                                          Var tname info -> return ([], ppName (getName tname)) -- prevent lambda wrapping recursively
                                          _ -> genExpr f
                       let (cresTp,cargTps) = case splitFunScheme (typeOf f) of 
                                               Just (_,_,argTps,_,resTp) 
                                                 -> (ppType resTp, tupled (map (ppType . snd) argTps))
                       return (fdecls ++ decls, text "function_call" <.> tupled [cresTp,cargTps,fdoc,tupled argDocs])


genAppSpecial :: Expr -> [Expr] -> Asm (Maybe Doc)
genAppSpecial f args 
  = case (f,args) of
      (Var tname _, [Lit (LitInt i)]) | getName tname == nameInt32 && isSmallInt32 i
        -> return (Just (genLitInt32 i))
      _ -> case extractExtern f of
             Just (tname,formats)
               -- inline external
               -> case args of
                   [Lit (LitInt i)] | getName tname == nameInt32 && isSmallInt32 i
                     -> return (Just (parens (text "(int32_t)" <.> pretty i)))
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

-- not fully applied external gets wrapped in a function
genWrapExternal :: TName -> [(Target,String)] -> Asm Doc
genWrapExternal tname formats
  = do let n = snd (getTypeArities (typeOf tname))
       vs  <- genVarNames n
       (decls,doc) <- genExprExternal tname formats vs
       return $ error ("Backend.C.FromCore.genWrapExternal: TODO: " ++ show (vcat (decls++[doc])))
                 -- parens (text "function" <.> tupled vs <+> block (vcat (decls ++ [text "return" <+> doc <.> semi])))

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
genExprExternal tname formats argDocs0 | getName tname == nameBox || getName tname == nameUnbox
  = let isBox = (getName tname == nameBox)
        tp    = case typeOf tname of
                  TFun [(_,fromTp)] _ toTp -> if (isBox) then fromTp else toTp
        fun   = text (if (isBox) then "box_" else "unbox_") <.> ppType tp
    in return ([], fun <.> tupled argDocs0)

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
              in assertion ("illegal index in external: " ++ show tname ++ "("++k++"): index: " ++ show i) (i < n) $
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
                      ("__std_core._unsupported_external(\"" ++ (show tname) ++ "\")")
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
      App (Var _ (InfoExternal _)) args -> all isPureExpr args  -- yielding() etc.      
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
      Var _ _ -> True
      Con _ _ -> True
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
               , inStatement       :: Bool                    -- | for generating correct function declarations in strict mode
               }

data Result = ResultReturn (Maybe Name) [TName] -- first field carries function name if not anonymous and second the arguments which are always known
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


getDataDefRepr :: Type -> Asm (DataDef,DataRepr)
getDataDefRepr tp
  = case extractDataDefType tp of
      Nothing -> return (DataDefNormal,DataNormal)
      Just name -> do newtypes <- getNewtypes
                      case newtypesLookupAny name newtypes of
                        Nothing -> failure $ "Backend.C.FromCore.getDataInfo: cannot find type: " ++ show name
                        Just di -> return (dataInfoDef di, fst (getDataRepr di))

extractDataDefType tp
  = case expandSyn tp of
      TApp t _      -> extractDataDefType t
      TForall _ _ t -> extractDataDefType t
      TCon tc       -> Just (typeConName tc)
      _             -> Nothing


---------------------------------------------------------------------------------
-- Pretty printing
---------------------------------------------------------------------------------

ppLit :: Lit -> Doc
ppLit lit
    = case lit of
      LitInt i    -> if (isSmallInt(i))
                      then text "integer_from_small" <.> parens (pretty i)
                     else if (isSmallInt32(i)) 
                      then text "integer_from_int" <.> parens (pretty i)
                      else text "integer_from_str" <.> parens (dquotes (pretty i))
      LitChar c   -> let i = fromEnum c
                     in if (c >= ' ' || c <= '~') 
                         then text (show c)
                         else text ("0x" ++ showHex 4 (fromEnum c))
      LitFloat d  -> text (showsPrec 20 d "")
      LitString s -> dquotes (hcat (map escape s))
    where
      escape c
        = if (c < ' ')
           then (if (c=='\n') then text "\\n"
                 else if (c == '\r') then text "\\r"
                 else if (c == '\t') then text "\\t"
                 else text "\\x" <.> text (showHex 2 (fromEnum c)))
          else if (c <= '~')
           then (if (c == '\"') then text "\\\""
                 else if (c=='\'') then text "\\'"
                 else if (c=='\\') then text "\\\\"
                 else if (c=='?')  then text "\\?"  -- to avoid accidental trigraphs
                 else char c)
          else if (fromEnum c <= 0xFF)
           then text "\\x" <.> text (showHex 2 (fromEnum c))
          -- TODO: encode to UTF8-0 ourselves and don't use \u and \U                 
          else if (fromEnum c <= 0xFFFF)
           then text "\\u" <.> text (showHex 4 (fromEnum c))
          else if (fromEnum c > 0x10FFFF)
           then text "\\uFFFD"  -- error instead?
           else text "\\U" <.> text (showHex 8 (fromEnum c))

genLitInt32 :: Integer -> Doc
genLitInt32 i 
  = parens (text "(int32_t)" <.> pretty i)

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


ppName :: Name -> Doc
ppName name
  = if isQualified name
     then ppModName (qualifier name) <.> text "_" <.> encode False (unqualify name)
     else encode False name

ppQName :: Name -> Name -> Doc
ppQName modName name
  = if (modName == qualifier name)   -- We need to qualify always since otherwise we may clash with local variables. i.e. fun f( x : int ) { Main.x( x ) }
     then ppName (unqualify name)
     else ppName name

ppModName :: Name -> Doc
ppModName name
  = text "__" <.> encode True (name)

encode :: Bool -> Name -> Doc
encode isModule name
  = let s = show name
    in if (isReserved s)
         then text ("__" ++ s)
         else text ( (asciiEncode isModule s))

isReserved :: String -> Bool
isReserved s
  = if (not $ null s) && (head s == 'T') && all isDigit (tail s)
      then True
      else s `S.member` reserved

reserved :: S.Set String
reserved
  = S.fromList $ -- JavaScript pseudo-keywords
    [ "prototype"
    , "toString"
    , "arguments"
    , "eval"
    ]
    ++ -- word literals
    [ "null"
    , "Infinity"
    , "NaN"
    ]
    ++ -- JavaScript keywords
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
    ++ -- special globals
    [ "window"
    , "document"
    , "process"
    , "exports"
    , "module"
    , "Date"
    , "Error"
    ]

block :: Doc -> Doc
block doc
  = text "{" <--> tab doc <--> text "}"

tblock :: Doc -> Doc -> Doc
tblock tpDoc doc
  = text "{" <+> tpDoc <--> tab doc <--> text "}"


tcoBlock :: Doc -> Doc -> Doc
tcoBlock tpDoc doc
  = tblock tpDoc (text "_tailcall:" <-> doc)

tailcall :: Doc
tailcall  = text "goto _tailcall;"

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
  = parameters [(name,tp) | TName name tp <- tnames]

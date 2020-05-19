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
  = case runAsm uniq (Env moduleName penv externalNames newtypes False) (genModule mbMain core) of
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

genLocalGroups :: [DefGroup] -> Asm Doc
genLocalGroups dgs
  = do docs <- mapM genLocalGroup dgs
       return (vcat docs)

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
        ppType (typeOf body) <+> ppName name <.> tupled (map ppParam params)

ppParam (TName name tp) = ppType tp <+> ppName name

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
    in tryFun defBody                                  
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
           emit $ linebreak 
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
       conInfos <- mapM (\(conInfo,conRepr) -> do (fields,scanCount0) <- orderConFields (dataInfoDef info) (conInfoParams conInfo)
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
       emitToH empty
       mapM_ (genConstructor info dataRepr) conInfos

  where
    ppEnumCon (con,conRepr)
      = ppName (conInfoName con)  -- <+> text "= datatype_enum(" <.> pretty (conTag conRepr) <.> text ")"

    ppStructConField con
      = text "struct" <+> ppName ((conInfoName con)) <+> ppName (unqualify (conInfoName con)) <.> semi


genConstructorType :: DataInfo -> DataRepr -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Asm ()
genConstructorType info dataRepr (con,conRepr,conFields,scanCount) =
  case conRepr of
    ConEnum _ _
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
          text "static inline" <+> ppName (typeClassName (dataInfoName info)) <+> conCreateName con
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
                                        text "{" <+> ppConTag con conRepr dataRepr <+> text "}; // zero initializes remaining fields"]
                                      ++ map (assignField (\fld -> tmp <.> text "._cons." <.> ppDefName (conInfoName con) <.> text "." <.> fld)) conFields
                                 else [ ppName (typeClassName (dataInfoName info)) <+> tmp <+> text "= {0}; // zero initializes all fields" <+>
                                        text "{" <+> ppConTag con conRepr dataRepr <+> text "};"]
                                      ++ map (assignField (\fld -> tmp <.> text "." <.> fld)) conFields
                               )
                               ++ [text "return" <+> tmp <.> semi])
                    else if (null conFields)
                     then text "return datatype_ptr(&" <.> conSingletonName con <.> text ");"
                     else vcat([text "struct" <+> nameDoc <.> text "*" <+> tmp <+> text "="
                               <+> text "alloc_tp" <.> tupled [text "struct" <+> nameDoc, pretty scanCount, 
                                                               if (dataRepr /= DataOpen) then ppConTag con conRepr dataRepr else text "TAG_OPEN"]
                               <.> semi]
                              ++ (if (dataRepr /= DataOpen) then [] else [tmp <.> text "->_type._tag =" <+> ppConTag con conRepr dataRepr <.> semi ])
                              ++ map (assignField (\fld -> tmp <.> text "->" <.> fld)) conFields
                              ++ [text "return datatype_ptr(" <.> tmp <.> text ");"])
          )

genConstructorAccess :: DataInfo -> DataRepr -> ConInfo -> ConRepr -> Asm ()
genConstructorAccess info dataRepr con conRepr
  = case conRepr of
      ConSingle{}    -> gen
      ConAsCons{}    -> gen
      ConNormal{}    -> gen
      ConOpen{}      -> gen
      _              -> return ()
  where
    gen = emitToH $ text "static inline struct" <+> ppName (conInfoName con) <.> text "*" <+> conAsName con
                    <.> parens( ppName (typeClassName (dataInfoName info)) <+> text "x" )
                    <+> block( vcat $
                          [text "assert(" <.> conTestName con <.> text "(x)" <.> text ");"
                          ,text "return" <+> parens (text "struct"  <+> ppName (conInfoName con) <.> text "*") <.> text "x;"]
                        )


conCreateName :: ConInfo -> Doc
conCreateName con = ppName (makeHiddenName "new" (conInfoName con))

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
orderConFields :: DataDef -> [(Name,Type)] -> Asm ([(Name,Type)],Int)
orderConFields ddef fields
  = visit ([],[],0) fields
  where
    visit (rraw, rscan, scanCount) []
      = do case ddef of
             DataDefValue raw scan | scanCount > scan
               -> failure $ "Backend.C.FromCore.orderConFields: scan count seems wrong: " ++ show scanCount ++ " vs " ++ show (raw,scan) ++ ", in " ++ show fields
             _ -> return (reverse rscan ++ reverse rraw, scanCount)
    visit (rraw,rscan,scanCount) (field@(name,tp) : fs)
      = do (dd,dataRepr) <- getDataDefRepr tp
           case dd of
             DataDefValue raw scan
               -> let extra = if (dataRepr == DataStruct) then 1 else 0 in -- adjust scan count for added "tag_t" members in structs with multiple constructors
                  if (raw > 0 && scan > 0)
                   then -- mixed raw/scan: put it at the head of the raw fields (there should be only one of these as checked in Kind/Infer)
                        visit (rraw ++ [field], rscan, scanCount + scan  + extra) fs
                   else if (raw > 0)
                         then visit (field:rraw, rscan, scanCount) fs
                         else visit (rraw, field:rscan, scanCount + scan + extra) fs
             _ -> visit (rraw, field:rscan, scanCount + 1) fs


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
                                  Just l  -> text "break" <+> ppName l <.> semi

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
        do (stmts, varNames) <- do args' <- mapM tailCallArg args
                                   bs    <- mapM genVarBinding args'
                                   return (unzip bs)
           docs1             <- mapM genTName params
           docs2             <- mapM genTName varNames
           let assigns    = map (\(p,a)-> if p == a
                                            then debugComment ("genOverride: skipped overriding `" ++ (show p) ++ "` with itself")
                                            else debugComment ("genOverride: preparing tailcall") <.> p <+> text "=" <+> a <.> semi
                                ) (zip docs1 docs2)
           return $
             linecomment (text "tail call") <-> vcat stmts <-> vcat assigns

    -- if local variables are captured inside a tailcalling function argument,
    -- we need to capture it by value (instead of reference since we will overwrite the local variables on a tailcall)
    -- we do this by wrapping the argument inside another function application.
    tailCallArg :: Expr -> Asm Expr
    tailCallArg expr
      = let captured = filter (not . isQualified . getName) $ tnamesList $ capturedVar expr
        in if (null captured)
            then return expr
            else -- trace ("Backend.JavaScript.FromCore.tailCall: capture: " ++ show captured ++ ":\n" ++ show expr) $
                 do ns <- mapM (newVarName . show) captured
                    let cnames = [TName cn tp | (cn,TName _ tp) <- zip ns captured]
                        sub    = [(n,Var cn InfoNone) | (n,cn) <- zip captured cnames]
                    return $ App (Lam cnames typeTotal (sub |~> expr)) [Var arg InfoNone | arg <- captured]

    capturedVar :: Expr -> TNames
    capturedVar expr
      = case expr of
          Lam _ _  _  -> fv expr  -- we only care about captures inside a lambda
          Let bgs body -> S.unions (capturedVar body : map capturedDefGroup bgs)
          Case es bs   -> S.unions (map capturedVar es ++ map capturedBranch bs)
          App f args   -> S.unions (capturedVar f : map capturedVar args)
          TypeLam _ e  -> capturedVar e
          TypeApp e _  -> capturedVar e
          _            -> S.empty

    capturedDefGroup bg
      = case bg of
          DefRec defs  -> S.difference (S.unions (map capturedDef defs)) (bv defs)
          DefNonRec def-> capturedDef def

    capturedDef def
      = capturedVar (defExpr def)

    capturedBranch (Branch pat grds)
      = S.difference (S.unions (map capturedGuard grds)) (bv pat)

    capturedGuard (Guard test expr)
      = S.union (capturedVar test) (capturedVar expr)

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


genExprStat result expr
  = case expr of
      -- If expression is inlineable, inline it
      _  | isInlineableExpr expr
        -> do exprDoc <- genInline expr
              return (getResult result exprDoc)

      Case exprs branches
         -> do (docs, scrutinees) <- fmap unzip $ mapM (\e-> if isInlineableExpr e && isTypeBool (typeOf e)
                                                               then do d       <- genInline e
                                                                       return (text "", d)
                                                               else do (sd,vn) <- genVarBinding e
                                                                       vd      <- genTName vn
                                                                       return (sd, vd)
                                                       ) exprs
               doc                <- genMatch result scrutinees branches
               return (vcat docs <-> doc)

      Let groups body
        -> do doc1 <- genLocalGroups groups
              doc2 <- genStat result body
              return (doc1 <-> doc2)

      -- Handling all other cases
      _ -> do (statDoc,exprDoc) <- genExpr expr
              return (statDoc <-> getResult result exprDoc)

-- | Generates a statement for a match expression regarding a given return context
genMatch :: Result -> [Doc] -> [Branch] -> Asm Doc
genMatch result scrutinees branches
  = fmap (debugWrap "genMatch") $ do
    case branches of
        []  -> fail ("Backend.JavaScript.FromCore.genMatch: no branch in match statement: " ++ show(scrutinees))
        [b] -> fmap snd $ genBranch True result scrutinees b

       -- Special handling of return related cases - would be nice to get rid of it
        [ Branch [p1] [Guard t1 (App (Var tn _) [r1])], Branch [p2] [Guard t2 e2] ]
            | getName tn == nameReturn &&
              isPat True p1 && isPat False p2 &&
              isExprTrue t1 && isExprTrue t2
           -> case e2 of
                 App (Var tn _) [r2]
                    | getName tn == nameReturn
                   -> do (stmts1, expr1) <- genExpr r1
                         (stmts2, expr2) <- genExpr r2
                         return $ text "if" <.> parens (head scrutinees) <+> block (stmts1 <-> text "return" <+> expr1 <.> semi)
                                                        <-> text "else" <+> block (stmts2 <-> text "return" <+> expr2 <.> semi)
                 _ -> do (stmts1,expr1) <- genExpr r1
                         (stmts2,expr2) <- genExpr e2
                         return $
                           (text "if" <.> parens (head scrutinees) <+> block (stmts1 <-> text "return" <+> expr1 <.> semi))
                            <-->
                           (stmts2 <-> getResultX result (if (isExprUnit e2) then text "" else expr2,expr2))
{-
        [Branch [p1] [Guard t1 e1], Branch [p2] [Guard t2 e2]]
           | isExprTrue t1
          && isExprTrue t2
          && isInlineableExpr e1
          && isInlineableExpr e2
          -> do modName <- getModule
                let nameDoc = head scrutinees
                let test    = genTest modName (nameDoc, p1)
                if (isExprTrue e1 && isExprFalse e2)
                  then return $ getResult result $ parens (conjunction test)
                  else do doc1 <- withNameSubstitutions (getSubstitutions nameDoc p1) (genInline e1)
                          doc2 <- withNameSubstitutions (getSubstitutions nameDoc p2) (genInline e2)
                          return $ debugWrap "genMatch: conditional expression"
                                 $ getResult result
                                 $ parens (conjunction test) <+> text "?" <+> doc1 <+> text ":" <+> doc2
-}
        bs
           | all (\b-> length (branchGuards   b) == 1) bs
          && all (\b->isExprTrue $ guardTest $ head $ branchGuards b) bs
          -> do xs <- mapM (withStatement . genBranch True result scrutinees) bs
                return $  debugWrap "genMatch: guard-free case"
                       $  hcat  ( map (\(conds,d)-> text "if" <+> parens (conjunction conds)
                                                             <+> block d <-> text "else "
                                      ) (init xs)
                                )
                      <.> block (snd (last xs))

        _ -> do (labelF, result') <- case result of
                      ResultReturn _ _        -> return (id, result)
                      ResultAssign n (Just _) -> return (id, result) -- wohoo, we can jump out from deep in!
                      ResultAssign n Nothing  -> return ( \d-> text "match: " <.> block d
                                                        , ResultAssign n (Just $ newName "match")
                                                        )
                bs <- mapM (withStatement . genBranch False result' scrutinees) (init branches)
                b  <-      (withStatement . genBranch True  result' scrutinees) (last branches)
                let ds = map (\(cds,stmts)-> if null cds
                                                  then stmts
                                                  else text "if" <+> parens (conjunction cds)
                                                                <+> block stmts
                             ) bs
                let d  = snd b
                return $ debugWrap "genMatch: regular case"
                       $ labelF (vcat ds <-> d)
  where
    -- | Generates a statement for a branch with given return context
    genBranch :: Bool -> Result -> [Doc] -> Branch -> Asm ([ConditionDoc], Doc)
    -- Regular catch-all branch generation
    genBranch lastBranch result tnDocs branch@(Branch patterns guards)
      = do modName <- getModule
           let substs     = concatMap (uncurry getSubstitutions) (zip tnDocs patterns)
           let conditions = concatMap (genTest modName) (zip tnDocs patterns)
           -- let se         = withNameSubstitutions substs
           let decls = [ppVarDecl tname <+> text "=" <+> doc | (tname,doc) <- substs]
           
           gs <- mapM (genGuard False      result) (init guards)
           g  <-      (genGuard lastBranch result) (last guards)
           return (conditions, debugWrap ("genBranch") $ vcat (decls ++ gs) <-> g)

    getSubstitutions :: Doc -> Pattern -> [(TName, Doc)]
    getSubstitutions nameDoc pat
          = case pat of
              PatCon tn args repr _ _ _ info
                -> -- trace ("pattern: " ++ show tn ++ ": " ++ show args ++ ",  " ++ show info) $
                   concatMap (\(pat',fn)-> getSubstitutions
                                             (nameDoc <.> (if (getName tn == nameOptional || isConIso repr) then empty else (text "."  <.> fn)))
                                             pat'
                            )
                            (zip args (map (ppName . fst) (conInfoParams info)) )
              PatVar tn pat'      -> (tn, nameDoc):(getSubstitutions nameDoc pat')
              PatWild             -> []
              PatLit lit          -> []

    genGuard  :: Bool -> Result -> Guard -> Asm Doc
    genGuard lastBranchLastGuard result (Guard t expr)
      = do (testSt, testE) <- genExpr t
           let result'      = case result of
                               ResultAssign n _ | lastBranchLastGuard -> ResultAssign n Nothing
                               _                                      -> result
           exprSt          <- genStat result' expr
           return $ if isExprTrue t
                      then exprSt
                      else testSt <-> text "if" <+> parens testE <.> block exprSt

    -- | Generates a list of boolish expression for matching the pattern
    genTest :: Name -> (Doc, Pattern) -> [Doc]
    genTest modName (scrutinee,pattern)
      = case pattern of
              PatWild ->  []
              PatVar _ pat
                -> genTest modName (scrutinee,pat)
              PatLit lit
                -> [scrutinee <+> text "===" <+> ppLit lit]
              PatCon tn fields repr _ _ _ info
                | getName tn == nameTrue
                -> [scrutinee]
                | getName tn == nameFalse
                -> [text "!" <.> scrutinee]
                | otherwise
                -> case repr of
                     ConEnum _ tag
                       -> [debugWrap "genTest: enum"      $ scrutinee <+> text "===" <+> int tag]
                     ConSingleton{} -- the only constructor without fields (=== null)
                       -> [debugWrap "genTest: singleton" $ scrutinee <+> text "== null"]  -- use == instead of === since undefined == null (for optional arguments)
                     ConSingle{} -- always succeeds, but need to test the fields
                       -> concatMap
                            (\(field,fieldName) -> genTest modName (
                                                    debugWrap ("genTest: single: " ++ show field ++ " -> " ++ show fieldName) $
                                                   scrutinee <.> dot <.> fieldName, field) )
                            (zip fields (map (ppName . fst) (conInfoParams info)) )

                     ConIso{} -- alwasy success
                       -> []
                     ConStruct{}
                       -> fail "Backend.JavaScript.FromCore.genTest: encountered ConStruct, which is not supposed to happen"
                     ConAsCons{}
                       | getName tn == nameOptional
                       -> [scrutinee <+> text "!== undefined"] ++ concatMap (\field -> genTest modName (scrutinee,field) ) fields
                       | otherwise
                       -> let conTest    = debugWrap "genTest: asCons" $ scrutinee <+> text "!= null" -- use === instead of == since undefined == null (for optional arguments)
                              fieldTests = concatMap
                                             (\(field,fieldName) -> genTest modName (scrutinee <.> dot <.> fieldName, field) )
                                             (zip fields (map (ppName . fst) (conInfoParams info)) )
                          in (conTest:fieldTests)
                     _ -> let conTest    = debugWrap "genTest: normal" $ scrutinee <.> dot <.> tagField <+> text "===" <+> getConTag modName info repr
                              fieldTests  =  concatMap
                                             (\(field,fieldName) -> genTest modName (debugWrap ("genTest: normal: " ++ show field ++ " -> " ++ show fieldName) $ scrutinee <.> dot <.> fieldName, field) )
                                             ( zip fields (map (ppName . fst) (conInfoParams info)) )
                          in (conTest:fieldTests)


    
    -- | Takes a list of docs and concatenates them with logical and
    conjunction :: [Doc] -> Doc
    conjunction []
      = text "true"
    conjunction docs
      = hcat (intersperse (text " && ") docs)

getConTag modName coninfo repr
  = case repr of
      ConOpen{} -> -- ppLit (LitString (show (openConTag (conInfoName coninfo))))
                   let name = toOpenTagName (conInfoName coninfo)
                   in ppName (if (qualifier name == modName) then unqualify name else name)
      _ -> int (conTag repr)



---------------------------------------------------------------------------------
-- Expressions that produce statements on their way
---------------------------------------------------------------------------------

-- | Generates javascript statements and a javascript expression from core expression
genExpr :: Expr -> Asm (Doc,Doc)
genExpr expr
  = -- trace ("genExpr: " ++ show expr) $
    case expr of
     -- check whether the expression is pure an can be inlined
     _  | isInlineableExpr expr
       -> do doc <- genInline expr
             return (empty,doc)

     TypeApp e _ -> genExpr e
     TypeLam _ e -> genExpr e

     -- handle not inlineable cases
     {-
     App (TypeApp (Con name repr) _) [arg]  | getName name == nameOptional || isConIso repr
       -> genExpr arg
     App (Con _ repr) [arg]  | isConIso repr
       -> genExpr arg
     -}
     App (Var tname _) [Lit (LitInt i)] | getName tname == nameInt32 && isSmallInt i
       -> return (empty, pretty i)
     App f args
       -> case extractList expr of
              -- inline list
              Just (xs,tl) -> genList xs tl
              Nothing -> case extractExtern f of
               Just (tname,formats)
                 -- inline external
                 -> case args of
                     [Lit (LitInt i)] | getName tname == nameInt32 && isSmallInt i
                       -> return (empty,pretty i)
                     _ -> -- genInlineExternal tname formats argDocs
                          do (decls,argDocs) <- genExprs args                             
                             (edecls,doc) <- genExprExternal tname formats argDocs
                             if (getName tname == nameReturn)
                              then return (vcat (decls ++ edecls ++ [doc <.> semi]), text "")
                              else return (vcat (decls ++ edecls), doc)
               Nothing
                -- normal application
                -> do lsDecls <- genExprs (f:args)
                      let (decls,fdoc:argDocs) = lsDecls                          
                      return (vcat decls, fdoc <.> tupled argDocs)

     Let groups body
       -> do decls1       <- genLocalGroups groups
             (decls2,doc) <- genExpr body
             return (decls1 <-> decls2, doc)

     Case _ _
       -> do (doc, tname) <- genVarBinding expr
             nameDoc <- genTName tname
             return (doc, nameDoc)

     _ -> failure ("JavaScript.FromCore.genExpr: invalid expression:\n" ++ show expr)


extractList :: Expr -> Maybe ([Expr],Expr)
extractList e
  = let (elems,tl) = extract [] e
    in if (length elems > 10) -- only use inlined array for larger lists
        then Just (elems,tl)
        else Nothing
  where
    extract acc expr
      = case expr of
          App (TypeApp (Con name info) _) [hd,tl]  | getName name == nameCons
            -> extract (hd:acc) tl
          _ -> (reverse acc, expr)

genList :: [Expr] -> Expr -> Asm (Doc,Doc)
genList elems tl
  = do (decls,docs) <- genExprs elems
       (tdecl,tdoc) <- genExpr tl
       return (vcat (decls ++ [tdecl]), text "$std_core.vlist" <.> tupled [list docs, tdoc])

{-
genExternalExpr :: TName -> String -> [Expr] -> Asm (Doc,Doc)
genExternalExpr tname format args
  | getName tname == nameReturn
  = do (statDoc,exprDoc) <- genExpr (head args)
       return (statDoc <-> text "return" <+> exprDoc <.> semi <.> debugComment "premature return statement (2)"
              , text "") -- emptyness of doc is important! no other way to tell to not generate assignment/return/whatever!
  | otherwise
  = do (statDocs,argDocs) <- genExprs args
       doc <- genExternal tname format argDocs
       return ( debugComment "<genExternalExpr.stmt>" <.> vcat statDocs <.> debugComment "</genExternalExpr.stmt>"
              , debugComment "<genExternalExpr.expr>" <.> doc           <.> debugComment "</genExternalExpr.expr>"
              )
-}

genExprs :: [Expr] -> Asm ([Doc],[Doc])
genExprs exprs
  = do xs <- mapM genExpr exprs
       return (unzip xs)

-- | Introduces an additional let binding in core if necessary
--   The expression in the result is guaranteed to be a Var afterwards
genVarBinding :: Expr -> Asm (Doc, TName)
genVarBinding expr
  = case expr of
      Var tn _ -> return $ (empty, tn)
      _        -> do name <- newVarName "x"
                     let tname = TName name (typeOf expr)
                     doc  <- genStat (ResultAssign tname Nothing) expr
                     return ( ppVarDecl tname <.> semi <//> doc, tname )

---------------------------------------------------------------------------------
-- Pure expressions
---------------------------------------------------------------------------------

genPure   :: Expr -> Asm Doc
genPure expr
  = case expr of
     TypeApp e _ -> genPure e
     TypeLam _ e -> genPure e
     Var name (InfoExternal formats)
       -> genWrapExternal name formats  -- unapplied inlined external: wrap as function
     Var name info
       -> genTName name
     Con name repr
       -> genTName name
     Lit l
       -> return $ ppLit l
     Lam params eff body
       -> do args    <- mapM genCommentTName params
             bodyDoc <- genStat (ResultReturn Nothing params) body
             return (text "function" <.> tupled args <+> block bodyDoc)
     _ -> failure ("JavaScript.FromCore.genPure: invalid expression:\n" ++ show expr)

isPat :: Bool -> Pattern -> Bool
isPat b q
  = case q of
      PatWild     -> False
      PatLit _    -> False
      PatVar _ q' -> isPat b q'
      PatCon {}   -> getName (patConName q) == if b then nameTrue else nameFalse

-- | Generates an effect-free javasript expression
--   NOTE: Throws an error if expression is not guaranteed to be effectfree
genInline :: Expr -> Asm Doc
genInline expr
  = case expr of
      _  | isPureExpr expr -> genPure expr
      TypeLam _ e -> genInline e
      TypeApp e _ -> genInline e
      App f args
        -> do argDocs <- mapM genInline args
              case extractExtern f of
                Just (tname,formats)
                  -> case args of
                       [Lit (LitInt i)] | getName tname == nameInt32 && isSmallInt i
                         -> return (pretty i)
                       _ -> genInlineExternal tname formats argDocs
                Nothing
                  -> case (f,args) of
                       ((Var tname _),[Lit (LitInt i)]) | getName tname == nameInt32 && isSmallInt i
                         -> return (pretty i)
                       _ -> do fdoc <- genInline f
                               return (fdoc <.> tupled argDocs)

      _ -> failure ("Backend.C.FromCore.genInline: invalid expression:\n" ++ show expr)

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

genTName :: TName -> Asm Doc
genTName tname
  = do env <- getEnv
       case lookup tname (substEnv env) of
          Nothing -> genName (getName tname)
          Just d  -> return d

genName :: Name -> Asm Doc
genName name
  = if (isQualified name)
      then do modname <- getModule
              if (qualifier name == modname)
               then return (ppName (unqualify name))
               else return (ppName name)
      else return (ppName name)

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
      App (Var v _) [arg] | getName v `elem` [nameBox,nameUnbox] -> isInlineableExpr arg
      App (Var _ (InfoExternal _)) args -> all isPureExpr args
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
      Var n _  | getName n == nameReturn -> False -- make sure return will never be inlined
               | otherwise               -> True
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
               }

data Env = Env { moduleName        :: Name                    -- | current module
               , prettyEnv         :: Pretty.Env              -- | for printing nice types
               , substEnv          :: [(TName, Doc)]          -- | substituting names
               , newtypes          :: Newtypes
               , inStatement       :: Bool                    -- | for generating correct function declarations in strict mode
               }

data Result = ResultReturn (Maybe Name) [TName] -- first field carries function name if not anonymous and second the arguments which are always known
            | ResultAssign TName (Maybe Name)    -- variable name and optional label to break

initSt uniq = St uniq [] [] []

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

getInit :: Asm Doc
getInit
  = Asm (\env st -> (vcat (reverse (idoc st)), st{ idoc = [] }))

getEnv
  = Asm (\env st -> (env, st))

withEnv f (Asm asm)
  = Asm (\env st -> asm (f env) st)

localUnique asm
  = do u <- updateUnique id
       x <- asm
       setUnique u
       return x

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
                      case newtypesLookup name newtypes of
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

-- | Approved for use in JavaScript according to ECMA definition
ppLit :: Lit -> Doc
ppLit lit
    = case lit of
      LitInt i    -> if (isSmallInt(i))
                      then (pretty i)
                      else ppName nameIntConst <.> parens (dquotes (pretty i))
      LitChar c   -> text ("0x" ++ showHex 4 (fromEnum c))
      LitFloat d  -> text (showsPrec 20 d "")
      LitString s -> dquotes (hcat (map escape s))
    where
      escape c
        = if (c < ' ')
           then (if (c=='\n') then text "\\n"
                 else if (c == '\r') then text "\\r"
                 else if (c == '\t') then text "\\t"
                 else text "\\u" <.> text (showHex 4 (fromEnum c)))
          else if (c <= '~')
           then (if (c == '\"') then text "\\\""
                 else if (c=='\'') then text "\\'"
                 else if (c=='\\') then text "\\\\"
                 else char c)
          else if (fromEnum c <= 0xFFFF)
           then text "\\u" <.> text (showHex 4 (fromEnum c))
          else if (fromEnum c > 0x10FFFF)
           then text "\\uFFFD"  -- error instead?
           else let code = fromEnum c - 0x10000
                    hi = (code `div` 0x0400) + 0xD800
                    lo = (code `mod` 0x0400) + 0xDC00
                in text ("\\u" ++ showHex 4 hi ++ "\\u" ++ showHex 4 lo)

isSmallLitInt expr
  = case expr of
      Lit (LitInt i)  -> isSmallInt i
      _ -> False

isSmallInt i = (i > minSmallInt && i < maxSmallInt)

maxSmallInt, minSmallInt :: Integer
maxSmallInt = 9007199254740991  -- 2^53 - 1
minSmallInt = -maxSmallInt

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

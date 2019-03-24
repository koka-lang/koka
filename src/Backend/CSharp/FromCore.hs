-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- System F-like core language.
-----------------------------------------------------------------------------

module Backend.CSharp.FromCore( csharpFromCore
                              -- , ArityMap, ExtMap
                              -- , arityMapInit, externalMapInit
                              ) where

import Platform.Config(version)
import Lib.Trace( trace )
import Control.Applicative hiding (empty)
import Control.Monad
import Data.Char( isDigit, isAlphaNum )
import Data.List( transpose )
import Lib.PPrint

import Kind.Kind
import Type.Type
import Type.TypeVar
import Type.Kind( getKind )
import Type.Assumption( getArity )
import Type.Pretty( niceType )

import Common.Syntax( Target(..) )
import Common.Name
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Range
import Common.File(notdir)

import Core.Core
import Core.Pretty
import Core.CoreVar
import Type.Pretty(defaultEnv)
-- import Lib.Trace ( trace )

--------------------------------------------------------------------------
-- Generate CSharp code from System-F
--------------------------------------------------------------------------

csharpFromCore :: Int -> Bool -> Maybe (Name,Type) -> Core -> Doc
csharpFromCore maxStructFields useCps mbMain core
  = let body = runAsm initEnv (genProgram maxStructFields core)
    in text "// Koka generated module:" <+> string (showName (coreProgName core)) <.> text ", koka version:" <+> string version <->
       text "#pragma warning disable 164 // unused label" <->
       text "#pragma warning disable 162 // unreachable code" <->
       text "#pragma warning disable 219 // variable is assigned but never used" <->
       text "using System;" <->
       text "using System.Numerics;" <->
       vcat (concatMap includeExternal (coreProgExternals core)) <->
       text "// module" <+> pretty (coreProgName core) <->
       text "public static class" <+> ppModName (coreProgName core) <+> block (linebreak <.> body)  <->
       (case mbMain of
          Just (name,tp)
            -> (text "public static class Program {" <-> indent 2 (ppMain name tp) <-> text "}")
          Nothing
            -> empty)
  where
    isAsync = case mbMain of 
                Just (name,tp) -> isAsyncFunction tp
                _ -> False

    initEnv = Env { moduleName = coreProgName core
                  , currentDef = nameNil
                  , resultKind = ResultReturn True
                  , currentIndent = 0
                  , currentArgs = Nothing
                  , withCps     = useCps
                  }

includeExternal :: External -> [Doc]
includeExternal (ExternalInclude includes range)
  = let content = case lookup CS includes of
                    Just s -> s
                    Nothing -> case lookup Default includes of
                                 Just s -> s
                                 Nothing -> "" -- failure ("backend does not support external inline at " ++ show range)
    in [text content]
includeExternal _  = []


ppMain :: Name -> Type -> Doc
ppMain name tp
  = classFun <->
    text "static void Main() {" <->
     indent 4 (ppName nameMainConsole <.> text "<Unit,Unit>( new MainFun() );" ) <->
    text "}"
  where
    classFun = vcat [
                 text "sealed class MainFun : Fun0<Unit> {",
                 indent 2 $ text "public object Apply() {",
                 indent 4 $ text "return (object)" <.> parens callMain <.> semi,
                 indent 2 $ text "}",
                 text "}"
               ]
    callMain    = if (isAsyncFunction tp) then asyncMain else consoleMain
    asyncMain   = text "__std_async.async_handle( new Primitive.FunFunc0<Unit>( () => " <+> consoleMain <+> text "))"
    consoleMain = ppName name <.> typeArgs <.> text "()"
    typeArgs = case expandSyn tp of
                 TForall pars _ _ | not (null pars)
                   -> angled (map (\_ -> text "object") pars)
                 _ -> empty

genProgram :: Int -> Core -> Asm ()
genProgram maxStructFields core
  = do mapM_ (genTypeDefGroup maxStructFields) (coreProgTypeDefs core)
       mapM_ genDefGroup (coreProgDefs core)




---------------------------------------------------------------------------------
-- Type definitions
---------------------------------------------------------------------------------

genTypeDefGroup msf (TypeDefGroup tdefs)
  = mapM_ (genTypeDef msf) tdefs



vcatBreak []  = empty
vcatBreak xs  = linebreak <.> vcat xs

isStruct :: DataRepr -> Bool
isStruct DataSingleStruct = True
isStruct DataStruct       = True
isStruct DataIso          = True  -- because C# distinguishes on types, we cannot unwrap :-(
isStruct _                = False

hasTagField :: DataRepr -> Bool
hasTagField DataNormal = True
hasTagField DataStruct = True
hasTagField _          = False

genTypeDef :: Int -> TypeDef -> Asm ()
genTypeDef maxStructFields (Synonym synInfo vis)
  = return ()
genTypeDef maxStructFields (Data info vis conViss isExtend)
  = onTopLevel $
    do -- generate the type constructor
       ctx <- getModule
       putLn $ text "// type" <+> pretty (dataInfoName info)
       case getDataRepr maxStructFields info of
         (DataEnum,_)
           -> do putLn (ppVis vis <+> text "enum" <+> ppDefName (typeClassName (dataInfoName info)) <+>
                      block (vcatBreak (punctuate comma (map ppEnumCon (zip (dataInfoConstrs info) conViss))))
                     )
         (dataRepr,conReprs)
           -> do if (isExtend) then return ()
                  else do  -- generate type parameter constants
                           if (null (dataInfoParams info))
                            then return ()
                            else putLn (ppVis vis <+>
                                        text "sealed class" <+>
                                        ppDefName (typeConClassName (dataInfoName info)) <+> text "{ }")
                           -- generate the type
                           let noCons = null conReprs
                           putLn (ppVis vis <+> (if (noCons && not (dataInfoIsOpen info)) then text "sealed " else empty) <.>
                                  (if isStruct dataRepr then text "struct" else text "class") <+>
                                  ppDefName (typeClassName (dataInfoName info)) <.>
                                  ppTypeParams (dataInfoParams info) <.>
                                  (if (null (dataInfoParams info))
                                    then empty
                                    else (colon <+> ppTAAppDocs (ppDefName (typeConClassName (dataInfoName info))) (map (ppType ctx) (map TVar (dataInfoParams info))))
                                  ))
                           putLn $ text "{"
                 indented $
                   do -- tag field
                      if (hasTagField dataRepr)
                       then do onTopLevel $
                                  putLn (text "public enum" <+> ppTagType ctx (unqualify (dataInfoName info)) <+>
                                          block (vcatBreak (punctuate comma (map ppDefName (map conInfoName (dataInfoConstrs info))))))
                               putLn (text "public readonly" <+> ppTagType ctx (dataInfoName info) <+> ppTagName <.> semi <->
                                             text (if (isDataStruct dataRepr) then "private" else "protected") 
                                              <+> ppDefName (typeClassName (dataInfoName info)) <.> parens (ppTagType ctx (dataInfoName info) <+> ppTagName) <.>
                                               block (linebreak <.> vcat (
                                                 [text "this." <.> ppTagName <+> text "=" <+> ppTagName <.> semi]
                                                 ++
                                                 (case dataRepr of
                                                    DataStruct -> let allfields = concatMap conInfoParams (dataInfoConstrs info)
                                                                  in map (ppAssignDefault ctx) allfields
                                                    _          -> [])
                                               ))
                                            )                                
                       else if (dataRepr == DataAsList)
                        then putLn (text "public" <+> ppDefName (typeClassName (dataInfoName info)) <.> text "() { }")
                        else return ()
                      -- generate constructors
                      mapM_ (genConstructor info dataRepr) (zip (zip (dataInfoConstrs info) conViss) conReprs)
                        
                 if (isExtend) then return () else putLn (text "}")
  where
    ppEnumCon (con,vis)
      = ppDefName (conInfoName con)

genConstructor :: DataInfo -> DataRepr -> ((ConInfo,Visibility),ConRepr) -> Asm ()
genConstructor info dataRepr ((con,vis),conRepr) =
  case conRepr of
    ConEnum _ _
       -> return ()
    ConSingleton typeName _
       -> assertion ("CSharp.FromCore.genTypeDef: singleton constructor with existentials?") (null (conInfoExists con)) $
          conSingleton typeName      

    ConAsCons typeName nilName _
       -> -- merge it into the type class itself
          do ctx <- getModule
             putLn (vcat (map (ppConField ctx) (conInfoParams con) ++ ppConConstructor ctx con conRepr []))

    ConSingle typeName _
       -> -- merge it into the type class itself
          do ctx <- getModule
             let docs = map (ppConField ctx) (conInfoParams con) ++ ppConConstructor ctx con conRepr []
             if (null docs)
              then return ()
              else putLn (vcat docs)

    ConStruct typeName _
       -> conStruct typeName

    ConIso typeName _
       -> conStruct typeName

    _  -> onTopLevel $
          do ctx <- getModule
             (ppSuper,matchMethods) <- conExistsMatch ctx (conInfoExists con)
             let ppConType = ppDefName (conClassName (conInfoName con)) <.> ppTypeParams (dataInfoParams info ++ conInfoExists con)
             putLn (ppVis vis <+> text "sealed class" <+> ppConType <.> colon 
                      <+> ppSuper <.> ppTypeParams (dataInfoParams info) <+>
                   block ( linebreak <.> vcat
                         (map (ppConField ctx) (conInfoParams con) ++
                          ppConConstructor ctx con conRepr [] ++
                          matchMethods
                         )
                        )
                 )
             -- genConCreator con conRepr vis
             -- putLn (linebreak)
  where
    conStruct typeName  | null (conInfoParams con)
      = conSingleton typeName
    conStruct typeName
        -- merge it into the type class itself
      = do ctx <- getModule
           let defaults = concatMap conInfoParams (filter (\ci -> conInfoName ci /= conInfoName con) (dataInfoConstrs info))
               docs = map (ppConField ctx) (conInfoParams con) ++ 
                      ppConConstructor ctx con conRepr defaults
           if (null docs)
            then return ()
            else putLn (vcat (docs))

    conSingleton typeName        
      = do let ppTpParams = ppTypeParams (dataInfoParams info ++ conInfoExists con)
               ppConType  = ppDefName (typeClassName typeName) <.> ppTpParams
           ctx <- getModule
           putLn (text "public static readonly" <+> ppConType <+> ppDefName (conClassName (conInfoName con)) <+>
                  text "=" <+> text "new" <+> ppConType <.> parens (if (hasTagField dataRepr) then ppTag ctx typeName (conInfoName con) else empty) <.> semi)


    conExistsMatch ctx exists
      = let super = ppQName ctx (typeClassName (dataInfoName info))
        in if (null exists)
            then return (super,[])
            else do let ppConName = ppDefName (conClassName (conInfoName con))
                        ppConType = ppConName <.> ppTypeParams (dataInfoParams info)  
                        ppExistsMatchMethodHeader 
                          = text "object ExistsMatch" <.> parens (ppExistsApplyType (length exists) ppConType <+> text "_match")             
                    putLn (ppVis vis <+> text "abstract class" <+> ppConType <+> colon <+> super <.> ppTypeParams (dataInfoParams info) <.>                        
                             block (linebreak <.> vcat
                                    [text "public" <+> ppConName <.> parens (ppTagType ctx (dataInfoName info) <+> text "_tag")
                                        <+> text ": base(_tag) { }"
                                    ,text "public abstract" <+> ppExistsMatchMethodHeader <.> semi
                                    ]
                                   )
                           )
                    let ppExistsMatchMethod 
                          = text "public override" <+> ppExistsMatchMethodHeader 
                              <+> block (linebreak <.> text "return _match.ExistsApply" <.> ppTypeParams exists <.> parens (text "this") <.> semi)
                    return (ppConName, [ppExistsMatchMethod])

ppExistsApplyType :: Int -> Doc -> Doc
ppExistsApplyType i tp = text ("ExistsApply" ++ show i) <.> angled [tp]
           
ppConField :: ModuleName -> (Name,Type) -> Doc
ppConField ctx (name,tp)
  = text "public readonly" <+> ppType ctx tp <+> ppQName ctx name <.> semi

ppConConstructor :: ModuleName -> ConInfo -> ConRepr -> [(Name,Type)] -> [Doc]
ppConConstructor ctx con conRepr defaults
  = ppConConstructorEx ctx con conRepr (conInfoParams con) defaults

ppConConstructorEx :: ModuleName -> ConInfo -> ConRepr -> [(Name,Type)] -> [(Name,Type)] -> [Doc]
ppConConstructorEx ctx con conRepr conParams defaults
  = if (null conParams && not (isConNormal conRepr))
     then []
     else [text "public" <+>
           (case conRepr of
              ConAsCons typeName nilName _ -> ppDefName (typeClassName typeName)
              ConSingle typeName _ -> ppDefName (typeClassName typeName)
              ConStruct typeName _ -> ppDefName (typeClassName typeName)
              ConIso    typeName _ -> ppDefName (typeClassName typeName)
              _                    -> ppDefName (conClassName (conInfoName con))) <.>
           tupled (map ppParam (conInfoParams con)) <+>
           (case conRepr of
              ConNormal typeName _ -> text ":" <+> text "base" <.> parens (ppTag ctx typeName (conInfoName con)) <.> space
              _                    -> empty) <.>
           block (linebreak <.> vcat (
              (case conRepr of
                 ConStruct typeName _ -> [text "this." <.> ppTagName <+> text "=" <+> ppTag ctx typeName (conInfoName con) <.> semi]
                 _             -> [])
              ++ map ppAssignConField conParams
              ++ map (ppAssignDefault ctx) defaults
             )
          )]
  where
    ppParam (name,tp)
      = ppType ctx tp <+> ppDefName name

    ppAssignConField (name,tp)
      = text "this." <.> ppDefName name <+> text "=" <+> ppQName ctx name <.> semi

ppAssignDefault ctx (name,tp)
  = text "this." <.> ppDefName name <+> text "=" <+> text "default" <.> parens (ppType ctx tp) <.> semi;



ppTag ctx typeName conName
  = ppTagType ctx typeName <.> text "." <.> ppDefName conName

ppTagType ctx typeName
  = ppQName ctx (typeClassName typeName) <.> text "_Tag"

ppTagName
  = text tagName

tagName
  = "tag_";

ppSingletonName
  = text "singleton_";

---------------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------------
ppFunctionHeaderGen :: ModuleName -> Name -> [TypeVar] -> [Pred] -> Type -> Doc
ppFunctionHeaderGen ctx name vars preds tp
  = case expandSyn tp of
      TForall vars' preds' t -> ppFunctionHeaderGen ctx name (vars' ++ vars) (preds' ++ preds) t
      TFun pars eff res
        -> ppType ctx res <+> ppDefName name <.>
           ppTypeParams vars <.>
           ppParams ctx (map predToParam (zip preds [1..]) ++ pars)
      _ -> matchFailure "Backend.CSharp.FromCore.ppFunctionHeaderGen"
  where
    predToParam (p,n)
      = (newHiddenName (show n), predType p)


type ModuleName = Name

ppArgs names
  = tupled (map ppName names)

ppTypeArgs ctx []
  = empty
ppTypeArgs ctx tps
  = angled (map (ppType ctx) tps)

ppParams :: ModuleName -> [(Name,Type)] -> Doc
ppParams ctx params
  = tupled (map ppParam params)
  where
    ppParam (name,tp)  = ppType ctx tp <+> ppDefName name

-- | Returns the type constructor class name, for "List" it would be ".List"
typeConClassName :: Name -> Name
typeConClassName name
  = postpend "." (prepend "." name)

conClassName, typeClassName :: Name -> Name
typeClassName name
  = prepend "." name
conClassName name
  = postpend "." name


---------------------------------------------------------------------------------
-- Definitions
---------------------------------------------------------------------------------

unzipTNames ts
  = [(name,tp) | TName name tp <- ts]

genDefGroup (DefNonRec def)
  = genDef False def
genDefGroup (DefRec [def])
  = genDef True def
genDefGroup (DefRec defs)
  = mapM_ (genDef True) defs

genDef :: Bool -> Def -> Asm ()
genDef isRec def@(Def name tp expr vis defsort nameRng doc)
  = if (defIsVal def)
     then genDefX isRec def
     else case expr of  -- Ensure a top level non-value always gets compiled to a top-level function
            TypeLam _ _ -> genDefX isRec def
            Lam _ _ _   -> genDefX isRec def
            _ -> let (n,m) = getTypeArities tp
                 in if (m <= 0) then genDefX isRec def else
                     do expr1 <- tetaExpand expr [] n
                        expr2 <- etaExpand expr1 [] m
                        genDefX isRec (Def name tp expr2 vis defsort nameRng doc)

genDefZ :: Bool -> Def -> Asm ()
genDefZ isRec def  = genDefX isRec def

genDefX :: Bool -> Def -> Asm ()
genDefX isRec (Def name tp expr vis isVal nameRng doc)
  = -- trace ("genDef: " ++ show name) $
    onTopLevel $
    (if (isRec) then withRecDef name (extractArgs name expr) else withDef name) $
    do ctx <- getModule
       putLn empty
       putLineNo nameRng
       case expr of
         TypeLam tpars (Lam pars eff e)
            -> do putLn (hang 2 $ ppVis vis <+> text "static" <+> ppType ctx (typeOf e) </> ppDefName name <.> ppTypeParams tpars <.> ppParams ctx (unzipTNames pars))
                  genBody isRec True e

         Lam pars eff e
            -> do putLn (hang 2 $ ppVis vis <+> text "static" <+> ppType ctx (typeOf e) </> ppDefName name <.> ppParams ctx (unzipTNames pars))
                  genBody isRec True e
         Lit lit
            -> do putLn (ppVis vis <+> text constdecl <+> ppType ctx tp <+> ppDefName name <+> text "=" <+> ppLit lit <.> semi)
            where
               constdecl = case lit of
                             LitInt _ -> "static readonly"
                             _        -> "const"
         _  -> do putLn (text "private static " <+> ppType ctx tp <+> ppEvalName name <.> text "()")
                  genBody False True expr
                  putLn (hang 2 $ ppVis vis <+> text "static readonly" <+> ppType ctx tp <+> ppDefName name </> text "=" <+> ppEvalName name <.> text "()" <.> semi)
                  return ()

extractArgs :: Name -> Expr -> ([Type],[Name])
extractArgs name expr
  = case expr of
      TypeLam tpars (Lam pars eff e)
        -> (map TVar tpars,map getName pars)
      Lam pars eff e
        -> ([],map getName pars)
      _ -> failure ("CSharp.FromCore.genDef.extractArgs: recursive definition that is not a function? " ++ show name)


etaExpand :: Expr -> [Expr] -> Int -> Asm Expr
-- etaExpand fun [] 0 = return fun
etaExpand fun args n
  = assertion "Backend.CSharp.FromCore.etaExpand" (n > length args || (n == 0 && null args)) $
    do names <- mapM (\i -> newVarName "x") [length args + 1 .. n]
       let types = map snd (fst (splitFun (typeOf fun)))
           tnames = zipWith TName names types
           args'  = map (\n -> Var n InfoNone) tnames
       return (Lam tnames typeTotal (App fun (args ++ args')))

tetaExpand :: Expr -> [Type] -> Int -> Asm Expr
tetaExpand fun [] 0 = return fun
tetaExpand fun targs m
  = assertion "Backend.CSharp.FromCore.tetaExpand" (m > length targs || (m==0 && null targs)) $
    do ids <- uniqueIds ".t" (m - length targs)
       let (vars,_,_) = splitPredType (typeOf fun)
           kinds      = map getKind (drop (length targs) vars)
           vars'      = map (\(id,kind) -> TypeVar id kind Bound) (zip ids kinds)
       return (TypeLam vars' (TypeApp fun (targs ++ map TVar vars')))

genReturnExpr :: Bool -> Expr -> Asm ()
genReturnExpr tailCall expr
  = withReturn tailCall $ genExpr expr


genArguments :: [Expr] -> Asm [Doc]
genArguments exprs
  = if (length (filter (not . isInline) exprs) <= 1)
     then mapM genInline exprs
     else mapM genAtomic exprs



genInline :: Expr -> Asm Doc
genInline expr
  = if isInline expr
     then withIdOne (genExpr expr)
     else genAtomic expr

isInline expr
  = case expr of
      Case{} -> False
      _      -> True

isAtomic expr
  = case expr of
      Lit _          -> True
      Var _ InfoNone -> True
      Con _ _        -> True
      _              -> False

genExpr :: Expr  -> Asm ()
genExpr expr
  = -- trace ("genExpr: " ++ show expr) $
    do def <- getCurrentDef
       case expr of
          -- note: values with a generic parameter become functions in the C# translation (i.e. nil)

          -- ignore .open function applications
          App (App (TypeApp var@(Var tname (InfoExternal _)) [from,to]) [arg]) args  | getName tname == nameEffectOpen
            -> genExpr (App arg args)

          -- int32 constants
          App (Var tname _) [Lit (LitInt i)]  | getName tname == nameInt32 && isSmallInt i
            -> result (pretty i)

          -- function calls
          TypeApp (Var tname (InfoArity m n _)) targs
            -> genStatic tname m n targs Nothing

          App var@(Var tname (InfoArity m n _)) args
            -> genStatic tname m n [] (Just args)

          App (TypeApp (Var tname (InfoArity m n _)) targs) args
            -> genStatic tname m n targs (Just args)

          -- possible dynamic tail calls
          App (TypeApp (Var tname InfoNone) targs) args | def == getName tname
            -> genTailCall expr tname targs args

          App (Var tname InfoNone) args  | def == getName tname
            -> genTailCall expr tname [] args

          -- constructors
          TypeApp (Con tname repr) targs
            -> genCon tname repr targs []

          App con@(Con tname repr) args
            -> genCon tname repr [] args

          App tapp@(TypeApp (Con tname repr) targs) args
            -> genCon tname repr targs args

          -- externals
          TypeApp (Var tname (InfoExternal formats)) targs
            -> genExternal tname formats targs []

          App var@(Var tname (InfoExternal formats)) args
            -> genExternal tname formats [] args

          App (TypeApp (Var tname (InfoExternal formats)) targs) args
            -> genExternal tname formats targs args


          -- Optimize case of booleans to if statements or expressions.
          Case [e] [Branch ps1 [g1], Branch ps2 [g2]] | typeOf e == typeBool && isExprTrue (guardTest g1) && isExprTrue (guardTest g2)
            -> if (isAtomic (guardExpr g1) && isAtomic (guardExpr g2))
                then do d  <- genInline e
                        b1 <- genAtomic (guardExpr g1)
                        b2 <- genAtomic (guardExpr g2)
                        result $ parens (d <+> text "?" <+> b1 <+> text ":" <+> b2)
                else do rk <- getResultKind
                        case rk of
                          ResultId -> do d <- genAtomic expr -- recurse but this time under a ResultAssign
                                         result d
                          _        -> do d <- genInline e
                                         putLn (text "if" <+> parens d <+> text "{")
                                         indented $ genExpr (guardExpr g1)
                                         putLn (text "}")
                                         putLn (text "else {")
                                         indented $ genExpr (guardExpr g2)
                                         putLn (text "}")

          -- the rest
          _ -> genExprBasic expr



genExternal :: TName -> [(Target,String)] -> [Type] -> [Expr] -> Asm ()
genExternal  tname formats targs args
 = do let (m,n) = getTypeArities (typeOf tname)
      cps <- useCps
      ctx <- getModule
      if (n > length args)
        then assertion "CSharp.FromCore.genExternal: m /= targs" (m == length targs) $
             do eta <- etaExpand (TypeApp (Var tname (InfoExternal formats)) targs) args n
                genExpr eta
      {- else if (not cps && getName tname == nameYieldOp && length targs == 2)
        then do let resTp = last (filter (\t -> isKindStar (getKind t)) targs)
                currentDef <- getCurrentDef
                result (text "Primitive.UnsupportedExternal<" <.> ppType ctx (resTp) <.> text ">(" <.> ppLit (LitString (show currentDef)) <.> text ")")
                -}
        else -- assertion ("CSharp.FromCore.genExternal: " ++ show tname ++ ": n < args: " ++ show (m,n) ++ show (length targs,length args)) (n == length args && m == length targs) $
             do argDocs <- genArguments args
                if (getName tname == nameReturn)
                 then -- return statements
                      assertion "CSharp.FromCore.genExternal: return with arguments > 1" (length argDocs <= 1) $
                      do let argDoc = if (length argDocs == 1) then head argDocs else (text "Unit.unit")
                         isret <- isReturnContext
                         if (isret)
                          then result argDoc
                          else do putLn (text "return" <+> argDoc <.> semi)
                                  result (text "Primitive.Unreachable<" <.> ppType ctx (resultType [] (typeOf tname)) <.> text ">()")
                 else -- general external
                      do currentDef <- getCurrentDef
                         let resTp = resultType targs (typeOf tname)
                             targDocs = map (ppType ctx) targs
                             extDoc = ppExternal currentDef tname formats (ppType ctx resTp) targDocs argDocs
                         if (isTypeUnit resTp)
                           then do putLn (extDoc <.> semi)
                                   result (text "Unit.unit")
                           else result extDoc

resultType targs tp
  = let (vars,preds,rho) = splitPredType tp
    in subNew (zip vars targs) |->
       (case splitFunType rho of
         Just (pars,eff,res) -> res
         Nothing             -> rho)

kindCast :: ModuleName -> [Type] -> Type -> (Doc -> Doc)
kindCast ctx targs tp doc
  = if (all (\t -> not (isKindFun (getKind t))) targs)
     then doc
     else parens (ppType ctx (resultType targs tp)) <.> parens doc


genStatic :: TName -> Int -> Int -> [Type] -> Maybe [Expr] -> Asm ()
genStatic tname m n targs mbArgs
 = -- trace ("genStatic: " ++ show tname ++ show (m,n)) $
   let args = case mbArgs of
                Just xs -> xs
                Nothing -> []
   in if (null args && m > length targs)
    then do teta <- tetaExpand (Var tname (InfoArity m n NoMon)) targs m
            genExpr teta
   else if ((n == 0 || n > length args) && isNothing mbArgs)
    then assertion ("CSharp.FromCore.genStatic: m /= targs: " ++ show tname ++ show (m,n)) (m == length targs) $
         do eta <- etaExpand (TypeApp (Var tname (InfoArity m n NoMon)) targs) args n
            genExpr eta
    else do cdef <- getCurrentDef
            assertion ("CSharp.FromCore.genApp in: " ++ show cdef ++ ": " ++ show tname ++ " " ++ show (m,n) ++ show (length targs,length args)) (n == length args && m == length targs) $
            -- trace("genStatic: " ++ show cdef ++ ": " ++ show (m,n) ++ show (length targs, length args)) $
             do argDocs <- genArguments args
                -- let cast  = kindCast ctx targs (typeOf tname)
                ctx <- getModule
                ret <- isTailCallContext
                def <- getCurrentDef
                mbArgs <- getCurrentArgs
                -- trace ("test rec: " ++ show (getName tname,def,ret,mbArgs,targs)) $ return ()
                case (ret,mbArgs) of
                  (True,Just (tpars,parNames)) | def == getName tname && targs == tpars
                    -> -- tail call
                       assertion ("CSharp.FromCore.genStatic: tail arguments /= arguments") (length args == length parNames) $
                       do assignArguments parNames argDocs args
                          putLn (text "goto recurse;")
                  _ -> result (kindCast ctx targs (typeOf (App (TypeApp (Var tname (InfoArity m n NoMon)) targs) args))
                               (hang 2 $ ppQName ctx (getName tname) <.>
                                 (if (null targs) then empty else angled (map (ppType ctx) targs)) <//>
                                 ({- if (null args && null targs) then empty else -} septupled argDocs)))

genDynamic :: Expr -> [Expr] -> Asm ()
genDynamic v@(Var tname (InfoArity m n _)) args
  = genStatic tname m n [] (Just args)

genDynamic f args
  = do d <- genInline f
       ds <- genArguments (args)
       -- result (parens (ppType ctx (typeOf expr)) <.> parens (d <.> dot <.> text "Apply" <.> tupled ds))
       -- trace ("dynamic call: " ++ show f) $
       result (d <.> dot <.> text "Call" <.> tupled ds) 

septupled docs
  = lparen <.> vsep (punctuate (comma) docs) <.> rparen

isNothing Nothing = True
isNothing _       = False

genTailCall :: Expr -> TName -> [Type] -> [Expr] -> Asm ()
genTailCall expr tname targs args
  = do ctx <- getModule
       ret <- isTailCallContext
       def <- getCurrentDef
       mbArgs <- getCurrentArgs
       -- trace ("genTailCall: " ++ show (getName tname, def, ret,mbArgs)) $ return ()
       case (ret,mbArgs) of
         (True,Just (tpars,parNames)) | def == getName tname && targs == tpars
            -> do argDocs <- genArguments args
                  assignArguments parNames argDocs args
                  putLn (text "goto recurse;")
         _  -> genExprBasic expr

assignArguments parNames0 argDocs0 args0
  = let (parNames,argDocs,args)
            = unzip3 (filter notIdentity (zip3 parNames0 argDocs0 args0))
            where
              notIdentity (par,argDoc,Var tname _) = getName tname /= par
              notIdentity _                        = True

        fvs = map (map fst . localFv) args
        independent (par:pars) (fv:fvs)
          = all (not . elem par) fvs  && independent pars fvs
        independent [] []
          = True
        independent _ _
          = matchFailure "Backend.CSharp.FromCore.assignArguments"

    in if (independent parNames fvs)
        then mapM_ (\(par,argDoc) -> putLn (ppDefName par <+> text "=" <+> argDoc <.> semi)) (zip parNames argDocs)
        else do ctx <- getModule
                temps <- mapM (\parName -> newVarName (show parName)) parNames
                mapM_ (\(tmp,(arg,tp)) -> putLn (ppType ctx tp <+> ppDefName tmp <+> text "=" <+> arg <.> semi)) (zip temps (zip argDocs (map typeOf args)))
                mapM_ (\(par,tmp) -> putLn (ppDefName par <+> text "=" <+> ppDefName tmp <.> semi)) (zip parNames temps)


extractResultType :: Type -> Type
extractResultType tp
  = let (vars,preds,rho) = splitPredType tp
    in case splitFunType rho of
         Just (pars,eff,res)  -> TForall vars [] res
         Nothing              -> TForall vars [] rho

genCon :: TName -> ConRepr -> [Type] -> [Expr] -> Asm ()
genCon tname repr targs args
 = let (m,n) = getTypeArities (typeOf tname)
   in if (n > length args)
       then assertion "CSharp.FromCore.genCon: m /= targs" (m == length targs) $
         do eta <- etaExpand (TypeApp (Con tname repr) targs) args n
            genExpr eta
       else assertion "CSharp.FromCore.genCon: n < args" (n == length args && m == length targs) $
         do argDocs <- genArguments args
            -- let cast  = kindCast ctx targs (typeOf tname)
            ctx <- getModule
            result $ hang 2 $ -- cast $
             case repr of
              ConEnum _ _
                -> assertion "genCon: ConEnum has type args or args?" (null targs && null args) $
                   ppConEnum ctx tname
              ConSingleton typeName _
                -> ppConSingleton ctx typeName tname targs
              ConStruct typeName _ | null args 
                -> ppConSingleton ctx typeName tname targs
              ConStruct typeName _
                -> text "new" <+>
                   ppQName ctx (typeClassName typeName) <.>
                   ppTypeArgs ctx targs <//>
                   tupled ({- ppTag ctx typeName (getName tname) : -} argDocs)
              ConIso typeName _
                -> text "new" <+>
                   ppQName ctx (typeClassName typeName) <.>
                   ppTypeArgs ctx targs <//>
                   tupled ({- ppTag ctx typeName (getName tname) : -} argDocs)
              _ -> text "new" <+>
                   (case repr of
                      ConAsCons typeName _ _
                         -> ppQName ctx (typeClassName typeName)
                      ConSingle typeName _
                         -> ppQName ctx (typeClassName typeName)
                      _  -> ppQName ctx (conClassName (getName tname))) <.>
                   (ppTypeArgs ctx targs) <//>
                   (-- if (null targs && null args && not (isConNormal repr)) then empty else
                      tupled argDocs)

ppConEnum :: ModuleName -> TName -> Doc
ppConEnum ctx tname
  = let name = getName tname in
     if (name == nameTrue)
      then text "true"
     else if (name == nameFalse)
      then text "false"
     else if (name == nameTuple 0)
      then text "Unit.unit"
      else ppType ctx (typeOf tname) <.> dot <.> ppDefName (getName tname)

ppConSingleton :: ModuleName -> Name -> TName -> [Type] -> Doc
ppConSingleton ctx typeName tname targs
  = ppQName ctx (typeClassName typeName) <.> ppTypeArgs ctx targs <.> text "." <.> ppDefName (conClassName (getName tname))

ppExternal :: Name -> TName -> [(Target,String)] -> Doc -> [Doc] -> [Doc] -> Doc
ppExternal currentDef extName formats resTp targs args0
  = let args = map (\argDoc -> if (all (\c -> isAlphaNum c || c == '_') (asString argDoc)) then argDoc else parens argDoc) args0
    in case lookup CS formats of
     Nothing -> case lookup Default formats of
      Nothing ->
        trace( "warning: backend does not support external in " ++ show currentDef ) $
        (text "Primitive.UnsupportedExternal<" <.>
          resTp <.> text ">(\"" <.> text (show currentDef) <.> text "\")")
      Just s  -> ppExternalF s targs args
     Just s -> ppExternalF s targs args
  where
    ppExternalF :: String -> [Doc] -> [Doc] -> Doc
    ppExternalF fmt targs args
      = case fmt of
          [] -> empty
          ('#':'#':y:xs) ->
            if y `elem` ['1'..'9']
             then (index targs ((fromEnum y) - (fromEnum '1'))) <.> ppExternalF xs targs args
             else char y <.> ppExternalF  xs targs args
          ('#':y:xs) ->
            if y `elem` ['1'..'9']
             then (index args ((fromEnum y) - (fromEnum '1'))) <.> ppExternalF xs targs args
             else char y <.> ppExternalF  xs targs args
          (x:xs) ->
            char x <.> ppExternalF  xs targs args
      where
        index :: [Doc] -> Int -> Doc
        index xs i
          = if (i >= 0 && i < length xs)
             then xs !! i
             else failure $ "Backend.CSharp.FromCore.ppExternalF: external index out of range: " ++
                              "in " ++ show currentDef ++ ": " ++ show extName ++ ": " ++ fmt

genExprBasic :: Expr -> Asm ()
genExprBasic expr
  = do ctx <- getModule
       case expr of
          Var tname info
            -> case info of
                 InfoNone
                  -> do defName <- getCurrentDef
                        {- if (getName tname == defName)
                         then result (text "this")  -- recursive call to a first-class function: this only works because we disallow polymorphic recursive local definitions
                         else -}
                        result (ppQName ctx (getName tname))
                 InfoArity m n _
                  -> genStatic tname m n [] Nothing
                 InfoExternal format
                  -> genExternal tname format [] []
          Con tname repr
            -> genCon tname repr [] []
          App e es
            -> genDynamic e es
          TypeApp e ts
            -> do d <- genInline e
                  result (parens (parens (ppType ctx (typeOf expr)) <.> parens (d <.> dot <.> text "TypeApply"  <.> angled (map (ppType ctx) ts) <.> text "()")))
                  -- (foldl typeApp d (zip ts (replicate (length ts-1) (text "TypeFun") ++ [ppType ctx (typeOf e)])))
          TypeLam vars e
            -> do (newTp,gen) <- genLamOrTypeLam False expr
                  gen

          Lam vars eff e
            -> do funname <- getCurrentDef
                  name <- genName funname
                  let freeTVars = tvsList (ftv expr)
                      freeVars  = {- filter (\(nm,tp) -> nm /= funname) -} (localFv expr)
                  -- trace ("lift expr: " ++ show funname ++ ": " ++ show (map fst freeVars) ++ "\n" ++ show (prettyExpr defaultEnv expr)) $
                  genClass name freeTVars freeVars
                      (text "Fun" <.> pretty (length vars) <.> angled (map (ppType ctx) ([tp | TName _ tp <- vars] ++ [typeOf e])))
                      ((genApplyMethod False vars e))
                  result (if null freeVars
                           then (ppQName ctx name <.> ppTypeParams freeTVars <.> dot <.> ppSingletonName)
                           else (text "new" <+> ppQName ctx name <.> ppTypeParams freeTVars <.> ppArgs (map fst freeVars)))

          Case exprs branches
            -> do exprDocs <- mapM genAtomic exprs
                  {-
                  let patternss = transpose (map branchPatterns branches) -- list of list of patterns per expr
                      tagDoc   = vcat (concat (map ppGetTag (zip exprDocs (zip [1..] patternss))))
                  putLocal tagDoc
                  -}
                  lab <- genName (newName "label")
                  genBranches (ppDefName lab) exprDocs branches
                  -- putLn (text ("throw new Exception(\"" ++ show defName ++ ": pattern match failed.\");"))
                  ctx <- getModule
                  defName <- getCurrentDef
                  -- putLn (text "Primitive.PatternMatchError" <.> ppTypeArgs ctx [typeOf expr] <.> parens (dquotes (string (show defName))) <.> semi)
                  ret <- isReturnContext
                  if (ret)
                   then return ()
                   else putLn (ppDefName lab <.> text ": ;")

          Let defgroups expr
            -> genLetGroups defgroups expr

          Lit lit
            -> result (ppLit lit)
          -- _ -> result (text "todo genExpr")


genLamOrTypeLam tailCtx expr
  = do ctx     <- getModule
       funname <- getCurrentDef
       name    <- genName funname
       case expr of
         TypeLam vars e
           -> do let freeTVars = tvsList (ftv expr)
                     freeVars  = localFv expr -- filter (\(nm,tp) -> not (isQualified nm) {- && nm /= funname -}) (tnames (fv expr)) -- trick: only local names are not qualified
                     newType   = ppQName ctx name <.> ppTypeParams freeTVars

                 genClass name freeTVars freeVars (text "TypeFun" <.> pretty (length vars)) ((genTypeApplyMethod vars e))
                 return (newType
                        ,result (if null freeVars
                           then (newType <.> dot <.> ppSingletonName)
                           else (text "new" <+> newType <.> ppArgs (map fst freeVars)))
                        )
         Lam vars eff e
           -> do let freeTVars = tvsList (ftv expr)
                     freeVars  = {- filter (\(nm,tp) -> nm /= funname) -} (localFv expr)
                     newType   = ppQName ctx name <.> ppTypeParams freeTVars
                 -- trace("lift: " ++ show (map fst freeVars)) $
                 genClass name freeTVars freeVars
                      (text "Fun" <.> pretty (length vars) <.> angled (map (ppType ctx) ([tp | TName _ tp <- vars] ++ [typeOf e])))
                      ((genApplyMethod tailCtx vars e))
                 return (newType
                        ,result (if null freeVars
                           then (newType <.> dot <.> ppSingletonName)
                           else (text "new" <+> newType <.> ppArgs (map fst freeVars)))
                        )
         _ -> matchFailure "Backend.CSharp.FromCore.genLamOrTypeLam"


localFv expr
  = filter (not . isQualified . fst) (tnames (fv expr)) -- trick: only local names are not qualified

genLetGroups [] expr
  = genExpr expr
genLetGroups (group:groups) expr
  = case group of
      DefNonRec def -> genLetDefs False [def] groups expr
      DefRec [def]  -> genLetDefs True [def] groups expr
      DefRec defs   -> genLetDefs True defs  groups expr

genLetDefs isRec defs groups expr
  = do (subs,defs') <- fmap unzip (mapM uniquefyTopLevel defs)
       let sub = concat subs
           (defs'',groups',expr')
            = if null sub then (defs',groups,expr)
               else sub |~> (defs',groups,expr)
       mapM_ (genLetDef isRec) defs''
       genLetGroups groups' expr'
  where
    uniquefyTopLevel def@(Def name tp expr vis isVal nameRng doc)
      = if not (liftDefToTopLevel  def)
         then return ([],def)
         else do defname <- getCurrentDef
                 newVName <- (newVarName (show (unqualify defname) ++ "-" ++ show (unqualify name)))
                 let newName = qualify (qualifier defname) newVName -- need to qualify or otherwise its considered local
                     newDef = Def newName tp expr vis isVal nameRng ""
                     (m,n)  = getArity tp
                 return ([(TName name tp,Var (TName newName (typeOf expr)) (InfoArity m n NoMon))], newDef)

liftDefToTopLevel def
  = case (defExpr def) of
      TypeLam tpars (Lam pars eff e) -> isTopLevel def
      Lam pars eff e                 -> isTopLevel def
      _ -> False



genLetDef :: Bool -> Def -> Asm ()
genLetDef isRec def@(Def name tp expr vis isVal nameRng doc)
  = if isLambda expr && isTopLevel def
     then genDef isRec def
    else if (isLambda expr && isRec)
     then do ctx <- getModule
             let with gen = (if isRec then withRecDef name (extractArgs name expr) else withDef name) gen
             (newTp,genLam) <- with (genLamOrTypeLam True expr)
             putLn ({-ppType ctx tp -} newTp <+> ppDefName name <+> text "= null;")
             withAssign (\_ doc -> ppDefName name <+> text "=" <+> doc <.> semi) (with (genLam))
             putLn (ppDefName name <.> text "." <.> ppDefName name <+> text "=" <+> ppDefName name <.> semi)
     else do ctx <- getModule
             -- let with gen = (if isSingleRec then withRecDef name (extractArgs name expr) else withDef name) gen
             if (nameIsNil name && isStatement expr)
              then do withAssign (\_ doc ->  if (show doc == "Unit.unit") -- prevents empty statement, but it is ugly :-(
                                               then empty
                                               else (doc <.> semi)) (withDef name (genExpr expr))
              else do name' <- if nameIsNil name
                                then genName name
                                else return name
                      if (isInline expr)
                       then do exprDoc <- withDef name' (genInline expr)
                               putLn (hang 2 $ ppType ctx tp <+> ppDefName name' </> text "=" <+> exprDoc <.> semi)
                       else do putLn (ppLocalVar ctx tp name')
                               withAssign (\_ doc -> if (isTypeUnit tp)
                                                       then if (show doc == "Unit.unit") -- prevents empty statement, but it is ugly :-(
                                                             then empty
                                                             else (doc <.> semi)
                                                       else (ppDefName name' <+> text "=" <+> doc <.> semi))
                                          (withDef name' (genExpr expr))

ppLocalVar ctx tp name
  = ppType ctx tp <+> ppDefName name <.>
    (if isTypeUnit tp
      then text " = Unit.unit;"
      else semi)


isStatement :: Expr -> Bool
isStatement expr
  = case expr of
      App (Var _ InfoNone) _     -> False
      App _ _     -> True
      Let _ body  -> isStatement body
      _           -> False

isLambda :: Expr -> Bool
isLambda expr
  = case expr of
      TypeLam tpars (Lam pars eff e) -> True
      Lam pars eff e                 -> True
      _ -> False


genAtomic :: Expr -> Asm Doc
genAtomic expr
  = case expr of
          Var tname InfoNone
            -> do ctx <- getModule
                  return (ppQName ctx (getName tname))
          Con tname repr
            -> withIdOne (genCon tname repr [] [])
          Lit lit
            -> return (ppLit lit)
          _ -> do ctx <- getModule
                  local   <- newVarName "x"
                  if (isInline expr)
                   then do doc <- genInline expr
                           putLn (ppType ctx (typeOf expr) <+> ppDefName local <+> text "=" <+> doc <.> semi)
                   else do putLn (ppType ctx (typeOf expr) <+> ppDefName local <.> semi)
                           withAssign (\_ doc -> ppDefName local <+> text "=" <+> doc <.> semi) (genExpr expr)
                  return (ppDefName local)


genBranches :: Doc -> [Doc] -> [Branch] -> Asm ()
genBranches lab exprDocs branches
  = do mbTagDocs <- mapM genTag (zip exprDocs (transpose (map branchPatterns branches)))
       let generate = do mapM_ (genBranch mbTagDocs exprDocs True) (init branches)
                         genBranch mbTagDocs exprDocs False (last branches)
       rk <- getResultKind
       case rk of
         ResultId -> do ctx <- getModule
                        local  <- newVarName "tmp"
                        withAssign (\_ doc -> ppDefName local <+> text "=" <+> doc <.> semi <+> text "goto" <+> lab <.> semi) $
                         generate
                        result (ppDefName local)
         ResultReturn _
                  -> generate
         ResultAssign f
                  -> withAssign (\f doc -> f doc <+> text "goto" <+> lab <.> semi) $
                      generate

genTag :: (Doc,[Pattern]) -> Asm (Maybe Doc)
genTag (exprDoc,patterns)
  = if (null (filter isConMatch patterns)) -- for two or more, it pays to get a tag
     then return Nothing
     else do -- local <- newVarName "tag"
             -- putLn (text "int" <+> ppDefName local <+> text "=" <+> exprDoc <.> text "." <.> ppTagName <.> semi)
             return (Just (exprDoc <.> text "." <.> ppTagName))
  where
    isConMatch (PatCon _ _ (ConNormal _ _) _ _ _ _) = True
    isConMatch (PatCon _ _ (ConStruct _ _) _ _ _ _) = True
    isConMatch (PatCon _ _ (ConIso _ _) _ _ _ _)    = True
    isConMatch _                                  = False

genBranch :: [Maybe Doc] -> [Doc] -> Bool -> Branch -> Asm ()
genBranch mbTagDocs exprDocs doTest branch@(Branch patterns [g@(Guard guard expr)]) -- TODO: adapt for multiple guards!
  = do ctx <- getModule
       let rtypeDoc = ppType ctx (typeOf expr)
           freeVars = localFv branch -- overestimate..
           freeTVars= tvsList (ftv branch)
       genPattern doTest (zip3 mbTagDocs exprDocs patterns) (rtypeDoc,freeVars,freeTVars) (genGuard guard expr)
genBranch _ _ _ _
  = fail "Backend.CSharp.FromCore.genBranch: multiple guards not implemented"

genGuard :: Expr -> Expr -> Asm ()
genGuard guard expr 
  = case guard of
      Con tname repr | getName tname == nameTrue
        -> genExpr expr
      _ -> do gdoc <- withIdOne $ genExpr guard  -- TODO: wrap the guard for existentials
              do putLn (text "if" <+> parens (gdoc))
                 genScoped expr

genScoped :: Expr -> Asm ()
genScoped expr
  = do putLn (text "{")
       indented (genExpr expr)
       putLn (text "}")
       return ()

genPattern :: Bool -> [(Maybe Doc,Doc,Pattern)] -> (Doc,[(Name,Type)],[TypeVar]) -> Asm () -> Asm ()
genPattern doTest [] einfo genBody
  = genBody 
genPattern doTest dpatterns einfo@(rtypeDoc,freeVars,freeTVars) genBody
  = do (testss,localss,nextPatternss,ematchess) <- fmap (unzip4 . concat) $ mapM (genPatternTest doTest) dpatterns
       let tests = concat testss
           locals = concat localss
           nextPatterns = concat nextPatternss
           ematches = concat ematchess

           genPatBody = do genPattern doTest nextPatterns einfo genBody

           genPat = do if (null locals) then return () else putLn (vcat locals)
                       case ematches of
                          [] -> genPatBody
                          [(etypeDoc,typeDoc,local,exists)] 
                            -> genExistsApply etypeDoc typeDoc rtypeDoc local exists freeTVars freeVars genPatBody
                          _ -> failure ("Backend.CSharp.FromCore.genPattern: sorry can only handle toplevel simple existential pattern matches")

       if (null tests)
        then do genPat
        else do putLn (text "if" <+> parens (hcat (punctuate (text "&&") tests)) <+> text "{")
                indented genPat
                putLn (text "}")

genExistsApply ::  Doc -> Doc -> Doc -> Name -> [TypeVar] -> [TypeVar] -> [(Name,Type)] -> Asm () -> Asm ()
genExistsApply etypeDoc typeDoc rtypeDoc local exists freeTVars freeVars genRetE
  = do ctx     <- getModule
       funname <- getCurrentDef
       name    <- genName funname
       let newType = ppQName ctx name <.> ppTypeParams freeTVars
       genClass name freeTVars freeVars (ppExistsApplyType (length exists) typeDoc) 
                (genExistsApplyMethod etypeDoc typeDoc local exists genRetE)
       let inst = if null freeVars
                     then (newType <.> dot <.> ppSingletonName)
                     else (text "new" <+> newType <.> ppArgs (map fst freeVars))
       result (parens rtypeDoc <.> ppDefName local <.> dot <.> text "ExistsMatch" <.> parens inst)

genExistsApplyMethod :: Doc -> Doc -> Name -> [TypeVar] -> Asm() -> Asm ()
genExistsApplyMethod etypeDoc typeDoc local exists genRetE
  = do putLn (text "public object ExistsApply" <.> ppTypeParams exists <.> parens (typeDoc <+> text "_ex"))
       putLn (text "{")
       indented $ 
         do putLn (etypeDoc <+> ppDefName local <+> text "= " <.> parens etypeDoc <.> text "_ex;")
            withReturn False $ genRetE
       putLn (text "}")


genPatternTest :: Bool -> (Maybe Doc,Doc,Pattern) -> Asm [([Doc],[Doc],[(Maybe Doc,Doc,Pattern)],[(Doc,Doc,Name,[TypeVar])])]
genPatternTest doTest (mbTagDoc,exprDoc,pattern)
  = let test xs = if doTest then xs else [] in
    case pattern of
      PatWild -> return []
      PatVar tname pattern
        -> do ctx <- getModule
              let after = ppType ctx (typeOf tname) <+> ppDefName (getName tname) <+> text "=" <+> exprDoc <.> semi
                  next  = genNextPatterns (ppDefName (getName tname)) (typeOf tname) [pattern]
              return [([],[after],next,[])]
      PatLit lit
        -> return [(test [exprDoc <+> text "==" <+> ppLit lit],[],[],[])]
      PatCon tname patterns repr targs exists tres info
        -> do ctx <- getModule
              case repr of
                 ConEnum _ _
                  -> assertion "CSharp.FromCore.ppPatternTest.enum with patterns?" (null patterns) $
                     return [(test [exprDoc <+> text "==" <+> ppConEnum ctx tname],[],[],[])]
                 ConSingleton typeName _
                  -> assertion "CSharp.FromCore.ppPatternTest.singleton with patterns?" (null patterns) $
                     return [(test [exprDoc <+> text "==" <+> ppConSingleton ctx typeName tname tpars],[],[],[])]
                 ConSingle typeName _
                  -> -- assertion ("CSharp.FromCore.ppPatternTest.single with test? ")  (doTest == False) $
                     -- note: the assertion can happen when a nested singleton is tested
                     do -- generate local for the test result
                        ctx <- getModule
                        -- local <- newVarName (show (unqualify (getName tname)))
                        let next = genNextPatterns (exprDoc) (typeOf tname) patterns
                        return [([] -- test [exprDoc <+> text "!=" <+> ppConSingleton ctx typeName (TName nilName (typeOf tname)) targs]
                                ,[],next,[])]

                 ConAsCons typeName nilName _
                  -> do let next    = genNextPatterns (exprDoc) (typeOf tname) patterns
                        return [(test [exprDoc <+> text "!=" <+>
                                    ppConSingleton ctx typeName (TName nilName (typeOf tname)) tpars]
                                ,[],next,[])]
                 ConStruct typeName _
                  -> testStruct typeName
                 ConIso typeName _
                  -> testStruct typeName
                 ConNormal typeName _
                  -> conTest ctx typeName exists -- TODO: use tags if available
                 ConOpen typeName
                  -> conTest ctx typeName exists
        where
          testStruct typeName
            = case mbTagDoc of
               Nothing -> failure "CSharp.FromCore: should always have tag when matching on structs"
               Just tagDoc
                -> do ctx <- getModule
                      let next    = genNextPatterns (exprDoc) (typeOf tname) patterns
                      return [(test [tagDoc <+> text "==" <+> ppTag ctx typeName (getName tname)],[],next,[])]
          tpars
            = case expandSyn tres of
                TApp _ targs -> targs
                _ -> -- trace ("could not expand to app: " ++ show (niceType defaultEnv tres)) $
                     []

          conTest ctx typeName exists
            =do -- generate local for the test result
                ctx <- getModule
                local <- newVarName (show (unqualify (getName tname)))
                let typeDoc = ppQName ctx (conClassName (getName tname)) <.> ppTypeArgs ctx tpars
                    next    = genNextPatterns (ppDefName local) (typeOf tname) patterns
                case mbTagDoc of
                  Nothing
                    -> assertion ("Backend.CSharp.FromCore.genPattern: existentials without tag!") (null exists) $
                       do putLn (typeDoc <+> ppDefName local <+> text "=" <+>
                                 (if (doTest)
                                   then parens exprDoc <+> text "as" <+> typeDoc <.> semi
                                   else parens typeDoc <.> parens exprDoc <.> semi))
                          return [(test [ppDefName local <+> text "!= null"],[],next,[])]
                  Just tagDoc
                    -> do let localCast
                                   = -- tests show that a cast is faster than "as" here !?!
                                     typeDoc <+> ppDefName local <+> text "=" <+> parens typeDoc <.> parens exprDoc <.> semi
                              ematch
                                   = if (null exists) then []
                                        else let etypeDoc = ppQName ctx (conClassName (getName tname)) <.> ppTypeArgs ctx (tpars ++ map TVar exists)
                                             in [(etypeDoc,typeDoc,local,exists)]
                              cast = if (null next && null ematch)
                                      then [] else [localCast]
                          return [(test [tagDoc <+> text "==" <+> ppTag ctx typeName (getName tname)],cast,next,ematch)]


genNextPatterns :: Doc -> Type -> [Pattern] -> [(Maybe Doc,Doc,Pattern)]
genNextPatterns exprDoc tp []
  = []
genNextPatterns exprDoc tp patterns
  = let (vars,preds,rho) = splitPredType tp
    in case expandSyn rho of
         TFun args eff res
          -> case patterns of
               [PatWild]  | length args > 1 -> []
               [pat]      | length args == 0 || length args > 1 -> [(Nothing, exprDoc, pat)]
               _          -> assertion ("CSharp.FromCore.genNextPatterns: args != patterns " ++ show (length args, length patterns) ++ show (args,patterns) ++ ":\n expr: " ++ show exprDoc ++ "\n type: " ++ show tp) (length args == length patterns) $
                             concatMap genNextPattern (zip [if nameIsNil name then newFieldName i else name  | (name,i) <- zip (map fst args) [1..]]
                                                       patterns)
         _ -> case patterns of
                [PatWild] -> []
                [pat]     -> [(Nothing,exprDoc,pat)]
                _         -> failure "CSharp.FromCore.genNextPatterns: patterns but not a function"
  where
    genNextPattern (name,pattern)
      = case pattern of
          PatWild -> []
          _       -> let patDoc = exprDoc <.> dot <.> ppDefName name
                     in [(Nothing,patDoc, pattern)]


tnames :: TNames -> [(Name,Type)]
tnames tns
  = [(name,tp) | (TName name tp) <- tnamesList tns]

ppEvalName name
  = ppDefName (makeHiddenName "eval" name)

ppNewName
  = text "New"

genClass :: Name -> [TypeVar] -> [(Name,Type)] -> Doc -> Asm () -> Asm ()
genClass name freeTVars freeVars derives genMore
  = onTopLevel $
    do ctx <- getModule
       let ppClassType = ppDefName name <.> ppTypeParams (freeTVars)
           ppNewExpr   = text "new" <+> ppClassType <.> ppArgs (map fst freeVars)
       putLn (text "sealed class" <+> ppClassType <+> colon <+> derives <+>
            text "{" <.> tab (vcatBreak
             (map (\var -> ppField ctx var) freeVars
              ++
              (if (null freeVars)
                then [text "public readonly static" <+> ppClassType <+> ppSingletonName <+> text "=" <+> ppNewExpr <.> semi]
                else [text "public" <+> ppDefName name <.> ppParams ctx freeVars <+>
                      block (linebreak <.> vcat (map ppAssignField freeVars))])
              {-
              ++
               (if (null freeTVars)
                 then []
                 else [text "public int Tag() { return 0; }"])
              -}
              {-
              ++
              [text "public static" <+> ppClassType <+> ppNewName <.> ppParams ctx freeVars <+>
               block (linebreak <.> text "return" <+> (if null freeVars then ppSingletonName else ppNewExpr) <.> semi)]
              -}
             )) <.> linebreak
           )
       withDoc tab genMore
       putLn (text "}")
  where
    ppAssignField (name,tp) = text "this." <.> ppName name <+> text "=" <+> ppName name <.> semi

genTypeApplyMethod :: [TypeVar] -> Expr -> Asm ()
genTypeApplyMethod tvars expr
  = do putLn (text "public object TypeApply" <.> ppTypeParams tvars <.> text "()" )
       genBody False False expr


genApplyMethod :: Bool -> [TName] -> Expr -> Asm ()
genApplyMethod tailCtx vars expr
  = do ctx <- getModule
       putLn (text "public" <+> text "object" {- ppType ctx (typeOf expr) -} <+> text "Apply" <.> ppParams ctx [(name,tp) | TName name tp <- vars] )
       genBody True tailCtx expr


genBody :: Bool -> Bool -> Expr -> Asm ()
genBody genLabel tailCall expr
  = do putLn (text "{")
       if (genLabel)
        then putLn (text "recurse:")
        else return ()
       indented $ genReturnExpr tailCall expr
       putLn (text "}")
       return ()

ppField ctx (name,tp)
  = (if (isFunOrForall tp) then text "public" else text "readonly") <+>
    ppType ctx tp <+> ppQName ctx name <.> semi
  where
    isFunOrForall tp
      = case expandSyn tp of
          TForall _ _ _ -> True
          TFun _ _ _    -> True
          _             -> False

ppLit :: Lit -> Doc
ppLit lit
  = case lit of
      LitInt i  -> if (isSmallInt i) 
                    then (case i of
                            -1 -> text "BigInteger.MinusOne"
                            0  -> text "BigInteger.Zero"
                            1  -> text "BigInteger.One"
                            _  -> text "(BigInteger)" <.> (if (i < 0) then parens (pretty i) else pretty i))
                    else text ("Primitive.IntString(\"" ++ show i ++ "\")")
      LitChar c -> text ("0x" ++ showHex 4 (fromEnum c))
      LitFloat d -> text (showsPrec 20 d "")
      LitString s -> dquotes (hcat (map escape s))
  where
    escape c
      = if (c >= ' ' && c <= '~' && not (elem c "\\\"'"))
         then char c
        else if (fromEnum c <= 0xFFFF)
         then text ("\\u" ++ showHex 4 (fromEnum c))
         else text ("\\U" ++ showHex 8 (fromEnum c))

isSmallInt :: Integer -> Bool
isSmallInt i = (i >= -0x80000000 && i <= 0x7FFFFFFF)

---------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------

ppTypeParams  [] = empty
ppTypeParams tvs
  = angled (map ppTypeVar tvs)




ppType :: ModuleName -> Type -> Doc
ppType ctx tp
  = case expandSyn tp of
      TForall vars preds t
        -> if (not (null vars))
            then primitive ("TypeFun" ++ show (length vars))
            else case expandSyn t of
                   TFun pars eff res -> ppTypeFun ctx preds pars eff res
                   _                 -> ppType ctx t
      TFun pars eff res
        -> ppTypeFun ctx [] pars eff res

      TApp t ts
        -> -- case expandSyn t of
           --  TCon c | getKind tp == kindStar -> ppQName ctx (typeClassName (typeConName c)) <.> angled (map (ppType ctx) ts)
           --  _      ->
           ppTypeApp ctx t ts
      TCon c
        -> ppTypeCon ctx c (getKind tp)
      TVar v
        -> ppTypeVar v
      TSyn syn args t
        -> ppType ctx t

ppTypeCon ctx c kind
   = let name = typeConName c
     in if (name == nameTpInt)
         then text "BigInteger"
        else if (name == nameTpString)
         then text "string"
        else if (name == nameTpChar)
         then text "int"  -- we need to represent as int since Char in C# is only defined as a UTF16 point
        else if (name == nameTpInt32)
         then text "int"
        else if (name == nameTpFloat)
         then text "double"
        else if (name == nameTpBool)
         then text "bool"
        else if (name == nameTpUnit)
         then text "Unit"
        else if (name == nameTpRef)
         then text "Ref"
        else if (name == nameTpAny)
         then text "object"
        -- else if (name == nameTpAsyncEvent)
        --  then text "Async"
        else if (name == nameTpException)
         then text "Exception"
        else if (name == nameTpDict)
         then text "Primitive.Dict"
        else if (name == nameTpMDict)
         then text "Primitive.MDict"
        else if (name == nameTpException)
         then text "Exception"
        else if (name == nameTpHandlerBranch0)
         then text "Eff.Branch"
        else if (name == nameTpHandlerBranch1)
         then text "Eff.Branch1"
        else if (isKindFun kind)
         then ppQName ctx (typeConClassName name)
         else ppQName ctx (typeClassName name)

ppTypeApp ctx t ts
  = case expandSyn t of
      TVar v -> ppTAApp ctx t ts
      TCon c  | typeConName c == nameTpArray && length ts == 2
             -> ppType ctx (head (tail ts)) <.> text "[]"
             | typeConName c == nameTpBuilder && length ts == 1
             -> text "System.Text.StringBuilder"
             | typeConName c == nameTpVector && length ts == 1
             -> ppType ctx (head ts) <.> text "[]"
             | typeConName c == nameTpNull && length ts == 1
             -> ppType ctx (head ts)
             | (typeConName c == nameTpHandlerBranch0 || typeConName c == nameTpHandlerBranch1) && length ts >= 1
             -> ppTypeCon ctx c (getKind (TApp t ts)) <.> angled (map (ppType ctx) (tail ts)) -- discard effect type
             | otherwise
             -> (ppTypeCon ctx c (getKind (TApp t ts))) <.> angled (map (ppType ctx) ts)
      _      -> (ppType ctx t) <.> angled (map (ppType ctx) ts)

ppTAApp ctx t ts
  = ppTAAppDocs (ppType ctx t) (map (ppType ctx) ts)

ppTAAppDocs t ts
  = foldl (\d targ -> primitive "TA" <.> angled [d,targ]) t ts

ppTypeVar v
  = text ("T" ++ show (typeVarId v))

ppTypeFun ctx preds pars eff res
  = ppTFun (map predType preds ++ map snd pars ++ [res])
  where
    ppTFun (arg:rest)
      = primitive ("Fun" ++ show (length rest)) <.> angled (ppType ctx arg :   map (ppType ctx) rest)
    ppTFun []
      = matchFailure "Backend.CSharp.ppTypeFun"

ppVis Public  = text "public"
ppVis Private = text "private"


primitive s
  = text s


ppTypeEx :: ModuleName -> Type -> [Type] -> Doc
ppTypeEx ctx tp targs
  = let (tvars,preds,rho) = splitPredType tp in
    assertion "Backend.CSharp.FromCore.ppTypeApp" (length tvars == length targs) $
    if (all (\targ -> kindStar == getKind targ) targs)
     then ppType ctx (subNew (zip tvars targs) |-> rho)
     else let argDocs = map (ppType ctx) targs
              sub = zip tvars argDocs
          in -- trace ("ppTypeEx: " ++ show (tp,targs) ++ ": " ++ show sub) $
             ppTypeSub ctx sub (TForall [] preds rho)

ppTypeSub :: ModuleName -> [(TypeVar,Doc)] -> Type -> Doc
ppTypeSub ctx sub tp
  = case expandSyn tp of
      TForall vars preds t
        -> if (not (null vars))
            then primitive ("TypeFun" ++ show (length vars))
            else case expandSyn t of
                   TFun pars eff res -> ppTypeFunSub ctx sub preds pars eff res
                   _                 -> ppTypeSub ctx sub t
      TFun pars eff res
        -> ppTypeFunSub ctx sub [] pars eff res
      TApp t ts
        -> -- case expandSyn t of
           --   TCon c | getKind tp == kindStar -> ppQName ctx (typeClassName (typeConName c)) <.> angled (map (ppTypeSub ctx sub) ts)
           --   _      ->
           ppTypeAppSub ctx sub t ts
      TVar v
        -> case lookup v sub of
             Just doc -> doc
             Nothing  -> ppTypeVar v
      t -> ppType ctx t


ppTypeAppSub ctx sub t ts
  = foldl (\d targ -> primitive "TA" <.> angled [d,ppTypeSub ctx sub targ]) (ppTypeSub ctx sub t) ts

ppTypeFunSub ctx sub preds pars eff res
  = ppTFun (map predType preds ++ map snd pars ++ [res])
  where
    ppTFun (arg:rest)
      = primitive ("Fun" ++ show (length rest)) <.> angled (ppTypeSub ctx sub arg : map (ppTypeSub ctx sub) rest)
    ppTFun []
      = matchFailure "Backend.CSharp.ppTypeFunSub"

---------------------------------------------------------------------------------
-- Monad and Environment
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

runAsm :: Env -> Asm () -> Doc
runAsm initEnv (Asm asm)
  = case asm initEnv initSt of
      ((),st) -> vcat (reverse (toplevel st))


data St  = St  { uniq     :: Int
               , results  :: [Doc]
               , locals   :: [Doc]
               , toplevel :: [Doc]
               }

data Env = Env { moduleName :: Name      -- | current module
               , currentDef :: Name
               , currentArgs :: Maybe ([Type],[Name])  -- | current recursive definition argument types and argument names
               , resultKind :: ResultKind
               , currentIndent :: Int
               , withCps     :: Bool
               }

data ResultKind = ResultReturn Bool -- ^ True if in a tail call context
                | ResultAssign (Doc -> Doc)
                | ResultId


initSt = St 0 [] [] []


instance HasUnique Asm where
  updateUnique f
    = Asm (\env st -> (uniq st, st{ uniq = f (uniq st)}))


updateSt f
  = Asm (\env st -> (st,f st))

getSt
  = updateSt id

setSt st
  = updateSt (const st)

getEnv
  = Asm (\env st -> (env, st))

withEnv f (Asm asm)
  = Asm (\env st -> asm (f env) st)

withTopLevel :: ([Doc] -> [Doc]) -> Asm ()
withTopLevel f
  = do updateSt (\st -> st{ toplevel = f (toplevel st) })
       return ()

onTopLevel :: Asm () -> Asm ()
onTopLevel asm
  = withEnv (\env -> env{ currentIndent = if (currentIndent env > 0) then 2 else 0 }) $
    do st0 <- getSt
       let (d,ds) = case toplevel st0 of
                      [] -> (empty,[])
                      (d:ds) -> (d,ds)
       setSt (st0{ toplevel = empty:ds })
       asm
       updateSt (\st -> st{ toplevel = d:toplevel st })
       return ()

withDoc :: (Doc -> Doc) -> Asm a -> Asm a
withDoc f  asm
  = do st0 <- getSt
       let (d,ds) = case toplevel st0 of
                      [] -> (empty,[])
                      (d:ds) -> (d,ds)
       setSt (st0{ toplevel = empty:ds })
       x <- asm
       updateSt (\st -> let (e:es) = toplevel st
                        in st{ toplevel = ((d <.> f e):es) })
       return x



onLocals :: Asm a -> Asm ([Doc],a)
onLocals asm
  = do st0 <- updateSt (\st -> st{ locals = [] })
       x   <- asm
       st1 <- updateSt (\st -> st{ locals = (locals st0)})
       return ((locals st1), x)

putLineNo range
  = -- return ()
    if (rangeNull == range || posLine (rangeStart range) >= bigLine)
     then putLn (text "#line default")
     else putLn (text "// #line" <+> pretty (posLine (rangeStart range)) <+> dquotes (string (notdir (sourceName (rangeSource range)))))


putLn :: Doc -> Asm ()
putLn doc
  = do env <- getEnv
       let ndoc = indent (currentIndent env) doc
       updateSt (\st -> st{ toplevel = case (toplevel st) of { [] -> [ndoc]; (d:ds) -> ((d <-> ndoc) : ds)}})
       return ()

put :: Doc -> Asm ()
put doc
  = do env <- getEnv
       -- let ndoc = hang (currentIndent env) doc
       updateSt (\st -> st{ toplevel = case (toplevel st) of { [] -> [doc]; (d:ds) -> ((d <.> doc) : ds)}})
       return ()

indented :: Asm a -> Asm a
indented asm
  = withEnv (\env -> env{ currentIndent = (currentIndent env) + 2 }) asm


useCps :: Asm Bool
useCps
  = do env <- getEnv
       return (withCps env)

getModule :: Asm Name
getModule
  = do env <- getEnv
       return (moduleName env)

getCurrentDef :: Asm Name
getCurrentDef
  = do env <- getEnv
       return (currentDef env)

getCurrentArgs :: Asm (Maybe ([Type],[Name]))
getCurrentArgs
  = do env <- getEnv
       return (currentArgs env)

result :: Doc -> Asm ()
result doc
  = do env <- getEnv
       case (resultKind env) of
         ResultReturn _   -> putLn (text "return" <+> doc <.> semi)
         ResultAssign f   -> putLn (f doc)
         ResultId         -> do updateSt (\st -> st{ results = doc : results st })
                                return ()

getResultKind :: Asm ResultKind
getResultKind
  = do env <- getEnv
       return (resultKind env)

isReturnContext
  = do rk <- getResultKind
       case rk of
         ResultReturn _ -> return True
         _              -> return False

isTailCallContext
  = do rk <- getResultKind
       case rk of
         ResultReturn tailCall -> return tailCall
         _                     -> return False

noTailCall asm
  = withEnv (\env -> env{ resultKind = case resultKind env of
                                         ResultReturn _ -> ResultReturn False
                                         _              -> resultKind env }) asm
withResult k asm
  = withEnv (\env -> env{ resultKind = k }) asm

withDef name asm
  = -- trace ("withDef: " ++ show name) $
    withEnv (\env -> env{ currentDef = name, currentArgs = Nothing }) asm

withRecDef name (targs,parNames) asm
  = -- trace ("withRecDef: " ++ show name ++ show (targs,parNames)) $
    withEnv (\env -> env{ currentDef = name, currentArgs = Just (targs,parNames) }) asm

withReturn :: Bool -> Asm () -> Asm ()
withReturn tailCall asm
  = withResult (ResultReturn tailCall) asm

withId :: Asm () -> Asm [Doc]
withId asm
  = do st0 <- updateSt (\st -> st{ results = [] })
       withResult (ResultId) asm
       st1 <- updateSt (\st -> st{ results = results st0 })
       return (reverse (results st1))

withIdOne :: Asm () -> Asm Doc
withIdOne asm
  = do docs <- withId asm
       assertion "CSharp.FromCore.withIdOne" (length docs == 1) $
        return (head docs)

withAssign :: ((Doc -> Doc) -> Doc -> Doc) -> Asm () -> Asm ()
withAssign f asm
  = do rk <- getResultKind
       let g = case rk of
                 ResultAssign g -> g
                 _              -> id
       withResult (ResultAssign (f g)) asm

genName :: Name -> Asm Name
genName name
  = do i <- unique
       return (postpend ("." ++ show i) name)

-- non-proper morphisms


putLocal   :: Doc -> Asm ()
putLocal doc
  = Asm (\env st -> ((), st{ locals = locals st ++ [nest (currentIndent env) doc] }))

captureLocals :: Asm a -> Asm (a,[Doc])
captureLocals (Asm f)
  = Asm (\env st -> case f env (st{ locals = [] }) of
                      (x,st') -> ((x,locals st'), st'{ locals = locals st }))




newVarNames :: Int -> Asm [Name]
newVarNames 0 = return []
newVarNames i
  = do n <- newVarName "x"
       ns <- newVarNames (i - 1)
       return (n:ns)

newVarName :: String -> Asm Name
newVarName s
  = do u <- unique
       return (newHiddenName (s ++ show u))

---------------------------------------------------------------------------
-- Helpers for name generation
---------------------------------------------------------------------------
ppDefName :: Name -> Doc
ppDefName name
  = ppName (unqualify name)

ppQName :: Name -> Name -> Doc
ppQName modName name
  = {- if (modName == qualifier name)   -- We need to qualify always since otherwise we may clash with local variables. i.e. fun f( x : int ) { Main.x( x ) }
     then ppName (unqualify name)
     else -} ppName name

ppName :: Name -> Doc
ppName name
  = if isQualified name
     then ppModName (qualifier name) <.> dot <.> encode False (unqualify name)
     else encode False name

ppModName :: Name -> Doc
ppModName name
  = text "__" <.> encode True name

encode :: Bool -> Name -> Doc
encode isModule name
  = let s = show name
    in if (isReserved s)
        then text ("@" ++ s)
        else text (asciiEncode isModule s)
    {-
    text $ concatMap encodeChar $
    if (null s || isReserved s || not (isAlpha (head s) || head s == '.'))
     then (".." ++ s)
     else s
  where
    s = show name -- (nonCanonicalName name)

    encodeChar c
      = if (isAlphaNum c)
         then [c]
        else if (c=='.')  -- we use '.' for internal variables
         then "_"
        else  "_" ++ showHex 4 (fromEnum c)
  -}

isReserved :: String -> Bool
isReserved s
  = case s of
      ('T':rest)  | all isDigit rest
          -> True                 -- type variables
      _   -> s `elem` reserved

reserved :: [String]
reserved
  = -- Primitive types
    ["Fun0","Fun1","Fun2"
    ,"TypeFun0", "TypeFun1","TypeFun2"
    ,"TA", "Unit", "unit"
    ,"Ref"
    ,"Tag", "Apply", "TypeApply", "New"
    ,"Program", "Main"
    ]
    ++
    -- C# types
    ["Object","String"]
    ++
    -- C# imports
    ["System"]
    ++
    -- C# pseudo-keywords -- reserved only in certain contexts
    ["add", "alias", "get", "global", "partial", "remove", "set", "value", "where", "yield"]
    ++
    -- C# keywords
    ["abstract", "as",
     "base", "bool", "break", "byte",
     "case", "catch", "char", "checked", "class", "const", "continue",
     "decimal", "default", "delegate", "do", "double",
     "else", "enum", "event", "explicit", "extern",
     "false", "finally", "fixed", "float", "for", "foreach",
     "goto",
     "if", "implicit", "in", "int", "interface", "internal", "is",
     "lock", "long",
     "namespace", "new", "null",
     "object", "operator", "out", "override",
     "params", "private", "protected", "public",
     "readonly", "ref", "return",
     "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string", "struct", "switch",
     "this", "throw", "true", "try", "typeof",
     "uint", "ulong", "unchecked", "unsafe", "ushort", "using",
     "virtual", "void", "volatile",
     "while"]

--------------------------------------------------------------------------
--  Auxillary Document Combinators
--------------------------------------------------------------------------
commas docs
  = hcat (punctuate comma docs)

fblock doc
  = linebreak <.> block doc

block doc
  = lbrace <.> tab doc <-> rbrace

tab doc
  = nest 2 doc

xcat :: [Doc] -> Doc
xcat []
  = empty
xcat docs
  = vcat docs <.> linebreak

unzip4 xs = unzipx4 [] [] [] [] xs
unzipx4 acc1 acc2 acc3 acc4 []           = (reverse acc1, reverse acc2, reverse acc3, reverse acc4)
unzipx4 acc1 acc2 acc3 acc4 ((x,y,z,zz):xs) = unzipx4 (x:acc1) (y:acc2) (z:acc3) (zz:acc4) xs

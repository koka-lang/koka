------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Responsibilities of the kind checker:

    - Kindcheck all types in the program

    - Translate user types to internal types

    - Collect lists of data types, synonyms and constructors

    - Expand all synonyms (i.e., replace @id(int)@ by @id(int) == int@)

    - Transate type definition groups and externals to Core.
-}
-----------------------------------------------------------------------------

module Kind.Infer (inferKinds) where

import Lib.Trace
-- import Type.Pretty

import Data.Char(isAlphaNum)
import Data.List(groupBy,intersperse,nubBy)
import Data.Maybe(catMaybes)

import Lib.PPrint
import Common.Failure
import Common.Unique( uniqueId )
import Common.Error
import Common.ColorScheme( ColorScheme, colorType, colorSource )
import Common.Range
import Common.Name
import Common.NamePrim( nameTrue, nameFalse, nameEffectEmpty, nameEffectExtend, nameEffectAppend,
                        nameTpHandled, nameTpHandled1, nameCopy, namePatternMatchError,
                        nameJust, nameNothing )
import Common.Syntax
import qualified Common.NameMap as M
import Syntax.Syntax
import qualified Core.Core as Core

import Kind.ImportMap
import Kind.Kind
import Kind.Assumption
import Kind.Constructors
import Kind.Newtypes
import Kind.Synonym

import Type.Type
import Type.Assumption
import Type.TypeVar( tvsIsEmpty, ftv, subNew, (|->) )

import Kind.InferKind
import Kind.InferMonad
import Kind.Unify

import Syntax.RangeMap


{--------------------------------------------------------------------------
  Kindcheck a program and calculate types
--------------------------------------------------------------------------}
-- | Kindcheck a program and calculate types
inferKinds
  :: ColorScheme      -- ^ Color scheme used for error messages
  -> Int              -- ^ max struct fields option
  -> Maybe RangeMap   -- ^ possible range map for tool integration
  -> ImportMap        -- ^ Import aliases
  -> KGamma           -- ^ Initial kind kgamma
  -> Synonyms         -- ^ Initial list of synonyms
  -> Int              -- ^ Unique
  -> Program UserType UserKind  -- ^ Original program
  -> Error ( DefGroups Type       --  Translated program (containing translated types)
           -- , Gamma                --  Gamma containing generated functions, i.e type scheme for every constructor
           , KGamma               --  updated kind gamma
           , Synonyms             --  Updated synonyms
           , Newtypes             --  Data type information
           , Constructors         --  Constructor information
           -- , Core.TypeDefGroups   --  Core type definition groups
           -- , Core.Externals       --  Core externals
           , Core.Core            --  Initial core program with type definition groups, externals, and some generated definitions for data types (like folds).
           , Int                  --  New unique
           , Maybe RangeMap
           )
inferKinds colors maxStructFields mbRangeMap imports kgamma0 syns0 unique0
            (Program source modName nameRange tdgroups defs importdefs externals fixdefs doc)
  = let (errs1,warns1,rm1,unique1,(cgroups,kgamma1,syns1)) = runKindInfer colors mbRangeMap modName imports kgamma0 syns0 unique0 (infTypeDefGroups tdgroups)
        (errs2,warns2,rm2,unique2,externals1)              = runKindInfer colors rm1 modName imports kgamma1 syns1 unique1 (infExternals externals)
        (errs3,warns3,rm3,unique3,defs1)                   = runKindInfer colors rm2 modName imports kgamma1 syns1 unique2 (infDefGroups defs)
--        (errs4,warns4,unique4,cgroups)                 = runKindInfer colors modName imports kgamma1 syns1 unique3 (infCoreTDGroups cgroups)
        (synInfos,dataInfos) = unzipEither (extractInfos cgroups)
        conInfos  = concatMap dataInfoConstrs dataInfos
        cons1     = constructorsFromList conInfos
        gamma1    = constructorGamma maxStructFields dataInfos
        errs4     = constructorCheckDuplicates colors conInfos
        errs      = errs1 ++ errs2 ++ errs3 ++ errs4
        warns     = warns1 ++ warns2 ++ warns3
        dgroups   = concatMap (synTypeDefGroup modName) cgroups
    in addWarnings warns $
       if (null errs)
        then return (dgroups ++ defs1
                    -- ,gamma1
                    ,kgamma1
                    ,syns1
                    ,newtypesNew dataInfos
                    ,cons1
                    -- ,cgroups
                    -- ,externals1
                    ,Core.Core modName [] [] cgroups [] externals1 doc
                    ,unique3
                    ,rm3
                    )
        else errorMsg (ErrorKind errs)

unzipEither :: [Either a b] -> ([a],[b])
unzipEither xs
  = unz [] [] xs
  where
    unz left right (Left x:xs)  = unz (x:left) right xs
    unz left right (Right x:xs) = unz left (x:right) xs
    unz left right []           = (reverse left, reverse right)


extractInfos :: [Core.TypeDefGroup] -> [Either SynInfo DataInfo]
extractInfos groups
  = concatMap extractGroupInfos groups
  where
    extractGroupInfos (Core.TypeDefGroup ctdefs)
      = map extractInfo ctdefs

    extractInfo (Core.Synonym synInfo vis)                 = Left synInfo
    extractInfo (Core.Data dataInfo vis convss isExtend)   = Right dataInfo

{---------------------------------------------------------------

---------------------------------------------------------------}
synTypeDefGroup :: Name -> Core.TypeDefGroup -> DefGroups Type
synTypeDefGroup modName (Core.TypeDefGroup ctdefs)
  = concatMap (synTypeDef modName) ctdefs

synTypeDef :: Name -> Core.TypeDef -> DefGroups Type
synTypeDef modName (Core.Synonym synInfo vis) = []
synTypeDef modName (Core.Data dataInfo vis conviss isExtend) | isHiddenName (dataInfoName dataInfo) = []
synTypeDef modName (Core.Data dataInfo vis conviss isExtend)
  = synAccessors modName dataInfo vis conviss
    ++
    (if (length (dataInfoConstrs dataInfo) == 1 && not (dataInfoIsOpen dataInfo) && not (isHiddenName (conInfoName (head (dataInfoConstrs dataInfo)))))
      then [synCopyCon modName dataInfo (head conviss) (head (dataInfoConstrs dataInfo))]
      else [])
    ++
    (if (length (dataInfoConstrs dataInfo) > 1 || (dataInfoIsOpen dataInfo))
      then concatMap (synTester dataInfo) (zip conviss (dataInfoConstrs dataInfo))
      else [])
    ++
    (if (dataInfoIsOpen dataInfo)
      then map synConstrTag (zip conviss (dataInfoConstrs dataInfo))
      else [])


synCopyCon :: Name -> DataInfo -> Visibility -> ConInfo -> DefGroup Type
synCopyCon modName info vis con
  = let rc = conInfoRange con
        tp = typeApp (TCon (TypeCon (dataInfoName info) (dataInfoKind info))) [TVar (TypeVar id kind Meta) | TypeVar id kind _ <- (dataInfoParams info)]

        fullTp = let (vars,preds,rho) = splitPredType (conInfoType con)
                 in case splitFunType rho of
                      Just (args,eff,res) -> TForall vars preds (TFun ([(argName,res)] ++ [(name,if name==nameNil then t else makeOptional t) | (name,t) <- args]) eff res)
                      Nothing             -> TForall vars preds (TFun [(argName,rho)] typeTotal rho)    -- for unary constructors, like unit

        var n = Var n False rc
        app x []  = x
        app x xs  = App x [(Nothing,y) | y <- xs] rc

        argName  = newName ".this"

        params = [ValueBinder name Nothing (if isFieldName name then Nothing else (Just (app (var name) [var argName]))) rc rc| (name,t) <- conInfoParams con]
        expr = Lam ([ValueBinder argName Nothing Nothing rc rc] ++ params) body rc
        body = app (var (conInfoName con)) [var name | (name,tp) <- conInfoParams con]
        def  = DefNonRec (Def (ValueBinder nameCopy () (Ann expr fullTp rc) rc rc) rc vis (DefFun NoMon) "")
    in def

synAccessors :: Name -> DataInfo -> Visibility -> [Visibility] -> [DefGroup Type]
synAccessors modName info vis conviss
  = let paramss = map (\conInfo -> zipWith (\(name,tp) (pvis,rng) -> (name,(tp,rng,pvis)))
                                   (conInfoParams conInfo) (zip (conInfoParamVis conInfo) (conInfoParamRanges conInfo)))
                      (dataInfoConstrs info)

        fields :: [(Name,(Type,Range,Visibility))]
        fields  = filter occursOnAll $
                  nubBy (\x y -> fst x == fst y) $
                  filter (not . isFieldName . fst) $
                  concat paramss

        occursOnAll (name,(tp,rng,pvis))
          = all (\ps -> any (\(n,(t,_,_)) -> n == name && t == tp) ps) paramss

        synAccessor :: (Name,(Type,Range,Visibility)) -> DefGroup Type
        synAccessor (name,(tp,rng,visibility))
          = let dataName = unqualify $ dataInfoName info
                arg = if (all isAlphaNum (show dataName))
                       then dataName else newName ".this"
                fld = newName ".x"

                fullTp = quantifyType (dataInfoParams info) $
                         typeFun [(arg,dataTp)] (if isPartial then typePartial else typeTotal) tp
                dataTp = typeApp (TCon (TypeCon (dataInfoName info) (dataInfoKind info))) (map TVar (dataInfoParams info))

                expr       = Ann (Lam [ValueBinder arg Nothing Nothing rng rng] caseExpr rng) fullTp rng
                caseExpr   = Case (Var arg False rng) (map snd branches ++ defaultBranch) rng
                -- visibility = if (all (==Public) (map fst branches)) then Public else Private

                isPartial = (length branches < length (dataInfoConstrs info)) || dataInfoIsOpen info

                branches :: [(Visibility,Branch Type)]
                branches = concatMap makeBranch (zip conviss (dataInfoConstrs info))
                makeBranch (vis,con)
                    = let r = conInfoRange con
                      in case lookup name (zip (map fst (conInfoParams con)) [0..]) of
                        Just i
                          -> let patterns = [(Nothing,PatWild r) | _ <- [0..i-1]] ++ [(Nothing,PatVar (ValueBinder fld Nothing (PatWild r) r r))] ++ [(Nothing,PatWild r) | _ <- [i+1..length (conInfoParams con)-1]]
                             in [(vis,Branch (PatCon (conInfoName con) patterns r r) guardTrue (Var fld False r))]
                        Nothing -> []
                defaultBranch
                  = if isPartial
                     then [Branch (PatWild rng) guardTrue (App (Var namePatternMatchError False rng) [(Nothing,msg) | msg <- messages] rng)]
                     else []
                messages
                  = [Lit (LitString (sourceName (posSource (rangeStart rng)) ++ show rng) rng), Lit (LitString (show name) rng)]
                doc = "// Automatically generated. Retrieves the `" ++ show name ++ "` constructor field of the `:" ++ nameId (dataInfoName info) ++ "` type.\n"
            in DefNonRec (Def (ValueBinder name () expr rng rng) rng visibility (DefFun NoMon) doc)

    in map synAccessor fields

synTester :: DataInfo -> (Visibility,ConInfo) -> [DefGroup Type]
synTester info (vis,con) | isHiddenName (conInfoName con)
  = []
synTester info (vis,con)
  = let name = (postpend "?" (toVarName (unqualify (conInfoName con))))
        arg = unqualify $ dataInfoName info
        rc  = conInfoRange con

        expr      = Lam [ValueBinder arg Nothing Nothing rc rc] caseExpr rc
        caseExpr  = Case (Var arg False rc) [branch1,branch2] rc
        branch1   = Branch (PatCon (conInfoName con) patterns rc rc) guardTrue (Var nameTrue False rc)
        branch2   = Branch (PatWild rc) guardTrue (Var nameFalse False rc)
        patterns  = [(Nothing,PatWild rc) | _ <- conInfoParams con]
        doc = "// Automatically generated. Tests for the `" ++ nameId (conInfoName con) ++ "` constructor of the `:" ++ nameId (dataInfoName info) ++ "` type.\n"
    in [DefNonRec (Def (ValueBinder name () expr rc rc) rc vis (DefFun NoMon) doc)]

synConstrTag :: (Visibility,ConInfo) -> DefGroup Type
synConstrTag (vis,con)
  = let name = toOpenTagName (unqualify (conInfoName con))
        rc   = conInfoRange con
        expr = Lit (LitString (show (conInfoName con)) rc)
    in DefNonRec (Def (ValueBinder name () expr rc rc) rc vis DefVal "")

{---------------------------------------------------------------
  Types for constructors
---------------------------------------------------------------}
constructorGamma :: Int -> [DataInfo] -> Gamma
constructorGamma maxStructFields dataInfos
  = conInfoGamma (concatMap (\info -> zip (dataInfoConstrs info) (snd (Core.getDataRepr maxStructFields info))) dataInfos)
  where
    conInfoGamma conInfos
      = gammaNew [(conInfoName conInfo,InfoCon (conInfoType conInfo) conRepr conInfo (conInfoRange conInfo)) | (conInfo,conRepr) <- conInfos]

constructorCheckDuplicates :: ColorScheme -> [ConInfo] -> [(Range,Doc)]
constructorCheckDuplicates cscheme conInfos
  = concatMap duplicate $ groupBy sameName conInfos
  where
    sameName ci1 ci2
      = conInfoName ci1 == conInfoName ci2

    duplicate (ci1:ci2:_)
      = [(conInfoRange ci2
         ,text "Constructor" <+> color (colorSource cscheme) (pretty (conInfoName ci2)) <+>
          text "is already defined at" <+> text (show (conInfoRange ci1)))]
    duplicate _
      = []

{---------------------------------------------------------------
  Infer kinds for type definition groups
---------------------------------------------------------------}
infTypeDefGroups :: [TypeDefGroup UserType UserKind] -> KInfer ([Core.TypeDefGroup],KGamma,Synonyms)
infTypeDefGroups (tdgroup:tdgroups)
  = do (ctdgroup)  <- infTypeDefGroup tdgroup
       (ctdgroups,kgamma,syns) <- extendKGamma (getRanges tdgroup) ctdgroup $ infTypeDefGroups tdgroups
       return (ctdgroup:ctdgroups,kgamma,syns)
  where
    getRanges (TypeDefRec tdefs)   = map getRange tdefs
    getRanges (TypeDefNonRec tdef) = [getRange tdef]

infTypeDefGroups []
  = do kgamma <- getKGamma
       syns <- getSynonyms
       return ([],kgamma,syns)

infTypeDefGroup :: TypeDefGroup UserType UserKind -> KInfer (Core.TypeDefGroup)
infTypeDefGroup (TypeDefRec tdefs)
  = infTypeDefs True tdefs

infTypeDefGroup (TypeDefNonRec tdef)
  = infTypeDefs False [tdef]

infTypeDefs isRec tdefs
  = do -- trace ("infTypeDefs: " ++ show (length tdefs)) $ return ()
       xinfgamma <- mapM bindTypeDef tdefs -- set up recursion
       let infgamma = map fst (filter snd xinfgamma)
       ctdefs   <- extendInfGamma infgamma $ -- extend inference gamma, also checks for duplicates
                   do let names = map tbinderName infgamma
                      tdefs1 <- mapM infTypeDef (zip (map fst xinfgamma) tdefs)
                      mapM (resolveTypeDef isRec names) tdefs1
       checkRecursion tdefs -- check for recursive type synonym definitions rather late so we spot duplicate definitions first
       return (Core.TypeDefGroup ctdefs)

checkRecursion :: [TypeDef UserType UserType UserKind] -> KInfer ()
checkRecursion tdefs
  = if (length tdefs <= 1 || any isDataType tdefs)
     then return ()
     else do addError (getRange tdefs) (text "Type synonyms cannot be recursive")
             return ()
  where
    isDataType (DataType{}) = True
    isDataType _            = False


{---------------------------------------------------------------
  Setup type environment for recursive definitions
---------------------------------------------------------------}
bindTypeDef :: TypeDef UserType UserType UserKind -> KInfer (TypeBinder InfKind,Bool {-not isExtend-})
bindTypeDef tdef -- extension
  = do (TypeBinder name kind rngName rng) <- bindTypeBinder (typeDefBinder tdef)
       qname <- if isExtend then return name else qualifyDef name
       return (TypeBinder qname kind rngName rng, not isExtend)
  where
    isExtend =
      case tdef of
        (DataType newtp args constructors range vis sort ddef isExtend doc) -> isExtend
        _ -> False

bindTypeBinder :: TypeBinder UserKind -> KInfer (TypeBinder InfKind)
bindTypeBinder (TypeBinder name userKind rngName rng)
  = do kind <- userKindToKind userKind
       return (TypeBinder name kind rngName rng)

userKindToKind :: UserKind -> KInfer InfKind
userKindToKind userKind
  = convert userKind
  where
    convert userKind
      = case userKind of
          KindCon name rng -> return $ KICon (KCon name)
          KindArrow k1 k2  -> do k1' <- convert k1
                                 k2' <- convert k2
                                 case (k1',k2') of
                                   (KICon kk1,KICon kk2) -> return (KICon (kindFun kk1 kk2))
                                   _ -> return $ KIApp k1' k2'
          KindParens k rng -> convert k
          KindNone         -> freshKind -- failure ("Kind.Infer.userKindToKind.convert: unexpected kindNone")


{---------------------------------------------------------------
  Infer kinds of external definitions
---------------------------------------------------------------}
infExternals :: [External] -> KInfer Core.Externals
infExternals externals
  = walk [] externals
  where
    walk names [] = return []
    walk names (external:externals)
      = do (ext,names2)  <- infExternal names external
           exts <- walk names2 externals
           return (ext:exts)

infExternal :: [Name] -> External -> KInfer (Core.External,[Name])
infExternal names (External name tp nameRng rng calls vis doc)
  = do tp' <- infResolveType tp (Check "Externals must be values" rng)
       qname <- qualifyDef name
       let cname = let n = length (filter (==qname) names) in
                   Core.canonicalName n qname
       if (isHiddenName name)
        then return ()
        else do addRangeInfo nameRng (Id qname (NIValue tp') True)
                addRangeInfo rng (Decl "external" qname (mangle cname tp'))
       -- trace ("infExternal: " ++ show cname ++ ": " ++ show (pretty tp')) $
       return (Core.External cname tp' (map (formatCall tp') calls)
                  vis nameRng doc, qname:names)
infExternal names (ExternalInclude include range)
  = return (Core.ExternalInclude include range, names)
infExternal names (ExternalImport imports range)
  = return (Core.ExternalImport imports range, names)

formatCall tp (target,ExternalInline inline) = (target,inline)
formatCall tp (target,ExternalCall fname)
  = case target of
      CS      -> (target,formatCS)
      JS      -> (target,formatJS)
      Default -> (target,formatJS)
  where
    (foralls,preds,rho) = splitPredType tp

    argumentCount
      = case splitFunType rho of
          Just (args,eff,res) -> length args
          Nothing             -> 0

    arguments
      = "(" ++ concat (intersperse "," ["#" ++ show i | i <- [1..argumentCount]]) ++ ")"

    typeArguments
      = if null foralls then ""
        else ("<" ++ concat (intersperse "," ["##" ++ show i | i <- [1..length foralls]]) ++ ">")

    formatJS
      = fname ++ arguments

    formatCS
      = fname ++ typeArguments ++ arguments



infResolveType :: UserType -> Context -> KInfer Type
infResolveType tp ctx
  = do infTp <- infUserType infKindStar ctx tp
       resolveType M.empty False infTp

infResolveHX :: UserType -> Context -> KInfer Type
infResolveHX tp ctx
  = do infTp <- infUserType infKindHandled ctx tp
       resolveType M.empty False infTp

infResolveX :: UserType -> Context -> Range -> KInfer Type
infResolveX tp ctx rng
  = do ikind <- freshKind
       infTp <- infUserType ikind ctx tp
       skind <- subst ikind
       -- allow also effect label constructors without giving type parameters
       let (kargs,kres) = infExtractKindFun skind
       if (not (null kargs))
        then let vars     = [(newName ("_" ++ show i)) | (_,i) <- zip kargs [1..]]
                 quals    = map (\(name) -> TypeBinder name KindNone rng rng) vars
                 tpvars   = map (\(name) -> TpVar name rng) vars
                 newtp    = foldr (\q t -> TpQuan QSome q t rng) (TpApp tp tpvars rng) quals
             in infResolveX newtp ctx rng -- recurse..
       -- auto upgrade bare labels of HX or HX1 to X kind labels.
        else do effect <- case skind of
                            KICon kind | kind == kindLabel   -> return infTp
                            KICon kind | isKindHandled kind  -> return (makeHandled infTp rng)
                            KICon kind | isKindHandled1 kind -> return (makeHandled1 infTp rng)
                            _ -> do unify ctx rng infKindLabel skind
                                    return infTp
                resolveType M.empty False effect



{---------------------------------------------------------------
  Infer kinds of definitions
---------------------------------------------------------------}
infDefGroups :: [DefGroup UserType] -> KInfer [DefGroup Type]
infDefGroups defgroups
  = mapM infDefGroup defgroups

infDefGroup :: DefGroup UserType -> KInfer (DefGroup Type)
infDefGroup (DefRec defs)
  = do defs' <- mapM infDef defs
       return (DefRec defs')
infDefGroup (DefNonRec def)
  = do def' <- infDef def
       return (DefNonRec def')

infDef :: (Def UserType) -> KInfer (Def Type)
infDef (Def binder rng vis isVal doc)
  = do binder' <- infValueBinder binder
       return (Def binder' rng vis isVal doc)

infValueBinder (ValueBinder name () expr nameRng rng)
  = do expr'   <- infExpr expr
       return (ValueBinder name () expr' nameRng rng)


infLamValueBinder (ValueBinder name mbTp mbExpr nameRng rng)
  = do mbTp' <- case mbTp of
                  Nothing -> return Nothing
                  Just tp -> do tp' <- infResolveType tp (Check "Function parameters must be values" rng)
                                return (Just tp')
       mbExpr' <- case mbExpr of
                  Nothing -> return Nothing
                  Just expr -> do expr' <- infExpr expr
                                  return (Just expr')
       return (ValueBinder name mbTp' mbExpr' nameRng rng)

infPatValueBinder (ValueBinder name mbTp pat nameRng rng)
  = do mbTp' <- case mbTp of
                  Nothing -> return Nothing
                  Just tp -> do tp' <- infResolveType tp (Check "Matched expressions must be values" rng)
                                return (Just tp')
       pat'  <- infPat pat
       return (ValueBinder name mbTp' pat' nameRng rng)

infHandlerValueBinder (ValueBinder name mbTp () nameRng rng)
  = do mbTp' <- case mbTp of
                  Nothing -> return Nothing
                  Just tp -> do tp' <- infResolveType tp (Check "Handler parameters must be values" rng)
                                return (Just tp')
       return (ValueBinder name mbTp' () nameRng rng)


infExpr :: Expr UserType -> KInfer (Expr Type)
infExpr expr
  = case expr of
      Lam    binds expr rng  -> do binds' <- mapM infLamValueBinder binds
                                   expr' <- infExpr expr
                                   return (Lam binds' expr' rng)
      Let    defs expr range -> do defs' <- infDefGroup defs
                                   expr' <- infExpr expr
                                   return (Let defs' expr' range)
      Bind   def expr range  -> do def' <- infDef def
                                   expr' <- infExpr expr
                                   return (Bind def' expr' range)
      App    fun nargs range -> do fun'   <- infExpr fun
                                   let (names,exprs) = unzip nargs
                                   exprs' <- mapM infExpr exprs
                                   return (App fun' (zip names exprs') range)
      Var    name isOp range -> do name' <- infQualifiedName name range
                                   return (Var name' isOp range)
      Lit    lit             -> return (Lit lit)
      Ann    expr tp range   -> do expr' <- infExpr expr
                                   tp'   <- infResolveType tp (Check "Expressions must be values" range)
                                   -- trace ("resolve ann: " ++ show (pretty tp')) $
                                   return (Ann expr' tp' range)
      Case   expr brs range  -> do expr' <- infExpr expr
                                   brs'   <- mapM infBranch brs
                                   return (Case expr' brs' range)
      Parens expr range      -> do expr' <- infExpr expr
                                   return (Parens expr' range)
      Handler hsort scoped meff pars reinit ret final ops hrng rng
                             -> do pars' <- mapM infHandlerValueBinder pars
                                   meff' <- case meff of
                                              Nothing  -> return Nothing
                                              Just eff -> do eff' <- infResolveX eff (Check "Handler types must be effect constants (of kind X)" hrng) hrng
                                                             return (Just eff')
                                   hsort' <- case hsort of
                                               HandlerResource (Just rexpr)
                                                -> do rexpr' <- infExpr rexpr
                                                      return (HandlerResource (Just rexpr'))
                                               HandlerResource Nothing -> return $ HandlerResource Nothing
                                               HandlerShallow -> return HandlerShallow
                                               HandlerDeep -> return HandlerDeep
                                   ret' <- infExpr ret
                                   reinit' <- infExpr reinit
                                   final'  <- infExpr final
                                   ops' <- mapM infHandlerBranch ops
                                   return (Handler hsort' scoped meff' pars' reinit' ret' final' ops' hrng rng)
      Inject tp expr range  -> do expr' <- infExpr expr
                                  tp'   <- infResolveX tp (Check "Can only inject effect constants (of kind X)" range) range
                                  -- trace ("resolve ann: " ++ show (pretty tp')) $
                                  return (Inject tp' expr' range)


infPat pat
  = case pat of
      PatWild range           -> return (PatWild range)
      PatLit lit              -> return (PatLit lit)
      PatVar  binder          -> do binder' <- infPatValueBinder binder
                                    return (PatVar binder')
      PatAnn  pat tp range    -> do pat' <- infPat pat
                                    tp'  <- infResolveType tp (Check "Patterns must be values" range)
                                    return (PatAnn pat' tp' range)
      PatCon  name args r1 r2 -> do args' <- mapM (\(mbName,pat) -> do pat' <- infPat pat; return (mbName,pat')) args
                                    return (PatCon name args' r1 r2)
      PatParens pat range     -> do pat' <- infPat pat
                                    return (PatParens pat' range)


infHandlerBranch (HandlerBranch name pars expr nameRng rng)
  = do pars' <- mapM infHandlerValueBinder pars
       expr' <- infExpr expr
       return (HandlerBranch name pars' expr' nameRng rng)

infBranch (Branch pattern guard body)
  = do pattern'<- infPat pattern
       guard'  <- infExpr guard
       body'   <- infExpr body
       return (Branch pattern' guard' body')


{---------------------------------------------------------------
  Infer the kinds for a type definition
---------------------------------------------------------------}
infTypeDef :: (TypeBinder InfKind, TypeDef UserType UserType UserKind) -> KInfer (TypeDef (KUserType InfKind) UserType InfKind)
infTypeDef (tbinder, Synonym syn args tp range vis doc)
  = do infgamma <- mapM bindTypeBinder args
       kind <- freshKind
       tp' <- extendInfGamma infgamma (infUserType kind (Infer range) tp)
       tbinder' <- unifyBinder tbinder syn range infgamma kind
       return (Synonym tbinder' infgamma tp' range vis doc)

infTypeDef (tbinder, td@(DataType newtp args constructors range vis sort ddef isExtend doc))
  = -- trace ("inf datatype: " ++ show (tbinderName newtp)) $
    do infgamma <- mapM bindTypeBinder args
       constructors' <- extendInfGamma infgamma (mapM infConstructor constructors)
       -- todo: unify extended datatype kind with original
       reskind <- if dataDefIsOpen ddef then return infKindStar else freshKind
       tbinder' <- unifyBinder tbinder newtp range infgamma reskind
       if not isExtend then return ()
        else do (qname,kind) <- findInfKind (tbinderName newtp) (tbinderRange newtp)
                unify (Check "extended type must have the same kind as the open type" (tbinderRange newtp) ) (tbinderRange newtp) (typeBinderKind tbinder') kind
       return (DataType tbinder' infgamma constructors' range vis sort ddef isExtend doc)

unifyBinder tbinder defbinder range infgamma reskind
 = do let kind = infKindFunN (map typeBinderKind infgamma) reskind
      unify (Infer range) range (typeBinderKind tbinder) kind
      return tbinder

typeBinderKind (TypeBinder name kind _ _) = kind

infConstructor :: UserCon UserType UserType UserKind -> KInfer (UserCon (KUserType InfKind) UserType InfKind)
infConstructor (UserCon name exist params rngName rng vis doc)
  = do infgamma <- mapM bindTypeBinder exist
       params'  <- extendInfGamma infgamma (mapM infConValueBinder params)
       return (UserCon name infgamma params' rngName rng vis doc)

infConValueBinder :: (Visibility,ValueBinder UserType (Maybe (Expr UserType))) -> KInfer (Visibility,ValueBinder (KUserType InfKind) (Maybe (Expr UserType)))
infConValueBinder (vis,ValueBinder name tp mbExpr nameRng rng)
  = do tp' <- infUserType infKindStar (Check "Constructor parameters must be values" rng) tp
       return (vis,ValueBinder name tp' mbExpr nameRng rng)


infUserType :: InfKind -> Context -> UserType -> KInfer (KUserType InfKind)
infUserType expected  context userType
  = let range = getRange userType in
    case userType of
      TpQuan quant tname tp rng
        -> do ikind  <- case quant of
                          QSome -> return expected
                          _     -> do unify context range expected infKindStar
                                      return expected
              tname' <- bindTypeBinder tname
              tp'    <- extendInfGamma [tname'] $
                        infUserType ikind (checkQuant range) tp
              return (TpQuan quant tname' tp' rng)
      TpQual preds tp
        -> do preds' <- mapM (infUserType (KICon kindPred) (checkPred range)) preds
              tp'    <- infUserType expected context tp
              return (TpQual preds' tp')
      TpFun args effect tp rng
        -> do unify context range expected infKindStar
              args'   <- mapM (infParam infKindStar (checkArg range)) args
              -- somewhat involved since we auto-wrap labels L into effects E here
              ekind   <- freshKind
              etp     <- infUserType ekind (checkEff range) effect
              skind   <- subst ekind
              effect' <- case skind of
                          KICon kind | kind == kindLabel -> return (makeEffectExtend etp makeEffectEmpty)
                          KICon kind | isKindHandled kind ->  -- TODO: check if there is an effect declaration
                                          return (makeEffectExtend (makeHandled etp rng) makeEffectEmpty)
                          KICon kind | isKindHandled1 kind ->  -- TODO: check if there is an effect declaration
                                          return (makeEffectExtend (makeHandled1 etp rng) makeEffectEmpty)
                          _  -> do unify (checkEff range) range (KICon kindEffect) skind
                                   return etp
              tp'     <- infUserType infKindStar (checkRes range) tp
              return (TpFun args' effect' tp' rng)

      TpApp tp@(TpCon name _) [lab,tl] rng  | name == nameEffectExtend
        -> do tp'   <- infUserType (infKindFunN [KICon kindLabel,KICon kindEffect] expected) (checkApp range) tp
              tl'   <- infUserType (KICon kindEffect) (checkExtendTail range) tl
              -- somewhat involved since we allow fixed effects to be used as labels
              -- the append nodes are just used temporarily (see resolveApp in this file)
              lkind <- freshKind
              ltp   <- infUserType lkind (Infer range) lab
              skind <- subst lkind
              case skind of
                KICon kind | kind == kindEffect
                  -> return (makeEffectAppend ltp tl')
                KICon kind | isKindHandled kind -- TODO: check effects environment if really effect?
                  -> do unify (checkExtendLabel range) range (KICon kindHandled) skind
                        return (TpApp tp' [makeHandled ltp rng, tl'] rng)
                KICon kind | isKindHandled1 kind -- TODO: check effects environment if really effect?
                  -> do unify (checkExtendLabel range) range (KICon kindHandled1) skind
                        return (TpApp tp' [makeHandled1 ltp rng, tl'] rng)
                _ -> do unify (checkExtendLabel range) range (KICon kindLabel) skind
                        return (TpApp tp' [ltp,tl'] rng)

      TpApp tp args rng
        -> do kinds <- mapM (\arg -> freshKind) args
              tp'   <- infUserType (infKindFunN kinds expected) (checkApp range) tp
              args' <- mapM (\(kind,arg) -> infUserType kind (Infer range) arg) (zip kinds args)
              return (TpApp tp' args' rng)
      TpVar name rng
        -> do (qname,kind) <- findInfKind name rng
              unify context range expected kind
              return (TpVar qname rng)
      TpCon name rng
        -> do (qname,kind) <- findInfKind name rng
              unify context range expected kind
              return (TpCon qname rng)
      TpParens tp rng
        -> do tp' <- infUserType expected context tp
              return (TpParens tp' rng)
      TpAnn tp userKind
        -> do kind <- userKindToKind userKind
              unify context range expected kind
              tp' <- infUserType kind (checkAnnot range) tp
              return (TpAnn tp' kind)

infParam expected context (name,tp)
  = do tp' <- infUserType expected context tp
       return (name,tp')

checkQuant range  = Check "Can only quantify over types" range
checkPred range   = Check "The left-hand side of a \"=>\" can only contain predicates" range
checkArg range    = Check "The parameters of a function type must be types" range
checkEff range    = Check "The effect of a function type must be an effect type" range
checkRes range    = Check "The result of a function type must be a type" range
checkAnnot range  = Check "The inferred kind doesn't match the annotated kind" range
checkApp range    = Check "The type cannot be applied to those arguments" range
checkExtendTail r = Check "The extension of an effect must be an effect type" r
checkExtendLabel r= Check "The elements of an effect must be effect constants" r

{---------------------------------------------------------------
  Resolve kinds: from InfKind to Kind, and UserType to Type
---------------------------------------------------------------}
resolveTypeDef :: Bool -> [Name] -> TypeDef (KUserType InfKind) UserType InfKind -> KInfer (Core.TypeDef)
resolveTypeDef isRec recNames (Synonym syn params tp range vis doc)
  = do syn' <- resolveTypeBinderDef syn
       params' <- mapM resolveTypeBinder params
       typeVars  <- mapM (\param -> freshTypeVar param Bound) params'
       let tvarMap = M.fromList (zip (map getName params') typeVars)
       tp' <- resolveType tvarMap True tp
       -- eta-expand type synonyms
       let kind = typeBinderKind syn'
           arity = kindArity kind
           etaKinds = drop (length typeVars) arity

       (etaTp,etaParams)
          <- if (null etaKinds)
              then return (tp',typeVars)
              else do etaVars <- mapM (\kind -> do id <- uniqueId "eta"; return (TypeVar id kind Bound)) etaKinds
                      return (typeApp tp' (map TVar etaVars), typeVars ++ etaVars)

       -- trace (showTypeBinder syn') $
       addRangeInfo range (Decl "alias" (getName syn') (mangleTypeName (getName syn')))
       let synInfo = SynInfo (getName syn') (typeBinderKind syn') etaParams etaTp (maxSynonymRank etaTp + 1) range doc
       return (Core.Synonym synInfo vis)
  where
    kindArity (KApp (KApp kcon k1) k2)  | kcon == kindArrow = k1 : kindArity k2
    kindArity _ = []

resolveTypeDef isRec recNames (DataType newtp params constructors range vis sort ddef isExtend doc)
  = do -- trace ("datatype: " ++ show(tbinderName newtp) ++ " " ++ show isExtend) $ return ()
       newtp' <- if isExtend
                  then do (qname,ikind) <- findInfKind (tbinderName newtp) (tbinderRange newtp)
                          kind  <- resolveKind ikind
                          -- addRangeInfo range (Id qname (NITypeCon kind) False)
                          return (TypeBinder qname kind (tbinderNameRange newtp) (tbinderRange newtp))
                  else resolveTypeBinderDef newtp
       params' <- mapM resolveTypeBinder params
       let typeResult = TCon (TypeCon (getName newtp') (typeBinderKind newtp'))
       typeVars  <- let (kargs,kres) = extractKindFun (typeBinderKind newtp')
                    in if (null params' && not (null kargs))
                     then mapM (\karg -> do{ id <- uniqueId "k"; return (TypeVar id karg Bound) }) kargs  -- invent parameters if they are not given (and it has an arrow kind)
                     else mapM (\param -> freshTypeVar param Bound) params'
       let tvarMap = M.fromList (zip (map getName params') typeVars)
       consinfos <- mapM (resolveConstructor (getName newtp') sort (not (dataDefIsOpen ddef) && length constructors == 1) typeResult typeVars tvarMap) constructors
       let (constructors',infos) = unzip consinfos
       if (sort == Retractive)
        then return ()
        else if (any (occursNegative recNames) (concatMap (map snd . conInfoParams) infos))
              then do cs <- getColorScheme
                      addError range (text "Type" <+> color (colorType cs) (pretty (unqualify (getName newtp'))) <+> text "is declared as being (co)inductive but it occurs" <-> text " recursively in a negative position." <->
                                     text " hint: declare it as a 'rectype' to allow negative occurrences")
              else return ()
       -- trace (showTypeBinder newtp') $
       addRangeInfo range (Decl (show sort) (getName newtp') (mangleTypeName (getName newtp')))
       let ddef' = case ddef of
                     DataDefNormal | isRec -> DataDefRec
                     _ -> ddef
           dataInfo = DataInfo sort (getName newtp') (typeBinderKind newtp') typeVars infos range ddef' doc
       return (Core.Data dataInfo vis (map conVis constructors) isExtend)
  where
    conVis (UserCon name exist params rngName rng vis _) = vis

occursNegative :: [Name] -> Type -> Bool
occursNegative names tp
  = occurs names False tp

occurs :: [Name] -> Bool -> Type -> Bool
occurs names isNeg tp
  = case tp of
      TForall vars preds tp   -> occurs names isNeg tp
      TFun args effect result -> any (occurs names (not isNeg)) (map snd args) || occurs names (not isNeg) effect || occurs names isNeg result
      TCon tcon               -> if (typeConName tcon `elem` names) then isNeg else False
      TVar tvar               -> False
      TApp tp args            -> any (occurs names isNeg) (tp:args)
      TSyn syn xs tp          -> occurs names isNeg tp


showTypeBinder :: TypeBinder Kind -> String
showTypeBinder (TypeBinder name kind _ _)
  = show name ++ ": " ++ show kind


resolveTypeBinderDef :: TypeBinder InfKind -> KInfer (TypeBinder Kind)
resolveTypeBinderDef (TypeBinder name infkind rngName rng)
  = do infkind' <- resolveKind infkind
       qname    <- qualifyDef name
       addRangeInfo rngName (Id qname (NITypeCon infkind') True)
       return (TypeBinder qname infkind' rngName rng)

resolveTypeBinder :: TypeBinder InfKind -> KInfer (TypeBinder Kind)
resolveTypeBinder (TypeBinder name infkind rngName rng)
  = do infkind' <- resolveKind infkind
       addRangeInfo rngName (Id name (NITypeCon infkind') True)
       return (TypeBinder name infkind' rngName rng)

resolveKind :: InfKind -> KInfer Kind
resolveKind infkind
  = do skind <- subst infkind
       return (resolve skind)
  where
    resolve (KIVar id)   = kindStar  -- default unconstrained parameter to kind star
    resolve (KICon kind) = kind
    resolve (KIApp k1 k2) = KApp (resolve k1) (resolve k2)

resolveConstructor :: Name -> DataKind -> Bool -> Type -> [TypeVar] -> M.NameMap TypeVar -> UserCon (KUserType InfKind) UserType InfKind -> KInfer (UserCon Type Type Kind, ConInfo)
resolveConstructor typeName typeSort isSingleton typeResult typeParams idmap (UserCon name exist params rngName rng vis doc)
  = do qname  <- qualifyDef name
       exist' <- mapM resolveTypeBinder exist
       existVars <- mapM (\ename -> freshTypeVar ename Bound) exist'
       let idmap' = M.union (M.fromList (zip (map getName exist) existVars)) idmap  -- ASSUME: left-biased union
       params' <- mapM (resolveConParam idmap') params -- mapM (resolveType idmap' False) params
       let result = typeApp typeResult (map TVar typeParams)
           scheme = quantifyType (typeParams ++ existVars) $
                    if (null params') then result else typeFun [(binderName p, binderType p) | (_,p) <- params'] typeTotal result
       addRangeInfo rngName (Id qname (NICon scheme) True)
       addRangeInfo rng (Decl "con" qname (mangleConName qname))
       return (UserCon qname exist' params' rngName rng vis doc
              ,ConInfo qname typeName typeParams existVars
                  (map (\(i,b) -> (if (nameIsNil (binderName b)) then newFieldName i else binderName b, binderType b)) (zip [1..] (map snd params')))
                  scheme
                  typeSort rngName
                  (map (binderNameRange . snd) params')
                  (map fst params')
                  isSingleton
                  doc)

resolveConParam :: M.NameMap TypeVar -> (Visibility,ValueBinder (KUserType InfKind) (Maybe (Expr UserType))) -> KInfer (Visibility,ValueBinder Type (Maybe (Expr Type)))
resolveConParam idmap (vis,vb)
  = do tp <- resolveType idmap False (binderType vb)
       expr <- case (binderExpr vb) of
                 Nothing -> return Nothing
                 Just e  -> {- do e' <- infExpr e
                                  return (Just e') -}
                            return (Just (failure "Kind.Infer.resolveConParam: optional parameter expression in constructor"))
       addRangeInfo (binderNameRange vb) (Id (binderName vb) (NIValue tp) True)
       return (vis,vb{ binderType = tp, binderExpr = expr })

-- | @resolveType@ takes: a map from locally quantified type name variables to types,
-- a boolean that is 'True' if partially applied type synonyms are allowed (i.e. when
-- these are arguments to type synonyms themselves), a user type with inference kinds,
-- and it returns a fully resolved type.
resolveType :: M.NameMap TypeVar -> Bool -> KUserType InfKind -> KInfer Type
resolveType idmap partialSyn userType
  = case userType of
      TpQuan QForall tname tp rng
        -> do tname' <- resolveTypeBinder tname
              tvar   <- freshTypeVar tname' Bound
              tp'    <- resolveType (M.insert (getName tname) tvar idmap) False tp
              return (quantifyType [tvar] tp')
      TpQuan QSome tname tp rng
        -> do tname' <- resolveTypeBinder tname
              tvar   <- freshTypeVar tname' Meta
              tp'    <- resolveType (M.insert (getName tname) tvar idmap) False tp
              -- trace ("Kind.Infer.Some") $
              return tp'
      TpQuan QExists tname tp rng
        -> todo "Kind.Infer.resolveType: existentials are not supported yet"
      TpQual preds tp
        -> do preds' <- mapM (resolvePredicate idmap) preds
              tp'    <- resolveType idmap False tp
              return (qualifyType preds' tp')
      TpFun args effect tp rng
        -> do args' <- mapM resolveParam args
              effect' <- resolveType idmap False effect
              tp'     <- resolveType idmap False tp
              return (TFun args' effect' tp')
      TpApp tp args rng
        -> resolveApp idmap partialSyn (collectArgs tp args) rng
      TpVar name rng
        -> resolveApp idmap partialSyn (userType,[]) rng
      TpCon name rng
        -> resolveApp idmap partialSyn (userType,[]) rng
      TpParens tp rng
        -> resolveType idmap partialSyn tp
      TpAnn tp userKind
        -> resolveType idmap partialSyn tp

  where
    resolveParam (name,tp)
      = do tp' <- resolveType idmap False tp
           return (name,tp')

    collectArgs tp args
      = case tp of
          TpApp tp' args' _ -> collectArgs tp' (args' ++ args)
          TpParens tp' _    -> collectArgs tp' args
          TpAnn tp' _       -> collectArgs tp' args
          _                 -> (tp, args)

resolvePredicate :: M.NameMap TypeVar -> KUserType InfKind -> KInfer Pred
resolvePredicate idmap tp
  = do tp' <- resolveType idmap False tp
       case tp' of
         TApp (TCon tc) targs
            -> return (PredIFace (typeconName tc) targs)
         TCon tc
            -> return (PredIFace (typeconName tc) [])
         _  -> failure ("Kind.Infer.resolvePredicate: invalid predicate: " ++ show tp')


resolveApp idmap partialSyn (TpVar name r,args) rng
  = do (tp',kind) <- case M.lookup name idmap of
                      Nothing   -> do cs <- getColorScheme
                                      -- failure ("Kind.Infer.ResolveApp: cannot find: " ++ show name ++ " at " ++ show rng)
                                      addError rng (text "Type variable" <+> color (colorType cs) (pretty name) <+> text "is undefined" <->
                                                    text " hint: bind the variable using" <+> color (colorType cs) (text "forall<" <> pretty name <> text ">"))
                                      id <- uniqueId (show name)
                                      return (TVar (TypeVar id kindStar Bound), kindStar)

                      Just tvar -> return (TVar tvar, typevarKind tvar)
       if (not (isImplicitTypeVarName name))
        then addRangeInfo r (Id name (NITypeVar kind) False)
        else return ()
       args' <- mapM (resolveType idmap False) args
       return (typeApp tp' args')

resolveApp idmap partialSyn (TpCon name r,[fixed,ext]) rng  | name == nameEffectAppend
  = do fixed' <- resolveType idmap False fixed
       ext'   <- resolveType idmap False ext
       let (ls,tl) = extractOrderedEffect fixed'
       if isEffectEmpty tl
        then return ()
        else addError rng (text "Effects can only have one extension point (use a `|` instead of a comma in the effect type ?)")
       return (shallowEffectExtend fixed' ext')

resolveApp idmap partialSyn (TpCon name r,args) rng
  =  do (qname,ikind) <- findInfKind name rng
        kind  <- resolveKind ikind
        addRangeInfo r (Id qname (NITypeCon kind) False)

        mbSyn <- lookupSynInfo name
        case mbSyn of
          Just syn@(SynInfo name kind params tp rank range doc)
            -> do -- check over/under application
                  if (not partialSyn && length args < length params)
                   then do cs <- getColorScheme
                           addError rng (text "Type alias" <+> color (colorType cs) (pretty name) <+> text "has too few arguments")
                   else if (length args > length params)
                    then do cs <- getColorScheme
                            addError rng (text "Type alias" <+> color (colorType cs) (pretty name) <+> text "has too many arguments")
                    else return ()
                  args' <- mapM (resolveType idmap True) args    -- partially applied synonyms are allowed in synonym applications
                  let tsyn = (TSyn (TypeSyn name kind rank (Just syn)) args' (subNew (zip params args') |-> tp))
                  -- trace ("resolved type syn: " ++ show (pretty syn)) $
                  return tsyn
                  -- NOTE: on partially applied type synonyms, we get a funky body type with free parameters but this
                  -- is only inside synonyms arguments so we are ok.
          Nothing
            -> do args' <- mapM (resolveType idmap False) args
                  return (typeApp (TCon (TypeCon name kind)) args')

resolveApp idmap partialSyn _ rng
  = failure "Kind.Infer.resolveApp: this case should never occur after kind checking"


makeEffectAppend fixed ext
  = TpApp (TpCon nameEffectAppend rangeNull) [fixed,ext] rangeNull

makeEffectExtend (label) ext
  = TpApp (TpCon nameEffectExtend rangeNull) [label,ext] rangeNull

makeEffectEmpty
  = TpCon nameEffectEmpty rangeNull

makeHandled u rng
  = TpApp (TpCon nameTpHandled rangeNull) [u] rangeNull

makeHandled1 u rng
  = TpApp (TpCon nameTpHandled1 rangeNull) [u] rangeNull

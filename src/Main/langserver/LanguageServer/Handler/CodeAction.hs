------------------------------------------------------------------------------
-- Copyright 2024, Tim Whiting
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module LanguageServer.Handler.CodeAction(codeActionHandler) where
import Language.LSP.Server (Handlers, requestHandler)
import qualified Data.Map.Strict as M
import GHC.Unicode (isAlphaNum)
import Data.List (find, intersperse, foldl', intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import LanguageServer.Monad
import LanguageServer.Conversions
import Syntax.RangeMap
import Syntax.Syntax
import Syntax.Pretty
import Common.NamePrim
import Common.Name
import Common.Range
import Common.IdNice
import Common.Syntax
import Common.Error
import Kind.Kind
import Kind.Newtypes
import Kind.Constructors
import Type.Type
import Type.Pretty
import qualified Type.TypeVar as TV
import qualified Core.Core as Core
import Compile.Module (modCoreImports)
import Lib.Trace (trace)

-- Handles codeAction requests
codeActionHandler :: Handlers LSM
codeActionHandler
  = requestHandler J.SMethod_TextDocumentCodeAction $ \req responder ->
    do let J.CodeActionParams _ _ docid rng0 _ = req ^. J.params
           origuri = (docid ^. J.uri)
           uri = J.toNormalizedUri origuri

           done :: LSM ()
           done = responder $ Right $ J.InR J.Null

           liftMaybe :: LSM (Maybe a) -> (a -> LSM ()) -> LSM ()
           liftMaybe action next = do res <- action
                                      maybe done next res
       rng <- liftIO $ fromLspRange uri rng0
       liftMaybe (lookupModuleName uri) $ \(fpath,modname) ->
        liftMaybe (lookupProgram modname) $ \program ->
        do
          defs <- lookupDefinitions [modname]
          case findType defs rng of
            Just info -> do
              let actions = [("show", synShowString modname info), ("==", synEquality modname info),
                             ("cmp", synOrd modname info), ("order2", synOrder2 modname info)]
              let results = map (\(nm, action) -> (nm, Core.runCorePhase 0 action)) actions
              env <- getPrettyEnvFor modname
              responder $ Right $ J.InL (mapMaybe (\(nm, res) -> J.InR <$> toCodeAction env origuri (dataInfoRange info) nm res) results)
            _ -> done

findType :: Definitions -> Range -> Maybe DataInfo
findType defs range = do
  let dataInfos = M.elems $ newtypesTypeDefs $ defsNewtypes defs
  find (\di -> dataInfoRange di `rangeContains` rangeEnd range || dataInfoRange di `rangeContains` rangeStart range) dataInfos

toCodeAction :: Env -> J.Uri -> Range -> String ->  Error b (Def UserType) -> Maybe J.CodeAction
toCodeAction env uri rng kind err =
  case checkError err of
    Left errs -> Nothing
    Right (def, _) -> Just $
      J.CodeAction
        (case kind of
          "show" -> "Generate show function"
          "==" -> "Generate (==) function"
          "cmp" -> "Generate cmp (comparison) function"
          "order2" -> "Generate order2 (fip comparison) function"
        )
        (Just J.CodeActionKind_QuickFix) Nothing Nothing Nothing
        (Just (J.WorkspaceEdit (Just (M.singleton uri [
             J.TextEdit (toLspRange (rangeJustAfter rng)) (Text.pack $ "\n\n" ++ show (ppSyntaxDefUserType env def))
          ])) Nothing Nothing)) -- TODO: Ensure correct document version etc.
        Nothing Nothing

nameShow = newName "show"
nameEq = newName "=="
nameCmp = newName "cmp"
nameOrder2 = newName "order2"

isStarType_ :: Type -> Bool
isStarType_ (TVar (TypeVar id kind _)) = hasKindStarResult kind
isStarType_ (TApp (TCon (TypeCon _ kind)) _) = hasKindStarResult kind
isStarType_ (TCon (TypeCon _ kind)) = hasKindStarResult kind
isStarType_ _ = False

isStarType :: Type -> Bool
isStarType = isStarType_ . canonicalForm

isStarTypeVar :: TypeVar -> Bool
isStarTypeVar (TypeVar id kind _) = hasKindStarResult kind

mkBind :: Name -> Range -> ValueBinder (Maybe UserType) (Maybe (Expr UserType))
mkBind arg r = ValueBinder arg Nothing Nothing r r

mkBindt :: Name -> UserType -> Range -> ValueBinder (Maybe UserType) (Maybe (Expr UserType))
mkBindt arg tp r = ValueBinder arg (Just tp) Nothing r r

nonFunctionFields fields = map snd (filter fst fields)

tpForall :: [UserType] -> UserType -> UserType
tpForall [] tp = tp
tpForall (TpVar nm _:rest) tp = TpQuan QForall (TypeBinder nm KindNone rangeNull rangeNull) (tpForall rest tp) rangeNull

tpString :: UserType
tpString = TpCon nameTpString rangeNull

tpBool :: UserType
tpBool = TpCon nameTpBool rangeNull

tpVarName :: UserType -> Name
tpVarName (TpVar x _) = x

userTp :: Nice -> Type -> UserType
userTp nice tp =
  case tp of
    TVar tv -> TpVar (showTV tv) rangeNull
    TFun xs eff y -> TpFun (map (\(x, t) -> (x, userTp nice t)) xs) (userTp nice eff) (userTp nice y) rangeNull
    TCon (TypeCon x _) -> TpCon x rangeNull
    TApp f xs -> TpApp (userTp nice f) (map (userTp nice) xs) rangeNull
    TSyn syn xs _ -> TpApp (TpCon (typesynName syn) rangeNull) (map (userTp nice) xs) rangeNull
    TForall tvs _ x -> tpForall (map (\tv -> TpVar (showTV tv) rangeNull) tvs) (userTp nice x)
  where showTV tv   = newName $ show $ ppTypeVar defaultEnv{nice=nice} tv

instance Eq UserQuantifier where
  QForall == QForall = True
  QExists == QExists = True
  QSome == QSome = True
  _ == _ = False

instance Eq (TypeBinder k) where
  TypeBinder x _ _ _ == TypeBinder y _ _ _ = x == y

instance Eq UserType where
  TpVar x _ == TpVar y _ = x == y
  TpCon x _ == TpCon y _ = x == y
  TpApp x xs _ == TpApp y ys _ = x == y && xs == ys
  TpFun xs eff y _ == TpFun xs' eff' y' _ = xs == xs' && eff == eff' && y == y'
  TpQuan q x y _ == TpQuan q' x' y' _ = q == q' && x == x' && y == y'
  TpQual x y == TpQual x' y' = x == x' && y == y'
  _ == _ = False

appendOp :: Expr UserType
appendOp = Var (newName "++") True rangeNull

appendStr :: Expr UserType -> Expr UserType -> Expr UserType
appendStr (Lit (LitString s1 _)) (App (Var op _ _) [(_, Lit (LitString s2 _)), (_, s3)] _) =
                 -- combine basic adjacent string literals 
                appendStr (Lit (LitString (s1 ++ s2) rangeNull)) s3
appendStr expr1 expr2 = App appendOp [(Nothing, expr1), (Nothing, expr2)] rangeNull

synShowString :: Name -> DataInfo -> Core.CorePhase b (Def UserType)
synShowString modName info = do
  evar <- TV.freshTypeVar kindEffect Bound
  let drng        = dataInfoRange info
      dataName    = dataInfoName info
      defName     = newLocallyQualified "" (nameStem dataName) "show"
      visibility  = dataInfoVis info
      anyFunctionFields = any (any (isFun . snd) . conInfoParams) (dataInfoConstrs info)
      doc         = "// Automatically generated.\n// Shows a string representation of the `" ++ nameStem (dataInfoName info) ++ "` type" ++ (if anyFunctionFields then " (ignores function fields).\n" else ".\n")
      def         = Def (ValueBinder defName () showExpr drng drng) drng visibility (DefFun [Borrow] (NoFip False)) InlineAlways doc
      tyParams    = dataInfoParams info
      nice        = niceTypeExtendVars (evar:tyParams) niceEmpty
      showTV tv   = newName $ show $ ppTypeVar defaultEnv{nice=nice} tv
      tpParams    = map (\tv -> TpVar (showTV tv) drng) tyParams
      dive        = TpVar (showTV evar) drng
      dataTp      = TpApp (TpCon dataName drng) tpParams drng
      selfArg     = if all isAlphaNum (show dataName) then dataName else newName "this"
      tVarName tv = toImplicitParamName (newLocallyQualified "" (nameStem $ tpVarName tv) "show")
      starTVs     = map (\tv -> TpVar (showTV tv) drng) $ filter isStarTypeVar tyParams
      tvArgs      = map (\x -> (tVarName x, TpFun [(prepend "tv" (tpVarName x), x)] dive tpString rangeNull)) starTVs
      tvBinds     = map (\(x, tp) -> mkBindt x tp drng) tvArgs
      fullTp      = tpForall (tpParams ++ [dive]) $ TpFun ((selfArg,dataTp):tvArgs) dive tpString rangeNull
      showExpr    = Ann (Lam (mkBindt selfArg dataTp drng:tvBinds) caseExpr drng) fullTp drng
      branches    = concatMap makeBranch (dataInfoConstrs info)
      caseExpr    = Case (Var selfArg False drng) branches False drng
      makeBranch :: ConInfo -> [Branch UserType]
      makeBranch con
        = let
              crng                  = conInfoRange con
              getTV :: UserType -> Maybe Name
              getTV ty =
                case ty of
                  TpVar x _ ->
                    do tvar <- find (\(TpVar xx _) -> x == xx) starTVs
                       return (tVarName tvar)
                  _ -> Nothing
              tyShowName :: UserType -> Expr UserType
              tyShowName ty =
                  case getTV ty of
                    Just x -> Var (fst $ splitImplicitParamName x) False crng
                    Nothing -> Var nameShow False crng
              showTp :: Expr UserType -> UserType -> Expr UserType
              showTp exp ty =
                -- Use fully qualified defName if the type is the same as the data type
                if ty == dataTp then App (Var defName False crng) [(Nothing, exp)] crng
                else App (tyShowName ty) [(Nothing, exp)] crng
              lString s             = Lit (LitString s crng)
              start                 = lString (nameStem (conInfoName con) ++ "(")
              fields                = map (\(nm, tp) -> (not (isFun tp), (nm, userTp nice tp))) (conInfoParams con)
              pVar fld rng          = if fst fld then PatVar (ValueBinder (fst (snd fld)) Nothing (PatWild rng) rng rng)
                                      else PatWild rng
              patterns              = [(Nothing,pVar fld crng) | fld <- fields]
              showField2(fldN,fldTp)= [lString (nameStem fldN ++ ": "), showTp (Var fldN False crng) fldTp]
              varExprs              = map showField2 (nonFunctionFields fields)
              varExprs2             = intercalate [lString ", "] varExprs
              toString              = appendStr start (foldr appendStr (lString ")") varExprs2)
              conMatch              = PatCon (conInfoName con) patterns crng crng
              branchExpr            = [Branch conMatch [Guard guardTrue toString]]
              branchExprNoFields    = [Branch conMatch [Guard guardTrue (lString (nameStem (conInfoName con)))]]
            in if null fields then branchExprNoFields else branchExpr
  return def

hasSingleCon :: DataInfo -> Bool
hasSingleCon info = length (dataInfoConstrs info) == 1

synEquality :: Name -> DataInfo -> Core.CorePhase b (Def UserType)
synEquality modName info = do
  evar <- TV.freshTypeVar kindEffect Bound
  let drng        = dataInfoRange info
      dataName    = dataInfoName info
      tyParams    = dataInfoParams info
      nice        = niceTypeExtendVars (evar:tyParams) niceEmpty
      showTV tv   = newName $ show $ ppTypeVar defaultEnv{nice=nice} tv
      tpParams    = map (\tv -> TpVar (showTV tv) rangeNull) tyParams
      starTVs     = map (\tv -> TpVar (showTV tv) rangeNull) (filter isStarTypeVar tyParams)
      ediv        = TpVar (showTV evar) rangeNull
      selfArg     = newName "this"
      otherArg    = newName "other"
      dataTp      = TpApp (TpCon dataName drng) tpParams drng
      tVarName tv = toImplicitParamName (newLocallyQualified "" (nameStem $ prepend "eq" (tpVarName tv)) "==")
      tvArgs      = map (\x -> (tVarName x, TpFun
                          [(prepend "this" (tpVarName x), x),
                           (prepend "other" (tpVarName x), x)] ediv tpBool rangeNull))
                      starTVs
      tvBinds     = map (\(x, t) -> mkBindt x t drng) tvArgs
      fullTp      = tpForall (tpParams ++ [ediv]) $ TpFun ((selfArg,dataTp):(otherArg,dataTp):tvArgs) ediv tpBool rangeNull
      branches    = concatMap makeBranch (dataInfoConstrs info)
      litBool b rng = if b then Var nameTrue False rng else Var nameFalse False rng
      caseArg     = [(Nothing, Var selfArg False drng), (Nothing, Var otherArg False drng)]
      caseExpr    = Case (App (Var (nameTuple 2) False drng) caseArg drng) (branches ++ defaultBranch) False drng
      defExpr     = Ann (Lam (mkBindt selfArg dataTp drng:mkBindt otherArg dataTp drng:tvBinds) caseExpr drng) fullTp drng
      visibility  = dataInfoVis info
      anyFunctionFields = any (any (isFun . snd) . conInfoParams) (dataInfoConstrs info)
      doc         = "// Automatically generated.\n// Equality comparison of the `" ++ nameStem (dataInfoName info) ++ "` type" ++ (if anyFunctionFields then " (ignores function fields).\n" else ".\n")
      defName     = newLocallyQualified "" (nameStem dataName) "=="
      def         = Def (ValueBinder defName () defExpr drng drng) drng visibility (DefFun [Borrow] (NoFip False)) InlineAlways doc
      tupleBranch :: Pattern UserType -> Pattern UserType -> Expr UserType -> Range -> Branch UserType
      tupleBranch p1 p2 res r = Branch (PatCon (nameTuple 2) [(Nothing, p1), (Nothing, p2)] r r) [Guard guardTrue res]
      defaultBranch :: [Branch UserType]
      defaultBranch = ([tupleBranch (PatWild drng) (PatWild drng) (litBool False drng) drng | not (hasSingleCon  info)])
      makeBranch :: ConInfo -> [Branch UserType]
      makeBranch con
        = let crng = conInfoRange con
              getTV :: UserType -> Maybe Name
              getTV ty =
                case ty of
                  TpVar x _ ->
                    do tvar <- find (\(TpVar xx _) -> x == xx) starTVs
                       return (tVarName tvar)
                  _ -> Nothing
              tpEqName :: UserType -> Expr UserType
              tpEqName ty =
                case getTV ty of
                  Just x -> Var (fst $ splitImplicitParamName x) True crng
                  Nothing -> Var nameEq True crng
              eqTp :: Expr UserType -> Expr UserType -> UserType -> Expr UserType
              eqTp expL expR ty   =
                -- Use fully qualified defName if the type is the same as the data type
                if ty == dataTp then App (Var defName True crng) [(Nothing, expL), (Nothing, expR)] crng
                else App (tpEqName ty) [(Nothing, expL), (Nothing, expR)] crng
              fields = map (\(nm, tp) -> (not (isFun tp), (nm, userTp nice tp))) (conInfoParams con)
              pVar :: (Bool, (Name, UserType)) -> String -> Pattern UserType
              pVar fld postfix     = if fst fld then PatVar (ValueBinder (postpend postfix (fst (snd fld))) Nothing (PatWild crng) crng crng)
                                    else PatWild crng
              patternsL           = [(Nothing,pVar fld "") | fld <- fields]
              patternsR           = [(Nothing,pVar fld "'") | fld <- fields]
              andOp               = Var (newName "&&") True crng
              andExpr expr1 expr2 = App andOp [(Nothing, expr1), (Nothing, expr2)] crng
              nonFunctionFields   = map snd (filter fst fields)
              eqField(fldN, fldT) = eqTp (Var fldN False crng) (Var (postpend "'" fldN) False crng) fldT
              varExprs            = map eqField nonFunctionFields
              branchExpr = case varExprs of
                [] -> litBool True crng
                _ -> foldr1 andExpr varExprs
              branch              = tupleBranch (PatCon (conInfoName con) patternsL crng crng) (PatCon (conInfoName con) patternsR crng crng)
            in [branch branchExpr crng]
  return def

nameCmpEq = newName "Eq"
tpOrder = TpCon (newName "order") rangeNull

synOrd :: Name -> DataInfo -> Core.CorePhase b (Def UserType)
synOrd modName info = do
  evar <- TV.freshTypeVar kindEffect Bound
  let drng        = dataInfoRange info
      dataName    = dataInfoName info
      tyParams    = dataInfoParams info
      nice        = niceTypeExtendVars (evar:tyParams) niceEmpty
      showTV tv   = newName $ show $ ppTypeVar defaultEnv{nice=nice} tv
      tpParams    = map (\tv -> TpVar (showTV tv) rangeNull) tyParams
      starTVs     = map (\tv -> TpVar (showTV tv) rangeNull) (filter isStarTypeVar tyParams)
      ediv        = TpVar (showTV evar) rangeNull
      selfArg     = newName "this"
      otherArg    = newName "other"
      dataTp      = TpApp (TpCon dataName drng) tpParams drng
      tVarName tv = toImplicitParamName (newLocallyQualified "" (nameStem $ tpVarName tv) "cmp")
      tvArgs      = map (\x -> (tVarName x, TpFun
                          [(prepend "this" (tpVarName x), x),
                           (prepend "other" (tpVarName x), x)] ediv tpOrder rangeNull))
                      starTVs
      tvBinds     = map (\(x, t) -> mkBindt x t drng) tvArgs
      fullTp      = tpForall (tpParams ++ [ediv]) $ TpFun ((selfArg,dataTp):(otherArg,dataTp):tvArgs) ediv tpOrder rangeNull
      doMakeBranches [] = []
      doMakeBranches [c] = [head (makeBranches c)] -- Last constructor only needs to check equality
      doMakeBranches (c:cs) = makeBranches c ++ doMakeBranches cs
      branches    = doMakeBranches (dataInfoConstrs info)
      litLt       = Var (newName "Lt") False drng
      litGt       = Var (newName "Gt") False drng
      litEq       = Var nameCmpEq False drng
      caseArg     = [(Nothing, Var selfArg False drng), (Nothing, Var otherArg False drng)]
      caseExpr    = Case (App (Var (nameTuple 2) False drng) caseArg drng) branches False drng
      defExpr     = Ann (Lam (mkBindt selfArg dataTp drng:mkBindt otherArg dataTp drng:tvBinds) caseExpr drng) fullTp drng
      visibility  = dataInfoVis info
      anyFunctionFields = any (any (isFun . snd) . conInfoParams) (dataInfoConstrs info)
      doc         = "// Automatically generated.\n// Comparison of the `" ++ nameStem (dataInfoName info) ++ "` type" ++ (if anyFunctionFields then " (ignores function fields).\n" else ".\n")
      defName     = newLocallyQualified "" (nameStem dataName) "cmp"
      def         = Def (ValueBinder defName () defExpr drng drng) drng visibility (DefFun [Borrow] (NoFip False)) InlineAlways doc
      tupleBranch :: Pattern UserType -> Pattern UserType -> Expr UserType -> Range -> Branch UserType
      tupleBranch p1 p2 res r = Branch (PatCon (nameTuple 2) [(Nothing, p1), (Nothing, p2)] r r) [Guard guardTrue res]
      makeBranches :: ConInfo -> [Branch UserType]
      makeBranches con
        = let crng = conInfoRange con
              getTV :: UserType -> Maybe Name
              getTV ty =
                case ty of
                  TpVar x _ ->
                    do tvar <- find (\(TpVar xx _) -> x == xx) starTVs
                       return (tVarName tvar)
                  _ -> Nothing
              tpCmpName :: UserType -> Expr UserType
              tpCmpName ty =
                case getTV ty of
                  Just x -> Var (fst $ splitImplicitParamName x) False crng
                  Nothing -> Var nameCmp False crng
              cmpTp :: Expr UserType -> Expr UserType -> UserType -> Expr UserType
              cmpTp expL expR ty   =
                -- Use fully qualified defName if the type is the same as the data type
                if ty == dataTp then App (Var defName False crng) [(Nothing, expL), (Nothing, expR)] crng
                else App (tpCmpName ty) [(Nothing, expL), (Nothing, expR)] crng
              fields = map (\(nm, tp) -> (not (isFun tp), (nm, userTp nice tp))) (conInfoParams con)
              pVar :: (Bool, (Name, UserType)) -> String -> Pattern UserType
              pVar fld postfix     = if fst fld then PatVar (ValueBinder (postpend postfix (fst (snd fld))) Nothing (PatWild crng) crng crng)
                                    else PatWild crng
              patternsL           = [(Nothing,pVar fld "") | fld <- fields]
              patternsR           = [(Nothing,pVar fld "'") | fld <- fields]
              nonFunctionFields   = map snd (filter fst fields)
              cmpField(fldN, fldT) = cmpTp (Var fldN False crng) (Var (postpend "'" fldN) False crng) fldT
              binderNeqName nm       = (prepend (nameStem nm) $ newName "_order")
              binderNeqPat nm        = ValueBinder (binderNeqName nm)  Nothing (PatWild crng) crng crng
              branchExpr' [field]                = cmpField field
              branchExpr' (field@(nm,tp):fields) = Case (cmpField field) [
                                                      Branch (PatCon nameCmpEq [] crng crng) [Guard guardTrue (branchExpr' fields)],
                                                      Branch (PatVar (binderNeqPat nm)) [Guard guardTrue (Var (binderNeqName nm) False crng)]
                                                    ] False crng
              branchExpr = case nonFunctionFields of
                [] -> litEq
                _ -> branchExpr' nonFunctionFields
              branch              = tupleBranch (PatCon (conInfoName con) patternsL crng crng) (PatCon (conInfoName con) patternsR crng crng)
              ltbranch            = tupleBranch (PatCon (conInfoName con) [] crng crng) (PatWild crng) litLt crng
              gtbranch            = tupleBranch (PatWild crng) (PatCon (conInfoName con) [] crng crng) litGt crng
            in [branch branchExpr crng, ltbranch, gtbranch]
  return def

nameOrd2Eq = newName "Eq2"
nameOrd2Lt = newName "Lt2"
nameOrd2Gt = newName "Gt2"
nameOrd2 = newName "order2"

synOrder2 :: Name -> DataInfo -> Core.CorePhase b (Def UserType) -- TODO: Make this actually fip
synOrder2 modName info = do
  evar <- TV.freshTypeVar kindEffect Bound
  let drng        = dataInfoRange info
      dataName    = dataInfoName info
      tyParams    = dataInfoParams info
      nice        = niceTypeExtendVars (evar:tyParams) niceEmpty
      showTV tv   = newName $ show $ ppTypeVar defaultEnv{nice=nice} tv
      tpParams    = map (\tv -> TpVar (showTV tv) rangeNull) tyParams
      starTVs     = map (\tv -> TpVar (showTV tv) rangeNull) (filter isStarTypeVar tyParams)
      ediv        = TpVar (showTV evar) rangeNull
      selfArg     = newName "this"
      otherArg    = newName "other"
      dataTp      = TpApp (TpCon dataName drng) tpParams drng
      tVarName tv = toImplicitParamName (newLocallyQualified "" (nameStem $ tpVarName tv) "order2")
      tpOrder2    = TpApp (TpCon nameOrd2 drng) [dataTp] drng
      tvArgs      = map (\x -> (tVarName x, TpFun
                          [(prepend "this" (tpVarName x), x),
                           (prepend "other" (tpVarName x), x)] ediv (TpApp (TpCon nameOrd2 drng) [x] rangeNull) rangeNull))
                      starTVs
      tvBinds     = map (\(x, t) -> mkBindt x t drng) tvArgs
      fullTp      = tpForall (tpParams ++ [ediv]) $ TpFun ((selfArg,dataTp):(otherArg,dataTp):tvArgs) ediv tpOrder2 rangeNull
      doMakeBranches [] = []
      doMakeBranches [c] = [head (makeBranches c)] -- Last constructor only needs to check equality
      doMakeBranches (c:cs) = makeBranches c ++ doMakeBranches cs
      branches    = doMakeBranches (dataInfoConstrs info)
      litLt       = Var nameOrd2Lt False drng
      litGt       = Var nameOrd2Gt False drng
      litEq       = Var nameOrd2Eq False drng
      caseArg     = [(Nothing, Var selfArg False drng), (Nothing, Var otherArg False drng)]
      caseExpr    = Case (App (Var (nameTuple 2) False drng) caseArg drng) branches False drng
      defExpr     = Ann (Lam (mkBindt selfArg dataTp drng:mkBindt otherArg dataTp drng:tvBinds) caseExpr drng) fullTp drng
      visibility  = dataInfoVis info
      anyFunctionFields = any (any (isFun . snd) . conInfoParams) (dataInfoConstrs info)
      doc         = "// Automatically generated.\n// Fip comparison of the `" ++ nameStem (dataInfoName info) ++ "` type" ++ (if anyFunctionFields then " (ignores function fields).\n" else ".\n")
      defName     = newLocallyQualified "" (nameStem dataName) "order2"
      def         = Def (ValueBinder defName () defExpr drng drng) drng visibility (DefFun [Borrow] (NoFip False)) InlineAlways doc
      tupleBranch :: Pattern UserType -> Pattern UserType -> Expr UserType -> Range -> Branch UserType
      tupleBranch p1 p2 res r = Branch (PatCon (nameTuple 2) [(Nothing, p1), (Nothing, p2)] r r) [Guard guardTrue res]
      makeBranches :: ConInfo -> [Branch UserType]
      makeBranches con
        = let crng = conInfoRange con
              getTV :: UserType -> Maybe Name
              getTV ty =
                case ty of
                  TpVar x _ ->
                    do tvar <- find (\(TpVar xx _) -> x == xx) starTVs
                       return (tVarName tvar)
                  _ -> Nothing
              tpOrder2Name :: UserType -> Expr UserType
              tpOrder2Name ty =
                case getTV ty of
                  Just x -> Var (fst $ splitImplicitParamName x) False crng
                  Nothing -> Var nameOrder2 False crng
              order2Tp :: Expr UserType -> Expr UserType -> UserType -> Expr UserType
              order2Tp expL expR ty   =
                -- Use fully qualified defName if the type is the same as the data type
                if ty == dataTp then App (Var defName False crng) [(Nothing, expL), (Nothing, expR)] crng
                else App (tpOrder2Name ty) [(Nothing, expL), (Nothing, expR)] crng
              fields = map (\(nm, tp) -> (not (isFun tp), (nm, userTp nice tp))) (conInfoParams con)
              varN fld postfix     = postpend postfix (fst (snd fld))
              pVar :: (Bool, (Name, UserType)) -> String -> Pattern UserType
              pVar fld postfix     = if fst fld then PatVar (ValueBinder (varN fld postfix) Nothing (PatWild crng) crng crng)
                                    else PatWild crng
              patternsL           = [(Nothing,pVar fld "") | fld <- fields]
              patternsR           = [(Nothing,pVar fld "'") | fld <- fields]
              nonFunctionFields   = map snd (filter fst fields)
              order2Field(fldN, fldT) = order2Tp (Var fldN False crng) (Var (postpend "'" fldN) False crng) fldT
              patEqName nm       = (prepend (nameStem nm) $ newName "_eq")
              patLtLName nm       = (prepend (nameStem nm) $ newName "_lt")
              patLtRName nm       = (prepend (nameStem nm) $ newName "_gt")
              patGtLName nm       = (prepend (nameStem nm) $ newName "_lt")
              patGtRName nm       = (prepend (nameStem nm) $ newName "_gt")
              constr args         = App (Var (conInfoName con) False crng) (map (\nm -> (Nothing, Var nm False crng)) args) crng
              patEq nm            = PatCon nameOrd2Eq [(Just (patEqName nm, crng), PatWild crng)] crng crng
              patLt nm            = PatCon nameOrd2Lt [(Just (patLtLName nm, crng), PatWild crng), (Just (patLtRName nm, crng), PatWild crng)] crng crng
              patGt nm            = PatCon nameOrd2Gt [(Just (patGtLName nm, crng), PatWild crng), (Just (patGtRName nm, crng), PatWild crng)] crng crng
              patOther' = PatVar (ValueBinder (newName "other'") Nothing (PatWild crng) crng crng)
              patThis' = PatVar (ValueBinder (newName "this'") Nothing (PatWild crng) crng crng)
              varNThis fld         = fst fld
              varNOther fld         = postpend "'" (fst fld)
              branchExpr' [] eqFields                = App litEq [(Nothing, constr (reverse eqFields))] crng
              branchExpr' (field@(nm,tp):fields) eqFields = Case (order2Field field) [
                                                      Branch (patEq nm) [Guard guardTrue (branchExpr' fields (patEqName nm:eqFields))],
                                                      Branch (patLt nm) [Guard guardTrue (App litLt [(Nothing, constr (reverse eqFields ++ [patLtLName nm] ++ map varNThis fields)), (Nothing, constr (reverse eqFields ++ [patLtRName nm] ++ map varNOther fields))] crng)],
                                                      Branch (patGt nm) [Guard guardTrue (App litGt [(Nothing, constr (reverse eqFields ++ [patGtLName nm] ++ map varNOther fields)), (Nothing, constr (reverse eqFields ++ [patGtRName nm] ++map varNThis fields))] crng)]
                                                    ] False crng
              branchExpr = branchExpr' nonFunctionFields []
              branch              = tupleBranch (PatCon (conInfoName con) patternsL crng crng) (PatCon (conInfoName con) patternsR crng crng)
              ltbranch            = tupleBranch (PatCon (conInfoName con) patternsL crng crng) patOther'
                                                    (App litLt [(Nothing, constr (map (varNThis . snd) fields)), (Nothing, Var (newName "other'") False crng)] crng) crng
              gtbranch            = tupleBranch patThis' (PatCon (conInfoName con) patternsR crng crng)
                                                    (App litGt [(Nothing, constr (map (varNOther . snd) fields)), (Nothing, Var (newName "this'") False crng)] crng) crng
            in [branch branchExpr crng, ltbranch, gtbranch]
  return def


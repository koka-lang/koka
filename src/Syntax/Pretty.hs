
------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Pretty print user syntax
-}
-----------------------------------------------------------------------------
module Syntax.Pretty where
import Type.Pretty
import Type.Type
import Core.Pretty
import Lib.PPrint
import Syntax.Syntax
import Common.Syntax
import Common.ColorScheme
import Common.NamePrim
import qualified Syntax.Syntax as S
import Common.Range
import Data.List

class PrettyEnv t where
  prettyEnv :: Env -> t -> Doc

instance PrettyEnv t => PrettyEnv (Maybe t) where
  prettyEnv env (Just x) = prettyEnv env x
  prettyEnv env Nothing = empty

instance PrettyEnv Type where
  prettyEnv env tp = ppType env tp

instance PrettyEnv (KUserType k) where
  prettyEnv env tp =
    case tp of
      TpAnn tp _ -> prettyEnv env tp
      TpVar n _ -> ppName env n
      TpCon n _ -> ppName env n
      TpQuan QForall tb tp rng -> text "forall<" <.> ppName env (tbinderName tb) <.> text ">" <+> prettyEnv env tp
      TpQuan QExists tb tp rng -> text "exists<" <.> ppName env (tbinderName tb) <.> text ">" <+> prettyEnv env tp
      TpQuan QSome tb tp rng -> text "some<" <.> ppName env (tbinderName tb) <.> text ">" <+> prettyEnv env tp
      TpApp tp [] _ -> prettyEnv env tp
      TpApp tp args _ -> prettyEnv env tp <.> angled (map (prettyEnv env) args)
      TpFun args e res _ -> tupled (map (\(n, tp) -> prettyEnv env tp) args) <+> text "->" <+> prettyEnv env e <+> prettyEnv env res
      TpParens tp _ -> tupled [prettyEnv env tp]
      TpQual _ _ -> text "unhandled tp"

allDefs :: S.DefGroup t -> [S.Def t]
allDefs defs =
  case defs of
    S.DefNonRec d -> [d]
    S.DefRec ds   -> ds

ppLit :: S.Lit -> String
ppLit (S.LitInt i range) = show i
ppLit (S.LitFloat f range) = show f
ppLit (S.LitChar c range) = show c
ppLit (S.LitString s range) = show s

ppVis :: Env -> Visibility -> Doc
ppVis env vis
  = case vis of
      Private -> empty
      Public -> keyword env "pub "

ppSyntaxDef :: PrettyEnv t => Env -> S.Def t -> Doc
ppSyntaxDef env (S.Def binder range vis sort inline doc)
  = prettyComment env doc $
    ppVis env vis <.> text (defSortShowFull sort) <+> ppValBinder env binder

ppSyntaxDefUserType :: Env -> S.Def UserType -> Doc
ppSyntaxDefUserType env (S.Def binder range vis sort inline doc)
  = prettyComment env doc $
    ppVis env vis <.> text (defSortShowFull sort) <+> (
    case sort of
      DefFun _ _ -> ppFunDef env binder
      _ -> ppValBinder env binder
      )

ppSyntaxExtern :: Env -> S.External -> Doc
ppSyntaxExtern env e = text "extern" <+> ppName env (extName e)

returnType :: UserType -> Maybe (UserType, UserType)
returnType (TpFun _ eff tp _) = Just (eff, tp)
returnType (TpQuan _ _ tp _) = returnType tp
returnType (TpQual _ tp) = returnType tp
returnType _ = Nothing

ppFunDef :: Env -> ValueBinder () (S.Expr UserType) -> Doc
ppFunDef env (ValueBinder name _ (Lam vbs body _) _ _)
  = ppName env name <.> tupled (map (ppMValBinder env) vbs) <--> indent 2 (ppSyntaxExpr env body)
ppFunDef env (ValueBinder name _ (Ann (Lam vbs body _) tp _) _ _)
  = case returnType tp of
      Just (eff, ret) ->
          ppName env name <.> tupled (map (ppMValBinder env) vbs) <+> colon <+> prettyEnv env eff <+> prettyEnv env ret <-->
                                    indent 2 (ppSyntaxExpr env body)
      Nothing -> ppName env name <.> tupled (map (ppMValBinder env) vbs) <--> indent 2 (ppSyntaxExpr env body)

ppValBinder :: PrettyEnv t => Env -> ValueBinder () (S.Expr t) -> Doc
ppValBinder env (ValueBinder name _ expr nameRange range)
  = ppName env name <+> text "=" <+> ppSyntaxExpr env expr

ppMValBinder :: PrettyEnv t => Env -> ValueBinder (Maybe t) (Maybe (S.Expr t)) -> Doc
ppMValBinder env (ValueBinder name (Just tp) (Just expr) nameRange range)
  = ppName env name <+> text "=" <+> ppSyntaxExpr env expr
ppMValBinder env (ValueBinder name Nothing (Just expr) nameRange range)
  = ppName env name <+> text "=" <+> ppSyntaxExpr env expr
ppMValBinder env (ValueBinder name (Just tp) Nothing nameRange range)
  = ppName env name <+> text ":" <+> prettyEnv env tp
ppMValBinder env (ValueBinder name Nothing Nothing nameRange range)
  = ppName env name

ppValEmptyBinder ::  PrettyEnv t => Env -> ValueBinder (Maybe t) () -> Doc
ppValEmptyBinder env (ValueBinder name _ () nameRange range)
  = ppName env name

ppPatBinder ::  PrettyEnv t => Env -> ValueBinder (Maybe t) (S.Pattern t) -> Doc
ppPatBinder env (ValueBinder name _ (S.PatWild rng) nameRange range)
  = ppName env name
ppPatBinder env (ValueBinder name _ pat nameRange range)
  = ppName env name <+> text "as" <+> ppSyntaxPattern env pat

ppArg ::  PrettyEnv t => Env -> (Maybe (Name,Range),S.Expr t) -> Doc
ppArg env (Nothing,expr) = ppSyntaxExpr env expr
ppArg env (Just (name,_),expr) = ppName env name <+> text "=" <+> ppSyntaxExpr env expr

ppSyntaxExpr ::  PrettyEnv t => Env -> S.Expr t -> Doc
ppSyntaxExpr env e =
  case e of
    S.Lam pars expr range ->
      text "fn" <.> tupled (map (ppMValBinder env) pars) <--> indent 2 (ppSyntaxExpr env expr)
    S.Let defs expr range ->
      vcat (map (ppSyntaxDef env) (allDefs defs) ++ [ppSyntaxExpr env expr])
    Bind def expr range ->
      vcat [ppSyntaxDef env def, ppSyntaxExpr env expr]
    App (Var na _ _) [] _ | isConstructorName na -> -- Singleton constructor
      ppName env na
    App (Var na True _) [(Nothing, x0), (Nothing, x1)] _ | not (isQualified na || isLocallyQualified na) -> -- Unqualified infix operations
      ppSyntaxExpr env x0 <+> text (nameStem na) <+> ppSyntaxExpr env x1 
    App (Var na True _) [(Nothing, x0)] _ -> -- Postfix operations?
      ppSyntaxExpr env x0 <.> ppName env na
    App (Var na _ _) args _ | isNameTuple na -> -- Tuple
      tupled (map (ppArg env) args)
    S.App (S.Var name _ _) args range | name == nameOpExpr ->
      hcat (intersperse (text " ") (map (ppArg env) args))
    S.App hnd@Handler{} [(_, a)] range ->
      tupled [ppSyntaxExpr env hnd] <.> tupled [ppSyntaxExpr env a]
    S.App fun args range ->
      ppSyntaxExpr env fun <.> tupled (map (ppArg env) args)
    S.Var nm isop range | nm == nameUnit -> text "()"
    S.Var name isop range -> ppName env name
    S.Lit lit -> text $ ppLit lit
    Ann expr tp range -> ppSyntaxExpr env expr
    S.Case expr branches lazy range ->
      text "match" <+> ppSyntaxExpr env expr <--> indent 2 (vcat (map (ppSyntaxBranch env) branches))
    Parens expr name _ range -> ppSyntaxExpr env expr
    Inject tp expr behind range ->
      text "mask<" <.> prettyEnv env tp <.> text ">{" <--> indent 2 (ppSyntaxExpr env expr) <--> text "}"
    Handler sort scope override allowmask eff pars reinit ret final branches drange range ->
      text "handler" <--> indent 2 (vcat (ppMaybeExpr env ret: ppMaybeExpr env reinit : ppMaybeExpr env final : map (ppHandlerBranch env) branches))

ppHandlerBranch ::  PrettyEnv t => Env -> S.HandlerBranch t -> Doc
ppHandlerBranch env (HandlerBranch nm pars expr sort nmRng patRng) =
  pretty (show sort) <+> text (show nm) <.> tupled (map (ppValEmptyBinder env) pars) <--> indent 2 (ppSyntaxExpr env expr)

ppMaybeExpr ::  PrettyEnv t => Env -> Maybe (Expr t) -> Doc
ppMaybeExpr env (Just expr) = ppSyntaxExpr env expr
ppMaybeExpr env Nothing = empty

ppSyntaxBranch :: PrettyEnv t => Env -> S.Branch t -> Doc
ppSyntaxBranch env (S.Branch pat [S.Guard (S.Var n _ _) body]) | nameTrue == n
  = ppSyntaxPattern env pat <+> text "->" <--> indent 2 (ppSyntaxExpr env body)
ppSyntaxBranch env (S.Branch pat [guard])
  = ppSyntaxPattern env pat <+> text "|" <+> ppSyntaxGuard env guard
ppSyntaxBranch env b = text "Unhandled branch"

ppSyntaxPattern :: PrettyEnv t => Env -> S.Pattern t -> Doc
ppSyntaxPattern env (S.PatWild range) = text "_"
ppSyntaxPattern env (S.PatVar binder) = ppPatBinder env binder
ppSyntaxPattern env (PatAnn pat tp range) = ppSyntaxPattern env pat
ppSyntaxPattern env (S.PatCon name args nameRng range)
  | isNameTuple name = tupled (map (ppPatternArg env) args)
  | null args = ppName env name
  | otherwise = ppName env name <.> tupled (map (ppPatternArg env) args)
ppSyntaxPattern env (PatParens pat range) = tupled [ppSyntaxPattern env pat]
ppSyntaxPattern env (S.PatLit lit) = text $ ppLit lit

ppPatternArg :: PrettyEnv t => Env -> (Maybe (Name,Range), S.Pattern t) -> Doc
ppPatternArg env (Nothing,pat) = ppSyntaxPattern env pat
ppPatternArg env (Just (name,_),S.PatWild rng) = ppName env name
ppPatternArg env (Just (name,_),pat) = ppName env name <+> text "=" <+> ppSyntaxPattern env pat

alwaysTrue :: Expr t -> Bool
alwaysTrue (Var n _ _) | n == nameTrue = True
alwaysTrue _ = False

ppSyntaxGuard :: PrettyEnv t => Env -> S.Guard t -> Doc
ppSyntaxGuard env (S.Guard guard body)
  = if alwaysTrue guard then text "->" <+> ppSyntaxExpr env body else ppSyntaxExpr env guard <+> text "->" <--> indent 2 (ppSyntaxExpr env body)

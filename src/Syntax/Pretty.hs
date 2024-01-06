
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
module Syntax.Pretty( ppBinder, ppPatBinder, alwaysTrue, ppGuard, ppPattern, ppLit, ppBranch, ppDefBinder, ppDef, ppDefGroup, ppExpr) where
import Type.Pretty
import Type.Type
import Core.Pretty
import Lib.PPrint
import Syntax.Syntax
import Common.Syntax
import Common.ColorScheme
import Common.NamePrim(nameTrue)

ppBinder :: Env -> ValueBinder (Maybe Type) (Maybe (Expr Type)) -> Doc
ppBinder env (ValueBinder name tp expr _ _)
  =  ppName env name <+> case tp of Nothing -> empty; Just x -> text ":" <+> ppType env x <+> case expr of Nothing -> empty; Just x -> text "=" <+> ppExpr env x

ppPatBinder :: Env -> ValueBinder (Maybe Type) (Pattern Type) -> Doc
ppPatBinder env (ValueBinder name _ _ _ _)
  =  ppName env name

alwaysTrue :: Expr Type -> Bool
alwaysTrue (Var n _ _) | n == nameTrue = True
alwaysTrue _ = False

ppGuard :: Env -> Guard Type -> Doc
ppGuard env (Guard expr expr2)
  = if alwaysTrue expr then text "->" <+> ppExpr env expr2 else  text "|" <+> ppExpr env expr <+> text "->" <+> ppExpr env expr2

ppPattern :: Env -> Pattern Type -> Doc
ppPattern env pat
  = case pat of
  PatWild _ -> text "_"
  PatVar vb -> ppPatBinder env vb
  PatAnn pat' ty _ -> ppPattern env pat' <+> text ":" <+> ppType env ty
  PatCon na x0 _ _ -> ppName env na <.> tupled (map (\x -> ppPattern env (snd x)) x0)
  PatParens pat' _ -> tupled [ppPattern env pat']
  PatLit lit -> ppLit env lit
   
ppLit :: Env -> Lit -> Doc
ppLit env lit
  = case lit of
      LitInt i _ -> text (show i)
      LitChar c _ -> text (show c)
      LitString s _ -> text (show s)
      LitFloat f _ -> text (show f)

ppBranch:: Env -> Branch Type -> Doc
ppBranch env (Branch pat guards)
  = ppPattern env pat <-> hang 2 (vcat (map (ppGuard env) guards))

ppDefBinder :: Env -> ValueBinder () (Expr Type) -> Doc
ppDefBinder env (ValueBinder name _ expr _ _)
  = ppName env name <+> text "=" <+> ppExpr env expr

ppDef :: Env -> Def Type -> Doc
ppDef env (Def binder _ vis sort inline doc)
  = prettyComment env doc $
    (if isPrivate vis then empty else ppVis env vis) <+> text (show sort) <+>
    ppDefBinder env binder

ppDefGroup:: Env -> DefGroup Type -> Doc
ppDefGroup env dg
  = case dg of
      DefNonRec def -> ppDef env def
      DefRec defs -> sep (map (ppDef env) defs)

ppExpr :: Env -> Expr Type -> Doc
ppExpr env expr
  = color (colorSource (colors env)) $
  case expr of
   Lam vbs ex _ -> keyword env "fn" <.> tupled (map (ppBinder env) vbs) <-> hang 2 (ppExpr env ex)
   App ex x0 _ -> ppExpr env ex <.> 
    tupled (map (\(n, x) -> (case n of {Just (n,_) -> ppName env n <+> text "= "; Nothing -> empty}) <+> ppExpr env x) x0)
   Var na b ra -> ppName env na
   Case ex brs ra -> keyword env "match" <+> ppExpr env ex <-> hang 2 (vcat (map (ppBranch env) brs))
   Ann ex ty _ -> ppExpr env ex <+> text ":" <+> ppType env ty
   Parens ex _ _ _ -> tupled [ppExpr env ex]
   Lit lit -> ppLit env lit
   -- Not as sure about these
   Let dg ex _ -> keyword env "val" <+> ppDefGroup env dg <+> text "=" <+> ppExpr env ex
   Inject ty ex b _ -> keyword env "mask" <+> if b then keyword env "behind" else empty <.> angled [ppType env ty] <+> tupled [ppExpr env ex]
   Bind dg ex _ -> keyword env "val" <+> ppDef env dg <+> text "=" <+> ppExpr env ex
   _ -> text "Pretty print for handlers is not implemented yet"
   -- Handler hs hs' ho m_b m_ty vbs m_ex ma m_ex' hbs ra ra' -> text "handle"

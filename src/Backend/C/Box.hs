-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Backend.C.Box ( boxCore ) where

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
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.Name
import Common.Range
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty
import Core.CoreVar

import Core.Simplify

--------------------------------------------------------------------------
-- Box/unbox transformation
--------------------------------------------------------------------------

boxCore :: Core -> Unique Core
boxCore core
  = do defs <- boxDefGroups (coreProgDefs core)
       return (core{ coreProgDefs = defs })


boxDefGroups :: DefGroups -> Unique DefGroups
boxDefGroups dgs
  = mapM boxDefGroup dgs

boxDefGroup :: DefGroup -> Unique DefGroup
boxDefGroup dg
  = case dg of
      DefRec defs   -> fmap DefRec (mapM boxDef defs)
      DefNonRec def -> fmap DefNonRec (boxDef def)

boxDef :: Def -> Unique Def
boxDef def
    = do bexpr <- boxExpr (boxType (defType def)) (defExpr def)
         expr  <- uniqueSimplify True {- unsafe -} 2 {- duplicationMax -} bexpr
         return def{ defExpr = expr }

-- add box/unbox such that the type of `expr` matches `BoxType`
boxExpr :: BoxType -> Expr -> Unique Expr
boxExpr expectTp expr
  = case expr of
      -- remove type abstraction and applications
      TypeLam tvs e        -> boxExpr expectTp e
      TypeApp e tps        -> boxExpr expectTp e
      -- Regular
      App e args           -> do let argTps = map boxTypeOf args
                                     eTp    = TFun [(nameNil,tp) | tp <- argTps] typeTotal expectTp
                                 bargs <- mapM (\(arg) -> boxExpr (boxTypeOf arg) arg) args
                                 bexpr <- boxExpr eTp e
                                 return (App bexpr bargs)
      Lam tparams eff body -> do let funTp = boxTypeOf expr
                                 bbody <- boxExpr (boxTypeOf body) body
                                 bcoerce funTp (expectTp) (Lam tparams eff bbody)
      Let defGroups body   -> do bdgs <- boxDefGroups defGroups
                                 bbody <- boxExpr expectTp body
                                 return (Let bdgs bbody)
      Case exprs branches  -> do let exprTps = map boxTypeOf exprs
                                 bexprs    <- mapM (\(tp,e) -> boxExpr tp e) (zip exprTps exprs)
                                 bbranches <- mapM (boxBranch exprTps expectTp) branches
                                 return (Case bexprs bbranches)
      _                    -> bcoerce (boxTypeOf expr) expectTp expr

{-
    isBoxOp (App (Var (TName name _) (InfoExternal _)) [arg]) = (name == newHiddenName ("box") || name == newHiddenName ("unbox"))
    isBoxOp _ = False
-}

boxBranch :: [BoxType] -> BoxType -> Branch -> Unique Branch
boxBranch patTps expectTp (Branch patterns guards)
  = do (bpatterns,defss) <- unzipM $ mapM (\(patTp,pat) -> boxPattern patTp pat) (zip patTps patterns)
       let binds expr  = makeLet [DefNonRec def | def <- concat defss] expr
       bguards <- mapM (boxGuard expectTp binds) guards
       return (Branch bpatterns bguards)

boxGuard :: BoxType -> (Expr -> Expr) -> Guard -> Unique Guard
boxGuard expectTp binds (Guard test expr)
  =do btest <- boxExpr typeBool test
      bexpr <- boxExpr expectTp expr
      return (Guard btest (binds bexpr))  -- TODO: binds come too late to appear in guards but we need binds for function wrappers?.. perhaps create a specal pattern just for the C backend?
                   

-- add bindings and box/unbox such that pattern matches the required boxtype
boxPattern :: BoxType -> Pattern -> Unique (Pattern, [Def])
boxPattern fromTp PatWild
  = boxPatternX fromTp PatWild
boxPattern fromTp pat | cType (fromTp) /= cType toTp
  = do mcoerce <- -- trace ("pattern coerce: " ++ show (pretty fromTp) ++ " ~> " ++ show (pretty toTp)) $
                  bcoerceX fromTp toTp (Var (TName nameNil toTp) InfoNone)
       case mcoerce of
         Just coerce0
           -> -- We just insert a specially named pattern variable -- the backend recognizes this
              -- and generates unbox/box expressions appropiately so nested patterns are handled correctly
              -- Unfortunately, this goes wrong for function wrappers; for those we rename and generate a
              -- binding; this works as a function type is never pattern matched further.
              -- TODO: this may fail if the function is used in a guard test? (perhaps substitute in there?)
              case coerce0 of
                Lam{} -> -- function match
                         case pat of 
                           PatVar tname _
                             -> -- ok, no nested match
                                do i <- unique
                                   let uname = newHiddenName ("fun-unbox-x" ++ show i)
                                   coerce <- bcoerce fromTp toTp (Var (TName uname fromTp) InfoNone)  -- regenerate the coercion
                                   let def = makeTDef (TName (getName tname) toTp) coerce
                                   -- trace ("unbox function: " ++ show uname) $
                                   return (PatVar (TName uname fromTp) PatWild, [def])
                           _ -> failure "Backend/C/FromCore.boxPattern: nested match on a function?"
                _     -> -- regular box/unbox 
                         do i <- unique
                            let uname = newHiddenName ("unbox-x" ++ show i)
                            (bpat,defs) <- boxPatternX toTp pat
                            -- trace ("unbox pattern: " ++ show uname) $
                            return (PatVar (TName uname toTp) bpat, defs)  -- toTp for generating correct unbox call in the C backend
         _ -> boxPatternX fromTp pat
                     
  where
    toTp  = case pat of
              PatCon{}       -> patTypeRes pat
              PatVar tname _ -> typeOf tname
              PatLit lit     -> typeOf lit
              PatWild        -> typeAny  -- cannot happen

boxPattern fromTp pat
  = boxPatternX fromTp pat

boxPatternX :: BoxType -> Pattern -> Unique (Pattern,[Def])
boxPatternX fromTp pat
  = case pat of
      PatCon name params repr targs exists tres conInfo
        -> do (bparams,defss) <- unzipM $ mapM (\(ftp,par) -> boxPattern ftp par)  (zip (map snd (conInfoParams conInfo)) params)
              return (PatCon name bparams repr targs exists tres conInfo, concat defss)
      PatVar tname arg
        -> do (barg,defs) <- boxPattern (typeOf tname) arg
              return (PatVar tname barg, defs)
      PatWild  -> return (pat,[])
      PatLit _ -> return (pat,[])

-- coerce `expr` of `fromTp` to `toTp`
bcoerce :: Type -> Type -> Expr -> Unique Expr
bcoerce fromTp toTp expr
  = do mb <- bcoerceX fromTp toTp expr 
       case mb of
        Just expr' -> return expr'
        Nothing    -> return expr
      
bcoerceX :: Type -> Type -> Expr -> Unique (Maybe Expr)
bcoerceX fromTp toTp expr
  = case (cType fromTp, cType toTp) of
      (CBox, CBox)             -> return Nothing
      (CBox, CData)            -> return $ Just $ App (unboxVar) [expr]
      (CBox, CFun cpars cres)  -> return $ Just $ App (unboxVar) [expr]
      (CData, CBox)            -> return $ Just $ App (boxVar) [expr]
      (CFun cpars cres, CBox)  -> return $ Just $ App (boxVar) [expr]

      (CFun fromPars fromRes, CFun toPars toRes)
          | not (all (\(t1,t2) -> t1 == t2) (zip fromPars toPars) && fromRes == toRes)
          -> case splitFunScheme toTp of
               Just (_,_,toParTps,toEffTp,toResTp)
                 -> case splitFunScheme fromTp of
                      Just (_,_,fromParTps,fromEffTp,fromResTp)
                        -> do  names <- mapM (\_ -> uniqueName "b") toParTps
                               let pars  = zipWith TName names (map snd toParTps)
                                   args  = [Var par InfoNone | par <- pars]
                               bargs <- mapM (\(arg,argTp) -> boxExpr argTp arg) (zip args (map snd fromParTps))
                               bapp  <- bcoerce fromResTp toResTp (App expr bargs)
                               return (Just (Lam pars toEffTp bapp))

      _   -> return Nothing
  where
    boxVar
      = Var (TName nameBox (coerceTp)) (InfoExternal [(C, "box(#1)")])
    unboxVar
      = Var (TName nameUnbox (coerceTp )) (InfoExternal [(C, "unbox(#1)")])
    coerceTp
      = TFun [(nameNil,fromTp)] typeTotal toTp


type BoxType = Type

-- type without quantification
boxTypeOf :: Expr -> BoxType
boxTypeOf expr
  = case splitPredType (typeOf expr) of
      (_,_,tp) -> tp

boxType :: Type -> BoxType
boxType tp
   = case tp of
       TForall vars preds t
         -> boxType t -- (subNew [(tv,typeBox (getKind tv)) | tv <- vars] |-> t)
       TFun pars eff res
         -> TFun [(name, boxType par) | (name,par) <- pars] (boxType eff) (boxType res)
       TApp t ts
         -> TApp (boxType t) [typeBox (getKind t) | t <- ts]
       TSyn syn args t
         -> TSyn syn (map boxType args) (boxType t)
       _ -> tp


typeBox :: Kind -> BoxType
typeBox k
  = TCon (TypeCon nameTpBox k)


data CType
  = CBox
  | CFun [CType] CType
  | CData
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
        -> CData
      TVar v
        -> CBox
      TSyn syn args t
        -> cType t

-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation.
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
         expr  <- uniqueSimplify 2 {- duplicationMax -} bexpr
         return def{ defExpr = expr }

      
boxExpr :: BoxType -> Expr -> Unique Expr
boxExpr expectTp expr
  = case expr of
      -- remove type abstraction and applications
      TypeLam tvs e        -> boxExpr expectTp e
      TypeApp e tps        -> boxExpr expectTp e      
      -- Regular
      App e args           -> do let funTp = boxTypeOf e
                                 case splitFunScheme funTp of
                                   Just (_,_,paramTps,eff,resTp) 
                                     -> assertion ("Backend.C.box: boxArgs: arguments do not match: " ++ show expr) (length paramTps == length args) $
                                        do bargs <- mapM (\((_,paramTp),arg) -> boxExpr paramTp arg) (zip paramTps args)
                                           bexpr <- boxExpr funTp e
                                           bcoerce resTp expectTp (App bexpr bargs)
      Lam tparams eff body -> do let funTp = boxTypeOf expr
                                 bbody <- boxExpr (resultType funTp) body
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
  = do bs <- mapM (\(patTp,pat) -> boxPattern patTp pat) (zip patTps patterns)
       let (bdeclss,bpatterns) = unzip bs
           bsubs    = concat bdeclss
           bgroups  = [DefNonRec (Def name tp expr Private DefVal InlineAuto rangeNull "") | ((TName name tp),expr) <- bsubs]
           bguards0 = [Guard (bsubs |~> test) (makeLet bgroups body) | (Guard test body) <- guards]
       bguards <- mapM (boxGuard expectTp) bguards0
       return (Branch bpatterns bguards)

boxGuard :: BoxType -> Guard -> Unique Guard
boxGuard expectTp (Guard test expr)
  =do btest <- boxExpr typeBool test 
      bexpr <- boxExpr expectTp expr
      return (Guard btest bexpr)
    
boxPattern :: BoxType -> Pattern -> Unique ([(TName,Expr)],Pattern)
boxPattern fromTp pat
  = case pat of
      PatCon name params repr targs exists tres conInfo
        -> do bs <- mapM (\(ftp,par) -> boxPattern ftp par)  (zip (map snd (conInfoParams conInfo)) params)
              let (subss,bparams) = unzip bs
              return (concat subss, PatCon name bparams repr targs exists tres conInfo)
      PatVar tname arg 
        -> do (subs,barg) <- boxPattern fromTp arg
              uname <- uniqueName (show (getName tname))
              let utname = TName uname fromTp
              coerce <- bcoerce fromTp (typeOf tname) (Var utname InfoNone)
              case coerce of
                Var{} -> return (subs, PatVar tname barg) -- no coercion
                _     -> return ((tname,coerce):subs, PatVar utname barg)              
      PatWild  -> return ([],pat)
      PatLit _ -> return ([],pat)
    
bcoerce :: Type -> Type -> Expr -> Unique Expr
bcoerce fromTp toTp expr
  = case (cType fromTp, cType toTp) of
      (CBox, CBox)             -> return expr      
      (CBox, CData)            -> return $ App (unboxVar) [expr]
      (CBox, CFun cpars cres)  -> return $ App (unboxVar) [expr]      
      (CData, CBox)            -> return $ App (boxVar) [expr]
      (CFun cpars cres, CBox)  -> return $ App (boxVar) [expr]
      
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
                               return (Lam pars toEffTp bapp)

      _   -> return expr
  where
    boxVar
      = Var (TName nameBox (coerceTp)) (InfoExternal [(C, "box(#1)")])
    unboxVar
      = Var (TName nameUnbox (coerceTp )) (InfoExternal [(C, "unbox(#1)")])
    coerceTp 
      = TFun [(nameNil,fromTp)] typeTotal toTp


type BoxType = Type

boxTypeOf :: Expr -> BoxType
boxTypeOf expr
  = case expr of
      TypeLam tvs e     -> boxTypeOf e
      TypeApp e tps     -> boxTypeOf e
      Lam pars eff body -> typeFun [(name,boxType tp) | TName name tp <- pars] (boxType eff) (boxTypeOf body)
      App e args        -> resultType (boxTypeOf e)
      _                 -> typeOf expr    

resultType :: Type -> Type
resultType tp
  = case splitFunScheme tp of
      Just (_,_,_,_,resTp) -> resTp
      Nothing              -> error ("Backend.C.Box.resultType: not a function: " ++ show tp)


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

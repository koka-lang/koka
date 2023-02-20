-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
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
-- import Core.CTail( isCTailOp )

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
    = -- trace ("box def: " ++ show (defName def) ++ ": " ++ show (pretty (defType def)) ++ "\n" ++ show (prettyExpr Pretty.defaultEnv{Pretty.coreShowTypes=True} (defExpr def))) $
      do bexpr <- boxExpr (boxType (defType def)) (defExpr def)
         let bdef = def{ defExpr = bexpr }
         -- simplify the whole def to avoid simplifying away functions to values (e.g. `fun f(x){ g(x) } ~> val f = g`)
         uniqueSimplify Pretty.defaultEnv True {- unsafe -} False {-ndebug-} 3 {-runs-} 6 {- duplicationMax -} bdef
         

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
                  bcoerceX fromTp toTp (Var (TName nameNil fromTp) InfoNone)
       case mcoerce of
         Just coerce0
           -> -- We just insert a specially named Box pattern bound to fresh variable -- the backend recognizes this
              -- and generates unbox/box expressions appropiately so nested patterns are handled correctly
              -- Unfortunately, this does not work for function wrappers (as we need to generate a
              -- wrapped unbox/box function around it); for those we rename and generate an explicit binding
              -- binding; this works as a function type is never pattern matched further.
              -- TODO: this may fail if the function is used in a guard test itself where it is not bound yet.
              --       we could work around this by substituting explicitly in the guard in that case.
              if (isComplexCoerce coerce0)
                then -- function match
                     case pat of
                       PatVar tname PatWild
                         -> -- ok, no nested match
                            do i <- unique
                               let uname = newHiddenName ("fun-unbox-x" ++ show i)
                               coerce <- bcoerce fromTp toTp (Var (TName uname fromTp) InfoNone)  -- regenerate the coercion
                               let def = makeTDef (TName (getName tname) toTp) coerce
                               --trace ("unbox function: " ++ show uname ++ ": " ++ show (pretty fromTp) ++ " to " ++ show (pretty toTp)
                               --      ++ "\n: coerce tp: " ++ show (pretty (typeOf coerce))) $
                               return (PatVar (TName uname fromTp) PatWild, [def])
                       _ -> failure "Backend/C/FromCore.boxPattern: nested match on a function?"
                else -- regular box/unbox
                     do i <- unique
                        let uname = newHiddenName ("box-x" ++ show i)
                        (bpat,defs) <- boxPatternX toTp pat
                        -- trace ("unbox pattern: " ++ show uname ++ ": " ++ show (pretty toTp)) $
                        -- return (PatVar (TName uname toTp) bpat, defs)  -- toTp for generating correct unbox call in the C backend
                        return (PatVar (TName uname typeBoxStar) (patBox toTp typeBoxStar bpat), defs)
         _ -> -- trace ("pattern: no-coerce: " ++ show (pretty fromTp) ++ " to " ++ show (pretty toTp)) $
              boxPatternX fromTp pat

  where
    toTp  = case pat of
              PatCon{}       -> patTypeRes pat
              PatVar tname _ -> typeOf tname
              PatLit lit     -> typeOf lit
              PatWild        -> typeAny  -- cannot happen

    isComplexCoerce coerce
      = case (cType fromTp, cType toTp) of
          (CFun{},_) -> True
          (_,CFun{}) -> True
          _          -> False

boxPattern fromTp pat
  = boxPatternX fromTp pat

boxPatternX :: BoxType -> Pattern -> Unique (Pattern,[Def])
boxPatternX fromTp pat
  = case pat of
      PatCon name params repr targs exists tres conInfo skip
        -> do (bparams,defss) <- unzipM $ mapM (\(ftp,par) -> boxPattern ftp par)  (zip (map snd (conInfoParams conInfo)) params)
              return (PatCon name bparams repr targs exists tres conInfo skip, concat defss)
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
      (CData, CBox)            -> return $ Just $ App (boxVar) [expr]
      -- boxed functions need to wrapped to take all arguments and results as boxed as well :-(
      -- see test/cgen/box3 and test/cgen/box3a
      (CBox, CFun cpars cres)
        -> --trace ("box to fun: " ++ show expr) $
            do boxedToTp <- boxedFunType toTp
               let unboxed = App (unboxVarAtTp (TFun [(nameNil,fromTp)] typeTotal boxedToTp)) [expr]
               Just <$> bcoerce boxedToTp toTp unboxed -- unwrap function; we must return Just even if no further wrapping was needed

      (CFun cpars cres, CBox)
         -> --trace ("fun to box: " ++ show expr) $
            do boxedFromTp <- boxedFunType fromTp
               expr'  <- bcoerce fromTp boxedFromTp expr  -- wrap function
               return $ Just $ App (boxVarAtTp (TFun [(nameNil,boxedFromTp)] typeTotal toTp)) [expr']         -- and box it itselfob

      -- coerce between function arguments/results
      (CFun fromPars fromRes, CFun toPars toRes)
          | not (all (\(t1,t2) -> t1 == t2) (zip fromPars toPars) && fromRes == toRes)
          -> case splitFunScheme toTp of
               Just (_,_,toParTps,toEffTp,toResTp)
                 -> case splitFunScheme fromTp of
                      Just (_,_,fromParTps,fromEffTp,fromResTp)
                        -> Just <$> (boxBindExprAsValue fromTp toTp expr $ \vexpr ->
                                      boxCoerceFun toParTps toEffTp toResTp fromParTps fromEffTp fromResTp vexpr)
                      _ -> failure $ "Backend.C.Box: bcoerceX: expecting function (from): " ++ show (pretty fromTp)
               _ -> failure $ "Backend.C.Box: bcoerceX: expecting function (to): " ++ show (pretty toTp)
      _   -> return Nothing
  where
    boxVar
      = boxVarAtTp coerceTp

    unboxVar
      = unboxVarAtTp coerceTp

    coerceTp
      = TFun [(nameNil,fromTp)] typeTotal toTp

boxVarAtTp tp
  = Var (TName nameBox tp) (InfoExternal [(C CDefault, "box(#1)")])
unboxVarAtTp tp
  = Var (TName nameUnbox tp) (InfoExternal [(C CDefault, "unbox(#1)")])


boxCoerceFun :: [(Name,Type)] -> Effect -> Type -> [(Name,Type)] -> Effect -> Type  -> Expr -> Unique Expr
boxCoerceFun toParTps toEffTp toResTp fromParTps fromEffTp fromResTp expr
  = -- trace ("box coerce fun: " ++ show expr) $
    do names <- mapM (\_ -> uniqueName "b") toParTps
       let pars  = zipWith TName names (map snd toParTps)
           args  = [Var par InfoNone | par <- pars]
       bargs <- -- mapM (\(arg,argTp) -> boxExpr argTp arg) (zip args (map snd fromParTps))
                mapM (\(arg,parToTp,parFromTp) -> bcoerce parToTp parFromTp arg) (zip3 args (map snd toParTps) (map snd fromParTps))
       bapp  <- bcoerce fromResTp toResTp (App expr bargs)
       return (Lam pars toEffTp bapp)

boxBindExprAsValue :: Type -> Type -> Expr -> (Expr -> Unique Expr) -> Unique Expr
boxBindExprAsValue fromTp toTp expr action  | isTotal expr
  = action expr
boxBindExprAsValue fromTp toTp expr action
  = -- trace ("box coerce with yield extension: " ++ show expr) $
    do v    <- uniqueTName "bv" fromTp
       body <- action (Var v InfoNone)
       return (Let [DefNonRec (makeTDef v expr)] body)

  {-
    do yextend <- do vb <- uniqueTName "bb" (TVar tvarA)
                     w  <- uniqueTName "bw" fromTp
                     x  <- uniqueTName "bx" toTp
                     let varVb = Var vb InfoNone
                         varX  = Var x InfoNone
                     unboxVb <- bcoerce (typeOf vb) (typeOf w)  varVb
                     boxResX <- bcoerce (typeOf x) (TVar tvarB) varX
                     body1   <- action (Var w InfoNone)
                     makeYieldExtend (typeOf vb) toTp $
                        Lam [vb] typeTotal {-?-} $
                        Let [DefNonRec (makeTDef w unboxVb)
                            ,DefNonRec (makeTDef x body1)]
                        boxResX
       v  <- uniqueTName "bv" fromTp
       body2 <- action (Var v InfoNone)
       return (Let [DefNonRec (makeTDef v expr)] $
               makeIfExpr makeYielding yextend body2
              )
  where
    coerceTp
      = TFun [(nameNil,fromTp)] typeTotal toTp

makeYielding :: Expr
makeYielding
  = App (Var (TName nameYielding typeYielding) (InfoExternal [(C,"kk_yielding(kk_context())")])) []
  where
    typeYielding = TFun [] typeTotal typeBool

makeYieldExtend :: Type -> Type -> Expr -> Unique Expr
makeYieldExtend fromTp toTp expr
  = do let yextend = App (TypeApp (Var (TName nameYieldExtend typeYieldExtend) (InfoArity 3 1)) [TVar tvarA, TVar tvarB, typeTotal]) [expr]
       {- -- no need for nice unbox, it will be box_any anyways...
       v <- uniqueTName "b" (TVar tvarB)
       body <- bcoerce (typeOf v) toTp (Var v InfoNone)
       return (Let [DefNonRec (makeTDef v yextend)] body)
      -}
       case cType toTp of
         CBox -> return yextend
         _    -> return $ App (unboxVarAtTp (TFun [(nameNil,TVar tvarB)] typeTotal toTp)) [yextend]
  where
    typeYieldExtend = TForall [tvarA,tvarB,tvarE] []
                        (TFun [(nameNil,TFun [(nameNil,TVar tvarA)] (TVar tvarE) (typeYld (TVar tvarB)))]
                              (TVar tvarE) (typeYld (TVar tvarB)))

    typeYld tp = tp

tvarA,tvarB,tvarE :: TypeVar
tvarA = TypeVar 0 kindStar Bound
tvarB = TypeVar 1 kindStar Bound
tvarE = TypeVar 2 kindEffect Bound
-}

type BoxType = Type

-- type without quantification
boxTypeOf :: Expr -> BoxType
boxTypeOf expr
  = -- trace ("boxTypeOf: typeApp: " ++ show expr) $
    case splitPredType (typeOf expr) of
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


boxedFunType :: Type -> Unique Type
boxedFunType tp
  = case tp of
      TForall vars preds t
        -> boxedFunType t -- (subNew [(tv,typeBox (getKind tv)) | tv <- vars] |-> t)
      TSyn syn args t
        -> boxedFunType t
      TFun pars eff res
        -> do bpars <- mapM (\_ -> boxedTypeVar) pars
              bres  <- boxedTypeVar
              return (TFun [(name, bpar) | ((name,_),bpar) <- zip pars bpars] eff bres)
      _ -> failure $ "Backend.C.Box.boxedFunType: not a function type: " ++ show (pretty tp)

boxedTypeVar :: Unique Type
boxedTypeVar
  = do i <- unique
       return (TVar (TypeVar i kindStar Bound))


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


typeBoxStar = typeBox kindStar

isBoxPat :: Pattern -> Bool
isBoxPat (PatCon{ patConName = name })  = (getName name == nameBoxCon)
isBoxPat _                              = False

patBox :: Type -> Type -> Pattern -> Pattern
patBox tpPat tpRes pat
  = PatCon (TName nameBoxCon (conInfoType boxConInfo)) [pat] boxConRepr [tpPat] [] tpRes boxConInfo True

boxConRepr :: ConRepr
boxConRepr = ConSingle nameTpBox (DataSingle False) (valueReprScan 1) 0

boxConInfo :: ConInfo
boxConInfo
  = ConInfo nameBox nameTpBox [a] [] [(nameNil,TVar a)] tp
            Inductive rangeNull [] [Public] True 
            [(nameNil,TVar a)] 
            (valueReprScan 1) {- size is wrong with knowing the platform ? -}
            Public ""
  where
    tp = TForall [a] [] (TFun [(nameNil,TVar a)] typeTotal typeBoxStar)
    a  = TypeVar (0) kindStar Bound


uniqueTName nm tp
  = do n <- uniqueName nm
       return (TName n tp)

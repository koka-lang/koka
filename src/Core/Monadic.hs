-----------------------------------------------------------------------------
-- Copyright 2016-2017 Microsoft Research, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Transform user-defined effects into monadic bindings.
-----------------------------------------------------------------------------

module Core.Monadic( monTransform
                   , monMakeBind
                   ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim( nameEffectOpen, nameYieldOp, nameReturn, nameTpCont, nameDeref, nameByref,
                        nameTrue, nameFalse, nameTpBool, nameApplyK, nameUnsafeTotal, nameIsValidK,
                        nameBind, nameLift, nameTpYld, nameSystemCore, namePerform )
import Common.Error
import Common.Syntax

import Kind.Kind( kindStar, isKindEffect, kindFun, kindEffect,   kindHandled )
import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Type.Operations( freshTVar )
import Core.Core
import qualified Core.Core as Core
import Core.Pretty
import Core.CoreVar

trace s x =
   -- Lib.Trace.trace s
    x

monTransform :: Pretty.Env -> CorePhase b ()
monTransform penv 
  = liftCorePhaseUniq $ \uniq defs -> runMon penv uniq (monDefGroups defs)


{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}
monDefGroups :: DefGroups -> Mon DefGroups
monDefGroups monDefGroups
  = do defGroups <- mapM monDefGroup monDefGroups
       return (defGroups)

monDefGroup (DefRec defs)
  = do defs <- mapM (monDef True) defs
       return (DefRec defs)

monDefGroup (DefNonRec def)
  = do def <- monDef False def
       return (DefNonRec def)


{--------------------------------------------------------------------------
  transform a definition
--------------------------------------------------------------------------}
monDef :: Bool -> Def -> Mon Def
monDef recursive def
  = if (not (isMonDef def))
     then return def
     else withCurrentDef def $
          do expr' <- monExpr' True (defExpr def)
             return def{ defExpr = expr' id }


type Trans a = TransX a a
type TransX a b  = (a -> b) ->b

monExpr :: Expr -> Mon (TransX Expr Expr)
monExpr expr
  = do monExpr' False expr

monExpr' :: Bool -> Expr -> Mon (TransX Expr Expr)
monExpr' topLevel expr
  = case expr of
      -- optimized open binding
      -- note: we cannot just check for `isMonEffect effFrom` as the effFrom
      -- might be total but inside we may still need a monadic translation if `f`
      -- contains handlers itself for example.
      App (App eopen@(TypeApp (Var open _) [effFrom,effTo,_,_]) [f]) args
          | getName open == nameEffectOpen && not (isMonExpr f) -- not (isMonEffect effFrom)
          -> do args' <- mapM monExpr args
                return $ \k -> applies args' (\argss -> k (App (App eopen [f]) argss))

      App (TypeApp (App eopen@(TypeApp (Var open _) [effFrom,effTo,_,_]) [f]) targs) args
          | getName open == nameEffectOpen && not (isMonExpr f) -- not (isMonEffect effFrom)
          -> do args' <- mapM monExpr args
                return $ \k -> applies args' (\argss -> k (App (TypeApp (App eopen [f]) targs) argss))
      
      --  lift _open_ applications
      App eopen@(TypeApp (Var open _) [effFrom,effTo,_,_]) [f]
        | getName open == nameEffectOpen
        -> do f' <- monExpr f
              return $ \k -> f' (\ff -> k (App eopen [ff]))

      -- regular cases
      Lam args eff body
        -> do -- monTraceDoc $ \env -> text "not effectful lambda:" <+> niceType env eff
              body' <- monExpr body
              return $ \k -> k (Lam args eff (body' id))

      App f args
        -> do f' <- monExpr f
              args' <- mapM monExpr args
              let -- ff  = f' id
                  ftp = typeOf f -- ff
              feff <- let (tvs,preds,rho) = splitPredType ftp -- can happen with: ambient control abort() : a
                      in case splitFunType rho of
                           Just(_,feff,_) -> return feff
                           _ -> do monTraceDoc $ \env -> text "Core.Monadic.App: illegal application:" <+> ppType env ftp
                                   failure ("Core.Monadic.App: illegal application")
              if ((not (isMonType ftp || isAlwaysMon f)) || isNeverMon f)
               then do monTraceDoc $ \env -> text "app non-mon: eff:" <+> pretty feff <+> text ", expr:" <+> prettyExpr env expr
                       return $ \k -> f' (\ff ->
                                            applies args' (\argss ->
                                              k (App ff argss)
                                          ))
               else  do monTraceDoc $ \env -> text "app mon:" <+> prettyExpr env expr
                        nameY <- uniqueName "y"
                        return $ \k ->
                          let resTp = typeOf expr
                              tnameY = TName nameY resTp
                              contBody = k (Var tnameY InfoNone)
                              cont = case contBody of
                                        -- optimize (fun(y) { let x = y in .. })
                                       Let [DefNonRec def@(Def{ defExpr = Var v _ })] body
                                        | getName v == nameY
                                        -> Lam [TName (defName def) (defType def)] feff body
                                       -- TODO: optimize (fun (y) { lift(expr) } )?
                                       body -> Lam [tnameY] feff body
                          in
                          f' (\ff ->
                            applies args' (\argss ->
                              appBind resTp feff (typeOf contBody) ff argss cont
                          ))
      Let defgs body
        -> monLetGroups defgs body

      Case exprs bs
        -> do exprs' <- monTrans monExpr exprs
              bs'    <- mapM monBranch bs
              if (not (any isMonBranch bs))
               then return $ \k -> exprs' (\xxs -> k (Case xxs bs'))
               else do nameC <- uniqueName "c"
                       let resTp = typeOf expr
                           tnameC = TName nameC resTp
                       return $ \k ->
                         let effTp    = typeTotal
                             contBody = k (Var tnameC InfoNone)
                             cont = Lam [tnameC] effTp contBody
                         in  exprs' (\xss -> applyBind resTp effTp (typeOf contBody) (Case xss bs')  cont)

      Var (TName name tp) info
        -> do -- tp' <- monTypeX tp
              return (\k -> k (Var (TName name tp) info))

      -- type application and abstraction
      TypeLam tvars body
        -> do body' <- monExpr' topLevel body
              return $ \k -> body' (\xx -> k (TypeLam tvars xx))
              -- return $ \k -> k (TypeLam tvars (body' id))

      TypeApp body tps
        -> do body' <- monExpr' topLevel body
              return $ \k -> body' (\xx -> k (TypeApp xx tps))

      _ -> return (\k -> k expr) -- leave unchanged


monBranch :: Branch -> Mon Branch
monBranch (Branch pat guards)
  = do guards' <- mapM monGuard guards
       return $ Branch pat guards'

monGuard :: Guard -> Mon Guard
monGuard (Guard guard body)
  = do -- guard' <- monExpr guard  -- guards are total!
       body'  <- monExpr body
       return $ Guard guard (body' id)

monLetGroups :: DefGroups -> Expr -> Mon (TransX Expr Expr)
monLetGroups [] body
  = monExpr body
monLetGroups (dg:dgs) body
  = do dg' <- monLetGroup dg
       expr' <- monLetGroups dgs body
       return $ \k -> dg' (\xdg -> Let xdg (expr' k))

monLetGroup :: DefGroup -> Mon (TransX [DefGroup] Expr)
monLetGroup dg
  = case dg of
      DefRec defs -> do ldefs <- monTrans (monLetDef True) defs
                        return $ \k -> ldefs (\xss -> k (concat ([[DefRec xds] ++ (if null yds then [] else [DefRec yds]) ++ map DefNonRec nds | (xds,yds,nds) <- xss])))
      DefNonRec d -> do ldef <- monLetDef False d
                        return $ \k -> ldef (\(xds,yds,nds) -> k (map DefNonRec (xds ++ yds ++ nds)))

monLetDef :: Bool -> Def -> Mon (TransX ([Def],[Def],[Def]) Expr)
monLetDef recursive def
  = withCurrentDef def $
    do expr' <- monExpr' True (defExpr def) -- don't increase depth
       return $ \k -> expr' (\xx -> k ([def{defExpr = xx}],[],[]))
                         -- \k -> k [def{ defExpr = expr' id}]


{-
simplify :: Expr -> Expr
simplify expr
  = case expr of
      App (TypeApp (Var openName _) [eff1,eff2]) [arg]
        | getName openName == nameEffectOpen && matchType eff1 eff2
        -> simplify arg
      TypeApp (TypeLam tvars body) tps  | length tvars == length tps
        -> simplify (subNew (zip tvars tps) |-> body)
      _ -> expr

-}

monTrans :: (a -> Mon (TransX b c)) -> [a] -> Mon (TransX [b] c)
monTrans f xs
  = case xs of
      [] -> return $ \k -> k []
      (x:xx) -> do x'  <- f x
                   xx' <- monTrans f xx
                   return $ \k -> x' (\y -> xx' (\ys -> k (y:ys)))


applies :: [Trans a] -> ([a] -> a) -> a
applies [] f = f []
applies (t:ts) f
  = t (\c -> applies ts (\cs -> f (c:cs)))


appBind :: Type -> Effect ->  Type -> Expr -> [Expr] -> Expr -> Expr
appBind tpArg tpEff tpRes fun args cont
  = applyBind tpArg tpEff tpRes (App fun args) cont

applyBind tpArg tpEff tpRes expr cont
  = case cont of
      Lam [aname] eff (Var v _) | getName v == getName aname -> expr      
      _ -> monMakeBind tpArg tpEff tpRes expr cont
           -- App (TypeApp (Var (TName nameBind typeBind) info) [tpArg, tpRes, tpEff]) [expr,cont]
  
    
monMakeBind :: Type -> Effect -> Type -> Expr -> Expr -> Expr
monMakeBind tpArg tpEff tpRes arg next
  =  App (TypeApp (Var (TName nameBind typeBind) info) [tpArg, tpRes, tpEff]) [arg,next]
  where
    info = Core.InfoArity 2 3 -- Core.InfoExternal [(CS,"Eff.Op.Bind<##1,##2>(#1,#2)"),(JS,"$std_core._bind(#1,#2)")]
    
typeBind :: Type
typeBind
  = TForall [tvarA,tvarB,tvarE] []
      (TFun [(nameNil,typeYld (TVar tvarA)),
             (nameNil,TFun [(nameNil,TVar tvarA)] (TVar tvarE) (typeYld (TVar tvarB)))]
            (TVar tvarE) (typeYld (TVar tvarB)))


typeYld :: Type -> Type  -- Yld<a> == a
typeYld tp
  = tp -- TSyn (TypeSyn nameTpYld (kindFun kindStar kindStar) 0 Nothing) [tp] tp

tvarA :: TypeVar
tvarA = TypeVar 0 kindStar Bound

tvarB :: TypeVar
tvarB = TypeVar 1 kindStar Bound

tvarE :: TypeVar
tvarE = TypeVar 2 kindEffect Bound


{--------------------------------------------------------------------------
  Check if expressions need monadic translation
--------------------------------------------------------------------------}

-- Some expressions always need mon translation
isAlwaysMon :: Expr -> Bool
isAlwaysMon expr
  = case expr of
      TypeApp e _ -> isAlwaysMon e
      Var v _     -> -- getName v == nameYieldOp ||
                     getName v == nameUnsafeTotal -- TODO: remove these special cases?
                     -- getName v == namePerform 0
      _ -> False

-- Some expressions never need mon translation
isNeverMon :: Expr -> Bool
isNeverMon expr
  = case expr of
      App eopen@(TypeApp (Var open _) [effFrom,effTo,tpFrom,tpTo]) [f] | getName open == nameEffectOpen
        -> isTypeTotal effFrom  -- TODO: more cases? generally handler free
      TypeApp e _ -> isNeverMon e
      Var v _     -> getName v == canonicalName 1 nameDeref --TODO: remove special case?
      _ -> isTotal expr


-- Does this definition need any mon translation (sometimes deeper inside)
isMonDef :: Def -> Bool
isMonDef def
  = isMonType (defType def) || isMonExpr (defExpr def)

isMonExpr :: Expr -> Bool
isMonExpr expr
  = case expr of
      App (TypeApp (Var open _) [_, effTo]) [f] | getName open == nameEffectOpen
        -> isMonEffect effTo || isMonExpr f
      App f args
        -> any isMonExpr (f:args)
      Lam pars eff body
        -> or [isMonEffect eff, isMonExpr body]

      TypeApp (TypeLam tpars body) targs
        -> (any isMonType targs) || (isMonExpr body)
      TypeApp (Var tname info) targs
        -> any isMonType targs || isMonType (typeOf expr)

      TypeApp body targs
        -> any isMonType targs || isMonExpr body
      TypeLam tpars body
        -> isMonExpr body
      Let defs body
        -> any isMonDefGroup defs || isMonExpr body
      Case exprs bs
        -> any isMonExpr exprs || any isMonBranch bs
      _ -> isMonType (typeOf expr)

isMonDefGroup defGroup
  = case defGroup of
      DefRec defs -> any isMonDef defs
      DefNonRec def -> isMonDef def

isMonBranch (Branch pat guards)
  = any isMonGuard  guards

isMonGuard (Guard g e)
  = any isMonExpr [g,e]


{--------------------------------------------------------------------------
  Mon monad
--------------------------------------------------------------------------}
newtype Mon a = Mon (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                isInBind   :: Bool,
                prettyEnv :: Pretty.Env }

data State = State{ uniq :: Int }

data Result a = Ok a State

runMon :: Pretty.Env -> Int -> Mon a -> (a,Int)
runMon penv u (Mon c)
  = case c (Env [] False penv) (State u) of
      Ok x (State u') -> (x,u')

instance Functor Mon where
  fmap f (Mon c)  = Mon (\env st -> case c env st of
                                      Ok x st' -> Ok (f x) st')

instance Applicative Mon where
  pure x = Mon (\env st -> Ok x st)
  (<*>)  = ap

instance Monad Mon where
  -- return = pure
  (Mon c) >>= f = Mon (\env st -> case c env st of
                                    Ok x st' -> case f x of
                                                   Mon d -> d env st' )

instance HasUnique Mon where
  updateUnique f = Mon (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = Mon (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> Mon a -> Mon a
withEnv f (Mon c)
  = Mon (\env st -> c (f env) st)

getEnv :: Mon Env
getEnv
  = Mon (\env st -> Ok env st)

updateSt :: (State -> State) -> Mon State
updateSt f
  = Mon (\env st -> Ok st (f st))

withCurrentDef :: Def -> Mon a -> Mon a
withCurrentDef def action
  = -- trace ("mon def: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $
    action

withBindContext :: Bool -> Mon a -> Mon a
withBindContext inBind action
  = withEnv (\env -> env{ isInBind = inBind }) action

isInBindContext :: Mon Bool
isInBindContext
  = do env <- getEnv
       return (isInBind env)


monTraceDoc :: (Pretty.Env -> Doc) -> Mon ()
monTraceDoc f
  = do env <- getEnv
       monTrace (show (f (prettyEnv env)))

monTrace :: String -> Mon ()
monTrace msg
  = do env <- getEnv
       trace ("mon: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

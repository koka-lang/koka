-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen, Ningning Xie
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
--
-----------------------------------------------------------------------------

module Core.MonadicLift( monadicLift ) where

import qualified Lib.Trace
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.Error
import Common.Syntax
import Common.NamePrim( nameBind, nameBind2 )
import Kind.Kind
import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Core.Core hiding (makeDef)
import qualified Core.Core as Core
import Core.Pretty
import Core.CoreVar

trace s x =
  Lib.Trace.trace s
    x

monadicLift :: Pretty.Env -> CorePhase b ()
monadicLift penv 
  = liftCorePhaseUniq $ \uniq defs ->
    runLift penv uniq (liftDefGroups True defs)


{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}

liftDefGroups :: Bool   -- top-level function?
              -> DefGroups -> Lift DefGroups
liftDefGroups topLevel defGroups
  = do -- traceDoc (\penv -> text "lifting")
       fmap concat $ mapM (liftDefGroup topLevel) defGroups

liftDefGroup :: Bool {-toplevel -} -> DefGroup -> Lift DefGroups
liftDefGroup True (DefNonRec def)
  = do (def', groups) <- collectLifted $ liftDef True def
       return $  groups ++ [DefNonRec def'] -- all lifted definitions are put before the current definition

liftDefGroup True (DefRec defs)
  = do (defs', groups) <- collectLifted $ mapM (liftDef True) defs
       let groups' = flattenDefGroups groups
       return [DefRec (groups' ++ defs')] -- defs' depend on groups', groups' might depend on defs'

liftDefGroup False (DefNonRec def)
  = do def' <- liftDef False def
       return [DefNonRec def']

liftDefGroup False (DefRec defs)
  = do defs' <- mapM (liftDef False) defs
       return [DefRec defs']


liftDef :: Bool -> Def -> Lift Def
liftDef topLevel def
 = withCurrentDef def $
   do expr' <- if topLevel
                then do (_,iexpr') <- liftExprInl topLevel (defExpr def)
                        return iexpr'
                else liftExpr topLevel (defExpr def)
      return def{ defExpr = expr' }


{--------------------------------------------------------------------------
 transform expressions
--------------------------------------------------------------------------}

liftExprInl :: Bool -> Expr -> Lift (Expr,Expr)
liftExprInl topLevel expr =
  case expr of
    App tpApp@(TypeApp bind@(Var (TName nameB tpB) infoB) [tpArg,tpRes,tpEff]) [bexpr,cont@(Lam [arg] eff body)] | nameB == nameBind -- nameBind typeBind) info) [tpArg, tpRes, tpEff]) [expr,cont]
      -> -- bexpr >>= (\arg -> body)
         do (body',ibody')  <- liftExprInl False body
            bexpr'          <- liftExpr False bexpr
            f <- liftLocalFun (Lam [arg] eff body') typeTotal
            let bind = App tpApp [bexpr',f]
                ibind = App (TypeApp (Var (TName nameBind2 typeBind2) (InfoArity 3 3)) [tpArg,tpRes,tpEff])
                            [bexpr',f,(Lam [arg] eff ibody')]
                typeBind2 = TForall [a,b,e] [] (typeFun [(nameNil,TVar a),
                                                         (nameNil,typeFun [(nameNil,TVar a)] (TVar e) (TVar b)),
                                                         (nameNil,typeFun [(nameNil,TVar a)] (TVar e) (TVar b))] (TVar e) (TVar b))
                          where
                            a = TypeVar (0) kindStar Bound
                            b = TypeVar (1) kindStar Bound
                            e = TypeVar (2) kindEffect Bound
            return (bind,ibind)

    Let defgs body
      -> do defgs' <- liftDefGroups False defgs
            (body',ibody') <- liftExprInl False body
            return (Let defgs' body', Let defgs' ibody')

    Case exprs bs
      -> do exprs' <- mapM (liftExpr False) exprs
            (bs',ibs') <- unzip <$> mapM liftBranchInl bs
            return (Case exprs' bs', Case exprs' ibs')

    TypeLam tvars body
      -> do (body',ibody') <- liftExprInl topLevel body
            return (TypeLam tvars body', TypeLam tvars ibody')

    TypeApp body tps
      -> do (body',ibody') <- liftExprInl topLevel body
            return (TypeApp body' tps, TypeApp ibody' tps)

    Lam args eff body
      -> do (body',ibody') <- liftExprInl False body
            return (Lam args eff body', Lam args eff ibody')

    _ -> do expr' <- liftExpr topLevel expr
            return (expr',expr')


liftBranchInl :: Branch -> Lift (Branch,Branch)
liftBranchInl (Branch pat guards)
  = do (guards',iguards') <- unzip <$> mapM liftGuardInl guards
       return (Branch pat guards', Branch pat iguards')

liftGuardInl :: Guard -> Lift (Guard,Guard)
liftGuardInl (Guard guard body)
  = do guard' <- liftExpr False guard
       (body',ibody')  <- liftExprInl False body
       return (Guard guard' body', Guard guard' ibody')




liftExpr :: Bool -> Expr -> Lift Expr
liftExpr topLevel expr =
  case expr of
    App tpApp@(TypeApp bind@(Var (TName nameB tpB) infoB) [tpArg,tpRes,tpEff]) [bexpr,cont@(Lam [arg] eff body)] | nameB == nameBind -- nameBind typeBind) info) [tpArg, tpRes, tpEff]) [expr,cont]
      -> -- bexpr >>= (\arg -> body)
         do body'  <- liftExpr False body
            bexpr' <- liftExpr False bexpr
            f <- liftLocalFun (Lam [arg] eff body') typeTotal
            return (App tpApp [bexpr',f])
    App f args
      -> do f' <- liftExpr False f
            args' <- mapM (liftExpr False) args
            return (App f' args')

    Lam args eff body
      -> do body' <- liftExpr False body
            let expr' = Lam args eff body'
            return expr'

    Let defgs body
      -> do defgs' <- liftDefGroups False defgs
            body'  <- liftExpr False body
            return (Let defgs' body')

    Case exprs bs
      -> do exprs' <- mapM (liftExpr False) exprs
            bs'    <- mapM liftBranch bs
            return (Case exprs' bs')

    TypeLam tvars body
      -> do body' <- liftExpr topLevel body
            return (TypeLam tvars body')

    TypeApp body tps
      -> do body' <- liftExpr topLevel body
            return (TypeApp body' tps)

    _ -> return expr

liftLocalFun :: Expr -> Effect -> Lift Expr
liftLocalFun expr eff
  = do let fvs = tnamesList $ freeLocals expr
           tvs = tvsList (ftv expr)
       (expr2, liftDef) <- makeDef fvs tvs expr
       emitLifted (DefNonRec liftDef)
       return expr2


makeDef :: [TName] -> [TypeVar] -> Expr -> Lift (Expr, Def)
makeDef fvs tvs expr
  = do -- traceDoc $ \penv -> (text "makeDef:" <+> prettyExpr penv{coreShowTypes=True} expr)
       (name,inl) <- uniqueNameCurrentDef
       let (callExpr,lifted) = (etaExpr name, liftedDef name inl)
       -- traceDoc $ \penv -> text "lifting:" <+> ppName penv name <.> colon <+> text "tvs:" <+> tupled (map (ppTypeVar penv) tvs) <//> prettyExpr penv expr <//> text "to:" <+> prettyDef penv{coreShowDef=True} lifted
       return (callExpr,lifted)
  where
    (tpars,pars,eff,body) -- :: ([TypeVar],[TName],Type)
      = case expr of
          (TypeLam tpars (Lam pars eff lbody)) -> (tpars, map unwild pars, eff, lbody)
          (Lam pars eff lbody)                 -> ([], map unwild pars, eff, lbody)
          _ -> failure $ ("Core.MonadicLift.makeDef: lifting non-function? " ++ show expr)

    unwild (TName name tp)
      = TName (if (null (nameId name) || head (nameId name) == '_') then prepend "wild" name else name) tp

    alltpars = tvs ++ tpars
    allpars  = fvs ++ pars
    allargs  = [Var tname InfoNone | tname <- allpars]

    liftedFun = addTypeLambdas alltpars $ Lam allpars eff body
    liftedTp  = -- trace ("makeDef: liftedFun: " ++ show (prettyExpr defaultEnv{coreShowTypes=True} expr) ++ "\nraw: " ++ show expr) $
                typeOf liftedFun
    liftedDef name inl = Def name liftedTp liftedFun Private (defFun [] {-all owned-}) InlineAuto rangeNull "// monadic lift"

    funExpr name
      = Var (TName name liftedTp) (InfoArity (length alltpars) (length allargs))

    etaExpr name
      = case (tvs,fvs) of
         ([],[]) -> funExpr name
         _ -> addTypeLambdas tpars $ Lam pars eff $
               App (addTypeApps (alltpars) (funExpr name)) (allargs)


{--------------------------------------------------------------------------
  Branches
--------------------------------------------------------------------------}

liftBranch :: Branch -> Lift Branch
liftBranch (Branch pat guards)
  = do guards' <- mapM liftGuard guards
       return (Branch pat guards')

liftGuard :: Guard -> Lift Guard
liftGuard (Guard guard body)
  = do guard' <- liftExpr False guard
       body'  <- liftExpr False body
       return (Guard guard' body')



{--------------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------------}

uniqueNameCurrentDef :: Lift (Name,DefInline)
uniqueNameCurrentDef =
  do env <- getEnv
     let defNames = map defName (currentDef env)
     i <- unique
     let -- base     = concatMap (\name -> nameId name ++ "-") (tail $ reverse defNames) ++ "x" ++ show i
         udefName =  toHiddenUniqueName i "mlift" (last defNames)
     return (udefName, defInline (last (currentDef env)))



{--------------------------------------------------------------------------
  Lift monad
--------------------------------------------------------------------------}
newtype Lift a = Lift (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env }

data State = State{ uniq :: Int }

data Result a = Ok a State [DefGroup]

runLift :: Pretty.Env -> Int -> Lift a -> (a,Int)
runLift penv u (Lift c)
  = case c (Env [] penv) (State u) of
      Ok x st [] -> (x,uniq st)
      Ok x st _  -> failure $ "Core.MonadicLift.runLift: unprocessed defgroups"

instance Functor Lift where
  fmap f (Lift c)  = Lift (\env st -> case c env st of
                                        Ok x st' dgs -> Ok (f x) st' dgs)

instance Applicative Lift where
  pure x = Lift (\env st -> Ok x st [])  
  (<*>)  = ap

instance Monad Lift where
  -- return = pure
  (Lift c) >>= f = Lift (\env st -> case c env st of
                                      Ok x st' dgs -> case f x of
                                                        Lift d -> case d env st' of
                                                                    Ok x' st'' dgs' -> Ok x' st'' (dgs ++ dgs'))

instance HasUnique Lift where
  updateUnique f = Lift (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) } [])
  setUnique  i   = Lift (\env st -> Ok () st{ uniq = i} [])

withEnv :: (Env -> Env) -> Lift a -> Lift a
withEnv f (Lift c)
  = Lift (\env st -> c (f env) st)

getEnv :: Lift Env
getEnv
  = Lift (\env st -> Ok env st [])

updateSt :: (State -> State) -> Lift State
updateSt f
  = Lift (\env st -> Ok st (f st) [])

collectLifted :: Lift a -> Lift (a, DefGroups)
collectLifted (Lift d)
  = Lift (\env st -> case d env st of
                       Ok x st' dgs -> Ok (x,dgs) st' [])

emitLifted :: DefGroup -> Lift ()
emitLifted dg
  = Lift (\env st -> Ok () st [dg])

emitLifteds :: DefGroups -> Lift ()
emitLifteds dg
  = Lift (\env st -> Ok () st dg)

withCurrentDef :: Def -> Lift a -> Lift a
withCurrentDef def action
  = -- trace ("lifting: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $
    action

traceDoc :: (Pretty.Env -> Doc) -> Lift ()
traceDoc f
  = do env <- getEnv
       liftTrace (show (f (prettyEnv env)))

liftTrace :: String -> Lift ()
liftTrace msg
  = do env <- getEnv
       trace ("mlift: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

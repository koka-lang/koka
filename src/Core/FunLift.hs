-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen, Ningning Xie
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Lift all local and anonymous functions to top level. No more letrec :-)
-----------------------------------------------------------------------------

module Core.FunLift( liftFunctions
                   ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative
import Data.List( partition, intersperse )

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.Error
import Common.Syntax

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

traceGroups :: [DefGroup] -> String
traceGroups dgs 
  = show (map showDG dgs) 
  where
    showDG (DefRec defs) = show (map defName defs)
    showDG (DefNonRec def) = show (defName def)    


liftFunctions :: Pretty.Env -> CorePhase b ()
liftFunctions penv 
  = liftCorePhaseUniq $ \uniq defs ->
    runLift penv uniq (liftDefGroups True defs)


{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}

liftDefGroups :: Bool   -- top-level functions are allowed
              -> DefGroups -> Lift DefGroups
liftDefGroups topLevel defGroups
  = do -- traceDoc (\penv -> text "lifting")
       fmap concat $ mapM (liftDefGroup topLevel) defGroups

liftDefGroup :: Bool {-toplevel -} -> DefGroup -> Lift DefGroups
liftDefGroup True (DefNonRec def)
  = do (def', groups) <- collectLifted $ liftDef True def
       -- trace ("liftDefNonRec: " ++ show (defName def) ++ ":\n - " ++ traceGroups groups) $
       return $  groups ++ [DefNonRec def'] -- all lifted definitions are put before the current definition

liftDefGroup True (DefRec defs)
  = do (defs', dgroups) <- collectLifted $ mapM (liftDef True) defs
       -- defs' depend on dgroups, but dgroups might depend on defs'
       -- we could to a topological sort here (as in Static/BindingGroups) but for simplicity
       -- we approximate here for now. 
       -- Note that it can be important to have precice DefRec groups for other optimizations, like TRMC. (src/Core/CTail)
       let dnames = defsTNames defs'
           (gnonrecs,grecs) = partition (\dg -> tnamesDisjoint (fv dg) dnames) dgroups
       -- trace ("liftDefRec: " ++ show (map defName defs) ++ ":\n - " ++ traceGroups gnonrecs ++ "\n - " ++ traceGroups grecs) $
       return (gnonrecs ++ [DefRec (flattenDefGroups grecs ++ defs')]) 



liftDefGroup False (DefNonRec def)
  = do def' <- liftDef False def
       return [DefNonRec def']

liftDefGroup False (DefRec defs)
  = do {- traceDoc $ \penv -> text "not-toplevel, recursive def:" <+> text (show (length defs)) 
                             <+> ppName penv (defName (head defs)) 
                             <+> text ", tvs:" 
                             <+> tupled (map (ppTypeVar penv) (tvsList (ftv (defExpr (head defs))))) 
                             <+> text ", fvs:"
                             <+> tupled (map (ppName penv . getName) fvs)
                             <//> prettyDef penv{coreShowDef=True} (head defs) 
       -}
       (callExprs, liftedDefs0) <- fmap unzip $ mapM (makeDef fvs tvs) (zip pinfoss (zip names exprDocs))
       let subst       = zip names callExprs
           liftedDefs  = map (substWithLiftedExpr subst) liftedDefs0
       groups <- liftDefGroup True (DefRec liftedDefs) -- lift all recs to top-level
       -- traceDoc $ \penv -> text ("lifted: " ++ show (map defName liftedDefs)) 
       emitLifteds groups

       let defs' = zipWith (\def callExpr -> def{ defExpr = callExpr
                                                , defSort = liftSort False (defSort def)})
                          defs callExprs
       return (map DefNonRec defs') -- change a DefRec to all DefNonRecs
  where pinfoss = map defParamInfos defs
        exprDocs = map (\def -> (defExpr def, defDoc def)) defs
        exprs = map fst exprDocs
        names = map defTName defs
        fvs = tnamesList $ tnamesRemove names (tnamesUnions $ map freeLocals exprs)
        tvs = tvsList $ tvsUnions $ map ftv exprs

        substWithLiftedExpr subst def
          = let body = case defExpr def of
                        (TypeLam tpars (Lam pars eff lbody)) -> TypeLam tpars (Lam pars eff (subst |~> lbody))
                        (Lam pars eff lbody)                 -> Lam pars eff (subst |~> lbody)
                        expr -> failure $ ("Core.FunLift.liftDefGroup False DefRec: lifting non-function? " ++ show expr)
            -- Note here we only want to skip the substiution for fvs and tvs,
            -- but we have skipped more than necessary, i.e.,
            -- the part of those tpars and pars that are not in fvs and tvs.
            -- But it is OK here, because fvs/tvs are in the scope of all defs,
            -- and we have ensured that fvs/pars and tvs/tpars are always unique (see Type/Infer.hs).
            in def{defExpr = body}

liftDef :: Bool -> Def -> Lift Def
liftDef topLevel def
  = withCurrentDef def $
    do expr' <- liftExpr topLevel (defExpr def)
       return def{ defExpr = expr', defSort = liftSort topLevel (defSort def)}

liftSort :: Bool -> DefSort -> DefSort
liftSort False (DefFun{}) = DefVal
liftSort _ sort = sort

{-
liftDefExpr :: Bool -> Expr -> Lift Expr
liftDefExpr topLevel expr
  = case expr of
      Lam args eff body
        -> do body' <- liftExpr False body
              let expr' = Lam args eff body'
              return expr'  -- don't lift locally named functions
              {-
              -- top level or simple functions are allowed
              if (topLevel || isSimpleFunc expr)
                then return expr'
                -- lift local functions
                else liftLocalFun expr' eff
              -}
      TypeLam tvars (Lam pars eff lbody) | not topLevel || not (isSimpleFunc expr)
        -> do expr1 <- liftExpr False lbody
              liftLocalFun (TypeLam tvars (Lam pars eff expr1)) eff

      _ -> liftExpr topLevel expr
-}

liftExpr :: Bool
         -> Expr
         -> Lift Expr
liftExpr topLevel expr
  = case expr of
    App f args
      -> do f' <- liftExpr False f
            args' <- mapM (liftExpr False) args
            return (App f' args')

    Lam args eff body  -- don't lift anonymous functions
      -> do body' <- liftExpr False body
            let expr' = Lam args eff body'
            return expr'
{-
            --  top level or simple functions are allowed
            if (topLevel || isSimpleFunc expr) then return expr'
            --  lift local functions
            else liftLocalFun expr' eff

    TypeLam tvars (Lam pars eff lbody) | not topLevel && not (isSimpleFunc expr)
      -> do expr1 <- liftExpr False lbody
            liftLocalFun (TypeLam tvars (Lam pars eff expr1)) eff
-}
    Let defgs body
      -> do -- liftTrace ("let hi "  ++ show expr)
            defgs' <- liftDefGroups False defgs
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

{-
liftLocalFun :: Expr -> Effect -> Lift Expr
liftLocalFun expr eff
  = do let fvs = tnamesList $ freeLocals expr
           tvs = tvsList (ftv expr)
       (expr2, liftDef) <- makeDef fvs tvs (expr,"")
       emitLifted (DefNonRec liftDef)
       return expr2
-}

makeDef :: [TName] -> [TypeVar] -> ([ParamInfo], (TName, (Expr, String))) -> Lift (Expr, Def)
makeDef fvs tvs (pinfos, (origName, (expr, doc)))
  = do -- liftTrace (show expr)
       dnames <- currentDefNames
       (name,inl) <- uniqueNameCurrentDef
       let (callExpr,lifted) = (etaExpr name, liftedDef dnames name inl)
       -- traceDoc $ \penv -> text "lifting:" <+> ppName penv name <.> colon <+> text "tvs:" <+> tupled (map (ppTypeVar penv) tvs) <//> prettyExpr penv expr <//> text "to:" <+> prettyDef penv{coreShowDef=True} lifted
       return (callExpr,lifted)
  where
    (tpars,pars,eff,body) -- :: ([TypeVar],[TName],Type)
      = case expr of
          (TypeLam tpars (Lam pars eff lbody)) -> (tpars, map unwild pars, eff, lbody)
          (Lam pars eff lbody)                 -> ([], map unwild pars, eff, lbody)
          _ -> failure $ ("Core.FunLift.makeDef: lifting non-function? " ++ show expr)

    unwild (TName name tp)
      = TName (if (head (nameId name) == '_') then prepend "wild" name else name) tp

    alltpars = tvs ++ tpars
    allpars  = fvs ++ pars
    allargs  = [Var tname InfoNone | tname <- allpars]
    allpinfos = [Own | _ <- fvs] ++ pinfos

    liftedFun = addTypeLambdas alltpars $ Lam allpars eff body
    liftedTp  = typeOf liftedFun
    liftedDef dnames name inl 
            = Def name liftedTp liftedFun Private (defFun allpinfos) inl rangeNull 
              $ "// lifted local: " ++ concat (intersperse ", " (map (show . unqualify) (dnames ++ [getName origName]))) ++ "\n" ++ doc

    funExpr name
      = Var (TName name liftedTp) (InfoArity (length alltpars) (length allargs))

    etaExpr name
      = case (tvs,fvs) of
         ([],[]) -> funExpr name
         _ -> addTypeLambdas tpars $ Lam pars eff $
               App (addTypeApps (alltpars) (funExpr name)) (allargs)

liftBranch :: Branch -> Lift Branch
liftBranch (Branch pat guards)
  = do guards' <- mapM liftGuard guards
       return (Branch pat guards')

liftGuard :: Guard -> Lift Guard
liftGuard (Guard guard body)
  = do guard' <- liftExpr False guard
       body'  <- liftExpr False body
       return (Guard guard' body')

uniqueNameCurrentDef :: Lift (Name,DefInline)
uniqueNameCurrentDef =
  do env <- getEnv
     let defNames = map defName (currentDef env)
     i <- unique
     let -- base     = concatMap (\name -> nameId name ++ "-") (tail $ reverse defNames) ++ "x" ++ show i
         udefName =  toHiddenUniqueName i "lift" (last defNames)
     return (udefName, defInline (last (currentDef env)))

-- Don't lift a simple function. A function is simple if its body is simply an
-- application consisting of simple arguments.
-- More general than eta-expanded expressions.
isSimpleFunc :: Expr -> Bool
isSimpleFunc expr =
  case expr of
    Lam pars _ (App _ args) -> all isSimpleArg args
    TypeLam tpars (Lam pars _ (App (TypeApp _ targs) args))
      -> all isSimpleTArg targs && all isSimpleArg args
    TypeLam tpars (Lam pars _ (App _ args)) -> all isSimpleArg args
    _ -> False
 where -- The definition of simple arguments can be extended.
       isSimpleTArg TCon{}        = True
       isSimpleTArg (TVar x)      = True
       isSimpleTArg (TApp ty tys) = all isSimpleTArg (ty:tys)
       isSimpleTArg _             = False

       isSimpleArg Con{}      = True
       isSimpleArg Lit{}      = True
       isSimpleArg (Var x _)  = True
       isSimpleArg (App e es) = all isSimpleArg (e:es)
       isSimpleArg _          = False

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
      Ok x st _  -> failure $ "Core.FunLift.runLift: unprocessed defgroups"

instance Functor Lift where
  fmap f (Lift c)  = Lift (\env st -> case c env st of
                                        Ok x st' dgs -> Ok (f x) st' dgs)

instance Applicative Lift where
  pure x = Lift (\env st -> Ok x st [])
  (<*>) = ap

instance Monad Lift where
  -- return  = pure
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

currentDefNames :: Lift [Name]
currentDefNames 
  = do env <- getEnv
       return (map defName (currentDef env))

traceDoc :: (Pretty.Env -> Doc) -> Lift ()
traceDoc f
  = do env <- getEnv
       liftTrace (show (f (prettyEnv env)))

liftTrace :: String -> Lift ()
liftTrace msg
  = do env <- getEnv
       trace ("lift: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

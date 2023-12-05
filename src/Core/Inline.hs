-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen, Ningning Xie
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Inline top-level functions (across modules)
-- Does not inline recursive top-level functions, that is done by specialization
-- (See Core/Specialize).
-----------------------------------------------------------------------------

module Core.Inline( inlineDefs
                   ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative
import Data.Maybe( catMaybes )
import Lib.PPrint
import Common.Failure
import Common.NamePrim ( nameEffectOpen )
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
import Core.Core
import qualified Core.Core as Core
import Core.Pretty
import Core.Simplify
import Core.Inlines
import Core.Uniquefy

trace s x =
  -- Lib.Trace.trace s
    x



inlineDefs :: Pretty.Env -> Int -> Inlines -> CorePhase b ()
inlineDefs penv inlineMax inlines
  = liftCorePhaseUniq $ \uniq defs ->
    runInl penv inlineMax uniq inlines  $
    do --traceDoc $ \penv -> text "Core.Inline.inlineDefs:" <+> ppInlines penv inlines
       --inlDefGroups defs
       defs1 <- fmap uniquefyDefGroups $ inlDefGroups defs
       defs2 <- liftUnique (uniqueSimplify penv False True 3 0 defs1)
       fmap uniquefyDefGroups $ inlDefGroups defs2





{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}
inlDefGroups :: DefGroups -> Inl DefGroups
inlDefGroups [] = return []
inlDefGroups (dg:dgs) = inlDefGroup dg (inlDefGroups dgs)

inlDefGroup (DefRec defs) next
  = do defs' <- mapM inlDef defs
       inlExtend True defs' $ do dgs <- next
                                 return (DefRec defs':dgs)
inlDefGroup (DefNonRec def) next
 = do def' <- inlDef def
      inlExtend False [def'] $ do dgs <-  next
                                  return (DefNonRec def':dgs)

inlLocalDefGroups :: DefGroups -> Inl DefGroups
inlLocalDefGroups dgs = mapM inlLocalDefGroup dgs

inlLocalDefGroup (DefRec defs)
  = do defs' <- mapM inlDef defs
       return (DefRec defs')
inlLocalDefGroup (DefNonRec def)
 = do def' <- inlDef def
      return (DefNonRec def')


inlDef :: Def -> Inl Def
inlDef def
  = withCurrentDef def $
    do traceDoc $ \penv -> text "enter def"
       expr' <- inlExpr (defExpr def)
       return def{ defExpr = expr' }

inlExpr :: Expr -> Inl Expr
inlExpr expr
  = case expr of
    -- Applications
    App (TypeApp f targs) args
      -> do f0 <- inlExpr f
            args' <- mapM inlExpr args
            f' <- inlAppExpr f0 (length targs) (argLength args) (onlyZeroCost args)
            return (App (TypeApp f' targs) args')

    App f args
      -> do f0    <- inlExpr f
            args' <- mapM inlExpr args
            f' <- inlAppExpr f0 0 (argLength args) (onlyZeroCost args)
            return (App f' args')

    -- regular cases
    Lam args eff body
      -> do -- inlTraceDoc $ \env -> text "not effectful lambda:" <+> niceType env eff
            body' <- inlExpr body
            return (Lam args eff body')

    Let defgs body
      -> do defgs' <- inlLocalDefGroups defgs
            body'  <- inlExpr body
            return (Let defgs' body')
    Case exprs bs
      -> do exprs' <- mapM inlExpr exprs
            bs'    <- mapM inlBranch bs
            return (Case exprs' bs')

    -- type application and abstraction
    TypeLam tvars body
      -> do body' <- inlExpr body
            return $ TypeLam tvars body'

    TypeApp body tps
      -> do body' <- inlAppExpr body (length tps) 0 False
            return $ TypeApp body' tps

    -- the rest
    _ -> inlAppExpr expr 0 0 False
 where
   argLength args -- prevent inlining of functions with just variable argmuments
     = length args
       -- if (all isVar args) then 0 else length args
   onlyZeroCost args
     = all isVar args

   isVar (Var _ _) = True
   isVar _         = False

inlAppExpr :: Expr -> Int -> Int -> Bool -> Inl Expr
inlAppExpr expr m n onlyZeroCost
  = case expr of
      Var tname varInfo
        -> do mbInfo <- inlLookup (getName tname)
              case mbInfo of
                Just (info,m',n') | not (inlineRec info) && (m >= m') && (n >= n')
                                       && (not onlyZeroCost || inlineKind info == InlineAlways || inlineCost info <= 4)
                  -> do traceDoc $ \penv -> text "inlined:" <+> ppName penv (getName tname)
                        return (inlineExpr info)
                Just (info,m',n')
                  -> do traceDoc $ \penv -> text "inline candidate:" <+> ppName penv (getName tname) <+> 
                                              text (show (m',n')) <+> text "vs" <+> text (show (m,n)) <+>
                                                text ", onlyZeroCost:" <+> pretty onlyZeroCost <+>
                                                  text ", inlineCost:" <+>  pretty (inlineCost info)
                        return (expr)
                Nothing -> do traceDoc $ \penv -> text "not inline candidate:" <+> text (showTName tname)
                              return (expr)
      -- handle .open(f) calls
      TypeApp f targs | m == 0  -- can happen if it is inside an open as:  .open<..>(f<..>)  (test: cgen/inline4)
        -> do f' <- inlAppExpr f (length targs) n onlyZeroCost
              return (TypeApp f' targs)
      App eopen@(TypeApp (Var open info) targs) [f] | getName open == nameEffectOpen
        -> do -- traceDoc $ \penv -> text "go through open:" <+> text (show (m,n))
              f' <- inlAppExpr f m n onlyZeroCost
              return (App eopen [f'])      

      _ -> return (expr)  -- no inlining


inlBranch :: Branch -> Inl Branch
inlBranch (Branch pat guards)
  = do guards' <- mapM inlGuard guards
       return $ Branch pat guards'

inlGuard :: Guard -> Inl Guard
inlGuard (Guard guard body)
  = do guard' <- inlExpr guard
       body'  <- inlExpr body
       return $ Guard guard' body'


{--------------------------------------------------------------------------
  Inl inlad
--------------------------------------------------------------------------}
newtype Inl a = Inl (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env,
                inlines :: Inlines,
                inlineMax :: Int }

data State = State{ uniq :: !Int }

data Result a = Ok a State

runInl :: Pretty.Env -> Int -> Int -> Inlines -> Inl a -> (a,Int)
runInl penv inlMax u inlines (Inl c)
  = case c (Env [] penv inlines inlMax) (State u) of
      Ok x st -> (x,uniq st)

instance Functor Inl where
  fmap f (Inl c)  = Inl (\env st -> case c env st of
                                        Ok x st' -> Ok (f x) st')

instance Applicative Inl where
  pure x = Inl (\env st -> Ok x st)
  (<*>)  = ap

instance Monad Inl where
  -- return = pure
  (Inl c) >>= f = Inl (\env st -> case c env st of
                                      Ok x st' -> case f x of
                                                    Inl d -> d env st' )

instance HasUnique Inl where
  updateUnique f = Inl (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = Inl (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> Inl a -> Inl a
withEnv f (Inl c)
  = Inl (\env st -> c (f env) st)

--withUnique :: (Int -> (a,Int)) -> Inl a
--withUnique f
-- = Inl (\env st -> let (x,u') = f (uniq st) in Ok x (st{ uniq = u'}))

getEnv :: Inl Env
getEnv
  = Inl (\env st -> Ok env st)

updateSt :: (State -> State) -> Inl State
updateSt f
  = Inl (\env st -> Ok st (f st))

withCurrentDef :: Def -> Inl a -> Inl a
withCurrentDef def action
  = -- trace ("inl def: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $
    do -- traceDoc $ (\penv -> text "\ndefinition:" <+> prettyDef penv{Pretty.coreShowDef=True} def)
       action

inlExtend :: Bool -> [Def] -> Inl a -> Inl a
inlExtend isRec defs
  = withEnv (\env -> let inls = catMaybes (map (extractInlineDef (inlineMax env) isRec) defs)
                     in env{ inlines = inlinesExtends inls (inlines env)} )

inlLookup :: Name -> Inl (Maybe (InlineDef,Int,Int))
inlLookup name
  = do env <- getEnv
       case (inlinesLookup name (inlines env)) of
         Just idef | not (inlineDefIsSpecialize idef)  -- no specialization definitions
           -> let (m,n) = getArity (inlineExpr idef)
              in return (Just (idef,m,n))
         _ -> return Nothing

  where
    getArity expr
      = case expr of
          TypeLam tpars (Lam args eff body) -> (length tpars, length args)
          TypeLam tpars body                -> (length tpars, 0)
          Lam args eff body                 -> (0, length args)
          _                                 -> (0,0)

traceDoc :: (Pretty.Env -> Doc) -> Inl ()
traceDoc f
  = do env <- getEnv
       inlTrace (show (f (prettyEnv env)))       

inlTrace :: String -> Inl ()
inlTrace msg
  = do env <- getEnv
       trace ("inl: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

verboseDoc :: (Pretty.Env -> Doc) -> Inl ()
verboseDoc f
  = do env <- getEnv
       when (verbose (prettyEnv env) >= 3) $
         traceDoc f

-----------------------------------------------------------------------------
-- Copyright 2022, Microsoft Research, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

{----------------------------------------------------------------------------
Unroll one level of (some) recursive functions:

  fun map( xs : list<a>, f : a -> e b ) : e list<b>
    match xs
      Cons(x,xx) -> Cons(f(x),xx.map(f))
      Nil        -> Nil

maps to: 

  fun mapx( xs : list<a>, f : a -> e b ) : e list<b>
    match xs
      Cons(x,xx) -> Cons(f(x),xx.mapx(f))
      Nil        -> Nil

  fun map( xs : list<a>, f : a -> e b ) : e list<b>
    match xs
      Nil  -> Nil
      _    -> xs.mapxx(f)
-----------------------------------------------------------------------------}

module Core.Unroll( unrollDefs ) where

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
import Core.CoreVar
import Core.Uniquefy

trace s x =
  Lib.Trace.trace s
    x



unrollDefs :: Pretty.Env -> Int -> CorePhase b ()
unrollDefs penv unrollMax 
  = liftCorePhaseUniq $ \uniq defs ->
    runUnroll penv unrollMax uniq $
    do --traceDoc $ \penv -> text "Core.Unrolline.inlineDefs:" <+> ppUnrollines penv inlines
       unrollDefGroups defs


{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}
unrollDefGroups :: DefGroups -> Unroll DefGroups
unrollDefGroups dgs
  = do xss <- mapM unrollDefGroup dgs
       return (concat xss)

unrollDefGroup :: DefGroup -> Unroll DefGroups
unrollDefGroup (DefRec [def]) 
  = unrollRecDef def

unrollDefGroup dg
 = return [dg]

unrollRecDef :: Def -> Unroll [DefGroup]
unrollRecDef def
  = withCurrentDef def $
    do -- traceDoc $ \penv -> text "enter def"
       dgs <- case defExpr def of
                Lam pars eff body 
                  -> unrollBody def [] pars eff body
                TypeLam tpars (Lam pars eff body) 
                  -> unrollBody def tpars pars eff body
                _ -> return []
       return (if null dgs then [DefRec [def]] else dgs)

unrollBody :: Def -> [TypeVar] -> [TName] -> Effect -> Expr -> Unroll [DefGroup]
unrollBody def tpars pars eff body
  = case body of
      Case exprs (branches@((Branch pats _):(_:_)))  | all (\x -> costExpr x == 0) exprs   -- todo: allow more (total) expressions?
        -> case extractNonRecBranches (defTName def) [] branches of
             (nonrecbs,recbs) | length nonrecbs > 0 && length recbs > 0
               -> do -- unrollTrace "do unroll"
                     let dname = defTName def
                     rname <- uniqueTNameFrom dname
                     let info = InfoArity (length tpars) (length pars)
                         sub  = [(dname, Var rname info)]
                         rdef = def{ defName = getName rname, defExpr = (sub |~> defExpr def), defVis = Private }

                         rcall = App (makeTypeApp (Var rname info) [TVar tv | tv <- tpars]) [Var v InfoNone | v <- pars]
                         wild = Branch (map (\_ -> PatWild) pats) [Guard exprTrue rcall]
                         mkFun b = (if null tpars then id else TypeLam tpars) (Lam pars eff b)
                         ddef = def{ defExpr = mkFun (Case exprs (nonrecbs ++ [wild])), defInline = InlineAlways,
                                     defDoc = "// unrolling of singleton matches of " ++ show (getName rname) ++ "\n" }
                     verboseDoc $ \penv -> text ("unroll " ++ show (defName ddef) ++ "  (to " ++ show (defName rdef) ++ ")")
                     return [DefRec [rdef], DefNonRec ddef]
             _ -> do -- unrollTrace "no unroll"
                     return []
      -- todo: allow (small) let bindings?
      _ -> return []



extractNonRecBranches :: TName -> [Branch] -> [Branch] -> ([Branch],[Branch])
-- stop on end
extractNonRecBranches defname recs []
  = ([],recs)
-- stop also when we cannot push down patterns of recursive branches any further
extractNonRecBranches defname recs (b@(Branch pats guards) : bs)  | any (matchCanOverlap b) recs
  = ([],recs ++ [b] ++ bs)
-- otherwise  
extractNonRecBranches defname recs (b@(Branch pats guards) : bs)
  = if not (all singletonPat pats) ||       -- we only want cheap matches in the unrolling    
       tnamesMember defname (fv guards)     -- and they should be non-recursive
      then -- assume it contains a recursive call
           -- push down as long the other patterns don't match to maximize non-recursive matches
           extractNonRecBranches defname (recs ++ [b]) bs
      else -- surely non-recursive, keep going
           let (nonrecbs,recbs) = extractNonRecBranches defname recs bs
               newb = if null recs then b else dontSkip b
           in (newb:nonrecbs,recbs)

-- is this a singleton (which can be matched without memory access)
singletonPat :: Pattern -> Bool
singletonPat pat
  = case pat of
      PatVar _ p -> singletonPat p
      PatWild    -> True
      PatLit _   -> True
      PatCon{patConPatterns=[]} -> True
      _          -> False

-- Patterns could overlap? (can be conservative, returning True is always ok)
matchCanOverlap (Branch pats1 _) (Branch pats2 _)
  = any patCanOverlap (zip pats1 pats2)
  where
    patCanOverlap pp 
      = case pp of
          (PatWild, _) -> True
          (_, PatWild) -> True
          (PatVar _ p1,p2) -> patCanOverlap (p1,p2)
          (p1,PatVar _ p2) -> patCanOverlap (p1,p2)
          (PatLit lit1,PatLit lit2) 
            -> lit1 == lit2
          (PatCon{patConName=name1}, PatCon{patConName=name2}) 
            -> name1 == name2  -- TODO: make more precise?
          _ -> True

dontSkip :: Branch -> Branch
dontSkip (Branch pats guards)
  = Branch (map noskip pats) guards
  where
    noskip pat 
      = case pat of
          PatVar name p             -> PatVar name (noskip p)
          PatCon{patConPatterns=ps} -> pat{ patConSkip = False, patConPatterns = map noskip ps }
          _                         -> pat 

{--------------------------------------------------------------------------
  Unroll monad
--------------------------------------------------------------------------}
newtype Unroll a = Unroll (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env,
                unrollMax :: Int }

data State = State{ uniq :: !Int }

data Result a = Ok a State

runUnroll :: Pretty.Env -> Int -> Int -> Unroll a -> (a,Int)
runUnroll penv unrollMax u (Unroll c)
  = case c (Env [] penv unrollMax) (State u) of
      Ok x st -> (x,uniq st)

instance Functor Unroll where
  fmap f (Unroll c)  = Unroll (\env st -> case c env st of
                                            Ok x st' -> Ok (f x) st')

instance Applicative Unroll where
  pure x = Unroll (\env st -> Ok x st)
  (<*>)  = ap

instance Monad Unroll where
  -- return = pure
  (Unroll c) >>= f = Unroll (\env st -> case c env st of
                                          Ok x st' -> case f x of
                                                        Unroll d -> d env st' )

instance HasUnique Unroll where
  updateUnique f = Unroll (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = Unroll (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> Unroll a -> Unroll a
withEnv f (Unroll c)
  = Unroll (\env st -> c (f env) st)

--withUnique :: (Int -> (a,Int)) -> Unroll a
--withUnique f
-- = Unroll (\env st -> let (x,u') = f (uniq st) in Ok x (st{ uniq = u'}))

getEnv :: Unroll Env
getEnv
  = Unroll (\env st -> Ok env st)

updateSt :: (State -> State) -> Unroll State
updateSt f
  = Unroll (\env st -> Ok st (f st))

withCurrentDef :: Def -> Unroll a -> Unroll a
withCurrentDef def action
  = -- trace ("inl def: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $
    do -- traceDoc $ (\penv -> text "\ndefinition:" <+> prettyDef penv{Pretty.coreShowDef=True} def)
       action


traceDoc :: (Pretty.Env -> Doc) -> Unroll ()
traceDoc f
  = do env <- getEnv
       unrollTrace (show (f (prettyEnv env)))

unrollTrace :: String -> Unroll ()
unrollTrace msg
  = do env <- getEnv
       trace ("inl: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

uniqueTNameFrom :: TName -> Unroll TName
uniqueTNameFrom tname
  = do i <- unique
       let name = toHiddenUniqueName i "unroll" (getName tname)
       return (TName name (typeOf tname))

verboseDoc :: (Pretty.Env -> Doc) -> Unroll ()
verboseDoc f
  = do env <- getEnv
       when (verbose (prettyEnv env) >= 2) $
         Lib.Trace.trace (show (f (prettyEnv env))) (return ())
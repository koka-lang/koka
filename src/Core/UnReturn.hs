-----------------------------------------------------------------------------
-- Copyright 2016-2017 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

{----------------------------------------------------------------------------
 Transform core with return statements into pure core.
 This is necessary for transformations like "Monadic" that introduce new
 lambda abstractions over pieces of code; if those would still contain return
 statements, these would now return from the inner function instead of the 
 outer one.
-----------------------------------------------------------------------------}

module Core.UnReturn( unreturn
                   ) where


import qualified Lib.Trace 
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim( nameReturn )
import Common.Error
import Common.Syntax

import Kind.Kind
import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty

import Core.Core
import qualified Core.Core as Core
import Core.Pretty
import Core.CoreVar

trace s x =
   -- Lib.Trace.trace s
    x

unreturn :: Pretty.Env -> DefGroups -> Error DefGroups
unreturn penv defs
  = runUR penv 0 (urTopDefGroups defs)
       

{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}  
urTopDefGroups :: DefGroups -> UR (DefGroups)
urTopDefGroups defgs
  = mapM urTopDefGroup defgs

urTopDefGroup :: DefGroup -> UR DefGroup
urTopDefGroup (DefRec defs) 
  = do defs' <- mapM urTopDef defs
       return (DefRec defs')

urTopDefGroup (DefNonRec def)
  = do def' <- urTopDef def
       return (DefNonRec def')

urTopDef :: Def -> UR Def
urTopDef def
  = do (makeDef, kexpr) <- urDef def
       return (makeDef (toExpr kexpr))
  where
    toExpr kexpr 
      = case kexpr of
          U org  -> org
          I e    -> e
          _      -> failure "Core.UnReturn.urTopDef: should not happen: return inside top level definition"




{--------------------------------------------------------------------------
  transform a definition
--------------------------------------------------------------------------}  
urDef :: Def -> UR (Expr -> Def, KExpr)
urDef def 
  = withCurrentDef def $
    do kexpr <- urExpr (defExpr def)
       return (\e -> def{ defExpr = e }, kexpr)

urExpr :: Expr -> UR KExpr
urExpr expr
  = case expr of
      -- lambdas and type lambdas use UnK to contain returns inside their body
      Lam pars eff body
        -> withEffect eff $
           do kbody <- urExpr body
              return (emapUnK expr (Lam pars eff) kbody)

      TypeLam tvars body
        -> do kbody <- urExpr body
              return (emapUnK expr (TypeLam tvars) kbody)

      -- type applications may contain return (? todo check this)
      TypeApp body targs
        -> do kbody <- urExpr body
              return (emapK (Just expr) (\b -> TypeApp b targs) kbody)

      -- bindings
      Let defgs body 
        -> do kbody <- urExpr body 
              urLet expr defgs kbody

      -- case: scrutinee cannot contain return due to grammar
      Case scruts branches
        -> urCase expr scruts branches  
      
      -- return
      App ret@(Var v _) [arg] | getName v == nameReturn
        -> return (R arg)

      -- pure expressions that do not contain return (as checked by the grammar)
      _ -> return (U expr)


urLet :: Expr -> [DefGroup] -> KExpr -> UR KExpr
urLet org defgroups kbody 
  = do kdefgs <- mapM urLetDefGroup defgroups
       trace ("defgroups: " ++ show (length kdefgs)) $
        if (all isUnchanged kdefgs) 
          then return (emapK (Just org) (makeLet defgroups) kbody)
          else return (fold (reverse kdefgs) kbody)
  where
    isUnchanged (Left (_,ks)) = all isU ks
    isUnchanged (Right (_,k)) = isU k

    fold :: [Either ([Expr] -> DefGroup, [KExpr]) (Expr -> DefGroup, KExpr)] -> KExpr -> KExpr
    fold [] kexpr  = kexpr
    fold (Left (makeDefGroup,kexprs) : kdefgs) kexpr
      = fold kdefgs (emapK Nothing (addDef (makeDefGroup (map toExpr kexprs))) kexpr)
    fold (Right (makeDefGroup,kdefexpr) : kdefgs) kexpr
      = fold kdefgs (bind Nothing combine kdefexpr kexpr)
      where
        combine e1 e2 = trace ("combine: " ++ show (e1,e2)) $
                        addDef (makeDefGroup e1) e2

    addDef :: DefGroup -> Expr -> Expr
    addDef def (Let defs expr) = Let (def:defs) expr
    addDef def expr            = Let [def] expr

    toExpr :: KExpr -> Expr
    toExpr kexpr 
      = case kexpr of 
          U org -> org
          I e   -> e
          _     -> failure ("Core.UnReturn.urLet.toExpr: should not happen: return inside recursive definition group")


    urLetDefGroup :: DefGroup -> UR (Either ([Expr] -> DefGroup, [KExpr]) (Expr -> DefGroup, KExpr))
    urLetDefGroup (DefRec defs) 
      = do (mkDefs,kexprs) <- fmap unzip $ mapM urDef defs
           let make exprs = DefRec (zipApply mkDefs exprs)
           return (Left (make,kexprs))
    urLetDefGroup (DefNonRec def) 
      = do (mkDef,kexpr) <- urDef def
           let make expr = DefNonRec (mkDef expr)
           return (Right (make,kexpr))

zipApply fs xs = zipWith (\f x -> f x) fs xs       



urCase :: Expr -> [Expr] -> [Branch] -> UR KExpr
urCase org scruts branches
  = do (mkBranches,kexprss) <- fmap unzip $ mapM urBranch branches
       let ks = concat kexprss  
       if (all isU ks)
        then return (U org) 
       else if (all isUorI ks)
        then return (I (Case scruts $ zipWith (\kexprs mkBranch -> mkBranch $ map toExpr kexprs) 
                                              kexprss mkBranches))
       else if (length (filter (not . isR) ks) <= 1)
        then -- directly inline
             do let f c = Case scruts $
                          zipWith (\kexprs mkBranch -> mkBranch $ map (applyK c) kexprs) 
                                  kexprss mkBranches
                return (F f)
        else -- generate a local continuation function
             do name <- uniqueName "cont"
                pname <- uniqueName "x"
                eff   <- getCurrentEffect
                let tp  = typeOf org
                    parName = TName pname tp
                    parVar  = Var (parName) InfoNone

                let f c = let lam    = Lam [parName] eff (c parVar)
                              defTp  = typeOf lam
                              def    = Def name defTp lam Private (DefFun NoMon) rangeNull ""
                              defVar = Var (TName name defTp) InfoNone -- (InfoArity 0 1 NoMon) -- with arity C# code gets wrong
                              app e  = App defVar [e] 
                          in makeLet [DefNonRec def] $ 
                             Case scruts $
                             zipWith (\kexprs mkBranch -> mkBranch $ map (applyK app) kexprs) 
                                      kexprss mkBranches
                return (F f)
  where
    toExpr :: KExpr -> Expr
    toExpr kexpr 
      = case kexpr of 
          U org -> org
          I e   -> e
          _     -> failure ("Core.UnReturn.urCase.toExpr: should not happen: return inside branches")

    urBranch :: Branch -> UR ([Expr] -> Branch, [KExpr])
    urBranch (Branch pat guards)
      = do (mkGuards,kexprs) <- fmap unzip (mapM urGuard guards)
           return (\exprs -> Branch pat (zipApply mkGuards exprs), kexprs)

    urGuard :: Guard -> UR (Expr -> Guard, KExpr)
    urGuard (Guard test expr)
      = do kexpr <- urExpr expr
           return (Guard test, kexpr)


data KExpr  = U Expr
            | I Expr
            | R Expr
            | F ((Expr -> Expr) -> Expr)



isU (U _) = True
isU _     = False

isUorI (U _) = True
isUorI (I _) = True
isUorI _     = False

isR (R _) = True
isR _     = False


applyK ::(Expr -> Expr) -> KExpr -> Expr
applyK c kexpr
  = case kexpr of
      U org -> c org
      I e -> c e
      R r -> r
      F f -> f c


emapUnK :: Expr -> (Expr -> Expr) -> KExpr -> KExpr
emapUnK org g kexpr
  = case kexpr of
      U _ -> U org
      I e -> I (g e)
      R r -> I (g r)
      F f -> I (g (f id))

emapK :: Maybe Expr -> (Expr -> Expr) -> KExpr -> KExpr
emapK mbOrg g kexpr 
  = case kexpr of
      U e -> case mbOrg of
               Nothing -> I (g e)
               Just org -> U org
      I e -> I (g e)
      R r -> R (g r)
      F f -> F (\c -> g (f c))


bind :: Maybe Expr -> (Expr -> Expr -> Expr) -> KExpr -> KExpr -> KExpr
bind mbOrg combine  ke1 ke2 
  = case (ke1,ke2) of
      (R r, _) -> R r
      (U a, k) -> case k of 
                    U b -> case mbOrg of
                             Nothing -> I (combine a b)
                             Just org -> U org
                    I e -> I (combine a e)
                    R r -> R (combine a r)
                    F g -> F (\c -> combine a (g c))
      (I e1, k)-> case k of 
                    U b -> I (combine e1 b)
                    I e -> I (combine e1 e)
                    R r -> R (combine e1 r)
                    F g -> F (\c -> combine e1 (g c))                   
      (F f, k) -> case k of 
                    U b -> F (\c -> f (\e -> combine e (c b)))
                    I e -> F (\c -> f (\e1 -> combine e1 (c e)))
                    R r -> R (f (\e -> combine e r))
                    F g -> F (\c -> f (\e -> combine e (g c)))


{--------------------------------------------------------------------------
  UR monad
--------------------------------------------------------------------------}  
newtype UR a = UR (Env -> State -> Result a)

data Env = Env{ currentEff :: Effect, currentDef :: [Def], prettyEnv :: Pretty.Env }

data State = State{ uniq :: Int }

data Result a = Ok a State

runUR :: Monad m => Pretty.Env -> Int -> UR a -> m a
runUR penv u (UR c)
  = case c (Env typeTotal [] penv) (State u) of
      Ok x _ -> return x

instance Functor UR where
  fmap f (UR c)  = UR (\env st -> case c env st of 
                                      Ok x st' -> Ok (f x) st')
                                                      
instance Applicative UR where
  pure  = return
  (<*>) = ap                    

instance Monad UR where
  return x      = UR (\env st -> Ok x st)
  (UR c) >>= f = UR (\env st -> case c env st of 
                                    Ok x st' -> case f x of 
                                                   UR d -> d env st' )

instance HasUnique UR where
  updateUnique f = UR (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = UR (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> UR a -> UR a
withEnv f (UR c)
  = UR (\env st -> c (f env) st)

getEnv :: UR Env
getEnv 
  = UR (\env st -> Ok env st)

updateSt :: (State -> State) -> UR State
updateSt f
  = UR (\env st -> Ok st (f st))

withCurrentDef :: Def -> UR a -> UR a
withCurrentDef def action
  = -- trace ("mon def: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $ action


withEffect :: Effect -> UR a -> UR a
withEffect eff action
  = -- trace ("mon def: " ++ show (defName def)) $
    withEnv (\env -> env{currentEff = eff}) $ action

getCurrentEffect :: UR Effect 
getCurrentEffect
  = do env <- getEnv
       return (currentEff env)


urTraceDoc :: (Pretty.Env -> Doc) -> UR ()
urTraceDoc f
  = do env <- getEnv
       urTrace (show (f (prettyEnv env)))

urTrace :: String -> UR ()
urTrace msg
  = do env <- getEnv
       trace ("unreturn: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()


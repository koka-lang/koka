-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen, Alex Reinking
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving  #-}

-----------------------------------------------------------------------------
-- Tail Recursive Modulo Cons implementation
-- See: "Tail Recursion Modulo Context -- An Equational Approach",
--      Daan Leijen and Anton Lorenzen, POPL'22.
-----------------------------------------------------------------------------

module Core.CTail ( ctailOptimize, uctailOptimize ) where

import Lib.Trace (trace)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (catMaybes, isJust, maybeToList)
import Data.Monoid (Any(..))
import qualified Data.Set as S
import qualified Data.IntMap as M

import Kind.Kind
import Kind.Newtypes
import Type.Type
import Type.Kind (effectIsAffine)
import qualified Type.Pretty as Pretty
import Type.Assumption (Gamma)

import Lib.PPrint
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.CoreVar
import Core.Pretty
import Core.AnalysisCCtx( makeCCtxPrim, makeCCtxEmpty, makeCCtxHole, makeFieldAddrOf )

--------------------------------------------------------------------------
-- Reference count transformation
--------------------------------------------------------------------------
ctailOptimize :: Pretty.Env -> TargetPlatform -> Newtypes -> Gamma -> Bool -> CorePhase b ()
ctailOptimize penv tpl newtypes gamma useContextPath
  = liftCorePhaseUniq $ \uniq defs ->
    runUnique uniq (uctailOptimize penv tpl newtypes gamma useContextPath defs)

uctailOptimize :: Pretty.Env -> TargetPlatform -> Newtypes -> Gamma -> Bool -> DefGroups -> Unique DefGroups
uctailOptimize penv tpl newtypes gamma useContextPath defs
  = ctailRun penv tpl newtypes gamma useContextPath (ctailDefGroups True defs)

--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

ctailDefGroups :: Bool -> DefGroups -> CTail DefGroups
ctailDefGroups topLevel defs
  = do dss <- mapM (ctailDefGroup topLevel)  defs
       return (concat dss)

ctailDefGroup :: Bool -> DefGroup -> CTail [DefGroup]
ctailDefGroup topLevel dg
  = -- trace log <$>
    case dg of
      DefRec [def] | hasCTailCall (defTName def) True (defExpr def)
        -> ctailDef topLevel def
      _ -> return [dg]
  where
    log
      | DefRec [def] <- dg  = "ctailDefGroup: " ++ show (defName def) ++ " " ++ (if (hasCTailCall (defTName def) True (defExpr def)) then "IS " else "is NOT ") ++ "eligible for ctail"
      | DefRec defs <- dg   = "ctailDefGroup: found larger DefRec with names: " ++ unwords [show (defName def) | def <- defs ]
      | DefNonRec def <- dg = "ctailDefGroup: found DefNonRec with name: " ++ show (defName def)


{-
we generate
- if the runtime can copy contexts (setContextPath)
  we always generate a single definition which is optimized a bit if the effect is affine for sure (alwaysAffine)
- otherwise, a single definition of the effect is affine for sure (alwaysAffine)
- or two definitions for multiple resumptions (isMulti)
-}
ctailDef :: Bool -> Def -> CTail [DefGroup]
ctailDef topLevel def
  = withCurrentDef def $
    do -- ctailTrace "ctail call definition"
       resRefType <- hasResultRefType (defType def)
       case resRefType of
         Nothing -> return [DefRec [def]]
         Just (tforall,targs,teff,tres)
           -> do -- ctailTrace "- has reference type result"
                 -- With polymorphic recursion a recursive call can be at another type than the
                 -- definition itself, so the type of the hole changes but the root of the context
                 -- stays the same. In that case the trmc function is polymorphic in the root type.
                 mbRoot <- if hasPolymorphicRecursion (defTName def) (defExpr def)
                             then do id <- uniqueId "root"
                                     return (Just (TypeVar id kindStar Bound))
                             else return Nothing
                 let rootTp    = maybe tres TVar mbRoot
                     rootArgs  = map TVar (maybeToList mbRoot)
                     tforallC  = tforall ++ maybeToList mbRoot
                     ctailSlotType = case mbRoot of
                                       Nothing   -> typeCCtx tres
                                       Just root -> TApp typeCCtxx [TVar root, tres]
                     ctailName = makeHiddenName "trmc" (defName def)
                     ctailSlot = newHiddenName "acc"
                     ctailType = tForall tforallC (TFun (targs ++ [(ctailSlot,ctailSlotType)]) teff rootTp)
                     ctailTName= TName ctailName ctailType
                     ctailTSlot= TName ctailSlot ctailSlotType

                 let alwaysAffine = effectIsAffine teff
                 cdefExpr <- withContext ctailTName rootArgs False {-isMulti-} alwaysAffine (Just ctailTSlot) $
                             ctailExpr True (makeCDefExpr mbRoot ctailTSlot (defExpr def))

                 useContextPath <- getUseContextPath
                 let cdef = def{ defName = ctailName, defType = ctailType, defExpr = cdefExpr }
                     needsMulti = not (useContextPath || alwaysAffine)
                     ctailMultiSlotType = TFun [(nameNil,tres)] typeTotal rootTp
                     ctailMultiName  = makeHiddenName "trmcm" (defName def)
                     ctailMultiSlot  = newHiddenName "accm"
                     ctailMultiType  = tForall tforallC (TFun (targs ++ [(ctailMultiSlot,ctailMultiSlotType)]) teff rootTp)
                     ctailMultiTName = TName ctailMultiName ctailMultiType
                     ctailMultiTSlot = TName ctailMultiSlot ctailMultiSlotType
                     ctailMultiVar   = Var ctailMultiTName (InfoArity (length tforallC) (length targs + 1))

                 wrapExpr <- withContext ctailTName [] False alwaysAffine Nothing $
                             do ctailWrapper (isJust mbRoot) ctailTSlot
                                  (if needsMulti then Just ctailMultiVar else Nothing)
                                  (defExpr def)

                  -- todo: check if rec is needed (only if there are non-tail calls left over)
                 if (not needsMulti)
                   then -- for sure, each op resumes at most once
                        return [ DefRec [cdef, def{defExpr = wrapExpr }] ]
                   else -- some ops may resume more than once; specialize for those
                        do cdefMultiExpr <- withContext ctailMultiTName rootArgs True {-isMulti-} alwaysAffine  (Just ctailMultiTSlot) $
                                             ctailExpr True (makeCDefExpr mbRoot ctailMultiTSlot (defExpr def))
                           let cdefMulti = def{ defName = ctailMultiName, defType = ctailMultiType, defExpr = cdefMultiExpr }
                           return $ [ DefRec [cdef, cdefMulti, def{defExpr = wrapExpr} ] ]


makeCDefExpr :: Maybe TypeVar -> TName -> Expr -> Expr
makeCDefExpr mbRoot slot (TypeLam targs body)  = TypeLam (targs ++ maybeToList mbRoot) (makeCDefExpr Nothing slot body)
makeCDefExpr mbRoot slot (Lam args eff body)   = Lam (args ++ [slot]) eff body
makeCDefExpr mbRoot slot body                  = failure $ "Core.CTail: illegal ctail function shape: " ++ show body

-- Is there a recursive call at other type arguments than the type parameters of the definition?
hasPolymorphicRecursion :: TName -> Expr -> Bool
hasPolymorphicRecursion defName (TypeLam tpars body)
  = getAny (foldMapExpr isOtherInstance body)
  where
    isOtherInstance (TypeApp (Var name _) targs)
      = Any (name == defName && not (length targs == length tpars && eqTypes targs (map TVar tpars)))
    isOtherInstance _ = Any False
hasPolymorphicRecursion defName _
  = False


hasResultRefType :: Type -> CTail (Maybe ([TypeVar],[(Name,Type)],Effect,Type))
hasResultRefType tp
  = case splitFunScheme tp of
      Just t@(foralls,targs,teff,tres)
        -> do isRType <- hasRefType tres
              return (if (isRType) then (Just t) else Nothing)
      _ -> return Nothing

hasRefType :: Type -> CTail Bool
hasRefType tp
  = case expandSyn tp of
      TApp t ts   -> hasRefType t
      TCon con    -> not <$> isValueType (typeConName con)
      _           -> return False

----------------------------------------------------------------------------
-- Create a wrapper: this costs one allocation but prevents code duplication
----------------------------------------------------------------------------

ctailWrapper :: Bool -> TName -> Maybe Expr -> Expr -> CTail Expr
ctailWrapper hasRoot slot mbMulti (TypeLam targs (Lam args eff body))
  = do body' <- ctailWrapperBody hasRoot (typeOf body) slot mbMulti targs args
       return (TypeLam targs (Lam args eff body'))
ctailWrapper hasRoot slot mbMulti (Lam args eff body)
  = do body' <- ctailWrapperBody hasRoot (typeOf body) slot mbMulti [] args
       return (Lam args eff body')
ctailWrapper hasRoot slot mbMulti body
  = failure $ "Core.CTail.ctailWrapper: illegal ctail function shape: " ++ show body

-- if the trmc function is polymorphic in the root of the context, the root is the result type
ctailWrapperBody :: Bool -> Type -> TName -> Maybe Expr -> [TypeVar] -> [TName] -> CTail Expr
ctailWrapperBody hasRoot resTp slot mbMulti targs0 args
  = do tailVar <- getCTailFun
       tpl <- getTargetPlatform
       gamma <- getGamma
       let  targs      = [TVar tv | tv <- targs0] ++ (if hasRoot then [resTp] else [])
            ctailCall  = App (makeTypeApp tailVar targs)
                           ([Var name InfoNone | name <- args] ++ [makeCCtxEmpty gamma tpl resTp])
       case mbMulti of
         Nothing -> return ctailCall
         Just ctailMultiVar
           -> -- call either ctail or the regular code
              do x <- uniqueTName resTp
                 let -- ctailMultiVar  = Var ctailMultiTName (InfoArity (length targs) (length args))
                     ctailMultiCall = App (makeTypeApp ctailMultiVar targs)
                                          ([Var name InfoNone | name <- args] ++ [Lam [x] typeTotal (Var x InfoNone)])
                 return (makeIfIsAffine ctailCall ctailMultiCall)

makeIfIsAffine :: Expr -> Expr -> Expr
makeIfIsAffine onTrue onFalse
  = makeIfExpr (App (Var tnameEvvIsAffine (InfoArity 0 0)) []) onTrue onFalse
  where
    tnameEvvIsAffine = TName nameEvvIsAffine (TFun [] typeTotal typeBool)


--------------------------------------------------------------------------
-- Does there exist a tail call definition?
--------------------------------------------------------------------------

hasCTailCall :: TName -> Bool -> Expr -> Bool
hasCTailCall defName top expr
  = case expr of
      TypeLam tpars body  -> hasCTailCall defName top body
      TypeApp body targs  -> hasCTailCall defName top body
      Let dgs body        -> hasCTailCall defName top body
      Lam pars eff body   -> if (top) then hasCTailCall defName False body else False
      Case _ branches     -> any (hasCTailCallBranch defName) branches

      App (TypeApp (Con{}) _) args  -> hasCTailCallArg defName (reverse args)
      App (Con{}) args              -> hasCTailCallArg defName (reverse args)
      _                   -> False

hasCTailCallBranch defName (Branch pat guards)
  = any (hasCTailCallGuard defName) guards

hasCTailCallGuard defName (Guard test expr)
  = hasCTailCall defName False expr


hasCTailCallArg :: TName -> [Expr] -> Bool
hasCTailCallArg defName [] = False
hasCTailCallArg defName (rarg:rargs)
  = case rarg of
      App (TypeApp (Var name _) targs) args   | defName == name -> True
      App (Var name _) args                   | defName == name -> True
      App f@(TypeApp (Con{}) _) fargs
         | tnamesMember defName (fv fargs) && hasCTailCallArg defName (reverse fargs) -- && all isTotal rargs
        -> True
      App f@(Con{}) fargs
        | tnamesMember defName (fv fargs) && hasCTailCallArg defName (reverse fargs) -- && all isTotal rargs
        -> True
      -- todo: emit warning that TRMC does not apply?
      -- _ | not (isTotal rarg) -> trace ("non-total argument: " ++ show rarg) $ False
      _ -> (isTotal rarg && hasCTailCallArg defName rargs)



--------------------------------------------------------------------------
-- Convert to a tail-call function using an accumulator (slot)
--------------------------------------------------------------------------

ctailExpr :: Bool -> Expr -> CTail Expr
ctailExpr top expr
  = do dname <- getCurrentDefName
       case expr of
          TypeLam tpars body  -> TypeLam tpars <$> ctailExpr top body
          Lam pars eff body   | top -> Lam pars eff <$> ctailExpr top body
          Let dgs body        -> Let dgs <$> ctailExpr top body
          Case xs branches    -> Case xs <$> mapM ctailBranch branches

          TypeApp body targs
            -> do expr' <- ctailExpr top body
                  mbSlot  <- getCTailSlot
                  case (expr',mbSlot) of
                    (App v@(Var ctailmSlot _) [arg], Just slot) | getName ctailmSlot == getName slot
                      -> return (App v [TypeApp arg targs])   -- push down typeapp
                    (App v@(TypeApp (Var ctailApply _) _) [acc,arg],_) | getName ctailApply == nameCCtxApply
                      -> return (App v [acc,TypeApp arg targs])   -- push down typeapp into ctail set
                    _ -> return (TypeApp expr' targs)


          App f@(TypeApp (Con cname _) _) fargs
            -> handleConApp dname cname f fargs

          App f@(Con cname _) fargs
            -> handleConApp dname cname f fargs

          App f@(TypeApp (Var name _) targs) fargs | name == dname
            -> handleTailCall (\ctailAt slot -> App (ctailAt targs) (fargs ++ [slot]))

          App f@(Var name _) fargs | name == dname
            -> handleTailCall (\ctailAt slot -> App (ctailAt []) (fargs ++ [slot]))

          _ -> tailResult expr
  where
    tailResult body
      = do mbSlot <- getCTailSlot
           case mbSlot of
              Nothing   -> return body
              Just slot -> do isMulti <- getIsMulti
                              alwaysAffine <- getIsAlwaysAffine
                              tpl <- getTargetPlatform
                              gamma <- getGamma
                              return (makeCCtxApply gamma tpl isMulti alwaysAffine slot body)

    handleConApp dname cname fcon fargs
      = do let mkCons cpath args = bindArgs args $ \xs -> return ([],mkConApp cpath fcon xs)
           isMulti <- getIsMulti
           useContextPath <- getUseContextPath
           alwaysAffine <- getIsAlwaysAffine
           let useCtx = not isMulti && useContextPath && not alwaysAffine
           mbExpr <- ctailTryArg useCtx dname cname Nothing mkCons (length fargs) (reverse fargs)
           case mbExpr of
             Nothing          -> tailResult (App fcon fargs)
             Just (defs,expr) -> return (makeLet defs expr)

    handleTailCall mkCall
      = do mbSlot <- getCTailSlot
           case mbSlot of
             Nothing   -> return expr -- not in ctail variant
             Just slot -> do ctailAt <- getCTailFunAt   -- do a tail call with the current slot
                             return (mkCall ctailAt (Var slot InfoNone))

mkConApp :: CtxPath -> Expr -> [Expr] -> Expr
mkConApp cpath fcon xs
  = case cpath of
      CtxField fname -> case fcon of
                          Con conName conRepr -> App (Con conName conRepr{conCtxPath=cpath}) xs
                          TypeApp (Con conName conRepr) targs -> App (TypeApp (Con conName conRepr{conCtxPath=cpath}) targs) xs
                          _ -> failure ("Core.CTail.mkConApp: invalid constructor: " ++ show fcon)
      _ -> App fcon xs


bindArgs :: [Expr] -> ([Expr] -> CTail ([DefGroup],Expr)) -> CTail ([DefGroup],Expr)
bindArgs args use
  = do (defss,args') <- unzip <$> mapM bindArg args
       (defs,expr) <- use args'
       return (concat defss ++ defs, expr)
  where
    bindArg :: Expr -> CTail ([DefGroup],Expr)
    bindArg arg
      = if (isTotal arg) then return ([],arg)
         else do x <- uniqueTName (typeOf arg)
                 return ([DefNonRec (makeTDef x arg)],Var x InfoNone)


ctailBranch :: Branch -> CTail Branch
ctailBranch (Branch pats guards)
  = do guards' <- mapM ctailGuard guards
       return (Branch pats guards')

ctailGuard :: Guard -> CTail (Guard)
ctailGuard (Guard test expr)  -- expects patAdded in depth-order
  = do expr' <- ctailExpr False expr
       return (Guard test expr')


--------------------------------------------------------------------------
-- See if the tailcall is inside a (nested) constructor application
--------------------------------------------------------------------------

ctailTryArg :: Bool -> TName -> TName -> Maybe TName -> (CtxPath -> [Expr] -> CTail ([DefGroup],Expr)) -> Int -> [Expr] -> CTail (Maybe ([DefGroup],Expr))
ctailTryArg useCtxPath dname cname mbC mkApp field []  = return Nothing
ctailTryArg useCtxPath dname cname mbC mkApp field (rarg:rargs)
  = case rarg of
      App f@(TypeApp (Var name info) targs) fargs
       | (dname == name) -> do expr <- ctailFoundArg cname mbC mkAppNew field
                                         (\ctailAt acc -> App (ctailAt targs) (fargs ++ [acc])) (typeOf rarg) --f farg
                               return (Just expr)
      App f@(Var name info) fargs
       | (dname == name) -> do expr <- ctailFoundArg cname mbC mkAppNew field
                                         (\ctailAt acc -> App (ctailAt []) (fargs ++ [acc])) (typeOf rarg) --f fargs
                               return (Just expr)

      -- recurse into other con
      App f@(TypeApp (Con cname2 _) _) fargs  | tnamesMember dname (fv fargs) -- && all isTotal rargs
       -> do x <- uniqueTName (typeOf rarg)
             ctailTryArg useCtxPath dname cname2 (Just x) (mkAppNested x f) (length fargs) (reverse fargs)

      App f@(Con cname2 _) fargs  | tnamesMember dname (fv fargs)  -- && all isTotal rargs
       -> do x <- uniqueTName (typeOf rarg)
             ctailTryArg useCtxPath dname cname2 (Just x) (mkAppNested x f) (length fargs) (reverse fargs)

      _ -> if (isTotal rarg) then ctailTryArg useCtxPath dname cname mbC (\cpath args -> mkApp cpath (args ++ [rarg])) (field-1) rargs
                             else return Nothing
  where
    -- create a tail call
    mkAppNew
      = \args ->  do cpath <- getCtxPath useCtxPath cname field
                     mkApp cpath (reverse rargs ++ args)
                     {-
                     if not useCtxPath then return (defs,cexpr)
                      else do setfld <- setContextPathExpr cname field
                              x <- uniqueTName (typeOf cexpr)
                              y <- uniqueTName (typeOf cexpr)
                              let cexprdef = DefNonRec (makeTDef y cexpr)
                              let setdef   = DefNonRec (makeTDef x (setfld y))
                              return (defs ++ [cexprdef,setdef], (Var x InfoNone)) -}


    -- create the constructor context (ending in a hole)
    mkAppNested :: TName -> Expr -> (CtxPath -> [Expr] -> CTail ([DefGroup],Expr))
    mkAppNested x fcon
      = \fcpath args ->  do cpath <- getCtxPath useCtxPath cname field
                            (defs,expr) <- bindArgs (reverse rargs) $ \xs -> mkApp cpath (xs ++ [Var x InfoNone])
                            let condef = DefNonRec (makeTDef x (mkConApp fcpath fcon args))
                            return ([condef] ++ defs, expr)
                     {-
                     --(defs,expr) <- bindArgs (reverse rargs) $ \xs -> mkApp cpath (xs ++ [Var x InfoNone])
                     if not useCtxPath
                      then do let condef = DefNonRec (makeTDef x (App fcon args))
                              return ([condef] ++ defs, expr)
                      else do setfld <- setContextPathExpr cname field
                              y <- uniqueTName (typeOf x)
                              let condef = DefNonRec (makeTDef y (App fcon args))
                              let setdef = DefNonRec (makeTDef x (setfld y))
                              return ([condef,setdef] ++ defs, expr)
                     -}

getCtxPath :: Bool -> TName -> Int -> CTail CtxPath
getCtxPath False cname fieldIdx = return CtxNone
getCtxPath useContextPath cname fieldIdx
  = do fieldInfo <- getFieldName cname fieldIdx
       case fieldInfo of
         Left msg -> failure msg -- todo: allow this? see test/cgen/ctail7
         Right (_,fieldName) -> return (CtxField fieldName)



--------------------------------------------------------------------------
-- Found a tail call inside a constructor application
--------------------------------------------------------------------------

ctailFoundArg :: TName -> Maybe TName -> ([Expr] -> CTail ([DefGroup],Expr)) -> Int ->
                  (([Type] -> Expr) -> Expr -> Expr) -> Type {- Expr -> [Expr] -} -> CTail ([DefGroup],Expr)
ctailFoundArg cname mbC mkConsApp field mkTailApp resTp -- f fargs
  = -- ctailTrace $ "found arg: " ++ show cname ++ ", " ++ show mbC ++ ", " ++ show field
    do mbSlot <- getCTailSlot
       case mbSlot of
         Nothing
           -> failure "Core.CTail.ctailFoundArg: inlined trmc is not supported"
         Just slot
           -> do isMulti <- getIsMulti
                 ctailVar  <- getCTailFunAt
                 if isMulti
                   then do x <- uniqueTName resTp
                           (defs,cons) <- mkConsApp [Var x InfoNone]
                           let acc = Lam [x] typeTotal ((App (Var slot InfoNone) [cons]))
                           let ctailCall   = mkTailApp ctailVar acc
                           return (defs,ctailCall)
                   else do fieldInfo <- getFieldName cname field
                           case fieldInfo of
                             Left msg -> failure msg -- todo: allow this? see test/cgen/ctail7
                             Right (_,fieldName) ->
                               do gamma <- getGamma
                                  let -- tp    = typeOf (App f fargs)
                                      hole  = makeCCtxHole gamma resTp
                                  (defs,cons) <- mkConsApp [hole]
                                  consName    <- uniqueTName (typeOf cons)
                                  alwaysAffine <- getIsAlwaysAffine
                                  tpl <- getTargetPlatform
                                  let comp = makeCCtxExtend gamma tpl slot consName (maybe consName id mbC) cname (getName fieldName) resTp alwaysAffine
                                      ctailCall   = mkTailApp ctailVar comp
                                  return $ (defs ++ [DefNonRec (makeTDef consName cons)]
                                            ,ctailCall)


--------------------------------------------------------------------------
-- Primitives
--------------------------------------------------------------------------

-- Extend a context with a non-empty context
makeCCtxExtend :: Gamma -> TargetPlatform -> TName -> TName -> TName -> TName -> Name -> Type -> Bool -> Expr
makeCCtxExtend gamma tpl slot resName objName conName fieldName tp alwaysAffine
  = let fieldOf = makeFieldAddrOf gamma (Var objName InfoNone) conName fieldName tp
        (rootTp,holeTp) = cctxTypeArgs (typeOf slot)
    in  makeCCtxPrim gamma nameCCtxExtend external [rootTp,holeTp,tp] [Var slot InfoNone, Var resName InfoNone, fieldOf]
  where
    external = case tplTarget tpl of
                 C _  -> "kk_cctx_extend" ++ (if alwaysAffine then "_linear" else "") ++ "(#1,#2,#3,kk_context())"
                 JS _ -> "$std_core_types._cctx_extend(#1,#2,#3)"
                 _    -> unsupportedExternal "kk_cctx_extend"



-- Apply a context to its final value.
makeCCtxApply :: Gamma -> TargetPlatform -> Bool {-isMulti-} -> Bool {-isAlwaysAffine-} -> TName -> Expr -> Expr
makeCCtxApply gamma tpl True _ slot expr   -- slot `a -> a` is an accumulating function; apply to resolve
  = App (Var slot InfoNone) [expr]
makeCCtxApply gamma tpl False alwaysAffine slot expr  -- slot is a `ctail<a>`
  = makeCCtxPrim gamma nameCCtxApply external [rootTp,holeTp] [Var slot InfoNone, expr]
  where
    (rootTp,holeTp) = cctxTypeArgs (typeOf slot)
    external = case tplTarget tpl of
                 C _  -> "kk_cctx_apply" ++ (if alwaysAffine then "_linear" else "") ++ "(#1,#2,kk_context())"
                 JS _ -> "$std_core_types._cctx_apply(#1,#2)"
                 _    -> unsupportedExternal "kk_cctx_apply"



-- The root and hole type of a context type `cctx<a,b>` (or `ctx<a>`)
cctxTypeArgs :: Type -> (Type,Type)
cctxTypeArgs tp
  = case expandSyn tp of
      TApp _ [root,hole] -> (root,hole)
      _ -> failure ("Core.CTail.cctxTypeArgs: not a context type: " ++ show tp)

--------------------------------------------------------------------------
-- Utilities for readability
--------------------------------------------------------------------------

-- create a unique name specific to this module
uniqueTName :: Type -> CTail TName
uniqueTName tp = (`TName` tp) <$> uniqueName "trmc"

-- for mapping over a set and collecting the results into a list.
foldMapM :: (Monad m, Foldable t) => (a -> m b) -> t a -> m [b]
foldMapM f = foldr merge (return [])
  where merge x r = do y <- f x
                       (y:) <$!> r

maybeStats :: [Maybe Expr] -> Expr -> Expr
maybeStats xs expr
  = makeStats (catMaybes xs ++ [expr])

--------------------------------------------------------------------------
-- CTail monad
--------------------------------------------------------------------------

-----------------
-- definitions --


data Env = Env { currentDef :: [Def],
                 prettyEnv :: Pretty.Env,
                 newtypes :: Newtypes,
                 gamma :: Gamma,
                 ctailName :: TName,
                 ctailRootArgs :: [Type],
                 ctailSlot :: Maybe TName,
                 isMulti :: Bool,
                 useContextPath :: Bool,
                 alwaysAffine :: Bool,
                 tplatform :: TargetPlatform
               }

data CTailState = CTailState { uniq :: Int }

type CTailM a = ReaderT Env (State CTailState) a

newtype CTail a = CTail (CTailM a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState CTailState)

instance HasUnique CTail where
  updateUnique f = do { old <- getUniq; modifyUniq f; return old }
  setUnique = setUniq

withEnv :: (Env -> Env) -> CTail a -> CTail a
withEnv = local

getEnv :: CTail Env
getEnv = ask

updateSt :: (CTailState -> CTailState) -> CTail ()
updateSt = modify

getSt :: CTail CTailState
getSt = get

ctailRun :: Pretty.Env -> TargetPlatform -> Newtypes -> Gamma -> Bool -> CTail a -> Unique a
ctailRun penv tpl newtypes gamma useContextPath (CTail action)
  = withUnique $ \u ->
      let env = Env [] penv newtypes gamma (TName nameNil typeUnit) [] Nothing True useContextPath False tpl
          st = CTailState u
          (val, st') = runState (runReaderT action env) st
       in (val, uniq st')


withContext :: TName -> [Type] -> Bool -> Bool -> Maybe TName -> CTail a -> CTail a
withContext name rootArgs isMulti alwaysAffine mbSlot action
  = withEnv (\env -> env{ ctailName = name, ctailRootArgs = rootArgs, ctailSlot = mbSlot, isMulti = isMulti, alwaysAffine = alwaysAffine }) action

getCTailFun :: CTail Expr
getCTailFun
  = do ctail <- ctailName <$> getEnv
       let info = case splitFunScheme (typeOf ctail) of
                    Just (foralls,targs,_,_) -> InfoArity (length foralls) (length targs)
                    _ -> InfoNone
       return (Var ctail info)

-- The ctail function applied to the type arguments of a recursive call (followed by the
-- root of the context if the ctail function is polymorphic in it)
getCTailFunAt :: CTail ([Type] -> Expr)
getCTailFunAt
  = do ctailVar <- getCTailFun
       rootArgs <- ctailRootArgs <$> getEnv
       return (\targs -> makeTypeApp ctailVar (targs ++ rootArgs))

getCTailSlot :: CTail (Maybe TName)
getCTailSlot
  = ctailSlot <$> getEnv

getIsMulti :: CTail Bool
getIsMulti
  = isMulti <$> getEnv

getUseContextPath :: CTail Bool
getUseContextPath
  = useContextPath <$> getEnv

getIsAlwaysAffine :: CTail Bool
getIsAlwaysAffine
  = alwaysAffine <$> getEnv

getTargetPlatform :: CTail TargetPlatform
getTargetPlatform 
  = tplatform <$> getEnv

getGamma :: CTail Gamma
getGamma
  = gamma <$> getEnv

getFieldName :: TName -> Int -> CTail (Either String (Expr,TName))
getFieldName cname field
  = do env <- getEnv
       case newtypesLookupAny (getDataTypeName cname) (newtypes env) of
         Just dataInfo ->
           do let (dataRepr,_) = getDataRepr dataInfo
              if (dataReprIsValue dataRepr)
                then return (Left ("cannot optimize modulo-cons tail-call through a value type (" ++ show (getName cname) ++ ")"))
                else do case filter (\con -> conInfoName con == getName cname) (dataInfoConstrs dataInfo) of
                          [con] -> case drop (field - 1) (conInfoParams con) of
                                      ((fname,ftp):_) -> return $ Right (Con cname (getConRepr dataInfo con), TName fname ftp)
                                      _ -> failure $ "Core.CTail.getFieldName: field index is off: " ++ show cname ++ ", field " ++ show  field ++ ", in " ++ show (conInfoParams con)
                          _ -> failure $ "Core.CTail.getFieldName: cannot find constructor: " ++ show cname ++ ", field " ++ show  field ++ ", in " ++ show (dataInfoConstrs dataInfo)
         _ -> failure $ "Core.CTail.getFieldName: no such constructor: " ++ show cname ++ ", field " ++ show  field
  where
    getDataTypeName cname  = case splitFunScheme (typeOf cname) of
                               Just (_,_,_,tres) -> getDataTypeNameRes tres
                               Nothing             -> failure $ "Core.CTail.getFieldName: illegal constructor type: " ++ show cname ++ ", field " ++ show  field ++ ": " ++ show (pretty (typeOf cname))
    getDataTypeNameRes tp  = case expandSyn tp of
                               TApp t ts -> getDataTypeNameRes t
                               TCon tc   -> typeConName tc
                               _         -> failure $ "Core.CTail.getFieldName: illegal result type: " ++ show cname ++ ", field " ++ show  field ++ ": " ++ show (pretty (typeOf cname))

-------------------
-- env accessors --

getCurrentDef :: CTail [Def]
getCurrentDef = currentDef <$> getEnv

withCurrentDef :: Def -> CTail a -> CTail a
withCurrentDef def = withEnv (\e -> e { currentDef = def : currentDef e })

getCurrentDefName :: CTail TName
getCurrentDefName
  = do defs <- getCurrentDef
       let def = head defs
       return (TName (defName def) (defType def))

--

getPrettyEnv :: CTail Pretty.Env
getPrettyEnv = prettyEnv <$> getEnv

withPrettyEnv :: (Pretty.Env -> Pretty.Env) -> CTail a -> CTail a
withPrettyEnv f = withEnv (\e -> e { prettyEnv = f (prettyEnv e) })


---------------------
-- state accessors --

getUniq :: CTail Int
getUniq = uniq <$> getSt

modifyUniq :: (Int -> Int) -> CTail ()
modifyUniq f = updateSt (\s -> s { uniq = f (uniq s) })

setUniq :: Int -> CTail ()
setUniq = modifyUniq . const


isValueType :: Name -> CTail Bool
isValueType name
  = do env <- getEnv
       case newtypesLookupAny name (newtypes env) of
         Just dataInfo -> return $ dataReprIsValue (fst (getDataRepr dataInfo))
         Nothing       -> return False


--------------------------------------------------------------------------
-- Tracing
--------------------------------------------------------------------------

ctailTraceDoc :: (Pretty.Env -> Doc) -> CTail ()
ctailTraceDoc f
 = do pretty <- getPrettyEnv
      ctailTrace (show (f pretty))

ctailTrace :: String -> CTail ()
ctailTrace msg
 = do defs <- getCurrentDef
      trace ("Core.CTail: " ++ show (map defName defs) ++ ": " ++ msg) $
        return ()

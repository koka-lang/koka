-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen, Alex Reinking
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving  #-}

-----------------------------------------------------------------------------
-- Tail Recursive Modulo Cons implementation (called "ctail")
-----------------------------------------------------------------------------

module Core.CTail ( ctailOptimize, uctailOptimize ) where

import Lib.Trace (trace)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.IntMap as M

import Kind.Kind
import Kind.Newtypes
import Type.Type
import Type.Kind (effectIsAffine )
import qualified Type.Pretty as Pretty
import Type.Assumption hiding (InfoExternal)-- Gamma

import Lib.PPrint
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.CoreVar
import Core.Pretty

--------------------------------------------------------------------------
-- Reference count transformation
--------------------------------------------------------------------------
ctailOptimize :: Pretty.Env -> Platform -> Newtypes -> Gamma -> Bool -> DefGroups -> Int -> (DefGroups,Int)
ctailOptimize penv platform newtypes gamma ctailInline defs uniq
  = runUnique uniq (uctailOptimize penv platform newtypes gamma ctailInline defs)

uctailOptimize :: Pretty.Env -> Platform -> Newtypes -> Gamma -> Bool -> DefGroups -> Unique DefGroups
uctailOptimize penv platform newtypes gamma ctailInline defs
  = ctailRun penv platform newtypes gamma ctailInline (ctailDefGroups True defs)

--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

ctailDefGroups :: Bool -> DefGroups -> CTail DefGroups
ctailDefGroups topLevel defs
  = do dss <- mapM (ctailDefGroup topLevel)  defs
       return (concat dss)

ctailDefGroup :: Bool -> DefGroup -> CTail [DefGroup]
ctailDefGroup topLevel dg
  = case dg of
      DefRec [def] | hasCTailCall (defTName def) True (defExpr def)
        -> ctailDef topLevel def
      _ -> return [dg]


ctailDef :: Bool -> Def -> CTail [DefGroup]
ctailDef topLevel def
  = withCurrentDef def $
    do -- ctailTrace "ctail call definition"
       resRefType <- hasResultRefType (defType def)
       case resRefType of
         Nothing -> return [DefRec [def]]
         Just (tforall,tpreds,targs,teff,tres)
           -> do -- ctailTrace "- has reference type result"
                 let ctailSlotType = TApp typeCTail [tres]
                     ctailName = makeHiddenName "ctail" (defName def)
                     ctailSlot = newHiddenName "acc"
                     ctailType = tForall tforall tpreds (TFun (targs ++ [(ctailSlot,ctailSlotType)]) teff tres)
                     ctailTName= TName ctailName ctailType
                     ctailTSlot= TName ctailSlot ctailSlotType

                 cdefExpr <- withContext ctailTName False (Just ctailTSlot) $
                             ctailExpr True (makeCDefExpr ctailTSlot (defExpr def))

                 let cdef = def{ defName = ctailName, defType = ctailType, defExpr = cdefExpr }
                     needsMulti = not (effectIsAffine teff)
                     ctailMultiSlotType = TFun [(nameNil,tres)] typeTotal tres
                     ctailMultiName  = makeHiddenName "ctailm" (defName def)
                     ctailMultiSlot  = newHiddenName "accm"
                     ctailMultiType  = tForall tforall tpreds (TFun (targs ++ [(ctailMultiSlot,ctailMultiSlotType)]) teff tres)
                     ctailMultiTName = TName ctailMultiName ctailMultiType
                     ctailMultiTSlot = TName ctailMultiSlot ctailMultiSlotType
                     ctailMultiVar   = Var ctailMultiTName (InfoArity (length tforall) (length targs + 1))

                 wrapExpr <- withContext ctailTName False Nothing $
                             do ctailWrapper ctailTSlot
                                  (if needsMulti then Just ctailMultiVar else Nothing)
                                  (defExpr def)

                  -- todo: check if rec is needed (only if there are non-tail calls left over)
                 if (not needsMulti)
                   then -- for sure, each op resumes at most once
                        return [ DefRec [cdef, def{defExpr = wrapExpr }] ]
                   else -- some ops may resume more than once; specialize for those
                        do cdefMultiExpr <- withContext ctailMultiTName True (Just ctailMultiTSlot) $
                                             ctailExpr True (makeCDefExpr ctailMultiTSlot (defExpr def))
                           let cdefMulti = def{ defName = ctailMultiName, defType = ctailMultiType, defExpr = cdefMultiExpr }
                           return $ [ DefRec [cdef, cdefMulti, def{defExpr = wrapExpr} ] ]


makeCDefExpr :: TName -> Expr -> Expr
makeCDefExpr slot (TypeLam targs body)  = TypeLam targs (makeCDefExpr slot body)
makeCDefExpr slot (Lam args eff body)   = Lam (args ++ [slot]) eff body
makeCDefExpr slot body                  = failure $ "Core.CTail: illegal ctail function shape: " ++ show body


hasResultRefType :: Type -> CTail (Maybe ([TypeVar],[Pred],[(Name,Type)],Effect,Type))
hasResultRefType tp
  = case splitFunScheme tp of
      Just t@(foralls,preds,targs,teff,tres)
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

ctailWrapper :: TName -> Maybe Expr -> Expr -> CTail Expr
ctailWrapper slot mbMulti (TypeLam targs (Lam args eff body))
  = do body' <- ctailWrapperBody (typeOf body) slot mbMulti targs args
       return (TypeLam targs (Lam args eff body'))
ctailWrapper slot mbMulti (Lam args eff body)
  = do body' <- ctailWrapperBody (typeOf body) slot mbMulti [] args
       return (Lam args eff body')
ctailWrapper slot mbMulti body
  = failure $ "Core.CTail.ctailWrapper: illegal ctail function shape: " ++ show body

ctailWrapperBody :: Type -> TName -> Maybe Expr -> [TypeVar] -> [TName] -> CTail Expr
ctailWrapperBody resTp slot mbMulti targs args
  = do tailVar <- getCTailFun
       let  ctailCall  = App (makeTypeApp tailVar [TVar tv | tv <- targs])
                           ([Var name InfoNone | name <- args] ++ [makeCTailNil resTp])
       case mbMulti of
         Nothing -> return ctailCall
         Just ctailMultiVar
           -> -- call either ctail or the regular code
              do x <- uniqueTName resTp
                 let -- ctailMultiVar  = Var ctailMultiTName (InfoArity (length targs) (length args))
                     ctailMultiCall = App (makeTypeApp ctailMultiVar [TVar tv | tv <- targs])
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
      App f@(TypeApp (Con{}) _) fargs  | tnamesMember defName (fv fargs) && all isTotal rargs
        -> hasCTailCallArg defName (reverse fargs)
      App f@(Con{}) fargs              | tnamesMember defName (fv fargs) && all isTotal rargs
        -> hasCTailCallArg defName (reverse fargs)
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
                    (App v@(TypeApp (Var ctailResolve _) _) [acc,arg],_) | getName ctailResolve == nameCTailResolve
                      -> return (App v [acc,TypeApp arg targs])   -- push down typeapp into ctail set
                    _ -> return (TypeApp expr' targs)


          App f@(TypeApp (Con cname _) _) fargs
            -> handleConApp dname cname f fargs

          App f@(Con cname _) fargs
            -> handleConApp dname cname f fargs

          App f@(TypeApp (Var name _) targs) fargs | name == dname
            -> handleTailCall (\v slot -> App (TypeApp v targs) (fargs ++ [slot]))

          App f@(Var name _) fargs | name == dname
            -> handleTailCall (\v slot -> App v (fargs ++ [slot]))

          _ -> tailResult expr
  where
    tailResult body
      = do mbSlot <- getCTailSlot
           case mbSlot of
              Nothing   -> return body
              Just slot -> do isMulti <- getIsMulti
                              return (makeCTailResolve isMulti slot body)

    handleConApp dname cname f fargs
      = do (defs,body) <- ctailTryArg dname cname Nothing (\args -> ([],App f args)) (length fargs) (reverse fargs)
           if (null defs)
            then tailResult body
            else return $ makeLet defs body

    handleTailCall mkCall
      = do mbSlot <- getCTailSlot
           case mbSlot of
             Nothing   -> return expr -- not in ctail variant
             Just slot -> do ctailVar <- getCTailFun   -- do a tail call with the current slot
                             return (mkCall ctailVar (Var slot InfoNone))


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

ctailTryArg :: TName -> TName -> Maybe TName -> ([Expr] -> (DefGroups,Expr)) -> Int -> [Expr] -> CTail (DefGroups,Expr)
ctailTryArg dname cname mbC mkApp field []  = return (mkApp [])  -- not found
ctailTryArg dname cname mbC mkApp field (rarg:rargs)
  = case rarg of
      App f@(TypeApp (Var name info) targs) fargs
       | (dname == name) -> ctailFoundArg cname mbC mkAppNew field (\v acc -> App (TypeApp v targs) (fargs ++ [acc])) (typeOf rarg) --f fargs
      App f@(Var name info) fargs
       | (dname == name) -> ctailFoundArg cname mbC mkAppNew field (\v acc -> App (v) (fargs ++ [acc])) (typeOf rarg) --f fargs

      -- recurse into other con
      App f@(TypeApp (Con cname2 _) _) fargs  | tnamesMember dname (fv fargs) && all isTotal rargs
       -> do x <- uniqueTName (typeOf rarg)
             ctailTryArg dname cname2 (Just x) (mkAppNested x f) (length fargs) (reverse fargs)

      App f@(Con cname2 _) fargs  | tnamesMember dname (fv fargs)  && all isTotal rargs
       -> do x <- uniqueTName (typeOf rarg)
             ctailTryArg dname cname2 (Just x) (mkAppNested x f) (length fargs) (reverse fargs)

      _ -> if (isTotal rarg) then ctailTryArg dname cname mbC (\args -> mkApp (args ++ [rarg])) (field-1) rargs
                             else return orig
  where
    orig     = (mkApp (reverse (rarg:rargs)))
    mkAppNew = (\args -> mkApp (reverse rargs ++ args))
    mkAppNested x f = (\args -> let (defs,expr) = mkApp (reverse rargs ++ [Var x InfoNone])
                                in ([DefNonRec (makeTDef x (App f args))]++defs, expr))


--------------------------------------------------------------------------
-- Found a tail call inside a constructor application
--------------------------------------------------------------------------

ctailFoundArg :: TName -> Maybe TName -> ([Expr] -> (DefGroups,Expr)) -> Int ->
                  (Expr -> Expr -> Expr) -> Type {- Expr -> [Expr] -} -> CTail (DefGroups,Expr)
ctailFoundArg cname mbC mkConsApp field mkTailApp resTp -- f fargs
  = -- ctailTrace $ "found arg: " ++ show cname ++ ", " ++ show mbC ++ ", " ++ show field
    do mbSlot <- getCTailSlot
       case mbSlot of
         Nothing
           -> failure "Core.CTail.ctailFoundArg: inlined trmc is not supported"
         Just slot
           -> do isMulti <- getIsMulti
                 ctailVar  <- getCTailFun
                 if isMulti
                   then do x <- uniqueTName resTp
                           let (defs,cons) = mkConsApp [Var x InfoNone]
                               acc         = Lam [x] typeTotal ((App (Var slot InfoNone) [cons]))
                           f <- uniqueTName (typeOf acc)
                           let ctailCall   = mkTailApp ctailVar (Var f InfoNone)
                           return (defs ++ [DefNonRec (makeTDef f acc)],ctailCall)  -- note: don't return empty defs
                   else do (_,fieldName) <- getFieldName cname field
                           let -- tp    = typeOf (App f fargs)
                               hole  = makeCFieldHole resTp
                               (defs,cons)  = mkConsApp [hole]
                           consName  <- uniqueTName (typeOf cons)
                           let link = makeCTailLink slot consName (maybe consName id mbC) cname fieldName resTp
                               ctailCall   = mkTailApp ctailVar link -- App ctailVar (fargs ++ [link])
                           return $ (defs ++ [DefNonRec (makeTDef consName cons)]
                                    ,ctailCall)


--------------------------------------------------------------------------
-- Primitives
--------------------------------------------------------------------------

makeCFieldHole :: Type -> Expr
makeCFieldHole tp
  = App (TypeApp (Var (TName nameCFieldHole funType) (InfoExternal [])) [tp]) []
  where
    funType = TForall [a] [] (TFun [] typeTotal (TVar a))
    a = TypeVar 0 kindStar Bound


makeCTailNil :: Type -> Expr
makeCTailNil tp
  = App (TypeApp (Var (TName nameCTailNil funType) (InfoArity 1 0)) [tp]) []
  where
    funType = TForall [a] [] (TFun [] typeTotal (TApp typeCTail [TVar a]))
    a = TypeVar 0 kindStar Bound


makeCFieldOf :: TName -> TName -> Name -> Type -> Expr
makeCFieldOf objName conName fieldName tp
  = App (TypeApp (Var (TName nameCFieldOf funType) (InfoExternal [])) [tp])
        [Var objName InfoNone, Lit (LitString (showTupled (getName conName))), Lit (LitString (showTupled fieldName))]
  where
    funType = TForall [a] [] (TFun [(nameNil,TVar a),(nameNil,typeString),(nameNil,typeString)]
                                   typeTotal (TApp typeCField [TVar a]))
    a = TypeVar 0 kindStar Bound


makeCTailLink :: TName -> TName -> TName -> TName -> Name -> Type -> Expr
makeCTailLink slot resName objName conName fieldName tp
  = let fieldOf = makeCFieldOf objName conName fieldName tp
    in  App (TypeApp (Var (TName nameCTailLink funType) (InfoArity 1 3)) [tp])
            [Var slot InfoNone, Var resName InfoNone, fieldOf]
  where
    funType = TForall [a] [] (TFun [(nameNil,TApp typeCTail [TVar a]),
                                    (nameNil,TVar a),
                                    (nameNil,TApp typeCField [TVar a])] typeTotal (TApp typeCTail [TVar a]))
    a = TypeVar 0 kindStar Bound


makeCTailResolve :: Bool -> TName -> Expr -> Expr
makeCTailResolve True slot expr   -- slot `a -> a` is an accumulating function; apply to resolve
  = App (Var slot InfoNone) [expr]
makeCTailResolve False slot expr  -- slot is a `ctail<a>`
  = App (TypeApp (Var (TName nameCTailResolve funType) (InfoArity 1 2)) [tp])
        [Var slot InfoNone, expr]
  where
    tp = case typeOf slot of
           TApp _ [t] -> t
    funType = TForall [a] [] (TFun [(nameNil,TApp typeCTail [TVar a]),(nameNil,TVar a)] typeTotal (TVar a))
    a = TypeVar (-1) kindStar Bound


--------------------------------------------------------------------------
-- Utilities for readability
--------------------------------------------------------------------------

-- create a unique name specific to this module
uniqueTName :: Type -> CTail TName
uniqueTName tp = (`TName` tp) <$> uniqueName "ctail"

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
                 platform  :: Platform,
                 newtypes :: Newtypes,
                 gamma :: Gamma,
                 ctailInline :: Bool,
                 ctailName :: TName,
                 ctailSlot :: Maybe TName,
                 isMulti :: Bool
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

ctailRun :: Pretty.Env -> Platform -> Newtypes -> Gamma -> Bool -> CTail a -> Unique a
ctailRun penv platform newtypes gamma ctailInline (CTail action)
  = withUnique $ \u ->
      let env = Env [] penv platform newtypes gamma ctailInline (TName nameNil typeUnit) Nothing True
          st = CTailState u
          (val, st') = runState (runReaderT action env) st
       in (val, uniq st')


withContext :: TName -> Bool -> Maybe TName -> CTail a -> CTail a
withContext name isMulti mbSlot action
  = withEnv (\env -> env{ ctailName = name, ctailSlot = mbSlot, isMulti = isMulti }) action

getCTailFun :: CTail Expr
getCTailFun
  = do ctail <- ctailName <$> getEnv
       let info = case splitFunScheme (typeOf ctail) of
                    Just (foralls,_,targs,_,_) -> InfoArity (length foralls) (length targs)
                    _ -> InfoNone
       return (Var ctail info)

getCTailSlot :: CTail (Maybe TName)
getCTailSlot
  = ctailSlot <$> getEnv

getIsMulti :: CTail Bool
getIsMulti
  = isMulti <$> getEnv

getFieldName :: TName -> Int -> CTail (Expr,Name)
getFieldName cname field
  = do env <- getEnv
       case newtypesLookupAny (getDataTypeName cname) (newtypes env) of
         Just dataInfo -> case filter (\con -> conInfoName con == getName cname) (dataInfoConstrs dataInfo) of
                            [con] -> case drop (field - 1) (conInfoParams con) of
                                       ((fname,ftp):_) -> return $ (Con cname (getConRepr dataInfo con), fname)
                                       _ -> failure $ "Core.CTail.getFieldName: field index is off: " ++ show cname ++ ", field " ++ show  field ++ ", in " ++ show (conInfoParams con)
                            _ -> failure $ "Core.CTail.getFieldName: cannot find constructor: " ++ show cname ++ ", field " ++ show  field ++ ", in " ++ show (dataInfoConstrs dataInfo)
         _ -> failure $ "Core.CTail.getFieldName: no such constructor: " ++ show cname ++ ", field " ++ show  field
  where
    getDataTypeName cname  = case splitFunScheme (typeOf cname) of
                               Just (_,_,_,_,tres) -> getDataTypeNameRes tres
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

getPlatform :: CTail Platform
getPlatform = platform <$> getEnv

getOptCtailInline :: CTail Bool
getOptCtailInline = ctailInline <$> getEnv

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

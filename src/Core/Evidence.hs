-----------------------------------------------------------------------------
-- Copyright 2019 Microsoft Corporation, Daan Leijen, Daniel Hillerström
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Evidence translation for effects
-----------------------------------------------------------------------------

module Core.Evidence( evidenceTransform
                    ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative

import Lib.PPrint

import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim( nameTpEv, nameConEv, nameEffectOpen, nameReturn, nameTpHandled, nameTpHandled1 )
import Common.Error
import Common.Syntax

import Kind.Kind( kindStar, isKindEffect, kindFun, kindEffect, kindHandled )

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

import Control.Exception (assert)

trace s x =
  -- Lib.Trace.trace s
    x

evidenceTransform :: Pretty.Env -> DefGroups -> Error DefGroups
evidenceTransform penv defGroups
  = return defGroups

{--------------------------------------------------------------------------
  Transform definitions.
--------------------------------------------------------------------------}
evDefGroups :: Bool -> DefGroups -> Ev DefGroups
evDefGroups toplevel defGroups
  = mapM (evDefGroup toplevel) defGroups

evDefGroup :: Bool -> DefGroup -> Ev DefGroup
evDefGroup toplevel (DefRec defs)
  = do defs <- mapM (evDef toplevel) defs
       return (DefRec defs)
evDefGroup toplevel (DefNonRec def)
  = do def' <- evDef toplevel def
       return (DefNonRec def')

evDef :: Bool -> Def -> Ev Def
evDef toplevel def
  = do expr' <- evExpr (defExpr def)
       ty' <- evType (defType def)
       return $ def { defExpr = expr', defType = ty' }

{--------------------------------------------------------------------------
  Transform expressions.
--------------------------------------------------------------------------}
evExpr :: Expr -> Ev Expr
-- Effect row open.
evExpr (App (TypeApp (Var open _) [effFrom, effTo, _, _]) [f])
  | getName open == nameEffectOpen = undefined -- FIXME TODO.

-- Interesting cases.
evExpr (Lam params eff body)
  = do (expr, p) <- runIsolated $
         do params' <- mapM param params
            body' <- evExpr body
            p <- getEvContext
            assert (p <<= eff) $
              do q <- bindEvidence eff
                 let p' = p ||= q
                 assert (p' <<= eff) $ return (Lam params' eff body')
       return $ evAbstract p expr
         where param :: TName -> Ev TName
               param (TName name ty)
                 = do ty' <- runIsolated' (evType ty)
                      return (TName name ty')

evExpr (App f args)
  = do f' <- evExpr f
       return (App f' undefined)


--       App f args
--         -> do (p1, f') <- evExpr evCtx f
--               args' <- mapM (evExpr evCtx) args
--               let (p3, f'') = evClosureR pnil (fst . decomposeEvType $ typeOf f') f'
--                   p2        = foldr (<>) pnil (map fst args')
--                   (p6, args'') = transform (typeOf f') (map snd args')
--               return $ (p1 <> p2 <> p3, abstract p6 (App f'' args''))
--               where
--                 transform :: Type -> [Expr] -> (P, [Expr])
--                 transform sft args
--                   = let (q3, ft) = decomposeEvType sft
--                         Just(dom, _, cod) = splitFunType ft
--                         (p6, args')    = pointwise dom args
--                     in if not (isEmptyP p6) && not (isFun cod)
--                        then error "s' is not a function type."
--                        else (p6, args')
--                 pointwise :: [(Name, Type)] -> [Expr] -> (P, [Expr])
--                 pointwise [] [] = (pnil, [])
--                 pointwise ((_, s1) : ps) (arg : args)
--                   = if isFun s1
--                     then let (q4, s1'') = decomposeEvType s1
--                              (q5, s2'') = decomposeEvType (typeOf arg)
--                          in if s1'' /= s2'' then error "s1'' != s2''"
--                             else let p4 = toP q4
--                                      (p6, arg') = evClosureR p4 q5 arg
--                                      (p6', args') = pointwise ps args
--                                  in (p6 <> p6', (abstract p4 arg') : args')
--                     else let (p, args') = pointwise ps args
--                          in (p, arg : args')
--                 pointwise _ _ = error "Arity mismatch."


-- Regular cases.
evExpr (Let defgs body)
  = do defgs' <- evDefGroups False defgs
       body' <- evExpr body
       return $ makeLet defgs' body'


evExpr (Case exprs bs)
  = do exprs' <- mapM evExpr exprs
       bs' <- mapM branch bs
       return $ Case exprs' bs'
         where branch :: Branch -> Ev Branch
               branch (b@(Branch _ guards))
                 = do guards' <- mapM guard guards
                      return $ b { branchGuards = guards' }
               guard :: Guard -> Ev Guard
               guard (Guard test expr)
                 = do test' <- evExpr test
                      expr' <- evExpr expr
                      return $ Guard { guardTest = test'
                                     , guardExpr = expr' }

evExpr (Var (TName name typ) info)
  = do typ' <- evType typ
       let tname = TName name typ'
       let info' =
             case typ of
               TFun _ _ _ ->
                 if arity typ /= arity typ'
                 then InfoNone
                 else info
               _ -> info
       return (Var tname info)
         where arity :: Type -> Int
               arity (TFun dom _ _) = length dom
               arity _ = error "arity applied to non-function type."

evExpr (TypeLam tvars body)
  = do body' <- evExpr body
       return (TypeLam tvars body')

evExpr (TypeApp body tps)
  = do body' <- evExpr body
       tps' <- mapM evType tps
       return (TypeApp body' tps')

evExpr expr = return expr -- leave unchanged.


{--------------------------------------------------------------------------
  Transform types.
--------------------------------------------------------------------------}
evType :: Type -> Ev Type
evType (TFun params eff cod)
  = do params' <- mapM isolateParam params
       q <- bindEvidence eff
       let evs = toTFunParams q
       cod' <- evType cod
       return (typeFun (evs ++ params') eff cod')
         where isolateParam :: (Name, Type) -> Ev (Name, Type)
               isolateParam (name, typ)
                 = do typ' <- runIsolated' (evType typ)
                      return (name, typ')
evType (TForall tyvars preds rho)
  = do preds' <- mapM evPred preds
       rho'   <- evType rho
       return (TForall tyvars preds' rho')
evType (TApp t ts)
  = do t' <- evType t
       ts' <- mapM evType ts
       return (TApp t' ts')
evType (TSyn tysyn ts t)
  = do ts' <- mapM evType ts
       t'  <- evType t
       return (TSyn tysyn ts' t')
evType typ = return typ  -- base cases.


evPred :: Pred -> Ev Pred
evPred (PredSub t0 t1)
  = do t0' <- evType t0
       t1' <- evType t1
       return (PredSub t0' t1')
evPred (PredIFace name ts)
  = do ts' <- mapM evType ts
       return (PredIFace name ts')



{--------------------------------------------------------------------------
  Auxiliary functions on types.
--------------------------------------------------------------------------}
decomposeEvType :: Type -> (Q, Type)
decomposeEvType (TFun params _ _ )
  = (undefined, undefined)
decomposeEvType _ = error "decomposeEvType applied to non-function type."

extractTypeConstant :: Tau -> Tau
extractTypeConstant t
  = case expandSyn t of
      TApp (TCon (TypeCon name _)) [htp] | (name == nameTpHandled || name == nameTpHandled1)
        -> extractTypeConstant htp -- reach under the handled<htp> name to extract htp.
      t' -> t'

{-----------------------------------------------------------------------------
  Evidence context.
-----------------------------------------------------------------------------}
type Evidence = TName
type P = [(Name, Evidence)]
type Q = P

-- Computes the Q-extension of P.
(||=) :: P -> Q -> P
p ||= [] = p
p ||= ((name, tname) : q) = case lookup name p of
                              Nothing -> ((name, tname) : p) ||= q
                              Just _  -> p ||= q

toTFunParams :: Q -> [(Name, Type)]
toTFunParams q = map (\(_, TName name typ) -> (name, typ)) q

toFunParams :: P -> [(Name, Type)]
toFunParams = toTFunParams

pempty :: P
pempty = []

(<<=) :: P -> Effect -> Bool
p <<= eff = let present = map labelName (fst . extractOrderedEffect $ eff)
            in all (\(label, _) -> label `elem` present) p

evAbstract :: P -> Expr -> Expr
evAbstract p (e @ (Lam _ eff _)) = addLambdas (toFunParams p) eff e
evAbstract _ _ = error "evAbstract applied to non-lambda term."

{-----------------------------------------------------------------------------
  Evidence monad.
-----------------------------------------------------------------------------}

data Result a = Result a State
data State    = State { uniq :: Int
                      , penv :: P }

newtype Ev a = Ev (State -> Result a)

instance HasUnique Ev where
  setUnique    i = Ev (\st -> Result () (st { uniq = i }) )
  updateUnique f = Ev (\st -> Result (uniq st) (st { uniq = (f (uniq st)) }))

instance Functor Ev where
  fmap f (Ev env) = Ev (\st -> case env st of
                                Result x st' -> Result (f x) st')

instance Applicative Ev where
  pure = return
  (<*>) = ap

instance Monad Ev where
  return x       = Ev (\st -> Result x st)
  (Ev env) >>= k = Ev (\st -> case env st of
                               Result x st' -> case k x of
                                                Ev env' -> env' st')

-- Monadic operations.
getEvContext :: Ev P
getEvContext = Ev (\st -> Result (penv st) st)

setEvContext :: P -> Ev ()
setEvContext p = Ev (\st -> let st' = st { penv = p }
                            in Result () st')

assertEmptyEvContext :: Ev ()
assertEmptyEvContext
  = do p <- getEvContext
       case p of
         [] -> return ()
         _  -> error "Assertion failure: the evidence context is non-empty."

runIsolated :: Ev a -> Ev (a, P)
runIsolated comp
 = do p <- getEvContext   -- backup context.
      setEvContext pempty -- reset context.
      result <- comp      -- run the computation under the empty context.
      p' <- getEvContext  -- get the final context.
      setEvContext p      -- restore the original context.
      return (result, p')

runIsolated' :: Ev a -> Ev a
runIsolated' comp
  = do result <- runIsolated comp
       return (fst result)

-- bindEvidence :: Effect -> Ev TName
-- bindEvidence eff
--   = let (members, tail) = extractOrderedEffect eff
--         labelNames      = map labelName members
--     in do p <- getEvContext
--        case lookup
-- -- (constants, tail) = extractOrderedEffect eff
-- --         labels            = map (toRuntimeLabelName . labelName) constants
-- --         evTypes           = map (makeEvType . extractTypeConstant) constants

-- getWitness :: Effect -> Ev Expr
-- getWitness eff = undefined

addEvidence :: Name -> Evidence -> Ev ()
addEvidence name ev
  = do p <- getEvContext
       setEvContext ((name, ev) : p)

bindEvidence :: Effect -> Ev Q
bindEvidence eff
  = let (members, tail)  = extractOrderedEffect eff
        staticLabelNames = map labelName members
        types            = map extractTypeConstant members
    in go staticLabelNames types
       where go :: [Name] -> [Type] -> Ev Q
             go [] [] = return []
             go (l : ls) (t : ts)
               = do p <- getEvContext
                    case lookup l p of
                      Nothing ->
                           let runtimeLabel = toRuntimeLabel l
                               ev = TName runtimeLabel (makeEvType t)
                           in do addEvidence l ev
                                 q <- go ls ts
                                 return ((l, ev) : q)
                      Just ev ->
                        do q <- go ls ts
                           return ((l, ev) : q)
             go _ _   = error "impossible"


{--------------------------------------------------------------------------
  Evidence naming.
--------------------------------------------------------------------------}
toRuntimeLabel :: Name -> Name
toRuntimeLabel name = name -- FIXME TODO: use the names generated by the parser.

freshRuntimeLabel :: Name -> Ev Name
freshRuntimeLabel name
  = uniqueName (nameId (prepend "ev-" name))

-- {--------------------------------------------------------------------------
--   transform definition groups
-- --------------------------------------------------------------------------}
-- evDefGroups :: P -> DefGroups -> EvMon (P, DefGroups)
-- evDefGroups evCtx evDefGroups
--   = do defGroups' <- mapM (evDefGroup evCtx) evDefGroups
--        let evCtx' = foldr (<>) pnil (map fst defGroups')
--        return $ (evCtx', map snd defGroups')

-- evDefGroup evCtx (DefRec defs)
--   = do defs' <- mapM (evDef evCtx) defs
--        let evCtx' = foldr (<>) pnil (map fst defs')
--        return $ (evCtx, DefRec (map snd defs'))

-- evDefGroup evCtx (DefNonRec def)
--   = do (evCtx', def') <- evDef evCtx def
--        return $ (evCtx', DefNonRec def')


-- {--------------------------------------------------------------------------
--   transform a definition
-- --------------------------------------------------------------------------}
-- evDef :: P -> Def -> EvMon (P, Def)
-- evDef evCtx def
--   = do (evCtx', expr') <- evExpr evCtx (defExpr def)
--        let ty' = evType (defType def)
--        return $ (evCtx', def { defExpr = expr', defType = ty' })

-- evExpr :: P -> Expr -> EvMon (P, Expr)
-- evExpr evCtx expr
--   = case expr of
--       --  lift _open_ applications
--       App eopen@(TypeApp (Var open _) [effFrom,effTo,_,_]) [f]
--         | getName open == nameEffectOpen
--         -> undefined

--       -- Interesting cases.
--       Lam params eff body
--         -> let params' = map param params
--            in do (evCtx', body') <- evExpr pnil body
--                  return $ (pnil, abstract evCtx' $ Lam params' eff body')
--                    where param :: TName -> TName
--                          param (TName name ty) = TName name (evType ty)
--                          toParam :: (Name, Type) -> TName
--                          toParam (name, ty) = TName name ty

--       App f args
--         -> do (p1, f') <- evExpr evCtx f
--               args' <- mapM (evExpr evCtx) args
--               let (p3, f'') = evClosureR pnil (fst . decomposeEvType $ typeOf f') f'
--                   p2        = foldr (<>) pnil (map fst args')
--                   (p6, args'') = transform (typeOf f') (map snd args')
--               return $ (p1 <> p2 <> p3, abstract p6 (App f'' args''))
--               where
--                 transform :: Type -> [Expr] -> (P, [Expr])
--                 transform sft args
--                   = let (q3, ft) = decomposeEvType sft
--                         Just(dom, _, cod) = splitFunType ft
--                         (p6, args')    = pointwise dom args
--                     in if not (isEmptyP p6) && not (isFun cod)
--                        then error "s' is not a function type."
--                        else (p6, args')
--                 pointwise :: [(Name, Type)] -> [Expr] -> (P, [Expr])
--                 pointwise [] [] = (pnil, [])
--                 pointwise ((_, s1) : ps) (arg : args)
--                   = if isFun s1
--                     then let (q4, s1'') = decomposeEvType s1
--                              (q5, s2'') = decomposeEvType (typeOf arg)
--                          in if s1'' /= s2'' then error "s1'' != s2''"
--                             else let p4 = toP q4
--                                      (p6, arg') = evClosureR p4 q5 arg
--                                      (p6', args') = pointwise ps args
--                                  in (p6 <> p6', (abstract p4 arg') : args')
--                     else let (p, args') = pointwise ps args
--                          in (p, arg : args')
--                 pointwise _ _ = error "Arity mismatch."

--       -- Regular cases
--       Let defgs body
--         -> do (_, defgs') <- evDefGroups evCtx defgs
--               (evCtx', body') <- evExpr evCtx body
--               return $ (evCtx', Let defgs' body')

--       Case exprs bs
--         -> do exprs' <- mapM (evExpr evCtx) exprs
--               bs' <- mapM branch bs
--               let evCtx' = foldr (<>) pnil (map fst exprs')
--               let evCtx'' = foldr (<>) evCtx' (map fst bs')
--               let exprs'' = map snd exprs'
--               let bs'' = map snd bs'
--               return $ (evCtx'', Case exprs'' bs'')
--                 where branch :: Branch -> EvMon (P, Branch)
--                       branch (Branch pats guards)
--                         = do guards' <- mapM guard guards
--                              let evCtx' = foldr (<>) pnil (map fst guards')
--                              let guards'' = map snd guards'
--                              return $ (evCtx', Branch { branchPatterns = pats
--                                                       , branchGuards   = guards'' })
--                       guard :: Guard -> EvMon (P, Guard)
--                       guard (Guard test expr)
--                         = do (evCtx', test') <- evExpr evCtx test
--                              (evCtx'', expr') <- evExpr evCtx expr
--                              return $ (evCtx' <> evCtx'',
--                                        Guard { guardTest = test'
--                                              , guardExpr = expr' })

--       Var (TName name tp) info -- FIXME TODO: potentially update arity information.
--         -> let tp' = evType tp
--            in return $ (pnil, Var (TName name tp') info)

--       TypeLam tvars body
--         -> do (evCtx', body') <- evExpr evCtx body
--               return $ (evCtx', TypeLam tvars body')

--       TypeApp body tps
--         -> let tps' = map evType tps
--            in do (evCtx', body') <- evExpr evCtx body
--                  return $ (evCtx', TypeApp body' tps')

--       _ -> return (pnil, expr) -- leave unchanged


-- abstract :: P -> Expr -> Expr
-- abstract p expr
--   = foldr (\(_, (tname, _)) expr -> Lam [tname] typeTotal expr) expr p

-- {--------------------------------------------------------------------------
--   transform a type
-- --------------------------------------------------------------------------}
-- evType :: Type -> Type
-- evType typ
--   = case typ of
--      TCon tycon              -> TCon tycon
--      TVar tyvar              -> TVar tyvar
--      TFun params eff cod     ->
--        let params' = map (\(name, t) -> (name, evType t)) params
--            evs     = evFromEffectType eff
--            cod'    = evType cod
--        in foldr
--             (\ev cod -> TFun [ev] typeTotal cod)
--             (TFun params' eff cod')
--             evs
--      TForall tyvars pred rho ->
--        let pred' = map evPred pred
--            rho'  = evType rho
--        in TForall tyvars pred' rho'
--      TApp t ts               ->
--        let t'  = evType t
--            ts' = map evType ts
--        in TApp t' ts'
--      TSyn tysyn ts t         ->
--        let ts' = map evType ts
--            t'  = evType t
--        in TSyn tysyn ts t

-- evPred :: Pred -> Pred
-- evPred (PredSub t t') = PredSub (evType t) (evType t')
-- evPred (PredIFace name ts) = PredIFace name (map evType ts)

-- evFromEffectType :: Tau -> [(Name, Type)]
-- evFromEffectType eff
--   = let (constants, tail) = extractOrderedEffect eff
--         labels            = map (toRuntimeLabelName . labelName) constants
--         evTypes           = map (makeEvType . extractTypeConstant) constants
--     in zip labels evTypes

-- extractTypeConstant :: Tau -> Tau
-- extractTypeConstant t
--   = case expandSyn t of
--       TApp (TCon (TypeCon name _)) [htp] | (name == nameTpHandled || name == nameTpHandled1)
--         -> extractTypeConstant htp -- reach under the handled<htp> name to extract htp.
--       t' -> t'

-- toRuntimeLabelName :: Name -> Name
-- toRuntimeLabelName lbl = makeHiddenName "ev" lbl -- FIXME TODO: match whatever names are generated by the parser.

-- evsFromType :: Type -> Q
-- evsFromType (TFun [(name, ty)] _ cod)
--    | isEvType ty = (recoverLabel name, (name, ty)) : evsFromType cod
--    | otherwise   = []
-- evsFromType _ = []

-- dropEvs :: Type -> Type
-- dropEvs (ty' @ (TFun [(_, ty)] _ cod))
--   | isEvType ty = dropEvs cod
--   | otherwise   = ty'
-- dropEvs ty = ty

-- decomposeEvType :: Type -> (Q, Type)
-- decomposeEvType ty = (evsFromType ty, dropEvs ty)

-- recoverLabel :: Name -> Label
-- recoverLabel name
--   = case nameId name of
--      '.' : 'e' : 'v' : '-' : label -> label
--      _ -> error "Expected an evidence name."

-- -----------------------------------------------------------------------------
-- -- Evidence constructor
-- -----------------------------------------------------------------------------
-- type Label = String
-- data Ev = Ev Label

-- evClosureR :: P -> Q -> Expr -> (P, Expr)
-- evClosureR _ [] expr = (pnil, expr)
-- evClosureR p ((l, nt) : q) expr
--   = case lookup l p of
--       Nothing
--         -> let (p', expr') = evClosureR p q expr
--                w           = makeWitness nt
--                x           = toVar w
--                p''         = (l, (varName x, varInfo x)) : p'
--                expr''      = App expr' [x]
--            in (p'', expr'')
--       Just (name, info)
--         -> let (p', expr') = evClosureR p q expr
--                expr''      = App expr' [Var { varName = name, varInfo = info }]
--            in (p', expr'')

-- toVar :: Witness -> Expr
-- toVar (name, info) = Var { varName = name, varInfo = info }

-- type Witness = (TName, VarInfo)

-- makeWitness :: (Name, Type) -> Witness
-- makeWitness (lbl, t)
--   = (TName lbl t, InfoNone) -- FIXME TODO: include correct var info.

-- toP :: Q -> P
-- toP q = map (\(lbl, nt) -> (lbl, makeWitness nt)) q

-- -----------------------------------------------------------------------------
-- -- Evidence Monad
-- -----------------------------------------------------------------------------

-- newtype EvMon a = EvMon (Env -> State -> Result a)

-- type Q = [(Label, (Name, Type))]
-- type P = [(Label, (TName, VarInfo))]

-- pnil :: P
-- pnil = []

-- (<>) :: P -> P -> P
-- p0 <> p1 = mappend p0 p1

-- isEmptyP :: P -> Bool
-- isEmptyP [] = True
-- isEmptyP _  = False

-- data Env = Env { penv_ :: P }

-- getEnv :: EvMon Env
-- getEnv = EvMon (\env st -> Ok env st)

-- withEnv :: (Env -> Env) -> EvMon a -> EvMon a
-- withEnv f (EvMon ctxt)
--   = EvMon (\env st -> ctxt (f env) st)

-- -- evLookup :: Label -> EvMon (Maybe Ev)
-- -- evLookup label
-- --   = do env <- getEnv
-- --        return $ lookup label (penv_ env)

-- -- evInsert :: Label -> Ev -> EvMon a -> EvMon a
-- -- evInsert label ev evm
-- --   = withEnv (\env -> Env { penv_ = (label, ev) : (penv_ env) }) evm

-- data State = State { uniq :: Int }

-- data Result a = Ok a State

-- instance Functor EvMon where
--   fmap f (EvMon ctxt) = EvMon (\env st -> case ctxt env st of
--                                             Ok x st' -> Ok (f x) st')

-- instance Applicative EvMon where
--   pure = return
--   (<*>) = ap

-- instance Monad EvMon where
--   return x           = EvMon (\_ st -> Ok x st)
--   (EvMon ctxt) >>= k = EvMon (\env st -> case ctxt env st of
--                                            Ok x st' -> case k x of
--                                                          EvMon ctxt' -> ctxt' env st')

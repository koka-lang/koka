-----------------------------------------------------------------------------
-- Copyright 2016 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

module Core.Cps( cpsTransform ) where


import Lib.Trace 
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim( nameTpCps, nameEffectOpen, nameYieldOp )
import Common.Error

import Kind.Kind( kindStar, isKindEffect )
import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Core.Core

cpsTransform :: Pretty.Env -> DefGroups -> Error DefGroups
cpsTransform penv defs
  = runCps penv 0 (cpsDefGroups defs)

{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}  
cpsDefGroups :: DefGroups -> Cps DefGroups
cpsDefGroups cpsDefGroups
  = do defGroupss <- mapM cpsDefGroup cpsDefGroups
       return (concat defGroupss)

cpsDefGroup (DefRec defs) 
  = do defss <- mapM  cpsDef defs
       return [DefRec (concat defss)]

cpsDefGroup (DefNonRec def)
  = do defs <- cpsDef def
       return (map DefNonRec defs)


{--------------------------------------------------------------------------
  transform a definition
--------------------------------------------------------------------------}  
cpsDef :: Def -> Cps [Def]
cpsDef def 
  = do pureTvs <- getPureTVars
       if (needsCpsDef pureTvs def) -- only translate when necessary
        then cpsDefX def
        else return [def]

cpsDefX :: Def -> Cps [Def]
cpsDefX def
  = withCurrentDef def $
    do tp   <- cpsTypeX (defType def)
       expr <- cpsExpr (defExpr def)
       return [def{ defType = tp, defExpr = expr id}]

type Trans a = TransX a a 
type TransX a b  = (a -> b) ->b

cpsExpr :: Expr -> Cps (TransX Expr Expr)
cpsExpr expr
  = case expr of
      Lam args eff body 
        -> do body' <- cpsExpr body
              isCps <- needsCpsEffectX eff
              if (not isCps)
               then do cpsTrace "not effectful lambda"
                       return $ \k -> k (Lam args eff (body' id))
               else let bodyTp = typeOf body
                    in return $ \k -> k (Lam (args ++ [tnameK bodyTp]) eff (body' (\xx -> App (varK bodyTp) [xx])))
      App f args
        -> do f' <- cpsExpr f
              args' <- mapM cpsExpr args
              let ftp = typeOf f
              isCps <- needsCpsTypeX (typeOf f)
              cpsTraceDoc $ \env -> text "app:" <+> pretty isCps <+> text "tp:" <+> niceType env (typeOf f)                         
              if (not (isCps || isSpecialCps f))
               then return $ \k -> 
                f' (\ff -> 
                  applies args' (\argss -> 
                    k (App ff argss)
                ))
               else  do cpsTraceDoc $ \env -> text "app tp:" <+> niceType env (typeOf f) 
                        return $ \k ->
                          let resTp = typeOf expr
                              tnameY = TName nameY resTp
                              cont = case k (Var tnameY InfoNone) of
                                        -- optimize (fun(y) { let x = y in .. })
                                       Let [DefNonRec def@(Def{ defExpr = Var v _ })] body 
                                        | getName v == nameY 
                                        -> Lam [TName (defName def) (defType def)] typeTotal body 
                                       -- optimize (fun (y) { k(y) } )
                                       App vark@(Var k _) [Var v _] 
                                        | getName k == nameK && getName v == nameY
                                        -> vark
                                       body -> Lam [tnameY] typeTotal body
                          in
                          f' (\ff ->
                            applies args' (\argss -> 
                              App ff (argss ++ [cont])
                          ))
      Let defgs body 
        -> do defgs' <- cpsLetGroups defgs
              body'  <- cpsExpr body
              return $ \k -> defgs' (\dgs -> Let dgs (body' k))
      Case exprs bs
        -> do exprs' <- cpsTrans cpsExpr exprs
              bs'    <- mapM cpsBranch bs
              return $ \k -> exprs' (\xxs -> Case xxs (map (\b -> b k) bs'))              
      TypeLam tvars body
        -> do body' <- cpsExpr body
              return $ \k -> body' (\xx -> k (TypeLam tvars xx))
      TypeApp body tps
        -> do body' <- cpsExpr body
              tps'  <- mapM cpsTypeX tps
              return $ \k -> body' (\xx -> k (TypeApp xx tps'))
      Var (TName name tp) info
        -> do tp' <- cpsTypeX tp
              return (\k -> k (Var (TName name tp') info))              
      _ -> return (\k -> k expr) -- leave unchanged

cpsBranch :: Branch -> Cps ((Expr -> Expr) -> Branch)
cpsBranch (Branch pat guards)
  = do guards' <- mapM cpsGuard guards
       return $ \k -> Branch pat (map (\g -> g k) guards')

cpsGuard :: Guard -> Cps ((Expr -> Expr) -> Guard)
cpsGuard (Guard guard body)       
  = do -- guard' <- cpsExpr guard  -- guards are total!
       body'  <- cpsExpr body
       return $ \k -> Guard guard (body' k)

cpsLetGroups :: DefGroups -> Cps (TransX DefGroups Expr)
cpsLetGroups 
  = cpsTrans cpsLetGroup 

cpsLetGroup :: DefGroup -> Cps (TransX DefGroup Expr)
cpsLetGroup dg
  = case dg of
      DefRec defs -> do ldefs <- cpsTrans cpsLetDef defs
                        return $ \k -> ldefs (\ds -> k (DefRec ds))
      DefNonRec d -> do ldef <- cpsLetDef d
                        return $ \k -> ldef (\dd -> k (DefNonRec dd))

cpsLetDef :: Def -> Cps (TransX Def Expr)
cpsLetDef def
  = {- do pureTvs <- getPureTVars
       if (not (needsCpsDef pureTvs def)) -- only translate when necessary
        then return $ \k -> k def
        else 
    -}
    withCurrentDef def $
             do tp'   <- cpsTypeX (defType def)
                expr' <- cpsExpr (defExpr def)
                return $ \k -> expr' (\xx -> k def{ defType=tp', defExpr=xx })

cpsTrans :: (a -> Cps (TransX a b)) -> [a] -> Cps (TransX [a] b)
cpsTrans f xs
  = case xs of
      [] -> return $ \k -> k []
      (x:xx) -> do x'  <- f x
                   xx' <- cpsTrans f xx
                   return $ \k -> x' (\y -> xx' (\ys -> k (y:ys)))



applies :: [Trans Expr] -> ([Expr] -> Expr) -> Expr
applies [] f = f []
applies (t:ts) f 
  = t (\c -> applies ts (\cs -> f (c:cs)))

needsCpsTypeX :: Type -> Cps Bool
needsCpsTypeX tp
  = do pureTvs <- getPureTVars
       return (needsCpsType pureTvs tp)

cpsTypeX :: Type -> Cps Type
cpsTypeX tp
  = do pureTvs <- getPureTVars
       return (cpsType pureTvs tp)

needsCpsEffectX :: Type -> Cps Bool
needsCpsEffectX tp
  = do pureTvs <- getPureTVars
       return (needsCpsEffect pureTvs tp)

cpsType :: Tvs -> Type -> Type
cpsType pureTvs tp
  = case tp of
      TFun pars eff res  
        -> let pars' = [(name, cpsType pureTvs par) | (name,par) <- pars]
               res'  = cpsType pureTvs res
               eff'  = cpsType pureTvs eff
           in if (needsCpsEffect pureTvs eff')
               then TFun (pars ++ [(nameK, typeK res')]) eff' typeCps 
               else TFun pars' eff' res'
      TForall tvars preds t
        -> TForall tvars preds (cpsType pureTvs t)
      TApp t targs
        -> TApp (cpsType pureTvs t) (map (cpsType pureTvs) targs)
      TVar _ -> tp
      TCon _ -> tp
      TSyn syn tps t 
        -> TSyn syn (map (cpsType pureTvs) tps) (cpsType pureTvs t)

varK tp   = Var (tnameK tp) (InfoArity 0 1)
tnameK tp = TName nameK (typeK tp)
typeK tp  = TFun [(nameNil,tp)] typeTotal typeCps
typeCps   = TCon (TypeCon (nameTpCps) kindStar)

nameK = newHiddenName "k"
nameX = newHiddenName "x"
nameY = newHiddenName "y"


{--------------------------------------------------------------------------
  Check if expressions need to be cps translated
--------------------------------------------------------------------------}  

isSpecialCps :: Expr -> Bool
isSpecialCps expr
  = case expr of
      TypeApp e _ -> isSpecialCps e
      Var v _     -> getName v == nameYieldOp
      _ -> False

-- Does this definition need any cps translation (sometimes deeper inside)
needsCpsDef :: Tvs -> Def -> Bool
needsCpsDef pureTvs def
  = needsCpsType pureTvs (defType def) || needsCpsExpr pureTvs (defExpr def)

needsCpsExpr :: Tvs -> Expr -> Bool
needsCpsExpr pureTvs expr
  = case expr of
      App (TypeApp (Var open _) [_, effTo]) [_] | getName open == nameEffectOpen
        -> needsCpsEffect pureTvs effTo
      App f args 
        -> any (needsCpsExpr pureTvs) (f:args)
      Lam pars eff body
        -> needsCpsEffect pureTvs eff || needsCpsExpr pureTvs body
      TypeApp body targs
        -> any (needsCpsType pureTvs) targs || needsCpsExpr pureTvs body
      TypeLam tpars body
        -> any (isKindEffect . getKind) tpars || needsCpsExpr (tvsRemove tpars pureTvs) body
      Let defs body
        -> any (needsCpsDefGroup pureTvs) defs || needsCpsExpr pureTvs body
      Case exprs bs
        -> any (needsCpsExpr pureTvs) exprs || any (needsCpsBranch pureTvs) bs
      _ -> False

needsCpsDefGroup pureTvs defGroup
  = case defGroup of
      DefRec defs -> any (needsCpsDef pureTvs) defs
      DefNonRec def -> needsCpsDef pureTvs def

needsCpsBranch pureTvs (Branch pat guards)
  = any (needsCpsGuard pureTvs) guards

needsCpsGuard pureTvs (Guard g e)
  = needsCpsExpr pureTvs g || needsCpsExpr pureTvs e

-- Is the type a function with a handled effect?
needsCpsType :: Tvs -> Type -> Bool
needsCpsType pureTvs tp
  = case expandSyn tp of
      TForall vars preds t -> needsCpsType pureTvs t
      TFun args eff res    -> needsCpsEffect pureTvs eff
      _ -> False

needsCpsEffect :: Tvs -> Effect -> Bool
needsCpsEffect pureTvs eff
  = let (ls,tl) = extractEffectExtend eff 
    in any isHandledEffect ls || needsCpsTVar pureTvs tl

needsCpsTVar :: Tvs -> Type -> Bool
needsCpsTVar pureTvs tp
  = case expandSyn tp of
      TVar tv -> not (tvsMember tv pureTvs)
      _       -> False


{--------------------------------------------------------------------------
  Cps monad
--------------------------------------------------------------------------}  
newtype Cps a = Cps (Env -> State -> Result a)

data Env = Env{ pureTVars :: Tvs, prettyEnv :: Pretty.Env, currentDef :: [Def] }

data State = State{ uniq :: Int }

data Result a = Ok a State

runCps :: Monad m => Pretty.Env -> Int -> Cps a -> m a
runCps penv u (Cps c)
  = case c (Env tvsEmpty penv []) (State u) of
      Ok x _ -> return x

instance Functor Cps where
  fmap f (Cps c)  = Cps (\env st -> case c env st of 
                                      Ok x st' -> Ok (f x) st')
                                                      
instance Applicative Cps where
  pure  = return
  (<*>) = ap                    

instance Monad Cps where
  return x      = Cps (\env st -> Ok x st)
  (Cps c) >>= f = Cps (\env st -> case c env st of 
                                    Ok x st' -> case f x of 
                                                   Cps d -> d env st' )

instance HasUnique Cps where
  updateUnique f = Cps (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
  setUnique  i   = Cps (\env st -> Ok () st{ uniq = i} )

withEnv :: (Env -> Env) -> Cps a -> Cps a
withEnv f (Cps c)
  = Cps (\env st -> c (f env) st)

getEnv :: Cps Env
getEnv 
  = Cps (\env st -> Ok env st)

updateSt :: (State -> State) -> Cps State
updateSt f
  = Cps (\env st -> Ok st (f st))

withCurrentDef :: Def -> Cps a -> Cps a
withCurrentDef def 
  = trace ("cps def: " ++ show (defName def)) $
    withEnv (\env -> env{ currentDef = def:currentDef env})

withPureTVars :: [TypeVar] -> Cps a -> Cps a
withPureTVars vs
  = withEnv (\env -> env{ pureTVars = tvsUnion (tvsNew vs) (pureTVars env)})

getPureTVars :: Cps Tvs
getPureTVars
  = do env <- getEnv
       return (pureTVars env)  

isPureTVar :: TypeVar -> Cps Bool
isPureTVar tv 
  = do env <- getEnv
       return (tvsMember tv (pureTVars env))

cpsTraceDoc :: (Pretty.Env -> Doc) -> Cps ()
cpsTraceDoc f
  = do env <- getEnv
       cpsTrace (show (f (prettyEnv env)))

cpsTrace :: String -> Cps ()
cpsTrace msg
  = do env <- getEnv
       trace ("cps: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

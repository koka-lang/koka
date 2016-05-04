-----------------------------------------------------------------------------
-- Copyright 2016 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

module Core.Cps( cpsTransform, cpsType, typeK ) where


import Lib.Trace 
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim( nameTpYld, nameEffectOpen, nameYieldOp, nameTpCps, nameTpCont, nameEnsureK, nameTrue, nameFalse, nameTpBool )
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
import Core.Simplify( simplify )

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
  = do defss <- mapM (cpsDef True) defs
       return [DefRec (concat defss)]

cpsDefGroup (DefNonRec def)
  = do defs <- cpsDef False def
       return (map DefNonRec defs)


{--------------------------------------------------------------------------
  transform a definition
--------------------------------------------------------------------------}  
cpsDef :: Bool -> Def -> Cps [Def]
cpsDef recursive def 
  = do pureTvs <- getPureTVars
       if (needsCpsDef pureTvs def) -- only translate when necessary
        then cpsDefX recursive def 
        else return [def]

cpsDefX :: Bool -> Def -> Cps [Def]
cpsDefX recursive def
  = withCurrentDef def $
    do cpsk <- getCpsTypeX (defType def)
       if (cpsk == PolyCps && defSort def == DefFun)
        then cpsDefDup recursive def 
        else do expr <- cpsExpr' (defExpr def) -- don't increase depth
                return [def{defExpr = expr id}]

cpsDefDup :: Bool -> Def -> Cps [Def]
cpsDefDup recursive def
  = do let teffs = case (expandSyn (defType def)) of
                     TForall tvars _ _ -> filter (isKindEffect . getKind) tvars
                     _ -> []

       exprCps0   <- cpsExpr' (defExpr def)
       exprNoCps0 <- withPureTVars teffs $ cpsExpr' (defExpr def)
       let createDef name expr
            = let tname    = TName name (defType def)
                  (n,m)    = getArity (defType def)
                  var      = Var tname (InfoArity n m)
                  expr'    = if (recursive) 
                               then [(defTName def, var)] |~> expr
                               else expr
              in (def{ defName = name, defExpr = expr' }, var)
           nameCps  = postpend "@cps" (defName def)
           nameNoCps= postpend "@fast" (defName def)
           (defCps,varCps)   = createDef nameCps (exprCps0 id)     
           (defNoCps,varNoCps) = createDef nameNoCps (exprNoCps0 id)

           defPick  = def{ defExpr = exprPick }
           exprPick = case (defExpr defCps) of -- assume (forall<as>(forall<bs> ..)<as>) has been simplified by the cps transform..
                        TypeLam tpars (Lam pars eff body) | length pars > 0 -> TypeLam tpars (Lam pars eff (bodyPick tpars pars))
                        Lam pars eff body                 | length pars > 0 -> Lam pars eff (bodyPick [] pars)
                        _ -> failure $ "Core.Cps.cpsDefDup: illegal cps transformed non-function?: " ++ show (prettyDef defaultEnv def) 

           bodyPick :: [TypeVar] -> [TName] -> Expr
           bodyPick tpars pars 
            = let tnameK = head (reverse pars)
              in makeIfExpr (App (TypeApp varValidK [typeOf tnameK]) [Var tnameK InfoNone])
                            (callPick varCps tpars pars) 
                            (callPick varNoCps tpars (init pars))
           
           callPick :: Expr -> [TypeVar] -> [TName] -> Expr
           callPick var tpars pars
            = let typeApp e = (if null tpars then e else TypeApp e (map TVar tpars))                    
              in App (typeApp var) [Var par InfoNone | par <- pars]

       return [defPick,defCps,defNoCps]


type Trans a = TransX a a 
type TransX a b  = (a -> b) ->b

cpsExpr :: Expr -> Cps (TransX Expr Expr)
cpsExpr expr
  = withIncDepth $ cpsExpr' expr

cpsExpr' :: Expr -> Cps (TransX Expr Expr)
cpsExpr' expr 
  = case expr of
      -- open
      -- simplify open away if it is directly applied
        {-
      App (App (TypeApp (Var open _) [effFrom, effTo]) [f]) args
        | getName open == nameEffectOpen 
        -> cpsExpr (App f args)
      -}

      -- otherwise lift the function
      App eopen@(TypeApp (Var open _) [effFrom,effTo]) [f]
        | getName open == nameEffectOpen         
        -> do isCpsFrom <- needsCpsTypeX effFrom
              isCpsTo   <- needsCpsTypeX effTo
              if (isCpsFrom || not (isCpsTo))
               -- simplify open away if already in cps form, or not in cps at all
               then do cpsTraceDoc $ \env -> text "open: ignore: " <+> tupled (niceTypes env [effFrom,effTo])
                       -- cpsExpr f                       
                       f' <- cpsExpr f
                       return $ \k -> f' (\ff -> k (App eopen [ff]))

               -- lift the function to a continuation function
               else do let Just((partps,_,restp)) = splitFunType (typeOf f)
                       pars <- mapM (\(name,partp) ->
                                     do pname <- if (name==nameNil) then uniqueName "x" else return name
                                        return (TName pname partp)) partps
                       let args = [Var tname InfoNone | tname <- pars]
                           lam  = Lam pars effTo (App f args)
                       cpsExpr lam
      -- regular cases
      Lam args eff body 
        -> do depth <- getExprDepth
              body' <- cpsExpr body
              cpsk  <- getCpsEffectX eff
              args' <- mapM cpsTName args
              if (cpsk == NoCps)
               then do cpsTraceDoc $ \env -> text "not effectful lambda:" <+> niceType env eff
                       return $ \k -> k (Lam args' eff (body' id))
               else -- cps converted lambda: add continuation parameter
                    do resTp <- freshTVar kindStar Meta
                       -- let resTp = typeAny
                       let bodyTp = typeOf body
                           nameK  = tnameK bodyTp eff resTp
                           (nameK0,initk)  = (nameK,id) 
                                             {-
                                              if (depth>0 || cpsk /= PolyCps) 
                                              then (nameK,id) 
                                              else (tnameKN "0" bodyTp eff resTp, 
                                                     ensureK nameK nameK0)
                                             -}
                       return $ \k -> 
                        k (Lam (args' ++ [nameK0]) eff 
                            (initk (body' (\xx -> App (varK bodyTp eff resTp) [xx]))))
      App f args
        -> do f' <- cpsExpr f
              args' <- mapM cpsExpr args
              let -- ff  = f' id
                  ftp = typeOf f -- ff
                  Just(_,feff,_) = splitFunType ftp
              isCps <- needsCpsTypeX ftp
              cpsTraceDoc $ \env -> text "app" <+> (if isCps then text "cps" else text "") <+> text "tp:" <+> niceType env (typeOf f)
              if (not (isCps || isSpecialCps f))
               then return $ \k -> 
                f' (\ff -> 
                  applies args' (\argss -> 
                    k (App ff argss)
                ))
               else  do -- cpsTraceDoc $ \env -> text "app tp:" <+> niceType env (typeOf f) 
                        nameY <- uniqueName "y"
                        return $ \k ->
                          let resTp = typeOf expr
                              tnameY = TName nameY resTp
                              cont = case k (Var tnameY InfoNone) of
                                        -- optimize (fun(y) { let x = y in .. })
                                       Let [DefNonRec def@(Def{ defExpr = Var v _ })] body 
                                        | getName v == nameY 
                                        -> Lam [TName (defName def) (defType def)] feff body 
                                       -- optimize (fun (y) { k(y) } )
                                       App vark@(Var k _) [Var v _] 
                                        | getName k == nameK && getName v == nameY
                                        -> vark
                                       body -> Lam [tnameY] feff body
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
      TypeApp (TypeLam tvars body) tps  | length tvars == length tps
        -- propagate types so that the pure tvs are maintained.
        -- todo: This will fail if the typeapp is not directly around the type lambda!
        -> do cpsExpr' (subNew (zip tvars tps) |-> body)
      TypeLam tvars body
        -> do body' <- cpsExpr' body
              return $ \k -> body' (\xx -> k (TypeLam tvars xx))
      TypeApp body tps
        -> do body' <- cpsExpr body
              -- tps'  <- mapM cpsTypeX tps
              -- tps'  <- mapM cpsTypePar tps0
              return $ \k -> body' (\xx -> k (TypeApp xx tps))
      Var (TName name tp) info
        -> do -- tp' <- cpsTypeX tp
              return (\k -> k (Var (TName name tp) info))              
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
             do -- tp'   <- cpsTypeX (defType def)
                expr' <- cpsExpr (defExpr def)
                return $ \k -> expr' (\xx -> k def{ defExpr=xx })

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

cpsTName :: TName -> Cps TName
cpsTName (TName name tp)
  = do -- tp' <- cpsTypeX tp
       return (TName name tp)

{-
cpsTypePar :: Type -> Cps Type
cpsTypePar tp
  = if (not (isKindEffect (getKind tp))) then return tp 
     else do isCps <- needsCpsTypeX tp
             if (isCps) then return tp
              else -- we go from a polymorpic (cps) type to a non-cps type; mark it with a cont effect
                   -- to do a sound translation. At an application to cont we pass the identity as the continuation.
                   return $ effectExtend (handledToLabel (TCon (TypeCon nameTpCps kindHandled))) tp
-}

needsCpsTypeX :: Type -> Cps Bool
needsCpsTypeX tp
  = do pureTvs <- getPureTVars
       return (needsCpsType pureTvs tp)

getCpsEffectX :: Effect -> Cps CpsTypeKind
getCpsEffectX eff
  = do pureTvs <- getPureTVars
       return (getCpsEffect pureTvs eff)

getCpsTypeX :: Type -> Cps CpsTypeKind
getCpsTypeX tp
  = do pureTvs <- getPureTVars
       return (getCpsType pureTvs tp)       

cpsTypeU :: Type -> Cps Type
cpsTypeU tp
  = do pureTvs <- getPureTVars
       return (cpsType pureTvs tp)

needsCpsEffectX :: Type -> Cps Bool
needsCpsEffectX tp
  = do pureTvs <- getPureTVars
       return (needsCpsEffect pureTvs tp)


cpsType :: Tvs -> Type -> Type
cpsType pureTvs tp
  = fst $ runUnique 0 (cpsTypeX pureTvs tp)
  where
    quantify t = quantifyType (tvsList (ftv t)) t

cpsTypeX :: HasUnique m => Tvs -> Type -> m Type  
cpsTypeX pureTvs tp
  = case tp of
      TFun pars eff res  
        -> do pars' <- mapM (\(name,par) -> do{ t' <- cpsTypeX pureTvs par; return (name,t') }) pars
              res'  <- cpsTypeX pureTvs res
              eff'  <- cpsTypeX pureTvs eff
              if (needsCpsEffect pureTvs eff')
               then do tpYld <- freshTVar kindStar Meta
                       return $ TFun (pars' ++ [(nameK, typeK res' eff' tpYld)]) eff' tpYld
               else return $ TFun pars' eff' res'
      TForall tvars preds t
        -> do t' <- cpsTypeX (tvsRemove tvars pureTvs) t
              return $ TForall tvars preds t'
      TApp t targs
        -> do t'     <- cpsTypeX pureTvs t
              targs' <- mapM (cpsTypeX pureTvs) targs
              return $ TApp t' targs'
      TVar _ -> return tp
      TCon _ -> return tp
      TSyn syn targs t 
        -> do targs' <- mapM (cpsTypeX pureTvs) targs
              t'     <- cpsTypeX pureTvs t
              return $ TSyn syn targs' t'

varValidK :: Expr
varValidK 
  = let typeVar = TypeVar 1 kindStar Bound
    in  Var (TName (newName "@validk") (TForall [typeVar] [] (TFun [(nameNil,TVar typeVar)] typeTotal typeBool)))
            (Core.InfoExternal [(JS,"(#1 !== undefined)")])

ensureK :: TName -> TName -> (Expr -> Expr)
ensureK tname@(TName name tp) namek body
  = let expr = App (Var (TName nameEnsureK (TFun [(nameNil,tp)] typeTotal tp)) 
                  (Core.InfoExternal [(JS,"(#1 || $std_core.id)")])) [Var namek InfoNone]
        def = Def name tp expr Private DefVal rangeNull ""
    in Let [DefNonRec def] body

varK tp effTp resTp    = Var (tnameK tp effTp resTp) (InfoArity 0 1)
tnameK tp effTp resTp  = tnameKN "" tp effTp resTp
tnameKN post tp effTp resTp = TName (postpend post nameK) (typeK tp effTp resTp)
typeK tp effTp resTp   = TSyn (TypeSyn nameTpCont (kindFun kindStar (kindFun kindEffect (kindFun kindStar kindStar))) 0 Nothing) 
                           [tp,effTp,resTp]
                           (TFun [(nameNil,tp)] effTp resTp) 
                          -- TFun [(nameNil,tp)] typeTotal typeYld
 
typeYld   = TCon (TypeCon (nameTpYld) kindStar)

nameK = newHiddenName "k"
nameX = newHiddenName "x"



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
      _ -> needsCpsType pureTvs (typeOf expr) -- because instantiating polymorphic variables may need translation

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
  = getCpsType pureTvs tp /= NoCps

needsCpsEffect :: Tvs -> Effect -> Bool
needsCpsEffect pureTvs eff
  = getCpsEffect pureTvs eff /= NoCps

data CpsTypeKind = NoCps | PolyCps | AlwaysCps deriving (Eq,Show)

-- Is the type a function with a handled effect?
getCpsType :: Tvs -> Type -> CpsTypeKind
getCpsType pureTvs tp
  = case expandSyn tp of
      TForall vars preds t -> getCpsType (tvsRemove vars pureTvs) t
      TFun args eff res    -> getCpsEffect pureTvs eff
      -- TVar tv -> not (tvsMember tv pureTvs)
      _ -> if (isKindEffect (getKind tp))
            then getCpsEffect pureTvs tp
            else NoCps

getCpsEffect :: Tvs -> Effect -> CpsTypeKind
getCpsEffect pureTvs eff
  = let (ls,tl) = extractEffectExtend eff 
    in if (any isHandledEffect ls)
        then AlwaysCps
        else getCpsTVar pureTvs tl

getCpsTVar :: Tvs -> Type -> CpsTypeKind
getCpsTVar pureTvs tp
  = case expandSyn tp of
      TVar tv | not (tvsMember tv pureTvs) -> PolyCps
      _       -> NoCps


{--------------------------------------------------------------------------
  Cps monad
--------------------------------------------------------------------------}  
newtype Cps a = Cps (Env -> State -> Result a)

data Env = Env{ depth:: Int, pureTVars :: Tvs, prettyEnv :: Pretty.Env, currentDef :: [Def] }

data State = State{ uniq :: Int }

data Result a = Ok a State

runCps :: Monad m => Pretty.Env -> Int -> Cps a -> m a
runCps penv u (Cps c)
  = case c (Env 0 tvsEmpty penv []) (State u) of
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
    withEnv (\env -> env{ depth = 0, currentDef = def:currentDef env})

withIncDepth :: Cps a -> Cps a
withIncDepth cps
  = withEnv (\env -> env{ depth = 1 + depth env } ) cps

getExprDepth :: Cps Int
getExprDepth
  = do env <- getEnv
       return (depth env)

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



{--------------------------------------------------------------------------
  Get CpsType
--------------------------------------------------------------------------}
{-
class HasCpsType a where
  cpsTypeOf :: Tvs -> a -> Type

instance HasCpsType Def where
  cpsTypeOf ptvs def  = cpsType ptvs (defType def)

instance HasCpsType TName where
  cpsTypeOf ptvs (TName _ tp)   = cpsType ptvs tp

instance HasCpsType Expr where
  -- Lambda abstraction
  cpsTypeOf ptvs (Lam pars eff expr)
    = let resTp  = cpsTypeOf ptvs expr 
          effTp  = cpsType ptvs eff
          parTps = [(name,cpsType ptvs tp) | TName name tp <- pars]
      in if (needsCpsEffect pureTvs effTp)
          then TFun (parTps ++ [(nameK,typeK resTp)]) effTp typeYld
          else TFun parTps effTp resTp 

  -- Variables
  cpsTypeOf ptvs (Var tname info)
    = cpsTypeOf ptvs tname

  -- Constants 
  cpsTypeOf ptvs (Con tname repr)
    = cpsTypeOf ptvs tname

  -- Application
  cpsTypeOf ptvs (App fun args)
    = snd (splitFun (cpsTypeOf ptvs fun))

  -- Type lambdas
  cpsTypeOf ptvs (TypeLam xs expr)
    = TForall xs [] (cpsTypeOf ptvs expr)

  -- Type application
  cpsTypeOf ptvs (TypeApp expr [])
    = cpsTypeOf ptvs expr

  cpsTypeOf ptvs (TypeApp expr tps)
    = let (tvs,tp1) = splitTForall (cpsTypeOf ptvs expr)
      in -- assertion "Core.Core.cpsTypeOf.TypeApp" (getKind a == getKind tp) $
         subNew (zip tvs (map (cpsTypeOf ptvs tps)) |-> tp1

  -- Literals
  cpsTypeOf (Lit l) 
    = cpsTypeOf l

  -- Let
  cpsTypeOf (Let defGroups expr) 
    = cpsTypeOf expr 

  -- Case
  cpsTypeOf (Case exprs branches)
    = cpsTypeOf (head branches)


instance HasCpsType Lit where
  cpsTypeOf lit
    = case lit of
        LitInt _    -> typeInt
        LitFloat _  -> typeFloat
        LitChar _   -> typeChar
        LitString _ -> typeString

instance HasCpsType Branch where
  cpsTypeOf (Branch _ guards) 
    = case guards of
        (guard:_) -> cpsTypeOf guard
        _         -> failure "Core.Core.HasCpsType Branch: branch without any guards" 

instance HasCpsType Guard where
  cpsTypeOf (Guard _ expr)
    = cpsTypeOf expr
-}
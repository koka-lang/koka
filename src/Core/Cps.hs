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
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim( nameTpYld, nameEffectOpen, nameYieldOp, nameReturn, nameTpCont, nameDeref, nameByref,
                        nameEnsureK, nameTrue, nameFalse, nameTpBool, nameApplyK, nameUnsafeTotal, nameIsValidK )
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
import Core.CoreVar

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
  = do needCps <- needsCpsDef def
       if (not (needCps)) -- only translate when necessary
        then return [def]
        else if (isDupFunctionDef (defExpr def))
              then do defs' <- cpsLetDef recursive def -- re-use letdef
                      case (defs' (\(xds,yds,nds) -> Let (DefRec xds:DefRec yds:map DefNonRec nds) (Var (defTName def) InfoNone))) of
                        Let dgs _ -> return (flattenDefGroups dgs)
                        expr -> do cpsTraceDoc $ \env -> text "Core.Cps.cpsDef: illegal duplicated definition: " <+> prettyExpr env expr
                                   failure "Core.Cps.cpsDef: internal failure"
              else withCurrentDef def $
                   do expr' <- cpsExpr' True (defExpr def)
                      return [def{ defExpr = expr' id }] -- at top level this should be ok since the type is total
                                   

type Trans a = TransX a a 
type TransX a b  = (a -> b) ->b

cpsExpr :: Expr -> Cps (TransX Expr Expr)
cpsExpr expr
  = cpsExpr' False expr

cpsExpr' :: Bool -> Expr -> Cps (TransX Expr Expr)
cpsExpr' topLevel expr 
  = case expr of
      -- open
      -- simplify open away if it is directly applied
        {-
      App (App (TypeApp (Var open _) [effFrom, effTo, _, _]) [f]) args
        | getName open == nameEffectOpen 
        -> cpsExpr (App f args)
      -}


      --  lift _open_ applications
      App eopen@(TypeApp (Var open _) [effFrom,effTo, _, _]) [f]
        | getName open == nameEffectOpen         
        -> do cpskFrom <- getCpsType effFrom
              cpskTo   <- getCpsType effTo
              if (cpskFrom /= NoCps || cpskTo == NoCps)
               -- simplify open away if already in cps form, or not in cps at all
               then do -- cpsTraceDoc $ \env -> text "open: ignore: " <+> tupled (niceTypes env [effFrom,effTo])
                       -- cpsExpr f                       
                       f' <- cpsExpr f
                       return $ \k -> f' (\ff -> k (App eopen [ff]))

               -- lift the function to a continuation function
               else do let Just((partps,_,restp)) = splitFunType (typeOf f)
                       pars <- mapM (\(name,partp) ->
                                     do pname <- if (name==nameNil) then uniqueName "x" else return name
                                        return (TName pname partp)) partps
                       let args = [Var tname InfoNone | tname <- pars]                       
                       -- The first argument should be 'True' if we need to ensure _k is defined;
                       -- see algeff/open1a for a good example. If the to effect is guaranteed to be 
                       -- in cps, we can leave out the check (which enables the simplifier to do a better job)
                       cpsLambda (cpskTo/=AlwaysCps) pars effTo (App f args) 

      -- leave 'return' in place
      App ret@(Var v _) [arg] | getName v == nameReturn
        -> do -- cpsTraceDoc $ \env -> text "found return: " <+> prettyExpr env expr
              -- cpsExpr arg
              arg' <- cpsExpr arg
              mbK  <- getCurrentK
              case mbK of
                Nothing -- no cps
                  -> return $ \k -> arg' (\xx -> k (App ret [xx]))
                Just exprK 
                  -> return $ \k -> arg' (\xx -> 
                          let cexpr = App ret [App exprK [xx]] in  -- ignore  k since nothing can happen after return!
                              --trace ("return after cps: " ++ show (prettyExpr defaultEnv cexpr)) $ 
                              cexpr)

      -- lift out lambda's into definitions so they can be duplicated if necessary                  
      TypeLam tpars (Lam pars eff body) | not topLevel 
        -> cpsExprAsDef tpars pars eff body
      Lam pars eff body | not topLevel
        -> cpsExprAsDef [] pars eff body

      -- regular cases
      Lam args eff body 
        -> do cpsk  <- getCpsEffect eff
              if (cpsk == NoCps)
               then withCurrentK Nothing $
                    do -- cpsTraceDoc $ \env -> text "not effectful lambda:" <+> niceType env eff
                       body' <- cpsExpr body
                       args' <- mapM cpsTName args                      
                       return $ \k -> k (Lam args' eff (body' id))
               else -- cps converted lambda: add continuation parameter
                    cpsLambda False args eff body

      App f args
        -> do f' <- cpsExpr f
              args' <- mapM cpsExpr args
              let -- ff  = f' id
                  ftp = typeOf f -- ff
                  Just(_,feff,_) = splitFunType ftp
              isCps <- needsCpsType ftp
              -- cpsTraceDoc $ \env -> text "app" <+> (if isNeverCps f then text "never-cps" else text "") <+> prettyExpr env f <+> text ",tp:" <+> niceType env (typeOf f)
              if ((not (isCps || isAlwaysCps f)) || isNeverCps f)
               then return $ \k -> 
                f' (\ff -> 
                  applies args' (\argss -> 
                    k (App ff argss)
                ))
               else  do -- cpsTraceDoc $ \env -> text "app cps:" <+> prettyExpr env expr
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
              nameX  <- uniqueName "x"
              nameF  <- uniqueName "f"
              let resTp = typeOf expr
                  tnameX = TName nameX resTp
                  typeF  = typeFun [(nameX,resTp)] effectEmpty typeVoid
                  tnameF = TName nameF typeF
                  klam k = Lam [tnameX] effectEmpty (k (Var tnameX InfoNone))
                  kdef k body = Let [DefNonRec (Def nameF typeF (klam k) Private DefFun rangeNull "")] body  
                  kapp x   = App (Var tnameF (InfoArity 0 1)) [x]
              return $ \k -> exprs' (\xxs -> kdef k (Case xxs (map (\b -> b kapp) bs')))
              -- return $ \k -> exprs' (\xxs -> Case xxs (map (\b -> b k) bs'))
              -- return $ \k -> exprs' (\xxs -> k (Case xxs (map (\b -> b id) bs')))              
      Var (TName name tp) info
        -> do -- tp' <- cpsTypeX tp
              return (\k -> k (Var (TName name tp) info)) 

      -- type application and abstraction

      TypeApp (TypeLam tvars body) tps  | length tvars == length tps
        -- propagate types so that the pure tvs are maintained.
        -- todo: This will fail if the typeapp is not directly around the type lambda!
        -> do cpsExpr' topLevel (subNew (zip tvars tps) |-> body)

      TypeLam tvars body
        -> do body' <- cpsExpr' topLevel body
              return $ \k -> body' (\xx -> k (TypeLam tvars xx))
      
      TypeApp body tps
        -> do body' <- cpsExpr' topLevel body
              -- tps'  <- mapM cpsTypeX tps
              -- tps'  <- mapM cpsTypePar tps0
              return $ \k -> body' (\xx -> k (TypeApp xx tps))
                   
      _ -> return (\k -> k expr) -- leave unchanged


cpsLambda ensure pars eff body 
  = do resTp <- freshTVar kindStar Meta
       unameK <- uniqueName "k"
       -- let resTp = typeAny
       let bodyTp = typeOf body
           nameK  = tnameK bodyTp eff resTp
           pnameK = if (ensure) then TName unameK (typeOf nameK) else nameK
           exprK  = varK bodyTp eff resTp
           appK x = App exprK [x]
                    {-
                    if (cpsk==AlwaysCps) 
                     then App exprK [x]
                     else varApplyK exprK x
                    -}
       withCurrentK (Just exprK) $
        withCpsTVars (freeEffectTVars eff) $ -- todo: is this correct?
         do body' <- cpsExpr body
            pars' <- mapM cpsTName pars 
            return $ \k -> 
              k (Lam (pars' ++ [pnameK]) eff 
                  ((if ensure then ensureK nameK pnameK else id)
                   (body' (\xx -> appK xx))))

cpsExprAsDef :: [TypeVar] -> [TName] -> Effect -> Expr -> Cps (Trans Expr)
cpsExprAsDef tpars pars eff body
  = do let expr = addTypeLambdas tpars (Lam pars eff body)
       cpsk <- getCpsEffect eff
       if (cpsk/=PolyCps)
         then cpsExpr' True expr 
         else do name <- uniqueName "lam"
                 let expr = addTypeLambdas tpars (Lam pars eff body)
                     tp   = typeOf expr
                     def  = Def name tp expr Private DefFun rangeNull ""
                     var  = Var (TName name tp) (InfoArity (length tpars) (length pars))
                 -- cpsTraceDoc $ \env -> text "cps as expr:" <--> prettyExpr env expr
                 cpsExpr (Let [DefNonRec def] var) -- process as let definition

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
cpsLetGroups dgs 
  = do dgss' <- cpsTrans cpsLetGroup dgs
       return $ \k -> dgss' (\dgss -> k (concat dgss))
  -- = cpsDefGroups dgs
  
cpsLetGroup :: DefGroup -> Cps (TransX [DefGroup] Expr)
cpsLetGroup dg
  = case dg of
      DefRec defs -> do ldefs <- cpsTrans (cpsLetDef True) defs
                        return $ \k -> ldefs (\xss -> k (concat [DefRec xds : DefRec yds : map DefNonRec nds | (xds,yds,nds) <- xss]))
      DefNonRec d -> do ldef <- cpsLetDef False d
                        return $ \k -> ldef (\(xds,yds,nds) -> k (map DefNonRec (xds ++ yds ++ nds)))

cpsLetDef :: Bool -> Def -> Cps (TransX ([Def],[Def],[Def]) Expr)
cpsLetDef recursive def
  = withCurrentDef def $
    do cpsk <- getCpsType (defType def)
       -- cpsTraceDoc $ \env -> text "analyze typex: " <+> ppType env (defType def) <.> text ", result: " <.> text (show (cpsk,defSort def)) -- <--> prettyExpr env (defExpr def)
       if ((cpsk == PolyCps {-|| cpsk == MixedCps-}) && isDupFunctionDef (defExpr def))
        then cpsLetDefDup cpsk recursive def 
        else do -- when (cpsk == PolyCps) $ cpsTraceDoc $ \env -> text "not a function definition but has cps type" <+> ppType env (defType def)
                expr' <- cpsExpr' True (defExpr def) -- don't increase depth
                return $ \k -> expr' (\xx -> k ([def{defExpr = xx}],[],[]))
                         -- \k -> k [def{ defExpr = expr' id}]

cpsLetDefDup :: CpsTypeKind -> Bool -> Def -> Cps (TransX ([Def],[Def],[Def]) Expr)
cpsLetDefDup cpsk recursive def
  = do let teffs = let (tvars,_,rho) = splitPredType (defType def)
                   in -- todo: we use all free effect type variables; that seems too much. 
                      -- we should use the ones that caused this to be cps-translated and
                      -- return those as part of CpsPoly or CpsAlways
                      freeEffectTVars rho

       -- cpsTraceDoc (\env -> text "cps translation") 
       exprCps'    <- withCpsTVars teffs $ cpsExpr' True (defExpr def)
       -- cpsTraceDoc (\env -> text "fast translation: free:" <+> tupled (niceTypes env (defType def:map TVar teffs)))
       exprNoCps'  <- withPureTVars teffs $ cpsExpr' True (defExpr def)

       return $ \k -> 
        exprCps' $ \exprCps -> 
        exprNoCps' $ \exprNoCps ->           
         let createDef name expr
              = let tname    = TName name (defType def)
                    (n,m)    = getArity (defType def)
                    var      = Var tname (InfoArity n m)
                    expr'    = if (recursive) 
                                 then [(defTName def, var)] |~> expr
                                 else expr
                in (def{ defName = name, defExpr = expr' }, var)
             nameCps  = makeHiddenName "cps" (defName def)
             nameNoCps= makeHiddenName "fast" (defName def)
             (defCps,varCps)   = createDef nameCps (exprCps)     
             (defNoCps,varNoCps) = createDef nameNoCps (exprNoCps)

             defPick  = def{ defExpr = exprPick }
             exprPick = case simplify (defExpr defCps) of -- assume (forall<as>(forall<bs> ..)<as>) has been simplified by the cps transform..
                          TypeLam tpars (Lam pars eff body) | length pars > 0 -> TypeLam tpars (Lam pars eff (bodyPick tpars pars))
                          Lam pars eff body                 | length pars > 0 -> Lam pars eff (bodyPick [] pars)
                          _ -> failure $ "Core.Cps.cpsLetDefDup: illegal cps transformed non-function?: " ++ show (prettyDef defaultEnv def) 

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

         in k ([defCps],[defNoCps],[defPick])

-- is  this a function definition that may need to be duplicated with a
-- plain and cps-translated definition?
isDupFunctionDef :: Expr -> Bool
isDupFunctionDef expr
  = case expr of
      TypeLam tpars (Lam pars eff body) | length pars > 0 -> True
      Lam pars eff body                 | length pars > 0 -> True
      TypeApp (TypeLam tvars body) tps  | length tvars == length tps
        -> isDupFunctionDef body
      TypeLam tpars (TypeApp body tps)  | length tpars == length tps 
         -> isDupFunctionDef body
      _ -> False





simplify :: Expr -> Expr
simplify expr
  = case expr of
      App (TypeApp (Var openName _) [eff1,eff2, _, _]) [arg]  
        | getName openName == nameEffectOpen && matchType eff1 eff2
        -> simplify arg
      TypeApp (TypeLam tvars body) tps  | length tvars == length tps
        -> simplify (subNew (zip tvars tps) |-> body)
      _ -> expr


cpsTrans :: (a -> Cps (TransX b c)) -> [a] -> Cps (TransX [b] c)
cpsTrans f xs
  = case xs of
      [] -> return $ \k -> k []
      (x:xx) -> do x'  <- f x
                   xx' <- cpsTrans f xx
                   return $ \k -> x' (\y -> xx' (\ys -> k (y:ys)))




applies :: [Trans a] -> ([a] -> a) -> a
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
{-

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
-}

varValidK :: Expr
varValidK 
  = let typeVar = TypeVar 1 kindStar Bound
    in  Var (TName nameIsValidK (TForall [typeVar] [] (TFun [(nameNil,TVar typeVar)] typeTotal typeBool)))
            (Core.InfoExternal [(JS,"(#1 !== undefined)")])

ensureK :: TName -> TName -> (Expr -> Expr)
ensureK tname@(TName name tp) namek body
  = let expr = App (Var (TName nameEnsureK (TFun [(nameNil,tp)] typeTotal tp)) 
                  (Core.InfoExternal [(JS,"(#1 || $std_core.id)")])) [Var namek InfoNone]
        def = Def name tp expr Private DefVal rangeNull ""
    in Let [DefNonRec def] body

varK tp effTp resTp    = Var (tnameK tp effTp resTp) InfoNone -- (InfoArity 0 1)
tnameK tp effTp resTp  = tnameKN "" tp effTp resTp
tnameKN post tp effTp resTp = TName (postpend post nameK) (typeK tp effTp resTp)
typeK tp effTp resTp   = TSyn (TypeSyn nameTpCont (kindFun kindStar (kindFun kindEffect (kindFun kindStar kindStar))) 0 Nothing) 
                          {- [tp,effTp,resTp]
                           (TFun [(nameNil,tp)] effTp resTp) -}
                           [tp, effTp, tp]
                           (TFun [(nameNil,tp)] effTp tp)
                          -- TFun [(nameNil,tp)] typeTotal typeYld
 
typeYld   = TCon (TypeCon (nameTpYld) (kindFun kindStar kindStar))

nameK = newHiddenName "k"
nameX = newHiddenName "x"

varApplyK k x
  = let (Just (_,effTp,resTp)) = splitFunType (typeOf k)
        tp = typeFun [(nameNil,typeOf k),(nameNil,typeOf x)] effTp resTp
        applyK = Var (TName nameApplyK tp)
                     (Core.InfoExternal [(JS,"(#1||$std_core.id)(#2)")]) -- InfoArity (3,2)
    in App applyK [k,x]


{--------------------------------------------------------------------------
  Check if expressions need to be cps translated
--------------------------------------------------------------------------}  

-- Some expressions always need cps translation
isAlwaysCps :: Expr -> Bool
isAlwaysCps expr
  = case expr of
      TypeApp e _ -> isAlwaysCps e
      Var v _     -> getName v == nameYieldOp || getName v == nameUnsafeTotal
      _ -> False

-- Some expressions never need cps translation
isNeverCps :: Expr -> Bool
isNeverCps expr
  = case expr of
      TypeApp e _ -> isNeverCps e
      Var v _     -> getName v == canonicalName 1 nameDeref 
      _ -> False

-- Does this definition need any cps translation (sometimes deeper inside)
needsCpsDef :: Def -> Cps Bool
needsCpsDef def
  = do t <- needsCpsType (defType def)
       if (t) then return True
        else needsCpsExpr (defExpr def)
       
needsCpsExpr :: Expr -> Cps Bool
needsCpsExpr expr
  = case expr of
      App (TypeApp (Var open _) [_, effTo, _, _]) [_] | getName open == nameEffectOpen
        -> needsCpsEffect effTo
      App f args 
        -> anyM needsCpsExpr (f:args)
      Lam pars eff body
        -> orM [needsCpsEffect eff, needsCpsExpr body]
      
      TypeApp (TypeLam tpars body) targs
        -> do b1 <- anyM needsCpsType targs
              if (b1 || any (isKindEffect . getKind) tpars)
               then return True
               else needsCpsExpr (subNew (zip tpars targs) |-> body)
      TypeApp (Var tname info) targs
        -> orM [anyM needsCpsType targs, needsCpsType (typeOf expr)]                    
      
      TypeApp body targs
        -> orM [anyM needsCpsType targs, needsCpsExpr body]
      TypeLam tpars body
        -> if (any (isKindEffect . getKind) tpars) then return True
            else withRemoveTVars tpars $ needsCpsExpr body
      Let defs body
        -> orM [anyM needsCpsDefGroup defs, needsCpsExpr body]
      Case exprs bs
        -> orM [anyM needsCpsExpr exprs, anyM needsCpsBranch bs]
      _ -> needsCpsType (typeOf expr) -- because instantiating polymorphic variables may need translation

needsCpsDefGroup defGroup
  = case defGroup of
      DefRec defs -> anyM needsCpsDef defs
      DefNonRec def -> needsCpsDef def

needsCpsBranch (Branch pat guards)
  = anyM needsCpsGuard  guards

needsCpsGuard (Guard g e)
  = anyM needsCpsExpr [g,e]

anyM :: (a -> Cps Bool) -> [a] -> Cps Bool
anyM f xs = orM (map f xs)

orM :: [Cps Bool] -> Cps Bool
orM xs 
  = case xs of
      [] -> return False
      (x:xx) -> do b <- x
                   if (b) then return True else orM xx

-- Is the type a function with a handled effect?
needsCpsType :: Type -> Cps Bool
needsCpsType tp
  = do cpsk <- getCpsType tp 
       return (cpsk /= NoCps)

needsCpsEffect :: Effect -> Cps Bool
needsCpsEffect eff
  = do cpsk <- getCpsEffect eff 
       return (cpsk /= NoCps)

data CpsTypeKind 
  = NoCps      -- no cps type
  | AlwaysCps  -- always cps translated
  | PolyCps    -- polymorphic in cps: needs fast and cps version
  deriving (Eq,Ord,Show)


-- Is the type a function with a handled effect?
getCpsType :: Type -> Cps CpsTypeKind
getCpsType tp
  | isKindEffect (getKind tp) = getCpsEffect tp
  | otherwise =
    case expandSyn tp of
      TForall vars preds t -> withRemoveTVars vars $ getCpsType t
      TFun pars eff res    -> getCpsEffect eff 
      _ -> return NoCps


getCpsEffect :: Effect -> Cps CpsTypeKind
getCpsEffect eff
  = let (ls,tl) = extractEffectExtend eff 
    in if (any (\l -> case getHandledEffect l of
                        Just ResumeMany -> True
                        _ -> False) ls)
        then return AlwaysCps 
        else getCpsTVar tl

getCpsTVar :: Type -> Cps CpsTypeKind
getCpsTVar tp
  = case expandSyn tp of
      TVar tv | isKindEffect (typevarKind tv) 
         -> do isPure <- isPureTVar tv
               isCps  <- isCpsTVar tv
               return $
                if (isPure) then NoCps
                else if (isCps) then AlwaysCps
                else PolyCps
      _  -> return NoCps


freeEffectTVars :: Type -> [TypeVar]
freeEffectTVars tp
  = filter (isKindEffect . getKind) (tvsList (ftv tp))

{--------------------------------------------------------------------------
  Cps monad
--------------------------------------------------------------------------}  
newtype Cps a = Cps (Env -> State -> Result a)

data Env = Env{ currentK:: Maybe Expr, currentDef :: [Def], 
                pureTVars :: Tvs, cpsTVars :: Tvs, 
                prettyEnv :: Pretty.Env }

data State = State{ uniq :: Int }

data Result a = Ok a State

runCps :: Monad m => Pretty.Env -> Int -> Cps a -> m a
runCps penv u (Cps c)
  = case c (Env Nothing [] tvsEmpty tvsEmpty penv) (State u) of
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
  = -- trace ("cps def: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env})

withCurrentK :: Maybe Expr -> Cps a -> Cps a
withCurrentK mbk cps
  = withEnv (\env -> env{ currentK = mbk } ) cps

getCurrentK :: Cps (Maybe Expr)
getCurrentK
  = do env <- getEnv
       return (currentK env)

withRemoveTVars :: [TypeVar] -> Cps a -> Cps a
withRemoveTVars vs cps
  = let tvs = tvsNew vs
    in withEnv (\env -> env{ pureTVars = tvsDiff (pureTVars env) tvs, cpsTVars =tvsDiff (cpsTVars env) tvs}) $
       do cps

withPureTVars :: [TypeVar] -> Cps a -> Cps a
withPureTVars vs cps
  = withEnv (\env -> env{ pureTVars = tvsUnion (tvsNew vs) (pureTVars env)}) $
    do -- env <- getEnv
       -- cpsTraceDoc $ \penv -> text "with pure tvars:" <+> tupled (niceTypes penv (map TVar (tvsList (pureTVars env))))
       cps

withCpsTVars :: [TypeVar] -> Cps a -> Cps a
withCpsTVars vs cps
  = withEnv (\env -> env{ cpsTVars = tvsUnion (tvsNew vs) (cpsTVars env)}) $
    do -- env <- getEnv
       -- cpsTraceDoc $ \penv -> text "with cps tvars:" <+> tupled (niceTypes penv (map TVar (tvsList (cpsTVars env))))
       cps

getPureTVars :: Cps Tvs
getPureTVars
  = do env <- getEnv
       return (pureTVars env)  

isPureTVar :: TypeVar -> Cps Bool
isPureTVar tv 
  = do env <- getEnv
       return (tvsMember tv (pureTVars env))


getCpsTVars :: Cps Tvs
getCpsTVars
  = do env <- getEnv
       return (cpsTVars env)  

isCpsTVar :: TypeVar -> Cps Bool
isCpsTVar tv 
  = do env <- getEnv
       return (tvsMember tv (cpsTVars env))


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
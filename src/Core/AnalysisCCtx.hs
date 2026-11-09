-----------------------------------------------------------------------------
-- Copyright 2012-2023, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

{-
    Check if a constructor context is well formed, and create a context path
-}

module Core.AnalysisCCtx( analyzeCCtx,
                          makeCCtxPrim,
                          makeCCtxEmpty,
                          makeCCtxHole,
                          -- makeCCtxCreate,
                          -- makeCCtxSetContextPath,
                          makeFieldAddrOf
                          -- getFieldName
                        ) where


import Control.Monad
import Lib.Trace
import Lib.PPrint
import Common.Syntax( Target(..), JsTarget(..), CTarget(..) )
import Common.Id
import Common.Name
import Common.NamePrim(nameCCtxHoleCreate,nameCCtxCreate,nameCCtxEmpty,nameCCtxSetCtxPath,
                       nameFieldAddrOf,nameTpFieldAddr,
                       nameEffectOpen)
import Common.Range
import Common.Unique(HasUnique(..))
import Common.Failure
import Common.Syntax
import Kind.Newtypes
import Kind.Kind
import Type.Type
import Type.Pretty as Pretty
import Type.TypeVar
import Type.Assumption( Gamma, gammaLookupQ, infoType )
import Core.Core
import Core.Pretty

-- take a context and check if it is well-formed and return a well-typed context expression
analyzeCCtx :: TargetPlatform -> Range -> Newtypes -> Gamma -> Expr -> (Int -> ((Expr,[(Range,Doc)]),Int))
analyzeCCtx tpl rng newtypes gamma expr uniq
  = let (res,uniq') = runCCtx tpl rng newtypes gamma uniq (cctxCreate expr)
    in case res of
         Right e   -> ((e,[]),uniq')
         Left errs -> let errs' = if null errs then [(rng,text "ill-formed context")]
                                               else errs
                      in ((makeCCtxEmpty gamma tpl (typeOf expr),errs'),uniq)


data Hole = Hole{ holeAddr :: Expr, holeType :: Type }
data Ctx  = Ctx{ defs :: [Def], top :: Expr, hole :: Hole }

cctxCreate :: Expr -> CCtx Expr
-- empty context?
cctxCreate expr | isHole expr
  = do tpl <- getTargetPlatform
       gamma <- getGamma
       return (makeCCtxEmpty gamma tpl (typeOf expr))
-- non-empty context
cctxCreate expr
  = do -- mtrace ("expr: " ++ show expr)
       (Ctx defs top (Hole addr holetp)) <- cctxExpr expr
       tpl <- getTargetPlatform
       gamma <- getGamma
       let tp = typeOf top
       let cctx = makeCCtxCreate gamma tpl tp holetp top addr
       return (Let (map DefNonRec defs) cctx)


cctxExpr :: Expr -> CCtx Ctx
cctxExpr expr
  = case expr of
      -- constructor
      App con@(Con name repr) args       | conReprHasCtxPath repr && not (null args)
        -> cctxCon name repr [] args

      App (TypeApp (con@(Con name repr)) targs) args  | conReprHasCtxPath repr && not (null args)
        -> cctxCon name repr targs args

      -- App (App (TypeApp (Var open _) [effFrom,effTo,tpFrom,tpTo]) [f]) []) | getName open == nameEffectOpen

      _ -> illegal

      {-
      Var _ _     -> illegal "var"
      Lam _ _ _   -> illegal "lambda"
      TypeLam _ e -> illegal ""
      Lit _       -> illegal
      Let dgs e   ->
      Case _ _    ->
      App _ _     ->
      -}

-- todo: check dataRepr for non-value constructor
cctxCon :: TName -> ConRepr -> [Type] -> [Expr] -> CCtx Ctx
cctxCon conName conRepr targs args
  = case span (not . isHole) args of
      (pre,hole:post)
        -> cctxConFinal conName conRepr targs pre hole post
      _ -> cctxConRecurse conName conRepr targs args

cctxConRecurse :: TName -> ConRepr -> [Type] -> [Expr] -> CCtx Ctx
cctxConRecurse conName conRepr targs args
  =  do -- mtrace "recurse"
        (pre,ctx,post) <- cctxFind [] [] args
        mapM_ cctxCheckNoHole (pre ++ post)
        (ds,vars) <- unzip <$> mapM makeUniqueDef pre
        fname <- getFieldName conName (length pre + 1)
        let ctxrepr = conRepr{ conCtxPath = CtxField fname }
        (d1,var1) <- makeUniqueDef (App (makeTypeApp (Con conName ctxrepr) targs) (vars ++ [top ctx] ++ post))
        -- (d2,var2) <- makeUniqueDef (makeCCtxSetContextPath var1 conName fname)
        return (ctx{ defs = ds ++ defs ctx ++ [d1], top = var1 })

cctxConFinal :: TName -> ConRepr -> [Type] -> [Expr] -> Expr -> [Expr] -> CCtx Ctx
cctxConFinal conName conRepr targs pre hole post
  =  do -- mtrace "final"
        mapM_ cctxCheckNoHole (pre ++ post)
        fname <- getFieldName conName (length pre + 1)
        let holetp = typeOf hole
            ctxrepr = conRepr{ conCtxPath = CtxField fname }
        ensureValidHoleType holetp
        gamma <- getGamma
        (d1,var1) <- makeUniqueDef (App (makeTypeApp (Con conName ctxrepr) targs) (pre ++ [hole] ++ post))
        (d2,addr) <- makeUniqueDef (makeFieldAddrOf gamma var1 conName (getName fname) holetp)
        -- (d3,var3) <- makeUniqueDef (makeCCtxSetContextPath var1 conName fname) -- should be last as it consumes var1
        return (Ctx [d1,d2] var1 (Hole addr holetp))

cctxCheckNoHole :: Expr -> CCtx ()
cctxCheckNoHole expr
  = -- note: not needed as it as already checked during type inference
    return ()


cctxFind :: [(Range,Doc)] -> [Expr] -> [Expr] -> CCtx ([Expr],Ctx,[Expr])
-- no args
cctxFind errs acc []
  = emitErrors errs
-- try recursively
cctxFind errs acc (arg:args)
  = do r <- try (cctxExpr arg)
       case r of
         Left errs' -> cctxFind (errs ++ errs') (arg:acc) args
         Right ctx -> return (reverse acc,ctx,args)


illegal
  = emitErrors []

makeUniqueDef :: Expr -> CCtx (Def,Expr)
makeUniqueDef expr
  = do name <- uniqueName "cctx"
       return (makeDef name expr, Var (TName name (typeOf expr)) InfoNone)

isHole :: Expr -> Bool
isHole (App (TypeApp (Var (TName hname htp) _) [tp,_etp]) []) = (hname == nameCCtxHoleCreate)
isHole (App (App (TypeApp (Var open _) [effFrom,effTo,tpFrom,tpTo]) [TypeApp (Var hname _) _]) [])
  = (getName open == nameEffectOpen) && (getName hname == nameCCtxHoleCreate)
isHole _ = False

-- A primitive of `std/core/types` applied to type and value arguments. The type scheme is
-- taken from its declaration (so `--checkcore` checks the use against the declaration).
makeCCtxPrim :: Gamma -> Name -> String -> [Type] -> [Expr] -> Expr
makeCCtxPrim gamma name external targs args
  = assertion ("Core.AnalysisCCtx.makeCCtxPrim: wrong number of arguments for " ++ show name ++ ": " ++ show (pretty scheme)) arityOk $
    App (makeTypeApp (Var (TName name scheme) (InfoExternal external)) targs) args
  where
    scheme = case gammaLookupQ name gamma of
               [info] -> infoType info
               infos  -> failure ("Core.AnalysisCCtx.makeCCtxPrim: cannot find " ++ show name)
    arityOk
      = case splitFunScheme scheme of
          Just (tvars,pars,_,_) -> length tvars == length targs && length pars == length args
          Nothing -> False

-- Polymorphic hole
makeCCtxHole :: Gamma -> Type -> Expr
makeCCtxHole gamma tp
  = makeCCtxPrim gamma nameCCtxHoleCreate "" [tp] []

-- Initial empty context (ctx hole)
makeCCtxEmpty :: Gamma -> TargetPlatform -> Type -> Expr
makeCCtxEmpty gamma tpl tp
  = makeCCtxPrim gamma nameCCtxEmpty external [tp] []
  where
    external = case tplTarget tpl of
                 C _  -> "kk_cctx_empty(kk_context())"
                 JS _ -> "$std_core_types._cctx_empty()"
                 _    -> unsupportedExternal "kk_cctx_empty"

-- Create a context (ctx Cons(e,Cons(2,hole)))
makeCCtxCreate :: Gamma -> TargetPlatform -> Type -> Type -> Expr -> Expr -> Expr
makeCCtxCreate gamma tpl tp holetp top holeaddr
  = makeCCtxPrim gamma nameCCtxCreate external [tp,holetp] [top,holeaddr]
  where
    external = case tplTarget tpl of
                 C _  -> "kk_cctx_create(#1,#2,kk_context())"
                 JS _ -> "$std_core_types._cctx_create(#1,#2)"
                 _    -> unsupportedExternal "kk_cctx_create"


-- The adress of a field in a constructor (for context holes)
makeFieldAddrOf :: Gamma -> Expr -> TName -> Name -> Type -> Expr
makeFieldAddrOf gamma obj conName fieldName fieldTp
  = makeCCtxPrim gamma nameFieldAddrOf "" [typeOf obj,fieldTp]
        [obj, Lit (LitString (showTupled (getName conName))), Lit (LitString (showTupled fieldName))]

-- Set the index of the field in a constructor to follow the path to the hole at runtime.
makeCCtxSetContextPath :: Expr -> TName -> Name -> Expr
makeCCtxSetContextPath obj conName fieldName
  = App (Var (TName nameCCtxSetCtxPath funType) (InfoExternal "@cctx-setcp(#1,#2,#3)"))
        [obj, Lit (LitString (showTupled (getName conName))), Lit (LitString (showTupled fieldName))]
  where
    tp = typeOf obj
    funType = (TFun [(nameNil,tp),(nameNil,typeString),(nameNil,typeString)] typeTotal tp)


{--------------------------------------------------------------------------
  CC Monad
--------------------------------------------------------------------------}

newtype CCtx a = CCtx (Int -> CCtxEnv -> Result a)

runCCtx :: TargetPlatform -> Range -> Newtypes -> Gamma -> Int -> CCtx a -> (Either [(Range,Doc)] a,Int)
runCCtx tpl rng nt gamma uniq (CCtx c)
  = case (c uniq (CCtxEnv rng nt gamma tpl)) of
      Ok x u'  -> (Right x,u')
      Err errs -> (Left errs,uniq)



data CCtxEnv = CCtxEnv{ rng :: !Range, newtypes :: !Newtypes, gamma :: !Gamma, tplatform :: !TargetPlatform }

data Result a = Err [(Range,Doc)]
              | Ok a Int

instance Functor CCtx where
  fmap f (CCtx c)  = CCtx (\u env -> case c u env of
                                       Ok x u' -> Ok (f x) u'
                                       Err errs -> Err errs)

instance Applicative CCtx where
  pure x = CCtx (\u g -> Ok x u)
  (<*>) = ap

instance Monad CCtx where
  -- return = pure
  (CCtx c) >>= f  = CCtx (\u g -> case c u g of
                                      Ok x u' -> case f x of
                                                   CCtx d -> d u' g
                                      Err errs -> Err errs)

instance HasUnique CCtx where
  updateUnique f = CCtx (\u g -> Ok u (f u))
  setUnique  i   = CCtx (\u g -> Ok () i)

getEnv :: CCtx CCtxEnv
getEnv
  = CCtx (\u g -> Ok g u)

withEnv :: CCtxEnv -> CCtx a -> CCtx a
withEnv env (CCtx c)
  = CCtx (\u _ -> c u env)

updateEnv :: (CCtxEnv -> CCtxEnv) -> CCtx a -> CCtx a
updateEnv f (CCtx c)
  = CCtx (\u env -> c u (f env))

emitError :: Doc -> CCtx a
emitError doc
  = do env <- getEnv
       emitErrors [(rng env,doc)]

emitErrors :: [(Range,Doc)] -> CCtx a
emitErrors errs
  = do -- mtrace ("emit errors: " ++ show errs)
       (CCtx (\u env -> Err errs))

getTargetPlatform :: CCtx TargetPlatform
getTargetPlatform
  = tplatform <$> getEnv

getGamma :: CCtx Gamma
getGamma
  = gamma <$> getEnv
      
try :: CCtx a -> CCtx (Either [(Range,Doc)] a)
try (CCtx c)
  = CCtx (\u env -> case c u env of
                      Ok x u' -> Ok (Right x) u'
                      Err errs -> Ok (Left errs) u)


mtrace :: String -> CCtx ()
mtrace msg
  = do env <- getEnv
       trace ("Core.AnalysisCCtx: " ++ msg) $
         return ()

getFieldName :: TName -> Int -> CCtx TName
getFieldName cname fieldIdx
  = do info <- lookupFieldName cname fieldIdx
       case info of
         Left err -> failure ("Core.AnalysisCCtx: " ++ err)
         Right name -> return name

ensureValidHoleType :: Type -> CCtx ()
ensureValidHoleType tp
  = do env <- getEnv
       case dataTypeNameOf tp of
         Left (TVar{})  -> emitError (text "the hole in the constructor context has an unresolved or polymorphic type")
         Left _         -> emitError (text "the hole in the constructor context has an invalid data type")
         Right name -> case newtypesLookupAny name (newtypes env) of
                        Just dataInfo ->
                          do let (dataRepr,_) = getDataRepr dataInfo
                             when (dataDefIsValue (dataInfoDef dataInfo) || dataReprIsValue dataRepr) $
                               emitError (text "the hole in a constructor context cannot be a value type")
                             return ()

dataTypeNameOf :: Type -> Either Type Name
dataTypeNameOf tp = case expandSyn tp of
                      TApp t ts -> dataTypeNameOf t
                      TCon tc   -> Right (typeConName tc)
                      t         -> Left t


lookupFieldName :: TName -> Int -> CCtx (Either String TName)
lookupFieldName cname field
  = do env <- getEnv
       case newtypesLookupAny (getDataTypeName cname) (newtypes env) of
         Just dataInfo ->
           do let (dataRepr,_) = getDataRepr dataInfo
              if (dataReprIsValue dataRepr)
                then return (Left ("contexts cannot go through a value type (" ++ show (getName cname) ++ ")"))
                else do case filter (\con -> conInfoName con == getName cname) (dataInfoConstrs dataInfo) of
                          [con] -> case drop (field - 1) (conInfoParams con) of
                                      ((fname,ftp):_) -> return $ Right (TName fname ftp) {- Con cname (getConRepr dataInfo con), fname) -}
                                      _ -> failure $ "Core.CTail.getFieldName: field index is off: " ++ show cname ++ ", field " ++ show  field ++ ", in " ++ show (conInfoParams con)
                          _ -> failure $ "Core.CTail.getFieldName: cannot find constructor: " ++ show cname ++ ", field " ++ show  field ++ ", in " ++ show (dataInfoConstrs dataInfo)
         _ -> failure $ "Core.CTail.getFieldName: no such constructor: " ++ show cname ++ ", field " ++ show  field
  where
    getDataTypeName cname  = case splitFunScheme (typeOf cname) of
                               Just (_,_,_,tres) -> getDataTypeNameRes tres
                               Nothing             -> failure $ "Core.CTail.getFieldName: illegal constructor type: " ++ show cname ++ ", field " ++ show  field ++ ": " ++ show (pretty (typeOf cname))
    getDataTypeNameRes tp  = case dataTypeNameOf tp of
                               Right name -> name
                               _          -> failure $ "Core.CTail.getFieldName: illegal result type: " ++ show cname ++ ", field " ++ show  field ++ ": " ++ show (pretty (typeOf cname))

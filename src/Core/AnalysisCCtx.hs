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
                          
                          makeCCtxEmpty,
                          makeCCtxCreate,
                          makeCCtxSetContextPath,
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
import Core.Core
import Core.Pretty

-- take a context and check if it is well-formed and return a well-typed context expression
analyzeCCtx :: Range -> Newtypes -> Expr -> (Int -> ((Expr,[(Range,Doc)]),Int))
analyzeCCtx rng newtypes expr uniq
  = let (res,uniq') = runCCtx rng newtypes uniq (cctxCreate expr)
    in case res of
         Right e   -> ((e,[]),uniq')
         Left errs -> let errs' = if null errs then [(rng,text "ill-formed context")]
                                               else errs
                      in ((makeCCtxEmpty (typeOf expr),errs'),uniq)


data Hole = Hole{ holeAddr :: Expr, holeType :: Type }
data Ctx  = Ctx{ defs :: [Def], top :: Expr, hole :: Hole }

cctxCreate :: Expr -> CCtx Expr
-- empty context?
cctxCreate expr | isHole expr
  = do return (makeCCtxEmpty (typeOf expr))
-- non-empty context
cctxCreate expr
  = do -- mtrace ("expr: " ++ show expr)
       (Ctx defs top (Hole addr holetp)) <- cctxExpr expr  
       let tp = typeOf top
       let cctx = makeCCtxCreate tp holetp top addr
       return (Let (map DefNonRec defs) cctx)


cctxExpr :: Expr -> CCtx Ctx
cctxExpr expr
  = case expr of
      -- constructor
      App con@(Con name repr) args       | not (null args)
        -> cctxCon name repr [] args

      App (TypeApp (con@(Con name repr)) targs) args  | not (null args)
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
        (d1,var1) <- makeUniqueDef (App (makeTypeApp (Con conName conRepr) targs) (vars ++ [top ctx] ++ post))
        fname <- getFieldName conName (length pre + 1)
        (d2,var2) <- makeUniqueDef (makeCCtxSetContextPath var1 conName fname)
        return (ctx{ defs = ds ++ defs ctx ++ [d1,d2], top = var2 })

cctxConFinal :: TName -> ConRepr -> [Type] -> [Expr] -> Expr -> [Expr] -> CCtx Ctx
cctxConFinal conName conRepr targs pre hole post
  =  do -- mtrace "final"
        mapM_ cctxCheckNoHole (pre ++ post)
        fname <- getFieldName conName (length pre + 1)
        let holetp = typeOf hole
        ensureValidHoleType holetp
        (d1,var1) <- makeUniqueDef (App (makeTypeApp (Con conName conRepr) targs) (pre ++ [hole] ++ post))
        (d2,addr) <- makeUniqueDef (makeFieldAddrOf var1 conName fname holetp)
        (d3,var3) <- makeUniqueDef (makeCCtxSetContextPath var1 conName fname) -- should be last as it consumes var1
        return (Ctx [d1,d2,d3] var3 (Hole addr holetp))

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

-- Initial empty context (ctx hole)
makeCCtxEmpty :: Type -> Expr
makeCCtxEmpty tp
  = App (TypeApp (Var (TName nameCCtxEmpty funType) 
                        -- (InfoArity 1 0)
                        (InfoExternal [(C CDefault,"kk_cctx_empty(kk_context())"),(JS JsDefault,"$std_core_types._cctx_empty()")])
                      ) [tp]) []
  where
    funType = TForall [a] [] (TFun [] typeTotal (typeCCtx (TVar a)))
    a = TypeVar 0 kindStar Bound


-- Create a context (ctx Cons(e,Cons(2,hole)))
makeCCtxCreate :: Type -> Type -> Expr -> Expr -> Expr
makeCCtxCreate tp holetp top holeaddr
  = App (TypeApp (Var (TName nameCCtxCreate funType) 
                -- (InfoArity 1 3) 
                (InfoExternal [(C CDefault,"kk_cctx_create(#1,#2,kk_context())"),
                               (JS JsDefault,"$std_core_types._cctx_create(#1,#2)")])
         ) [tp,holetp]) [top,holeaddr]
  where
    funType = TForall [a,b] [] (TFun [(nameNil,TVar a),
                                      (nameNil,TApp typeFieldAddr [TVar a])]
                                      typeTotal (TApp typeCCtxx [TVar a,TVar b]))
    a = TypeVar 0 kindStar Bound
    b = TypeVar 1 kindStar Bound


-- The adress of a field in a constructor (for context holes)
makeFieldAddrOf :: Expr -> TName -> Name -> Type -> Expr
makeFieldAddrOf obj conName fieldName fieldTp
  = App (TypeApp (Var (TName nameFieldAddrOf funType) (InfoExternal [])) [fieldTp])
        [obj, Lit (LitString (showTupled (getName conName))), Lit (LitString (showTupled fieldName))]
  where
    funType = TForall [a] [] (TFun [(nameNil,TVar a),(nameNil,typeString),(nameNil,typeString)]
                                   typeTotal (TApp typeFieldAddr [TVar a]))
    a = TypeVar 0 kindStar Bound

-- Set the index of the field in a constructor to follow the path to the hole at runtime.
makeCCtxSetContextPath :: Expr -> TName -> Name -> Expr
makeCCtxSetContextPath obj conName fieldName
  = App (Var (TName nameCCtxSetCtxPath funType) (InfoExternal [(Default,".cctx-setcp(#1,#2,#3)")]))
        [obj, Lit (LitString (showTupled (getName conName))), Lit (LitString (showTupled fieldName))]
  where
    tp = typeOf obj
    funType = (TFun [(nameNil,tp),(nameNil,typeString),(nameNil,typeString)] typeTotal tp)


{--------------------------------------------------------------------------
  CC Monad
--------------------------------------------------------------------------}

newtype CCtx a = CCtx (Int -> CCtxEnv -> Result a)

runCCtx :: Range -> Newtypes -> Int -> CCtx a -> (Either [(Range,Doc)] a,Int)
runCCtx rng nt uniq (CCtx c)
  = case (c uniq (CCtxEnv rng nt)) of
      Ok x u'  -> (Right x,u')
      Err errs -> (Left errs,uniq)



data CCtxEnv = CCtxEnv{ rng :: Range, newtypes :: Newtypes }

data Result a = Err [(Range,Doc)]
              | Ok a Int

instance Functor CCtx where
  fmap f (CCtx c)  = CCtx (\u env -> case c u env of 
                                       Ok x u' -> Ok (f x) u'
                                       Err errs -> Err errs)

instance Applicative CCtx where
  pure  = return
  (<*>) = ap

instance Monad CCtx where
  return x      = CCtx (\u g -> Ok x u)
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

getFieldName :: TName -> Int -> CCtx Name
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


lookupFieldName :: TName -> Int -> CCtx (Either String Name)
lookupFieldName cname field
  = do env <- getEnv
       case newtypesLookupAny (getDataTypeName cname) (newtypes env) of
         Just dataInfo -> 
           do let (dataRepr,_) = getDataRepr dataInfo
              if (dataReprIsValue dataRepr)
                then return (Left ("contexts cannot go through a value type (" ++ show (getName cname) ++ ")"))
                else do case filter (\con -> conInfoName con == getName cname) (dataInfoConstrs dataInfo) of
                          [con] -> case drop (field - 1) (conInfoParams con) of
                                      ((fname,ftp):_) -> return $ Right fname {- Con cname (getConRepr dataInfo con), fname) -}
                                      _ -> failure $ "Core.CTail.getFieldName: field index is off: " ++ show cname ++ ", field " ++ show  field ++ ", in " ++ show (conInfoParams con)
                          _ -> failure $ "Core.CTail.getFieldName: cannot find constructor: " ++ show cname ++ ", field " ++ show  field ++ ", in " ++ show (dataInfoConstrs dataInfo)
         _ -> failure $ "Core.CTail.getFieldName: no such constructor: " ++ show cname ++ ", field " ++ show  field
  where
    getDataTypeName cname  = case splitFunScheme (typeOf cname) of
                               Just (_,_,_,_,tres) -> getDataTypeNameRes tres
                               Nothing             -> failure $ "Core.CTail.getFieldName: illegal constructor type: " ++ show cname ++ ", field " ++ show  field ++ ": " ++ show (pretty (typeOf cname))
    getDataTypeNameRes tp  = case dataTypeNameOf tp of
                               Right name -> name
                               _          -> failure $ "Core.CTail.getFieldName: illegal result type: " ++ show cname ++ ", field " ++ show  field ++ ": " ++ show (pretty (typeOf cname))

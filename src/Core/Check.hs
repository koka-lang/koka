-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-    Typechecker for Core-F
-}
-----------------------------------------------------------------------------

module Core.Check (checkCore) where

import Control.Monad
import qualified Control.Monad.Fail as F
import Control.Applicative
import Lib.Trace
import Lib.PPrint
import Common.Failure
import Common.NamePrim( nameYieldOp )
import Common.Name
import Common.Unique
import Common.Error
import Common.Range
import Common.Syntax( DefSort(..) )
import Common.Unique

import Core.Core hiding (check)
import qualified Core.Core as Core
import qualified Core.Pretty as PrettyCore
import Core.CoreVar
-- import Core.Monadic( monType )

import Kind.Kind
import Type.Type
import Type.TypeVar
import Type.Assumption
import Type.Pretty
import Type.Kind
import Type.TypeVar
import Type.Unify( unify, runUnify )
import qualified Type.Operations as Op ( instantiateNoEx )

import qualified Data.Set as S

checkCore :: Bool -> Bool -> Env -> Gamma -> CorePhase b ()
checkCore liberalEffects allowPartialApps prettyEnv gamma
  = do uniq      <- unique
       defGroups <- getCoreDefs
       case checkDefGroups defGroups (return ()) of
          Check c -> case c uniq (CEnv liberalEffects allowPartialApps gamma prettyEnv []) of
                      Ok x _  -> return x
                      Err doc -> liftError (warningMsg (rangeNull, doc))




{--------------------------------------------------------------------------
  Checking monad
--------------------------------------------------------------------------}
newtype Check a = Check (Int -> CheckEnv -> Result a)

data CheckEnv = CEnv{ liberalEff :: Bool, allowPartialApps :: Bool, gamma :: Gamma, prettyEnv :: Env, currentDef :: [Def] }

data Result a = Ok a Int
              | Err Doc

instance Functor Check where
  fmap f (Check c)  = Check (\u env -> case c u env of Ok x u' -> Ok (f x) u'
                                                       Err doc -> Err doc)

instance Applicative Check where
  pure x = Check (\u g -> Ok x u)
  (<*>)  = ap

instance Monad Check where
  -- return = pure
  (Check c) >>= f  = Check (\u g -> case c u g of
                                      Ok x u' -> case f x of
                                                   Check d -> d u' g
                                      Err doc -> Err doc)

instance F.MonadFail Check where
  fail s        = Check (\u g -> Err (text (show (defName (head (currentDef g)))) <.> colon <+> text s))

instance HasUnique Check where
  updateUnique f = Check (\u g -> Ok u (f u))
  setUnique  i   = Check (\u g -> Ok () i)

extendGamma :: [(Name,NameInfo)] -> Check a -> Check a
extendGamma ex (Check c) = Check (\u env -> c u env{ gamma = (gammaExtends ex (gamma env)) })

withDef :: Def -> Check a -> Check a
withDef def (Check c)
  = -- trace ("checking: " ++ show (defName def)) $
    Check (\u env -> c u env{ currentDef = def : (currentDef env) })

getEnv :: Check CheckEnv
getEnv
  = Check (\u env -> Ok env u)

getGamma :: Check Gamma
getGamma
  = do env <- getEnv
       return (gamma env)

lookupVar :: Name -> Check Scheme
lookupVar name
  = do gamma <- getGamma
       case gammaLookupQ name gamma of
         [info] -> return (infoType info)
         []     -> fail ("unknown variable: " ++ show name)
         _      -> fail ("cannot pick overloaded variable: " ++ show name)

failDoc :: (Env -> Doc) -> Check a
failDoc fdoc
  = Check (\u env -> Err (fdoc (prettyEnv env) <-> ppDefs (prettyEnv env) (currentDef env)))

ppDefs :: Env -> [Def] -> Doc
ppDefs env []
  = Lib.PPrint.empty
ppDefs env defs
  = text "in definition:" <+> tupled (map (text.show.defName) defs)
    <-> prettyDef (head defs) env

checkTName :: TName -> Check TName
checkTName (TName name tp)
  = do tp' <- checkType tp
       return (TName name tp')


checkType :: Type -> Check Type
checkType tp
  = return tp

{--------------------------------------------------------------------------
  Definition groups

  To check a recursive definition group, we assume the type for each
  definition is correct, add all of those to the environment, then check
  each definition
--------------------------------------------------------------------------}

checkDefGroups :: DefGroups -> Check a -> Check a
checkDefGroups [] body
  = body
checkDefGroups (dgroup:dgroups) body
  = checkDefGroup dgroup (checkDefGroups dgroups body)

checkDefGroup :: DefGroup -> Check a -> Check a
checkDefGroup defGroup body
  = let defs = case defGroup of
                DefRec defs   -> defs
                DefNonRec def -> [def]
        env  = map coreDefInfo defs
    in extendGamma env $
       do mapM_ checkDef defs
          body

checkDef :: Def -> Check ()
checkDef d
  = withDef d $
    do tp <- check (defExpr d)
       dtp <- checkType (defType d)
       -- trace ("deftype: " ++ show (defType d) ++ "\ninferred: " ++ show tp) $ return ()
       matchSub "checking annotation on definition" (prettyDef d) (dtp) tp

coreNameInfo :: TName -> (Name,NameInfo)
coreNameInfo tname = coreNameInfoX 
  where
    coreNameInfoX 
      = (getName tname, createNameInfoX Public (getName tname) DefVal rangeNull (typeOf tname))

{--------------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------------}

check :: Expr -> Check Type
check expr
  = case expr of
      Lam pars eff body
        -> do tpRes <- extendGamma (map coreNameInfo pars) (check body)
              pars' <- mapM checkTName pars
              return (typeFun [(name,tp) | TName name tp <- pars'] eff tpRes)
      Var tname info
        -> checkType $ typeOf tname
      Con tname info
        -> return $ typeOf tname
      App fun args
        -> do tpFun <- check fun
              tpArgs <- mapM check args
              case splitFunType tpFun of
                Nothing -> fail "expecting function type in application"
                Just (tpPars,eff,tpRes)
                  -> do -- env <- getEnv
                        --when (length tpPars /= length args + n) $
                        --  failDoc (\env -> text "wrong number of arguments in application: " <+> prettyExpr expr env)
                        env <- getEnv
                        if (allowPartialApps env)
                         then do sequence_ [matchSub "comparing formal and actual argument" (prettyExpr expr) formal actual | ((argname,formal),actual) <- zip tpPars tpArgs]
                                 let morePars = drop (length tpArgs) tpPars
                                 if (null morePars)
                                  then return tpRes
                                  else return (TFun morePars eff tpRes)
                         else do sequence_ [matchSub "comparing formal and actual argument" (prettyExpr expr) formal actual | ((argname,formal),actual) <- zip tpPars tpArgs]
                                 return tpRes
      TypeLam tvars body
        -> do tp <- check body
              return (quantifyType tvars tp)
      TypeApp e tps
        -> do tpTForall <- check e
              let (tvars,_,tp) = splitPredType tpTForall
              -- We can use actual equality for kinds, because any kind variables will have been
              -- substituted when doing kind application (above)
              -- when (length tps /= length tvars || or [getKind t /= getKind tp | (t,tp) <- zip tvars tps]) $
              when (length tps > length tvars || or [getKind t /= getKind tp | (t,tp) <- zip tvars tps]) $
                failDoc (\env -> let penv = env{coreShowTypes=True,showKinds=True}
                                 in text "kind error in type application:" <+> prettyExpr expr penv </> text " applied to: " <+> ppType penv tpTForall)
              return (tForall (drop (length tps) tvars) [] (subNew (zip tvars tps) |-> tp))
      Lit lit
        -> return (typeOf lit)

      Let defGroups body
        -> checkDefGroups defGroups (check body)

      Case exprs branches
        -> do tpScrutinees <- mapM check exprs
              tpBranchess   <- mapM (checkBranch tpScrutinees) branches
              mapConseqM (match "verifying that all branches have the same type" (prettyExpr expr)) (concat tpBranchess)
              return (head (head tpBranchess))

mapConseqM f (tp1:tp2:tps)
  = do f tp1 tp2
       mapConseqM f (tp2:tps)
mapConseqM f _
  = return ()

{--------------------------------------------------------------------------
  Type of a branch
--------------------------------------------------------------------------}

checkBranch :: [Type] -> Branch -> Check [Type]
checkBranch tpScrutinees b@(Branch patterns guard)
  = do mapM_ checkPattern (zip tpScrutinees patterns)
       let vars = [coreNameInfo tname | tname <- S.toList (bv patterns)]
       extendGamma vars (mapM checkGuard guard)


checkGuard (Guard guard expr)
  = do gtp <- check guard
       match "verify that the guard is a boolean expression" (prettyExpr guard) gtp typeBool
       check expr

checkPattern :: (Type,Pattern) -> Check ()
checkPattern (tpScrutinee,pat)
  = case pat of
      PatCon tname args _ tpargs exists resTp coninfo skip
        -> -- (if (null exists) then id else (trace ("check existential pattern: " ++ show exists ++ ", " ++ show tpScrutinee))) $
           do -- constrArgs <- findConstrArgs (prettyPattern pat) tpScrutinee (getName tname)
              mapM_  checkPattern  (zip tpargs args)
      PatVar tname _ -> match "comparing constructor argument to case annotation" (prettyPattern pat) tpScrutinee (typeOf tname)
      PatLit lit     -> match "comparing literal pattern to scrutinee" (prettyPattern pat) tpScrutinee (typeOf lit)
      PatWild        -> return ()


{--------------------------------------------------------------------------
  Util
--------------------------------------------------------------------------}

-- Find the types of the arguments of a constructor,
-- given the type of the result of the constructor
findConstrArgs :: (Env -> Doc) -> Type -> Name -> Check [Type]
findConstrArgs fdoc tpScrutinee con
  = do tpCon <- lookupVar con
       -- Until we add qualifiers to constructor types, the list of predicates
       -- returned by instantiate' must always be empty
       (_,_,tpConInst,_) <- Op.instantiateNoEx rangeNull tpCon
       let Just (tpArgs, eff, tpRes) = splitFunType tpConInst
       ures <- runUnify (unify tpRes tpScrutinee)
       case ures of
        (Left error, _)  -> showCheck "comparing scrutinee with branch type" "cannot unify" tpRes tpScrutinee fdoc
        (Right _, subst) -> return $ (subst |-> map snd tpArgs)

matchSub :: String -> (Env -> Doc) -> Type -> Type -> Check ()
matchSub when fdoc a b
  = do env <- getEnv
       if (not (liberalEff env))
        then match when fdoc a b
        else -- check if b can be used as an a
              case (splitFunType a, splitFunType b) of
                (Just (targs1,teff1,tres1), Just (targs2,teff2,tres2)) | length targs1 == length targs2
                  -> do match when fdoc tres1 tres2
                        sequence_ [match when fdoc targ1 targ2 | ((_,targ1),(_,targ2)) <- zip targs1 targs2]
                        matchSubEff when fdoc teff1 teff2
                _ -> match when fdoc a b

matchSubEff :: String -> (Env -> Doc) -> Type -> Type -> Check ()
matchSubEff when fdoc eff1 eff2
  = let (ls1,tl1) = extractHandledEffect eff1
        (ls2,tl2) = extractHandledEffect eff2
    in if (length ls1 /= length ls2)
        then showCheck "handled effects do not match" when eff1 eff2 fdoc
        else do sequence_ [match when fdoc targ1 targ2 | (targ1,targ2) <- zip ls1 ls2]
                return ()


-- In Core, when we are comparing types, we are interested in exact
-- matches only.
match :: String -> (Env -> Doc) -> Type -> Type -> Check ()
match when fdoc a b
  = do ures <- runUnify (unify a b)
       case ures of
         (Left error, _)  -> showCheck ("cannot unify (" ++ show error ++ ")") when a b fdoc
         (Right _, subst) -> if subIsNull subst
                              then return ()
                              else do showCheck "non-empty substitution" when a b (\env -> text "")
                                      return ()

-- Print unification error
showCheck :: String -> String -> Type -> Type -> (Env -> Doc) -> Check a
showCheck err when a b fdoc
  = failDoc (showMessage err when a b fdoc)

showMessage err when a b fdoc env
  = let [docA,docB] = niceTypes env [a,b]
    in align $ vcat [ text err
                     , text "     " <.> docA
                     , text "  =~ " <.> docB
                     , text "when" <+> text when
                     , indent 2 (fdoc env)
                     -- , text $ (show a) ++ "\n" ++ (show b)
                     ]

prettyExpr e env = PrettyCore.prettyExpr env e
prettyPattern e env = snd (PrettyCore.prettyPattern env e)
prettyDef d env     = PrettyCore.prettyDef env d

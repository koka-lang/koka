-----------------------------------------------------------------------------
-- Copyright 2020-2022, Microsoft Research, Daan Leijen, Anton Lorenzen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Check if a function is FIP/FBIP
-----------------------------------------------------------------------------

module Core.CheckFBIP( checkFBIP
                     ) where


import qualified Lib.Trace
import Control.Monad
import Data.List (foldl', tails, uncons, isSuffixOf, foldl1', partition, sortOn)
import qualified Data.Set as S
import qualified Data.Map as M

import Lib.PPrint
import Common.Name
import Common.Range
import Common.Unique
import Common.Error
import Common.Syntax

import Kind.Newtypes

import Type.Type
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Core.Core
import qualified Core.Core as Core
import Core.Pretty
import Core.CoreVar
import Core.Borrowed ( Borrowed, borrowedLookup, borrowedLookupFip )
import Common.NamePrim (nameEffectEmpty, nameTpDiv, nameEffectOpen, namePatternMatchError, nameTpException, nameTpPartial, nameTrue,
                        nameCCtxSetCtxPath, nameFieldAddrOf, nameTpInt,
                        nameLazyMemoizeTarget,nameLazyLeave,nameLazyEnter,nameLazyMemoize, nameCoreDebug)
import Backend.C.ParcReuse (getFixedDataAllocSize, Reusable(..), ruIsReusable)
import Backend.C.Parc (getDataInfo', needsDupDropData)
import Data.Ratio
import Data.Ord (Down (Down))
import Control.Monad.Reader
import Control.Monad.Writer
import Common.Id

trace s x =
  Lib.Trace.trace s
    x


checkFBIP :: Pretty.Env ->  Platform -> Newtypes -> Borrowed -> Gamma -> CorePhase b ()
checkFBIP penv platform newtypes borrowed gamma
  = do uniq      <- unique
       defGroups <- getCoreDefs
       let (_,warns) = runChk penv uniq platform newtypes borrowed gamma (chkDefGroups defGroups)
       liftError (warningMsgs [warningMessageKind ErrFip range code doc | (code,range,doc) <- warns])


{--------------------------------------------------------------------------
  check definition groups
--------------------------------------------------------------------------}

chkDefGroups :: DefGroups -> Chk ()
chkDefGroups = mapM_ chkDefGroup

chkDefGroup :: DefGroup -> Chk ()
chkDefGroup defGroup
  = case defGroup of
      DefRec defs -> mapM_ (chkTopLevelDef (map defName defs)) defs
      DefNonRec def -> chkTopLevelDef [defName def] def

chkTopLevelDef :: [Name] -> Def -> Chk ()
chkTopLevelDef defGroupNames def
  = withCurrentDef def $ do
      case defSort def of
        -- only check fip and fbip annotated functions
        DefFun borrows fip | not (isFipTop fip) ->
          withFip fip $
            do out <- extractOutput $
                      withInput (\_ -> Input S.empty defGroupNames True) $
                      chkTopLevelExpr borrows (defExpr def)
               checkOutputEmpty out
        _ -> return ()

-- | Lambdas at the top-level are part of the signature and not allocations.
chkTopLevelExpr :: [ParamInfo] -> Expr -> Chk ()
chkTopLevelExpr borrows (Lam pars eff body)
  = do chkEffect eff
       let bpars = map snd $ filter ((==Borrow) . fst) $ zipParamInfo borrows pars
       let opars = map snd $ filter ((==Own) . fst) $ zipParamInfo borrows pars
       withBorrowed (S.fromList $ map getName bpars) $ do
         out <- extractOutput $ chkExpr body
         writeOutput =<< foldM (\out nm -> bindName nm NotReusable out) out opars
chkTopLevelExpr borrows (TypeLam _ body)
  = chkTopLevelExpr borrows body
chkTopLevelExpr borrows (TypeApp body _)
  = chkTopLevelExpr borrows body
chkTopLevelExpr borrows expr
  = chkExpr expr

chkExpr :: Expr -> Chk ()
chkExpr expr
  = case expr of
      TypeLam _ body -> chkExpr body
      TypeApp body _ -> chkExpr body
      Lam pars eff body
        -> do chkEffect eff
              requireCapability FipWarningLambdaAllocation mayAlloc $ \ppenv -> Just $
                text "allocating a lambda expression"
              out <- extractOutput $ chkExpr body
              writeOutput =<< foldM (\out nm -> bindName nm NotReusable out) out pars

      App (TypeApp (Var tname _) _) _ | getName tname `elem` [nameCCtxSetCtxPath,nameLazyMemoizeTarget,nameLazyEnter,nameLazyLeave]
        -> return ()
      App (TypeApp (Var tname _) _) [_, conApp] | getName tname == nameLazyMemoize
        -> chkExpr conApp

      App fn args -> chkApp fn args
      Var tname info -> markSeen tname info

      Let [] body -> chkExpr body
      Let (DefNonRec def:dgs) body
        -> do out <- extractOutput $ chkExpr (Let dgs body)
              gamma2 <- bindName (defTName def) NotReusable out
              writeOutput gamma2
              withBorrowed (S.map getName $ M.keysSet $ gammaNm gamma2) $
                withTailMod [Let dgs body] $ chkExpr $ defExpr def
      Let _ _
        -> emitWarning FipWarningUnsupportedNestedBinding $ \penv -> text "internal: currently the fip analysis cannot handle nested function bindings"

      Case scrutinees branches
        -> chkBranches scrutinees branches
      Con _ _ -> pure () -- Atoms are non-allocated
      Lit lit -> chkLit lit

chkModCons :: [Expr] -> Chk ()
chkModCons [] = pure ()
chkModCons args
  = zipWithM_ (\a tl -> withTailMod tl $ chkExpr a) args (tail $ tails args)

chkBranches :: [Expr] -> [Branch] -> Chk ()
chkBranches scrutinees branches
  = do whichBorrowed <- mapM isBorrowedScrutinee scrutinees
       let branches' = filter (not . isPatternMatchError) branches
       let stypes = map typeOf scrutinees
       outs <- mapM (extractOutput . chkBranch whichBorrowed stypes) branches'
       gamma2 <- joinContexts (map branchPatterns branches') outs
       writeOutput gamma2
       withBorrowed (S.map getName $ M.keysSet $ gammaNm gamma2) $
         withTailModProduct branches' $ -- also filter out pattern match errors
           mapM_ chkScrutinee $ zip whichBorrowed scrutinees

isBorrowedScrutinee :: Expr -> Chk ParamInfo
isBorrowedScrutinee expr@(Var tname info)
  = do b <- isBorrowed tname
       pure $ if b then Borrow else Own
isBorrowedScrutinee _ = pure Own

chkScrutinee :: (ParamInfo, Expr) -> Chk ()
chkScrutinee (Borrow, Var tname info) = pure ()
chkScrutinee (_, expr) = chkExpr expr

chkBranch :: [ParamInfo] -> [Type] -> Branch -> Chk ()
chkBranch whichBorrowed stypes (Branch pats guards)
  = do let (borPats, ownPats) = partition ((==Borrow) .fst) $ zip whichBorrowed $ zip pats stypes
       outs <- withBorrowed (S.map getName $ bv $ map (fst . snd) borPats) $
                mapM (extractOutput . chkGuard) guards
       out <- joinContexts (repeat pats) outs
       writeOutput =<< foldM (flip bindPattern) out (map snd ownPats)

chkGuard :: Guard -> Chk ()
chkGuard (Guard test expr)
  = do out <- extractOutput $ chkExpr expr
       withBorrowed (S.map getName $ M.keysSet $ gammaNm out) $
         withNonTail $ chkExpr test
       writeOutput out

-- | We ignore default branches that create a pattern match error
isPatternMatchError :: Branch -> Bool
isPatternMatchError (Branch pats [Guard (Con gname _) (App (App _ [TypeApp (Var (TName fnname _) _) _]) _)])
  | all isPatWild pats && getName gname == nameTrue && fnname == namePatternMatchError = True
  where isPatWild PatWild = True; isPatWild _ = False
isPatternMatchError b = False

bindPattern :: (Pattern, Type) -> Output -> Chk Output
bindPattern (PatCon cname pats crepr types _ _ _ _, tp) out
  = do size <- getConstructorAllocSize crepr
       provideToken cname size =<< foldM (flip bindPattern) out (zip pats types)
bindPattern (PatVar tname (PatCon cname pats crepr types _ _ _ _), tp) out
  = do size <- getConstructorAllocSize crepr
       bindName tname size out
bindPattern (PatVar tname PatWild, _) out
  = bindName tname NotReusable out
bindPattern (PatVar tname pat, tp) out   -- Else, don't bind the name.
  = bindPattern (pat, tp) out            -- The end of the analysis fails if the name is actually used.
bindPattern (PatLit _, _) out = pure out
bindPattern (PatWild, tp) out
  = do ndd <- needsDupDropTp tp
       when ndd $
         requireCapability FipWarningDroppedPatternBinder mayDealloc $ \ppenv -> Just $
           vcat [text "binding a wildcard pattern of type" <+> pretty tp <+> text "causes deallocation"]
       pure out

chkApp :: Expr -> [Expr] -> Chk ()
chkApp (TypeLam _ fn) args = chkApp fn args -- ignore type machinery
chkApp (TypeApp fn _) args = chkApp fn args
chkApp (App (TypeApp (Var openName _) _) [fn]) args | getName openName == nameEffectOpen
  = chkApp fn args
chkApp (Con cname repr) args -- try reuse
  = do chkModCons args
       chkLazyCon cname repr
       chkAllocation cname repr
chkApp (Var tname info) args | not (infoIsRefCounted info) -- toplevel function
  = do bs <- getParamInfos (getName tname)
       withNonTail $ mapM_ chkArg $ zipParamInfo bs args
       chkFunCallable (getName tname)
       input <- getInput
       unless (isTailContext input || getName tname `notElem` defGroupNames input) $
         requireCapability FipWarningNonTailRecursive mayRecurse $ \ppenv -> Just $
           cat [text "non-tail call to a (mutually) recursive function: ", ppName ppenv (getName tname)]
chkApp fn args -- local function
  = do withNonTail $ mapM_ chkExpr args
       isBapp <- case fn of -- does the bapp rule apply?
         Var tname _ -> isBorrowed tname
         _ -> pure False
       unless isBapp $ do
         requireCapability FipWarningOwnedCall mayDealloc $ \ppenv -> Just $
           vcat [text "owned calls to functions require deallocation: ", source ppenv (prettyExpr ppenv fn) ]
         chkExpr fn

chkArg :: (ParamInfo, Expr) -> Chk ()
chkArg (Own, expr) = chkExpr expr
chkArg (Borrow, expr)
  = case expr of
      (TypeLam _ fn) -> chkArg (Borrow, fn)
      (TypeApp fn _) -> chkArg (Borrow, fn)
      (App (TypeApp (Var openName _) _) [fn]) | getName openName == nameEffectOpen
        -> chkArg (Borrow, fn) -- disregard .open calls
      (Var tname info) -> markBorrowed tname info
      (Lit _) -> pure ()
      _ -> do chkExpr expr
              ndd <- needsDupDropTp (typeOf expr)
              when ndd $
                requireCapability FipWarningOwnedAsBorrowed mayDealloc $ \ppenv -> Just $
                  vcat [text "passing owned expressions as borrowed causes deallocation:", source ppenv (prettyExpr ppenv expr)]


chkLit :: Lit -> Chk ()
chkLit lit
  = case lit of
      LitInt _ -> pure () -- we do not care about allocating big integers
      LitFloat _ -> pure ()
      LitChar _ -> pure ()
      LitString _ -> pure ()
      -- requireCapability mayAlloc $ \ppenv -> Just $
       -- text "Inline string literals are allocated. Consider lifting to toplevel to avoid this."

chkWrap :: TName -> VarInfo -> Chk ()
chkWrap tname info
  = do bs <- getParamInfos (getName tname)
       unless (Borrow `notElem` bs) $
         emitWarning FipWarningFunctionWrapped $ \penv -> text "a function with borrowed parameters is passed as an argument and implicitly wrapped (causing allocation)"
       fip1 <- getFip
       fip2 <- lookupFip (getName tname)
       unless (fip1 `fipSubsumes` fip2) $
         emitWarning FipWarningFunctionWrapped $ \penv -> vcat [text $ "the function " ++ nameLocal (getName tname) ++ " is passed as an argument, but has an incompatible FIP/FBIP annotation:",
                                      text " expected at least" <+> text (show fip1) <+> text "but found" <+> text (show fip2)]
       when (fipAlloc fip2 /= AllocAtMost 0 && fipAlloc fip1 /= AllocUnlimited) $
           emitWarning FipWarningFunctionWrapped $ \penv -> text $
                "the " ++ show fip2 ++ " function "
             ++ nameLocal (getName tname)
             ++ " is passed as an argument and may be called an unlimited number of times, causing unlimited allocation."

chkAllocation :: TName -> ConRepr -> Chk ()
chkAllocation cname repr | isConAsJust repr = pure ()
chkAllocation cname repr | "_noreuse" `isSuffixOf` nameLocal (conTypeName repr)
  = requireCapability FipWarningExplicitNoReuse mayAlloc $ \ppenv -> Just $
      cat [text "types suffixed with _noreuse are not reused: ", ppName ppenv $ conTypeName repr]
chkAllocation cname crepr
  = do size <- getConstructorAllocSize crepr
       -- chkTrace $ "Allocation " ++ show cname ++ "/" ++ show size
       getAllocation cname size

-- Only total/empty effects or divergence
chkEffect :: Tau -> Chk ()
chkEffect tp
  = if isFBIPExtend tp then pure () else
      emitWarning FipWarningEffectsAllocate $ \penv -> text "algebraic effects other than" <+> ppType penv typePure <+> text "may cause allocation."
  where
    isFBIPExtend tp = case extractEffectExtend tp of
      (taus, tau) -> all isFBIP taus
    isFBIP tp = case expandSyn tp of
        TCon tc -> typeConName tc `elem` [nameEffectEmpty,nameTpDiv,nameTpPartial]
        TApp tc1 [TCon (TypeCon nm _)] -> eqType tc1 tconHandled && nm == nameTpPartial
        _       -> False

{--------------------------------------------------------------------------
  Chk monad
--------------------------------------------------------------------------}
type Chk a = ReaderT (Env, Input) (WriterT (Output, [(FipErrorCode,Range,Doc)]) Unique) a

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env,
                platform  :: Platform,
                newtypes  :: Newtypes,
                borrowed  :: Borrowed,
                gamma     :: Gamma,
                fip       :: Fip
              }

data Input = Input{ delta :: S.Set Name,
                    defGroupNames :: [Name],
                    isTailContext :: Bool }

data AllocTree
  = Alloc Id          -- ^ allocation with unique identifier
  | Call FipAlloc     -- ^ call using allocation credits
  | CallSelf FipAlloc -- ^ self-call using allocation credits
  | Seq AllocTree AllocTree
  | Match [AllocTree]
  | Leaf

data Output = Output{ gammaNm :: M.Map TName Int,
                      -- ^ matches variables to their number of uses
                      gammaDia :: M.Map Int [(Ratio Int, [(TName,Id)])],
                      -- ^ matches token size to allocations with a "probability"
                      -- sorted in descending order of probability
                      allocTree :: AllocTree }

instance Semigroup Output where
  Output s1 m1 t1 <> Output s2 m2 t2 =
    Output (M.unionWith (+) s1 s2) (M.unionWith (\x y -> sortOn (Down . fst) (x ++ y)) m1 m2) (Seq t1 t2)

instance Monoid Output where
  mempty = Output M.empty M.empty Leaf

data FipErrorCode 
  = FipWarningLambdaAllocation
  | FipWarningFunctionWrapped
  | FipWarningDroppedPatternBinder
  | FipWarningNameUnbound
  | FipWarningOwnedCall
  | FipWarningOwnedAsBorrowed
  | FipWarningNonFipCall
  | FipWarningEffectsAllocate
  | FipWarningNonTailRecursive
  | FipWarningNonLazy
  | FipWarningMatchConstructorDropped
  | FipWarningBranchUsageDiffers
  | FipWarningLastUseBorrowed
  | FipWarningVariableShared
  | FipWarningVariableDropped
  | FipWarningFipDeclarationMismatch
  | FipWarningExplicitNoReuse
  | FipWarningFipInformationNotFound
  | FipWarningUnsupportedNestedBinding
instance ErrorCode FipErrorCode where
  codeNum FipWarningLambdaAllocation = 0
  codeNum FipWarningFunctionWrapped = 1
  codeNum FipWarningDroppedPatternBinder = 2
  codeNum FipWarningNameUnbound = 3
  codeNum FipWarningOwnedCall = 4
  codeNum FipWarningOwnedAsBorrowed = 5
  codeNum FipWarningNonFipCall = 6
  codeNum FipWarningEffectsAllocate = 7
  codeNum FipWarningNonTailRecursive = 8
  codeNum FipWarningNonLazy = 9
  codeNum FipWarningMatchConstructorDropped = 10
  codeNum FipWarningBranchUsageDiffers = 11
  codeNum FipWarningLastUseBorrowed = 12
  codeNum FipWarningVariableShared = 13
  codeNum FipWarningVariableDropped = 14
  codeNum FipWarningFipDeclarationMismatch = 15
  codeNum FipWarningExplicitNoReuse = 100
  codeNum FipWarningFipInformationNotFound = 101
  codeNum FipWarningUnsupportedNestedBinding = 200
  codeDoc FipWarningLambdaAllocation = text "lambda allocation is not fip"
  codeDoc FipWarningFunctionWrapped = text "function is wrapped and causes allocation"
  codeDoc FipWarningDroppedPatternBinder = text "wildcard pattern binder causes deallocation"
  codeDoc FipWarningNameUnbound = text "borrowed name may have been used"
  codeDoc FipWarningOwnedCall = text "owned call requires deallocation"
  codeDoc FipWarningOwnedAsBorrowed = text "passing owned expression as borrowed causes deallocation"
  codeDoc FipWarningNonFipCall = text "non-fip function call"
  codeDoc FipWarningEffectsAllocate = text "effects other than pure may cause allocation"
  codeDoc FipWarningNonTailRecursive = text "non-tail (mutually) recursive function causes allocation"
  codeDoc FipWarningNonLazy = text "non-lazy constructor causes allocation"
  codeDoc FipWarningMatchConstructorDropped = text "match constructor is not reused"
  codeDoc FipWarningBranchUsageDiffers = text "branch usage differs from the function definition"
  codeDoc FipWarningLastUseBorrowed = text "last use of variable is borrowed (causing deallocation)"
  codeDoc FipWarningVariableShared = text "variable is shared (causing deallocation)"
  codeDoc FipWarningVariableDropped = text "variable is dropped (causing deallocation)"
  codeDoc FipWarningExplicitNoReuse = text "explicit _no_reuse causes allocation"
  codeDoc FipWarningFipDeclarationMismatch = text "fip declaration mismatch"
  codeDoc FipWarningUnsupportedNestedBinding = text "nested function bindings not supported currently"

prettyGammaNm :: Pretty.Env -> Output -> Doc
prettyGammaNm ppenv (Output nm dia _)
  = tupled $ map
      (\(nm, cnt) -> cat [ppName ppenv (getName nm), text "/", pretty cnt])
      (M.toList nm)

prettyCon :: Pretty.Env -> TName -> Int -> Doc
prettyCon ppenv tname sz
  = ppName ppenv (getName tname) <.> text "/" <.> pretty (sz {-`div` 8-})

prettyGammaDia :: Pretty.Env -> Output -> Doc
prettyGammaDia ppenv (Output nm dia _)
  = tupled $ concatMap
      (\(sz, cs) -> map (\(_, (c,_):_) -> prettyCon ppenv c sz) cs)
      (M.toList dia)

runChk :: Pretty.Env -> Int -> Platform -> Newtypes -> Borrowed -> Gamma -> Chk a -> (a,[(FipErrorCode,Range,Doc)])
runChk penv u platform newtypes borrowed gamma c
  = fst $ runUnique 0 $
    fmap (fmap snd) $ runWriterT $
    runReaderT c (Env [] penv platform newtypes borrowed gamma noFip, Input S.empty [] True)

withEnv :: (Env -> Env) -> Chk a -> Chk a
withEnv f = withReaderT (\(e, i) -> (f e, i))

getEnv :: Chk Env
getEnv = asks fst

withInput :: (Input -> Input) -> Chk a -> Chk a
withInput f = withReaderT (\(e, i) -> (e, f i))

getInput :: Chk Input
getInput = asks snd

writeOutput :: Output -> Chk ()
writeOutput out = tell (out, [])

withFip :: Fip -> Chk a -> Chk a
withFip f chk
  = withEnv (\env -> env{fip=f}) chk

getFip :: Chk Fip
getFip = fip <$> getEnv

mayRecurse :: Chk Bool
mayRecurse
  = do fip <- getFip
       pure $ case fip of
          Fip n -> False
          Fbip n isTail -> not isTail
          NoFip isTail  -> not isTail

mayDealloc :: Chk Bool
mayDealloc
  = do fip <- getFip
       pure $ case fip of
          Fip n -> False
          _ -> True

mayAlloc :: Chk Bool
mayAlloc = (==AllocUnlimited) . fipAlloc <$> getFip

chkFunCallable :: Name -> Chk ()
chkFunCallable fn
  = do fip <- getFip
       g <- gamma <$> getEnv
       case getFipInfo (gammaLookupCanonical fn g) of
         Nothing | fn `elem` [nameCCtxSetCtxPath,nameFieldAddrOf]
           -> writeCallAllocation fn (Fip (AllocAtMost 0))
         Nothing
           -> emitWarning FipWarningFipInformationNotFound $  \penv -> text "internal: fip analysis could not find fip information for function:" <+> ppName penv fn
         Just fip'
           -> if fip' `isCallableFrom` fip then writeCallAllocation fn fip'
              else emitWarning FipWarningNonFipCall $ \penv -> text "calling a non-fip function:" <+> ppName penv fn
  where
    isCallableFrom (Fip _)    _          = True
    isCallableFrom (Fbip _ _) (Fbip _ _) = True
    isCallableFrom _          (NoFip _)  = True
    isCallableFrom _          _          = False

    getFipInfo :: [NameInfo] -> Maybe Fip
    getFipInfo [InfoFun _ _ _ _ _ fip _ _] = Just fip
    getFipInfo [Type.Assumption.InfoExternal _ _ _ _ fip _ _] = Just fip
    getFipInfo _ = Nothing

    writeCallAllocation :: Name -> Fip -> Chk ()
    writeCallAllocation fn fip
      = do defs <- currentDefNames
           let call = if fn `elem` defs then CallSelf else Call
           case fip of
             Fip n    -> tell (Output mempty mempty (call n), mempty)
             Fbip n _ -> tell (Output mempty mempty (call n), mempty)
             NoFip _  -> pure ()

chkLazyCon :: TName -> ConRepr -> Chk ()
chkLazyCon (TName cname _) repr
  = do fip <- getFip
       mdi <- getDataInfoTypeName (conTypeName repr)
       case mdi of
             Just di -> case filter (\ci -> cname == conInfoName ci) (dataInfoConstrs di) of
                          [conInfo] -> do
                             let fip' = conInfoLazyFip conInfo
                             if not (conInfoIsLazy conInfo) || fip' `isCallableFrom` fip
                             then writeCallAllocation cname fip'
                             else emitWarning FipWarningNonLazy $ \penv -> text "allocating a non-fip lazy constructor:" <+> ppName penv cname
                          _     -> warn
             Nothing -> warn
  where
    warn = emitWarning FipWarningFipInformationNotFound $  \penv -> text "internal: fip analysis could not find fip information for constructor:" <+> ppName penv cname

    -- you can use an fbip lazy constructor in a fip function
    isCallableFrom (NoFip _)  (Fip _)    = False
    isCallableFrom (NoFip _)  (Fbip _ _) = False
    isCallableFrom _          _          = True

    writeCallAllocation :: Name -> Fip -> Chk ()
    writeCallAllocation fn fip
      = do defs <- currentDefNames -- TODO: this should be type defs
           let call = if fn `elem` defs then CallSelf else Call
           case fip of
             Fip n    -> tell (Output mempty mempty (call n), mempty)
             Fbip n _ -> tell (Output mempty mempty (call n), mempty)
             NoFip _  -> pure ()

-- | Run the given check, keep the warnings but extract the output.
extractOutput :: Chk () -> Chk Output
extractOutput f
  = do ((), (out, doc)) <- censor (const mempty) $ listen f
       tell (mempty, doc)
       pure out

-- | Perform a test if the capability is not present
-- and emit a warning if the test is unsuccessful.
requireCapability :: FipErrorCode -> Chk Bool -> (Pretty.Env -> Maybe Doc) -> Chk ()
requireCapability code mayUseCap test
  = do hasCap <- mayUseCap
       unless hasCap $ do
         env <- getEnv
         case test (prettyEnv env) of
           Just warning -> emitWarning code (\_ -> warning)
           Nothing -> pure ()

withNonTail :: Chk a -> Chk a
withNonTail
  = withInput (\st -> st { isTailContext = False })

-- | Tail modulo a pattern-match. This handles modulo product contexts.
withTailModProduct :: [Branch] -> Chk a -> Chk a
withTailModProduct [Branch _ [Guard test expr]] | isExprTrue test
  = withTailMod [expr]
withTailModProduct _ = withNonTail

withTailMod :: [Expr] -> Chk a -> Chk a
withTailMod modExpr
  = withInput (\st -> st { isTailContext = isTailContext st && all isModCons modExpr })
  where
    isModCons :: Expr -> Bool
    isModCons expr
     = case expr of
         Var _ _     -> True
         TypeLam _ e -> isModCons e
         TypeApp e _ -> isModCons e
         Con _ _     -> True
         Lit _       -> True
         Let dgs e   -> all isModConsDef (flattenDefGroups dgs) && isModCons e
         App f args  -> isModConsFun f && all isModCons args
         _           -> False

    -- | Functions with non-observable execution can be moved before the mod-cons call.
    -- This is necessary for various casts introduced in the effect checker.
    isModConsFun :: Expr -> Bool
    isModConsFun expr
      = case expr of
          TypeLam _ e   -> isModConsFun e
          TypeApp e _   -> isModConsFun e
          Con _ _       -> True
          Let dgs e     -> all isModConsDef (flattenDefGroups dgs) && isModConsFun e
          App f args    -> hasTotalEffect (typeOf expr) && isModConsFun f && all isModCons args
          _             -> False

    isModConsDef def = isModCons (defExpr def)

withBorrowed :: S.Set Name -> Chk a -> Chk a
withBorrowed names action
  = withInput (\st -> st { delta = S.union names (delta st) }) action

isBorrowed :: TName -> Chk Bool
isBorrowed nm
  = do st <- getInput
       pure $ getName nm `S.member` delta st

markSeen :: TName -> VarInfo -> Chk ()
markSeen tname info | infoIsRefCounted info -- is locally defined?
  = do isHeapValue <- needsDupDrop tname
       when isHeapValue $
         writeOutput (Output (M.singleton tname 1) M.empty Leaf)
markSeen tname info = chkWrap tname info -- wrap rule

markBorrowed :: TName -> VarInfo -> Chk ()
markBorrowed nm info
  = do b <- isBorrowed nm
       unless b $ do
         markSeen nm info
         isHeapValue <- needsDupDrop nm
         when (isHeapValue && infoIsRefCounted info) $
           requireCapability FipWarningLastUseBorrowed mayDealloc $ \ppenv -> Just $
             text "the last use of" <+> ppName ppenv (getName nm) <+> text "is borrowed (causing deallocation)"

getAllocation :: TName -> Reusable -> Chk ()
getAllocation nm NotReusable = pure ()
getAllocation nm (ReusableWithSize size)
  = do id <- lift $ lift $ uniqueId "alloc"
       writeOutput (Output mempty (M.singleton size [(1 % 1, [(nm,id)])]) (Alloc id))

provideToken :: TName -> Reusable -> Output -> Chk Output
provideToken _ NotReusable out = pure out
provideToken debugName (ReusableWithSize size) out
  = do requireCapability FipWarningMatchConstructorDropped mayDealloc $ \ppenv ->
         let fittingAllocs = M.findWithDefault [] size (gammaDia out) in
         case fittingAllocs of
           [] -> Just $ text "the matched constructor" <+> prettyCon ppenv debugName size <+> text "is not reused"
           ((r, _):_) | r /= 1%1 ->
             Just $ text "not all branches can reuse the space provided by" <+> prettyCon ppenv debugName size
           _ -> Nothing
       pure $ out { gammaDia = M.update (fmap snd . uncons) size (gammaDia out) }

joinContexts :: [[Pattern]] -> [Output] -> Chk Output
joinContexts _ [] = pure mempty
joinContexts pats cs
  = do let unionNm = foldl1' (M.unionWith max) (map gammaNm cs)
       (noDealloc, cs') <- fmap unzip $ forM cs $ \c -> do
         let unused = M.difference unionNm (gammaNm c)
         (allReusable, c') <- foldM tryReuse (True, c) (map fst $ M.toList unused)
         pure (allReusable, c')
       unless (and noDealloc) $ do
         requireCapability FipWarningBranchUsageDiffers mayDealloc $ \ppenv -> Just $
           vcat $ text "not all branches use the same variables:"
             : zipWith (\ps out -> cat [tupled (map (prettyPat ppenv) ps), text " -> ", prettyGammaNm ppenv out]) pats cs
       let unionDia = foldl1' (M.unionWith zipTokens) $ map (M.map (adjustProb (length cs')) . gammaDia) cs'
       pure (Output unionNm unionDia (Match (map allocTree cs')))
  where
    adjustProb n xs = map (\(p, x) -> (p / (n%1), x) ) xs

    zipTokens ((px, x):xs) ((py, y):ys) = (px + py, x ++ y) : zipTokens xs ys
    zipTokens xs [] = xs
    zipTokens [] ys = ys

    tryReuse (allReusable, out) tname
      = do isReusable <- addTypeReuseInfo tname NotReusable
           case isReusable of
             NotReusable -> do
              isHeapVal <- needsDupDrop tname
              pure (allReusable && not isHeapVal, out)
             size@(ReusableWithSize _) -> do
              out <- provideToken tname size out
              pure (allReusable, out)

    prettyPat ppenv (PatCon nm [] _ _ _ _ _ _) = ppName ppenv (getName nm)
    prettyPat ppenv (PatCon nm pats _ _ _ _ _ _) = ppName ppenv (getName nm) <.> tupled (map (prettyPat ppenv) pats)
    prettyPat ppenv (PatVar nm PatWild) = ppName ppenv (getName nm)
    prettyPat ppenv (PatVar nm pat) = cat [ppName ppenv (getName nm), text " as ", prettyPat ppenv pat]
    prettyPat ppenv (PatLit l) = text $ show l
    prettyPat ppenv PatWild = text "_"

addTypeReuseInfo :: TName -> Reusable -> Chk Reusable
addTypeReuseInfo nm size@(ReusableWithSize _) = pure size
addTypeReuseInfo nm NotReusable
  = do newtypes <- getNewtypes
       platform <- getPlatform
       pure $ case getFixedDataAllocSize platform newtypes (tnameType nm) of
         Nothing -> NotReusable
         Just (sz, _) -> ReusableWithSize sz

bindName :: TName -> Reusable -> Output -> Chk Output
bindName nm msize out
  = case M.lookup nm (gammaNm out) of
      Nothing -- unused, so available for drop-guided reuse!
        -> do isReusable <- addTypeReuseInfo nm msize
              (out, isReused) <- case isReusable of
                NotReusable -> pure (out, False)
                size@(ReusableWithSize _) -> do
                  out <- provideToken nm size out
                  pure (out, True)
              chkDrop isReused nm
              pure out
      Just n -- variable is used 'n' times
        -> do isHeapVal <- needsDupDrop nm
              when (n > 1 && isHeapVal) $
                requireCapability FipWarningVariableShared mayAlloc $ \ppenv -> Just $
                  text "the variable" <+> ppName ppenv (getName nm) <+> text "is used multiple times (causing sharing and preventing reuse)"
              pure $ out { gammaNm = M.delete nm (gammaNm out) }

chkDrop :: Bool -> TName -> Chk ()
chkDrop isTopLevelReused nm
  = do isHeapValue <- needsDupDrop nm
       isFlat <- isFlatType nm
       unless ((isTopLevelReused && isFlat) || not isHeapValue) $
          -- non-reused heap values are dropped
         requireCapability FipWarningVariableDropped mayDealloc $ \ppenv -> Just $
           text "the variable" <+> ppName ppenv (getName nm) <+> text "is unused (causing deallocation)"

-- | We record if the program has both an allocation
-- and a self-call which may be executed in sequence.
-- If that is the case, the program may use unlimited allocation.
data AllocInLoop = AllocInLoop
  { hasAlloc :: Bool,
    hasSelfCall :: Bool,
    hasBothInSequence :: Bool }

-- | Sequential composition
instance Semigroup AllocInLoop where
  AllocInLoop a s b <> AllocInLoop a' s' b'
    = AllocInLoop (a || a') (s || s')
        (b || b' || (a && s') || (a' && s))

instance Monoid AllocInLoop where
  mempty = AllocInLoop False False False

-- | Non-sequential composition
joinBranches :: AllocInLoop -> AllocInLoop -> AllocInLoop
joinBranches (AllocInLoop a s b) (AllocInLoop a' s' b')
  = AllocInLoop (a || a') (s || s') (b || b')

getAllocCredits :: S.Set Id -> AllocTree -> (FipAlloc, AllocInLoop)
getAllocCredits notReused tree
  = case tree of
      Alloc id | id `S.member` notReused -> (AllocAtMost 1, mempty { hasAlloc = True })
               | otherwise -> mempty
      Call alloc -> (alloc, mempty)
      CallSelf alloc -> (alloc, mempty { hasSelfCall = True })
      Seq a1 a2 -> getAllocCredits notReused a1 <> getAllocCredits notReused a2
      Match as -> foldl' (\(a, b) (a', b') -> (max a a', joinBranches b b')) mempty (map (getAllocCredits notReused) as)
      Leaf -> mempty

prettyFipAlloc :: FipAlloc -> String
prettyFipAlloc f
  = case f of
      AllocAtMost 0  -> "nothing"
      AllocAtMost n  -> "at most " ++ show n
      AllocFinitely  -> "a finite amount"
      AllocUnlimited -> "unlimited"

checkOutputEmpty :: Output -> Chk ()
checkOutputEmpty out
  = do case M.maxViewWithKey $ gammaNm out of
         Nothing -> pure ()
         Just ((nm, _), _)
           -> emitWarning FipWarningNameUnbound $ \penv -> text "unbound name (which may have been used despite being borrowed):" <+> ppName penv (getName nm)
       let notReused = S.fromList $ map snd $ concatMap snd $ concatMap snd $ M.toList $ gammaDia out
           (allocations, allocInLoop) = getAllocCredits notReused (allocTree out)
           allocations' = if hasBothInSequence allocInLoop then AllocUnlimited else allocations
       -- chkTrace $ show notReused
       -- chkTrace $ show $ simplifyAllocTree (allocTree out)
       permission <- fipAlloc <$> getFip
       unless (allocations' <= permission) $
         emitWarning FipWarningFipDeclarationMismatch $ \penv -> text "function allocates"
           <+> text (prettyFipAlloc allocations')
           <+> text "but was declared as allocating"
           <+> text (prettyFipAlloc permission)

simplifyAllocTree :: AllocTree -> AllocTree
simplifyAllocTree (Seq a b)
  = case (simplifyAllocTree a, simplifyAllocTree b) of
      (Leaf, b) -> b
      (a, Leaf) -> a
      (a, b) -> Seq a b
simplifyAllocTree (Match as) = Match (map simplifyAllocTree as)
simplifyAllocTree t = t

zipParamInfo :: [ParamInfo] -> [b] -> [(ParamInfo, b)]
zipParamInfo xs = zip (xs ++ repeat Own)

-- value types with reference fields still need a drop
needsDupDrop :: TName -> Chk Bool
needsDupDrop tname | isCCtxName (getName tname)  = return False  -- ignore generated contexts
needsDupDrop tname
  = needsDupDropTp (tnameType tname)

needsDupDropTp :: Type -> Chk Bool
needsDupDropTp tp
  = do mbdi <- getDataInfo tp
       pure $ case mbdi of
         Nothing -> True
         Just di ->
           -- We pretend that integers are always small and don't need to be dropped.
           (dataInfoName di /= nameTpInt) && needsDupDropData di

isFlatType :: TName -> Chk Bool
isFlatType tname = dataTypeIsFlat (tnameType tname)

-- A type is flat if no field of any constructor needs a dup/drop.
dataTypeIsFlat :: Type -> Chk Bool
dataTypeIsFlat tp
  = do mbdi <- getDataInfo tp
       case mbdi of
         Nothing -> return True
         Just di -> allM constrIsFlat $ dataInfoConstrs di
  where
    allM f xs = and <$> traverse f xs
    constrIsFlat constr = allM (fmap not . needsDupDropTp . snd) $ conInfoParams constr

getDataInfoTypeName :: Name -> Chk (Maybe DataInfo)
getDataInfoTypeName name = newtypesLookupAny name <$> getNewtypes

getDataInfo :: Type -> Chk (Maybe DataInfo)
getDataInfo tp
  = do newtypes <- getNewtypes
       return (getDataInfo' newtypes tp)

getNewtypes :: Chk Newtypes
getNewtypes = newtypes <$> getEnv

getPlatform :: Chk Platform
getPlatform = platform <$> getEnv

-- track the current definition for nicer error messages
withCurrentDef :: Def -> Chk a -> Chk a
withCurrentDef def action
  = -- trace ("checking: " ++ show (defName def)) $
    withEnv (\env -> env{currentDef = def:currentDef env}) $
    action

currentDefNames :: Chk [Name]
currentDefNames
  = do env <- getEnv
       return (map defName (currentDef env))

-- | Return borrowing infos for a name. May return the empty list
-- if no borrowing takes place.
getParamInfos :: Name -> Chk [ParamInfo]
getParamInfos name
  = do b <- borrowed <$> getEnv
       case borrowedLookup name b of
         Nothing -> return []
         Just pinfos -> return pinfos

lookupFip :: Name -> Chk Fip
lookupFip name
  = do b <- borrowed <$> getEnv
       case borrowedLookupFip name b of
         Nothing -> return noFip
         Just fip -> return fip

traceDoc :: (Pretty.Env -> Doc) -> Chk ()
traceDoc f
  = do env <- getEnv
       chkTrace (show (f (prettyEnv env)))

chkTrace :: String -> Chk ()
chkTrace msg
  = do env <- getEnv
       trace ("chk: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

emitDoc :: FipErrorCode -> Range -> Doc -> Chk ()
emitDoc code rng doc = tell (mempty, [(code,rng,doc)])

emitWarning :: FipErrorCode -> (Pretty.Env -> Doc) -> Chk ()
emitWarning code makedoc
  = do env <- getEnv
       let (rng,name) = case currentDef env of
                          (def:_) -> (defNameRange def, defName def)
                          _ -> (rangeNull, nameNil)
           penv = prettyEnv env
           fdoc = text (show (fip env)) <+> text "fun" <+> ppName penv name <.> colon <+> makedoc penv
       when (qualifier name /= nameCoreDebug) $
          emitDoc code rng fdoc

getConstructorAllocSize :: ConRepr -> Chk Reusable
getConstructorAllocSize conRepr
  = do ruIsReusable conRepr <$> getPlatform

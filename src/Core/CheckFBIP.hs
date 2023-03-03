-----------------------------------------------------------------------------
-- Copyright 2020-2022, Microsoft Research, Daan Leijen, Anton Lorenzen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Check if a function is FBIP
-----------------------------------------------------------------------------

module Core.CheckFBIP( checkFBIP
                     ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative
import Data.List( partition, intersperse, foldl1', foldl', isSuffixOf, uncons )
import qualified Data.Set as S
import qualified Data.Map as M

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.Error
import Common.Syntax

import Kind.Kind
import Kind.Newtypes

import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Core.Core
import qualified Core.Core as Core
import Core.Pretty
import Core.CoreVar
import Core.Borrowed
import Common.NamePrim (nameEffectEmpty, nameTpDiv, nameEffectOpen, namePatternMatchError, nameTpException, nameTpPartial, nameTrue)
import Backend.C.ParcReuse (getFixedDataAllocSize)
import Backend.C.Parc (getDataDef')

trace s x =
  Lib.Trace.trace s
    x


checkFBIP :: Pretty.Env ->  Platform -> Newtypes -> Borrowed ->  CorePhase ()
checkFBIP penv platform newtypes borrowed
  = do uniq      <- unique
       defGroups <- getCoreDefs
       let (_,docs) = runChk penv uniq platform newtypes borrowed (chkDefGroups defGroups)
       mapM_ (\doc -> liftError (warningMsg (rangeNull, doc))) docs


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
        DefFun borrows fip | not (isNoFip fip) -> 
           do out <- withFip fip $
                     extractOutput $                      
                     withInput (\_ -> Input S.empty (capFromFip fip) defGroupNames True) $
                     chkTopLevelExpr borrows fip (defExpr def)
              checkOutputEmpty out
        _ -> return ()


-- | Lambdas at the top-level are part of the signature and not allocations.
chkTopLevelExpr :: [ParamInfo] -> Fip -> Expr -> Chk ()
chkTopLevelExpr borrows fip (Lam pars eff body)  -- todo: track fip to adjust warnings
  = do chkEffect eff
       let bpars = map snd $ filter ((==Borrow) . fst) $ zipDefault Own borrows pars
       let opars = map snd $ filter ((==Own) . fst) $ zipDefault Own borrows pars
       withBorrowed (S.fromList $ map getName bpars) $ do
         out <- extractOutput $ chkExpr body
         writeOutput =<< foldM (\out nm -> bindName nm Nothing out) out opars
chkTopLevelExpr borrows fip (TypeLam _ body)
  = chkTopLevelExpr borrows fip  body
chkTopLevelExpr borrows fip (TypeApp body _)
  = chkTopLevelExpr borrows fip  body
chkTopLevelExpr borrows fip expr 
  = chkExpr expr

chkExpr :: Expr -> Chk ()
chkExpr expr
  = case expr of
      TypeLam _ body -> chkExpr body
      TypeApp body _ -> chkExpr body
      Lam pars eff body
        -> do chkEffect eff
              requireCapability HasAlloc $ \ppenv -> Just $
                text "Lambdas are always allocated."
              out <- extractOutput $ chkExpr body
              writeOutput =<< foldM (\out nm -> bindName nm Nothing out) out pars

      App fn args -> chkApp fn args
      Var tname info -> markSeen tname info

      Let [] body -> chkExpr body
      Let (DefNonRec def:dgs) body
        -> do out <- extractOutput $ chkExpr (Let dgs body)
              gamma2 <- bindName (defTName def) Nothing out
              writeOutput gamma2
              withBorrowed (S.map getName $ M.keysSet $ gammaNm gamma2) $
                withNonTailCtx $ chkExpr $ defExpr def
      Let _ _
        -> unhandled $ text "FBIP check can not handle recursive let bindings."

      Case scrutinees branches
        -> chkBranches scrutinees branches
      Con _ _ -> pure () -- Atoms are non-allocated
      Lit lit -> chkLit lit

chkModCons :: [Expr] -> Chk ()
chkModCons [] = pure ()
chkModCons args
  = do let (larg:rargs) = reverse args
       withNonTailCtx $ mapM_ chkExpr rargs
       chkExpr larg -- can be tail-mod-cons

chkBranches :: [Expr] -> [Branch] -> Chk ()
chkBranches scrutinees branches
  = do whichBorrowed <- mapM chkScrutinee scrutinees
       let branches' = filter (not . isPatternMatchError) branches
       outs <- mapM (extractOutput . chkBranch whichBorrowed) branches'
       writeOutput =<< joinContexts (map branchPatterns branches') outs
  where
    fromVar (Var tname _) = Just tname
    fromVar _ = Nothing

chkScrutinee :: Expr -> Chk ParamInfo
chkScrutinee expr@(Var tname info)
  = do b <- isBorrowed tname
       unless b $ markSeen tname info
       pure (if b then Borrow else Own)
chkScrutinee expr
  = do withNonTailCtx $ chkExpr expr
       pure Own

chkBranch :: [ParamInfo] -> Branch -> Chk ()
chkBranch whichBorrowed (Branch pats guards)
  = do let (borPats, ownPats) = partition ((==Borrow) .fst) $ zipDefault Own whichBorrowed pats
       out <- extractOutput $
         withBorrowed (S.map getName $ bv $ map snd borPats) $
           mapM_ chkGuard guards
       writeOutput =<< foldM (flip bindPattern) out (map snd ownPats)

chkGuard :: Guard -> Chk ()
chkGuard (Guard test expr)
  = do out <- extractOutput $ chkExpr expr
       withBorrowed (S.map getName $ M.keysSet $ gammaNm out) $
         withNonTailCtx $ chkExpr test
       writeOutput out

-- | We ignore default branches that create a pattern match error
isPatternMatchError :: Branch -> Bool
isPatternMatchError (Branch pats [Guard (Con gname _) (App (TypeApp (Var (TName fnname _) _) _) _)])
  | all isPatWild pats && getName gname == nameTrue && fnname == namePatternMatchError = True
  where isPatWild PatWild = True; isPatWild _ = False
isPatternMatchError _ = False

bindPattern :: Pattern -> Output -> Chk Output
bindPattern (PatCon cname pats crepr _ _ _ _ _) out
  = do size <- getConstructorAllocSize crepr 
       provideToken cname size =<< foldM (flip bindPattern) out pats
bindPattern (PatVar tname (PatCon cname pats crepr _ _ _ _ _)) out
  = do size <- getConstructorAllocSize crepr        
       bindName tname (Just size) =<< foldM (flip bindPattern) out pats
bindPattern (PatVar tname PatWild) out
  = bindName tname Nothing out
bindPattern (PatVar tname pat) out -- Else, don't bind the name.
  = bindPattern pat out            -- The end of the analysis fails if the name is actually used.
bindPattern (PatLit _) out = pure out
bindPattern PatWild out = pure out

chkApp :: Expr -> [Expr] -> Chk ()
chkApp (TypeLam _ fn) args = chkApp fn args -- ignore type machinery
chkApp (TypeApp fn _) args = chkApp fn args
chkApp (App (TypeApp (Var openName _) _) [fn]) args | getName openName == nameEffectOpen
  = chkApp fn args
chkApp (Con cname repr) args -- try reuse
  = do chkModCons args
       chkAllocation cname repr
chkApp (Var tname info) args | not (infoIsRefCounted info) -- toplevel function
  = do bs <- getParamInfos (getName tname)
       withNonTailCtx $ mapM_ chkArg $ zipDefault Own bs args
       input <- getInput
       unless (isTailContext input) $
         requireCapability HasStack $ \ppenv ->
           if getName tname `elem` defGroupNames input
           then Just $ cat [text "Non-tail call to (mutually) recursive function: ", ppName ppenv (getName tname)]
           else Nothing
chkApp fn args -- local function
  = do withNonTailCtx $ mapM_ chkExpr args
       isBapp <- case fn of -- does the bapp rule apply?
         Var tname _ -> isBorrowed tname
         _ -> pure False
       unless isBapp $ do
         requireCapability HasDealloc $ \ppenv -> Just $
           cat [text "Owned calls to functions require deallocation: ", prettyExpr ppenv fn ]
         chkExpr fn

chkArg :: (ParamInfo, Expr) -> Chk ()
chkArg (Own, expr) = chkExpr expr
chkArg (Borrow, Var tname info) = markBorrowed tname info
chkArg (Borrow, expr)
  = do chkExpr expr
       requireCapability HasDealloc $ \ppenv -> Just $
         text "Passing owned expressions as borrowed require deallocation."

chkLit :: Lit -> Chk ()
chkLit lit
  = case lit of
      LitInt _ -> pure () -- we do not care about allocating big integers
      LitFloat _ -> pure ()
      LitChar _ -> pure ()
      LitString _ -> requireCapability HasAlloc $ \ppenv -> Just $
        text "Inline string literals are allocated. Consider lifting to toplevel to avoid this."

chkWrap :: TName -> VarInfo -> Chk ()
chkWrap tname info
  = do bs <- getParamInfos (getName tname)
       unless (Borrow `notElem` bs) $
         unhandled $ text "FBIP analysis detected that a top-level function was wrapped."

chkAllocation :: TName -> ConRepr -> Chk ()
chkAllocation cname repr | isConAsJust repr = pure ()
chkAllocation cname repr | "_noreuse" `isSuffixOf` nameId (conTypeName repr)
  = requireCapability HasAlloc $ \ppenv -> Just $
      cat [text "Types suffixed with _noreuse are not reused: ", ppName ppenv $ conTypeName repr]
chkAllocation cname crepr
  = do size <- getConstructorAllocSize crepr        
       getAllocation cname size

-- Only total/empty effects or divergence
chkEffect :: Tau -> Chk ()
chkEffect tp
  = if isFBIPExtend tp then pure () else
      unhandled $ text "Algebraic effects other than <exn,div> are not FBIP."
  where
    isFBIPExtend tp = case extractEffectExtend tp of
      (taus, tau) -> all isFBIP taus
    isFBIP tp = case expandSyn tp of
        TCon tc -> typeConName tc `elem` [nameEffectEmpty,nameTpDiv,nameTpPartial]
        TApp tc1 [TCon (TypeCon nm _)] -> tc1 == tconHandled && nm == nameTpPartial
        _       -> False

{--------------------------------------------------------------------------
  Chk monad
--------------------------------------------------------------------------}
newtype Chk a = Chk (Env -> Input -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env,
                platform  :: Platform,
                newtypes  :: Newtypes,
                borrowed  :: Borrowed,
                fip       :: Fip
              }

data Capability
  = HasAlloc   -- may allocate and dup
  | HasDealloc -- may use drop and free
  | HasStack   -- may use non-tail recursion
  deriving (Eq, Ord, Bounded, Enum)

capFromFip :: Fip -> [Capability]
capFromFip fip 
  = case fip of
      Fip n -> []
      Fbip n isTail -> [HasDealloc] ++ (if isTail then [] else [HasStack])
      NoFip isTail  -> [HasDealloc,HasAlloc] ++ (if isTail then [] else [HasStack])

data Input = Input{ delta :: S.Set Name,
                    capabilities :: [Capability],
                    defGroupNames :: [Name],
                    isTailContext :: Bool }

data Output = Output{ gammaNm :: M.Map TName Int,
                      gammaDia :: M.Map Int [TName] }

instance Semigroup Output where
  Output s1 m1 <> Output s2 m2 = Output (M.unionWith (+) s1 s2) (M.unionWith (++) m1 m2)

instance Monoid Output where
  mempty = Output M.empty M.empty

prettyGammaNm :: Pretty.Env -> Output -> Doc
prettyGammaNm ppenv (Output nm dia)
  = tupled $ map
      (\(nm, cnt) -> cat [ppName ppenv (getName nm), text "/", pretty cnt])
      (M.toList nm)

prettyCon :: Pretty.Env -> TName -> Int -> Doc
prettyCon ppenv tname sz
  = cat [ppName ppenv (getName tname), text "/", pretty (sz {-`div` 8-})]

prettyGammaDia :: Pretty.Env -> Output -> Doc
prettyGammaDia ppenv (Output nm dia)
  = tupled $ concatMap
      (\(sz, cs) -> map (\c -> prettyCon ppenv c sz) cs)
      (M.toList dia)

data Result a = Ok a Output [Doc]

runChk :: Pretty.Env -> Int -> Platform -> Newtypes -> Borrowed -> Chk a -> (a,[Doc])
runChk penv u platform newtypes borrowed (Chk c)
  = case c (Env [] penv platform newtypes borrowed noFip) (Input S.empty [] [] True) of
      Ok x _out docs -> (x,docs)

instance Functor Chk where
  fmap f (Chk c)  = Chk (\env input -> case c env input of
                                        Ok x out dgs -> Ok (f x) out dgs)

instance Applicative Chk where
  pure  = return
  (<*>) = ap

instance Monad Chk where
  return x      = Chk (\env input -> Ok x mempty [])
  (Chk c) >>= f = Chk (\env input -> case c env input of
                                      Ok x out dgs -> case f x of
                                                        Chk d -> case d env input of
                                                                    Ok x' out' dgs' -> Ok x' (out <> out') (dgs ++ dgs'))

withEnv :: (Env -> Env) -> Chk a -> Chk a
withEnv f (Chk c)
  = Chk (\env st -> c (f env) st)

getEnv :: Chk Env
getEnv
  = Chk (\env st -> Ok env mempty [])

withInput :: (Input -> Input) -> Chk a -> Chk a
withInput f (Chk c)
  = Chk (\env st -> c env (f st))

getInput :: Chk Input
getInput
  = Chk (\env st -> Ok st mempty [])

writeOutput :: Output -> Chk ()
writeOutput out
  = Chk (\env st -> Ok () out [])

withFip :: Fip -> Chk a -> Chk a
withFip f chk
  = withEnv (\env -> env{fip=f}) chk

getFip :: Chk Fip  
getFip = fip <$> getEnv

-- | Run the given check, keep the warnings but extract the output.
extractOutput :: Chk () -> Chk Output
extractOutput (Chk f)
  = Chk (\env st -> case f env st of
                      Ok () out doc -> Ok out mempty doc)

useCapabilities :: [Capability] -> Chk a -> Chk a
useCapabilities cs
  = withInput (\st -> st {capabilities = cs})

hasCapability :: Capability -> Chk Bool
hasCapability c
  = do st <- getInput
       pure $ c `elem` capabilities st

-- | Perform a test if the capability is not present
-- and emit a warning if the test is unsuccessful.
requireCapability :: Capability -> (Pretty.Env -> Maybe Doc) -> Chk ()
requireCapability cap test
  = do hasCap <- hasCapability cap
       unless hasCap $ do
         env <- getEnv
         case test (prettyEnv env) of
           Just warning -> emitWarning warning
           Nothing -> pure ()

unhandled :: Doc -> Chk ()
unhandled doc
  = do hasAll <- and <$> mapM hasCapability (enumFromTo minBound maxBound)
       unless hasAll $ emitWarning doc

withNonTailCtx :: Chk a -> Chk a
withNonTailCtx
  = withInput (\st -> st { isTailContext = False })

withBorrowed :: S.Set Name -> Chk a -> Chk a
withBorrowed names action
  = withInput (\st -> st { delta = S.union names (delta st) }) action

isBorrowed :: TName -> Chk Bool
isBorrowed nm
  = do st <- getInput
       pure $ getName nm `S.member` delta st

markSeen :: TName -> VarInfo -> Chk ()
markSeen tname info | infoIsRefCounted info -- is locally defined?
  = do b <- isBorrowed tname
       isHeapValue <- needsDupDrop (tnameType tname)
       when isHeapValue $ if b
         then requireCapability HasAlloc $ \ppenv -> Just $
           cat [text "Borrowed value used as owned (can cause allocations later): ", ppName ppenv (getName tname)]
         else writeOutput (Output (M.singleton tname 1) M.empty)
markSeen tname info = chkWrap tname info -- wrap rule

markBorrowed :: TName -> VarInfo -> Chk ()
markBorrowed nm info
  = do b <- isBorrowed nm
       unless b $ do
         markSeen nm info
         when (infoIsRefCounted info) $
           requireCapability HasDealloc $ \ppenv -> Just $
             cat [text "Last use of variable is borrowed: ", ppName ppenv (getName nm)]

getAllocation :: TName -> Int -> Chk ()
getAllocation nm 0 = pure ()
getAllocation nm size
  = writeOutput (Output mempty (M.singleton size [nm]))

provideToken :: TName -> Int -> Output -> Chk Output
provideToken _ 0 out = pure out
provideToken debugName size out
  = do requireCapability HasDealloc $ \ppenv ->
         let fittingAllocs = M.findWithDefault [] size (gammaDia out) in
         if null fittingAllocs then Just $
            cat [text "Unused reuse token provided by ", prettyCon ppenv debugName size]
         else Nothing
       pure $ out { gammaDia = M.update (fmap snd . uncons) size (gammaDia out) }

joinContexts :: [[Pattern]] -> [Output] -> Chk Output
joinContexts _ [] = pure mempty
joinContexts pats cs
  = do let unionNm = foldl1' (M.unionWith max) (map gammaNm cs)
       (noDealloc, cs') <- fmap unzip $ forM cs $ \c -> do
         let nm = M.difference unionNm (gammaNm c)
         (allReusable, c') <- foldM tryReuse (True, c) (map fst $ M.toList nm)
         pure (allReusable, c')
       unless (and noDealloc) $ do
         requireCapability HasDealloc $ \ppenv -> Just $
           vcat $ text "Not all branches use the same variables:"
             : zipWith (\ps out -> cat [tupled (map (prettyPat ppenv) ps), text " -> ", prettyGammaNm ppenv out]) pats cs
       let unionDia = foldl1' (M.unionWith chooseLonger) (map gammaDia cs')
       requireCapability HasDealloc $ \ppenv ->
          let noDealloc = all (M.null . M.filter (not . null) . M.differenceWith lengthDifferent unionDia . gammaDia) cs'
          in if noDealloc then Nothing else Just $
           vcat $ text "Not all branches use the same reuse tokens:"
             : zipWith (\ps out -> cat [tupled (map (prettyPat ppenv) ps), text " -> ", prettyGammaDia ppenv out]) pats cs'
       pure (Output unionNm unionDia)
  where
    chooseLonger a b = if length a >= length b then a else b
    lengthDifferent a b = if length a /= length b then Just b else Nothing

    tryReuse (allReusable, out) tname
      = do mOut <- tryDropReuse tname out
           isHeapVal <- needsDupDrop (tnameType tname)
           pure $ case mOut of
             Nothing -> (allReusable && not isHeapVal, out)
             Just out -> (allReusable, out)

    prettyPat ppenv (PatCon nm [] _ _ _ _ _ _) = ppName ppenv (getName nm)
    prettyPat ppenv (PatCon nm pats _ _ _ _ _ _) = ppName ppenv (getName nm) <.> tupled (map (prettyPat ppenv) pats)
    prettyPat ppenv (PatVar nm PatWild) = ppName ppenv (getName nm)
    prettyPat ppenv (PatVar nm pat) = cat [ppName ppenv (getName nm), text " as ", prettyPat ppenv pat]
    prettyPat ppenv (PatLit l) = text $ show l
    prettyPat ppenv PatWild = text "_"

tryDropReuse :: TName -> Output -> Chk (Maybe Output)
tryDropReuse nm out
  = do newtypes <- getNewtypes
       platform <- getPlatform
       case getFixedDataAllocSize platform newtypes (tnameType nm) of
         Nothing -> pure Nothing
         Just (sz, _) -> Just <$> provideToken nm sz out

bindName :: TName -> Maybe Int -> Output -> Chk Output
bindName nm msize out
  = do newtypes <- getNewtypes
       platform <- getPlatform
       out <- case M.lookup nm (gammaNm out) of
         Nothing -- unused, so available for drop-guided reuse!
           -> do mOut <- tryDropReuse nm out
                 case (msize, mOut) of
                   (Just sz, _) -> provideToken nm sz out
                   (_, Just out) -> pure out
                   (Nothing, Nothing) -> do
                     isHeapValue <- needsDupDrop (tnameType nm)
                     when isHeapValue $
                       requireCapability HasDealloc $ \ppenv -> Just $
                         cat [text "Variable unused: ", ppName ppenv (getName nm)]
                     pure out
         Just n
           -> do isHeapVal <- needsDupDrop (tnameType nm)
                 when (n > 1 && isHeapVal) $
                   requireCapability HasAlloc $ \ppenv -> Just $
                     cat [text "Variable used multiple times: ", ppName ppenv (getName nm)]
                 pure out
       pure (out { gammaNm = M.delete nm (gammaNm out) })

checkOutputEmpty :: Output -> Chk ()
checkOutputEmpty out
  = do case M.maxViewWithKey $ gammaNm out of
         Nothing -> pure ()
         Just ((nm, _), _)
           -> emitWarning $ text $ "FBIP analysis failed as it didn't bind a name: " ++ show nm
       case M.maxViewWithKey $ gammaDia out of
         Just ((sz, c:_), _) | sz > 0
           -> requireCapability HasAlloc $ \ppenv -> Just $
                cat [text "Unreused constructor: ", prettyCon ppenv c sz]
         _ -> pure ()

zipDefault :: a -> [a] -> [b] -> [(a, b)]
zipDefault x [] (b:bs) = (x, b) : zipDefault x [] bs
zipDefault x (a:as) (b:bs) = (a, b) : zipDefault x as bs
zipDefault x _ [] = []

-- value types with reference fields still need a drop
needsDupDrop :: Type -> Chk Bool
needsDupDrop tp
  = do dd <- getDataDef tp
       return $ case dd of
         (DataDefValue vrepr) | valueReprIsRaw vrepr -> False
         _                    -> True

getDataDef :: Type -> Chk DataDef
getDataDef tp
  = do newtypes <- getNewtypes
       return (case getDataDef' newtypes tp of
                 Just dd -> dd
                 Nothing -> DataDefNormal)

getNewtypes :: Chk Newtypes
getNewtypes = newtypes <$> getEnv

getPlatform :: Chk Platform
getPlatform = platform <$> getEnv

-- track the current definition for nicer error messages
withCurrentDef :: Def -> Chk a -> Chk a
withCurrentDef def action
  = -- trace ("chking: " ++ show (defName def)) $
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

traceDoc :: (Pretty.Env -> Doc) -> Chk ()
traceDoc f
  = do env <- getEnv
       chkTrace (show (f (prettyEnv env)))

chkTrace :: String -> Chk ()
chkTrace msg
  = do env <- getEnv
       trace ("chk: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()

emitDoc :: Doc -> Chk ()
emitDoc doc
  = Chk (\env st -> Ok () mempty [doc])

emitWarning :: Doc -> Chk ()
emitWarning doc
  = do names <- currentDefNames
       let fdoc = text (show names) <.> colon <+> doc
       emitDoc fdoc

getConstructorAllocSize :: ConRepr -> Chk Int
getConstructorAllocSize conRepr
  = do platform <- getPlatform
       return (conReprAllocSize platform conRepr)
       
module Core.MatchMerge(matchMergeDefs) where

import qualified Lib.Trace
import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Data.Maybe( catMaybes, isJust, maybeToList, isNothing, fromJust, fromMaybe )
import Lib.PPrint
import Common.Failure
import Common.NamePrim ( nameEffectOpen, namePatternMatchError )
import Common.Name
import Common.Range
import Common.Unique
import Common.Error
import Common.Syntax

import Kind.Kind
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
import Core.Uniquefy

trace s x =
  Lib.Trace.trace s
    x

matchMergeDefs :: CorePhase ()
matchMergeDefs
  = liftCorePhaseUniq $ \uniq defs ->
    runUnique uniq $ matchMergeDefGroups defs

{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}
matchMergeDefGroups :: DefGroups -> Unique DefGroups
matchMergeDefGroups
  = mapM matchMergeDefGroup

matchMergeDefGroup :: DefGroup -> Unique DefGroup
matchMergeDefGroup (DefRec dgs)
  = do ndgs <- mapM matchMergeRecDef dgs
       return $ DefRec ndgs
matchMergeDefGroup (DefNonRec def)
 = do ndef <- matchMergeRecDef def
      return $ DefNonRec ndef

matchMergeRecDef :: Def -> Unique Def
matchMergeRecDef def
  = do e <- rewriteBottomUpM matchMergeExpr $ defExpr def
       return def{defExpr=e}

matchMergeExpr :: Expr -> Unique Expr
matchMergeExpr body
  = case body of
      Case exprs branches ->
        do
          (branches', changed) <- mergeBranches branches
          -- if changed then trace ("matchMergeExpr:\n" ++ show branches ++ "\nrewrote to: \n" ++ show branches' ++ "\n")
          return $ Case exprs branches'
          -- else return $ Case exprs branches
      _ -> return body

mergeBranches :: [Branch] -> Unique ([Branch], Bool)
mergeBranches [] = return ([], False)
mergeBranches branches@(b@(Branch [pat@PatCon{patConPatterns=ps}] _): rst)
  = do
      splitted <- splitBranchConstructors b rst
      case splitted of
        ([b], [], err, pat') -> return ([b], False)
        ([b], rst, err, pat') ->
          do
            (rest, v) <- mergeBranches rst
            return (b:rest, v)
        (bs, rst, err, pat') ->
          do
            let
              vars' = collectVars pat'
              varsMatch =  [Var tn InfoNone | tn <- vars']
            (rest, v) <- mergeBranches rst
            (newBranches, innerV) <- mergeBranches $ map (stripOuterConstructors pat') bs ++ maybeToList (fmap (generalErrorBranch) err) -- Add back error to sub branches
            return (Branch [pat'] [Guard exprTrue (Case varsMatch newBranches)] : rest, True)
mergeBranches (b:bs) = mergeBranches bs >>= (\(bs', v) -> return (b:bs', v))

collectVars :: Pattern -> [TName]
collectVars p
  = case p of
      PatVar name _ -> [name]
      PatCon{patConPatterns = ps} -> concatMap collectVars ps
      _ -> []

-- Split branches into 
-- - a list of those that match
-- - those that are left
-- - a possible (implicit error) branch found 
-- - and the pattern that unifies the matched branches
-- Greedily in order processing, The first branch is the branch under consideration and the others are the next parameter
splitBranchConstructors :: Branch -> [Branch] -> Unique ([Branch], [Branch], Maybe Branch, Pattern)
splitBranchConstructors b@(Branch [p] _) branches =
  case branches of
    -- Only one branch, it matches it's own pattern
    [] -> return ([b], [], if isErrorBranch b then Just b else Nothing, p) 
    b'@(Branch [p'] _):bs ->
      do
        -- First do the rest other than b'
        (bs', bs2', e, accP) <- splitBranchConstructors b bs
        -- keep track of error branch to propagate into sub branches
        let newError = case (e, b') of
              (Just e, _) -> Just e -- implicit error is in the rest of the branches
              (_, b') | isErrorBranch b' -> Just b' -- b' is the error branch
              _ -> Nothing -- no error branch
        -- Acumulated pattern and p'
        patNew <- patternsMatch accP p'
        case patNew of
          -- Restrict the pattern to the smallest that matches multiple branches
          -- Add the new branch to the list of branches that match partially
          Just p | not (isPatWild p) -> return (bs' ++ [b'], bs2', newError, p)
          -- Didn't match the current branch, keep the old pattern
          -- Add the new branch to the list of branches that don't match any subpattern
          _ -> return (bs', b':bs2', newError, accP)

isPatWild :: Pattern -> Bool
isPatWild PatWild = True
isPatWild _ = False

-- Checks to see if the branch is an error branch
isErrorBranch:: Branch -> Bool
isErrorBranch (Branch _ [Guard _ (App (TypeApp (Var name _) _) _)]) = getName name == namePatternMatchError
isErrorBranch _ = False

generalErrorBranch:: Branch -> Branch
generalErrorBranch b@(Branch p g) | isErrorBranch b = Branch [PatWild] g
generalErrorBranch b = b

-- Returns largest common subpattern, with variables added where needed
patternsMatch :: Pattern -> Pattern -> Unique (Maybe Pattern)
patternsMatch p p'
  = case (p, p') of
    (PatLit l1, PatLit l2) -> if l1 == l2 then return (Just p) else newVarName >>= \name -> return $ Just $ PatVar (TName name (typeOf l1)) PatWild
    (PatVar tn1 v1, PatVar tn2 v2) | tn1 == tn2 -> do
      sub <- patternsMatch v1 v2
      case sub of
        Nothing -> return Nothing
        Just sub -> return $ Just $ PatVar tn1 sub
    (PatVar tn1 v1, PatVar tn2 v2) -> do
      name <- newVarName 
      sub <- patternsMatch v1 v2
      case sub of
        Nothing -> return Nothing
        Just sub -> return $ Just $ PatVar (TName name (typeOf tn1)) sub
    (PatWild, PatWild) -> return $ Just PatWild
    (PatCon name1 patterns1 cr targs1 exists1 res1 ci sk, PatCon name2 patterns2 _ targs2 exists2 res2 _ _) -> 
      if 
        name1 == name2 &&
        targs1 == targs2 &&
        exists1 == exists2 &&
        res1 == res2
      then do
        subs <- mapM orVar (zip3 patterns1 patterns2 targs1)
        return $ Just $ PatCon name1 subs cr targs1 exists1 res1 ci sk
      else return Nothing        
    (PatVar tn pat, _) -> do
      name <- newVarName
      return $ Just $ PatVar (TName name (typeOf tn)) PatWild
    (_, PatVar tn pat) -> do
      name <- newVarName
      return $ Just $ PatVar (TName name (typeOf tn)) PatWild
    (_, PatWild) -> return $ Just PatWild
    (PatWild, _) -> return $ Just PatWild
    (_, _) -> return Nothing
    where
      orVar (p1,p2,t) = do
        v <- patternsMatch p1 p2
        case v of
          Nothing -> do
            name <- newVarName
            return $ PatVar (TName name t) PatWild
          Just x -> return x
      newVarName = uniqueId "case" >>= (\id -> return $ newHiddenName ("case" ++ show id))

-- Strip the outer constructors and propagate variable substitution into branch expressions
stripOuterConstructors :: Pattern -> Branch -> Branch
stripOuterConstructors template (Branch [pt] exprs)
  = -- trace ("Using template\n" ++ show template ++ "\nand outer subpattern from\n" ++ show pt ++ "\ngot:\n" ++ show (patNew, replaceMap) ++ "\n") $ 
    Branch (fromMaybe [PatWild] patNew) $ map replaceInGuard exprs
  where
    replaceInPattern :: Pattern -> Pattern
    replaceInPattern p
      = case p of
          PatVar name _ -> case lookup name replaceMap of
            Just (Var name info) -> PatVar name PatWild
            _ -> p
          PatCon name patterns cr targs exists res ci sk -> PatCon name (map replaceInPattern patterns) cr targs exists res ci sk
          _ -> p
    replaceInGuard (Guard tst expr)
      = Guard (rewriteBottomUp replaceInExpr tst) (rewriteBottomUp replaceInExpr expr)
    replaceInExpr :: Expr -> Expr
    replaceInExpr e
      = case e of
          Var name _ -> case lookup name replaceMap of
            Just (Var name info) -> Var name info
            _ -> e
          e' -> e'
    (patNew, replaceMap) = getReplaceMap template pt

-- Get the new pattern that differs from the old pattern and the subsitution map
getReplaceMap :: Pattern -> Pattern -> (Maybe [Pattern], [(TName, Expr)])
getReplaceMap template p'
  = case (template, p') of
    (PatLit l1, PatLit l2) -> (Nothing, [])
    (PatVar tn1 v1, PatVar tn2 v2) -> 
      let (pat', rp) = getReplaceMap v1 v2
      in (pat', (tn2, Var tn1 InfoNone):rp)
    (PatWild, PatWild) -> (Nothing, [])
    (PatCon name1 patterns1 cr targs1 exists1 res1 ci _, PatCon name2 patterns2 _ targs2 exists2 res2 _ sk) -> 
      let res = map (\(p1,p2) -> getReplaceMap p1 p2) (zip patterns1 patterns2)
          (patterns', replaceMaps) = unzip res
          replaceMap = concat replaceMaps
      in (Just (concatMap (\m -> (fromMaybe [PatWild] m)) patterns'), replaceMap)
    (PatVar tn pat, pat2) -> getReplaceMap pat pat2
    (pat, PatVar tn pat2) -> getReplaceMap pat pat2
    (PatWild, pat2) -> (Just [pat2], [])
    _ -> failure $ "getReplaceMap: " ++ show template ++ " " ++ show p' 

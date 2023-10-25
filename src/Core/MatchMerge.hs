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
import Data.List (intercalate)

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

-- Takes a set of branches, and transforms them by merging branches that have some shared superstructure.
-- Returns the new branch structure and whether any changes were made
mergeBranches :: [Branch] -> Unique ([Branch], Bool)
-- No branches, no changes
mergeBranches [] = return ([], False)
-- Branch with constructor pattern, try to merge it with the rest
mergeBranches branches@(b@(Branch [pat@PatCon{patConPatterns=ps}] _): rst)
  = do
      splitted <- splitBranchConstructors b rst
      case splitted of
        -- Single branch, return itself unchanged
        ([b], [], err, pat') -> return ([b], False)
        -- Single branch shares structure, rest do not, merge the rest and append
        ([b], rst, err, pat') ->
          do
            (rest, v) <- mergeBranches rst
            return (b:rest, v)
        -- Multiple branches share structure
        (bs, rst, err, pat') ->
          do
            trace ("mergeBranches:\n" ++ intercalate "\n" (map (show . branchPatterns) bs) ++ "\n with common superstructure:\n" ++ show pat' ++ "\n\n") $ return ()
            let
              vars' = collectPatsVars pat' -- Collect the variables introduced by the shared structure
              varsMatch =  [Var tn InfoNone | tn <- vars'] -- Create expressions for those vars
            -- Get rid of the common superstructure from the branches that share superstructure
            -- Also add the implicit error branch if it exists
              subBranches = map (stripOuterConstructors pat') bs ++ maybeToList err
            (newSubBranches, innerV) <- mergeBranches subBranches 
            (rest, v) <- mergeBranches rst -- Merge the branches that do not share structure with the current set
            -- Replace the set of common branches, with a single branch that matches on the shared superstructure, and delegates
            -- to another case expression to distinguish between the different substructures
            return (Branch pat' [Guard exprTrue (Case varsMatch newSubBranches)] : rest, True)
-- Default (non-constructor patterns), just merge the rest, and add the first branch back
mergeBranches (b:bs) = mergeBranches bs >>= (\(bs', v) -> return (b:bs', v))
-- TODO: Add support for var patterns
-- TODO: Add support for branches with multiple patterns

-- Collects the vars from a pattern in a canonical order (instead of fvs which uses sets)
collectPatsVars :: [Pattern] -> [TName]
collectPatsVars = concatMap collectVars

collectVars :: Pattern -> [TName]
collectVars p
  = case p of
      PatVar name PatWild -> [name]
      PatVar name pt -> collectVars pt 
      PatCon{patConPatterns = ps} -> concatMap collectVars ps
      _ -> []

-- Split branches into 
-- - a list of those that match
-- - those that are left
-- - a possible (implicit error) branch found 
-- - and the pattern that unifies the matched branches
-- Greedily in order processing, The first branch is the branch under consideration and the others are the next parameter
splitBranchConstructors :: Branch -> [Branch] -> Unique ([Branch], [Branch], Maybe Branch, [Pattern])
splitBranchConstructors b@(Branch ps _) branches =
  case branches of
    -- Only one branch, it matches it's own pattern
    [] -> return ([b], [], if isErrorBranch b then Just b else Nothing, ps) 
    b'@(Branch ps' _):bs ->
      do
        -- First do the rest other than b'
        (bs', bs2', e, accP) <- splitBranchConstructors b bs
        -- keep track of error branch to propagate into sub branches
        let newError = case (e, b') of
              (Just e, _) -> Just e -- implicit error is in the rest of the branches
              (_, b') | isErrorBranch b' -> Just b' -- b' is the error branch
              _ -> Nothing -- no error branch
        -- Acumulated pattern and p'
        patNew <- zipWithM patternsMatch accP ps'
        if not $ isSimpleMatches patNew then
          -- Restrict the pattern to the smallest that matches multiple branches
          -- Add the new branch to the list of branches that match partially
          trace ("splitConstructors:\n" ++ show accP ++ "\nand\n" ++ show ps' ++ "\n have common superstructure:\n" ++ show patNew ++ "\n\n")
            $ return (bs' ++ [b'], bs2', newError, patNew)
          -- Didn't match the current branch, keep the old pattern
          -- Add the new branch to the list of branches that don't match any subpattern
        else return (bs', b':bs2', newError, accP)

isPatWild :: Pattern -> Bool
isPatWild PatWild = True
isPatWild _ = False

isSimpleMatches :: [Pattern] -> Bool
isSimpleMatches = all isSimpleMatch

isSimpleMatch :: Pattern -> Bool
isSimpleMatch p =
  case p of
    PatVar _ p -> isSimpleMatch p
    PatWild -> True
    _ -> False

-- Checks to see if the branch is an error branch
isErrorBranch:: Branch -> Bool
isErrorBranch (Branch _ [Guard _ (App (TypeApp (Var name _) _) _)]) = getName name == namePatternMatchError
isErrorBranch _ = False

generalErrorBranch:: Branch -> Branch
generalErrorBranch b@(Branch p g) | isErrorBranch b = Branch [PatWild] g
generalErrorBranch b = b

-- Returns largest common pattern superstructure, with variables added where needed
patternsMatch :: Pattern -> Pattern -> Unique Pattern
patternsMatch p p'
  = case (p, p') of
    (PatLit l1, PatLit l2) -> 
      if l1 == l2 then return p -- Literals that match, just match the literal
      else do -- Match a variable of the literal's type
        name <- newVarName 
        return $ PatVar (TName name (typeOf l1)) PatWild
    (PatVar tn1 v1, PatVar tn2 v2) | tn1 == tn2 -> do 
      -- Same pattern variable, reuse the variable name, but find common substructure 
      sub <- patternsMatch v1 v2
      return $ PatVar tn1 sub
    (PatVar tn1 v1, PatVar tn2 v2) -> do
      -- Variables that don't match name, but (should match types because of type checking)
      -- Create a common name to match for
      name <- newVarName 
      sub <- patternsMatch v1 v2
      return $ PatVar (TName name (typeOf tn1)) sub
    (PatWild, PatWild) -> return PatWild -- Wilds match trivially
    (PatCon name1 patterns1 cr targs1 exists1 res1 ci sk, PatCon name2 patterns2 _ targs2 exists2 res2 _ _) -> 
      if -- Same constructor (name, and types) -- types should match due to type checking, but names could differ
        name1 == name2 &&
        targs1 == targs2 &&
        exists1 == exists2 &&
        res1 == res2
      then do 
        -- Same constructor, match substructure
        subs <- zipWithM patternsMatch patterns1 patterns2
        return $ PatCon name1 subs cr targs1 exists1 res1 ci sk
      else do
        name <- newVarName
        return $ PatVar (TName name res1) PatWild -- Different constructors, no match
    (PatVar tn pat, _) -> do
      sub <- patternsMatch pat p'
      return $ PatVar tn sub
    (_, PatVar tn pat) -> do
      sub <- patternsMatch p pat
      return $ PatVar tn sub
    (_, PatWild) -> return PatWild
    (PatWild, _) -> return PatWild
    (_, _) -> failure $ "patternsMatch: " ++ show p ++ " " ++ show p' ++ " "
    where newVarName = uniqueId "case" >>= (\id -> return $ newHiddenName ("case" ++ show id))

-- Strip the outer constructors and propagate variable substitution into branch expressions
stripOuterConstructors :: [Pattern] -> Branch -> Branch
stripOuterConstructors templates (Branch pts exprs)
  = trace ("Using template\n" ++ show templates ++ "\nand outer subpattern from\n" ++ show pts ++ "\ngot:\n" ++ show (patNew, replaceMap) ++ "\n") $ 
      Branch (concatMap (fromMaybe [PatWild]) patNew) $ map replaceInGuard exprs
  where
    replaceInGuard (Guard tst expr)
      = Guard (rewriteBottomUp replaceInExpr tst) (rewriteBottomUp replaceInExpr expr)
    replaceInExpr :: Expr -> Expr
    replaceInExpr e
      = case e of
          Var name _ -> case lookup name replaceMap of
            Just (Var name info) -> Var name info
            _ -> e
          e' -> e'
    (patNew, replaceMaps) = unzip $ zipWith getReplaceMap templates pts
    replaceMap = concat replaceMaps

-- Get the new pattern that differs from the old pattern and the subsitution map
getReplaceMap :: Pattern -> Pattern -> (Maybe [Pattern], [(TName, Expr)])
getReplaceMap template p'
  = case (template, p') of
    (PatLit l1, PatLit l2) -> (Nothing, [])
    (PatVar tn1 v1, PatVar tn2 v2) | tn1 == tn2 -> 
      let (pat', rp) = getReplaceMap v1 v2
      in (pat', rp)
    (PatVar tn1 v1, PatVar tn2 v2) -> 
      let (pat', rp) = getReplaceMap v1 v2
      in (pat', (tn2, Var tn1 InfoNone):rp)
    (PatWild, PatWild) -> (Nothing, [])
    (PatCon name1 patterns1 cr targs1 exists1 res1 ci _, PatCon name2 patterns2 _ targs2 exists2 res2 _ sk) -> 
      let res = zipWith getReplaceMap patterns1 patterns2
          (patterns', replaceMaps) = unzip res
          replaceMap = concat replaceMaps
      in (Just (concatMap (fromMaybe []) patterns'), replaceMap)
    (PatVar tn PatWild, pat2) -> (Just [pat2], [])
    (PatVar tn pat, pat2) -> getReplaceMap pat pat2
    (pat, PatVar tn pat2) -> getReplaceMap pat pat2
    (PatWild, pat2) -> (Just [pat2], [])
    _ -> failure $ "\ngetReplaceMap:\n" ++ show template ++ "\n:" ++ show p' ++ "\n" 

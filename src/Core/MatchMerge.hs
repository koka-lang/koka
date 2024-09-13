module Core.MatchMerge(matchMergeDefs) where

import Data.Maybe( catMaybes, isJust, maybeToList, isNothing, fromJust, fromMaybe )
import Data.List (intercalate)
import qualified Data.Set as S
import qualified Lib.Trace
import Lib.PPrint
import Common.Failure
import Common.NamePrim ( nameEffectOpen, namePatternMatchError )
import Common.Name
import Common.Range
import Common.Unique
import Type.Type
import qualified Type.Pretty as Pretty
import Core.Core
import Core.Pretty
import Control.Monad (zipWithM)

trace s x =
  Lib.Trace.trace s
    x

matchMergeDefs :: CorePhase b ()
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
  = -- rewrite expressions using matchMergeExpr
    do e <- rewriteBottomUpM matchMergeExpr $ defExpr def
       return def{defExpr=e}

matchMergeExpr :: Expr -> Unique Expr
matchMergeExpr body
  = case body of
      Case exprs branches ->
        do
          (branches', changed) <- mergeBranches branches
          -- if changed then
          --   trace ("matchMergeExpr:\n" ++ show (vcat (map (text . show) branches)) ++ "\nrewrote to: \n" ++ show (vcat (map (text . show) branches')) ++ "\n") (return ())
          -- else return ()
          return $ Case exprs branches'
      _ -> return body

isTrueGuard :: Guard -> Bool
isTrueGuard guard = isExprTrue (guardTest guard)

isPatCon :: Pattern -> Bool
isPatCon (PatCon{}) = True
isPatCon _ = False

-- Takes a set of branches, and transforms them by merging branches that have some shared superstructure.
-- Returns the new branch structure and whether any changes were made
mergeBranches :: [Branch] -> Unique ([Branch], Bool)
-- No branches, no changes
mergeBranches [] = return ([], False)
-- Skip branches with complex guards (in the future we can optimize to merge guards)
mergeBranches (b@(Branch ps guard):bs) | not (all isTrueGuard guard) =
  mergeBranches bs >>= (\(bs', v) -> return (b:bs', v))
-- Branch with constructor pattern, try to merge it with the rest
mergeBranches branches@(b@(Branch ps _): rst) | any isPatCon ps =
  -- trace ("mergeBranches:\n" ++ show b ++ "\n\n" ++ show rst ++ "\n\n\n") $
  do
    splitted <- splitBranchConstructors b rst -- split into common structure and the rest, along with error and any branches
    case splitted of
      -- Single branch, return itself unchanged
      ([b], [], [], tns, pat') -> return ([b], False)
      -- Single branch shares structure, rest do not, merge the rest and append
      ([b], rst, matchAnys, tns, pat') ->
        do
          (rest, v) <- mergeBranches (rst ++ matchAnys)
          return (b : rest, v)
      -- Multiple branches share structure
      (bs, rst, matchAnys, distinguishingVars, pat') ->
        do
          -- trace ("mergeBranches: has error?\n " ++ show (length matchAnys) ++ "\n\n" ++ intercalate "\n" (map (show . branchPatterns) bs) ++ "\n with common superstructure:\n" ++ show pat' ++ "\n\n") $ return ()
          let
            varsMatch = [Var tn InfoNone | tn <- distinguishingVars] -- Create expressions for the vars distinguishing the branches
          -- Get rid of the common superstructure from the branches that share superstructure
          -- Also add wildcard/catch-all branches and implicit error branch if they exists
            subBranches = map (stripOuterConstructors distinguishingVars pat') bs ++ map (makeAnyMatches (length distinguishingVars)) matchAnys
          -- Now recur on the inner branches to merge 
          (newSubBranches, innerV) <- mergeBranches subBranches
          -- Now merge the branches that do not share structure with the current set
          (rest, v) <- mergeBranches (rst ++ matchAnys)
          -- Replace the set of common branches, with a single branch that matches on the shared superstructure, and delegates
          -- to another case expression to distinguish between the different substructures
          return (Branch pat' [Guard exprTrue (Case varsMatch newSubBranches)] : rest, True)
-- Default (non-constructor patterns), just merge the rest, and add the first branch back
mergeBranches (b:bs) = mergeBranches bs >>= (\(bs', v) -> return (b:bs', v))

makeAnyMatches :: Int -> Branch -> Branch
makeAnyMatches n (Branch _ g) =
  Branch [PatWild | _ <- [1..n]] g

-- Wrapper for splitBranchConstructors' that adds back the branch we are currently attempting to merge to the set of branches that match it
splitBranchConstructors :: Branch -> [Branch] -> Unique ([Branch], [Branch], [Branch], [TName], [Pattern])
splitBranchConstructors b branches = do
  res <- splitBranchConstructors' b branches
  case res of -- Add branch back to the front of the list
    (bs, no, anys, tns, pat) -> return (b:bs, no, anys, tns, pat)

-- Split branches into 
-- - a list of those that match (have common structure)
-- - those that are left
-- - (wildcard / match any or implicit error) branches found (which should be propagated into sub branches, as well as stay outside)
-- - The set of variables that discriminate the branches that have common structure
-- - and the pattern that unifies the matched branches
-- Does not change the order of the branches
splitBranchConstructors' :: Branch -> [Branch] -> Unique ([Branch], [Branch], [Branch], [TName], [Pattern])
splitBranchConstructors' b@(Branch ps _) branches =
  -- trace ("splitBranchConstructors:\n" ++ show b ++ "\n\n" ++ show (vcat (map (text . show) branches)) ++ "\n\n") $
  case branches of
    -- Only one branch, it matches it's own pattern
    [] -> return ([], [], [b | isMatchAnyBranch b], [], ps)
    b'@(Branch ps' g):bs ->
      do
        -- First recur on the rest of the branches
        (bs', bs2', matchAnys, discriminators, accP) <- splitBranchConstructors' b bs
         -- Check if the current branch would match any pattern (not discriminating and should be propagated to sub branches)
        if isMatchAnyBranch b' then do
          -- trace ("splitBranchConstructors: Match any branch\n" ++ show b' ++ "\n\n") $ return ()
          return (bs', bs2', b':matchAnys, discriminators, accP)
        else do
          -- Accumulate common pattern (least common superstructure)
          common <- zipWithM (patternsMatch discriminators) accP ps'
          let (newVars, patNews) = unzip common -- split into the discriminating variables and patterns
          let subs = map (stripOuterConstructors (concat newVars) patNews) bs' ++ matchAnys
          -- trace ("Common superstructure of\n" 
          --   ++ show (vcat (text (show accP) : map (text . show) ps')) 
          --   ++ "\nis\n" ++ show (vcat (map (text . show) patNews)) 
          --   ++ "\nwith new discriminators" ++ show newVars ++ "\n") $ return ()
          if all isSimpleMatch patNews || null newVars then
            -- Didn't match the current branch (i.e. returned a trivial common superstructure - just wildcards / vars), keep the old pattern
            -- Add the new branch to the list of branches that don't match any subpattern
            return (bs', b':bs2', matchAnys, discriminators, accP)
          else -- There is some common superstructure
            -- Add the new branch to the list of branches that match partially
            -- trace ("splitConstructors:\n" ++ show accP ++ "\nand\n" ++ show ps' ++ "\n have common superstructure:\n" ++ show patNew ++ "\n\n") $
              return (b':bs', bs2', matchAnys, concat newVars, patNews)

-- Checks to see if a pattern is trivial
isSimpleMatch :: Pattern -> Bool
isSimpleMatch p =
  case p of
    PatVar _ p -> isSimpleMatch p
    PatWild -> True
    _ -> False

-- Checks if a branch just has simple matches
isMatchAnyBranch:: Branch -> Bool
isMatchAnyBranch (Branch pts _) = all isSimpleMatch pts

-- Returns largest common pattern superstructure, with variables added where needed, and the distinguishing variables returned
-- Discriminating differences include:
-- - Different constructors
-- - Different literals
-- - A wildcard on one side but not the other -- if both sides have wildcards, they match trivially
-- - The tricky cases involved named subpatterns: we first see if the subpatterns match
--   - We recur into the named pattern, and then when it is returned we 
--       make sure that we don't double wrap pattern vars in two names 
--       if a name gets introduced due to it's subpattern being a discriminator in the recurrance
--   - While we could reuse names in this case, we would need to make sure we don't reuse names that are in use in the other pattern in some other location
-- - For all discriminating cases, we introduce a new named wildcard, and add the name to the set of distinguishing variables
patternsMatch :: [TName] -> Pattern -> Pattern -> Unique ([TName], Pattern)
patternsMatch distinguishingVars template p' =
  case (template, p') of
    (PatWild, PatWild) -> return ([], PatWild) -- Wilds match trivially
    (PatVar tn PatWild, _) | tn `elem` distinguishingVars -> -- The variable distinguishes another case, we need to keep it as a distinguishing variable
      return ([tn], PatVar tn PatWild)
    (PatVar tn PatWild, PatWild) -> do -- The variable doesn't distinguish, but the common pattern needs to bind the variable name
      newLeafVar (typeOf tn)
    (PatWild, PatVar tn PatWild) -> do -- ditto, but the tn needs to be unique from the bound variables in the template pattern
      newLeafVar (typeOf tn)
    -- Double sided wilds already handled so we can safely request the type, as well as one sided vars
    (_, PatWild) -> do
      newDiscriminator (patternType template)
    (PatWild, _) -> do
      newDiscriminator (patternType p')
    (PatLit l1, PatLit l2) ->
      if l1 == l2 then return ([], template) -- Literals that match, just match the literal
      else do -- Match a variable of the literal's type, add it to discriminating variables
        newDiscriminator (typeOf l1)
    (PatCon name1 patterns1 cr targs1 exists1 res1 ci sk, PatCon name2 patterns2 _ targs2 exists2 res2 _ _) ->
      if -- Same constructor (name, and types) -- types should match due to type checking, but names could differ
        name1 == name2 &&
        targs1 == targs2 &&
        exists1 == exists2 &&
        res1 == res2
      then do
        -- Same constructor, match substructure
        res <- zipWithM recur patterns1 patterns2
        let (subs, pats) = unzip res
        return (concat subs, PatCon name1 pats cr targs1 exists1 res1 ci sk)
      else do -- Different constructors, create a distinguishing variable
        newDiscriminator res1
    (PatVar tn1 v1, PatVar tn2 v2) -> do -- both pattern variables
      recurEnsureVar v1 v2 (typeOf tn1)
    (PatVar tn PatWild, _) -> do -- The variable definitely distinguishes, because it is a wild on one side, and not on the other (otherwise it would have been dealt with prior)
      newDiscriminator (typeOf tn)
    (_, PatVar tn PatWild) -> do -- ditto, but the tn needs to be unique from the bound variables in the template pattern
      newDiscriminator (typeOf tn)
    (PatVar tn pat, _) -> do -- Same as above but recurring on full rhs pattern
      recurEnsureVar pat p' (typeOf tn)
    (_, PatVar tn pat) -> do -- Same as above but recurring on full lhs pattern
      recurEnsureVar template pat (typeOf tn)
    (_, _) -> failure $ "patternsMatch: " ++ show template ++ " " ++ show p' ++ " "
    where
      recur = patternsMatch distinguishingVars
      -- recurs on the left and right patterns, creating a new variable if the new subpattern doesn't already introduce a variable
      recurEnsureVar pl pr tp = do
        -- Note we cannot reuse names from variables because they could be a name representing another location in another pattern, 
        -- which could have been or will be merged into the template at a different leaf, or become part of a subpattern
        (tns, sub) <- recur pl pr
        case sub of
           -- recurring could introduce a new variable which we should prefer over double wrapping the variable
          PatVar tnnew PatWild -> return (tns, sub)
          _ -> newInnerVar tp sub -- Not directly distinguishing
      -- creates a new unique name
      newVarName = uniqueId "case" >>= (\id -> return $ newHiddenName ("case" ++ show id))
      -- creates a new leaf discriminator variable
      newDiscriminator tp = do
        name <- newVarName
        let tn = TName name tp
        return ([tn], PatVar tn PatWild)
      -- creates a new leaf variable that doesn't discriminate patterns
      newLeafVar tp = do
        name <- newVarName
        let tn = TName name tp
        return ([], PatVar tn PatWild)
      -- creates a new inner variable (does not discriminate patterns)
      newInnerVar tp pat = do
        name <- newVarName
        let tn = TName name tp
        return ([], PatVar tn pat)

-- Precondition -- not a wildcard
-- Returns the type of a pattern
patternType :: Pattern -> Type
patternType p = case p of
  PatLit l -> typeOf l
  PatVar tn _ -> typeOf tn
  PatCon tn _ _ targs _ resTp _ _ -> resTp

-- Strip the outer constructors (matching the template)
-- Return a new subbranch involving the same guards but with patterns matching the discriminating variables of the template
-- and propagate name substitutios from the template into branch expressions
stripOuterConstructors :: [TName] -> [Pattern] -> Branch -> Branch
stripOuterConstructors discriminatingVars templates (Branch pts exprs) =
  -- trace ("Using template\n" ++ show templates ++"\n" ++ show pts
  --     ++ "\ngot:\n" ++ show discriminatingVars ++ "\n" ++ show patNew ++ "\n"
  --     ++ show (vcat (map (text . show) (zip discriminatingVars patNew)))
  --     ++ "\nWith variable name mapping:\n" ++ show replaceMap ++ "\n") $
    assertion "Invalid subpattern match " (length patNew == length discriminatingVars) $
    Branch patNew $ map replaceInGuard exprs
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
    (patsNew, replaceMaps) = unzip $ zipWith (getReplaceMap discriminatingVars) templates pts
    patNew = concat patsNew
    replaceMap = concat replaceMaps

-- Get the patterns where the template has holes for discriminating vars 
-- And the subsitution map to replace variables in the body with the new discriminator names
getReplaceMap :: [TName] -> Pattern -> Pattern -> ([Pattern], [(TName, Expr)])
getReplaceMap discriminatingVars template p'
  = -- trace (show template) $
    let recur = getReplaceMap discriminatingVars in
    case (template, p') of
    (PatLit l1, PatLit l2) -> ([], []) -- Must not have been discriminating (otherwise would be a variable)
    (PatWild, PatWild) -> ([], [])
    (PatCon name1 patterns1 cr targs1 exists1 res1 ci _, PatCon name2 patterns2 _ targs2 exists2 res2 _ sk) ->
      let (patterns', replaceMaps) = unzip $ zipWith recur patterns1 patterns2
      in (concat patterns', concat replaceMaps)
    (PatVar tn1 v1, PatVar tn2 v2) -> do
      -- trace (show tn2) $ return ()
      if tn1 `elem` discriminatingVars then ([v2], [(tn2, Var tn1 InfoNone) | tn1 /= tn2]) -- What is left of the pattern is v2 and tn2 should be substituted with tn1
      else
        let (pats', rp) = recur v1 v2
            rp' = if tn1 == tn2 then rp -- no replacement needed since the variables are identical
                  else (tn2, Var tn1 InfoNone):rp in
        case pats' of
          [] -> ([], rp') -- All subpatterns do not discriminate, just update the variable replacement map
          _ -> (pats', rp') -- Some subpatterns differ, propagate those, in addition to the replacement map
    (PatVar tn PatWild, pat2) ->
      -- No substitution needed, and what is left of the pattern is pat2 if it is needed due to being a discriminating variable
      ([pat2 | tn `elem` discriminatingVars], [])
    (PatVar tn pat, pat2) -> recur pat pat2 -- Recur on the subpattern, template added an inner variable we don't care about
    _ -> failure $ "\ngetReplaceMap:\n" ++ show template ++ "\n:" ++ show p' ++ "\n"

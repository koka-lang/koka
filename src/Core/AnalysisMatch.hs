-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Analyse partiality of core match statements
-}
-----------------------------------------------------------------------------

module Core.AnalysisMatch( analyzeBranches
                         ) where


-- import Lib.Trace
import Lib.PPrint( Doc, text )
import Common.Syntax( Target(..) )
import Common.Id
import Common.Name
import Common.Range
import Common.Unique()
import Common.NamePrim( namePatternMatchError, nameSystemCore )
import Kind.Kind( kindStar )
import Type.Type
import Type.Pretty ()
import Core.Core

analyzeBranches :: [Branch] -> Name -> Range -> [DataInfo] -> (Bool,[(Range,Doc)],[Branch])
analyzeBranches branches defName range infos
  = let (exhaustive,warnings)
          = if  any (\info -> dataInfoIsOpen info || dataInfoIsLiteral info) infos
             then -- literal or open type
                  (finalBranchIsCatchAll branches, [])
             else -- datatype match
                  let conNamess = [map conInfoName (dataInfoConstrs info) | info <- infos]  -- list of all constructors per pattern
                      allCases  = cart conNamess       -- all possible combinations
                  in visitBranches range allCases branches   -- visit all branches removing constructors from all combinations
    in (exhaustive, warnings, branches ++ (if exhaustive then [] else catchAll))
  where
    patternCount = length (branchPatterns (head branches))
    resultType   = typeOf (head branches)
    catchAll     = [ Branch (replicate patternCount PatWild)
                         [Guard exprTrue (patternMatchError resultType defName range)]
                   ]

finalBranchIsCatchAll :: [Branch] -> Bool
finalBranchIsCatchAll branches
  = case reverse branches of
      (Branch [pat] [Guard t _]:_) | alwaysMatch pat && isExprTrue t -> True
      _ -> False

noguards :: [Branch] -> Bool -- true if all branches have just one guard that is true
noguards branches 
  = all (\b-> case branchGuards b of
               [Guard t _] -> isExprTrue t
               _           -> False
        ) branches

-- Visit all branches: the `cases` are a list of all possible constructor patterns
visitBranches :: Range -> [[Name]] -> [Branch] -> (Bool {-exhaustive?-}, [(Range, Doc)] {-warnings-})
visitBranches range cases branches
  = case cases of
      -- every case has been matched; skip the rest of the branches
      [] -> let warnings = case branches of
                             [] -> []
                             (branch:_) -> [(range,text "Some branches in the match will never be reached")]
            in (True,warnings)

      -- otherwise
      _  -> case branches of
              [] -> -- no branches, but still cases left: insert a pattern match failure catchall
                    let warnings = {- if noguards
                                    then [(range,text "Not all possible patterns are matched")]
                                    else
                                   -}
                                   -- No warning for unmatched expressions, since we will infer "exn" anyway.
                                   -- [(range,text "Some expressions may not be matched")]
                                   []
                    in ( False, warnings)
              -- match a branch
              (branch@(Branch patterns guards):rest)
                 -- since every guard more complex than 'true', we have no idea if it matches anything
                 | not ( any ( isExprTrue . guardTest ) guards )
                  -> visitBranches range cases rest
                 -- at least one guard is true
                 | otherwise
                  -> let cases' = filter (not . matchPatterns patterns) cases
                     in {-
                        if (length cases' == length cases)
                         then -- nothing matches for sure
                              -- trace ("no matches: " ++ show (branch,cases)) $
                              let warning = (range,text "Some branches in the match will never execute")
                                  (b,ws,bs) = visitBranches cases' rest
                              in (b,warning:ws,bs)
                        else -}
                        visitBranches range cases' rest

-- Do the patterns match the specified constructors?
matchPatterns :: [Pattern] -> [Name] -> Bool
matchPatterns patterns conNames
  = all match (zip patterns conNames)
  where
    match (pattern,conName)
      = case pattern of
          PatWild         -> True
          PatLit lit      -> False
          PatVar _ pat    -> match (pat,conName)
          PatCon tname pats _ _ _ _ info
            -> (getName tname == conName && all alwaysMatch pats)  -- TODO: properly address nested patterns

-- does a pattern always match?
alwaysMatch PatWild               = True
alwaysMatch (PatLit _)            = False
alwaysMatch (PatVar _ pat)        = alwaysMatch pat
alwaysMatch (PatCon _ _ _ _ _ _ info) = conInfoSingleton info
-- alwaysMatch _                  = False

-- cartesion product
cart :: [[a]] -> [[a]]
cart xss
  = case xss of
      []   -> []
      [xs] -> [[x] | x <- xs]
      (ys : yss)
         -> [z : zs | z <- ys, zs <- cart yss]

-- construct a pattern match error
patternMatchError :: Type -> Name -> Range -> Expr
patternMatchError resultType defName range
  = App (TypeApp (Var (TName name tp) (InfoArity 1 2)) [resultType])
            [Lit (LitString (sourceName (posSource (rangeStart range)) ++ show range)), Lit (LitString (show defName))]
  where
    name = namePatternMatchError
    tp   = TForall [a] [] (typeFun [(newName "range",typeString),(newName "def",typeString)] typePartial (TVar a))
    a    = TypeVar (newId 0) kindStar Bound

    info = if (qualifier defName /= nameSystemCore)
            then (InfoArity 1 2)
            else (InfoExternal [(CS,"koka_" ++ aname ++ "<##1>(#1,#2)")
                               ,(JS,aname ++ "(#1,#2)")])
    aname = asciiEncode True (nameModule name) ++ "." ++ asciiEncode False (nameId name)

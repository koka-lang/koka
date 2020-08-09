-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Analyse partiality of core match statements
-}
-----------------------------------------------------------------------------

module Core.AnalysisMatch( analyzeBranches ) where


import Lib.Trace
import Lib.PPrint( Doc, text, pretty )
import Common.Syntax( Target(..) )
import Common.Id
import Common.Name
import Common.Range
import Common.Unique()
import Common.NamePrim( namePatternMatchError, nameSystemCore )
import Kind.Kind( kindStar )
import Kind.Newtypes
import Type.Type
import Type.Pretty 
import Type.TypeVar
import Type.Unify( runUnifyEx, unify )
import Core.Core

analyzeBranches :: Newtypes -> Name -> Range -> [Branch] -> [Type] -> [DataInfo] -> (Bool,[(Range,Doc)],[Branch])
analyzeBranches newtypes defName range branches types infos
  = let (exhaustive,branches',warnings)
          = if  any (\info -> dataInfoIsOpen info || dataInfoIsLiteral info) infos
             then -- literal or open type
                  (finalBranchIsCatchAll branches, branches, [])
             else -- datatype match
                  matchBranches newtypes defName range branches types infos                                    
    in (exhaustive, warnings, branches' ++ (if exhaustive then [] else catchAll))
  where
    patternCount = length (branchPatterns (head branches))
    resultType   = typeOf (head branches)
    catchAll     = [ Branch (replicate patternCount PatWild)
                         [Guard exprTrue (patternMatchError resultType defName range)]
                   ]


data Match = Match{ conInfos   :: [ConInfo],           -- datatype info
                    conMatches :: [(ConInfo,[Match])]  -- matched constructors
                  }
           | MatchComplete
           
isMatchComplete MatchComplete = True
isMatchComplete _ = False           

instance Show Match where
  show (Match cinfos cmatches)  = "match: " ++ show (map conInfoName cinfos) ++ ",\n" ++
                                  unlines [" " ++ show (conInfoName cinfo) ++ ": " ++
                                            unlines ["  " ++ show cmatch | cmatch <- ms] | (cinfo,ms) <- cmatches] 
  show (MatchComplete) = "_"                                   

type Warnings = [(Range,Doc)]


matchBranches :: Newtypes -> Name -> Range -> [Branch] -> [Type] -> [DataInfo] -> (Bool,[Branch],Warnings)
matchBranches newtypes defName range branches types dataInfos
  = let matches = [Match (dataInfoConstrs di) [] | di <- dataInfos]
    in fold (matches,[],[]) branches
  where
    fold (matches,acc,ws) []  
      = -- trace ("** match analyze: " ++ show defName ++ "\n" ++ unlines (map show matches)) $ 
        (all isMatchComplete matches, reverse acc, reverse ws)
    fold (matches,acc,ws) (b:bs)
      = let (matches',b',ws') = matchBranch newtypes defName range matches types b
        in fold (matches', b':acc, ws' ++ ws) bs

matchBranch :: Newtypes -> Name -> Range -> [Match] -> [Type] -> Branch -> ([Match],Branch,Warnings)
matchBranch newtypes defName range matches patTps branch@(Branch patterns guards) | not ( any (isExprTrue . guardTest) guards )
  = -- since every guard more complex than 'true', we have no idea if it matches anything
    (matches,branch,[])

matchBranch newtypes defName range matches patTps branch@(Branch patterns guards) 
  = -- some guard matches for sure; analyze the pattern
    let (matches',patterns',warnings) = matchPatterns newtypes defName range True matches patTps patterns
    in (matches', Branch patterns' guards, warnings)

matchPattern :: Newtypes -> Name -> Range -> Bool -> (Match,Type,Pattern) -> (Match,Pattern,Warnings)
matchPattern newtypes defName range top (MatchComplete, tp, pat)
  = -- already full matched
    let warnings = if top then [(range,text "Some branches in the match will never be reached")] else []
    in (MatchComplete, pat, warnings)
matchPattern newtypes defName range top (match@(Match cinfos cmatches), tp, pat) 
  = case pat of
      PatWild 
        -> let pat' = case (cinfos `remove` map fst (filter (\(_,ms) -> all isMatchComplete ms) cmatches)) of
                        [con] | null (conInfoExists con)
                              -> -- one constructor unmatched: replace wild with the constructor to improve reuse
                                 -- trace ("try replace wild: " ++ show defName ++ ": " ++ show (conInfoName con) ++ ": " ++ show (pretty (conInfoType con))) $
                                 case lookupDataInfo newtypes (conInfoTypeName con) of
                                   Nothing -> PatWild
                                   Just di -> -- trace (" found data: " ++ show (dataInfoName di)) $
                                              case instantiatePatCon tp (conInfoParams con) (conInfoType con) of
                                                Nothing -> PatWild
                                                Just (targs,tres)    -- only for constructors with arguments
                                                  -> -- trace (" success") $
                                                     PatCon (TName (conInfoName con) (conInfoType con))
                                                            [PatWild | _ <- conInfoParams con]
                                                            (getConRepr di con)
                                                            targs [] tres con
                        _ -> PatWild
           in (MatchComplete, pat', [])
      PatVar tname arg 
        -> let (match',pat',warnings) = matchPattern newtypes defName range top (match,typeOf tname,arg)
           in (match', PatVar tname pat', warnings)
      PatLit lit
        -> (match,pat,[])
      PatCon cname args repr targs _ _ cinfo
        -> case span (\(ci,_) -> getName cname /= conInfoName ci) cmatches of
             (pre,(ci,argMatches):post)
               -> -- matched before
                  let (argMatches',args',warnings) = matchPatterns newtypes defName range False argMatches targs args                  
                  in (makeMatch cinfos (pre ++ ((ci,argMatches'):post)), 
                      pat{ patConPatterns = args'}, warnings)
             _ -> -- first match
                  let argMatches = [makeMatch (lookupConInfos newtypes tp) [] | tp <- targs]
                      (argMatches',args',warnings) = matchPatterns newtypes defName range False argMatches targs args                  
                  in (makeMatch cinfos (cmatches ++ [(cinfo,argMatches')]), 
                      pat{ patConPatterns = args'}, warnings)                    
                  
  
matchPatterns :: Newtypes -> Name -> Range -> Bool -> [Match] -> [Type] -> [Pattern] -> ([Match], [Pattern], Warnings)
matchPatterns newtypes defName range top matches tps patterns
    = let (matches',patterns',warningss) = unzip3 $ map (matchPattern newtypes defName range top) (zip3 matches tps patterns)
      in (matches', patterns', concat warningss)
  
makeMatch :: [ConInfo] -> [(ConInfo,[Match])] -> Match
makeMatch cinfos cmatches
  = if (length cinfos == length cmatches && all (\(ci,ms) -> all isMatchComplete ms) cmatches)
     then MatchComplete
     else Match cinfos cmatches

instantiatePatCon :: Type -> [(Name,Type)] -> Scheme -> Maybe ([Type],Type)
instantiatePatCon tpRes [] conTp
  = Just ([],tpRes)
instantiatePatCon tpRes conParams conTp
  = case splitFunScheme conTp of
      Nothing -> Nothing
      Just (tforall,preds,tpars,eff,tres) 
        -> case runUnifyEx 0 (unify tpRes tres) of
             (Right _, sub, _) -> Just ([sub |-> tpar | (_,tpar) <- tpars], sub |-> tres)
             _ -> Nothing


remove :: [ConInfo] -> [ConInfo] -> [ConInfo]
remove cinfos1 cinfos2
  = let cnames = map conInfoName cinfos2
    in filter (\ci -> not (conInfoName ci `elem` cnames)) cinfos1


lookupDataInfo :: Newtypes -> Name -> Maybe DataInfo
lookupDataInfo newtypes tpname
  = newtypesLookupAny tpname newtypes

lookupConInfos :: Newtypes -> Type -> [ConInfo]
lookupConInfos newtypes tp
  = case expandSyn tp of
      TCon tcon -> case lookupDataInfo newtypes (typeconName tcon) of
                     Just di -> dataInfoConstrs di
                     Nothing -> []
      _         -> []


finalBranchIsCatchAll :: [Branch] -> Bool
finalBranchIsCatchAll branches
  = case reverse branches of
      (Branch [pat] [Guard t _]:_) | alwaysMatch pat && isExprTrue t -> True
      _ -> False

-- does a pattern always match?
alwaysMatch PatWild               = True
alwaysMatch (PatLit _)            = False
alwaysMatch (PatVar _ pat)        = alwaysMatch pat
alwaysMatch (PatCon _ _ _ _ _ _ info) = conInfoSingleton info
-- alwaysMatch _                  = False

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

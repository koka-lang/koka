-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-    Analyse partiality of core match statements
-}
-----------------------------------------------------------------------------

module Core.AnalysisMatch( analyzeBranches ) where


import Lib.Trace
import Lib.PPrint
import Common.Syntax( Target(..), JsTarget(..), CTarget(..) )
import Common.Id
import Common.Name
import Common.Range
import Common.Unique(HasUnique)
import Common.NamePrim( namePatternMatchError, nameSystemCore )
import Common.Failure
import Kind.Kind( kindStar, kindEffect )
import Kind.Newtypes
import Type.Type
import Type.Pretty 
import Type.TypeVar
import Type.Unify( runUnifyEx, unify )
import Core.Core
import Core.Core as Core
import Core.Pretty
import Type.Operations (freshTVar)

analyzeBranches :: HasUnique m => Newtypes -> Name -> Range -> [Branch] -> [Type] -> [DataInfo] -> m (Bool,[(Range,Doc)],[Branch])
analyzeBranches newtypes defName range branches types infos
  = let (exhaustive,branches',warnings)
          = matchBranches newtypes defName range branches types infos                                    
    in if exhaustive then 
        return (exhaustive, warnings, branches') 
       else do 
        matchError <- patternMatchError resultType defName range
        return (exhaustive, warnings, branches' ++ catchAll matchError)
  where
    patternCount = length (branchPatterns (head branches))
    resultType   = typeOf (head branches)
    catchAll    x = [ Branch (replicate patternCount PatWild)
                         [Guard exprTrue (x)]
                   ]


data Match = Match{ conInfos   :: ![ConInfo],           -- datatype info
                    conMatches :: ![(ConInfo,[Match])]  -- matched constructors
                  }
           | MatchComplete{ conInfos :: ![ConInfo] }
           
isMatchComplete (MatchComplete _) = True
isMatchComplete _ = False           

isComplete :: (ConInfo,[Match]) -> Bool
isComplete (_,cmatches)  = all isMatchComplete cmatches

instance Show Match where
  show match = show (pretty match)
  
instance Pretty Match where
  pretty (Match cinfos cmatches)  
    = text "match:" <+> list (map (pretty . conInfoName) cinfos)
      <-> (indent 2 $ vcat [pretty (conInfoName cinfo) <.> colon <->
                         (indent 2 (vcat [pretty i <.> dot <+> pretty cmatch | (cmatch,i) <- zip ms [(1::Int)..]])) 
                        | (cinfo,ms) <- cmatches]) 
  pretty (MatchComplete _) = text "<complete>"                                   

type Warnings = [(Range,Doc)]


dataInfoGetConInfos :: DataInfo -> [ConInfo]
dataInfoGetConInfos info
  = -- trace ("data info for: " ++ show (dataInfoName info) ++ ": " ++ show info)$
    if (dataInfoIsOpen info || dataInfoIsLiteral info) 
     then [] 
     else dataInfoConstrs info

matchBranches :: Newtypes -> Name -> Range -> [Branch] -> [Type] -> [DataInfo] -> (Bool,[Branch],Warnings)
matchBranches newtypes defName range branches types dataInfos
  = let matches = [Match (dataInfoGetConInfos di) [] | di <- dataInfos]
    in fold (matches,[],[]) branches
  where
    fold (matches,acc,ws) []  
      = -- trace ("** match analyze: " ++ show defName ++ "\n" ++ unlines (map show matches)) $ 
        (all isMatchComplete matches, reverse acc, reverse ws)
    fold (matches,acc,ws) (b:bs)
      = -- trace ("** match branch: " ++ show defName ++ "\n  branch: " ++ show b ++ "\n" ++ unlines (map show matches)) $ 
        let (matches',b',ws') = matchBranch newtypes defName range matches types b
        in -- trace ("** result branch: " ++ unlines (map show matches')) $
           fold (matches', b':acc, ws' ++ ws) bs

matchBranch :: Newtypes -> Name -> Range -> [Match] -> [Type] -> Branch -> ([Match],Branch,Warnings)
matchBranch newtypes defName range matches patTps branch@(Branch patterns guards) | not ( any (isExprTrue . guardTest) guards )
  = -- since every guard more complex than 'true', we have no idea if it matches anything
    (matches,branch,[])

matchBranch newtypes defName range matches patTps branch@(Branch patterns guards) 
  = -- some guard matches for sure; analyze the pattern
    let (matches',patterns',warnings1) = matchPatterns newtypes defName range True matches patTps patterns
        warnings2 = analyzeGuards range guards
    in (matches', Branch patterns' guards, warnings1 ++ warnings2)


analyzeGuards :: Range -> [Guard] -> Warnings
analyzeGuards range (Guard test expr : guards)  | isExprTrue test && not (null guards)
  = [(range, text "Some guards in the branches will never be reached")] 
analyzeGuards range (Guard test expr : guards)  | isExprFalse test 
  = [(range, text "Some guard condition in the branches is never true")] ++ analyzeGuards range guards
analyzeGuards range (g:gs) = analyzeGuards range gs
analyzeGuards range []     = []

matchPattern :: Newtypes -> Name -> Range -> Bool -> (Match,Type,Pattern) -> (Match,Pattern,Warnings)
matchPattern newtypes defName range top (m@(MatchComplete _), tp, pat)
  = -- already full matched
    let warnings = if top then [(range,text "Some branches in the match will never be reached:" <+> text (show pat))] else []
    in (m, pat, warnings)
matchPattern newtypes defName range top (match@(Match cinfos cmatches), tp, pat) 
  = case pat of
      PatWild 
        -> let pat' = case (cinfos `remove` map fst (filter isComplete cmatches)) of
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
                                                            targs [] tres con True {- skip -}
                        _ -> PatWild
           in (MatchComplete cinfos, pat', [])
      PatVar tname arg 
        -> let (match',pat',warnings) = matchPattern newtypes defName range top (match,typeOf tname,arg)
           in (match', PatVar tname pat', warnings)
      PatLit lit
        -> (match,pat,[])
      PatCon cname args repr targs _ _ cinfo _
        -> case span (\(ci,_) -> getName cname /= conInfoName ci) cmatches of
             (pre,(ci,argMatches):post)
               -> -- matched before
                  let skip = not (null cinfos) && (length cinfos == length cmatches) && all isComplete (pre ++ post)  -- all other constructors matched!
                      (argMatches',args',warnings) = matchPatterns newtypes defName range False argMatches targs args                                        
                      m = makeMatch cinfos (pre ++ ((ci,argMatches'):post))
                  in seq m $
                     (m, pat{ patConPatterns = args', patConSkip = skip }, warnings)
             _ -> -- first match
                  let skip = not (null cinfos) && (length cinfos == length cmatches + 1) && all isComplete cmatches  -- all other constructors matched!
                      argMatches = [makeMatch (lookupConInfos newtypes tp) [] | tp <- targs]
                      (argMatches',args',warnings) = matchPatterns newtypes defName range False argMatches targs args                                        
                      m = makeMatch cinfos (cmatches ++ [(cinfo,argMatches')])
                  in seq m $ 
                     (m, pat{ patConPatterns = args', patConSkip = skip }, warnings)                    
                  
  
matchPatterns :: Newtypes -> Name -> Range -> Bool -> [Match] -> [Type] -> [Pattern] -> ([Match], [Pattern], Warnings)
matchPatterns newtypes defName range top matches tps patterns
    = let (matches1,patterns1,warningss) = unzip3 $ map (matchPattern newtypes defName range top) (zip3 matches tps patterns)
          matches2 = if (length matches1 <= 1) then matches1
                     else case (filter (not . isMatchComplete) matches1) of
                       []  -> -- all matched fully 
                              matches1 
                       [m] -> -- one was matched, while all others were complete matches; info on m is valid
                              updateOneMatch matches matches1
                       _   -> -- multiple matches: discard the info to be conservative
                              matches
      in seq matches2 $
         (matches2, patterns1, concat warningss)
  
updateOneMatch (m1:ms1) (m2:ms2)  | isMatchComplete m2 = m1 : updateOneMatch ms1 ms2
updateOneMatch (m1:ms1) (m2:ms2)  = m2 : ms1
updateOneMatch [] _               = []
updateOneMatch _ _                = failure $ "Core.AnalysisMatch:updateOneMatch: no matching lists" 
  
makeMatch :: [ConInfo] -> [(ConInfo,[Match])] -> Match
makeMatch cinfos cmatches
  = seq cmatches $
    -- trace ("**make match: " ++ show (map conInfoName cinfos) ++ ":\n" ++ show cmatches) $
    if (not (null cinfos) && length cinfos == length cmatches && all isComplete cmatches)
     then MatchComplete cinfos
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
                     Just di -> dataInfoGetConInfos di    -- [] for open or literals
                     Nothing -> trace ("Core.AnalysisMatch.lookupConInfos: not found: " ++ show (typeconName tcon) ++ ": " ++ show newtypes) $
                                []
      TApp t targs -> lookupConInfos newtypes t -- list<a>
      _         -> -- trace ("Core.AnalysisMatch.lookupConInfos: not a tcon: " ++ show (pretty t)) $
                   []


finalBranchIsCatchAll :: [Branch] -> Bool
finalBranchIsCatchAll branches
  = case reverse branches of
      (Branch [pat] [Guard t _]:_) | alwaysMatch pat && isExprTrue t -> True
      _ -> False

-- does a pattern always match?
alwaysMatch PatWild               = True
alwaysMatch (PatLit _)            = False
alwaysMatch (PatVar _ pat)        = alwaysMatch pat
alwaysMatch (PatCon _ _ _ _ _ _ info _) = conInfoSingleton info
-- alwaysMatch _                  = False

-- construct a pattern match error
patternMatchError :: HasUnique m => Type -> Name -> Range -> m Expr
patternMatchError resultType defName range
  = do 
    tv <- freshTVar kindEffect Meta
    let openEff = effectExtends [typePartial] tv
        origTp  = TFun [] effectEmpty resultType
        openTp  = TFun [] openEff resultType
    return $ App ( 
      Core.openEffectExpr exnEff openEff origTp openTp $ 
      Lam [] exnEff $ 
      App (TypeApp (Var (TName name tp) (InfoArity 1 2)) [resultType])
                [Lit (LitString (sourceName (posSource (rangeStart range)) ++ show range)), Lit (LitString (show defName))]
     ) []
  where
    exnEff = effectExtend typePartial effectEmpty
    name = namePatternMatchError
    tp   = TForall [a] [] (typeFun [(newName "range",typeString),(newName "def",typeString)] typePartial (TVar a))
    a    = TypeVar (newId 0) kindStar Bound

    info = if (qualifier defName /= nameSystemCore)
            then (InfoArity 1 2)
            else (InfoExternal [(CS,"koka_" ++ aname ++ "<##1>(#1,#2)")
                               ,(JS JsDefault,aname ++ "(#1,#2)")])
    aname = asciiEncode True (nameModule name) ++ "." ++ asciiEncode False (nameId name)

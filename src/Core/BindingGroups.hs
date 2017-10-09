-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Re-analyze a recursive binding group just after type checking.
    Due to overloading, functions that seem mutually recursive may not
    actually be mutually recursive after the types are resolved.
-}
-----------------------------------------------------------------------------

module Core.BindingGroups( regroup ) where


import qualified Data.Set as S
import qualified Data.Map as M
import Lib.Scc( scc )  -- determine strongly connected components
import Common.Failure(failure)
import Common.Name
import Core.Core
import Core.CoreVar


type Deps = M.Map TName TNames
type FreeVar = TNames

-- | Re-analyze a recursive binding group and return a list of new
-- definition groups. After type checking it is possible that definitions
-- that seemed recursive are actually not mutually recursive.
regroup :: [Def] -> [DefGroup]
regroup [def]
  = let fvdef = fv def in
    if (S.member (TName (defName def) (defType def)) fvdef 
        || defName def `elem` [name | name <- map getName (S.toList fvdef), not (isQualified name)] -- this is for recursive definitions where the type may still differ since we cannot improve too early for mutual recursive definitions (or we unify too eagerly, see "while")
       )
     then [DefRec [def]]
     else -- trace (" not recursive? " ++ show (defName def, defType def, map showTName (S.toList (fv def)))) $
          [DefNonRec def]

regroup defs
  = let defNames = map (\def -> TName (defName def) (defType def)) defs
        defMap   = M.fromList (zip defNames defs)
        deps     = M.fromList (zip defNames (map (S.intersection (S.fromList defNames) . fv) defs))
    
        -- determine strongly connected components
        defOrder = scc [(name,S.toList fvs) | (name,fvs) <- M.toList deps]
        -- create a definition group
        makeGroup names = case names of
                           [name] -> if S.member name (find name deps)
                                      then DefRec [find name defMap]
                                      else DefNonRec (find name defMap)
                           _      -> DefRec [find name defMap | name <- names]
    in -- trace (" new order: " ++ show (map (map showTName) defOrder)) $
       map makeGroup defOrder


find :: TName -> M.Map TName a -> a
find name nameMap
  = case M.lookup name nameMap of
      Just x -> x
      Nothing -> failure ("Core.BindingGroups.find: could not find: " ++ show name)
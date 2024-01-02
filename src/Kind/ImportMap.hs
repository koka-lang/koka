------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    * Change PatVar into PatCon where necessary
-}
-----------------------------------------------------------------------------

module Kind.ImportMap( ImportMap
                       , importsEmpty
                       , importsExtend
                       , importsExpand
                       , importsAlias
                       , importsList
                       ) where

import Prelude hiding (lookup)
import qualified Prelude
import Data.List (lookup, intersperse, isPrefixOf)
import Common.Name
import Lib.Trace(trace)

-- | Maps short module aliases @core@ to full module paths @std/core@.
-- It is represented as a map from a reversed list of module path components to a full name
-- i.e. import my/core = std/core  ->  [(["core","my"], newModuleName "std/core")]
type ImportMap = [([String],Name)]

importsEmpty :: ImportMap
importsEmpty  = []

importsExtend :: Name -> Name -> ImportMap -> Maybe ImportMap
importsExtend aliasName fullName imp
  = let rpath = reverse $ splitModuleName aliasName in
    case lookup rpath imp of
      Nothing -> Just ((rpath,fullName):imp)
      Just fullName1  -> if fullName == fullName1 then Just imp else Nothing

-- | Given a fully qualified name, return the shorter aliased name.
-- For example, with @import f = system/foo@ a name @system/foo/bar@ is shortened to @f/bar@.
importsAlias :: Name -> ImportMap -> Name
importsAlias name imp
  = let mname = qualifier name
    in case filter (\(_,modName) -> modName==mname) imp of
         [(ralias,_)] -> let alias = unsplitModuleName (reverse ralias)
                         in qualify alias (unqualify name)
         _            -> name

importsList :: ImportMap -> [(Name,Name)]
importsList importMap
  = map (\(ralias,modName) -> (unsplitModuleName (reverse ralias),modName)) importMap

-- | @importsExpand name map@ takes a qualified name (@core/int@) and expands
-- it to its real fully qualified name (@std/core/int@). It also returns
-- the declared alias suffix (used to find case-errors).
-- On ambiguity, or if not found at all, it returns Left with a list of candidates.
-- Since declarations can have namespace'd names (@int/eq@) we take
-- the longest prefix that matches an import module.
importsExpand :: Name -> ImportMap -> Either [Name] (Name,Name)
importsExpand name imports
  = let rpath = reverse (splitModuleName (qualifier name))
    in importResolvePath (unqualify name) rpath imports

importResolvePath :: Name -> [String] -> ImportMap -> Either [Name] (Name,Name)
importResolvePath basename [] imports
  = Right (basename, nameNil)  -- unqualified
importResolvePath basename rpath imports
  = case filter (\(ralias,_) -> rpath `isPrefixOf` ralias) imports of
      -- found a unique match
      [(ralias,modName)]
          -> let qname = qualify modName basename
                 alias = unsplitModuleName (reverse (take (length rpath) ralias))
             in -- trace ("kind imports expand: " ++ show name ++ " to " ++ show qname) $
                Right (qname, alias)
      -- no match: assume local qualification (note: rpath cannot be null)
      []  -> case rpath of
              (q:qs) -> case importResolvePath basename qs imports of
                          Right (qname,alias) -> Right (qualifyLocally (newModuleName q) qname, alias)
                          Left err            -> Left err
      -- ambiguous
      amb -> Left (map (unsplitModuleName . reverse . fst) amb)

{-
-- | @importsExpand name map@ takes a qualified name (@core/int@) and expands
-- it to its real fully qualified name (@std/core/int@). It also returns
-- the declared alias suffix (used to find case-errors).
-- On ambiguity, or if not found at all, it returns Left with a list of candidates.
-- Since declarations can have namespace'd names (@int/eq@) we take
-- the longest prefix that matches an import module.
importsExpand :: Name -> ImportMap -> Either [Name] (Name,Name)
importsExpand name imp
  = if isQualified name
     then let rpath = reverse $ splitModuleName (qualifier name)
          in -- trace ("imports expand: " ++ show name ++ ": " ++ show rpath ++ ", in: " ++ show imp) $
             case filter (\(ralias,_) -> isPrefix rpath ralias) imp of
               -- found a match
               [(ralias,fullName)]
                   -> let qname = qualify fullName (unqualify name)
                      in -- trace ("kind imports expand: " ++ show name ++ " to " ++ show qname) $
                         Right (qname, unsplitModuleName (reverse (take (length rpath) ralias)))
               -- no import matches.. probably a locally qualified name?
               []  -> case rpath of
                        [q] ->
                           -- trace ("kind imports qualify locally: " ++ show name) $
                           Right (requalifyLocally name, nameNil)
                        (q:qs)
                          -> -- recursively try a shorter prefix
                             let name2 = qualify (unsplitModuleName (reverse qs)) (unqualify name)
                             in case importsExpand name2 imp of
                                  Right (qname,alias)
                                    -> Right (qualifyLocally (newModuleName q) qname, alias)
                                  other -> leftErr []
                        _ -> leftErr []
               amb -> leftErr amb
     else Right (name,nameNil)
  where
    leftErr amb             = Left (map (unsplitModuleName . reverse . fst) amb)
    prefixes [] = []
    prefixes (x:xs) = (x:xs) : prefixes xs

    isPrefix (x:xs) (y:ys)  = x==y && isPrefix xs ys
    isPrefix [] _           = True
    isPrefix _ _            = False
-}


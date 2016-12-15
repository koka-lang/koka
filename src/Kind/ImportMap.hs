------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
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
import Data.List (lookup, intersperse)
import Common.Name

-- | Maps short module aliases @core@ to full module paths @std/core@.
-- It is represented as a map from a reversed list of module path components to a full name
-- i.e. import my/core = std/core  ->  [(["core","my"], "std/core")]
type ImportMap = [([Name],Name)]

importsEmpty :: ImportMap
importsEmpty  = []

importsExtend :: Name -> Name -> ImportMap -> Maybe ImportMap
importsExtend name fullName imp
  = let rpath = reverse $ splitModuleName name in
    case lookup rpath imp of
      Nothing -> Just ((rpath,fullName):imp)
      Just _  -> Nothing

-- | @importsExpand name map@ takes a qualified name (@core/int@) and expands
-- it to its real fully qualified name (@std/core/int@). It also returns
-- the declared alias suffix (used to find case-errors). 
-- On ambiguity, or if not found at all, it returns Left with a list of candidates.
importsExpand :: Name -> ImportMap -> Either [Name] (Name,Name)
importsExpand name imp  
  = if isQualified name 
     then let rpath = reverse $ splitModuleName (qualifier name)
          in case filter (\(ralias,_) -> isPrefix rpath ralias) imp of
               [(ralias,fullName)] 
                   -> Right (qualify fullName (unqualify name), 
                               unsplitModuleName (reverse (take (length rpath) ralias)))
               amb -> Left (map (unsplitModuleName . reverse . fst) amb)
     else Right (name,nameNil)
  where
    isPrefix (x:xs) (y:ys)  = x==y && isPrefix xs ys
    isPrefix [] _           = True
    isPrefix _ _            = False

-- | Given a fully qualified name, return the shorter aliased name.
-- For example, with @import System.Foo as F@ a name @System.Foo.bar@ is shortened to @F.bar@.
importsAlias :: Name -> ImportMap -> Name
importsAlias name imp
  = let mname = if (isQualified name) then qualifier name else name
    in case filter (\(_,fullname) -> fullname==mname) imp of
         [(ralias,_)] -> let alias = unsplitModuleName (reverse ralias)
                         in if (isQualified name) then qualify alias (unqualify name) else alias
         _            -> name

importsList :: ImportMap -> [(Name,Name)]
importsList importMap
  = map (\(ralias,fullname) -> (unsplitModuleName (reverse ralias),fullname)) importMap

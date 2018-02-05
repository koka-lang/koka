-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    User defined names (just 'String's).
-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Common.Name
          ( Name, Names     -- instance Eq Ord Show
          , showName        -- show with quotes
          , newName, newQualified
          , nameNil, nameIsNil
          , nameCaseEqual, nameCaseOverlap, isSameNamespace
          , qualify, unqualify, isQualified, qualifier
          , nameId, nameModule
          
          , newFieldName, isFieldName, isWildcard
          , newHiddenExternalName
          , newHiddenName, isHiddenName
          , makeHiddenName
          , newImplicitTypeVarName, isImplicitTypeVarName
          , newCreatorName
          , toOperationsName, fromOperationsName
          , toOpsConName, toOpConName, toOpTypeName
          , toConstructorName, isConstructorName, toVarName
          , toOpenTagName, isOpenTagName
          , splitModuleName, unsplitModuleName
          
          , prepend, postpend
          , asciiEncode, showHex, moduleNameToPath
          ) where

import Lib.Trace( trace )
import Lib.PPrint (Pretty(pretty), text )
import Data.Char(isUpper,toLower,toUpper,isAlphaNum,isDigit,isAlpha)
import Common.Failure(failure)
import Common.File( joinPaths, splitOn, endsWith, startsWith )
import Data.List(intersperse)
----------------------------------------------------------------
-- Names
----------------------------------------------------------------
type Names = [Name]

-- | Names defined by the user.
-- Uses a hash to speed up comparisions. The hash is constructed
-- such that they can be compared too. (h1 > h2 => name1 > name2)
-- The hash is case-insensitive, just like comparisions on names.
-- Use 'nameCaseEqual' for case-sensitive comparisions.
data Name  = Name 
             { nameModule :: !String
             , hashModule :: !Int
             , nameId     :: !String
             , hashId     :: !Int
             }

nameCaseEqual name1 name2 -- (Name m1 _ n1 _) (Name m2 _ n2 _)
  = nameId name1 == nameId name2
    &&
    and (zipWith (==) (reverse (splitModuleName name1)) (reverse (splitModuleName name2)))
    -- (m1 == m2) && (n1 == n2) 

nameCaseOverlap :: Name -> Name -> Bool
nameCaseOverlap name1 name2 
  = (not (nameCaseEqual name1 name2)) && (isSameNamespace name1 name2)  

-- Checks whether both names are in the same namespace, ie. constructors or not
isSameNamespace name1 name2  
  = case (nameId name1, nameId name2) of
      (c:cs, d:ds) -> (isUpper c == isUpper d)
      _            -> True

lowerCompare (Name m1 _ n1 _) (Name m2 _ n2 _)
  = case lowerCompareS m1 m2 of
      EQ -> lowerCompareS n1 n2
      lg -> lg
  where
    lowerCompareS (c:cs) (d:ds)  
      = case compare (toLower c) (toLower d) of
          EQ -> lowerCompareS cs ds
          lg -> lg
    lowerCompareS (c:cs) [] = GT
    lowerCompareS [] (d:ds) = LT
    lowerCompareS [] []     = EQ

instance Eq Name where
  n1@(Name _ hm1 _ hn1) == n2@(Name _ hm2 _ hn2)
    = (hn1 == hn2) && (hm1 == hm2) && (lowerCompare n1 n2 == EQ)
      
instance Ord Name where
  compare n1@(Name _ hm1 _ hn1) n2@(Name _ hm2 _ hn2)
    = case compare hm1 hm2 of
        EQ -> case compare hn1 hn2 of
                EQ -> lowerCompare n1 n2
                lg -> lg
        lg -> lg
      
instance Show Name where
  show (Name m _ n _) 
    = if null m 
       then n
       else m ++ "/" ++ case n of
                          (c:cs) | not (isAlpha c || c=='_' || c=='(') -> "(" ++ n ++ ")"
                          _      -> n

instance Pretty Name where
  pretty name
    = text (show name)

-- | Show quotes around the name
showName :: Name -> String
showName name
  = show (show name)

newName :: String -> Name
newName s
  = newQualified "" s


newQualified :: String -> String -> Name
newQualified m n
  = Name m (hash m) n (hash n)
  where
    -- The hash function:
    --  1) can be compared: h1 < h2  => name1 < name2 && h1 > h2 => name1 > name2
    --  2) assumes 32 bit integers and no characters in strings >= \x128
    --  3) is case in-sensitive (ie. does tolower first)
    -- The hash is done taking the first 4 characters. This is of course a 
    -- terrible hash but we use it mostly to speed up *comparisions* for the NameMap
    hash :: String -> Int
    hash s = foldl (\h c -> h*256 + fromEnum c) 0 (map toLower (take 4 s))


nameNil :: Name
nameNil
  = newName ""

nameIsNil :: Name -> Bool
nameIsNil (Name m _ n _)
  = null n

qualify :: Name -> Name -> Name
qualify (Name x _ m hm) (Name y _ n hn) | null x && null y = Name m hm n hn
qualify (Name x _ m hm) (Name y _ n hn) | null x && m == y = Name m hm n hn
qualify n1 n2
  = failure ("Common.Name.qualify: Cannot use qualify on qualified names: " ++ show (n1,n2))

unqualify :: Name -> Name
unqualify (Name _ _ n hn)
  = Name "" 0 n hn

isQualified :: Name -> Bool
isQualified (Name m _ _ _)
  = not (null m)

qualifier :: Name -> Name
qualifier (Name m hm _ _) 
  = Name "" 0 m hm


----------------------------------------------------------------
-- Modules paths
----------------------------------------------------------------
splitModuleName :: Name -> [Name]
splitModuleName name
  = if (isQualified name) then splitModuleName (qualifier name)
     else map newName $ splitOn (=='/') (show name) 

unsplitModuleName :: [Name] -> Name
unsplitModuleName xs
  = newName (concat (intersperse "/" (map show xs)))


----------------------------------------------------------------
-- wildcards & constructors
----------------------------------------------------------------
isWildcard name
  = case nameId name of
      ('_':_) -> True
      _       -> False

isConstructorName name
  = case nameId name of
      ('.':c:cs) -> isUpper c || c == '('
      (c:cs)     -> isUpper c || c == '('
      _          -> False

toConstructorName name
  = newQualified (nameModule name) $
    case nameId name of
      ('.':c:cs) -> '.':toUpper c : cs  -- keep hidden names hidden
      (c:cs)     -> toUpper c : cs
      ""         -> ""

toVarName name
  = newQualified (nameModule name) $
    case nameId name of
      ('.':cs)   -> '.':toLowers cs  -- keep hidden names hidden
      cs         -> toLowers cs
  where
    toLowers s  -- while uppercase, map toLower
      = case s of
          (c:cs) | isUpper c -> toLower c : toLowers cs
          _      -> s


----------------------------------------------------------------
-- various special names
----------------------------------------------------------------

newHiddenName s
  = newName ("." ++ s)

isHiddenName name
  = case nameId name of
      ('.':_) -> True
      _       -> False

makeHiddenName s name
  = prepend ("." ++ s ++ "-") name

newFieldName i
  = newHiddenName ("field" ++ show i)

isFieldName name
  = isHiddenName name


newImplicitTypeVarName i
  = newHiddenName ("t" ++ show i)

isImplicitTypeVarName name
  = isHiddenName name


newHiddenExternalName name
  = makeHiddenName "extern" name


-- | Create a constructor creator name from the constructor name.
-- Used if special creation functions are used for the constructor.
-- in particular for the case of optional arguments.
newCreatorName :: Name -> Name
newCreatorName name
  = makeHiddenName "create" name


-- | Create an operations type name from an effect type name.
toOperationsName :: Name -> Name
toOperationsName name
  = makeHiddenName "ops" name
  
-- | Create an effect type name from an operations type name.
fromOperationsName :: Name -> Name
fromOperationsName name
  = newQualified (nameModule name) (drop 5 (nameId name))

-- | Create an operation type name from an operation name.
toOpTypeName :: Name -> Name
toOpTypeName name
  = makeHiddenName "op" name
  
-- | Create an operation constructor name from an operation name.
toOpConName :: Name -> Name
toOpConName name
  = makeHiddenName "Op" name
  
-- | Create an operations operation constructor.
toOpsConName :: Name -> Name
toOpsConName name
  = makeHiddenName "Ops" name

-- | Create an open tag name from a constructor name in an open type
toOpenTagName :: Name -> Name
toOpenTagName name
  = makeHiddenName "tag" name  

isOpenTagName :: Name -> Bool
isOpenTagName name
  = nameId name `startsWith` ".tag-"

prepend :: String -> Name -> Name
prepend s name
  = newQualified (nameModule name) 
    (case nameId name of 
      ('.':t) -> case s of
                   '.':_ -> s ++ t  -- keep hidden names hidden
                   _     -> '.' : s ++ t
      t       -> s ++ t
    )

postpend :: String -> Name -> Name
postpend s name
  = newQualified (nameModule name) (nameId name ++ s)

----------------------------------------------------------------
-- camel-case to dash-case
----------------------------------------------------------------
camelToDash :: String -> String
camelToDash s
  = case splitCamel s of
      (x:xs) -> x ++ concatMap (\y -> '-' : map toLower y) xs
      _      -> ""

splitCamel :: String -> [String]
splitCamel ""  = []
splitCamel ('-':cs) = splitCamel cs
splitCamel (c:cs)
  = let (pre,post) = span (not . isBreak) cs
    in if null pre
        then let (pre2,post2) = span isUpper post
             in if (null pre2 || (not (null post2) && isBreak (head post2)))
                 then (c:pre2) : splitCamel post2
                 else (c:init pre2) : splitCamel (last pre2 : post2)
        else (c:pre) : splitCamel post 
  where
    isBreak c = isUpper c || c=='-' 

----------------------------------------------------------------
-- name to file path
----------------------------------------------------------------
moduleNameToPath :: Name -> FilePath
moduleNameToPath name
  = asciiEncode True (show name)

{---------------------------------------------------------------
  Ascii encode a name
  - on module names  '/' becomes '_'
  - on normal names '-' becomes '_' 
---------------------------------------------------------------}
asciiEncode :: Bool -> String -> String
asciiEncode isModule name
  = case name of
      (c:cs)  | isAlphaNum c -> encodeChars name
      ""      -> "_null_"       
      ".<>"   -> "_Total_"
      ".<|>"  -> "_Extend_"
      ".()"   -> "_Unit_"
      ".(,)"  -> "_Tuple2_"
      ".(,,)" -> "_Tuple3_"
      ".(,,,)"-> "_Tuple4_"
      "()"    -> "_unit_"
      "(,)"   -> "_tuple2_"
      "(,,)"  -> "_tuple3_"
      "(,,,)" -> "_tuple4_"
      "[]"    -> "_index_"
      -- '.':'c':'o':'n':' ':cs -> trace ("con name: " ++ name) $ "_con_" ++ encodeChars cs
      -- '.':'t':'y':'p':'e':' ':cs -> "_type_" ++ encodeChars cs
      _       -> encodeChars name
  where
    encodeChars s
      = concat (zipWith3 encodeChar (' ':s) s (tail (s ++ " ")))

    encodeChar :: Char -> Char -> Char -> String
    encodeChar pre c post | isAlphaNum c  = [c]
    encodeChar pre c post
      = case c of
          '/' | isModule -> "_"
          '-' | not isModule && isAlphaNum post -> "_"
          '.' | isDigit post || post == ' ' || pre == ' ' -> "_"

          '_' -> "__"
          '.' -> "_dot_"
          '-' -> "_dash_"   
          '/' -> "_fs_"
          
          '+' -> "_plus_"
          '*' -> "_star_"
          '&' -> "_amp_"
          '~' -> "_tilde_"
          '!' -> "_excl_"
          '@' -> "_at_"
          '#' -> "_hash_"
          '$' -> "_dollar_"
          '%' -> "_perc_"
          '^' -> "_hat_"
          '=' -> "_eq_"
          ':' -> "_colon_"
          '<' -> "_lt_"
          '>' -> "_gt_"
          '[' -> "_lb_"
          ']' -> "_rb_"
          '?' -> "_ques_"
          '\\'-> "_bs_"
          '(' -> "_lp_"
          ')' -> "_rp_"
          ',' -> "_comma_"
          ' ' -> "_space_"
          '\'' -> "_sq_"
          '\"' -> "_dq_"
          '`'  -> "_bq_"
          '{'  -> "_lc_"
          '}'  -> "_rc_"
          '|'  -> "_bar_"
            
          _   -> "_x" ++ showHex 2 (fromEnum c) ++ "_"



showHex :: Int -> Int -> String
showHex len i
  = let hexs = map showHexChar (reverse (hexDigits i))
    in replicate (len - length hexs) '0' ++ hexs
  where
    showHexChar :: Int -> Char
    showHexChar d  | d <= 9    = toEnum (d + fromEnum '0')
                   | otherwise = toEnum (d - 10 + fromEnum 'A')  

    hexDigits :: Int -> [Int]
    hexDigits i
      = let (d,m) = i `divMod` 16
        in if d == 0 then [m]
                     else m : hexDigits d


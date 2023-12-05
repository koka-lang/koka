-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    User defined names (just 'String's).
-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Common.Name
          ( Name, Names     -- instance Eq Ord Show
          , showName        -- show with quotes
          , showPlain
          , showTupled, readTupled -- show and read back reliably
          , readQualified
          , labelNameCompare
          , toHiddenUniqueName
          , newName, newQualified
          , nameNil, nameIsNil
          , nameCaseEqual, nameCaseOverlap, isSameNamespace
          , qualify, unqualify, isQualified, qualifier
          , nameId, nameModule

          , newPaddingName, isPaddingName, isCCtxName
          , newFieldName, isFieldName, isWildcard
          , newHiddenExternalName, isHiddenExternalName
          , newHiddenName, isHiddenName, hiddenNameStartsWith
          , makeHiddenName, makeFreshHiddenName
          , toUniqueName
          , newImplicitTypeVarName, isImplicitTypeVarName
          , newCreatorName
          , toHandlerName, fromHandlerName, isHandlerName
          , toOpSelectorName, fromOpSelectorName, isOpSelectorName
          , toOperationsName, fromOperationsName, isOperationsName
          , toEffectTagName
          , toHandleName, isHandleName
          , toOpsConName, toOpConName, toOpTypeName
          , toConstructorName, isConstructorName, toVarName
          , toOpenTagName, isOpenTagName
          , toValueOperationName, isValueOperationName, fromValueOperationsName
          , splitModuleName, unsplitModuleName, mergeCommonPath
          , isEarlyBindName

          , prepend, postpend
          , asciiEncode, showHex, moduleNameToPath, pathToModuleName
          , canonicalSep, canonicalName, nonCanonicalName, canonicalSplit
          ) where

import Lib.Trace( trace )
import Lib.PPrint (Pretty(pretty), text )
import Data.Char(isUpper,toLower,toUpper,isAlphaNum,isDigit,isAlpha)
import Common.Failure(failure)
import Common.File( joinPaths, splitOn, endsWith, startsWith, isPathSep )
import Common.Range( rangeStart, posLine, posColumn )
import Data.List(intersperse)


isEarlyBindName name
  = isHandleName name -- || nameId name `startsWith` "clause-" || hiddenNameStartsWith name "tag"

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

-- Effects compare by name first, then by module name for efficiency at runtime
labelNameCompare nm1@(Name m1 hm1 n1 hn1) nm2@(Name m2 hm2 n2 hn2)
  = case compare hn1 hn2 of
      EQ -> case lowerCompareS n1 n2 of
              EQ -> case compare hm1 hm2 of
                      EQ -> lowerCompareS m1 m2
                      lg -> lg
              lg -> lg
      lg -> lg


canonicalSep = '.'

instance Show Name where
  show (Name m _ n _)
   = let (mid,post) = case (span isDigit (reverse n)) of
                        (postfix, c:rest) | c == canonicalSep && not (null postfix)
                           -> (reverse rest, c:reverse postfix)
                        _  -> (n,"")
         pre        = if null m then "" else m ++ "/"
     in pre ++ case mid of
                  (c:cs) -- | any (\c -> c `elem` ".([])") mid    -> "(" ++ n ++ ")"
                         | not (isAlphaNum c || c=='_' || c=='(' || c== '.') -> "(" ++ n ++ ")"
                  _      -> n


showPlain (Name m _ n _)
  = (if null m then "" else m ++ "/") ++ n

instance Pretty Name where
  pretty name
    = text (show name)

showTupled (Name m _ n _)
  = show (m,n)

readTupled s
  = let (m,n) = ((read s) :: (String,String))
    in newQualified m n

readQualified s
  = if (take 1 s == "(")
     then readTupled s
     else let (n,m) = span (/='/') (reverse s)
          in newQualified (reverse (drop 1 m)) (reverse n)


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
    hash s = foldl (\h c -> h*256 + fromEnum c) 0 (map toLower (take 4 (s ++ "\0\0\0\0")))


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

mergeCommonPath :: Name -> Name -> Name
mergeCommonPath mname name
  = let ns = splitModuleName name
        ms = splitModuleName mname
        new = unsplitModuleName (merge ms ns)
    in -- trace( "merge common: " ++ show (mname,name) ++ " -> " ++ show new) $
       new
  where
    merge (m:ms) (n:ns) | m==n && and (zipWith (==) ms ns) = (m:ms) ++ (drop (length ms) ns)
    merge (m:ms) ns     = m : merge ms ns
    merge [] ns         = ns


----------------------------------------------------------------
-- wildcards & constructors
----------------------------------------------------------------
isWildcard name
  = case nameId name of
      ('_':_)     -> True
      ('.':'_':_) -> True
      _           -> False

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
  = case nameId xname of
      c:cs | not (isAlpha c || c `elem` "()[]") -> newQualified (nameModule xname) ("." ++ s ++ asciiEncode False ('-':c:cs)) -- hidden operator
      _    -> prepend ("." ++ s ++ "-") xname
    where
      xname = case nameId name of
                '.':cs -> newQualified (nameModule name) cs
                s      -> name

makeFreshHiddenName s name range
  = makeHiddenName s (postpend (idFromPos (rangeStart range)) name)
    where idFromPos pos = "-l" ++ show (posLine pos) ++ "-c" ++ show (posColumn pos)

hiddenNameStartsWith name pre
  = nameId name `startsWith` ("." ++ pre ++ "-")

toUniqueName :: Int -> Name -> Name
toUniqueName i name
  = newQualified (nameModule name) $
    reverse (insert (reverse (nameId name)))
  where
    insert (c:cs) | c `elem` "'?" = c : insert cs
    insert cs     = reverse (show i) ++ cs

toHiddenUniqueName :: Int -> String -> Name -> Name
toHiddenUniqueName i "" name
  = prepend "." (toUniqueName i name)
toHiddenUniqueName i s name  
  = makeHiddenName (s ++ show i) xname
  where
    c = (head (nameId name))
    xname = if (isAlpha c || c=='.' ) then name else newQualified (nameModule name) ("op")


newPaddingName i
  = newHiddenName ("padding" ++ show i)

isPaddingName name
  = -- hiddenNameStartsWith name "padding"
    nameId name `startsWith` (".padding")

isCCtxName name
  = -- hiddenNameStartsWith name "padding"
    nameId name `startsWith` (".cctx")


newFieldName i
  = newHiddenName ("field" ++ show i)

isFieldName name
  = isHiddenName name -- hiddenNameStartsWith name "field"


newImplicitTypeVarName i
  = newHiddenName ("t" ++ show i)

isImplicitTypeVarName name 
  = isHiddenName name


newHiddenExternalName name
  = makeHiddenName "extern" name

isHiddenExternalName name
  = hiddenNameStartsWith name "extern"


-- | Create a constructor creator name from the constructor name.
-- Used if special creation functions are used for the constructor.
-- in particular for the case of optional arguments.
newCreatorName :: Name -> Name
newCreatorName name
  = makeHiddenName "create" name

-- | Create a handler type name from an effect type name.
toHandlerName :: Name -> Name
toHandlerName name
  = makeHiddenName "hnd" name

isHandlerName :: Name -> Bool
isHandlerName name
  = nameId name `startsWith` ".hnd-"

-- | Create an effect type name from an operations type name.
fromHandlerName :: Name -> Name
fromHandlerName name
  = newQualified (nameModule name) (drop 5 (nameId name))

-- | Create a handle function name from an effect type name.
toHandleName :: Name -> Name
toHandleName name
  = makeHiddenName "handle" name

isHandleName :: Name -> Bool
isHandleName name
  = hiddenNameStartsWith name "handle"


-- | Create an operations type name from an effect type name.
toOperationsName :: Name -> Name
toOperationsName name
  = makeHiddenName "ops" name

-- | Is this an operations name?
isOperationsName :: Name -> Bool
isOperationsName name
  = nameId name `startsWith` ".ops-"

-- | Create an effect type name from an operations type name.
fromOperationsName :: Name -> Name
fromOperationsName name
  = newQualified (nameModule name) (drop 5 (nameId name))

-- | Create an operations type name from an effect type name.
toOpSelectorName :: Name -> Name
toOpSelectorName name
  = makeHiddenName "select" name

-- | Is this an operations name?
isOpSelectorName :: Name -> Bool
isOpSelectorName name
  = nameId name `startsWith` ".select-"

-- | Create an effect type name from an operations type name.
fromOpSelectorName :: Name -> Name
fromOpSelectorName name
  = newQualified (nameModule name) (drop 8 (nameId name))

-- | Create an effect tag name from an effect type name.
toEffectTagName :: Name -> Name
toEffectTagName name
  = makeHiddenName "tag" name


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

-- | Create a name for a value operation
toValueOperationName :: Name -> Name
toValueOperationName name
  = makeHiddenName "val" name

-- | Is this an name of a value operation?
isValueOperationName :: Name -> Bool
isValueOperationName name
  = nameId name `startsWith` ".val-"

-- | Create an operation name from a value operation name
fromValueOperationsName :: Name -> Name
fromValueOperationsName name
  = newQualified (nameModule name) (drop 5 (nameId name))

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
postpend s cname
  = let (name,post) = canonicalSplit cname
    in newQualified (nameModule name) (nameId name ++ s ++ post)



canonicalName :: Int -> Name -> Name
canonicalName n name
  = if (n==0) then name
    else postpend (canonicalSep : show n) name
    {-
         case (span isDigit (reverse (nameId name))) of
           (postfix, c:rest) | c == canonicalSep && not (null postfix) -> (newQualified (nameModule name) (reverse rest))
           _ -> name -}

nonCanonicalName :: Name -> Name
nonCanonicalName name
  = fst (canonicalSplit name)

canonicalSplit :: Name -> (Name,String)
canonicalSplit name
  = case (span isDigit (reverse (nameId name))) of
      (postfix, c:rest) | c == canonicalSep && not (null postfix) -> (newQualified (nameModule name) (reverse rest), c:reverse postfix)
      _        -> (name,"")


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


pathToModuleName :: FilePath -> Name
pathToModuleName path
  = newName $ dropWhile (\c -> c `elem` "_./") $ 
    decode $ 
    map (\c -> if isPathSep c then '/' else c) $
    path
  where
    -- TODO: do proper decoding
    decode s
      = case s of
          _ | s `startsWith` "_dash_" -> '-':decode (drop 6 s)
          ('_':'_':cs) -> '_':decode cs          
          ('_':cs)     -> '/':decode cs
          ('.':cs)     -> decode cs
          ('\\':cs)    -> '/':decode cs
          (c:cs)       -> c:decode cs
          []           -> ""




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
      ".<>"   -> "_total_"
      ".<|>"  -> "_extend_"
      ".()"   -> "_unit_"
      ".(,)"  -> "_tuple2_"
      ".(,,)" -> "_tuple3_"
      ".(,,,)"-> "_tuple4_"
      "()"    -> "_Unit_"
      "(,)"   -> "_Tuple2_"
      "(,,)"  -> "_Tuple3_"
      "(,,,)" -> "_Tuple4_"
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

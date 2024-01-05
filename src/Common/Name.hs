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
          -- , showName        -- show with quotes
          , showPlain
          , showTupled, readTupled -- show and read back reliably
          , readQualified, readQualifiedName
          , labelNameCompare
          , toHiddenUniqueName
          , newName, newModuleName, newQualified, newLocallyQualified, nameAsModuleName
          , nameNil, nameIsNil, nameStartsWith
          , nameCaseEqual, nameCaseOverlap, isSameNamespace
          , nameCaseEqualPrefixOf, nameCaseOverlapPrefixOf
          , qualify, unqualify, isQualified, qualifier
          , nameModule, nameStem, nameLocal, nameLocalQual

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
          , splitModuleName, unsplitModuleName, mergeCommonPath, splitLocalQualName
          , missingQualifier
          , isEarlyBindName
          , toImplicitParamName, isImplicitParamName, plainImplicitParamName
          , namedImplicitParamName, splitImplicitParamName

          , prepend, postpend
          , asciiEncode, showHex, moduleNameToPath, pathToModuleName
          -- , canonicalSep, canonicalName, nonCanonicalName, canonicalSplit

          , prettyName, prettyCoreName
          , requalifyLocally, qualifyLocally, unqualifyFull, isLocallyQualified, fullQualifier
          , unqualifyAsModuleName
          ) where

-- import Lib.Trace( trace )
-- import Debug.Trace
import Lib.PPrint
import Data.Char(isUpper,toLower,toUpper,isAlphaNum,isDigit,isAlpha)
import Common.Failure(failure)
import Common.File( joinPaths, splitOn, endsWith, startsWith, isPathSep )
import Common.Range( rangeStart, posLine, posColumn )
import Data.List(intersperse,isPrefixOf)
import Common.ColorScheme

isEarlyBindName name
  = isHandleName name || isCreatorName name

----------------------------------------------------------------
-- Names
----------------------------------------------------------------
type Names = [Name]

-- | Names defined by the user.
-- Uses a hash to speed up comparisions. The hash is constructed
-- such that they can be compared too. (h1 > h2 => name1 > name2)
-- The hash is case-insensitive, just like comparisions on names.
-- Use 'nameCaseEqual' for case-sensitive comparisions.
--
-- Notes:
-- - We use `nameLocal` for the locally qualified name in the module (`int/show`)
-- - The stem is the plain name and operators are not parenthesized (`++`)
-- - The stem should always be a valid identifier; this means that an operator
--   must keep ending with symbols. When hiding names etc, we can get names like `@temp12-++` for example
-- - We assume that users cannot start identifiers with an `@`. (We may in the future allow
--   user identifiers to contain `@` though after the first character.)
-- - Plain module names have an empty local qualifier and stem
-- - If there is a local qualifier, the stem cannot be empty
data Name  = Name
             { nameModule     :: !String        -- module name (`std/core`)
             , hashModule     :: !Int
             , nameLocalQual  :: !String        -- local qualifier (`int`)
             , hashLocalQual  :: !Int
             , nameStem       :: !String        -- the stem (`show`)
             , hashStem       :: !Int
             }

joinWith sep m n  = (if null m then n else if null n then m else m ++ sep ++ n)
join m n          = joinWith "/" m n

nameLocal :: Name -> String
nameLocal (Name m _ l _ n _)
  = join l n

nameCaseEqual name1 name2
  = nameLocal name1 == nameLocal name2
    &&
    and (zipWith (==) (reverse (splitModuleName name1)) (reverse (splitModuleName name2)))

nameCaseEqualPrefixOf name1 name2
  = isPrefixOf (nameLocal name1) (nameLocal name2)
    &&
    and (zipWith (==) (reverse (splitModuleName name1)) (reverse (splitModuleName name2)))

nameCaseOverlap :: Name -> Name -> Bool
nameCaseOverlap name1 name2
  = (not (nameCaseEqual name1 name2)) && (isSameNamespace name1 name2)

nameCaseOverlapPrefixOf :: Name -> Name -> Bool
nameCaseOverlapPrefixOf name1 name2
  = (not (nameCaseEqualPrefixOf name1 name2)) && (isSameNamespace name1 name2)

-- Checks whether both names are in the same namespace, ie. constructors or not
isSameNamespace name1 name2
  = (isConstructorName name1 == isConstructorName name2)

lowerCompare (Name m1 _ l1 _ n1 _) (Name m2 _ l2 _ n2 _)
  = case lowerCompareS m1 m2 of
      EQ -> case lowerCompareS l1 l2 of
              EQ -> lowerCompareS n1 n2
              lg -> lg
      lg -> lg

lowerCompareS (c:cs) (d:ds)
  = case compare (toLower c) (toLower d) of
      EQ -> lowerCompareS cs ds
      lg -> lg
lowerCompareS (c:cs) [] = GT
lowerCompareS [] (d:ds) = LT
lowerCompareS [] []     = EQ

instance Eq Name where
  n1@(Name _ hm1 _ hl1 _ hn1) == n2@(Name _ hm2 _ hl2 _ hn2)
    = (hn1 == hn2) && (hl1 == hl2) && (hm1 == hm2) && (lowerCompare n1 n2 == EQ)

instance Ord Name where
  compare n1@(Name _ hm1 _ hl1 _ hn1) n2@(Name _ hm2 _ hl2 _ hn2)
    = case compare hm1 hm2 of
        EQ -> case compare hl1 hl2 of
                EQ -> case compare hn1 hn2 of
                        EQ -> lowerCompare n1 n2
                        lg -> lg
                lg -> lg
        lg -> lg

-- Effects compare by name first, then by module name for efficiency at runtime
labelNameCompare (Name m1 hm1 l1 hl1 n1 hn1) (Name m2 hm2 l2 hl2 n2 hn2)
  = case compare hn1 hn2 of
      EQ -> case lowerCompareS n1 n2 of
              EQ -> case compare hl1 hl2 of
                      EQ -> case lowerCompareS l1 l2 of
                              EQ -> case compare hm1 hm2 of
                                      EQ -> lowerCompareS m1 m2
                                      lg -> lg
                              lg -> lg
                      lg -> lg
              lg -> lg
      lg -> lg


isIdChar :: Char -> Bool
isIdChar c
  = (isAlphaNum c || c == '_' || c == '@' || c == '-')

isIdStartChar :: Char -> Bool
isIdStartChar c
  = (isAlpha c || c == '_' || c == '@')

isIdEndChar :: Char -> Bool
isIdEndChar c
  = isIdChar c || c == '\''

isSymbolId :: String -> Bool
isSymbolId "" = False
isSymbolId s  = not (isIdStartChar (head s)) || not (isIdEndChar (last s))
  -- where
  --
  --   isIdEndChar c    = (c == '\'' || c == '?')

wrapId :: String -> String
wrapId s
  = if isSymbolId s then "(" ++ s ++ ")" else s


showName :: Bool -> Name -> String
showName explicitLocalQualifier (Name m _ l _ n _)
  = let ln = join l (wrapId n)
    in if null m then ln
                 else if null ln then m
                                 else m ++ (if explicitLocalQualifier && not (null l) then "/#" else "/") ++ ln

showExplicit name
   = showName True name

showPlain (Name m _ l _ n _)
  = join m (join l n)


instance Show Name where
  show name
   = showExplicit name

instance Pretty Name where
  pretty name
    = text (show name)

prettyName :: ColorScheme -> Name -> Doc      -- not explicit /#
prettyName cs (Name m _ l _ n _)
  = let ln = join l (wrapId n)
    in if null m then text ln
                 else color (colorModule cs) (text m <.> (if null ln then empty else text "/")) <.> text ln

prettyCoreName :: ColorScheme -> Name -> Doc  -- explicit /# if needed
prettyCoreName cs (Name m _ l _ n _)
  = let ln = join l (wrapId n)
    in if null m then text ln
                 else color (colorModule cs)
                          (text m <.> (if null ln then empty else (if null l then text "/" else text "/#")))
                      <.> text ln


-- todo: remove these as we can now read/write reliably using readQualifiedName
showTupled (Name m _ l _ n _)
  = show (m,l,n)

readTupled s
  = let (m,l,n) = ((read s) :: (String,String,String))
    in newLocallyQualified m l n

readQualified s
  = if (take 1 s == "(")
     then readTupled s
     else readQualifiedName s


newName :: String -> Name
newName s
  = newQualified "" s

newModuleName :: String -> Name
newModuleName s
  = newQualified s ""

newQualified :: String -> String -> Name
newQualified m n
  = newLocallyQualified m "" n

newLocallyQualified :: String -> String -> String -> Name
newLocallyQualified m l n
  = Name m (hash m) l (hash l) n (hash n)

-- The hash function:
--  1) can be compared: h1 < h2  => name1 < name2 && h1 > h2 => name1 > name2
--  2) assumes 32 bit integers and no characters in strings >= \x128
--  3) is case in-sensitive (ie. does tolower first)
-- The hash is done taking the first 4 characters. This is of course a
-- terrible hash but we use it mostly to speed up *comparisions* for the NameMap
hash :: String -> Int
hash s = foldl (\h c -> h*256 + fromEnum c) 0 (map toLower (take 4 (s ++ "\0\0\0\0")))

nameMapStem :: Name -> (String -> String) -> Name
nameMapStem (Name m hm l hl n _) f
  = let fn = f n in Name m hm l hl fn (hash fn)



readQualifiedName :: String -> Name
readQualifiedName s
  = let (qual,lqual,id) = splitName s
    in newLocallyQualified qual lqual id
  where
    splitName :: String -> (String,String,String)
    splitName s
      = case reverse s of
          (')':rs1) -> -- operator
                      let (rop,rest) = span (/='(') rs1
                      in case rest of
                            ('(':rs2) -> let (qual,lqual,id) = splitIdNameRev rs2
                                        in (qual,lqual,id ++ reverse rop)
                            _ -> failure ("Lexer.splitName: unmatched parenthesis in name: " ++ s)
          rs -> splitIdNameRev rs
      where
        splitIdNameRev :: String -> (String,String,String)
        splitIdNameRev rs
          = let (rid,rest) = span (/='#') rs
            in case rest of
                ('#':'/':rs2) -> -- local qualifier
                                  let (lqual,id) = splitQualIdRev rid
                                  in (reverse rs2, lqual, id)
                [] -> let (qual,id) = splitQualIdRev rid
                      in (qual,"",id)
                _  -> failure ("Lexer.splitName.IdName: illegal locally qualified name: " ++ (reverse s))

        splitQualIdRev :: String -> (String,String)
        splitQualIdRev rs
          = let (rid,rqual) = span (/='/') rs
            in case rqual of
                ('/':rs1) -> -- qualifier
                              (reverse rs1, reverse rid)
                _         -> ("",reverse rid)


{-
-- A "local name" can be locally qualified (`int/(+)`).
-- we can split this out somewhat efficiently.
splitLocalName :: String -> (String,String)
splitLocalName s
  = case s of
      ('(':_) -> ("",s)  -- symbols, no local qualifier
      _       -> let (pre1,post1) = span (/='/') s
                 in case post1 of
                      ('/':rest) -> let (lqual,id) = splitLocalName rest
                                    in (pre1 ++ "/" ++ lqual, id)
                      "" -> ("",s)  -- no local quantifier
-}

isModuleName :: Name -> Bool
isModuleName name
  = null (nameStem name)

isQualified :: Name -> Bool
isQualified name
  = not (null (nameModule name))

isLocallyQualified :: Name -> Bool
isLocallyQualified name
  = not (null (nameLocalQual name))

isSymbolName :: Name -> Bool
isSymbolName name
  = isSymbolId (nameStem name)

isConstructorName :: Name -> Bool
isConstructorName name
  = case nameStem name of
      '@':c:cs -> isUpper c
      c:cs     -> isUpper c
      _        -> False

isWildcard :: Name -> Bool
isWildcard name
  = case nameStem name of
      ('_':_)     -> True
      ('@':'_':_) -> True
      _           -> False

isHiddenName :: Name -> Bool
isHiddenName name
  = case nameStem name of
      ('@':_)      -> True
      _            -> False


missingQualifier :: Name -> Name -> Name -> String
missingQualifier currentMod name qname
  = let missing0 = reverse $ drop (length (showPlain name)) $ reverse (showPlain qname)
        standard = [show currentMod,"std/core/types","std/core/hnd","std/core"]
        missing  = case filter (\std -> (std ++ "/") `isPrefixOf` missing0) standard of
                    (std:_) -> drop (length std + 1) missing0
                    _       -> missing0
    in -- trace ("missingQualifier: " ++ show [currentMod,name,qname] ++ ", missing: " ++ show (missing0,missing))$
       missing

{-
nameSplit :: Name -> (String,String,String)
nameSplit (Name m _ l _ n _)
  = (m,l,n)

missingQualifier :: Name -> Name -> String
missingQualifier name qname =
  case (nameSplit name, nameSplit qname) of
    ((qualifier1, localQual1, name1), (qualifier2, localQual2, name2)) | name1 == name2 ->
      -- trace ("missingQualifier: " ++ show (name1, name2, qualifier1, localQual1, qualifier2, localQual2)) $
      let q = if qualifier1 == "" then qualifier2 else ""
          lq
            | ensureTrailingSlash localQual1 == ensureTrailingSlash qualifier2 = ""
            | ensureTrailingSlash localQual2 == ensureTrailingSlash qualifier1 = ""
            | localQual1 == "" = localQual2
            | otherwise = ""
      in case (q, lq)  of
           ("", "") -> ""
           ("", lq) -> ensureTrailingSlash lq
           (q, "") -> ensureTrailingSlash q
           (q, lq) -> q ++ "/" ++ ensureTrailingSlash lq
    _ -> ""

ensureTrailingSlash n =
  case reverse n of
    ('/':_) -> n
    _ -> n ++ "/"
-}

----------------------------------------------------------------
--
----------------------------------------------------------------

nameNil :: Name
nameNil
  = newName ""

nameIsNil :: Name -> Bool
nameIsNil name
  = null (nameStem name) && null (nameModule name)

qualify :: Name -> Name -> Name
qualify (Name m hm _ 0 _ 0) (Name _ 0 l hl n hn)     = Name m hm l hl n hn
qualify (Name m1 _ _ 0 _ 0) name@(Name m2 _ _ _ _ _) | m1 == m2 = name
qualify n1 n2
  = failure ("Common.Name.qualify: illegal qualification: " ++ show (n1,n2))

unqualify :: Name -> Name
unqualify (Name _ _ l hl n hn)
  = Name "" 0 l hl n hn

qualifier :: Name -> Name
qualifier (Name m hm _ _ _ _)
  = Name m hm "" 0 "" 0

nameAsModuleName :: Name -> Name
nameAsModuleName (Name m _ l _ n _)
  = newModuleName (join m (join l n))

qualifyLocally :: Name -> Name -> Name
qualifyLocally (Name loc _ _ 0 _ 0) (Name m _ l _ n _)
  = newLocallyQualified m (join loc l) n
qualifyLocally name1 name2
  = failure ("Common.Name.qualifyLocally: illegal qualification: " ++ showExplicit name1 ++ ", " ++ showExplicit name2)

-- move the module qualifier to the local qualifier
requalifyLocally :: Name -> Name
requalifyLocally name@(Name m _ l _ n _)
  = if null m then name else newLocallyQualified "" (join m l) n

-- only keep the stem
unqualifyFull :: Name -> Name
unqualifyFull (Name _ _ _ _ n hn)
  = Name "" 0 "" 0 n hn

-- full qualifier: module + local qualifier
fullQualifier :: Name -> String
fullQualifier name
  = nameModule (unqualifyLocally name)

-- add the local qualifier to the module qualifier
unqualifyLocally :: Name -> Name
unqualifyLocally name@(Name m _ l _ n _)
  = if null l then name else newQualified (join m l) n

unqualifyAsModuleName :: Name -> Name
unqualifyAsModuleName (Name m _ l _ n _)
  = newModuleName (join m l)



----------------------------------------------------------------
-- Modules paths
----------------------------------------------------------------

splitModuleName :: Name -> [String]
splitModuleName name
  = splitOn (=='/') (nameModule name)

unsplitModuleName :: [String] -> Name
unsplitModuleName xs
  = newModuleName (concat (intersperse "/" xs))

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

splitLocalQualName :: Name -> [String]
splitLocalQualName name
  = splitOn (=='/') (nameLocalQual name)


----------------------------------------------------------------
-- wildcards & constructors
----------------------------------------------------------------

toConstructorName :: Name -> Name
toConstructorName name
  = nameMapStem name $ \stem ->
    case stem of
      ('@':c:cs) -> '@':toUpper c : cs  -- keep hidden names hidden
      (c:cs)     -> toUpper c : cs
      ""         -> ""

toVarName :: Name -> Name
toVarName name
  = nameMapStem name $ \stem ->
    case stem of
      ('@':cs)   -> '@':toLowers cs  -- keep hidden names hidden
      cs         -> toLowers cs
  where
    toLowers s  -- while uppercase, map toLower
      = case s of
          (c:cs) | isUpper c -> toLower c : toLowers cs
          _      -> s


nameStartsWith :: Name -> String -> Bool
nameStartsWith name pre
  = nameStem name `startsWith` pre

prepend :: String -> Name -> Name
prepend pre name
  = nameMapStem name $ \stem ->
    case stem of
      ('@':t) -> case pre of -- keep hidden names hidden
                   '@':_ -> pre ++ t
                   _     -> '@' : pre ++ t
      t       -> pre ++ t


postpend :: String -> Name -> Name
postpend post name | isSymbolName name
  = -- we must always end in symbols
    nameMapStem name $ \stem ->
    let (rsyms,rid) = span (not . isIdChar) (reverse stem)
    in if null rid
         then "@" ++ post ++ reverse rsyms
         else reverse rid ++ post ++ reverse rsyms
postpend post name
  = nameMapStem name $ \stem ->
    let (xs,ys) = span (\c -> c=='?' || c=='\'') (reverse stem)
    in reverse (xs ++ reverse post ++ ys)


----------------------------------------------------------------
-- various special names
----------------------------------------------------------------

newHiddenName s
  = newName ("@" ++ s)

newPaddingName i
  = newHiddenName ("padding" ++ show i)

isPaddingName name
  = nameStartsWith name "@padding"

isCCtxName name
  = nameStartsWith name "@cctx"


newFieldName i
  = newHiddenName ("field" ++ show i)

isFieldName name
  = nameStartsWith name "@field"


newImplicitTypeVarName i
  = newHiddenName ("tv" ++ show i)

isImplicitTypeVarName name
  = nameStartsWith name "@tv"


newHiddenExternalName name
  = makeHiddenName "extern" name

isHiddenExternalName name
  = hiddenNameStartsWith name "extern"




makeHidden :: Name -> Name
makeHidden name
  = nameMapStem name $ \stem ->
    case stem of
      ('@':cs)      -> stem
      _             -> '@':stem

makeHiddenName :: String -> Name -> Name
makeHiddenName s name
  = makeHidden (prepend (s ++ "-") name)

unmakeHidden :: String -> Name -> Name
unmakeHidden pre name
  = nameMapStem name $ \stem ->
    if stem `startsWith` ("@" ++ pre ++ "-")
      then drop (length pre + 2) stem
      else err
  where
    err = failure ("Name.unmakeHidden: expecting hidden name prefixed with @" ++ pre ++ "-, but found: " ++ show name)


makeFreshHiddenName s name range
  = makeHiddenName s (postpend (idFromPos (rangeStart range)) name)
    where idFromPos pos = "-l" ++ show (posLine pos) ++ "-c" ++ show (posColumn pos)

hiddenNameStartsWith name pre
  = nameStartsWith name ("@" ++ pre ++ "-")


toUniqueName :: Int -> Name -> Name
toUniqueName i name
  = postpend (show i) name

toHiddenUniqueName :: Int -> String -> Name -> Name
toHiddenUniqueName i pre name
  = makeHiddenName pre (toUniqueName i name)


-- | Create a constructor creator name from the constructor name.
-- Used if special creation functions are used for the constructor.
-- in particular for the case of optional arguments.
newCreatorName :: Name -> Name
newCreatorName name
  = makeHiddenName "create" name

isCreatorName :: Name -> Bool
isCreatorName name
  = hiddenNameStartsWith name "create"


-- | Create a handler type name from an effect type name.
toHandlerName :: Name -> Name
toHandlerName name
  = makeHiddenName "hnd" name

isHandlerName :: Name -> Bool
isHandlerName name
  = hiddenNameStartsWith name "hnd"

-- | Create an effect type name from an operations type name.
fromHandlerName :: Name -> Name
fromHandlerName name
  = unmakeHidden "hnd" name


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
  = hiddenNameStartsWith name "ops"

-- | Create an effect type name from an operations type name.
fromOperationsName :: Name -> Name
fromOperationsName name
  = unmakeHidden "ops" name


-- | Create an operations type name from an effect type name.
toOpSelectorName :: Name -> Name
toOpSelectorName name
  = makeHiddenName "select" name

-- | Is this an operations name?
isOpSelectorName :: Name -> Bool
isOpSelectorName name
  = hiddenNameStartsWith name "select"

-- | Create an effect type name from an operations type name.
fromOpSelectorName :: Name -> Name
fromOpSelectorName name
  = unmakeHidden "select" name


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
  = hiddenNameStartsWith name "tag"

-- | Create a name for a value operation
toValueOperationName :: Name -> Name
toValueOperationName name
  = makeHiddenName "val" name

-- | Is this an name of a value operation?
isValueOperationName :: Name -> Bool
isValueOperationName name
  = hiddenNameStartsWith name "val"

-- | Create an operation name from a value operation name
fromValueOperationsName :: Name -> Name
fromValueOperationsName name
  = unmakeHidden "val" name


implicitNameSpace :: String
implicitNameSpace = "implicit"

isImplicitParamName :: Name -> Bool
isImplicitParamName name
  = (nameLocalQual name == implicitNameSpace)

toImplicitParamName :: Name -> Name
toImplicitParamName name
  = qualifyLocally (newModuleName implicitNameSpace) name

plainImplicitParamName :: Name -> Name
plainImplicitParamName name
  = unqualifyFull name

namedImplicitParamName :: Name -> Name -> Name
namedImplicitParamName pname ename
  = toImplicitParamName (newName (nameStem pname ++ "@-@" ++ nameStem ename))

splitImplicitParamName :: Name -> (Name,Name)
splitImplicitParamName name
  = case splitAt "@-@" (nameStem name) of
      (pre,post) | not (null pre) && not (null post) -> (toImplicitParamName (newName pre), newName post)
      _ -> (name, plainImplicitParamName name)
  where
    splitAt sub s      | s `startsWith` sub  = ("",drop (length sub) s)
    splitAt sub (c:cs) = let (pre,post) = splitAt sub cs in (c:pre,post)
    splitAt sub ""     = ("","")

{-
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
-}

----------------------------------------------------------------
-- name to file path
----------------------------------------------------------------
moduleNameToPath :: Name -> FilePath
moduleNameToPath name
  = asciiEncode True (show name)

pathToModuleName :: FilePath -> Name
pathToModuleName path
  = newModuleName $ dropWhile (\c -> c `elem` "_./") $
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
      "@<>"   -> "_total_"
      "@<|>"  -> "_extend_"
      "@()"   -> "_unit_"
      "@(,)"  -> "_tuple2_"
      "@(,,)" -> "_tuple3_"
      "@(,,,)"-> "_tuple4_"
      "()"    -> "_Unit_"
      "(,)"   -> "_Tuple2_"
      "(,,)"  -> "_Tuple3_"
      "(,,,)" -> "_Tuple4_"
      "[]"    -> "_index_"
      -- '@':'c':'o':'n':' ':cs -> trace ("con name: " ++ name) $ "_con_" ++ encodeChars cs
      -- '@':'t':'y':'p':'e':' ':cs -> "_type_" ++ encodeChars cs
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
          '@' | (isDigit post || post == ' ' || pre == ' ' || pre == '/') -> "_"

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

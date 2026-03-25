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
          ( Name, Names, ModuleName     -- instance Eq Ord Show
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
          , nameModule, nameStem, nameLocal, nameLocalQual, isModuleName

          , newPaddingName, isPaddingName, isCCtxName
          , newFieldName, isFieldName, isWildcard, unWildcard, nameIsEtaHole
          , typeQualifiedName, typeQualifiedNameOf, typeQualifiedGetTypeName
          , newHiddenExternalName, isHiddenExternalName
          , newHiddenName, isHiddenName, hiddenNameStartsWith
          , makeHiddenName, makeFreshHiddenName, newHiddenNameEx
          , toUniqueName
          , newImplicitTypeVarName, isImplicitTypeVarName
          , newCreatorName, isCreatorName
          , toHandlerName, fromHandlerName, isHandlerName, isHandlerConName
          , toOpSelectorName, fromOpSelectorName, isOpSelectorName
          , toOperationsName, fromOperationsName, isOperationsName
          , toEffectTagName
          , toHandleName, isHandleName
          , toOpsConName, toOpConName, toOpTypeName
          , toConstructorName, isConstructorName, toVarName, toHandlerConName
          , toOpenTagName, isOpenTagName
          , toLazyIndirectConName, isLazyIndirectConName
          , toValueOperationName, isValueOperationName, fromValueOperationsName, toBasicOperationsName
          , splitModuleName, unsplitModuleName, mergeCommonPath, splitLocalQualName
          , missingQualifier
          , isEarlyBindName
          , toImplicitParamName, isImplicitParamName, splitImplicitParamName
          , fromImplicitParamName

          , prepend, postpend, isSymbolName
          , asciiEncode, moduleNameToPath, pathToModuleName
          -- , canonicalSep, canonicalName, nonCanonicalName, canonicalSplit

          , prettyName, prettyCoreName
          , requalifyLocally, qualifyLocally, unqualifyFull, isLocallyQualified, fullQualifier
          , unqualifyAsModuleName, unqualifyLocally

          , nameMapStem
          , isInDefaultNameSpace, isInWrongNameSpace

          , showHex, showBinary, showHexFloat
          ) where

-- import Lib.Trace( trace )
import Debug.Trace
import Lib.PPrint
import Data.Char(isUpper,isLower,toLower,toUpper,isAlphaNum,isDigit,isAlpha)
import Common.Failure(failure,assertion, HasCallStack)
import Common.File( joinPaths, splitOn, endsWith, startsWith, isPathSep )
import Common.Range( rangeStart, posLine, posColumn )
import Data.List(intersperse,isPrefixOf)
import Common.ColorScheme

-- just for hover info
import Data.Numbers.FloatingHex( showHFloat )
import Numeric (showIntAtBase)
import Data.Char( intToDigit )


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

type ModuleName = Name

joinWith sep m n  = (if null m then n else if null n then m else m ++ sep ++ n)
join m n          = joinWith "/" m n

joins ms  = foldr join "" ms

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
  nm1@(Name m1 hm1 l1 hl1 n1 hn1) == nm2@(Name m2 hm2 l2 hl2 n2 hn2)
    = let eq = (hn1 == hn2) && (hl1 == hl2) && (hm1 == hm2) in
      assertion ("Common.Name.Eq: wrong hashes: " ++ show [(hm1,hl1,hn1),(hm2,hl2,hn2)] ++ show (nm1,nm2))
                (if not eq then showFullyExplicit nm1 /= showFullyExplicit nm2 else True) $
      eq && (lowerCompare nm1 nm2 == EQ)
      -- eq && m1 == m2 && l1 == l2 && n1 == n2 -- (eq && (lowerCompare n1 n2 == EQ)) // Lower compare doesn't make a difference since the hash will likely not be equal



instance Ord Name where
  -- compare module, stem name, and then local name for dependencies
  compare nm1@(Name m1 hm1 l1 hl1 n1 hn1) nm2@(Name m2 hm2 l2 hl2 n2 hn2)
    = case compare hm1 hm2 of
        EQ -> case compare hn1 hn2 of
                EQ -> case compare hl1 hl2 of
                        EQ -> lowerCompare nm1 nm2
                        -- EQ -> case compare m1 m2 of -- Don't use lowerCompare here, since the hash will not be equal (ruled out by EQ)
                        --         EQ -> case compare l1 l2 of
                        --                 EQ -> compare n1 n2
                        --                 lg -> lg
                        --         lg -> lg
                        lg -> lg
                lg -> lg
        lg -> lg

-- Effects compare by name first, then by module name for efficiency at runtime
labelNameCompare (Name m1 hm1 l1 hl1 n1 hn1) (Name m2 hm2 l2 hl2 n2 hn2)
  = case lowerCompareS (n1 ++ "@") (n2 ++ "@") of -- Labels are name@module, so if a name looks like name-x@module and name@module, we need to compare @ and - (which is what the runtime does)
          EQ -> case compare l1 l2 of
                      EQ -> compare m1 m2
                      lg -> lg
          lg -> lg


stemIsEqual :: Name -> Name -> Bool
stemIsEqual (Name m1 _ l1 _ n1 hn1) (Name m2 _ l2 _ n2 hn2)
  = (hn1 == hn2 && n1 == n2)

isIdChar :: Char -> Bool
isIdChar c
  = (isAlphaNum c || c == '_' || c == '@' || c == '-')

isSymbolChar :: Char -> Bool
isSymbolChar c
  = c `elem` "$%&*+~!\\^#=.:-|<>/"

isIdStartChar :: Char -> Bool
isIdStartChar c
  = (isAlpha c || c == '_' || c == '@')

isIdEndChar :: Char -> Bool
isIdEndChar c
  = isIdChar c || c == '\''

isSymbolId :: String -> Bool
isSymbolId s
  = case s of
      [c] -> not (isIdStartChar c && isIdEndChar c)
      (c:cs) -> not (isIdStartChar c && isIdEndChar (last cs) && all isIdEndChar (init cs))
      "" -> False

wrapId :: String -> String
wrapId s
  = if isSymbolId s then "(" ++ s ++ ")" else s


showName :: Bool -> Name -> String
showName explicitLocalQualifier (Name m _ l _ n _)
  = let ln = join l (wrapId n)
    in if null m then ln
                 else if null ln then m
                                 else m ++ (if explicitLocalQualifier && not (null l) then "/#" else "/") ++ ln

showFullyExplicit (Name m _ l _ n _)
   = let ln = join l (wrapId n)
     in if null m then "#" ++ ln
                  else if null ln then m
                                  else m ++ "/#" ++ ln

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

prettyNameEx :: String -> ColorScheme -> Name -> Doc  -- explicit /# if needed
prettyNameEx lsep cs (Name m _ l _ n _)
  = let ln = join l (wrapId n)
    in if null m then text ln
                 else color (colorModule cs)
                          (text m <.> (if null ln then empty else (if null l then text "/" else text lsep)))
                      <.> text ln

prettyName :: ColorScheme -> Name -> Doc      -- not explicit /#
prettyName cs name
  = if isImplicitParamName name
      then text "?" <.> prettyNameEx "/" cs (requalifyLocally (fromImplicitParamName name))
      else prettyNameEx "/" cs name

prettyCoreName :: ColorScheme -> Name -> Doc  -- explicit /# if needed
prettyCoreName cs name
  = prettyNameEx "/#" cs name


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
readQualifiedName ('?':s)
  = toImplicitParamName (requalifyLocally (readQualifiedName s))
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

unWildcard :: String -> Name -> Name
unWildcard post name
  = case nameStem name of
      ('_':_)     -> nameMapStem name (\s -> tail s ++ post)
      ('@':'_':_) -> nameMapStem name (\s -> "@" ++ drop 2 s ++ post)
      _           -> name


nameIsEtaHole :: Name -> Bool
nameIsEtaHole name
  = isWildcard name

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
    in -- trace ("missingQualifier: " ++ show [currentMod,name,qname] ++ ", missing: " ++ show (missing0,missing)) $
       if missing `startsWith` (implicitNameSpace ++ "/")
         then "?" ++ drop (length implicitNameSpace + 1) missing
         else missing

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

qualify :: HasCallStack => Name -> Name -> Name
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

isInDefaultNameSpace, isInWrongNameSpace :: Name -> Bool
isInDefaultNameSpace name = isInSpecialNameSpace name "default"
isInWrongNameSpace name   = isInSpecialNameSpace name "wrong"

isInSpecialNameSpace :: Name -> String -> Bool
isInSpecialNameSpace name ns
  = case splitLocalQualName name of
      (first:_) -> first == ns
      _         -> False

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

toHandlerConName :: Name -> Name
toHandlerConName name
  = makeHiddenName "Hnd" name

isHandlerConName :: Name -> Bool
isHandlerConName name
  = hiddenNameStartsWith name "Hnd"

nameStartsWith :: Name -> String -> Bool
nameStartsWith name pre
  = nameStem name `startsWith` pre

-- | Check if a string is a valid lower-case identifier (starts with optional @ followed by lowercase)
isLowerId :: String -> Bool
isLowerId s = case dropWhile (=='@') s of
                (c:_) -> isLower c
                _     -> False

-- | Check if a string is a valid constructor identifier (starts with optional @ followed by uppercase)
isConstructorId :: String -> Bool
isConstructorId s = case dropWhile (=='@') s of
                      (c:_) -> isUpper c
                      _     -> False

-- | Check if a string is a valid identifier (lower or constructor)
isValidId :: String -> Bool
isValidId s = isLowerId s || isConstructorId s

-- | Append two strings, inserting 'x' if the suffix starts with a non-alpha character after a hyphen
-- to maintain well-formed identifiers.
appendWithXAfterHyphen :: String -> String -> String
appendWithXAfterHyphen pre suf = case (reverse pre, suf) of
  ('-':_, c:_) | not (isAlpha c) -> pre ++ "x" ++ suf
  _ -> pre ++ suf

-- | Ensure a prefix string is a valid lower-case identifier.
-- If not, prepend "@x".
-- Also drops a trailing '@' if adding '@x' (to avoid double @ or empty @).
ensureLowerId :: String -> String
ensureLowerId s
  | isLowerId s = s
  | otherwise   = "@x" ++ dropTrailingAt s
  where
    dropTrailingAt str = case reverse str of
                           ('@':rest) -> reverse rest
                           _          -> str

prepend :: String -> Name -> Name
prepend pre name | isSymbolName name
  = nameMapStem name $ \stem ->
    let (rsyms,rid) = span isSymbolChar (reverse stem)
        prefix = reverse rid
        syms = reverse rsyms
        newprefix = prependRaw pre prefix
    in ensureLowerId newprefix ++ syms

prepend pre name
  = nameMapStem name $ \stem -> prependStr pre stem

prependRaw :: String -> String -> String
prependRaw pre stem
  = case stem of
      ('@':t) -> case pre of -- keep hidden names hidden
                    '@':_ -> appendWithXAfterHyphen pre t
                    _     -> '@' : appendWithXAfterHyphen pre t
      _       -> appendWithXAfterHyphen pre stem

prependStr :: String -> String -> String
prependStr pre stem
  = let result = prependRaw pre stem
    in if result `startsWith` "@" && not (isValidId result)
         then "@x" ++ result
         else result


postpend :: String -> Name -> Name
postpend post name | isSymbolName name
  = -- we must always end in symbols for operators so postpend inserts before the symbols
    nameMapStem name $ \stem ->
    let (rsyms,rid) = span isSymbolChar (reverse stem)
        prefix = reverse rid
        -- ensure prefix is valid only when we actually end in operator symbols;
        -- note: we don't use ensureLowerId here as we don't want to drop trailing @
        validPrefix
          | null rsyms = prefix
          | null prefix || not (isLowerId prefix) = "@x" ++ prefix
          | otherwise = prefix
    in validPrefix ++ post ++ reverse rsyms

postpend post name
  = nameMapStem name $ \stem ->
    let (xs,ys) = span (\c -> c=='?' || c=='\'') (reverse stem)
    in reverse (xs ++ reverse post ++ ys)


typeQualifiedName :: Name -> String -> Name
typeQualifiedName typeName stem
  = qualify (qualifier typeName) $
    qualifyLocally (nameAsModuleName (unqualify typeName)) $
    newName stem

typeQualifiedNameOf :: Name -> Name -> Name
typeQualifiedNameOf typeName stem
  = typeQualifiedName typeName (nameStem stem)

typeQualifiedGetTypeName :: HasCallStack => Name -> Name
typeQualifiedGetTypeName name
  = case reverse (splitLocalQualName name) of
      m:ms -> newLocallyQualified (nameModule name) (joins (reverse ms)) m
      []   -> failure ("Common.Name.typeQualifiedGetTypeName: no locally qualified type: " ++ show name)


----------------------------------------------------------------
-- hidden names
----------------------------------------------------------------
makeHidden :: Name -> Name
makeHidden name
  = prepend "@" name

makeHiddenName :: String -> Name -> Name
makeHiddenName s name
  = prepend ("@" ++ s ++ "-") name

unmakeHidden :: HasCallStack => String -> Name -> Name
unmakeHidden pre name
  = nameMapStem name $ \stem ->
    if stem `startsWith` ("@" ++ pre ++ "-")
      then drop (length pre + 2) stem
      else err
  where
    err = failure ("Name.unmakeHidden: expecting hidden name prefixed with @" ++ pre ++ "-, but found: " ++ show name)

newHiddenNameEx :: String -> String -> Name
newHiddenNameEx base s
  = makeHiddenName base (newName s)

newHiddenName :: String -> Name
newHiddenName base
  = makeHidden (newName base)

toUniqueName :: Int -> Name -> Name
toUniqueName i name
  = postpend ("@" ++ show i) name

toHiddenUniqueName :: Int -> String -> Name -> Name
toHiddenUniqueName i pre name
  = makeHiddenName pre (toUniqueName i name)

makeFreshHiddenName s name range
  = makeHiddenName s (postpend (idFromPos (rangeStart range)) name)
    where idFromPos pos = "-l" ++ show (posLine pos) ++ "-c" ++ show (posColumn pos)

hiddenNameStartsWith :: Name -> String -> Bool
hiddenNameStartsWith name pre
  = (nameStem name `startsWith` ("@" ++ pre)) || (nameStartsWith name ("@" ++ pre ++ "-"))

newPaddingName i
  = newHiddenNameEx "padding"  (show i)

isPaddingName name
  = hiddenNameStartsWith name "padding"

newCCtxName s
  = newHiddenNameEx "cctx" s

isCCtxName name
  = hiddenNameStartsWith name "cctx"


newFieldName i
  = newHiddenNameEx "field"  (show i)

isFieldName name
  = hiddenNameStartsWith name "field"


newImplicitTypeVarName i
  = newHiddenNameEx "tv" (show i)

isImplicitTypeVarName name
  = nameStartsWith name "@tv"


newHiddenExternalName name
  = makeHiddenName "extern" name

isHiddenExternalName name
  = hiddenNameStartsWith name "extern"




-- | Create a constructor creator name from the constructor name.
-- Used if special creation functions are used for the constructor.
-- in particular for the case of optional arguments.
-- note: we cannot use `typeQualifiedName` as we need to use it in
-- expressions for dependency analysis before we know its type.
newCreatorName :: Name ->  Name
newCreatorName name
  = makeHiddenName "create" name

isCreatorName :: Name -> Bool
isCreatorName name
  = hiddenNameStartsWith name "create"

hndName = newHiddenName "hnd"
handleName = newHiddenName "handle"
effectTagName = newHiddenName "tag"
effectOpsName = newHiddenName "ops"
opsSelectName = newHiddenName "select"

-- | Create a handler type name from an effect type name.
toHandlerName :: Name -> Name
toHandlerName typeName
  = typeQualifiedNameOf typeName $ hndName

isHandlerName :: Name -> Bool
isHandlerName name
  = stemIsEqual hndName name

-- | Create an effect type name from an operations type name.
fromHandlerName :: HasCallStack => Name -> Name
fromHandlerName name
  = typeQualifiedGetTypeName name

-- | Create a handle function name from an effect type name.
toHandleName :: Name -> Name
toHandleName typeName
  = typeQualifiedNameOf typeName $ handleName -- makeHiddenName "handle" name

isHandleName :: Name -> Bool
isHandleName name
  = stemIsEqual handleName name -- hiddenNameStartsWith name "handle"


-- | Create an operations type name from an effect type name.
toOperationsName :: Name -> Name
toOperationsName typeName
  = typeQualifiedNameOf typeName effectOpsName --makeHiddenName "ops" name

-- | Is this an operations name?
isOperationsName :: Name -> Bool
isOperationsName name
  = stemIsEqual name effectOpsName -- hiddenNameStartsWith name "ops"

-- | Create an effect type name from an operations type name.
fromOperationsName :: Name -> Name
fromOperationsName name
  = typeQualifiedGetTypeName name -- unmakeHidden "ops" name


-- | Create an operations type name from an effect type name.
toOpSelectorName :: Name -> Name
toOpSelectorName typeName
  = typeQualifiedNameOf typeName opsSelectName -- makeHiddenName "select" name

-- | Is this an operations name?
isOpSelectorName :: Name -> Bool
isOpSelectorName name
  = stemIsEqual name opsSelectName -- hiddenNameStartsWith name "select"

-- | Create an effect type name from an operations type name.
fromOpSelectorName :: Name -> Name
fromOpSelectorName name
  = typeQualifiedGetTypeName name -- unmakeHidden "select" name


-- | Create an effect tag name from an effect type name.
toEffectTagName :: Name -> Name
toEffectTagName typeName
  = typeQualifiedNameOf typeName effectTagName -- makeHiddenName "tag" name

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


indirectName = newName "Indirect"

-- | Create an lazy indirect constructor name from a type name.
toLazyIndirectConName :: Name -> Name
toLazyIndirectConName typeName
  = typeQualifiedNameOf typeName indirectName -- "Indirect" -- makeHiddenName "Indirect" name

isLazyIndirectConName :: Name -> Bool
isLazyIndirectConName name
  = stemIsEqual name indirectName -- hiddenNameStartsWith name "Indirect"


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

-- | Create an operation name from either a value operation name or regular operations name
toBasicOperationsName :: Name -> Name
toBasicOperationsName name
  = if isValueOperationName name then unmakeHidden "val" name else name


implicitNameSpace :: String
implicitNameSpace = "@implicit"

isImplicitParamName :: Name -> Bool
isImplicitParamName name
  = case splitLocalQualName name of
      (m:ms) -> (m == implicitNameSpace)
      _      -> False

toImplicitParamName :: Name -> Name
toImplicitParamName name
  = qualifyLocally (newModuleName implicitNameSpace) name

fromImplicitParamName :: Name -> Name
fromImplicitParamName name
  = case splitLocalQualName name of
      (m:ms) | m == implicitNameSpace -> qualifyLocally (unsplitModuleName ms) (unqualifyFull name)
      _      -> name

splitImplicitParamName :: Name -> (Name,Name)
splitImplicitParamName name
  = (name, unqualifyFull name)

    {-
    case splitAt "@-@" (nameStem name) of
      (pre,post) | not (null pre) && not (null post) -> (toImplicitParamName (newName pre), newName post)
      _ -> (name, plainImplicitParamName name)
  where
    splitAt sub s      | s `startsWith` sub  = ("",drop (length sub) s)
    splitAt sub (c:cs) = let (pre,post) = splitAt sub cs in (c:pre,post)
    splitAt sub ""     = ("","")
    -}

{-
plainImplicitParamName :: Name -> Name
plainImplicitParamName name
  = unqualifyFull name

namedImplicitParamName :: Name -> Name -> Name
namedImplicitParamName pname ename
  = toImplicitParamName (newName (nameStem pname ++ "@-@" ++ nameStem ename))
-}

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
showHex len i | i < 0
  = failure ("Common.Name.showHex: negative number: " ++ show i)
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

showBinary :: Int -> Int -> String
showBinary len i
  = let bits = showIntAtBase 2 intToDigit i ""
    in replicate (len - length bits) '0' ++ bits

showHexFloat :: Double -> String
showHexFloat d
  = showHFloat d ""
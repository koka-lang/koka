------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Syntax.RangeMap( RangeMap, RangeInfo(..), NameInfo(..)
                      , rangeMapNew
                      , rangeMapInsert
                      , rangeMapSort
                      , rangeMapLookup
                      , rangeMapFindAt
                      , rangeMapFindIn
                      , rangeMapFind
                      , rangeMapAppend
                      , rangeInfoType
                      , lexemesFromPos
                      , getFunctionNameReverse, getFunctionIncompleteReverse, FnSyntax(..)
                      , previousLexemesReversed, dropMatchedParensReverse, dropAutoGenClosing
                      , getCurrentBlockReverse, getCurrentStatementReverse
                      , mangle
                      , mangleConName
                      , mangleTypeName
                      ) where

import Debug.Trace(trace)
import Data.Char    ( isSpace )
import Common.Failure
import Data.List    (sortBy, groupBy, minimumBy, foldl')
import Lib.PPrint
import Common.Range
import Common.Name
import Common.NamePrim (nameUnit, nameListNil, isNameTuple)
import Common.File( startsWith )
import Type.Type
import Kind.Kind
import Type.TypeVar
import Type.Pretty()
import Data.Maybe (fromMaybe)
import Syntax.Lexeme

newtype RangeMap = RM [(Range,RangeInfo)]
  deriving Show

mangleConName :: Name -> Name
mangleConName name
  = prepend "con " name

mangleTypeName :: Name -> Name
mangleTypeName name
  = prepend "type " name

mangle :: Name -> Type -> Name
mangle name tp
  = name
  -- newQualified (nameModule name) (nameId name ++ ":" ++ compress (show tp))
  where
    compress cs
      = case cs of
          [] -> []
          (c:cc) ->
            if (isSpace c)
             then ' ' : compress (dropWhile isSpace cc)
             else c : compress cc

data RangeInfo
  = Decl String Name Name  -- alias, type, cotype, rectype, fun, val
  | Block String           -- type, kind, pattern
  | Error Doc
  | Warning Doc
  | Id Name NameInfo [Doc] Bool  -- qualified name, info, extra doc (from implicits), is this the definition?
  | Implicits Doc                -- inferred implicit arguments

data NameInfo
  = NIValue   Type String Bool -- Has annotated type already
  | NICon     Type String
  | NITypeCon Kind String
  | NITypeVar Kind
  | NIModule
  | NIKind


instance Show RangeInfo where
  show ri
    = case ri of
        Decl kind nm1 nm2 -> "Decl " ++ kind ++ " " ++ show nm1 ++ " " ++ show nm2
        Block kind        -> "Block " ++ kind
        Error doc         -> "Error"
        Warning doc       -> "Warning"
        Id name info docs isDef -> "Id " ++ show name ++ (if isDef then " (def)" else "") ++ " " ++ show docs
        Implicits doc        -> "Implicits " ++ show doc

instance Enum RangeInfo where
  fromEnum r
    = case r of
        Decl _ name _    -> 0
        Block _          -> 10
        Id name info _ _ -> 20
        Implicits _      -> 25
        Warning _        -> 40
        Error _          -> 50

  toEnum i
    = failure "Syntax.RangeMap.RangeInfo.toEnum"

penalty :: Name -> Int
penalty name
  = if (nameModule name == "std/core/hnd")
     then 10 else 0

-- (inverse) priorities
instance Enum NameInfo where
  fromEnum ni
    = case ni of
        NIValue _ _ _ -> 1
        NICon   _ _   -> 2
        NITypeCon _ _ -> 3
        NITypeVar _   -> 4
        NIModule      -> 5
        NIKind        -> 6

  toEnum i
    = failure "Syntax.RangeMap.NameInfo.toEnum"

isHidden ri
  = case ri of
      Decl kind nm1 nm2       -> isHiddenName nm1
      Id name info docs isDef -> isHiddenName name
      _ -> False


rangeMapNew :: RangeMap
rangeMapNew
  = RM []

cut r
  = (makeRange (rangeStart r) (rangeStart r))

rangeMapInsert :: Range -> RangeInfo -> RangeMap -> RangeMap
rangeMapInsert r info (RM rm)
  = -- trace ("rangemap insert: " ++ show r ++ ": " ++ show info) $
    if isHidden info
     then RM rm
    else if beginEndToken info
     then RM ((r,info):(makeRange (rangeEnd r) (rangeEnd r),info):rm)
     else RM ((r,info):rm)
  where
    beginEndToken info
      = case info of
          Id name _ _ _ -> (name == nameUnit || name == nameListNil || isNameTuple name)
          _ -> False

rangeMapAppend :: RangeMap -> RangeMap -> RangeMap
rangeMapAppend (RM rm1) (RM rm2)
  = RM (rm1 ++ rm2)

rangeMapSort :: RangeMap -> RangeMap
rangeMapSort (RM rm)
  = RM (sortBy (\(r1,_) (r2,_) -> compare r1 r2) rm)

-- | select the best matching range infos from a selection
prioritize :: [(Range,RangeInfo)] -> [(Range,RangeInfo)]
prioritize rinfos
  = let idocs = reverse $
                concatMap (\(_,rinfo) -> case rinfo of
                                            Implicits doc -> [doc]
                                            _             -> []) rinfos
    in map (mergeDocs idocs) $
        map last $
        groupBy eq $
        sortBy cmp $
        filter (not . isImplicits . snd) rinfos
  where
    isImplicits (Implicits _) = True
    isImplicits _             = False

    eq (_,ri1) (_,ri2)  = (EQ == compare ((fromEnum ri1) `div` 10) ((fromEnum ri2) `div` 10))
    cmp (_,ri1) (_,ri2) = compare (fromEnum ri1) (fromEnum ri2)

    -- merge implicit documentation into identifiers
    mergeDocs ds (rng, Id name info docs isDef) = (rng, Id name info (docs ++ ds) isDef)
    mergeDocs ds x = x


rangeMapLookup :: Range -> RangeMap -> ([(Range,RangeInfo)],RangeMap)
rangeMapLookup r (RM rm)
  = let (rinfos,rm') = span startsAt (dropWhile isBefore rm)
    in -- trace ("lookup: " ++ show r ++ ": " ++ show rinfos) $
       (prioritize rinfos, RM rm')
  where
    pos = rangeStart r
    isBefore (rng,_)  = rangeStart rng < pos
    startsAt (rng,_)  = rangeStart rng == pos

rangeMapFindIn :: Range -> RangeMap -> [(Range, RangeInfo)]
rangeMapFindIn rng (RM rm)
  = filter (\(rng, info) -> rangeStart rng >= start || rangeEnd rng <= end) rm
    where start = rangeStart rng
          end = rangeEnd rng

-- Gets all lexemes less than the given position and then reverses their order
previousLexemesReversed :: [Lexeme] -> Pos -> [Lexeme]
previousLexemesReversed lexemes pos =
  reverse $ takeWhile (\lex -> rangeStart (getRange lex) <= pos) lexemes

-- Dropes everything inside and including matched parentheses, assumes the ending paren is already dropped
dropMatchedParensReverse :: [Lexeme] -> [Lexeme]
dropMatchedParensReverse = dropToLexMatching (== LexSpecial ")") (== LexSpecial "(")

-- Assumes in the middle of the function parameters 
-- (drops to nearest open paren that didn't have a close paren before it)
-- This takes care of finding signature info when a cursor is in an argument list
getFunctionIncompleteReverse :: [Lexeme] -> FnSyntax
getFunctionIncompleteReverse xs = getFunctionNameReverse (dropMatchedParensReverse (dropAutoGenClosing xs))

-- Assumes it is given reverse ordered lexemes ending at an end of a function invocation
-- 
-- e.g.
--   a.b(x, y, fn() {z}).abc
--   => FnChained "b" "abc" -- had a .abc after the b
-- also 
--   a.b
--   => FnNormal "a"
-- and
--   a.
--   => FnNormal "a"
-- and finally
--   (abc).abc => NotFound
getFunctionNameReverse :: [Lexeme] -> FnSyntax
getFunctionNameReverse xs =
  let xs' = getCurrentStatementReverse $ dropAutoGenClosing xs in
  -- trace ("getFunctionNameReverse: " ++ show xs') $
  let go xs =
        case xs of
          [] -> EmptyStatement
          -- "" 10 1.0 'c' [] x etc...
          v@(Lexeme _ (LexString s)):xs -> FnValue v
          v@(Lexeme _ (LexInt _ _)):xs -> FnValue v
          v@(Lexeme _ (LexFloat _ _)):xs -> FnValue v
          v@(Lexeme _ (LexChar _)):xs -> FnValue v
          v@(Lexeme _ (LexSpecial "]")):xs -> FnValue v
          [x@(Lexeme _ (LexId _))] -> FnValue x
          -- x(). or (x.y). or even (1 + 2). %the last will return a chain ending in FnNotFound%
          (Lexeme _ (LexKeyword "." _)):xs -> FnIncomplete $ go xs
          -- x() or (x.y) or even (1 + 2) %the last will return FnNotFound% 
          (Lexeme _ (LexSpecial ")")):xs -> 
            let dropped = dropMatchedParensReverse xs in
            -- trace ("getFunctionNameReverse: " ++ show xs ++ " dropped: " ++ show dropped) $
            case go dropped of
              -- (a).b -- if there is nothing before the parenthesized expression
              -- it doesn't mean there isn't a chained function target
              EmptyStatement -> go xs
              res -> res 
          -- x.partial, x().partial etc
          fn@(Lexeme _ (LexId _)):(Lexeme _ (LexKeyword "." _)):xs -> chain fn $ go xs
          _ -> FnNotFound xs
  in go xs'

-- Add a function to a chain of discovered functions
chain :: Lexeme -> FnSyntax -> FnSyntax
chain fn0 chain =
  case chain of
    FnChained{} -> FnChained fn0 chain
    FnValue{} -> FnChained fn0 chain
    FnIncomplete chain0 -> FnChained fn0 chain0
    EmptyStatement -> FnValue fn0
    FnNotFound prefix -> FnValue fn0

data FnSyntax = -- a.b.c
                FnChained{
                 fnName:: Lexeme, 
                 fnChain:: FnSyntax -- The chain's return type is the function's first argument type
                } 
              | FnIncomplete{fnChain::FnSyntax} -- a.b.
              | FnValue{fnValue:: Lexeme} -- a / ] / 10 / "abc" / etc
              | FnNotFound{fnPrefix:: [Lexeme]}
              | EmptyStatement -- start of line

instance Show FnSyntax where
  show fn =
    case fn of
      FnChained fn chain -> show fn ++ "." ++ show chain
      FnIncomplete chain -> show chain ++ "."
      FnValue fn -> show fn
      FnNotFound prefix -> show (length prefix) ++ ":" ++ show (take 6 prefix)
      EmptyStatement -> "EmptyStatement"

-- Assumes reverse ordered lexemes
-- Gets the current statement (e.g. up to the last ; or implicit ;, accounting for nesting, and blocks)
-- Ignores statements within nested blocks
getCurrentStatementReverse :: [Lexeme] -> [Lexeme]
getCurrentStatementReverse xs =
  let go :: Int -> [Lexeme] -> [Lexeme]
      go blockn xs =
        case xs of
          [] -> []
          (Lexeme _ LexInsSemi):xs | blockn == 0 -> []
          (Lexeme _ (LexSpecial ";"):xs) | blockn == 0 -> []
          x@(Lexeme _ (LexSpecial "}")):xs -> x:go (blockn + 1) xs
          x@(Lexeme _ LexInsRCurly):xs -> x:go (blockn + 1) xs
          x@(Lexeme _ (LexSpecial "{")):xs -> x:go (blockn - 1) xs
          x@(Lexeme _ LexInsLCurly):xs -> x:go (blockn - 1) xs
          x:xs -> x:go blockn xs
  in go 0 (getCurrentBlockReverse xs)

-- Gets the current block of syntax (e.g. up to the last { or implicit {, accounting for nesting)
getCurrentBlockReverse :: [Lexeme] -> [Lexeme]
getCurrentBlockReverse xs =
  let go n xs =
        case xs of
          [] -> []
          (Lexeme _ (LexSpecial "{"):xs) | n == 0 -> []
          (Lexeme _ LexInsLCurly):xs | n == 0-> []
          x@(Lexeme _ (LexSpecial "}")):xs -> x:go (n + 1) xs
          x@(Lexeme _ LexInsRCurly):xs -> x:go (n + 1) xs
          x@(Lexeme _ (LexSpecial "{")):xs -> x:go (n - 1) xs
          x@(Lexeme _ LexInsLCurly):xs -> x:go (n - 1) xs
          x:xs -> x:getCurrentBlockReverse xs
  in go 0 xs

-- Drops to a matching lexeme using `isStartLex` and `isEndLex` to detect nested lexemes
-- Assumes the first lexeme is already a start lexeme
dropToLexMatching :: (Lex -> Bool) -> (Lex -> Bool) -> [Lexeme] -> [Lexeme]
dropToLexMatching = dropToLexMatchingN 1

dropToLexMatchingN :: Int -> (Lex -> Bool) -> (Lex -> Bool) -> [Lexeme] -> [Lexeme]
dropToLexMatchingN n isStartLex isEndLex xs =
  case xs of
    [] -> []
    (Lexeme _ l):xs | isStartLex l -> dropToLexMatchingN (n + 1) isStartLex isEndLex xs
    (Lexeme _ l):xs | isEndLex l && n > 1 -> dropToLexMatchingN (n - 1) isStartLex isEndLex xs
    (Lexeme _ l):xs | isEndLex l && n == 1 -> xs -- dropping from 1 to 0
    (Lexeme _ l):xs -> dropToLexMatchingN n isStartLex isEndLex xs

-- Assumes reverse ordered lexemes dropping till we get to actual written code
dropAutoGenClosing :: [Lexeme] -> [Lexeme]
dropAutoGenClosing lexes =
  case lexes of
    [] -> []
    (Lexeme _ LexInsSemi):xs -> dropAutoGenClosing xs
    (Lexeme _ LexInsRCurly):xs -> dropAutoGenClosing xs
    _ -> lexes

-- we should use the lexemes to find the right start token
rangeMapFindAt :: [Lexeme] -> Pos -> RangeMap -> Maybe (Range, RangeInfo)
rangeMapFindAt lexemes pos (RM rm)
  = let lexStart  = case dropWhile (\lex -> not (rangeContains (getRange lex) pos)) lexemes of
                      (lex:_) -> rangeStart (getRange lex)
                      []      -> pos
        rinfos    = takeWhile (\(rng,_) -> rangeStart rng == lexStart) $
                    dropWhile (\(rng,_) -> rangeStart rng < lexStart) rm
    in  {- trace ("range map find at: " ++ show pos ++ "\n"
               ++ "start pos: " ++ show lexStart ++ "\n"
               ++ "rinfos: " ++ show rinfos ++ "\n"
               ++ "prioritized: " ++ show (prioritize rinfos)
               -- ++ unlines (map show lexemes)
               -- ++ unlines (map show rm)
             ) $ -}
        maybeHead (prioritize rinfos)

lexemesFromPos :: Pos -> [Lexeme] -> [Lexeme]
lexemesFromPos pos lexes = dropWhile (\lex -> not (rangeContains (getRange lex) pos)) lexes

maybeHead []    = Nothing
maybeHead (x:_) = Just x


rangeMapFind :: Range -> RangeMap -> [(Range, RangeInfo)]
rangeMapFind rng (RM rm)
  = filter ((== rng) . fst) rm

minimumByList :: Foldable t => (a -> a -> Ordering) -> t a -> [a]
minimumByList cmp la = fromMaybe [] (foldl' min' Nothing la)
  where
    min' mx y = Just $! case mx of
      Nothing -> [y]
      Just (x:xs) -> case cmp x y of
        GT -> [y]
        EQ -> y:x:xs
        _ -> x:xs

rangeInfoType :: RangeInfo -> Maybe Type
rangeInfoType ri
  = case ri of
      Id _ info _ _ -> case info of
                          NIValue tp _ _  -> Just tp
                          NICon tp _      -> Just tp
                          _               -> Nothing
      _ -> Nothing

rangeInfoDoc :: RangeInfo -> Maybe String
rangeInfoDoc ri
  = case ri of
      Id _ info _ _ -> case info of
                         NIValue _ doc _ -> Just doc
                         NICon _ doc  -> Just doc

      _ -> Nothing

instance HasTypeVar RangeMap where
  sub `substitute` (RM rm)
    = RM (map (\(r,ri) -> (r,sub `substitute` ri)) rm)

  ftv (RM rm)
    = ftv (map snd rm)

  btv (RM rm)
    = btv (map snd rm)

instance HasTypeVar RangeInfo where
  sub `substitute` (Id nm info docs isdef)  = Id nm (sub `substitute` info) docs isdef
  sub `substitute` ri                       = ri

  ftv (Id nm info _ _) = ftv info
  ftv ri               = tvsEmpty

  btv (Id nm info _ _) = btv info
  btv ri               = tvsEmpty

instance HasTypeVar NameInfo where
  sub `substitute` ni
    = case ni of
        NIValue tp annotated doc  -> NIValue (sub `substitute` tp) annotated doc
        NICon tp doc   -> NICon (sub `substitute` tp) doc
        _           -> ni

  ftv ni
    = case ni of
        NIValue tp _ _ -> ftv tp
        NICon tp _   -> ftv tp
        _           -> tvsEmpty

  btv ni
    = case ni of
        NIValue tp _ _  -> btv tp
        NICon tp _    -> btv tp
        _           -> tvsEmpty

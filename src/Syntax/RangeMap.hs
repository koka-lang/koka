------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
module Syntax.RangeMap( RangeMap, RangeInfo(..), NameInfo(..) 
                      , rangeMapNew
                      , rangeMapInsert
                      , rangeMapSort
                      , rangeMapLookup
                      , rangeMapAppend
                      , mangle
                      , mangleConName
                      , mangleTypeName
                      ) where

-- import Lib.Trace
import Data.Char    ( isSpace )
import Common.Failure 
import Data.List    (sortBy, groupBy)
import Lib.PPrint
import Common.Range
import Common.Name
import Common.NamePrim (nameUnit, nameNull, isNameTuple)
import Type.Type
import Kind.Kind
import Type.TypeVar
import Type.Pretty() 

newtype RangeMap = RM [(Range,RangeInfo)]

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
  = Decl String Name Name      -- alias, type, cotype, rectype, fun, val
  | Block String      -- type, kind, pattern
  | Error Doc
  | Warning Doc 
  | Id Name NameInfo Bool  -- qualified name, info, is the definition

data NameInfo
  = NIValue   Type
  | NICon     Type
  | NITypeCon Kind
  | NITypeVar Kind
  | NIModule
  | NIKind  

instance Show RangeInfo where
  show ri 
    = case ri of
        Decl kind nm1 nm2 -> "Decl " ++ kind ++ " " ++ show nm1 ++ " " ++ show nm2
        Block kind -> "Block " ++ kind
        Error doc  -> "Error"
        Warning doc -> "Warning"
        Id name info isDef -> "Id " ++ show name ++ (if isDef then " (def)" else "")
        
instance Enum RangeInfo where
  fromEnum r
    = case r of
        Decl _ _ _ -> 0
        Block _    -> 1
        Error _    -> 4
        Warning _  -> 3
        Id _ _ _     -> 2

  toEnum i
    = failure "Syntax.RangeMap.toEnum"

isHidden ri 
  = case ri of
      Decl kind nm1 nm2  -> isHiddenName nm1
      Id name info isDef -> isHiddenName name
      _ -> False


rangeMapNew :: RangeMap
rangeMapNew 
  = RM []

cut r
  = (makeRange (rangeStart r) (rangeStart r))

rangeMapInsert :: Range -> RangeInfo -> RangeMap -> RangeMap
rangeMapInsert r info (RM rm)
  = -- trace ("insert: " ++ showFullRange (r) ++ ": " ++ show info) $
    if isHidden info 
     then RM rm 
    else if beginEndToken info 
     then RM ((r,info):(makeRange (rangeEnd r) (rangeEnd r),info):rm)
     else RM ((r,info):rm)
  where 
    beginEndToken info
      = case info of
          Id name _ _ -> (name == nameUnit || name == nameNull || isNameTuple name)
          _ -> False

rangeMapAppend :: RangeMap -> RangeMap -> RangeMap
rangeMapAppend (RM rm1) (RM rm2)
  = RM (rm1 ++ rm2)

rangeMapSort :: RangeMap -> RangeMap
rangeMapSort (RM rm)
  = RM (sortBy (\(r1,_) (r2,_) -> compare r1 r2) rm)

rangeMapLookup :: Range -> RangeMap -> ([(Range,RangeInfo)],RangeMap)
rangeMapLookup r (RM rm)
  = let (rinfos,rm') = span startsAt (dropWhile isBefore rm)
    in -- trace ("lookup: " ++ showFullRange r ++ ": " ++ show (length rinfos)) $
       (prioritize rinfos, RM rm')
  where
    pos = rangeStart r
    isBefore (rng,_)  = rangeStart rng < pos
    startsAt (rng,_)  = rangeStart rng == pos

    prioritize rinfos
      = map last
        (groupBy eq (sortBy cmp rinfos))
      where
        eq x y              = (EQ == cmp x y)
        cmp (_,ri1) (_,ri2) = compare (fromEnum ri1) (fromEnum ri2)


instance HasTypeVar RangeMap where
  sub `substitute` (RM rm)
    = RM (map (\(r,ri) -> (r,sub `substitute` ri)) rm)

  ftv (RM rm)
    = ftv (map snd rm)

  btv (RM rm)
    = btv (map snd rm)

instance HasTypeVar RangeInfo where
  sub `substitute` (Id nm info isdef)  = Id nm (sub `substitute` info) isdef
  sub `substitute` ri            = ri

  ftv (Id nm info _) = ftv info
  ftv ri             = tvsEmpty

  btv (Id nm info _) = btv info
  btv ri             = tvsEmpty

instance HasTypeVar NameInfo where
  sub `substitute` ni
    = case ni of
        NIValue tp  -> NIValue (sub `substitute` tp)
        NICon tp    -> NICon (sub `substitute` tp)
        _           -> ni

  ftv ni
    = case ni of
        NIValue tp  -> ftv tp
        NICon tp    -> ftv tp
        _           -> tvsEmpty

  btv ni
    = case ni of
        NIValue tp  -> btv tp
        NICon tp    -> btv tp
        _           -> tvsEmpty

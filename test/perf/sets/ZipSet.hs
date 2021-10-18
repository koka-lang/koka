{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module ZipSet
  ( ZipSet
  , empty
  , member
  , insert
  ) where

import Data.Bits
import Data.Word
import GHC.Exts (lazy, isTrue#, reallyUnsafePtrEquality#)

data ZipSet a
  = ZipSet {-# UNPACK #-} !RandState !(ZipTree a)
  deriving (Eq, Ord, Show)

data ZipTree a
  = Bin {-# UNPACK #-} !Word32 !a !(ZipTree a) !(ZipTree a)
  | Tip
  deriving (Eq, Ord, Show)

empty :: ZipSet a
empty = ZipSet (RandState 2020832078 196622394 4171630633 291524055) Tip

member :: Ord a => a -> ZipSet a -> Bool
member x (ZipSet _ t) = go x t
  where
    go !_ Tip = False
    go x (Bin _ y l r) = case compare x y of
      LT -> go x l
      GT -> go x r
      EQ -> True

{-# INLINABLE member #-}

insert :: Ord a => a -> ZipSet a -> ZipSet a
insert x0 (ZipSet randState tree) =
  let (rank, randState') = next randState
  in ZipSet randState' (go (geo rank) x0 x0 tree)
  where
    go :: Ord a => Word32 -> a -> a -> ZipTree a -> ZipTree a
    go !rank orig !_ Tip = Bin rank (lazy orig) Tip Tip
    go !rank orig !x t@(Bin sz y l r) =
      case compare x y of
        LT | l' `ptrEq` l -> t
           | rank < sz -> Bin sz y l' r
           | otherwise -> Bin rank (lazy orig) ll (Bin sz y lr r)
          where l'@(Bin _ _ ll lr) = go rank orig x l
        GT | r' `ptrEq` r -> t
           | rank <= sz -> Bin sz y l r'
           | otherwise -> Bin rank (lazy orig) (Bin sz y l rl) rr
          where r'@(Bin _ _ rl rr) = go rank orig x r
        EQ | lazy orig `seq` (orig `ptrEq` y) -> t
           | otherwise -> Bin sz (lazy orig) l r

{-# INLINABLE insert #-}

ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)

fromList :: Ord a => [a] -> ZipSet a
fromList = foldr insert empty

test :: ZipSet Char
test = fromList "DFSKXHMGJL"

-- | Sample a geometric distribution from a uniform 32 bit uint.
-- The lower bits of xoshiros output don't have good quality,
-- but it doesn't seem to be a problem in practice.
geo :: Word32 -> Word32
geo !x0 = lowestBit x0

lowestBit :: Word32 -> Word32
lowestBit !x0 = x0 .&. (negate x0)

{-# INLINABLE lowestBit #-}

highestBit :: Word32 -> Word32
highestBit !x0 =
  let !x1 = x0 .|. (x0 `shiftR`  1)
      !x2 = x1 .|. (x1 `shiftR`  2)
      !x3 = x2 .|. (x2 `shiftR`  4)
      !x4 = x3 .|. (x3 `shiftR`  8)
      !x5 = x4 .|. (x4 `shiftR` 16)
  in x5 `xor` (x5 `shiftR` 1)

{-# INLINABLE highestBit #-}

-- | xoshiro128++ 1.0 by Blackman and Vigna, public domain (CC0)
-- Adapted from https://prng.di.unimi.it/xoshiro128plusplus.c

data RandState = RandState
  { s0 :: {-# UNPACK #-} !Word32
  , s1 :: {-# UNPACK #-} !Word32
  , s2 :: {-# UNPACK #-} !Word32
  , s3 :: {-# UNPACK #-} !Word32
  } deriving (Eq, Ord, Show)

rotl :: Word32 -> Int -> Word32
rotl !x !k = (x `shiftL` k) .|. (x `shiftR` (32 - k))

next :: RandState -> (Word32, RandState)
next !(RandState s0 s1 s2 s3) =
  let !result = rotl (s0 + s3) 7 + s0
      !t = s1 `shiftL` 9
      !s2' = s2 `xor` s0
      !s3' = s3 `xor` s1
      !s1' = s1 `xor` s2'
      !s0' = s0 `xor` s3'
      !s2'' = s2' `xor` t
      !s3'' = rotl s3' 11
  in (result, RandState s0' s1' s2'' s3'')
-- Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.hs
-- Modified to be strict in the Tree fields
--
-- This is a bottom up implementation following Lorenzen and Leijen's "frame limited reuse" implementation (ICFP'22)
import System.Environment

import Data.Bits
import Data.Int
import Data.Word

data Color = Red | Black

data Tree = Leaf
          | Node !Color !Tree !Int !Int !Tree

data Zipper = Done
            | NodeR !Color !Tree !Int !Int !Zipper
            | NodeL !Color !Zipper !Int !Int !Tree

key :: Tree -> Int
key (Node c l k v r) = k
key Leaf             = 0

is_red t =
  case t of
    Node Red _ _ _ _ -> True
    _ -> False

set_black n =
   case n of
    Node _ l k v r -> Node Black l k v r
    e              -> e

rebuild z t =
   case z of
    NodeR c l k v up -> rebuild up (Node c l k v t)
    NodeL c up k v r -> rebuild up (Node c t k v r)
    Done             -> t
 
balance z l k v r =  
   case z of
    NodeR Black l1 k1 v1 z1 -> rebuild z1 (Node Black l1 k1 v1 (Node Red l k v r))
    NodeL Black z1 k1 v1 r1 -> rebuild z1 (Node Black (Node Red l k v r) k1 v1 r1)
   -- red red violation 
    NodeR Red l1 k1 v1 z1 -> case z1 of
      NodeR _ l2 k2 v2 z2 -> balance z2 (Node Black l2 k2 v2 l1) k1 v1 (Node Black l k v r)
      NodeL _ z2 k2 v2 r2 -> balance z2 (Node Black l1 k1 v1 l) k v (Node Black r k2 v2 r2)
      Done                -> Node Black l1 k1 v1 (Node Red l k v r)    
    NodeL Red z1 k1 v1 r1 -> case z1 of
      NodeR _ l2 k2 v2 z2 -> balance z2 (Node Black l2 k2 v2 l) k v (Node Black r k1 v1 r1)
      NodeL _ z2 k2 v2 r2 -> balance z2 (Node Black l k v r) k1 v1 (Node Black r1 k2 v2 r2)
      Done                -> Node Black (Node Red l k v r) k1 v1 r1
    Done -> Node Black l k v r

find :: Tree -> Int -> Int -> Zipper -> Tree  
find t k v z =
   case t of
    Node c l kx vx r -> if (kx < k) then find r k v (NodeR c l kx vx z)
                          else if (kx > k) then find l k v (NodeL c z kx vx r)
                                           else rebuild z (Node c l k v r)
    Leaf -> balance z Leaf k v Leaf


--------------------------------------------------------------------------------------
-- Benchmarking

access :: Tree -> Int -> Tree
access t k
  = find t k k Done

sumAcc :: Tree -> Int -> Int
sumAcc Leaf acc  = acc
sumAcc (Node _ l x _ r) acc = sumAcc r $! (sumAcc l $! (acc + x))

tsum :: Tree -> Int
tsum t = sumAcc t 0

data Sfc = Sfc !Int32 !Int32 !Int32 !Int32


shiftRightLogical :: Int32 -> Int -> Int32
shiftRightLogical i s = fromIntegral (fromIntegral i `shiftR` s :: Word32)

minHeight (Node _ l _ _ r) = 1 + min (minHeight l) (minHeight r)
minHeight Leaf             = 0

maxHeight (Node _ l _ _ r) = 1 + max (maxHeight l) (maxHeight r)
maxHeight Leaf             = 0

top (Node _ _ x _ _) = x
top Leaf             = 0

sfcStep :: Sfc -> (Int,Sfc)
sfcStep (Sfc x y z cnt)
  = let res = x+y+cnt 
        sfc = Sfc (xor y (shiftRightLogical y 9))
                  (z + (shiftL z 3))
                  ((rotateL z 21) + res)
                  (cnt + 1)
    in seq sfc $ seq res $ (fromIntegral res, sfc)

sfcInit :: Int -> Int -> Sfc
sfcInit seed1 seed2
  = let sfc0 = Sfc 0 (fromIntegral seed1) (fromIntegral seed2) 1
        sfc1 = foldl (\s i -> snd (sfcStep s)) sfc0 [1..12]
    in seq sfc1 sfc1


modE :: Int -> Int -> Int
modE i j
  = let m = i `mod` j
    in if (i < 0 && m < 0) 
        then (if (j < 0) then m - j else m + j)
        else m

test :: Int -> Int -> (Tree -> Int -> Tree) -> (Tree,Int)
test n iter access
  = iloop 0 Leaf (sfcInit 42 43)
  where 
    iloop :: Int -> Tree -> Sfc -> (Tree,Int)
    iloop i t sfc
      = if (i >= iter) 
         then (t, fst (sfcStep sfc))
         else let (t',sfc') = nloop 0 t sfc
              in iloop (i+1) t' sfc'

    nloop i t sfc
      = if (i >= n) 
          then (t,sfc)
          else let (x,sfc') = sfcStep sfc
                   t' = access t $! (modE x n)
               in seq (key t') $ seq sfc' $
                  nloop (i+1) t' sfc'

benchMain :: (Tree -> Int -> Tree) -> IO ()
benchMain access
  = do args <- getArgs 
       let n = case args of
                 arg:_ -> read arg
                 _     -> 100000
       let sfc0 = sfcInit 42 43
       -- putStrLn ("init: " ++ show (fst (sfcStep sfc0)))
       let (t,final) = test n 100 access
       putStrLn ("sum: " ++ show (tsum t) ++ ", height: " ++ show (maxHeight t) ++ "/" ++ show (minHeight t) ++ ", top: " ++ show (top t) ++ ", final acces: " ++ show final)


main :: IO ()
main 
  = benchMain access

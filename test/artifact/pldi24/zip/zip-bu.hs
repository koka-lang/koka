import System.Environment

import Data.Bits
import Data.Int
import Data.Word

type Key = Int
type Rank = Int

data Tree =
  Leaf
  | Node Rank Tree Key Tree

data Zipper =
     Done
  | NodeR Rank Tree Key Zipper
  | NodeL Rank Zipper Key Tree


key :: Tree -> Key
key (Node _ _ k _) = k
key Leaf           = 0

is_higher_rank r1 k1 r2 k2 =
  (r1 > r2) || (r1 == r2 && k1 < k2)

rebuild :: Zipper -> Tree -> Tree
rebuild z t =
  case z of
    NodeR rnk l x up -> rebuild up (Node rnk l x t)
    NodeL rnk up x r -> rebuild up (Node rnk t x r)
    Done             -> t

tunzip :: Tree -> Key -> Zipper -> Zipper -> (Tree,Tree)
tunzip t k zs zb =
  case t of
    Node rnk l x r  -> if (x < k) then tunzip r k (NodeR rnk l x zs) zb
                       else if (x > k) then tunzip l k zs (NodeL rnk zb x r)
                       else (rebuild zs l, rebuild zb r)
    Leaf -> (rebuild zs Leaf, rebuild zb Leaf)

find :: Tree -> Rank -> Key -> Zipper -> Tree
find t rank k z =
  case t of
    Node rnk l x r  | is_higher_rank rnk x rank k
      -> if (x < k) then find r rank k (NodeR rnk l x z)
                    else find l rank k (NodeL rnk z x r)
    Node _ _ x _  | x == k -> rebuild z t
    _ -> let (s,b) = tunzip t k Done Done 
         in rebuild z (Node rank s k b)


--------------------------------------------------------------------------------------
-- Benchmarking

access :: Tree -> Int -> Tree
access t k
  = find t (rank_of k) k Done

sumAcc :: Tree -> Int -> Int
sumAcc Leaf acc  = acc
sumAcc (Node _ l x r) acc = sumAcc r $! (sumAcc l $! (acc + x))

tsum :: Tree -> Int
tsum t = sumAcc t 0

data Sfc = Sfc !Int32 !Int32 !Int32 !Int32


shiftRightLogical :: Int32 -> Int -> Int32
shiftRightLogical i s = fromIntegral (fromIntegral i `shiftR` s :: Word32)

    
rank_of1 :: Key -> Rank
rank_of1 k =
  let x0 = fromIntegral k 
      x1 = xor x0 (shiftRightLogical x0 15) * 0x2c1b3c6d
      x2 = xor x1 (shiftRightLogical x1 12) * 0x297a2d39
      x3 = xor x2 (shiftRightLogical x2 15)
  in countTrailingZeros x3

rank_of :: Key -> Rank
rank_of k =
  let x0 = fromIntegral k + 1
      x1 = xor x0 (shiftRightLogical x0 16) * 0x297a2d39
      x2 = xor x1 (shiftRightLogical x1 16)
  in countTrailingZeros x2


minHeight (Node _ l _ r) = 1 + min (minHeight l) (minHeight r)
minHeight Leaf           = 0

maxHeight (Node _ l _ r) = 1 + max (maxHeight l) (maxHeight r)
maxHeight Leaf           = 0

top (Node _ _ x _) = x
top Leaf           = 0

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

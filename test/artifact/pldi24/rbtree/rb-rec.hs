-- Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.hs
-- Modified to be strict in the Tree fields
--
-- This is a recursive implementation
import System.Environment

import Data.Bits
import Data.Int
import Data.Word


data Color =
  Red | Black

data Tree α β =
  Leaf
  | Node !Color !(Tree α β) !α !β !(Tree α β)

key (Node _ _ k _ _) = k
  

fold :: (α -> β  -> σ  -> σ) -> Tree α β -> σ  -> σ
fold _ Leaf b               = b
fold f (Node _ l k v r)   b = fold f r (f k v (fold f l b))

balance1 :: Tree α β -> Tree α β -> Tree α β
balance1 (Node _ _ kv vv t) (Node _ (Node Red l kx vx r₁) ky vy r₂) = Node Red (Node Black l kx vx r₁) ky vy (Node Black r₂ kv vv t)
balance1 (Node _ _ kv vv t) (Node _ l₁ ky vy (Node Red l₂ kx vx r)) = Node Red (Node Black l₁ ky vy l₂) kx vx (Node Black r kv vv t)
balance1 (Node _ _ kv vv t) (Node _ l  ky vy r)                     = Node Black (Node Red l ky vy r) kv vv t
balance1 _                                                        _ = Leaf

balance2 :: Tree α β -> Tree α β -> Tree α β
balance2 (Node _ t kv vv _) (Node _ (Node Red l kx₁ vx₁ r₁) ky vy r₂)  = Node Red (Node Black t kv vv l) kx₁ vx₁ (Node Black r₁ ky vy r₂)
balance2 (Node _ t kv vv _) (Node _ l₁ ky vy (Node Red l₂ kx₂ vx₂ r₂)) = Node Red (Node Black t kv vv l₁) ky vy (Node Black l₂ kx₂ vx₂ r₂)
balance2 (Node _ t kv vv _) (Node _ l ky vy r)                         = Node Black t kv vv (Node Red l ky vy r)
balance2 _                                                        _    = Leaf

is_red :: Tree α β -> Bool
is_red (Node Red _ _ _ _) = True
is_red _                  = False

lt x y = x < y

ins :: Ord α => Tree α β -> α -> β -> Tree α β
ins Leaf                 kx vx = Node Red Leaf kx vx Leaf
ins (Node Red a ky vy b) kx vx =
   (if lt kx ky then Node Red (ins a kx vx) ky vy b
    else if lt ky kx then Node Red a ky vy (ins b kx vx)
    else Node Red a kx vx b)
ins (Node Black a ky vy b) kx vx =
    if lt kx ky then
      (if is_red a then balance1 (Node Black Leaf ky vy b) (ins a kx vx)
       else Node Black (ins a kx vx) ky vy b)
    else if lt ky kx then
      (if is_red b then balance2 (Node Black a ky vy Leaf) (ins b kx vx)
       else Node Black a ky vy (ins b kx vx))
    else Node Black a kx vx b

set_black :: Tree α β -> Tree α β
set_black (Node _ l k v r) = Node Black l k v r
set_black e                = e

insert t k v =
  if is_red t then set_black (ins t k v)
  else ins t k v




--------------------------------------------------------------------------------------
-- Benchmarking

access :: Tree Int Int -> Int -> Tree Int Int
access t k
  = insert t k k

sumAcc :: Tree Int Int -> Int -> Int
sumAcc Leaf acc  = acc
sumAcc (Node _ l x _ r) acc = sumAcc r $! (sumAcc l $! (acc + x))

tsum :: Tree Int Int -> Int
tsum t = sumAcc t 0

data Sfc = Sfc !Int32 !Int32 !Int32 !Int32


shiftRightLogical :: Int32 -> Int -> Int32
shiftRightLogical i s = fromIntegral (fromIntegral i `shiftR` s :: Word32)

    
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

test :: Int -> Int -> (Tree Int Int -> Int -> Tree Int Int) -> (Tree Int Int,Int)
test n iter access
  = iloop 0 Leaf (sfcInit 42 43)
  where 
    iloop :: Int -> Tree Int Int -> Sfc -> (Tree Int Int,Int)
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

benchMain :: (Tree Int Int -> Int -> Tree Int Int) -> IO ()
benchMain access
  = do args <- getArgs 
       let n = case args of
                 arg:_ -> read arg
                 _     -> 100000
       let sfc0 = sfcInit 42 43
       -- putStrLn ("init: " ++ show (fst (sfcStep sfc0)))
       let (t,final) = test n 100 access
       putStrLn ("sum: " ++ show (tsum t) ++ ", final acces: " ++ show final)


main :: IO ()
main 
  = benchMain access
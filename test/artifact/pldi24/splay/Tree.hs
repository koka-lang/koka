module Tree where

import System.Environment

import Data.Bits
import Data.Int
import Data.Word

type Key = Int

data Tree = Node{ left :: !Tree, key :: !Key, right :: !Tree }
          | Leaf


--------------------------------------------------------------------------------------
-- Benchmarking

sumAcc :: Tree -> Int -> Int
sumAcc Leaf acc  = acc
sumAcc (Node l x r) acc = sumAcc r $! (sumAcc l $! (acc + x))

tsum :: Tree -> Int
tsum t = sumAcc t 0

minHeight (Node l _ r) = 1 + min (minHeight l) (minHeight r)
minHeight Leaf           = 0

maxHeight (Node l _ r) = 1 + max (maxHeight l) (maxHeight r)
maxHeight Leaf           = 0

top (Node _ x _) = x
top Leaf           = 0


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


{-

pub inline fun bench( n : int, iter : int, access : (tree,key) -> tree ) : (int,tree)
  val acc0 = Acc(sfc-init32(42.int32,43.int32),Leaf)
  val accN = fold-int(0,iter,acc0) fn(i,acc1)
               val acc = fold-int(0,n,acc1) fn(_,acc2)
                            val step = sfc-step(acc2.sfc)
                            val t2 = acc2.tree.access((step.rnd.int % n).to-key) // todo: avoid conversion?
                            Acc(step.state,t2)
               //println("\ntree " ++ i.show); acc.tree.println
               acc
  (sfc-step(accN.sfc).rnd.int, accN.tree)

pub inline fun benchmain( access : (tree,key) -> tree, scaledown : int = 1) : io ()
  val n = get-args().head("").parse-int.default(100000)
  //val n = 50000
  val (i,t) = bench(n / scaledown, 100 / scaledown, access)
  println("sum: " ++ sum(t).show ++ ", final access: " ++ i.show) // ++ ", size: " ++ size(t).show)
  //t.print
-}
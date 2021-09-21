{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Data.Fixed
import Data.Time
import Data.Time.Clock.System
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified ZipSet as ZipSet
import Control.DeepSeq

data TestConfig = TestConfig
  { testsize :: !Int
  , maxRandNumber :: !Int
  }

printTestConfig :: TestConfig -> IO ()
printTestConfig (TestConfig ts mrn)
  = putStrLn $ "\nCounting unique integers among " ++ show ts ++ " random integers in the range [0, " ++ show mrn ++ "):"

data RandState = RandState
  { s1 :: !Int
  , s2 :: !Int
  , s3 :: !Int
  }

rand :: RandState -> (Double, RandState)
rand (RandState s1 s2 s3)
  = let s1' = (171 * s1) `mod` 30269
        s2' = (172 * s2) `mod` 30307
        s3' = (170 * s3) `mod` 30323
        r = (fromIntegral s1' / 30269.0 + fromIntegral s2' / 30307.0 + fromIntegral s3' / 30323.0) `mod'` 1.0
    in (r, RandState s1' s2' s3')

randList :: TestConfig -> [Int]
randList (TestConfig ts mrn)
  = go (RandState 19379 17844 5912) ts []
  where
    mrnd = fromIntegral mrn
    go st n l = if n <= 0 then l
      else let (r, st') = rand st
               !r' = round (mrnd * r)
           in go st' (n - 1) (r' : l)

countSet :: Int -> Int -> [Int] -> Int
countSet start end list
  = count start 0
  where
    set = fromList Set.empty list

    fromList s [] = s
    fromList s (x:xs) = fromList (Set.insert x s) xs

    count n !acc = if n <= end
      then if Set.member n set then count (n+1) (acc+1) else count (n+1) acc
      else acc

countZipSet :: Int -> Int -> [Int] -> Int
countZipSet start end list
  = count start 0
  where
    set = fromList ZipSet.empty list

    fromList s [] = s
    fromList s (x:xs) = fromList (ZipSet.insert x s) xs

    count n !acc = if n <= end
      then if ZipSet.member n set then count (n+1) (acc+1) else count (n+1) acc
      else acc

countIntSet :: Int -> Int -> [Int] -> Int
countIntSet start end list
  = count start 0
  where
    set = fromList IntSet.empty list

    fromList s [] = s
    fromList s (x:xs) = fromList (IntSet.insert x s) xs

    count n !acc = if n <= end
      then if IntSet.member n set then count (n+1) (acc+1) else count (n+1) acc
      else acc

printElapsed :: (() -> a) -> IO ()
printElapsed fn = do
  t0 <- getSystemTime
  t1 <- fn () `seq` getSystemTime
  let t0' = systemToUTCTime t0
  let t1' = systemToUTCTime t1
  print(t1' `diffUTCTime` t0')

main :: IO ()
main = do
  let tcs = [ TestConfig     1000 10000
            , TestConfig    10000 100000
            , TestConfig   100000 1000000
            , TestConfig  1000000 10000000
            , TestConfig 10000000 100000000]
  forM_ tcs (\tc -> do
    printTestConfig tc
    let !input = force $ randList tc
    let start = 1
    let end = testsize tc

    putStr "Data.IntSet: "
    printElapsed $ \_ -> countIntSet start end input
    putStr "Data.Set: "
    printElapsed $ \_ -> countSet start end input
    putStr "ZipSet: "
    printElapsed $ \_ -> countZipSet start end input)
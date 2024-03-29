{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Reading file times
-}
-----------------------------------------------------------------------------
module Platform.Filetime( FileTime
                       , getCurrentTime
                       , getFileTime
                       , getFileTimeOrCurrent
                       , setFileTime
                       , fileTime0
                       , fileTimeToPicoseconds
                       , showTimeDiff
                       ) where

import System.Directory( getModificationTime, setModificationTime )
import Platform.Runtime( exCatch )

#if __GLASGOW_HASKELL__ >= 706
import qualified Data.Time as T
import qualified Data.Ratio as R

type FileTime = T.UTCTime

getCurrentTime :: IO FileTime
getCurrentTime
  = T.getCurrentTime

fileTime0 :: FileTime
fileTime0
  = T.UTCTime (T.ModifiedJulianDay 0) (T.secondsToDiffTime 0)

showTimeDiff :: FileTime -> FileTime -> String
showTimeDiff t1 t0
  = show (T.diffUTCTime t1 t0)

fileTimeToPicoseconds :: FileTime -> Integer
fileTimeToPicoseconds t
  = diffTimeToPicoseconds (T.utctDayTime t)  -- diffTimeToPicoseconds is not always defined on older installations

diffTimeToPicoseconds :: T.DiffTime -> Integer
diffTimeToPicoseconds t
  = R.numerator (toRational t * 1000000000000)  -- convert from pico-second rational

#else
import qualified System.Time as T

type FileTime = T.ClockTime

getCurrentTime :: IO FileTime
getCurrentTime
  = T.getClockTime

fileTime0 :: FileTime
fileTime0
  = T.TOD 0 0

fileTimeToPicoseconds :: FileTime -> Integer
fileTimeToPicoseconds t
  case t of
    T.TOD _ psecs -> psecs

#endif

-- | Returns the file modification time or 0 if it does not exist.
getFileTime :: FilePath -> IO FileTime
getFileTime fname
  = do getModificationTime fname
    `exCatch` \_ -> do -- putStrLn $ "filetime: 0 for : " ++ fname
                       return fileTime0

-- | Set the file modification time
setFileTime :: FilePath -> FileTime -> IO ()
setFileTime fname ftime
 = setModificationTime fname ftime

-- | returns the file modification time or the current time if it does not exist.
getFileTimeOrCurrent :: FilePath -> IO FileTime
getFileTimeOrCurrent fname
  = do getModificationTime fname
    `exCatch` \_ -> getCurrentTime

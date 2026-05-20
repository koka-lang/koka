-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Internal errors and assertions.
-}
-----------------------------------------------------------------------------
module Common.File(
                  -- * System
                    getEnvPaths, getEnvVar
                  , searchPaths, searchPathsSuffixes, searchPathsEx, searchPathsCanonical
                  , getMaximalPrefixPath
                  , searchProgram
                  , runSystem, runSystemRaw, runCmd, runCmdRead, runCmdEnv
                  , getProgramPath

                  -- * Strings
                  , startsWith, endsWith, splitOn, trim

                  -- * File names
                  , FileName
                  , basename, notdir, notext, joinPath, joinPaths, extname, dirname, noexts
                  , splitPath, undelimPaths
                  , isPathSep, isPathDelimiter
                  , findMaximalPrefixPath
                  , isAbsolute
                  , commonPathPrefix
                  , normalizeWith, normalize
                  , isLiteralDoc
                  , ensureExt

                  -- * Files
                  , FileTime, fileTime0, maxFileTime, maxFileTimes, showTimeDiff
                  , fileTimeCompare, getFileTime
                  , getFileTimeOrCurrent, getCurrentTime
                  , readTextFile, writeTextFile
                  , copyTextFile, copyTextIfNewer, copyTextIfNewerWith, copyTextFileWith
                  , copyBinaryFile, copyBinaryIfNewer, copyExeFile
                  , removeFileIfExists
                  , realPath
                  , doesFileExistAndNotEmpty
                  , makeRelativeToPaths
                  , getCwd
                  , relativeToPath
                  , seqList, seqMaybe, seqEither, seqTuple2, seqString
                  , seqqList, seqqMaybe, seqqEither, seqqTuple2, seqqString
                  ) where

import Debug.Trace
import Data.Maybe       ( isNothing )
import Data.List        ( intersperse, isPrefixOf, maximumBy )
import Data.Char        ( toLower, isSpace )
import Platform.Config  ( pathSep, pathDelimiter, sourceExtension, exeExtension )
import Common.Failure   ( raiseIO, catchIO )

-- Platform-specific IO primitives (no CPP needed!)
import Platform.FileIO  ( doesFileExist, doesDirectoryExist, createDirectoryIfMissing
                        , readTextFile, writeTextFile
                        , readBinaryContents, writeBinaryContents, copyBinaryContents
                        , copyBinaryFileWithMetaData
                        , removeFileIfExists
                        , getCwd, realPath
                        , getEnvVar, getEnvPaths, getProgramPath
                        , runSystem, runSystemRaw, runCmd, runCmdRead, runCmdEnv
                        , getFileSize )
import Platform.Filetime


seqList :: [a] -> b -> b
seqList [] b     = b
seqList (x:xs) b = seq x (seqList xs b)

seqMaybe :: Maybe a -> b -> b
seqMaybe Nothing  b = b
seqMaybe (Just x) b = seq x b

seqEither :: Either a b -> c -> c
seqEither (Left x) z  = seq x z
seqEither (Right y) z = seq y z

seqTuple2 :: (a,b) -> c -> c
seqTuple2 (x,y) z  = seq x (seq y z)

seqString :: String -> a -> a
seqString s z = if null s then z else seq (last s) z

-- use seqqXXX when assigning to a field that is already strict
seqqList :: [a] -> [a]
seqqList xs  = seqList xs xs

seqqMaybe :: Maybe a -> Maybe a
seqqMaybe x  = seqMaybe x x

seqqEither :: Either a b -> Either a b
seqqEither x  = seqEither x x

seqqTuple2 :: (a,b) -> (a,b)
seqqTuple2 x  = seqTuple2 x x

seqqString :: String -> String
seqqString s = seqString s s

startsWith, endsWith :: String -> String -> Bool
startsWith s  [] = True
startsWith [] _  = False
startsWith (c:cs) (p:ps) = if (p==c) then startsWith cs ps else False

endsWith s post
  = startsWith (reverse s) (reverse post)

splitOn pred xs
  = normalize [] xs
  where
    normalize acc [] = reverse acc
    normalize acc xs
      = case (span (not . pred) xs) of
          (pre,post) -> normalize (pre:acc) (dropWhile pred post)


trim s
  = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace $ s

isLiteralDoc :: FileName -> Bool
isLiteralDoc fname
  = endsWith fname (sourceExtension ++ ".md") ||
    endsWith fname (sourceExtension ++ ".mdk")

{--------------------------------------------------------------------------
  File names
--------------------------------------------------------------------------}
-- | File name
type FileName = FilePath

-- | Remove the extension and directory part
basename :: FileName -> FileName
basename fname
  = case dropWhile (/='.') (reverse (notdir fname)) of
      '.':rbase -> reverse rbase
      _         -> notdir fname


-- | Get the file extension
extname :: FileName -> FileName
extname fname
  = let (pre,post) = span (/='.') (reverse (notdir fname))
    in if null post
        then ""
        else ("." ++ reverse pre)

ensureExt :: FileName -> String -> FileName
ensureExt fname ext
  = if (extname fname == ext) then fname else fname ++ ext

-- | Return the directory prefix (including last separator if present)
dirname :: FileName -> FileName
dirname fname
  = joinPaths (init (splitPath fname))

-- | Remove the directory prefix
notdir :: FileName -> FileName
notdir fname
  = last (splitPath fname)


notext :: FileName -> FileName
notext fname
  = reverse (drop (length (extname fname)) (reverse fname))

noexts :: FileName -> FileName
noexts fname
  = joinPaths [dirname fname, takeWhile (/='.') (notdir fname)]

-- | Split a (semi-)colon separated list of directories into a directory list
undelimPaths :: String -> [FilePath]
undelimPaths xs
  = filter (not . null) (normalize [] "" xs)
  where
    -- initial spaces
    normalize ps "" (c:cs)  | isSpace c
      = normalize ps "" cs
    -- directory on windows
    normalize ps "" (c:':':cs)
      = normalize ps (':':c:[]) cs
    -- normal
    normalize ps p xs
      = case xs of
          []     -> if (null p)
                     then reverse ps
                     else reverse (reverse p:ps)
          (c:cs) | isPathDelimiter c -> normalize (reverse p:ps) "" cs
                 | otherwise         -> normalize ps (c:p) cs

-- | Split a path into its directory parts
splitPath :: FilePath -> [FilePath]
splitPath fdir
  = case normalize fdir of
      "" -> [""]
      '/':'/':fs -> "//":split fs
      '/':fs -> "/":split fs
      fs -> split fs
  where
    split f = splitOn isPathSep f

onWindows :: Bool
onWindows
  = (exeExtension == ".exe")


joinPath :: FilePath -> FilePath -> FilePath
joinPath p1 p2
  = joinPaths [p1,p2]

-- | Join a list of paths into one path
joinPaths :: [FilePath] -> FilePath
joinPaths dirs
  = concat
  $ filterPathSepDup
  $ intersperse ['/']
  $ resolveDot
  $ concatMap splitPath
  $ filter (not . null) dirs
  where
    resolveDot []            = []
    resolveDot (p:".":ps)    = resolveDot (p:ps)
    resolveDot (p:"..":ps)   | p == "."  = resolveDot ("..":ps)
                            | p == ".." = p : resolveDot ("..":ps)
                            | otherwise = resolveDot ps
    resolveDot (p:ps)        = p : resolveDot ps


    filterPathSepDup (p:q:rest)  | endsWith p "/" && q == "/"  = filterPathSepDup (p : rest)
    filterPathSepDup (p:ps)      = p : filterPathSepDup ps
    filterPathSepDup []          = []

-- | Normalize path separators
normalize :: FilePath -> FilePath
normalize path
  = case normalizeWith '/' path of
      (c:':':'/':rest) -> [toLower c] ++ ":/" ++ rest  -- use lower case drive letters on windows
      npath            -> npath

-- | Normalize path separators with a specified path separator
normalizeWith :: Char -> FilePath -> FilePath
normalizeWith newSep path
  = norm "" path
  where
    norm acc "" = reverse acc
    norm acc ('\\':c:cs) | isSpace c -- escaped space on unix/macos
      = norm (c:'\\':acc) cs
    norm acc (c:cs)
      = if (isPathSep c)
         then norm (newSep:acc) cs
         else norm (c:acc) cs


-- | Is this a file separator?
isPathSep :: Char -> Bool
isPathSep c
  = (c == '/' || c == '\\')

-- | Is this a path delimiter? (@;@ (and @:@ too on unix)
isPathDelimiter :: Char -> Bool
isPathDelimiter c
  = (c == ';' || c == pathDelimiter)

{--------------------------------------------------------------------------
  system
--------------------------------------------------------------------------}

-- | Compare two file modification times (uses 0 for non-existing files)
fileTimeCompare :: FilePath -> FilePath -> IO Ordering
fileTimeCompare fname1 fname2
  = do time1 <- getFileTime fname1
       time2 <- getFileTime fname2
       return (compare time1 time2)


maxFileTime :: FileTime -> FileTime -> FileTime
maxFileTime t1 t2
  = if (t1 >= t2) then t1 else t2

maxFileTimes :: [FileTime] -> FileTime
maxFileTimes times
  = foldr maxFileTime fileTime0 times

doesFileExistAndNotEmpty :: FilePath -> IO Bool
doesFileExistAndNotEmpty fpath
  = do exist <- doesFileExist fpath
       if exist
         then do fsize <- getFileSize fpath
                 return (fsize > 0)
         else return False

copyTextFile :: FilePath -> FilePath -> IO ()
copyTextFile src dest
  = copyTextFileWith src dest id

copyTextFileWith :: FilePath -> FilePath -> (String -> String) -> IO ()
copyTextFileWith src dest transform
  = if (src == dest)
     then return ()
     else catchIO (do createDirectoryIfMissing True (dirname dest)
                      ftime   <- getFileTime src
                      mbContent <- readTextFile src
                      case mbContent of
                        Just content -> do writeTextFile dest (transform content)
                                           setFileTime dest ftime
                        Nothing -> error ("could not read file " ++ show src))
            (error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyExeFile :: FilePath -> FilePath -> IO ()
copyExeFile src dest
  = if (src == dest)
     then return ()
     else catchIO (
            -- careful: keeps original file permissions
             do createDirectoryIfMissing True (dirname dest)
                copyBinaryFileWithMetaData src dest)
            (error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyBinaryFile :: FilePath -> FilePath -> IO ()
copyBinaryFile src dest
  = if (src == dest)
     then return ()
     else catchIO (
            -- do not use as the source may come from a (readonly) admin permission and should got to user permission
            -- copyBinaryFile src dest) (\_ -> error ("could not copy file " ++ show src ++ " to " ++ show dest))
             do createDirectoryIfMissing True (dirname dest)
                ftime <- getFileTime src
                copyBinaryContents src dest
                setFileTime dest ftime)
            (error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyBinaryIfNewer :: Bool -> FilePath -> FilePath -> IO ()
copyBinaryIfNewer always srcName outName
  = do ord <- if (srcName == outName) then return EQ
              else if always then return GT
              else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyBinaryFile srcName outName
        else do -- putStrLn $ "no copy for: " ++ srcName ++ " to " ++ outName
                return ()

copyTextIfNewer :: Bool -> FilePath -> FilePath -> IO ()
copyTextIfNewer always srcName outName
  = do ord <- if (srcName == outName) then return EQ
              else if always then return GT
              else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyTextFile srcName outName
        else do return ()

copyTextIfNewerWith :: Bool -> FilePath -> FilePath -> (String -> String) -> IO ()
copyTextIfNewerWith always srcName outName transform
  = do ord <- if (srcName == outName) then return EQ
              else if always then return GT
              else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyTextFileWith srcName outName transform
        else do return ()

commonPathPrefix :: FilePath -> FilePath -> FilePath
commonPathPrefix s1 s2
  = joinPaths $ map fst $ takeWhile (\(c,d) -> c == d) $ zip (splitPath s1) (splitPath s2)


relativeToPath :: FilePath -> FilePath -> FilePath
relativeToPath prefix path
  = case mbRelativeToPath prefix path of
      Just relpath -> relpath
      Nothing      -> path

mbRelativeToPath :: FilePath -> FilePath -> Maybe FilePath
mbRelativeToPath "" path = Just path
mbRelativeToPath prefix path
  = let prefixes = splitPath prefix
        paths    = splitPath path
    in if isPrefixOf prefixes paths then Just (joinPaths (drop (length prefixes) paths)) else Nothing


-- | Is a path absolute?
isAbsolute :: FilePath -> Bool
isAbsolute fpath
  = case fpath of
      (_:':':c:_) -> isPathSep c
      (c:_)       -> isPathSep c
      _           -> False


-- | Find a maximal prefix path given a path and list of root paths. Returns the root path and relative path.
findMaximalPrefixPath :: [FilePath] -> FilePath -> Maybe (FilePath,FilePath)
findMaximalPrefixPath roots p
  = let rels = concatMap (\r -> case mbRelativeToPath r p of
                                  Just rel -> [(r,rel)]
                                  _        -> []) roots
    in case rels of
      [] -> Nothing
      xs -> Just (maximumBy (\(root1,_) (root2,_) -> compare root1 root2) xs)

-- | Get the maximal relative path
getMaximalPrefixPath :: [FilePath] -> FilePath -> (FilePath,FilePath)
getMaximalPrefixPath roots p
  = case findMaximalPrefixPath roots p of
      Nothing   -> ("",p)
      Just just -> just

---------------------------------------------------------------
-- file searching
----------------------------------------------------------------

searchPaths :: [FilePath] -> [String] -> String -> IO (Maybe (FilePath))
searchPaths path exts name
  = searchPathsSuffixes path exts [] name

searchPathsSuffixes :: [FilePath] -> [String] -> [String] -> String -> IO (Maybe (FilePath))
searchPathsSuffixes paths exts suffixes name
  = fmap (fmap (\(root,name) -> joinPath root name)) (searchPathsEx paths (filter (not.null) exts) suffixes name)

searchPathsCanonical :: FilePath -> [FilePath] -> [String] -> [String] -> String -> IO (Maybe (FilePath,FilePath))
searchPathsCanonical relativeDir paths exts suffixes name
  = do searchPaths <- if isAbsolute name then return [""]
                        else if (null relativeDir) then return paths
                        else do relDir <- realPath relativeDir
                                return (relDir : paths)
       search (concatMap (\dir -> map (\n -> (dir,n)) nameext) searchPaths)
  where
    search [] = return Nothing  -- notfound envname nameext path
    search ((dir,fname):xs)
      = do{ let fullName = joinPath dir fname
          -- ; trace ("search: " ++ fullName) $ return ()
          ; exist <- doesFileExist fullName
          ; if exist
             then do rpath <- realPath fullName
                     let (root,stem) = getMaximalPrefixPath paths rpath   -- not to the relativeDir!
                     -- trace ("search found: " ++ show (root,stem)) $
                     return $ Just $! (root,stem)
             else search xs
          }

    nameext
      = concatMap (\fname -> fname : map (fname++) exts) $
        map (\suffix -> (notext nname) ++ suffix ++ (extname nname)) ("" : suffixes)

    nname
      = joinPaths $ dropWhile (==".") $ splitPath name

searchPathsEx :: [FilePath] -> [String] -> [String] -> String -> IO (Maybe (FilePath,FilePath))
searchPathsEx path exts suffixes name
  = -- trace ("search " ++ name ++ " in " ++ show path) $
    search (concatMap (\dir -> map (\n -> (dir,n)) nameext) ("":path))
  where
    search [] = return Nothing  -- notfound envname nameext path
    search ((dir,fname):xs)
      = do{ let fullName = joinPath dir fname
          ; exist <- doesFileExist fullName
          ; if exist
             then return (Just (dir,fname))
             else search xs
          }
    nameext
      = concatMap (\fname -> fname : map (fname++) exts) $
        map (\suffix -> (notext nname) ++ suffix ++ (extname nname)) ("" : suffixes)

    nname
      = joinPaths $ dropWhile (==".") $ splitPath name


-- | Make a file path relative to a set of given paths: return the (maximal) root and stem
-- if it is not relative to the paths, return dirname/notdir
makeRelativeToPaths :: [FilePath] -> FilePath -> (FilePath,FilePath)
makeRelativeToPaths paths fname
  = case findMaximalPrefixPath paths fname of
      Just (root,rpath) -> (root,rpath)
      _                 -> (dirname fname, notdir fname)



searchProgram :: FilePath -> IO (Maybe FilePath)
searchProgram ""
  = return Nothing
searchProgram fname | isAbsolute fname || fname `startsWith` "."
  = do exist <- doesFileExist fname
       if exist
         then return (Just fname)
         else return Nothing
searchProgram fname
  = do paths  <- getEnvPaths "PATH"
       searchPaths paths [exeExtension] fname

-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Integrate NPM style packages
-}
-----------------------------------------------------------------------------
module Compiler.Package( Packages, PackageName
                       , searchPackages
                       , discoverPackages
                       , packageInfoFromDir
                       , packagesEmpty
                       , joinPkg
                       ) where

import Data.Char        ( toLower )
import Lib.PPrint
import Data.List        ( intersperse, replicate, sort )
import Platform.Config  ( pathSep, pathDelimiter )
import Common.Failure   ( raiseIO, catchIO )

import System.Directory ( doesFileExist, doesDirectoryExist
                        , getModificationTime, copyFile
                        , getCurrentDirectory, getDirectoryContents
                        , createDirectoryIfMissing, canonicalizePath
                        , getHomeDirectory )
import Common.File
import Lib.Trace
import Lib.JSON

---------------------------------------------------------------
-- Package data type
----------------------------------------------------------------

type PackageName = String
data Packages    = Packages { packages :: [Package],
                              roots    :: [FilePath] }

data Package     = Package { pkgDir   :: FilePath,    -- /x/node_modules/A/lib
                             pkgQualName  :: PackageName, -- A/B/C
                             pkgLocal :: FilePath,    -- lib
                             pkgSub   :: [Package] }

packagesEmpty :: Packages
packagesEmpty = Packages [] []

pkgName :: Package -> PackageName
pkgName pkg = last $ splitPath $ pkgQualName pkg

---------------------------------------------------------------
-- search packages
----------------------------------------------------------------
{-
-- | Is this a package director?
isPackageDir :: Packages -> FilePath -> Bool
isPackageDir (Packages _ roots) dir
  = (node_modules `elem` splitPath dir)
    || (any (startsWith dir) roots)
-}


-- | Return package names: ("A/B/C", "lib")  (or ("","") for a non package)
packageInfoFromDir :: Packages -> FilePath -> (FilePath,FilePath)
packageInfoFromDir pkgs dir
  = case packageFromDir pkgs dir of
      Nothing  -> ("","")
      Just pkg -> (pkgQualName pkg, pkgLocal pkg)

packageFromDir :: Packages -> FilePath -> Maybe Package
packageFromDir pkgs0 dir
  = let pkgs = visiblePackages pkgs0 dir
    in case filter (\pkg -> pkgDir pkg == dir) pkgs of
         (pkg:_) -> Just pkg
         _       -> Nothing

-- | Search the packages from a certain directory
searchPackages :: Packages -> FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
searchPackages pkgs current pkgname name
  = do ccurrent <- canonicalizePath (if null current then "." else current)
       let toSearch = (if null pkgname then id else filter (\pkg -> pkgName pkg == pkgname)) $
                      visiblePackages pkgs ccurrent
       -- trace ("search packages for " ++ pkgname ++ "/" ++ name ++ " in " ++ show (map pkgQualName toSearch)) $ return ()
       searchIn toSearch
  where
    searchIn [] = return Nothing
    searchIn (pkg:pkgs)
      = do let path = joinPath (pkgDir pkg) name
           exist <- doesFileExist path
           if exist
            then return (Just path)
            else searchIn pkgs

-- | Return the visible packages given a (canonical) current directory
visiblePackages :: Packages -> FilePath -> [Package]
visiblePackages (Packages pkgs _) ccurrent
  = visiblePkgs pkgs
  where
    visiblePkgs pkgs
      = do let isVisible pkg = ccurrent `startsWith` packageBase (pkgDir pkg)
               isVisibleSubs [] = False
               isVisibleSubs (pkg:_) = isVisible pkg
               visible = dropWhile (not . isVisible) pkgs
           case filter (isVisibleSubs . pkgSub) visible of
             []      -> visible
             (pkg:_) -> visiblePkgs (pkgSub pkg) ++ visible -- note: _ should always be []

    packageBase :: FilePath -> FilePath
    packageBase pkgpath
      = case dropWhile (/=node_modules) (reverse (splitPath pkgpath)) of
          (_node_modules:base) -> joinPaths (reverse base)
          []                   -> pkgpath -- NOTE: this can happen for roots; leaving as it is is fine

---------------------------------------------------------------
-- discover packages
----------------------------------------------------------------

-- | Set up the packages
discoverPackages :: FilePath -> IO Packages
discoverPackages root
  = do croot <- canonicalizePath root
       pkgs <- walk (splitPath croot) 0 []
       -- trace ("packages: " ++ show (ppPackages (packages pkgs))) $ return ()
       return pkgs
  where
    walk [] n acc
      = do eroots  <- getEnvPaths node_path
           homedir <- getHomeDirectory
           let hroots = map (joinPath homedir) [".node_modules",".node_libraries"]
               -- bug: we do not search in $prefix/lib/node since it is deprecated
               roots = hroots ++ eroots
           pkgss <- mapM (\(m,p) -> readSubPackages m "" p) (zip [n..] roots)
           return (Packages (acc ++ concat pkgss) roots)

    walk ps n acc
      = do cpkgs <- readSubPackages n "" (joinPaths (ps ++ [node_modules]))
           walk (init ps) (if null cpkgs then n else n+1) (acc ++ cpkgs)

    readSubPackages n pname path
      = -- trace ("read sub packages: " ++ ppath) $
        do exist    <- doesDirectoryExist path
           contents <- if exist then getDirectoryContents path else return []
           let children = sort $ filter (\c -> not (null c) && head c /= '.') contents
           cpkgs    <- mapM (readPackage pname path n) children
           return (filter (not . null . pkgDir) cpkgs)

    readPackage pname path n cname
      = -- trace ("read package: " ++ joinPath path cname) $
        do let jsonpath = joinPaths [path,cname,package_json]
           exist <- doesFileExist jsonpath
           if (not exist)
            then return $ Package "" "" "" []
            else do json <- readJSONFromFile jsonpath
                    let keywords = case jsLookup json ["keywords"] of
                                     JsArray elems -> map (map toLower . toString) elems
                                     _             -> []
                        isKokaPkg = (cname `startsWith` "koka"  || "koka" `elem` keywords)
                    let local = toString $ jsFind json (JsString (if isKokaPkg then "lib" else "")) ["directories","lib"]
                        pkglocal = joinPaths $ dropWhile (==".") $ splitPath local -- strip off leading "./xxx"
                        pkgdir   = joinPaths [path,cname,pkglocal] -- TODO: read from json
                        pkgname  = joinPkgs [if (n==0) then "" else show n,pname,cname]
                    -- trace ("read package: " ++ cname ++ ": " ++ local ++ ": " ++ pkglocal) $ return ()
                    pkgs <- readSubPackages n (joinPkg pname cname) (joinPaths [path,cname,node_modules])
                    return $ Package pkgdir pkgname pkglocal pkgs

joinPkg p1 p2
  = joinPkgs [p1,p2]

joinPkgs ps
  = forwardSlash $ joinPaths ps
  where
    forwardSlash s = map (\c -> if isPathSep c then '/' else c) s


---------------------------------------------------------------
-- constants
----------------------------------------------------------------
node_path :: String
node_path = "NODE_PATH"

node_modules :: String
node_modules = "node_modules"

package_json :: String
package_json = "package.json"

---------------------------------------------------------------
-- show packages
----------------------------------------------------------------

instance Show Package where
  show pkg = show (ppPackage pkg)

ppPackage (Package d n l ps)
  = text (justify 20 (n ++ ": ") ++ atmost 50 d) <.>
    (if null ps then empty else line <.> indent 2 (ppPackages ps))
  where
    atmost n s
      = if length s > n then "..." ++ drop (length s - n) s else s
    justify n s
      = if length s < n then s ++ replicate (n - length s) ' ' else s

ppPackages [] = empty
ppPackages ps
  = vcat (map ppPackage ps)

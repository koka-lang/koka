-----------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Compile.BuildContext ( BuildContext
                            , buildcEmpty

                            , buildcFullBuild
                            , buildcValidate, buildcTypeCheck, buildcBuild

                            , buildcExpr

                            , buildcAddRootSources
                            , buildcAddRootModules
                            , buildcRoots
                            , buildcClearRoots, buildcRemoveRootModule, buildcRemoveRootSource
                            , buildcFindModuleName

                            , runBuildIO
                            ) where


import Data.List
import qualified Data.Map.Strict as M
import Platform.Config
import Common.Name
import Common.NamePrim (nameSystemCore)
import Common.Range
import Common.File
import Common.Failure
import Compiler.Options
import Compile.Module
import Compile.Build

data BuildContext = BuildContext {
                      buildcRoots   :: ![ModuleName],
                      buildcModules :: ![Module],
                      buildcFileSys :: !(M.Map FilePath (BString,FileTime))
                    }

buildcEmpty :: BuildContext
buildcEmpty = BuildContext [] [] M.empty

buildcVFS :: BuildContext -> VFS
buildcVFS buildc = let fs = buildcFileSys buildc
                   in seq fs $ VFS (\fpath -> M.lookup fpath fs)

buildcAddRootSources :: [FilePath] -> BuildContext -> Build (BuildContext,[ModuleName])
buildcAddRootSources fpaths buildc
  = do mods <- mapM (moduleFromSource (buildcVFS buildc)) fpaths
       let rootNames = map modName mods
           roots   = nub (map modName mods ++ buildcRoots buildc)
           modules = mergeModules mods (buildcModules buildc)
       seqList roots $ seqList modules $
        return (buildc{ buildcRoots = roots, buildcModules = modules }, rootNames)

buildcAddRootModules :: [ModuleName] -> BuildContext -> Build BuildContext
buildcAddRootModules moduleNames buildc
  = do mods <- mapM (moduleFromModuleName (buildcVFS buildc) "" {-relative dir-}) moduleNames
       let roots   = nub (map modName mods ++ buildcRoots buildc)
           modules = mergeModules mods (buildcModules buildc)
       seqList roots $ seqList modules $
        return buildc{ buildcRoots = roots, buildcModules = modules }

buildcClearRoots :: BuildContext -> BuildContext
buildcClearRoots buildc
  = buildc{ buildcRoots = [] }


buildcRemoveRootModule :: ModuleName -> BuildContext -> BuildContext
buildcRemoveRootModule mname buildc
  = buildc{ buildcRoots = filter (/=mname) (buildcRoots buildc) }

buildcFindModuleName :: FilePath -> BuildContext -> Maybe ModuleName
buildcFindModuleName fpath0 buildc
  = let fpath = normalize fpath0
    in modName <$> find (\m -> modSourcePath m == fpath || modSourceRelativePath m == fpath) (buildcModules buildc)

buildcRemoveRootSource :: FilePath -> BuildContext -> BuildContext
buildcRemoveRootSource fpath buildc
  = case buildcFindModuleName fpath buildc of
      Just mname -> buildcRemoveRootModule mname buildc
      _          -> buildc



buildcValidate :: Bool -> [ModuleName] -> BuildContext -> Build BuildContext
buildcValidate rebuild forced buildc
  = do let (roots,imports) = buildcSplitRoots buildc
       mods <- modulesReValidate (buildcVFS buildc) rebuild forced imports roots
       return buildc{ buildcModules = mods }

buildcSplitRoots :: BuildContext -> ([Module],[Module])
buildcSplitRoots buildc
  = partition (\m -> modName m `elem` buildcRoots buildc) (buildcModules buildc)

buildcTypeCheck :: BuildContext -> Build BuildContext
buildcTypeCheck buildc
  = do mods <- modulesTypeCheck (buildcModules buildc)
       return buildc{ buildcModules = mods }

buildcBuild :: [Name] -> BuildContext -> Build BuildContext
buildcBuild mainEntries buildc
  = do mods <- modulesBuild mainEntries (buildcModules buildc)
       return (buildc{ buildcModules = mods})

buildcFullBuild :: Bool -> [ModuleName] -> [Name] -> BuildContext -> Build BuildContext
buildcFullBuild rebuild forced mainEntries buildc
  = do let (roots,imports) = buildcSplitRoots buildc
       mods <- modulesFullBuild (buildcVFS buildc) rebuild forced mainEntries imports roots
       return (buildc{ buildcModules = mods})


buildcExpr :: [ModuleName] -> String -> BuildContext -> Build BuildContext
buildcExpr importNames expr buildc
  = do let sourcePath = (case [modSourcePath mod | mname <- importNames ++ [nameSystemCore], let mod = buildcFindModule mname buildc] of
                           (fpath:_) -> noexts fpath
                           _         -> "") ++ "-main" ++ sourceExtension
           importDecls = map (\mname -> "import " ++ show mname) importNames
           content     = stringToBString $ unlines $ importDecls ++ [
                           "pub fun expr()",
                           "  " ++ expr,
                           "",
                           "pub fun xmain() : io ()",
                           "  expr().println"
                         ]
       withVirtualModule sourcePath content buildc $ \mainModName buildc2 ->
         do let mainEntry = qualify mainModName (newName "xmain")
            buildc3 <- buildcFullBuild False [] [mainEntry] buildc2
            let mainMod = buildcFindModule mainModName buildc3
            case modEntry mainMod of
              Just (exePath,execute)
                -> liftIO $ execute
              _ -> return ()
            return buildc3

withVirtualModule :: FilePath -> BString -> BuildContext -> (ModuleName -> BuildContext -> Build a) -> Build a
withVirtualModule fpath0 content buildc action
  = do let fpath = normalize fpath0
       buildc1 <- buildcSetVirtualFile fpath content buildc
       (buildc2,[modName]) <- buildcAddRootSources [fpath] buildc1
       action modName buildc2

buildcSetVirtualFile :: FilePath -> BString -> BuildContext -> Build BuildContext
buildcSetVirtualFile fpath0 content buildc
  = do let fpath = normalize fpath0
       ftime <- liftIO $ getCurrentTime
       return buildc{ buildcFileSys = M.insert fpath (content,ftime) (buildcFileSys buildc) }

buildcDeleteVirtualFile :: FilePath -> BuildContext -> Build BuildContext
buildcDeleteVirtualFile fpath0 buildc
  = do let fpath = normalize fpath0
       return buildc{ buildcFileSys = M.delete fpath (buildcFileSys buildc) }

buildcFindModule :: HasCallStack => ModuleName -> BuildContext -> Module
buildcFindModule modname buildc
  = case find (\mod -> modName mod == modname) (buildcModules buildc) of
      Just mod -> mod
      _        -> failure ("Compile.BuildIde.btxFindModule: cannot find " ++ show modname ++ " in " ++ show (map modName (buildcModules buildc)))


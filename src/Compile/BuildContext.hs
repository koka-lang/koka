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
                            , buildcLoadFiles
                            , buildcImportModules

                            , runBuildIO
                            ) where


import qualified Data.Map.Strict as M
import Common.Name
import Common.Range
import Common.File
import Compiler.Options
import Compile.Module
import Compile.Build

data BuildContext = BuildContext {
                      buildcModules :: [Module],
                      buildcFS      :: M.Map FilePath (BString,FileTime)
                    }

buildcEmpty :: BuildContext
buildcEmpty = BuildContext [] M.empty

buildcVFS :: BuildContext -> VFS
buildcVFS buildc = let fs = buildcFS buildc
                   in seq fs $ VFS (\fpath -> M.lookup fpath fs)

buildcLoadFiles :: Bool -> [FilePath] -> BuildContext -> Build BuildContext
buildcLoadFiles force fpaths buildc
  = do mods <- mapM (moduleFromSource (buildcVFS buildc) force) fpaths
       return buildc{ buildcModules = mergeModules mods (buildcModules buildc) }

buildcImportModules :: [ModuleName] -> BuildContext -> Build BuildContext
buildcImportModules moduleNames buildc
  = do mods <- mapM (moduleFromModuleName (buildcVFS buildc) False "" {-relative dir-}) moduleNames
       return buildc{ buildcModules = mergeModules mods (buildcModules buildc) }

buildcValidate :: Bool -> [ModuleName] -> BuildContext -> Build BuildContext
buildcValidate rebuild forced buildc
  = do mods <- modulesReValidate (buildcVFS buildc) rebuild forced (buildcModules buildc)
       return buildc{ buildcModules = mods }

buildcTypeCheck :: BuildContext -> Build BuildContext
buildcTypeCheck buildc
  = do mods <- modulesTypeCheck (buildcModules buildc)
       return buildc{ buildcModules = mods }

buildcBuild :: [Name] -> BuildContext -> Build BuildContext
buildcBuild mainEntries buildc
  = do mods <- modulesBuild mainEntries (buildcModules buildc)
       return (buildc{ buildcModules = mods})

buildcFullBuild :: Bool -> [Name] -> BuildContext -> Build BuildContext
buildcFullBuild rebuild mainEntries buildc
  = do mods <- modulesFullBuild (buildcVFS buildc) rebuild mainEntries (buildcModules buildc)
       return (buildc{ buildcModules = mods})

{-
buildcExpr :: [ModuleName] -> String -> BuildContext -> Build BuildContext
buildcExpr importNames expr buildc
  = do buildc1 <- buildcImportModules importedModules buildc
       let sourcePath = (case [modSourcePath mod | mname <- importNames ++ [nameSystemCore], mod <- buildcFindModule mname buildc1] of
                           (fpath:_) -> noexts fpath
                           _         -> "") ++ "@main" ++ sourceExtension
           content    = "pub fun expr()\n  " ++ expr ++ "\n\npub fun main() : io ()\n  expr().println\n"
       withVirtualModule sourcePath content buildc1 $ \mainModName buildc2 ->
         do let mainEntry = qualify mainModName (newName "main")
            buildc3 <- buildcFullBuild [mainEntry] buildc2
            let mainMod = buildcFindModule mainModName buildc3
            case modEntry mainMod of
              Just (exePath,execute)
                -> liftIO $ execute
              _ -> return ()


buildcAddVirtualFile :: FilePath -> String -> BuildContext -> Build BuildContext
buildcAddVirtualFile fpath content buildc
  = do


buildcFindModule :: HasCallStack => ModuleName -> BuildContext -> Build BuildContext
buildcFindModule modname buildc
  = case find (\mod -> modName mod == modname) (buildcModules buildc) of
      Just mod -> mod
      _        -> failure ("Compile.BuildIde.btxFindModule: cannot find " ++ show modname ++ " in " ++ show (map modName (buildcModules buildc)))



-}
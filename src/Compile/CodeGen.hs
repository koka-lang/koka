-----------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Compile.CodeGen ( codeGen, Link, LinkResult(..), noLink ) where

import Debug.Trace
import Data.Char
import Data.Maybe
import Data.List
import Data.Either
import Control.Monad
import System.Directory ( doesFileExist, doesDirectoryExist, createDirectoryIfMissing )

import Platform.Config        ( version, exeExtension, dllExtension, libPrefix, libExtension, pathSep, sourceExtension )

import Lib.PPrint hiding (dquote)
import Lib.Printer
import Common.Error
import Common.Range
import Common.Name
import Common.Syntax
import Common.ColorScheme
import Common.File
import Common.Failure
import Kind.Assumption( KGamma )
import Kind.Newtypes( Newtypes )
import Type.Type
import Type.Assumption( Gamma )
import Type.Pretty( Env(colorizing,coreIface,coreShowDef,context,importsMap) )
import Syntax.Syntax
import Syntax.Colorize( colorize )
import Syntax.GenDoc( genDoc )

import qualified Core.Core as Core
import qualified Core.Pretty
import Core.Borrowed( Borrowed )

import Backend.CSharp.FromCore    ( csharpFromCore )
import Backend.JavaScript.FromCore( javascriptFromCore )
import Backend.C.FromCore         ( cFromCore )
import Backend.VM.FromCore        ( vmFromCore )

import Compile.Options
import Compile.Module( Definitions(..), Module(..), modCoreImports )
import Compile.TypeCheck( importMapFromCoreImports )    -- todo: break this dependency?
import Type.InferMonad (traceDefDoc)


data LinkResult = LinkDone
                | LinkExe{ linkExePath :: !FilePath, linkRun :: !(IO ()) }

type Link       = [Module] -> IO LinkResult

noLink :: Link
noLink _ = return LinkDone

{---------------------------------------------------------------
  Code generation
---------------------------------------------------------------}

codeGen :: Terminal -> Flags -> (IO () -> IO ()) -> Newtypes -> Borrowed -> KGamma -> Gamma ->
             Maybe (Name,Type) -> [Module] -> Module -> IO Link
codeGen term flags sequential newtypes borrowed kgamma gamma entry imported mod
  = -- compilerCatch ("code generation in " ++ show (modName mod)) term noLink $
    do let program    = fromJust (modProgram mod)
           core       = fromJust (modCore mod)

           importsMap = importMapFromCoreImports (programImports program) (modCoreImports mod)
           penv       = (prettyEnvFromFlags flags){ context = modName mod, importsMap = importsMap }
           outBase    = noexts (modIfacePath mod)
           inlineDefs = case modInlines mod of
                          Right defs -> defs
                          Left _     -> []
           ifaceDoc   = Core.Pretty.prettyCore penv{ coreIface = True } (target flags) inlineDefs core
                         <-> Lib.PPrint.empty
           coreDoc    = Core.Pretty.prettyCore penv{ coreIface = False, coreShowDef = (showCore flags) } (target flags) inlineDefs core
                         <-> Lib.PPrint.empty

       -- create output directory if it does not exist
       direxist <- doesDirectoryExist (dirname (modIfacePath mod))
       when (not direxist) $
         sequential (createDirectoryIfMissing True (dirname (modIfacePath mod)))

       -- remove existing kki file in case of errors
       removeFileIfExists (modIfacePath mod)

       -- core
       when (genCore flags)  $
         do termTrace term "generate core"
            let outCore  = outBase ++ ".kkc"
            writeDocW 10000 outCore coreDoc  -- just for debugging
       when (showFinalCore flags && not (isTargetC (target flags))) $
         do termInfo term coreDoc

       -- write documentation
       let fullHtml = outHtml flags > 1
           outHtmlFile  = outBase ++ "-source.html"
           source   = modSource mod -- maybe sourceNull programSource (modProgram mod)
           cenv     = penv{ colorizing=True }
       if (isLiteralDoc (sourceName source)) -- .kk.md
        then do termTrace term "write html document"
                withNewFilePrinter (outBase ++ ".md") $ \printer ->
                 colorize (modRangeMap mod) cenv kgamma gamma fullHtml (sourceName source) 1 (sourceBString source) printer
        else when (outHtml flags > 0) $
              do termTrace term "write html source"
                 withNewFilePrinter outHtmlFile $ \printer ->
                  colorize (modRangeMap mod) cenv kgamma gamma fullHtml (sourceName source) 1 (sourceBString source) printer
                 termTrace term "write html documentation"
                 withNewFilePrinter (outBase ++ ".xmp.html") $ \printer ->
                  genDoc cenv kgamma gamma core printer

       -- generate actual code
       link <- backend term flags sequential entry outBase core

       -- return the link as an action to increase concurrency
       return $ \fullImports ->
         -- compilerCatch ("linking in " ++ show (modName mod)) term LinkDone $
         do mbRun <- link fullImports
            -- write interface file last so on any error it will not be written
            writeDocW 10000 (modIfacePath mod) ifaceDoc

            -- copy final exe if -o is given
            case mbRun of
              LinkExe out _
                -> do let finalOut = outFinalPath flags
                      exe <- if (not (null finalOut))
                                then do let targetOut = ensureExt finalOut (targetExeExtension (target flags))
                                        when (osName == "macos") $
                                          removeFileIfExists targetOut  -- needed on macOS due to code signing issues (see https://developer.apple.com/forums/thread/669145)
                                        copyBinaryFile out targetOut
                                        return finalOut
                                else return out
                      termPhase term $ color (colorInterpreter (colorScheme flags)) (text "created :") <+>
                          color (colorSource (colorScheme flags)) (text (normalizeWith pathSep exe))
              _ -> return ()
            return (mbRun)
  where
    backend :: Terminal -> Flags -> (IO () -> IO ()) -> Maybe (Name,Type) -> FilePath -> Core.Core -> IO Link
    backend  = case target flags of
                 CS   -> codeGenCS
                 JS _ -> codeGenJS
                 VM   -> codeGenVM
                 _    -> {-
                         let -- for Perceus (Parc) we analyze types inside abstract types and thus need
                             -- access to all defined types; here we freshly extract all type definitions from all
                             -- imported modules.
                             newtypesAll = foldr1 newtypesCompose (map (extractNewtypes . modCore) (loadedModule loaded : loadedModules loaded))
                         in -}
                         codeGenC (modSourcePath mod) newtypes borrowed 0 {-unique-}


{---------------------------------------------------------------
  C#
---------------------------------------------------------------}

-- Generate C# through CS files without generating dll's
codeGenCS :: Terminal -> Flags -> (IO () -> IO ()) -> Maybe (Name,Type) -> FilePath -> Core.Core -> IO Link
codeGenCS term flags sequential entry outBase core
  = compilerCatch "csharp compilation" term noLink $
    do let (mbEntry,isAsync) = case entry of
                                 Just (name,tp) -> (Just (name,tp), isAsyncFunction tp)
                                 _ -> (Nothing, False)
           cs  = csharpFromCore (buildType flags) (enableMon flags) mbEntry core
           outcs       = outBase ++ ".cs"
           searchFlags = "" -- concat ["-lib:" ++ dquote dir ++ " " | dir <- [fullBuildDir flags] {- : includePath flags -}, not (null dir)] ++ " "
           outName fname = joinPath (dirname outBase) fname

       termTrace term $ "generate csharp" ++ maybe "" (\(name,_) -> ": entry: " ++ show name) mbEntry
       writeDoc outcs cs
       when (showAsmCS flags) (termInfo term cs)

       case mbEntry of
         Nothing -> return noLink
         Just _  ->
          do let linkFlags  = "-r:System.Numerics.dll " -- ++ (if isAsync then "-r:" ++ outName "std_async.dll ")
                 sources    = concat [dquote (outName (moduleNameToPath (Core.importName imp)) ++ ".cs") ++ " " | imp <- Core.coreProgImports core]
                 targetExe  = (if null (outBaseName flags) then outBase else outName (outBaseName flags)) ++ exeExtension
                 targetName = dquote targetExe
                 targetFlags= "-t:exe -out:" ++ targetName ++ " "
                 debugFlags = (if (debug flags) then "-debug -define:DEBUG " else "") ++ (if (optimize flags >= 1) then "-optimize " else "")
             let cmd = (csc flags ++ " " ++ targetFlags ++ debugFlags ++ " -nologo -warn:4 " ++ searchFlags ++ linkFlags ++ sources)
             runSystemEcho term flags cmd
             -- run the program
             return (\_ -> return (LinkExe targetExe (runSystemEcho term flags targetName)))

-- CS code generation via libraries; this catches bugs in C# generation early on but doesn't take a transitive closure of dll's
codeGenCSDll:: Terminal -> Flags -> (IO () -> IO ()) -> Maybe (Name,Type) -> FilePath -> Core.Core -> IO Link
codeGenCSDll term flags sequential entry outBase core
  = compilerCatch "csharp compilation" term noLink $
    do let (mbEntry,isAsync) = case entry of
                                 Just (name,tp) -> (Just (name,tp), isAsyncFunction tp)
                                 _ -> (Nothing, False)
           cs  = csharpFromCore (buildType flags) (enableMon flags) mbEntry core
           outcs       = outBase ++ ".cs"
           searchFlags = "" -- concat ["-lib:" ++ dquote dir ++ " " | dir <- [fullBuildDir flags] {- : includePath flags -}, not (null dir)] ++ " "
           outName fname = joinPath (dirname outBase) fname

       termTrace term $ "generate csharp" ++ maybe "" (\(name,_) -> ": entry: " ++ show name) mbEntry
       writeDoc outcs cs
       when (showAsmCS flags) (termInfo term cs)

       let linkFlags  = concat ["-r:" ++ outName (moduleNameToPath (Core.importName imp)) ++ dllExtension ++ " "
                                    | imp <- Core.coreProgImports core] -- TODO: link to correct package!
                        ++ "-r:System.Numerics.dll " ++ (if isAsync then "-r:" ++ outName "std_async.dll " else "")
           targetName = case mbEntry of
                          Just _ -> dquote ((if null (outBaseName flags) then outBase else outName (outBaseName flags)) ++ exeExtension)
                          _      -> dquote (outBase ++ dllExtension)
           targetFlags= case mbEntry of
                          Just _ -> "-t:exe -out:" ++ targetName
                          _      -> "-t:library -out:" ++ targetName
           debugFlags = (if (debug flags) then "-debug " else "") ++ (if (optimize flags >= 1) then "-optimize " else "")
       let cmd = (csc flags ++ " " ++ debugFlags ++ targetFlags ++ " -nologo -warn:4 " ++ searchFlags ++ linkFlags ++ dquote outcs)
       -- trace cmd $ return ()
       runSystemEcho term flags cmd
       -- run the program
       return (\_ -> return (LinkExe targetName (runSystemEcho term flags targetName)))


{---------------------------------------------------------------
  Javascript
---------------------------------------------------------------}

codeGenJS :: Terminal -> Flags -> (IO () -> IO ()) -> Maybe (Name,Type) -> FilePath -> Core.Core -> IO Link
codeGenJS term flags sequential entry outBase core
  = do let outjs         = outBase ++ ".mjs"
           outName fname = joinPath (dirname outBase) fname
           -- extractImport m = Core.Import (modName m) "" {- (modPackageQName m) -} Core.ImportUser Public ""
           js = javascriptFromCore (buildType flags) mbEntry (Core.coreProgImports core) core
           mbEntry = case entry of
                       Just (name,tp) -> Just (name,isAsyncFunction tp)
                       _              -> Nothing
       termTrace term ( "generate javascript: " ++ outjs )
       writeDocW 80 outjs js
       when (showAsmJS flags) (termInfo term js)

       case mbEntry of
        Nothing -> return noLink
        Just  _ ->
         do -- always generate an index.html file
            let outHtml = outName ((if (null (outBaseName flags)) then "index" else (outBaseName flags)) ++ ".html")
                contentHtml = text $ unlines $ [
                                "<!DOCTYPE html>",
                                "<html>",
                                "  <head>",
                                "    <meta charset=\"utf-8\">",
                                "    <script type='module' src='./" ++ notdir outjs ++ "'></script>",
                                "  </head>",
                                "  <body>",
                                "  </body>",
                                "</html>"
                              ]
            termTrace term ("generate index html: " ++ outHtml)
            writeDoc outHtml contentHtml
            case target flags of
              JS JsWeb ->
               do return (\_ -> return (LinkExe outHtml (runSystemEcho term flags (dquote outHtml ++ " &"))))
              _ ->
               do let stksize = if (stackSize flags == 0) then 100000 else (stackSize flags `div` 1024)
                  return (\_ -> return (LinkExe outjs (runCommand term flags [node flags,"--stack-size=" ++ show stksize,outjs])))


{---------------------------------------------------------------
  VM backend
---------------------------------------------------------------}
 
codeGenVM :: Terminal -> Flags -> (IO () -> IO ()) -> Maybe (Name,Type) -> FilePath -> Core.Core -> IO Link
codeGenVM term flags sequential entry outBase core
  = do let outmcore = outBase ++ ".mcore.json"
       let outrpy = outBase ++ ".rpyeffect"    
           vm = vmFromCore (buildType flags) mbEntry (Core.coreProgImports core) core
           mbEntry = case entry of
                       Just (name,tp) -> Just (name,isAsyncFunction tp)
                       _              -> Nothing
       termTrace term ( "generate vm: " ++ outmcore )
       writeDocW 80 outmcore vm
       when (showAsmVM flags) (termInfo term vm)

       -- FIXME: for now, use debug flags
       runCommand term flags [rpyeffectAsm flags,"--from", "mcore-json", outmcore, outrpy] -- for debugging:  "--debug", "--check-contracts", 

       case mbEntry of
        Nothing -> return noLink
        Just  _ ->
         return (\_ -> return (LinkExe outmcore (runCommand term flags [rpyeffectJit flags,outrpy])))

{---------------------------------------------------------------
  C backend
---------------------------------------------------------------}

codeGenC :: FilePath -> Newtypes -> Borrowed -> Int
             -> Terminal -> Flags -> (IO () -> IO ()) -> Maybe (Name,Type)
              ->FilePath -> Core.Core -> IO Link
codeGenC sourceFile newtypes borrowed0 unique0 term flags sequential entry outBase core0
 = do let outC = outBase ++ ".c"
          outH = outBase ++ ".h"
          sourceDir     = dirname sourceFile
          progName      = Core.coreProgName core0
          mbEntry       = case entry of
                            Just (name,tp) -> Just (name,isAsyncFunction tp)
                            _              -> Nothing
      -- generate C
      let -- (core,unique) = parcCore (prettyEnvFromFlags flags) newtypes unique0 core0
          ctarget = case target flags of
                      C ctarget -> ctarget
                      _         -> CDefault
          (cdoc,hdoc,_,bcore) = cFromCore False
                                          ctarget (buildType flags) sourceDir (prettyEnvFromFlags flags) (platform flags)
                                          newtypes borrowed0 unique0 (parcReuse flags) (parcSpecialize flags) (parcReuseSpec flags)
                                          (parcBorrowInference flags) (optEagerPatBind flags) (stackSize flags) mbEntry core0
          bcoreDoc  = Core.Pretty.prettyCore (prettyEnvFromFlags flags){ coreIface = False, coreShowDef = True } (C CDefault) [] bcore

      -- writeDocW 120 (outBase ++ ".c.kkc") bcoreDoc
      when (showFinalCore flags) $
        do termInfo term bcoreDoc

      -- termTrace term ( "generate c: " ++ outBase )
      writeDocW 120 outC (cdoc <.> linebreak)
      writeDocW 120 outH (hdoc <.> linebreak)
      termTrace term ( "generated c: " ++ outBase )
      when (showAsmC flags) (termInfo term (hdoc <//> cdoc))

      -- copy libraries
      let cc       = ccomp flags
          eimports = externalImportsFromCore (target flags) bcore
          clibs    = clibsFromCore flags bcore
      extraIncDirs <- concat <$> mapM (copyCLibrary term flags sequential cc (dirname outBase)) eimports

      -- return the C compilation and final link as a separate IO action to increase concurrency
      return $ \fullImports ->
               do -- compile the generated C
                  ccompile term flags cc outBase extraIncDirs [outC]
                  case entry of
                    Nothing -> return LinkDone
                    Just _  -> codeGenLinkC term flags sequential cc progName fullImports {-link all objs, not just direct imports -}
                                            outBase clibs


-- link obj's and create an executable
codeGenLinkC :: Terminal -> Flags -> (IO () -> IO ()) -> CC -> ModuleName -> [Module]
                  -> FilePath -> [FilePath] -> IO LinkResult
codeGenLinkC term flags sequential cc progName imported outBase clibs
  = do  -- names
        let outName fname = joinPath (dirname outBase) fname
            mainModName = moduleNameToPath progName
            mainName    = if null (outBaseName flags) then mainModName else outBaseName flags
            mainExe     = outName mainName

        -- build kklib for the specified build variant
        kklibObj <- kklibBuild term flags sequential cc (dirname outBase) "kklib" (ccObjFile cc "kklib")

        -- set up linker command line
        let objs   = [kklibObj] ++
                      [outName (ccObjFile cc (moduleNameToPath mname))
                          | mname <- map modName imported ++ [progName]]
                      -- ++ [mainObj]
            syslibs= concat [csyslibsFromCore flags mcore | mcore <- map (fromJust . modCore) imported]
                      ++ ccompLinkSysLibs flags
                      ++ (if onWindows && not (isTargetWasm (target flags))
                            then ["bcrypt","psapi","advapi32"]
                            else ["m","pthread"])
            libs   = -- ["kklib"] -- [normalizeWith '/' (outName (ccLibFile cc "kklib"))] ++ ccompLinkLibs flags
                      -- ++
                      clibs
                      ++
                      concat [clibsFromCore flags mcore | mcore <- map (fromJust . modCore) imported]

            libpaths = map (\lib -> outName (ccLibFile cc lib)) libs

            stksize = if (stackSize flags == 0 && (onWindows || isTargetWasm (target flags)))
                        then 8*1024*1024    -- default to 8Mb on windows and wasi
                        else stackSize flags
            hpsize  = if (heapSize flags == 0 && isTargetWasm (target flags))
                        then 1024*1024*1024 -- default to 1Gb on wasi
                        else heapSize flags

            clink  = concat $
                      [ [ccPath cc]
                      , ccFlags cc
                      , ccFlagsBuildFromFlags cc flags
                      , ccTargetExe cc mainExe
                      ]
                      ++ [objs]
                      ++ [ccFlagsLink cc]  -- must be last due to msvc
                      ++ [ccFlagStack cc stksize,ccFlagHeap cc hpsize]
                      -- ++ [ccAddLibraryDir cc (fullBuildDir flags)]
                      ++ map (ccAddLib cc) libpaths  -- libs
                      ++ map (ccAddSysLib cc) syslibs


        -- link
        -- termPhase term (color (colorInterpreter (colorScheme flags)) (text "linking:") <+>
        --                   color (colorSource (colorScheme flags)) (text mainName))
        runCommand term flags clink

        -- return command line to execute
        let mainTarget = mainExe ++ targetExeExtension (target flags)
        when (not (null (outFinalPath flags)) && verbose flags > 1) $
          termPhase term $ color (colorInterpreter (colorScheme flags)) (text "created :") <+>
                                color (colorSource (colorScheme flags)) (text (normalizeWith pathSep mainTarget))
        let cmdflags = if (showElapsed flags) then " --kktime" else ""

        case target flags of
          C Wasm
            -> do let cmd = wasmrun flags ++ " " ++ dquote mainTarget ++ " -- " ++ cmdflags ++ " " ++ execOpts flags
                  seqString cmd $ return (LinkExe mainTarget (runSystemEcho term flags cmd))
          C WasmWeb
            -> do return (LinkExe mainTarget
                          (runSystemEcho term flags (dquote mainTarget ++ " &")))
          C WasmJs
            -> do let nodeStack = if (stksize == 0) then 100000 else (stksize `div` 1024)
                  return (LinkExe mainTarget
                          (runCommand term flags [node flags,"--stack-size=" ++ show nodeStack,mainTarget]))
          _ -> do let cmd = dquote mainExe ++ cmdflags ++ " " ++ execOpts flags
                  seqString cmd $ return (LinkExe mainTarget (runSystemEcho term flags cmd)) -- use shell for proper rss accounting


-- Run the C compiler
ccompile :: Terminal -> Flags -> CC -> FilePath -> [FilePath] -> [FilePath] -> IO ()
ccompile term flags cc ctargetObj extraIncDirs csources
  = do let cmdline = concat $
                      [ [ccPath cc]
                      , ccFlags cc
                      , ccFlagsWarn cc
                      , ccFlagsBuildFromFlags cc flags
                      , ccFlagsCompile cc
                      , ccIncludeDir cc (localShareDir flags ++ "/kklib/include")
                      ]
                      ++
                      map (ccIncludeDir cc) (extraIncDirs ++ ccompIncludeDirs flags)
                      ++
                      map (ccAddDef cc) (ccompDefs flags)
                      ++
                      [ ccTargetObj cc (notext ctargetObj)
                      , csources
                      ]
       runCommand term flags cmdline



{---------------------------------------------------------------
  C libraries
---------------------------------------------------------------}

-- copy static C library to the output directory (so we can link and/or bundle) and
-- return needed include paths for imported C code
copyCLibrary :: Terminal -> Flags -> (IO () -> IO ()) -> CC -> FilePath -> [(String,String)] -> IO [FilePath] {-include paths-}
copyCLibrary term flags sequential cc outDir eimport
  = case Core.eimportLookup (buildType flags) "library" eimport of
      Nothing -> return []
      Just clib
        -> do mb  <- do mbSearch <- search [] [ searchCLibrary flags cc clib (ccompLibDirs flags)
                                              , case lookup "vcpkg" eimport of
                                                  Just pkg
                                                    -> vcpkgCLibrary term flags sequential cc eimport clib pkg
                                                  _ -> return (Left [])
                                              , case lookup "conan" eimport of
                                                  Just pkg | not (null (conan flags))
                                                    -> conanCLibrary term flags sequential cc eimport clib pkg
                                                  _ -> return (Left [])
                                              ]
                        case mbSearch of
                          Right res -> return (Just res)
                          Left warn -> do termWarning term flags warn
                                          return Nothing
              case mb of
                Just (libPath,includes)
                  -> do termPhase term (color (colorInterpreter (colorScheme flags)) (text "library :") <+>
                          color (colorSource (colorScheme flags)) (text libPath))
                        -- this also renames a suffixed libname to a canonical name (e.g. <vcpkg>/pcre2-8d.lib -> <out>/pcre2-8.lib)
                        sequential $ copyBinaryIfNewer (rebuild flags) libPath (joinPath outDir (ccLibFile cc clib))
                        return includes
                Nothing
                  -> -- TODO: suggest conan and/or vcpkg install?
                     do termWarning term flags $
                          text "unable to find C library:" <+> color (colorSource (colorScheme flags)) (text clib) <->
                          text "   hint: provide \"--cclibdir\" as an option, or use \"syslib\" in an extern import?"
                        raiseIO ("unable to find C library " ++ clib ++
                                 "\nlibrary search paths: " ++ show (ccompLibDirs flags))
  where
    search :: [Doc] -> [IO (Either [Doc] (FilePath,[FilePath]))] -> IO (Either Doc (FilePath,[FilePath]))
    search warns [] = return (Left (vcat (intersperse (text "or") warns)))
    search warns (io:ios)
      = do mbRes <- io
           case mbRes of
             Right res   -> return (Right res)
             Left warns' -> search (warns ++ warns') ios

searchCLibrary :: Flags -> CC -> FilePath -> [FilePath] -> IO (Either [Doc] (FilePath {-libPath-},[FilePath] {-include paths-}))
searchCLibrary flags cc clib searchPaths
  = do mbPath <- -- looking for specific suffixes is not ideal but it differs among plaforms (e.g. pcre2-8 is only pcre2-8d on Windows)
                 -- and the actual name of the library is not easy to extract from vcpkg (we could read
                 -- the lib/config/<lib>.pc information and parse the Libs field but that seems fragile as well)
                 do let suffixes = (if (buildType flags <= Debug) then ["d","_d","-d","-debug","_debug","-dbg","_dbg"] else [])
                    -- trace ("search in: " ++ show searchPaths) $
                    searchPathsSuffixes searchPaths [] suffixes (ccLibFile cc clib)
       case mbPath of
        Just fname
          -> case reverse (splitPath fname) of
               (_:"lib":"debug":rbase) -> return (Right (fname, [joinPaths (reverse rbase ++ ["include"])])) -- for vcpkg
               (_:"lib":rbase)         -> return (Right (fname, [joinPaths (reverse rbase ++ ["include"])])) -- e.g. /usr/local/lib
               _                       -> return (Right (fname, []))
        _ -> return (Left [])



{---------------------------------------------------------------
  Package managers: Conan and VCPkg
---------------------------------------------------------------}

conanCLibrary :: Terminal -> Flags -> (IO () -> IO ()) -> CC -> [(String,String)] -> FilePath -> String -> IO (Either [Doc] (FilePath,[FilePath]))
conanCLibrary term flags sequential cc eimport clib pkg
  = do mbConanCmd <- searchProgram (conan flags)
       case mbConanCmd of
         Nothing
          -> do return $ Left [text "this module requires a conan package but \"" <.> clrSource (text (conan flags)) <.> text "\" is not installed."
                                     <-> text "         install conan as:"
                                     <-> text "         >" <+> clrSource (text "pip3 install conan")
                                     <-> text "         or see <" <.> clrSource (text "https://docs.conan.io/en/latest/installation.html") <.> text ">"]
         Just conanCmd | onWindows && not (any (\pre -> ccName cc `startsWith` pre) ["cl","clang-cl"])
          -> do return $ Left [text "conan can only be used with the 'cl' or 'clang-cl' compiler on Windows"]
         Just conanCmd | isTargetWasm (target flags)
          -> do return $ Left [text "conan can not be used with a wasm target"]
         Just conanCmd
          -> do mbPkgDir <- getPackageDir conanCmd
                case mbPkgDir of
                  Nothing
                    -> do termWarning term flags $
                            text "unable to resolve conan package:" <+> clrSource (text pkg)
                          return (Left [])
                  Just (pkgName,pkgDir)
                    -> do termPhase term $ color (colorInterpreter (colorScheme flags)) $
                            text "package : conan" <+> clrSource (text pkgName) -- <.> colon <+> clrSource (text pkgDir)
                          let libDir = pkgDir ++ "/lib"
                          searchCLibrary flags cc clib [libDir]

  where
    pkgBase
      = takeWhile (/='/') pkg

    (baseSettings,conanEnv)
      = conanSettingsFromFlags flags cc

    settings
      = baseSettings ++ ["-o",pkgBase ++ "/*:shared=False","-o","shared=False"]

    clrSource doc
      = color (colorSource (colorScheme flags)) doc

    getPackageDir conanCmd
      = do mbPkgDir <- findPackagDir conanCmd
           case mbPkgDir of
             Just _   -> return mbPkgDir
             Nothing  -> do installPackage conanCmd
                            findPackagDir conanCmd

    findPackagDir conanCmd
      = do -- find out latest installed version
           let infoCmd = [conanCmd, "list", "-c", pkg]
           out <- runCommandRead term flags conanEnv infoCmd `catchIO` (\msg -> return "")
           -- termTrace term out
           let cachepkg = dropWhile isSpace $ concat $ take 1 $ dropWhile (all isSpace) $ reverse (lines out)
           if (not (cachepkg `startsWith` pkgBase))
             then return Nothing
             else -- and get the binary package location
                  do let queryCmd = [conanCmd, "install", "--requires", cachepkg] ++ settings
                     (_,out) <- runCommandReadAll term flags conanEnv queryCmd `catchIO` (\msg -> return ("",""))
                     -- termTrace term out
                     let prefix = cachepkg ++ ": Appending PATH environment variable: "
                         ppaths = [reverse $ drop 4 {- /bin -} $ reverse $
                                    drop (length prefix) line | line <- lines out, line `startsWith` prefix]
                     termTrace term (show (lines out))
                     termTrace term (unlines ppaths)
                     case ppaths of
                       [ppath] -> do exist <- doesDirectoryExist ppath
                                     return (if exist then Just (cachepkg,ppath) else Nothing)
                       _       -> return Nothing


    installPackage conanCmd
      = do let installCmd = [conanCmd, "install", "--build", "missing", "--requires", pkg] ++ settings
           if not (autoInstallLibs flags)
             then termWarning term flags (text "this module requires the conan package"
                    <+> clrSource (text pkg)
                    <+> text "         enable auto install using the \"--autoinstall\" option to koka,"
                    <+> text "         or install the package manually as:"
                    <-> text "         >" <+> clrSource (text (unwords installCmd))
                    <-> text "         to install the required C library and header files")
             else do let profileCmd = [conanCmd, "profile", "detect"] -- ensure default profile exists
                     sequential $ do runCommandReadAll term flags conanEnv profileCmd `catchIO` (\msg -> return ("",""))
                                     return ()
                     termPhase term (color (colorInterpreter (colorScheme flags)) (text "install: conan package:") <+> clrSource (text pkg))
                     sequential $ runCommandEnv term flags conanEnv installCmd


vcpkgCLibrary :: Terminal -> Flags -> (IO () -> IO ()) -> CC -> [(String,String)] -> FilePath -> String -> IO (Either [Doc] (FilePath,[FilePath]))
vcpkgCLibrary term flags sequential cc eimport clib pkg
  = do (root,vcpkg) <- vcpkgFindRoot (vcpkgRoot flags)
       exist <- doesFileExist vcpkg
       if (not exist)
         then do return $ Left [
                    text "this module requires vcpkg to install the" <+> clrSource (text clib) <+> text "library." <->
                    text "   hint: specify the root directory of vcpkg using the" <+> clrSource (text "--vcpkg=<dir>") <+> text "option" <->
                    text "         or the" <+> clrSource (text "VCPKG_ROOT") <+> text "environment variable," <->
                  (if onMacOS then
                   (text "         or install vcpkg as:" <->
                    text "         > brew install vcpkg")
                   else
                   (text "         or install vcpkg from <" <.> clrSource (text "https://vcpkg.io/en/getting-started.html") <.> text ">"))
                  ]
         else do let libDir = root ++ "/installed/" ++ (vcpkgTriplet flags)
                                ++ (if buildType flags <= Debug then "/debug/lib" else "/lib")
                 termPhase term $ color (colorInterpreter (colorScheme flags)) $
                    text "package : vcpkg" <+> clrSource (text pkg)
                 mbInstalled <- searchCLibrary flags cc clib [libDir]
                 case mbInstalled of
                   Right _ -> return mbInstalled
                   Left _  -> install root libDir vcpkg
  where
    clrSource doc
      = color (colorSource (colorScheme flags)) doc

    install rootDir libDir vcpkgCmd
      = do  let packageDir = joinPaths [rootDir,"packages",pkg ++ "_" ++ vcpkgTriplet flags]
            pkgExist <- doesDirectoryExist packageDir
            when (pkgExist) $
              termWarning term flags $
                text "vcpkg" <+> clrSource (text pkg) <+>
                text "is installed but the library" <+> clrSource (text clib) <+>
                text "is not found in" <+> clrSource (text libDir)
            let installCmd = [vcpkgCmd, "install", pkg ++ ":" ++ vcpkgTriplet flags, "--disable-metrics"]
            if (not (autoInstallLibs flags))
              then do termWarning term flags (text "this module requires vcpkg package"
                                              <+> clrSource (text pkg)
                                              <-> text "   hint: enable auto install using the \"--autoinstall\" option to koka,"
                                              <-> text "         or install the package manually as:"
                                              <-> text "         >" <+> clrSource (text (unwords installCmd))
                                              <-> text "         to install the required C library and header files")
                      return (Left [])
              else do termPhase term (color (colorInterpreter (colorScheme flags)) (text "install: vcpkg package:") <+> clrSource (text pkg))
                      sequential $ runCommand term flags installCmd
                      searchCLibrary flags cc clib [libDir] -- try to find again after install

clibsFromCore flags core    = externalImportKeyFromCore (target flags) (buildType flags) core "library"
csyslibsFromCore flags core = externalImportKeyFromCore (target flags) (buildType flags) core "syslib"


externalImportKeyFromCore :: Target -> BuildType -> Core.Core -> String -> [String]
externalImportKeyFromCore target buildType core key
  = catMaybes [Core.eimportLookup buildType key keyvals  | keyvals <- externalImportsFromCore target core]

externalImportsFromCore :: Target -> Core.Core -> [[(String,String)]]
externalImportsFromCore target core
  = [keyvals  | Core.ExternalImport imports _ <- Core.coreProgExternals core, (target,keyvals) <- imports]


{---------------------------------------------------------------
  kklib
---------------------------------------------------------------}

kklibBuild :: Terminal -> Flags -> (IO () -> IO ()) -> CC -> FilePath -> String -> FilePath -> IO FilePath
kklibBuild term flags sequential cc outDir name {-kklib-} objFile {-libkklib.o-}
  = do let objPath = joinPath outDir objFile  {-out/v2.x.x/clang-debug/libkklib.o-}
       exist <- doesFileExist objPath
       let binObjPath = joinPath (localLibDir flags) (buildVariant flags ++ "/" ++ objFile)
       let srcLibDir  = joinPath (localShareDir flags) (name)
       binExist <- doesFileExist binObjPath
       binNewer <- if (not binExist) then return False
                   else if (not exist) then return True
                   else do cmp <- fileTimeCompare binObjPath objPath
                           return (cmp==GT)
       srcNewer <- if (binNewer) then return False -- no need to check
                   else if (not exist) then return True
                   else do cmp <- fileTimeCompare (srcLibDir ++ "/include/kklib.h") objPath
                           return (cmp==GT)
       -- putStrLn ("binObjPath: " ++ binObjPath ++ ", newer: " ++ show binNewer)
       if (not binNewer && not srcNewer && not (rebuild flags))
        then return ()
         else if (binNewer)
           then -- use pre-compiled installed binary
                sequential $ copyBinaryFile binObjPath objPath
           else -- todo: check for installed binaries for the library
                -- compile kklib from sources
                do termInfo term $ color (colorInterpreter (colorScheme flags)) (text ("compile :")) <+>
                                   color (colorSource (colorScheme flags)) (text name) <+>
                                    color (colorInterpreter (colorScheme flags)) (text "from:") <+>
                                     color (colorSource (colorScheme flags)) (text srcLibDir)
                   let flags0 = if (useStdAlloc flags) then flags
                                  else flags{ ccompIncludeDirs = ccompIncludeDirs flags ++ [localShareDir flags ++ "/kklib/mimalloc/include"] }
                       flags1 = flags0{ ccompDefs = ccompDefs flags ++
                                                    [("KK_COMP_VERSION","\"" ++ version ++ "\""),
                                                     ("KK_CC_NAME", "\"" ++ ccName cc ++ "\"")] }
                   sequential $ ccompile term flags1 cc objPath [] [joinPath srcLibDir "src/all.c"]
       return objPath


{---------------------------------------------------------------
  Helpers
---------------------------------------------------------------}

termWarning term flags doc
  = termInfo term $ color (colorWarning (colorSchemeFromFlags flags)) (text "warning:" <+> doc)

runSystemEcho :: Terminal -> Flags -> String -> IO ()
runSystemEcho term flags cmd
  = do when (verbose flags >= 3) $
         termTrace term ("shell> " ++ cmd)
       runSystem cmd

runCommand :: Terminal -> Flags -> [String] -> IO ()
runCommand term flags cargs@(cmd:args)
  = do let command = unwords (shellQuote cmd : map shellQuote args)
       if (osName == "windows" && cmd `endsWith` "emcc") -- hack to run emcc correctly on windows (due to Python?)
         then runSystemEcho term flags command
         else  do when (verbose flags >= 3) $
                    termTrace term ("command> " ++ command) -- cmd ++ " [" ++ concat (intersperse "," args) ++ "]")
                  runCmd cmd (filter (not . null) args)
                    `catchIO` (\msg -> raiseIO ("error  : " ++ msg ++ "\ncommand: " ++ command))

runCommandRead :: Terminal -> Flags -> [(String,String)] -> [String] -> IO String
runCommandRead term flags env cargs
  = do (out,_) <- runCommandReadAll term flags env cargs
       return out

runCommandReadAll :: Terminal -> Flags -> [(String,String)] -> [String] -> IO (String,String)
runCommandReadAll term flags env cargs@(cmd:args)
  = do let command = unwords (shellQuote cmd : map shellQuote args)
       when (verbose flags >= 3) $
         termTrace term ("command> " ++ command) -- cmd ++ " [" ++ concat (intersperse "," args) ++ "]")
       runCmdRead env cmd (filter (not . null) args)
         `catchIO` (\msg -> raiseIO ("error  : " ++ msg ++ "\ncommand: " ++ command))

runCommandEnv :: Terminal -> Flags -> [(String,String)] -> [String] -> IO ()
runCommandEnv term flags env cargs@(cmd:args)
  = do let command = unwords (shellQuote cmd : map shellQuote args)
       when (verbose flags >= 3) $
         termTrace term ("command> " ++ command) -- cmd ++ " [" ++ concat (intersperse "," args) ++ "]")
       runCmdEnv env  cmd (filter (not . null) args)
         `catchIO` (\msg -> raiseIO ("error  : " ++ msg ++ "\ncommand: " ++ command))


shellQuote s
  = if (all (\c -> isAlphaNum c || c `elem` ":/-_.=") s) then s
     else "\"" ++ concatMap quote s ++ "\""
  where
    quote '"'  = "\\\""
    quote '\'' = "\\'"
    quote c    = [c]

joinWith sep xs
  = concat (intersperse sep xs)

dquote s
  = "\"" ++ s ++ "\""

compilerCatch comp term defValue io
  = io `catchSystem` \msg ->
    do (termError term) (errorMessageKind ErrBuild rangeNull
                           (hang 2 $ text ("failure during " ++ comp ++ ":")
                                           <-> string msg)) -- (fillSep $ map string $ words msg)))
       return defValue

catchSystem io f
  = io `catchIO` (\msg -> let cmdFailed = "command failed:" in
                          if (cmdFailed `isPrefixOf` msg)
                           then f (drop (length cmdFailed) msg)
                           else f msg)



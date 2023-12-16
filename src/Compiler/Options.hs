-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Main module.
-}
-----------------------------------------------------------------------------
module Compiler.Options( -- * Command line options
                         getOptions, processOptions, Mode(..), Flags(..), showTypeSigs
                       -- * Show standard messages
                       , showHelp, showEnv, showVersion, commandLineHelp, showIncludeInfo
                       -- * Utilities
                       , prettyEnvFromFlags
                       , colorSchemeFromFlags
                       , prettyIncludePath
                       , isValueFromFlags
                       , updateFlagsFromArgs
                       , CC(..), BuildType(..), ccFlagsBuildFromFlags
                       , buildType, unquote
                       , outName, fullBuildDir, buildVariant
                       , cpuArch, osName
                       , optionCompletions
                       , targetExeExtension
                       , targets
                       , conanSettingsFromFlags
                       , vcpkgFindRoot
                       , onWindows, onMacOS
                       ) where


import Data.Char              ( toLower, toUpper, isAlpha, isSpace )
import Data.List              ( intersperse )
import Control.Monad          ( when )
import qualified System.Info  ( os, arch )
import System.Environment     ( getArgs )
import System.Directory       ( doesFileExist, doesDirectoryExist, getHomeDirectory, getTemporaryDirectory )
import Platform.GetOptions
import Platform.Config
import Lib.PPrint
import Lib.Printer
import Common.Failure         ( raiseIO, catchIO )
import Common.ColorScheme
import Common.File        
import Common.Name    
import Common.Syntax          
import Compiler.Package
import Core.Core( dataInfoIsValue )
{--------------------------------------------------------------------------
  Convert flags to pretty environment
--------------------------------------------------------------------------}
import qualified Type.Pretty as TP

prettyEnvFromFlags :: Flags -> TP.Env
prettyEnvFromFlags flags
  = TP.defaultEnv{ TP.showKinds       = showKinds flags
                 , TP.expandSynonyms  = showSynonyms flags
                 , TP.colors          = colorSchemeFromFlags flags
                 , TP.htmlBases       = htmlBases flags
                 , TP.htmlCss         = htmlCss flags
                 , TP.htmlJs          = htmlJs flags
                 , TP.verbose         = verbose flags
                 , TP.coreShowTypes   = showCoreTypes flags
                 }


colorSchemeFromFlags :: Flags -> ColorScheme
colorSchemeFromFlags flags
  = colorScheme flags


prettyIncludePath :: Flags -> Doc
prettyIncludePath flags
  = let cscheme = colorScheme flags
        path    = includePath flags
    in align (if null path then color (colorSource cscheme) (text "<empty>")
               else cat (punctuate comma (map (\p -> color (colorSource cscheme) (text p)) path)))

{--------------------------------------------------------------------------
  Options
--------------------------------------------------------------------------}
data Mode
  = ModeHelp
  | ModeVersion
  | ModeCompiler       { files :: [FilePath] }
  | ModeInteractive    { files :: [FilePath] }
  | ModeLanguageServer { files :: [FilePath] }

data Option
  = Interactive
  | LanguageServer
  | Version
  | Help
  | Flag (Flags -> Flags)
  | Error String

showTypeSigs :: Flags -> Bool
showTypeSigs flags = showHiddenTypeSigs flags || _showTypeSigs flags

data Flags
  = Flags{ warnShadow       :: Bool
         , showKinds        :: Bool
         , showKindSigs     :: Bool
         , showSynonyms     :: Bool
         , showCore         :: Bool
         , showFinalCore    :: Bool
         , showCoreTypes    :: Bool
         , showAsmCS        :: Bool
         , showAsmJS        :: Bool
         , showAsmC         :: Bool
         , _showTypeSigs     :: Bool
         , showHiddenTypeSigs     :: Bool
         , showElapsed      :: Bool
         , evaluate         :: Bool
         , execOpts         :: String
         , library          :: Bool
         , target           :: Target
         , targetOS         :: String
         , targetArch       :: String
         , platform         :: Platform
         , stackSize        :: Int
         , heapSize         :: Int
         , simplify         :: Int
         , simplifyMaxDup   :: Int
         , colorScheme      :: ColorScheme
         , buildDir         :: FilePath      -- kkbuild
         , buildTag         :: String
         , outBuildDir      :: FilePath      -- actual build output: <builddir>/<version>-<buildtag>/<ccomp>-<variant>
         , outBaseName      :: String
         , outFinalPath     :: FilePath        
         , includePath      :: [FilePath]    -- .kk/.kki files 
         , csc              :: FileName
         , node             :: FileName
         , wasmrun          :: FileName
         , cmake            :: FileName
         , cmakeArgs        :: String
         , ccompPath        :: FilePath
         , ccompCompileArgs :: Args
         , ccompIncludeDirs :: [FilePath]
         , ccompDefs        :: [(String,String)]
         , ccompLinkArgs    :: Args
         , ccompLinkSysLibs :: [String]      -- just core lib name
         , ccompLinkLibs    :: [FilePath]    -- full path to library
         , ccomp            :: CC
         , ccompLibDirs     :: [FilePath]    -- .a/.lib dirs
         , autoInstallLibs  :: Bool
         , vcpkgRoot        :: FilePath
         , vcpkgTriplet     :: String
         {-
         , vcpkg            :: FilePath
         , vcpkgLibDir      :: FilePath
         , vcpkgIncludeDir  :: FilePath
         -}
         , conan            :: FilePath
         , editor           :: String
         , redirectOutput   :: FileName
         , outHtml          :: Int
         , htmlBases        :: [(String,String)]
         , htmlCss          :: String
         , htmlJs           :: String
         , verbose          :: Int
         , showSpan         :: Bool
         , console          :: String
         , rebuild          :: Bool
         , genCore          :: Bool
         , coreCheck        :: Bool
         , enableMon        :: Bool
         , semiInsert       :: Bool
         , genRangeMap      :: Bool
         , languageServerPort :: Int
         , localBinDir      :: FilePath  -- directory of koka executable
         , localDir         :: FilePath  -- install prefix: /usr/local
         , localLibDir      :: FilePath  -- precompiled object files: <prefix>/lib/koka/v2.x.x  /<cc>-<config>/libkklib.a, /<cc>-<config>/std_core.kki, ...
         , localShareDir    :: FilePath  -- sources: <prefix>/share/koka/v2.x.x  /lib/std, /lib/samples, /kklib
         , packages         :: Packages
         , forceModule      :: FilePath
         , debug            :: Bool      -- emit debug info
         , optimize         :: Int       -- optimization level; 0 or less is off
         , optInlineMax     :: Int
         , optctail         :: Bool
         , optctailCtxPath  :: Bool 
         , optUnroll        :: Int
         , optEagerPatBind  :: Bool      -- bind pattern fields as early as possible?
         , parcReuse        :: Bool
         , parcSpecialize   :: Bool
         , parcReuseSpec    :: Bool
         , parcBorrowInference    :: Bool
         , asan             :: Bool
         , useStdAlloc      :: Bool -- don't use mimalloc for better asan and valgrind support
         , optSpecialize    :: Bool
         , mimallocStats    :: Bool
         } deriving Eq

flagsNull :: Flags
flagsNull
  = Flags -- warnings
          True
          -- show
          False False  -- kinds kindsigs
          False False False False -- synonyms core fcore core-types
          False -- show asm
          False
          False
          False -- typesigs
          False -- hiddentypesigs
          False -- show elapsed time
          False -- do not execute by default
          ""    -- execution options
          False -- library
          (C LibC)  -- target
          osName  -- target OS
          cpuArch -- target CPU
          platform64
          0     -- stack size
          0     -- reserved heap size (for wasm)
          5     -- simplify passes
          10    -- simplify dup max (must be at least 10 to inline partial applications across binds)
          defaultColorScheme
          ""       -- builddir 
          ""       -- buildtag
          ("")     -- build dir
          ""       -- exe base name
          ""       -- final exe output path
          []       -- include paths
          "csc"
          "node"
          "wasmtime"
          "cmake"
          ""       -- cmake args
          
          ""       -- ccompPath
          []       -- ccomp args
          []       -- ccomp include dirs
          []       -- ccomp defs
          []       -- clink args
          []       -- clink sys libs
          []       -- clink full lib paths
          (ccGcc "gcc" 0 platform64 "gcc")
          (if onWindows then []        -- ccomp library dirs
                        else (["/usr/local/lib","/usr/lib","/lib"]
                               ++ if onMacOS then ["/opt/homebrew/lib"] else []))
          
          True     -- auto install libraries
          ""       -- vcpkg root
          ""       -- vcpkg triplet
          {-
          ""       -- vcpkg
          ""       -- vcpkg libdir
          ""       -- vcpkg incdir
          -}
          "conan"  -- conan command

          ""       -- editor
          ""
          0        -- out html
          []
          ("styles/" ++ programName ++ ".css")
          ("")
          1        -- verbosity
          False
          "ansi"  -- console: ansi, html, raw
          False -- rebuild
          False -- genCore
          False -- coreCheck
          True  -- enableMonadic
          True  -- semi colon insertion
          False -- generate range map
          6061  -- language server port
          ""    -- koka executable dir
          ""    -- prefix dir (default: <program-dir>/..)
          ""    -- localLib dir
          ""    -- localShare dir
          packagesEmpty -- packages
          "" -- forceModule
          True -- debug
          0    -- optimize
          12   -- inlineMax
          True -- optctail
          True -- optctailCtxPath
          (-1) -- optUnroll
          False -- optEagerPatBind (read fields as late as possible)
          True -- parc reuse
          True -- parc specialize
          True -- parc reuse specialize
          False -- parc borrow inference
          False -- use asan
          False -- use stdalloc
          True  -- use specialization (only used if optimization level >= 1)
          False -- use mimalloc stats

isHelp Help = True
isHelp _    = False

isVersion Version = True
isVersion _      = False

isInteractive Interactive = True
isInteractive _ = False

isLanguageServer LanguageServer = True
isLanguageServer _ = False

isValueFromFlags flags
 = dataInfoIsValue

{--------------------------------------------------------------------------
  Options and environment variables
--------------------------------------------------------------------------}
-- | The option table.
optionsAll :: [OptDescr Option]
optionsAll
 = let (xs,ys) = options in (xs++ys)

options :: ([OptDescr Option],[OptDescr Option])
options = (\(xss,yss) -> (concat xss, concat yss)) $ unzip
 [ option ['?','h'] ["help"]            (NoArg Help)                "show this information"
 , option []    ["version"]         (NoArg Version)                 "show the compiler version"
 , option ['p'] ["prompt"]          (NoArg Interactive)             "interactive mode"
 , option []    ["language-server"] (NoArg LanguageServer)          "language server mode"
 , flag   ['e'] ["execute"]         (\b f -> f{evaluate= b})        "compile and execute"
 , flag   ['c'] ["compile"]         (\b f -> f{evaluate= not b})    "only compile, do not execute (default)"
 , option ['i'] ["include"]         (OptArg includePathFlag "dirs") "add <dirs> to module search path (empty resets)"
 , option ['o'] ["output"]          (ReqArg outFinalPathFlag "file")"write final executable to <file> (without extension)"
 , numOption 0 "n" ['O'] ["optimize"]   (\i f -> f{optimize=i})     "optimize (0=default, 1=space, 2=full, 3=aggressive)"
 , flag   ['g'] ["debug"]           (\b f -> f{debug=b})            "emit debug information (on by default)" 
 , numOption 1 "n" ['v'] ["verbose"] (\i f -> f{verbose=i})         "verbosity 'n' (0=quiet, 1=default, 2=trace)"
 , flag   ['r'] ["rebuild"]         (\b f -> f{rebuild = b})        "rebuild all"
 , flag   ['l'] ["library"]         (\b f -> f{library=b, evaluate=if b then False else (evaluate f) }) "generate a library"
 , configstr [] ["target"]          (map fst targets) "tgt" targetFlag  ("target: " ++ show (map fst targets))
 -- , config []    ["host"]            [("node",Node),("browser",Browser)] "host" (\h f -> f{ target=JS, host=h}) "specify host for javascript: <node|browser>"
 , emptyline

 , option []    ["buildtag"]        (ReqArg buildTagFlag "tag")     "set build variant tag (e.g. 'bundle' or 'dev')"
 , option []    ["builddir"]        (ReqArg buildDirFlag "dir")     ("build under <dir> ('" ++ kkbuild ++ "' by default)")
 , option []    ["buildname"]       (ReqArg outBaseNameFlag "name") "base name of the final output"
 , option []    ["outputdir"]       (ReqArg outBuildDirFlag "dir")  "write intermediate files in <dir>.\ndefaults to: <builddir>/<ver>-<buildtag>/<cc>-<variant>"
 
 , option []    ["libdir"]          (ReqArg libDirFlag "dir")       "object library <dir> (= <prefix>/lib/koka/<ver>)"
 , option []    ["sharedir"]        (ReqArg shareDirFlag "dir")     "source library <dir> (= <prefix>/share/koka/<ver>)"
 , option []    ["cc"]              (ReqArg ccFlag "cmd")           "use <cmd> as the C backend compiler "
 , option []    ["ccincdir"]        (OptArg ccIncDirs "dirs")       "search semi-colon separated <dirs> for headers"
 , option []    ["cclibdir"]        (OptArg ccLibDirs "dirs")       "search semi-colon separated <dirs> for libraries"
 , option []    ["cclib"]           (ReqArg ccLinkSysLibs "libs")   "link with semi-colon separated system <libs>"
 , option []    ["ccopts"]          (OptArg ccCompileArgs "opts")   "pass <opts> to C backend compiler "
 , option []    ["cclinkopts"]      (OptArg ccLinkArgs "opts")      "pass <opts> to C backend linker "
 , option []    ["cclibpath"]       (OptArg ccLinkLibs "lpath")     "link with semi-colon separated libraries <lpath>"
 , option []    ["vcpkg"]           (ReqArg ccVcpkgRoot "dir")      "vcpkg root directory"
 , option []    ["vcpkgtriplet"]    (ReqArg ccVcpkgTriplet "tt")    "vcpkg target triplet"
 , option []    ["conan"]           (ReqArg ccConan "cmd")          "conan command"
 , flag   []    ["autoinstall"]     (\b f -> f{autoInstallLibs=b})  "automatically download required packages"
 , option []    ["csc"]             (ReqArg cscFlag "cmd")          "use <cmd> as the csharp backend compiler "
 , option []    ["node"]            (ReqArg nodeFlag "cmd")         "use <cmd> to execute node"
 , option []    ["wasmrun"]         (ReqArg wasmrunFlag "cmd")      "use <cmd> to execute wasm"
 , option []    ["editor"]          (ReqArg editorFlag "cmd")       "use <cmd> as editor"
 , option []    ["stack"]           (ReqArg stackFlag "size")       "set stack size (0 for platform default)"
 , option []    ["heap"]            (ReqArg heapFlag "size")        "set reserved heap size (0 for platform default)"
 , option []    ["color"]           (ReqArg colorFlag "colors")     "set colors"
 , option []    ["redirect"]        (ReqArg redirectFlag "file")    "redirect output to <file>"
 , configstr [] ["console"]  ["ansi","html","raw"] "fmt" (\s f -> f{ console = s }) "console output format: <ansi|html|raw>"
 
 , flag   []    ["html"]            (\b f -> f{outHtml = if b then 2 else 0}) "generate documentation"
 , option []    ["htmlbases"]       (ReqArg htmlBasesFlag "bases")  "set link prefixes for documentation"
 , option []    ["htmlcss"]         (ReqArg htmlCssFlag "link")     "set link to the css documentation style"
 , emptyline
 
 , flag   []    ["showtime"]       (\b f -> f{ showElapsed = b})    "show elapsed time and rss after evaluation"
 , flag   []    ["showspan"]       (\b f -> f{ showSpan = b})       "show ending row/column too on errors"
 , flag   []    ["showkindsigs"]   (\b f -> f{showKindSigs=b})      "show kind signatures of type definitions"
 , flag   []    ["showtypesigs"]   (\b f -> f{_showTypeSigs=b})      "show type signatures of definitions"
 , flag   []    ["showhiddentypesigs"]   (\b f -> f{showHiddenTypeSigs=b})"(implies --showtypesigs) show hidden type signatures of definitions"
 , flag   []    ["showsynonyms"]   (\b f -> f{showSynonyms=b})      "show expanded type synonyms in types"
 , flag   []    ["showcore"]       (\b f -> f{showCore=b})          "show core"
 , flag   []    ["showfcore"]      (\b f -> f{showFinalCore=b})     "show final core (with backend optimizations)"
 , flag   []    ["showcoretypes"]  (\b f -> f{showCoreTypes=b})     "show full types in core"
 , flag   []    ["showcs"]         (\b f -> f{showAsmCS=b})         "show generated c#"
 , flag   []    ["showjs"]         (\b f -> f{showAsmJS=b})         "show generated javascript"
 , flag   []    ["showc"]          (\b f -> f{showAsmC=b})          "show generated C"
 , flag   []    ["core"]           (\b f -> f{genCore=b})           "generate a core file"
 , flag   []    ["checkcore"]      (\b f -> f{coreCheck=b})         "check generated core" 
 , emptyline

 -- hidden
 , hide $ fflag       ["asan"]      (\b f -> f{asan=b})             "compile with address, undefined, and leak sanitizer"
 , hide $ fflag       ["stdalloc"]  (\b f -> f{useStdAlloc=b})      "use the standard libc allocator"
 , hide $ fflag       ["allocstats"]  (\b f -> f{mimallocStats=b})   "enable mimalloc statitistics"
 , hide $ fnum 3 "n"  ["simplify"]  (\i f -> f{simplify=i})          "enable 'n' core simplification passes"
 , hide $ fnum 10 "n" ["maxdup"]    (\i f -> f{simplifyMaxDup=i})    "set 'n' as maximum code duplication threshold"
 , hide $ fnum 10 "n" ["inline"]    (\i f -> f{optInlineMax=i})      "set 'n' as maximum inline threshold (=10)"
 , hide $ fflag       ["monadic"]   (\b f -> f{enableMon=b})         "enable monadic translation"
 , hide $ flag []     ["semi"]      (\b f -> f{semiInsert=b})        "insert semicolons based on layout"
 , hide $ fflag       ["binference"]  (\b f -> f{parcBorrowInference=b})     "enable reuse inference (does not work cross-module!)"
 , hide $ fflag       ["reuse"]       (\b f -> f{parcReuse=b})        "enable in-place update analysis"
 , hide $ fflag       ["dropspec"]    (\b f -> f{parcSpecialize=b})   "enable drop specialization"
 , hide $ fflag       ["reusespec"]   (\b f -> f{parcReuseSpec=b})    "enable reuse specialization"
 , hide $ fflag       ["trmc"]        (\b f -> f{optctail=b})         "enable tail-recursion-modulo-cons optimization"
 , hide $ fflag       ["trmcctx"]     (\b f -> f{optctailCtxPath=b})  "enable trmc context paths"
 , hide $ fflag       ["specialize"]  (\b f -> f{optSpecialize=b})    "enable inline specialization"
 , hide $ fflag       ["unroll"]      (\b f -> f{optUnroll=(if b then 1 else 0)}) "enable recursive definition unrolling"
 , hide $ fflag       ["eagerpatbind"] (\b f -> f{optEagerPatBind=b}) "load pattern fields as early as possible"
 , numOption (-1) "port" [] ["lsport"]    (\i f -> f{languageServerPort=i}) "Language Server port to connect to"

 -- deprecated
 , hide $ option []    ["cmake"]           (ReqArg cmakeFlag "cmd")        "use <cmd> to invoke cmake"
 , hide $ option []    ["cmakeopts"]       (ReqArg cmakeArgsFlag "opts")   "pass <opts> to cmake"
 ]
 where
  emptyline
    = flag [] [] (\b f -> f) ""

  option short long f desc
    = ([Option short long f desc],[])

  flag short long f desc
    = ([Option short long (NoArg (Flag (f True))) desc]
      ,[Option [] (map ("no-" ++) long) (NoArg (Flag (f False))) ""])

  numOption def optarg short long f desc
    = ([Option short long (OptArg (\mbs -> Flag (numOptionX def f mbs)) optarg) desc]
      ,[Option [] (map ("no-" ++) long) (NoArg (Flag (f (-1)))) ""])

  -- feature flags
  fflag long f desc
    = ([Option [] (map ("f"++) long) (NoArg (Flag (f True))) desc]
      ,[Option [] (map ("fno-" ++) long) (NoArg (Flag (f False))) ""])

  fnum def optarg long f desc
    = ([Option [] (map ("f"++) long) (OptArg (\mbs -> Flag (numOptionX def f mbs)) optarg) desc]
      ,[Option [] (map ("fno-" ++) long) (NoArg (Flag (f (-1)))) ""])

  hide (vis,hidden)
    = ([],vis ++ hidden)

  numOptionX def f mbs
    = case mbs of
        Nothing -> f def
        Just s  -> case reads s of
                     ((i,""):_) -> f i
                     _ -> f def  -- parse error

  config short long opts argDesc f desc
    = option short long (ReqArg validate valid) desc
    where
      valid = if null argDesc then "(" ++ concat (intersperse "|" (map fst opts)) ++ ")"
                              else argDesc
      validate s
        = case lookup s opts of
            Just x -> Flag (\flags -> f x flags)
            Nothing -> Error ("invalid value for --" ++ head long ++ " option, expecting any of " ++ valid)

  configstr short long opts argDesc f desc
    = config short long (map (\s -> (s,s)) opts) argDesc f desc

  targetFlag t f
    = case lookup t targets of
        Just update -> update f
        Nothing     -> f

  colorFlag s
    = Flag (\f -> f{ colorScheme = readColorFlags s (colorScheme f) })

  htmlBasesFlag s
    = Flag (\f -> f{ htmlBases = (htmlBases f) ++ readHtmlBases s })

  htmlCssFlag s
    = Flag (\f -> f{ htmlCss = s })

  includePathFlag mbs
    = Flag (\f -> f{ includePath = case mbs of
                                     Just s | not (null s) -> includePath f ++ undelimPaths s
                                     _ -> [] })

  buildDirFlag s
    = Flag (\f -> f{ buildDir = s })

  buildTagFlag s
    = Flag (\f -> f{ buildTag = s })    

  outBuildDirFlag s
    = Flag (\f -> f{ outBuildDir = s })

  libDirFlag s
    = Flag (\f -> f{ localLibDir = s })

  shareDirFlag s
    = Flag (\f -> f{ localShareDir = s })

  outBaseNameFlag s
    = Flag (\f -> f{ outBaseName = s })

  outFinalPathFlag s
    = Flag (\f -> f{ outFinalPath = s })    

  ccFlag s
    = Flag (\f -> f{ ccompPath = s })

  extendArgs prev mbs 
    = case mbs of Just s | not (null s) -> prev ++ unquote s
                  _      -> []
  
  ccCompileArgs mbs
    = Flag (\f -> f{ ccompCompileArgs = extendArgs (ccompCompileArgs f) mbs })

  ccIncDirs mbs
    = Flag (\f -> f{ ccompIncludeDirs = case mbs of
                                          Just s | not (null s) -> ccompIncludeDirs f ++ undelimPaths s
                                          _ -> [] })
  ccLibDirs mbs
    = Flag (\f -> f{ ccompLibDirs = case mbs of
                                          Just s | not (null s) -> ccompLibDirs f ++ undelimPaths s
                                          _ -> [] })


  ccLinkArgs mbs
    = Flag (\f -> f{ ccompLinkArgs = extendArgs (ccompLinkArgs f) mbs })

  ccLinkSysLibs s
    = Flag (\f -> f{ ccompLinkSysLibs = ccompLinkSysLibs f ++ undelimPaths s })
  ccLinkLibs mbs
    = Flag (\f -> f{ ccompLinkLibs = case mbs of
                                      Just s | not (null s) -> ccompLinkLibs f ++ undelimPaths s
                                      _ -> [] })
  ccVcpkgRoot dir
    = Flag (\f -> f{vcpkgRoot = dir })

  ccVcpkgTriplet triplet
    = Flag (\f -> f{vcpkgTriplet = triplet })

  ccConan cmd
    = Flag (\f -> f{conan = cmd })

  cscFlag s
    = Flag (\f -> f{ csc = s })

  nodeFlag s
    = Flag (\f -> f{ node = s })

  wasmrunFlag s
    = Flag (\f -> f{ wasmrun = s })

  editorFlag s
    = Flag (\f -> f{ editor = s })

  redirectFlag s
    = Flag (\f -> f{ redirectOutput = s })

  cmakeFlag s
      = Flag (\f -> f{ cmake = s })

  cmakeArgsFlag s
      = Flag (\f -> f{ cmakeArgs = s })

  stackFlag s
    = case parseSize s of
        Just n -> Flag (\f -> f{ stackSize = n })
        _      -> Flag (id)

  heapFlag s
    = case parseSize s of
        Just n -> Flag (\f -> f{ heapSize = n })
        _      -> Flag (id)        
    
  parseSize :: String -> Maybe Int
  parseSize s = case reads (map toLower s) of
                    [(n,rest)] | rest `elem` ["k","kb","kib"] -> Just (1024*n)
                               | rest `elem` ["m","mb","mib"] -> Just (1024*1024*n)
                               | rest `elem` ["g","gb","gib"] -> Just (1024*1024*1024*n)
                               | null rest                    -> Just n
                    _ -> Nothing

  
readHtmlBases :: String -> [(String,String)]
readHtmlBases s
  = map toBase (splitComma s)
  where
    splitComma :: String -> [String]
    splitComma xs
      = let (pre,ys) = span (/=',') xs
        in case ys of
             (_:post) -> pre : splitComma post
             []       -> [pre]

    toBase xs
      = let (pre,ys) = span (/='=') xs
        in case ys of
             (_:post) -> (pre,post)
             _        -> ("",xs)

targets :: [(String,Flags -> Flags)]
targets =
    [("c",      \f -> f{ target=C LibC, platform=platform64 }),
     ("c64",    \f -> f{ target=C LibC, platform=platform64 }),
     ("c32",    \f -> f{ target=C LibC, platform=platform32 }),
     ("c64c",   \f -> f{ target=C LibC, platform=platform64c }),
     ("js",     \f -> f{ target=JS JsNode, platform=platformJS }),
     ("jsnode", \f -> f{ target=JS JsNode, platform=platformJS }),
     ("jsweb",  \f -> f{ target=JS JsWeb, platform=platformJS }),
     ("wasm",   \f -> f{ target=C Wasm, platform=platform32 }),
     ("wasm32", \f -> f{ target=C Wasm, platform=platform32 }),
     ("wasm64", \f -> f{ target=C Wasm, platform=platform64 }),
     ("wasmjs", \f -> f{ target=C WasmJs, platform=platform32 }),
     ("wasmweb",\f -> f{ target=C WasmWeb, platform=platform32 }),
     ("cs",     \f -> f{ target=CS, platform=platformCS })
    ]

-- | Environment table
environment :: [ (String, String, (String -> [String]), String) ]
environment
  = [ -- ("koka_dir",     "dir",     dirEnv,       "The install directory")
      ("koka_options", "options", flagsEnv,         "Add <options> to the command line")
    , ("koka_editor",  "command", opt "editor",     "Use <cmd> as the editor (substitutes %l, %c, and %f)")
    , ("koka_vcpkg",   "dir",     opt "vcpkg",      "Set vcpkg root directory")
    , ("koka_lib_dir", "dir",     opt "libdir",     "Set the koka compiled library directory (= '<prefix>/lib/koka/<ver>')")
    , ("koka_share_dir", "dir",   opt "sharedir",   "Set the koka library sources directory (= '<prefix>/share/koka/<ver>')")
    , ("koka_build_dir", "dir",   opt "builddir",   ("Set the default koka build directory (= '" ++ kkbuild ++ "')"))
    ]
  where
    flagsEnv s      = [s]
    opt name dir    = ["--" ++ name ++ "=" ++ quote dir]
    

optionCompletions :: [(String,String)]
optionCompletions 
  = concatMap complete (fst options)
  where
    complete :: OptDescr Option -> [(String,String)]
    complete (Option shorts longs arg help)
      = let lreq = case arg of ReqArg _ _ -> "="
                               _          -> ""
            sreq = case arg of ReqArg _ _ -> " "
                               _          -> ""
        in zip ((map (\c -> "-" ++ [c] ++ sreq) shorts) ++ (map (\s -> "--" ++ s ++ lreq) longs))
               (repeat help)
        

{--------------------------------------------------------------------------
  Process options
--------------------------------------------------------------------------}
getOptions :: String -> IO (Flags,Flags,Mode)
getOptions extra
  = do env  <- getEnvOptions
       args <- getArgs
       processOptions flagsNull (env ++ words extra ++ args)

updateFlagsFromArgs :: Flags -> String -> Maybe Flags
updateFlagsFromArgs flags0 args =
  let 
    (preOpts,postOpts) = span (/="--") (words args)
    flags1 = case postOpts of
                   [] -> flags0
                   (_:rest) -> flags0{ execOpts = concat (map (++" ") rest) }
    (options,files,errs0) = getOpt Permute optionsAll preOpts
    errs = errs0 ++ extractErrors options
    in if (null errs)
        then Just $ extractFlags flags1 options else Nothing

processOptions :: Flags -> [String] -> IO (Flags,Flags,Mode)
processOptions flags0 opts
  = let (preOpts,postOpts) = span (/="--") opts
        flags1 = case postOpts of
                   [] -> flags0
                   (_:rest) -> flags0{ execOpts = concat (map (++" ") rest) }
        (options,files,errs0) = getOpt Permute optionsAll preOpts
        errs = errs0 ++ extractErrors options
    in if (null errs)
        then let flags2 = extractFlags flags1 options
                 mode = if (any isHelp options) then ModeHelp
                        else if (any isVersion options) then ModeVersion
                        else if (any isInteractive options) then ModeInteractive files
                        else if (any isLanguageServer options) then ModeLanguageServer files
                        else if (null files) then ModeInteractive files
                                             else ModeCompiler files                 
                 flags = case mode of 
                           ModeInteractive _ -> flags2{evaluate = True}
                           _                 -> flags2
             in do buildDir <- getKokaBuildDir (buildDir flags) (evaluate flags)
                   ed   <- if (null (editor flags))
                            then detectEditor 
                            else return (editor flags)
                   pkgs <- discoverPackages buildDir

                   (localDir,localLibDir,localShareDir,localBinDir) 
                        <- getKokaDirs (localLibDir flags) (localShareDir flags) buildDir
                   normalizedIncludes <- mapM realPath (includePath flags)
                   
                   -- cc
                   ccmd <- if (ccompPath flags == "") then detectCC (target flags)
                           else if (ccompPath flags == "mingw") then return "gcc"
                           else return (ccompPath flags)
                   (cc,asan) <- ccFromPath flags ccmd
                   ccCheckExist cc
                   let stdAlloc = if asan then True else useStdAlloc flags   -- asan implies useStdAlloc
                       cdefs    = ccompDefs flags 
                                   ++ (if stdAlloc then [] else [("KK_MIMALLOC",show (sizePtr (platform flags)))])
                                   ++ (if (buildType flags > DebugFull) then [] else [("KK_DEBUG_FULL","")])
                                   ++ (if optctailCtxPath flags then [] else [("KK_CTAIL_NO_CONTEXT_PATH","")])
                                   ++ (if platformHasCompressedFields (platform flags) then [("KK_INTB_SIZE",show (sizeField (platform flags)))] else [])
                                   ++ (if not stdAlloc && mimallocStats flags then [("MI_STAT","2")] else [])
                   
                   -- vcpkg
                   -- (vcpkgRoot,vcpkg) <- vcpkgFindRoot (vcpkgRoot flags)
                   let triplet          = if (not (null (vcpkgTriplet flags))) then vcpkgTriplet flags
                                            else if (isTargetWasm (target flags))
                                              then ("wasm" ++ show (8*sizePtr (platform flags)) ++ "-emscripten")
                                              else tripletArch ++ 
                                                    (if onWindows 
                                                        then (if (ccName cc `startsWith` "mingw") 
                                                                then "-mingw-static"
                                                                else "-windows-static-md")
                                                        else ("-" ++ tripletOsName))
                       {-
                       vcpkgInstalled   = (vcpkgRoot) ++ "/installed/" ++ triplet
                       vcpkgIncludeDir  = vcpkgInstalled ++ "/include"
                       vcpkgLibDir      = vcpkgInstalled ++ (if buildType flags <= Debug then "/debug/lib" else "/lib")
                       vcpkgLibDirs     = if (null vcpkg) then [] else [vcpkgLibDir]
                       vcpkgIncludeDirs = if (null vcpkg) then [] else [vcpkgIncludeDir] 
                       -}
                   return (flags{ packages    = pkgs,
                                  buildDir    = buildDir,
                                  localBinDir = localBinDir,
                                  localDir    = localDir,
                                  localLibDir = localLibDir,
                                  localShareDir = localShareDir,

                                  outBaseName = if null (outBaseName flags) && not (null (outFinalPath flags))
                                                  then basename (outFinalPath flags)
                                                  else outBaseName flags,

                                  optSpecialize  = if (optimize flags <= 0) then False 
                                                    else (optSpecialize flags),
                                  optInlineMax   = if (optimize flags < 0) 
                                                     then 0
                                                     else if (optimize flags <= 1) 
                                                       then (optInlineMax flags) `div` 3 
                                                       else (optInlineMax flags),
                                  optctailCtxPath = (optctailCtxPath flags && isTargetC (target flags)),
                                  optUnroll   = if (optUnroll flags < 0) 
                                                  then (if (optimize flags > 0) then 1 else 0)
                                                  else optUnroll flags,
                                  ccompPath   = ccmd,
                                  ccomp       = cc,
                                  ccompDefs   = cdefs,
                                  asan        = asan,
                                  useStdAlloc = stdAlloc,
                                  editor      = ed,
                                  includePath = (localShareDir ++ "/lib") : (map normalize normalizedIncludes),
                                  genRangeMap = outHtml flags > 0 || any isLanguageServer options,
                                  vcpkgTriplet= triplet
                                  

                                  {-
                                  vcpkgRoot   = vcpkgRoot,
                                  vcpkg       = vcpkg,
                                  vcpkgTriplet= triplet,
                                  vcpkgIncludeDir  = vcpkgIncludeDir,
                                  vcpkgLibDir      = vcpkgLibDir
                                  -}
                                  -- ccompLibDirs     = vcpkgLibDirs ++ ccompLibDirs flags
                                  -- ccompIncludeDirs = vcpkgIncludeDirs ++ ccompIncludeDirs flags  -- include path added when a library is used
                               }
                          ,flags,mode)
        else invokeError errs

getKokaBuildDir :: FilePath -> Bool -> IO FilePath
getKokaBuildDir "" eval
  = if (eval)
      then do exist <- doesDirectoryExist kkbuild
              if (exist)
                then return kkbuild
                else do tmp <- getTemporaryDirectory
                        return (joinPath tmp kkbuild)
      else return kkbuild
getKokaBuildDir buildDir _ = return buildDir    


kkbuild :: String
kkbuild = ".koka"

getKokaDirs :: FilePath -> FilePath -> FilePath -> IO (FilePath,FilePath,FilePath,FilePath)
getKokaDirs libDir1 shareDir1 buildDir0
  = do bin        <- getProgramPath
       let binDir  = dirname bin
           rootDir = rootDirFrom binDir
       isRootRepo <- doesDirectoryExist (joinPath rootDir "kklib")       
       let libDir   = if (not (null libDir1)) then libDir1
                      else if (isRootRepo) then joinPath rootDir kkbuild
                      else joinPath rootDir ("lib/koka/v" ++ version)
           shareDir = if (not (null shareDir1)) then shareDir1           
                      else if (isRootRepo) then rootDir
                      else joinPath rootDir ("share/koka/v" ++ version)
       return (normalizeWith '/' rootDir,
               normalizeWith '/' libDir,
               normalizeWith '/' shareDir,
               normalizeWith '/' binDir)

rootDirFrom :: FilePath -> FilePath
rootDirFrom binDir
 = case span (/="dist-newstyle") (reverse (splitPath binDir)) of
     -- cabal
     (_, _:es) -> joinPaths (reverse es)
     -- other
     (rs,[]) -> case rs of
                  -- stack build
                  ("bin":_:"install":".stack-work":es)     -> joinPaths (reverse es)
                  ("bin":_:_:"install":".stack-work":es)   -> joinPaths (reverse es)
                  ("bin":_:_:_:"install":".stack-work":es) -> joinPaths (reverse es)
                  -- regular install
                  ("bin":es)   -> joinPaths (reverse es)
                  -- minbuild 
                  (_:dir:es) | dir == kkbuild -> joinPaths (reverse es)
                  _          -> binDir


extractFlags :: Flags -> [Option] -> Flags
extractFlags flagsInit options
  = let flags = foldl extract flagsInit options
    in flags
  where
    extract flags (Flag f)  = f flags
    extract flags _         = flags

extractErrors :: [Option] -> [String]
extractErrors options
  = concatMap extract options
  where
    extract (Error s) = [s ++ "\n"]
    extract _         = []

getEnvOptions :: IO [String]
getEnvOptions
  = do csc <- getEnvCsc
       xss <- mapM getEnvOption environment
       return (concat (csc:xss))
  where
    getEnvOption (envName,_,extract,_)
      = do s <- getEnvVar envName
           if null s
            then return []
            else return (extract s)

    getEnvCsc
      = do fw <- getEnvVar "FRAMEWORK"
           fv <- getEnvVar "FRAMEWORKVERSION"
           if (null fw || null fv)
            then do mbsroot <- getEnvVar "SYSTEMROOT"
                    let sroot = if null mbsroot then "c:\\windows" else mbsroot
                        froot = joinPath sroot "Microsoft.NET\\Framework"
                    mbcsc <- searchPaths [joinPath froot "v4.0.30319"
                                         ,joinPath froot "v3.5"
                                         ,joinPath froot "v3.0"
                                         ,joinPath froot "v2.0.50727"
                                         ,joinPath froot "v1.1.4322"]
                                         [exeExtension] "csc"
                    case mbcsc of
                      Nothing  -> return []
                      Just csc -> return ["--csc=" ++ csc ]
            else return ["--csc="++ joinPaths [fw,fv,"csc"]]


vcpkgFindRoot :: FilePath -> IO (FilePath,FilePath)
vcpkgFindRoot root
  = if (null root) 
      then do eroot <- getEnvVar "VCPKG_ROOT"
              if (not (null eroot))
                then return (eroot, joinPath eroot vcpkgExe)
                else do homeDir <- getHomeDirectory
                        paths   <- getEnvPaths "PATH"
                        mbFile  <- searchPaths (paths ++ [joinPaths [homeDir,"vcpkg"]]) [] vcpkgExe
                        case mbFile of
                          Nothing     -> return ("", vcpkgExe)
                          Just fname0 -> do fname <- realPath fname0
                                            let root = case (reverse (splitPath (dirname fname))) of
                                                         ("bin":dirs) -> joinPaths (reverse ("libexec":dirs)) 
                                                         _ -> dirname fname
                                            return (root, fname)
      else return (root, joinPath root vcpkgExe)
  where 
    vcpkgExe = "vcpkg" ++ exeExtension


conanSettingsFromFlags :: Flags -> CC -> ([String],[(String,String)])
conanSettingsFromFlags flags cc
  = let name = ccName cc
        clRuntime = ["-s","compiler.runtime=" ++ (if buildType flags <= Debug then "MDd" else "MD")] 
        -- conan compiler <https://docs.conan.io/en/latest/integrations/compilers.html>
        settings  | (name `startsWith` "clang-cl") -- <https://github.com/conan-io/conan/pull/5705>
                  = clRuntime  
                  | (name `startsWith` "mingw") 
                  = []
                  | (name `startsWith` "emcc") 
                  = ["-s","os=Emscripten"] ++ 
                    (case target flags of  -- <https://docs.conan.io/en/latest/integrations/cross_platform/emscripten.html>
                       C Wasm | sizePtr (platform flags) == 4  -> ["-s","arch=wasm"]
                       C Wasm | sizePtr (platform flags) == 8  -> ["-s","arch=wasm64"]
                       C WasmJs -> ["-s","arch=asm.js"]
                       _        -> []
                    )
                  | (name `startsWith` "clang" || name `startsWith` "musl-clang") = []
                  | (name `startsWith` "musl-gcc" || name `startsWith` "musl-g++") = []
                  | (name `startsWith` "gcc" || name `startsWith` "g++")   = []
                  | (name `startsWith` "cl")    
                  = clRuntime
                  | (name `startsWith` "icc")   = []
                  | otherwise = []
        build     = ["-s","build_type=" ++ case buildType flags of
                        DebugFull -> "Debug"
                        Debug     -> "Debug"
                        RelWithDebInfo -> "Release"  -- "RelWithDebInfo" -- often not available
                        Release        -> "Release"]

    in ( build ++ settings
       , [("CC",ccPath cc)]  -- set CXX as well?
         ++ (if onWindows then [("CONAN_CMAKE_GENERATOR","Ninja")] else [])
       )


{--------------------------------------------------------------------------
  Detect C compiler
--------------------------------------------------------------------------}

type Args = [String]

data CC = CC{  ccName       :: String,
               ccPath       :: FilePath,               
               ccFlags      :: Args,
               ccFlagsBuild :: [(BuildType,Args)],
               ccFlagsWarn  :: Args,
               ccFlagsCompile :: Args,
               ccFlagsLink    :: Args,
               ccFlagStack  :: Int -> Args,
               ccFlagHeap   :: Int -> Args,
               ccAddLibraryDir :: FilePath -> Args,
               ccIncludeDir :: FilePath -> Args,
               ccTargetObj  :: FilePath -> Args,
               ccTargetExe  :: FilePath -> Args,
               ccAddSysLib  :: String -> Args,
               ccAddLib     :: FilePath -> Args,
               ccAddDef     :: (String,String) -> Args,
               ccLibFile    :: String -> FilePath,  -- make lib file name
               ccObjFile    :: String -> FilePath   -- make object file namen
            }

instance Eq CC where
  CC{ccName = name1, ccPath = path1, ccFlags = flags1, ccFlagsBuild = flagsB1, ccFlagsCompile= flagsC1, ccFlagsLink=flagsL1} == 
    CC{ccName = name2, ccPath = path2, ccFlags = flags2, ccFlagsBuild = flagsB2, ccFlagsCompile= flagsC2, ccFlagsLink=flagsL2}
    = name1 == name2 && path1 == path2 && flags1 == flags2 && flagsB1 == flagsB2 && flagsC1 == flagsC2 && flagsL1 == flagsL2

targetExeExtension target
  = case target of
      C Wasm   -> ".wasm"
      C WasmJs -> ".js"
      C WasmWeb-> ".html"
      C _      -> exeExtension
      JS JsWeb -> ".html"
      JS _     -> ".mjs"
      _        -> exeExtension

targetObjExtension target
  = case target of
      C Wasm   -> ".o"
      C WasmJs -> ".o"
      C WasmWeb-> ".o"
      C _      -> objExtension
      JS _     -> ".mjs"
      _        -> objExtension 

targetLibFile target fname
  = case target of
      C Wasm   -> "lib" ++ fname ++ ".a"
      C WasmJs -> "lib" ++ fname ++ ".a"
      C WasmWeb-> "lib" ++ fname ++ ".a"
      C _      -> libPrefix ++ fname ++ libExtension
      JS _     -> fname ++ ".mjs" -- ?
      _        -> libPrefix ++ fname ++ libExtension

outName :: Flags -> FilePath -> FilePath
outName flags s
  = joinPath (fullBuildDir flags) s

fullBuildDir :: Flags -> FilePath    -- usually <buildDir>/windows-x64-v2.x.x/<config>
fullBuildDir flags
  = if (null (outBuildDir flags))
     then joinPaths [buildDir flags, buildVersionTag flags, buildVariant flags]
     else outBuildDir flags

buildVersionTag :: Flags -> String   
buildVersionTag flags
  = "v" ++ version ++ (if (null (buildTag flags)) then "" else "-" ++ buildTag flags)

buildVariant :: Flags -> String   -- for example: clang-debug, js-release
buildVariant flags
  = let pre  = case target flags of
                 C ctarget 
                   -> ccName (ccomp flags) ++ 
                      (case ctarget of
                        Wasm   -> "-wasm" ++ show (8*sizePtr (platform flags))
                        WasmJs -> "-wasmjs"
                        WasmWeb-> "-wasmweb"
                        _      | platformHasCompressedFields (platform flags)
                               -> "-" ++ cpuArch ++ "c"
                               | otherwise -> "")                       
                 JS _  -> "-js"
                 _     -> "-" ++ show (target flags)
    in pre ++ "-" ++ show (buildType flags)


buildType :: Flags -> BuildType
buildType flags
  = if optimize flags < 0
      then DebugFull
      else if (optimize flags == 0)
        then Debug
        else if debug flags
               then RelWithDebInfo
               else Release

ccFlagsBuildFromFlags :: CC -> Flags -> Args
ccFlagsBuildFromFlags cc flags
  = case lookup (buildType flags) (ccFlagsBuild cc) of
      Just s -> s
      Nothing -> []

gnuWarn = words "-Wall -Wextra -Wpointer-arith -Wshadow -Wstrict-aliasing" ++
          words "-Wno-unknown-pragmas -Wno-missing-field-initializers" ++
          words "-Wno-unused-parameter -Wno-unused-variable -Wno-unused-value"

ccGcc,ccMsvc :: String -> Int -> Platform -> FilePath -> CC
ccGcc name opt platform path
  = CC name path []
        ([(DebugFull,     ["-g","-O0","-fno-omit-frame-pointer"] ++ arch),
          (Debug,         ["-g","-Og"] ++ arch),
          (RelWithDebInfo,["-O2", "-g", "-DNDEBUG"] ++ arch),
          (Release,       ["-O2", "-DNDEBUG"] ++ arch) ]
        )
        (gnuWarn ++ ["-Wno-unused-but-set-variable"])
        (["-c"]) -- ++ (if onWindows then [] else ["-D_GNU_SOURCE"]))
        []
        (\stksize -> if (onMacOS && stksize > 0)  -- stack size is usually set programmatically (except on macos/windows)
                       then ["-Wl,-stack_size,0x" ++ showHex 0 stksize]
                       else []) 
        (\heapsize -> [])
        (\libdir -> ["-L",libdir])
        (\idir -> ["-I",idir])
        (\fname -> ["-o", (notext fname) ++ objExtension])
        (\out -> ["-o",out])
        (\syslib -> ["-l" ++ syslib])
        (\lib -> [lib])
        (\(def,val) -> ["-D" ++ def ++ (if null val then "" else "=" ++ val)])
        (\lib -> libPrefix ++ lib ++ libExtension)
        (\obj -> obj ++ objExtension)
  where
    archBits= 8 * sizePtr platform
    arch    = -- unfortunately, these flags are not as widely supported as one may hope so we only enable at -O2 or higher
              if (opt < 2) then [] 
              else if (cpuArch=="x64" && archBits==64) then ["-march=haswell","-mtune=native"]      -- popcnt, lzcnt, tzcnt, pdep, pext
              else if (cpuArch=="arm64" && archBits==64) then ["-march=armv8.1-a","-mtune=native"]  -- popcnt, simd, lse
              else []

ccMsvc name opt platform path
  = CC name path ["-DWIN32","-nologo"] 
         [(DebugFull,words "-MDd -Zi -Od -RTC1"),
          (Debug,words "-MDd -Zi -O1"),
          (Release,words "-MD -O2 -Ob2 -DNDEBUG"),
          (RelWithDebInfo,words "-MD -Zi -O2 -Ob2 -DNDEBUG")]
         ["-W3"]
         ["-EHs","-TP","-c"]   -- always compile as C++ on msvc (for atomics etc.)
         ["-link"]             -- , "/NODEFAULTLIB:msvcrt"]
         (\stksize -> if stksize > 0 then ["/STACK:" ++ show stksize] else [])
         (\heapsize -> [])        
         (\libdir -> ["/LIBPATH:" ++ libdir])
         (\idir -> ["-I",idir])
         (\fname -> ["-Fo" ++ ((notext fname) ++ objExtension)])
         (\out -> ["-Fe" ++ out ++ ".exe"])
         (\syslib -> [syslib ++ libExtension])
         (\lib -> [lib])
         (\(def,val) -> ["-D" ++ def ++ (if null val then "" else "=" ++ val)])
         (\lib -> libPrefix ++ lib ++ libExtension)
         (\obj -> obj ++ objExtension)         


ccFromPath :: Flags -> FilePath -> IO (CC,Bool {-asan-})
ccFromPath flags path
  = let name    = -- reverse $ dropWhile (not . isAlpha) $ reverse $
                  basename path
        gcc     = ccGcc name (optimize flags) (platform flags) path        
        mingw   = gcc{ ccName = "mingw", 
                       ccLibFile = \lib -> "lib" ++ lib ++ ".a",
                       ccFlagStack = (\stksize -> if stksize > 0 then ["-Wl,--stack," ++ show stksize] else [])
                     }
        emcc    = gcc{ ccFlagsCompile = ccFlagsCompile gcc ++ ["-D__wasi__"],
                       ccFlagStack = (\stksize -> if stksize == 0 then [] else ["-s","TOTAL_STACK=" ++ show stksize]),
                       ccFlagHeap  = (\hpsize -> if hpsize == 0 then [] else ["-s","TOTAL_MEMORY=" ++ show hpsize]),
                       ccTargetExe = (\out -> ["-o", out ++ targetExeExtension (target flags)]),
                       ccTargetObj = (\fname -> ["-o", (notext fname) ++ targetObjExtension (target flags)]),
                       ccObjFile   = (\fname -> fname ++ targetObjExtension (target flags)),
                       ccLibFile   = (\fname -> targetLibFile (target flags) fname)
                     }
        clang   = gcc{ ccFlagsWarn = gnuWarn
                                     ++ words "-Wno-cast-qual -Wno-undef -Wno-reserved-id-macro -Wno-unused-macros -Wno-cast-align"
                                     ++ (if onMacOS && cpuArch == "arm64" then ["-Wno-unknown-warning-option","-Wno-unused-but-set-variable"] else []) 
                     }
        generic = gcc{ ccFlagsWarn = [] }
        msvc    = ccMsvc name (optimize flags) (platform flags) path
        clangcl = msvc{ ccFlagsWarn = ["-Wno-everything"] ++ ccFlagsWarn clang ++ 
                                      words "-Wno-extra-semi-stmt -Wno-extra-semi -Wno-float-equal",
                        ccFlagsLink = words "-Wno-unused-command-line-argument" ++ ccFlagsLink msvc,
                        ccFlagsCompile = ["-D__clang_msvc__"] ++ ccFlagsCompile msvc
                      }

        cc0     | (name `startsWith` "clang-cl") = clangcl
                | (name `startsWith` "mingw") = mingw
                | (name `startsWith` "emcc") = emcc
                | (name `startsWith` "clang" || name `startsWith` "musl-clang") = clang
                | (name `startsWith` "musl-gcc" || name `startsWith` "musl-g++") = gcc
                | (name `startsWith` "gcc" || name `startsWith` "g++")   = if onWindows then mingw else gcc
                | (name `startsWith` "cl")    = msvc
                | (name `startsWith` "icc")   = gcc
                | (name == "cc") = generic
                | otherwise      = gcc

        cc = cc0{ ccFlagsCompile = ccFlagsCompile cc0 ++ ccompCompileArgs flags
                , ccFlagsLink    = ccFlagsLink cc0 ++ ccompLinkArgs flags }

    in do when (isTargetWasm (target flags) && not (name `startsWith` "emcc")) $
            putStrLn ("\nwarning: a wasm target should use the emscripten compiler (emcc),\n  but currently '" 
                       ++ ccPath cc ++ "' is used." 
                       ++ "\n  hint: specify the emscripten path using --cc=<emcc path>?")   
          if (asan flags)
            then if (not (ccName cc `startsWith` "clang" || ccName cc `startsWith` "gcc" || ccName cc `startsWith` "g++"))
                    then do putStrLn "warning: can only use address sanitizer with clang or gcc (--fasan is ignored)"
                            return (cc,False)
                    -- asan on Apple Silicon can't find leaks and throws an error
                    -- We can't check for arch, since GHC 8.10 runs on Rosetta and detects x86_64
                    else do let sanitize = if onMacOS then "-fsanitize=address,undefined" else "-fsanitize=address,undefined,leak"
                            return (cc{ ccName         = ccName cc ++ "-asan"
                                      , ccFlagsCompile = ccFlagsCompile cc ++ [sanitize,"-fno-omit-frame-pointer","-O0"]
                                      , ccFlagsLink    = ccFlagsLink cc ++ [sanitize] }
                                  ,True)
          else if (useStdAlloc flags)
            then return (cc{ ccName = ccName cc ++ "-stdalloc" }, False)
          else if (mimallocStats flags)
            then return (cc{ ccName = ccName cc ++ "-allocstats" }, False)
            else return (cc,False)

ccCheckExist :: CC -> IO ()
ccCheckExist cc
  = do paths  <- getEnvPaths "PATH"
       mbPath <- searchPaths paths [exeExtension] (ccPath cc)
       case mbPath of
         Just _  -> return ()
         Nothing -> do putStrLn ("\nwarning: cannot find the C compiler: " ++ ccPath cc)
                       when (ccName cc == "cl") $
                         putStrLn ("   hint: run in an x64 Native Tools command prompt? or use the --cc=clang-cl flag?")
                       when (ccName cc == "clang-cl") $
                         putStrLn ("   hint: install clang for Windows from <https://llvm.org/builds/> ?")


quote s
  = "\"" ++ s ++ "\""  

-- unquote a shell argument string (as well as we can)
unquote :: String -> [String]
unquote s
 = filter (not . null) (scan "" s)
 where
   scan acc (c:cs) | c == '\"' || c == '\''     = reverse acc : scanq c "" cs
                   | c == '\\' && not (null cs) = scan (head cs:acc) (tail cs)
                   | isSpace c = reverse acc : scan "" (dropWhile isSpace cs)
                   | otherwise = scan (c:acc) cs
   scan acc []     = [reverse acc]

   scanq q acc (c:cs) | c == q    = reverse acc : scan "" cs
                      | c == '\\' && (not (null cs)) = scanq q (head cs:acc) (tail cs)
                      | otherwise = scanq q (c:acc) cs
   scanq q acc []     = [reverse acc]

onMacOS :: Bool
onMacOS
  = (dllExtension == ".dylib")

onWindows :: Bool
onWindows
  = (exeExtension == ".exe")

tripletOsName, osName :: String
tripletOsName
  = case System.Info.os of
      "linux-android" -> "android"
      "mingw32"       -> "mingw-static"
      "darwin"        -> "osx"
      os              -> os

osName
  = case System.Info.os of
      "mingw32"       -> "windows"
      "darwin"        -> "macos"
      "linux-android" -> "android"
      os              -> os

tripletArch :: String
tripletArch 
  = cpuArch

cpuArch :: String  
cpuArch
  = case System.Info.arch of 
      "aarch64"     -> "arm64"
      "x86_64"      -> "x64"
      "i386"        -> "x86"
      "powerpc"     -> "ppc"
      "powerpc64"   -> "ppc64"
      "powerpc64le" -> "ppc64le"
      arch          -> arch


detectCC :: Target -> IO String
detectCC target
  = do paths <- getEnvPaths "PATH"
       (name,path) <- do envCC <- getEnvVar "CC"
                         findCC paths ((if (isTargetWasm target) then ["emcc"] else []) ++
                                       (if (envCC=="") then [] else [envCC]) ++
                                       (if (onMacOS) then ["clang"] else []) ++
                                       (if (onWindows) then ["clang-cl","cl"] else []) ++
                                       ["gcc","clang","icc","cc","g++","clang++"])
       return path


findCC :: [FilePath] -> [FilePath] -> IO (String,FilePath)
findCC paths []
  = do -- putStrLn "warning: cannot find C compiler -- default to 'gcc'"
       return ("gcc","gcc")
findCC paths (name:rest)
  = do mbPath <- searchPaths paths [exeExtension] name
       case mbPath of
         Nothing   -> findCC paths rest
         Just path -> return (name,path)



detectEditor :: IO String
detectEditor
  = do paths <- getEnvPaths "PATH"
       findEditor paths [("code","--goto %f:%l:%c"),("atom","%f:%l:%c")]
       
findEditor :: [FilePath] -> [(String,String)] -> IO String
findEditor paths []
  = do -- putStrLn "warning: cannot find editor"
       return ""
findEditor paths ((name,options):rest)
  = do mbPath <- searchPaths paths [exeExtension] name
       case mbPath of
         Nothing -> findEditor paths rest
         Just _  -> return (name ++ " " ++ options)

{--------------------------------------------------------------------------
  Show options
--------------------------------------------------------------------------}
invokeError :: [String] -> IO a
invokeError errs
  = raiseIO (concat errs ++ " (" ++ helpMessage ++ ")\n")
  where
    helpMessage = "use \"--help\" for help on command line options"

-- | Show command line help
showHelp :: Printer p => Flags -> p -> IO ()
showHelp flags p
  = do doc <- commandLineHelp flags
       writePrettyLn p doc

-- | Show the morrow environment variables
showEnv :: Printer p => Flags -> p -> IO ()
showEnv flags p
  = do doc <- environmentInfo (colorSchemeFromFlags flags)
       writePrettyLn p (showIncludeInfo flags <-> doc)


commandLineHelp :: Flags -> IO Doc
commandLineHelp flags
  = do envInfo <- environmentInfo colors
       return $
          vcat
        [ infotext "usage:"
        , text "  " <.> text programName <+> text "<options> files"
        , empty
        , infotext "options:" <.> string (usageInfo "" (fst options))
        , infotext "remarks:"
        , text "  Boolean options can be negated, as in: --no-compile"
        , text "  The editor <cmd> can contain %f, %l, and %c to substitute the filename"
        , text "   line number and column number on the ':e' command in the interpreter."
        , text "  The html bases are comma separated <base>=<url> pairs where the base"
        , text "   is a prefix of module names. If using just a <url> it matches any module."
        , showIncludeInfo flags
        , envInfo
        , empty
        ]
  where
    colors
      = colorSchemeFromFlags flags

    infotext s
      = color (colorInterpreter colors) (text s)

showIncludeInfo flags
  = hang 2 (infotext "include path:" <-> prettyIncludePath flags) -- text (if null paths then "<empty>" else paths))
  where
    paths
      = concat $ intersperse [pathDelimiter] (includePath flags)

    colors
      = colorSchemeFromFlags flags

    infotext s
      = color (colorInterpreter colors) (text s)

environmentInfo colors
  = do vals <- mapM getEnvValue environment
       return (hang 2 (infotext "environment:" <->
                       vcat (map ppEnv vals) <-> text " "))
  where
    infotext s
      = color (colorInterpreter colors) (text s)

    ppEnv (name,val)
      = fill n (text name) <.> text "=" <+> val

    n = maximum [length name | (name,_,_,_) <- environment]

    getEnvValue (name,val,_,desc)
      = do s <- getEnvVar name
           if null s
            then return (name,text ("<" ++ val ++ ">"))
            else return (name,text s)


showVersion :: Printer p => Flags -> p -> IO ()
showVersion flags p
  = writePrettyLn p (versionMessage flags)

versionMessage :: Flags -> Doc
versionMessage flags
  =
  (vcat $ map text $
  [ capitalize programName ++ " " ++ version ++ ", " ++ buildTime ++
    (if null (compiler ++ compilerBuildVariant) then "" else " (" ++ compiler ++ " " ++ compilerBuildVariant ++ " version)")
  , ""
  ])
  <-> text "version:" <+> text version
  <-> text "bin    :" <+> text (localBinDir flags)
  <-> text "lib    :" <+> text (localLibDir flags)
  <-> text "share  :" <+> text (localShareDir flags)
  <-> text "output :" <+> text (fullBuildDir flags)
  <-> text "cc     :" <+> text (ccPath (ccomp flags))
  <->
  (color Gray $ vcat $ map text
  [ "Copyright 2012-2021, Microsoft Research, Daan Leijen."
  , "This program is free software; see the source for copying conditions."
  , "This program is distributed in the hope that it will be useful,"
  , "but without any warranty; without even the implied warranty"
  , "of merchantability or fitness for a particular purpose."
  ])
  where
    capitalize ""     = ""
    capitalize (c:cs) = toUpper c : cs

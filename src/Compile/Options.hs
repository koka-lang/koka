-----------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Main module.
-}
-----------------------------------------------------------------------------
module Compile.Options( -- * Command line options
                         getOptions, processOptions, Mode(..), Flags(..), showTypeSigs
                       -- * Show standard messages
                       , showHelp, showEnv, showVersion, commandLineHelp, showIncludeInfo
                       -- * Utilities
                       , prettyEnvFromFlags
                       , colorSchemeFromFlags
                       , prettyIncludePath
                       , isValueFromFlags
                       , processExtraOptions
                       , CC(..), BuildType(..), ccFlagsBuildFromFlags
                       , buildType, unquote
                       , outName, fullBuildDir, buildVariant, buildLibVariant
                       , optionCompletions
                       , targetExeExtension
                       , conanSettingsFromFlags
                       , vcpkgFindRoot
                       , onWindows, onMacOS
                       , flagsHash
                       , phaseVerboseIO
                       , Terminal(..)
                       , parseOptions
                       , flagsNull
                       , targetPlatformFromFlags, targetFromFlags, platformFromFlags
                       ) where

import Debug.Trace
import Data.Char              ( toLower, toUpper, isAlpha, isSpace, isDigit )
import Data.List              ( intersperse, isInfixOf, nub )
import Data.Hashable
import Control.Monad          ( when )
import Control.Concurrent     ( myThreadId )
import qualified System.Info  ( os, arch )
import System.Environment     ( getArgs )
import Platform.FileIO        ( doesFileExist, doesDirectoryExist, getHomeDirectory, getTemporaryDirectory )
import Platform.GetOptions
import Platform.Config
import Lib.PPrint
import Lib.Printer
import Common.Failure         ( raiseIO, catchIO )
import Common.ColorScheme
import Common.File
import Common.Name
import Common.Syntax
import Common.Error( ErrorMessage )
import Type.Type( Scheme )
import Compile.Package
import Core.Core( dataInfoIsValue )
{--------------------------------------------------------------------------
  Convert flags to pretty environment
--------------------------------------------------------------------------}
import qualified Type.Pretty as TP
import System.IO (hPutStrLn, stderr)
import Syntax.Pretty (PrettyEnv)

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
                 , TP.showIds         = showTypeIds flags
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


data Terminal = Terminal{ termError    :: !(ErrorMessage -> IO ())
                        , termTrace    :: !(String -> IO ())
                        , termProgress :: !((Double, Maybe Doc) -> IO ())
                        , termPhase    :: !(Doc -> IO ())
                        , termInfo     :: !(Doc -> IO ())
                        }


phaseVerboseIO :: Terminal -> Flags -> Int -> String -> (TP.Env -> Doc) -> IO ()
phaseVerboseIO term flags verboseLevel phase mkDoc
  = if (verbose flags >= verboseLevel)
      then phaseShowIO term flags verboseLevel phase mkDoc
      else return ()

phaseShowIO :: Terminal -> Flags -> Int -> String -> (TP.Env -> Doc) -> IO ()
phaseShowIO term flags verboseLevel phase mkdoc
  = do tid <- myThreadId
       let penv = prettyEnvFromFlags flags
           cscheme = TP.colors penv
           doc = mkdoc penv
           pre = (if isEmptyDoc doc then phase else (sfill 8 phase ++ ":"))
                   ++ (if verboseLevel >= 4 then " (thread " ++ showThreadId tid ++ ") " else "")
       termPhase term (color (colorInterpreter cscheme) (text pre) <+> (color (colorSource cscheme) doc))
  where
    showThreadId tid = takeWhile isDigit $ dropWhile (not . isDigit) $ show tid
    sfill n s = s ++ replicate (n - length s) ' '


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
  = Flags{ warnShadow       :: !Bool
         , showKinds        :: !Bool
         , showKindSigs     :: !Bool
         , showSynonyms     :: !Bool
         , showCore         :: !Bool
         , showInitialCore  :: !Bool
         , showFinalCore    :: !Bool
         , showCoreTypes    :: !Bool
         , showTypeIds      :: !Bool
         , showAsmCS        :: !Bool
         , showAsmJS        :: !Bool
         , showAsmC         :: !Bool
         , _showTypeSigs     :: !Bool
         , showHiddenTypeSigs     :: !Bool
         , showElapsed      :: !Bool
         , evaluate         :: !Bool
         , execOpts         :: ![String]
         , library          :: !Bool
         , targetPlatform   :: !TargetPlatform
         , stackSize        :: !Int
         , heapSize         :: !Int
         , simplify         :: !Int
         , simplifyMaxDup   :: !Int
         , colorScheme      :: !ColorScheme
         , buildDir         :: !FilePath      -- kkbuild
         , buildTag         :: !String
         , outBuildDir      :: !FilePath      -- actual build output: <builddir>/<version>-<buildtag>/<ccomp>-<variant>
         , outBaseName      :: !String
         , outFinalPath     :: !FilePath
         , includePath      :: ![FilePath]    -- .kk/.kki files
         , csc              :: !FileName
         , node             :: !FileName
         , wasmrun          :: !FileName
         , cmake            :: !FileName
         , cmakeArgs        :: !String
         , ccompPath        :: !FilePath
         , ccompCompileArgs :: !Args
         , ccompIncludeDirs :: ![FilePath]
         , ccompDefs        :: ![(String,String)]
         , ccompLinkArgs    :: !Args
         , ccompLinkSysLibs :: ![String]      -- just core lib name
         , ccompLinkLibs    :: ![FilePath]    -- full path to library
         , ccomp            :: !CC
         , ccompLibDirs     :: ![FilePath]    -- .a/.lib dirs
         , autoInstallLibs  :: !Bool
         , vcpkgRoot        :: !FilePath
         , vcpkgTriplet     :: !String
         {-
         , vcpkg            :: !FilePath
         , vcpkgLibDir      :: !FilePath
         , vcpkgIncludeDir  :: !FilePath
         -}
         , conan            :: !FilePath
         , editor           :: !String
         , redirectOutput   :: !FileName
         , outHtml          :: !Int
         , htmlBases        :: ![(String,String)]
         , htmlCss          :: !String
         , htmlJs           :: !String
         , verbose          :: !Int
         , showSpan         :: !Bool
         , console          :: !String
         , rebuild          :: !Bool
         , genCore          :: !Bool
         , coreCheck        :: !Bool
         , enableMon        :: !Bool
         , semiInsert       :: !Bool
         , genRangeMap      :: !Bool
         , languageServerPort :: !Int
         , languageServerStdio :: !Bool
         , localBinDir      :: !FilePath  -- directory of koka executable
         , localDir         :: !FilePath  -- install prefix: /usr/local
         , localLibDir      :: !FilePath  -- precompiled object files: <prefix>/lib/koka/v2.x.x  /<cc>-<config>/libkklib.a, /<cc>-<config>/std_core.kki, ...
         , localShareDir    :: !FilePath  -- sources: <prefix>/share/koka/v2.x.x  /lib/std, /lib/samples, /kklib
         , packages         :: !Packages
         , forceModule      :: !FilePath
         , debug            :: !Bool      -- emit debug info
         , optimize         :: !Int       -- optimization level; 0 or less is off
         , optInlineMax     :: !Int
         , optctail         :: !Bool
         , optctailCtxPath  :: !Bool
         , optUnroll        :: !Int
         , optEagerPatBind  :: !Bool      -- bind pattern fields as early as possible?
         , parcReuse        :: !Bool
         , parcSpecialize   :: !Bool
         , parcReuseSpec    :: !Bool
         , parcBorrowInference    :: !Bool
         , asan             :: !Bool
         , profile          :: !Bool      -- compile with profiling support (-pg, frame pointers, debug info)
         , useStdAlloc      :: !Bool      -- don't use mimalloc for better asan and valgrind support
         , optSpecialize    :: !Bool
         , mimallocStats    :: !Bool
         , allowInfiniteChains :: !Bool
         , maxConcurrency   :: !Int
         , maxErrors        :: !Int
         , useBuildDirHash  :: !Bool
         , outputEntryName :: !String
         , mainEntryName :: !String
         , baseFlags        :: Maybe Flags
         } deriving (Eq,Show)

instance Hashable Flags where
  hashWithSalt salt flags
    = let h = hashWithSalt salt relevantFlags
      in -- trace ("hash " ++ show salt ++ ", " ++ show h ++ unlines (map ("  " ++) relevantFlags)) $
         h
    where
      relevantFlags = [
          show $ targetPlatform flags,
          -- show $ stackSize flags,
          -- show $ heapSize flags,
          show $ simplify flags,
          show $ simplifyMaxDup flags,
          concat $ nub $ includePath flags,
          csc flags,
          ccompPath flags,
          concat $ ccompCompileArgs flags,
          concat $ ccompIncludeDirs flags,
          concat $ map show $ ccompDefs flags,
          concat $ ccompLinkArgs flags,
          concat $ ccompLinkSysLibs flags,
          concat $ ccompLinkLibs flags,
          show $ ccomp flags,
          concat $ ccompLibDirs flags,
          localBinDir flags,
          localLibDir flags,
          localShareDir flags,
          show $ debug flags,
          show $ optimize flags,
          show $ optInlineMax flags,
          show $ optctail flags,
          show $ optctailCtxPath flags,
          show $ optUnroll flags,
          show $ optEagerPatBind flags,
          show $ parcReuse flags,
          show $ parcSpecialize flags,
          show $ parcReuseSpec flags,
          show $ parcBorrowInference flags,
          show $ asan flags,
          show $ profile flags,
          show $ useStdAlloc flags,
          show $ optSpecialize flags,
          show $ allowInfiniteChains flags
        ]

flagsHash :: Flags -> String
flagsHash flags
  = let s = map toLower (take 6 (showHex 6 (abs (hash flags))))
    in seq (length s) s

flagsNull :: Flags
flagsNull
  = Flags -- warnings
          True
          -- show
          False False  -- kinds kindsigs
          False False False False -- synonyms core icore fcore
          False False -- core-types type-ids
          False -- show asm
          False
          False
          False -- typesigs
          False -- hiddentypesigs
          False -- show elapsed time
          False -- do not execute by default
          []    -- execution options (following --)
          False -- library
          (TargetPlatform (C LibC) "" "" platform64)  -- 64-bit C with libc
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
          (ccGcc "gcc" "gcc" True)
          systemLibDirs
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
          False -- language server stdio
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
          False -- use profile
          False -- use stdalloc
          True  -- use specialization (only used if optimization level >= 1)
          False -- use mimalloc stats
          False -- allow infinite chains
          16    -- max concurrency
          25    -- max errors
          True  -- use variant hash
          ""      -- main entry name (null for default for each target)
          ""      -- main target name (null for default)
          Nothing -- no base flags



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
 , numOption 16 "n" ['j'] ["jobs"]  (\i f -> f{maxConcurrency=max i 1})  "maximum concurrency (16)"
 , option ['i'] ["include"]         (OptArg includePathFlag "dirs") "add <dirs> to module search path (empty resets)"
 , option ['o'] ["output"]          (ReqArg outFinalPathFlag "file")"write executable to <file> (without extension)"
 , numOption 0 "n" ['O'] ["optimize"]   (\i f -> f{optimize=i})     "optimize (0=default,1=space,2=full,3=aggressive)"
 , flag   ['g'] ["debug"]           (\b f -> f{debug=b})            "emit debug information (on by default)"
 , numOption 1 "n" ['v'] ["verbose"] (\i f -> f{verbose=i})         "verbosity 'n' (0=quiet, 1=default, 2=trace)"
 , flag   ['r'] ["rebuild"]         (\b f -> f{rebuild = b})        "rebuild all"
 , flag   ['l'] ["library"]         (\b f -> f{library=b, evaluate=if b then False else (evaluate f) }) "generate a library"
 , configstr [] ["target"]          (map fst targetPlatformIds) "target" targetFlag  ("target: " ++ showL (map fst targetPlatformIds))
 , configstr [] ["target-arch"]     targetArchs "arch" targetArchFlag ("target architecture: " ++ showL targetArchs)
 -- , config []    ["host"]            [("node",Node),("browser",Browser)] "host" (\h f -> f{ target=JS, host=h}) "specify host for javascript: <node|browser>"
 , emptyline

 , option []    ["buildtag"]        (ReqArg buildTagFlag "tag")     "set build variant tag (e.g. 'bundle' or 'dev')"
 , option []    ["builddir"]        (ReqArg buildDirFlag "dir")     ("build under <dir> ('" ++ kkbuild ++ "' by default)")
 , option []    ["buildname"]       (ReqArg outBaseNameFlag "name") "base name of the final output"
 , flag   []    ["buildhash"]       (\b f -> f{useBuildDirHash=b})  "use hash in build directory name"
 , option []    ["outputdir"]       (ReqArg outBuildDirFlag "dir")  "write intermediate files in <dir>, defaults to:\n<builddir>/<ver>-<buildtag>/<cc>-<variant>-<hash>"

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
 , option []    ["output-entry"]    (ReqArg outputEntryFlag "name") "set the name of the generated main entrypoint (e.g. 'main')"
 , option []    ["main-entry"]      (ReqArg mainEntryFlag "name")   "set the name of the main entrypoint function (e.g. 'test1' or 'main')"
 , option []    ["color"]           (ReqArg colorFlag "colors")     "set colors (or a theme as --color=light|dark)"
 , option []    ["redirect"]        (ReqArg redirectFlag "file")    "redirect output to <file>"
 , configstr [] ["console"]  ["ansi","html","raw"] "fmt" (\s f -> f{ console = s }) "console output format: <ansi|html|raw>"
 , numOption (-1) "port" []  ["lsport"] (\i f -> f{languageServerPort=i})  "language server localhost port"
 , flag []      ["lsstdio"]             (\b f -> f{languageServerStdio=b}) "use language Server over stdio"


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
 , flag   []    ["showicore"]      (\b f -> f{showInitialCore=b})   "show initial core (right after type checking)"
 , flag   []    ["showfcore"]      (\b f -> f{showFinalCore=b})     "show final core (with backend optimizations)"
 , flag   []    ["showcoretypes"]  (\b f -> f{showCoreTypes=b})     "show full types in core"
 , flag   []    ["showtypeids"]    (\b f -> f{showTypeIds=b})       "show numeric type variable ids"
 , flag   []    ["showcs"]         (\b f -> f{showAsmCS=b})         "show generated c#"
 , flag   []    ["showjs"]         (\b f -> f{showAsmJS=b})         "show generated javascript"
 , flag   []    ["showc"]          (\b f -> f{showAsmC=b})          "show generated C"
 , flag   []    ["core"]           (\b f -> f{genCore=b})           "generate a core file"
 , flag   []    ["checkcore"]      (\b f -> f{coreCheck=b})         "check generated core"
 , emptyline

 -- hidden
 , hide $ fflag       ["asan"]      (\b f -> f{asan=b})             "compile with address, undefined, and leak sanitizer"
 , hide $ fflag       ["profile"]   (\b f -> f{profile=b})          "compile with profiling support (-pg, frame pointers, debug info)"
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
 , hide $ fflag       ["infchain"]     (\b f -> f{allowInfiniteChains=b}) "allow infinite implicit chains"

 -- deprecated
 , hide $ option []    ["cmake"]           (ReqArg cmakeFlag "cmd")        "use <cmd> to invoke cmake"
 , hide $ option []    ["cmakeopts"]       (ReqArg cmakeArgsFlag "opts")   "pass <opts> to cmake"
 ]
 where
  showL :: [String] -> String
  showL []  = ""
  showL xs  = concatMap (++",") (init xs) ++ (last xs)

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
    = case targetPlatformFromString t of
        Just tgt -> let tpl = targetPlatform f
                    in f{ targetPlatform = tpl{ tplTarget = tplTarget tgt, tplPlatform = tplPlatform tgt } }
        Nothing  -> f

  targetArchFlag t f
    = if t `elem` targetArchs
        then f{ targetPlatform=(targetPlatform f){ tplArch = t } }
        else f

  targetArchs :: [String]
  targetArchs
    = ["x64","arm64","x86","riscv64","riscv32"]

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

  outputEntryFlag s
    = Flag (\f -> f{ outputEntryName = s })

  mainEntryFlag s
    = Flag (\f -> f{ mainEntryName = s })

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
    opt name dir    = ["--" ++ name ++ "=" ++ dir]


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
getOptions :: String -> IO (Flags,Mode)
getOptions extra
  = do env  <- getEnvOptions
       args <- getArgs
       processOptions flagsNull (env ++ words extra ++ args)

processExtraOptions :: Flags -> String -> Either String (Flags,Mode)
processExtraOptions flags0 args
  = let defaultFlags  = case baseFlags flags0 of
                          Just f  -> f
                          Nothing -> flags0
    in case parseOptions defaultFlags (words args) of
        Left err -> Left err
        Right (flags1,mode) -> Right (processDerivedOptions defaultFlags flags1, mode)

platform flags    = tplPlatform (targetPlatform flags)
targetArch flags  = tplArch (targetPlatform flags)
targetOS flags    = tplOS (targetPlatform flags)

target flags      = tplTarget (targetPlatform flags)

processOptions :: Flags -> [String] -> IO (Flags,Mode)
processOptions flags0 opts
  = do (flags1,mode) <- processInitialOptions flags0 opts
       return (processDerivedOptions flags1 flags1,mode)

processDerivedOptions :: Flags -> Flags -> Flags
processDerivedOptions defaultFlags flags
  = let stdAlloc = if asan flags then True else useStdAlloc flags   -- asan implies useStdAlloc
        cdefs    = ccompDefs flags
                    ++ (if stdAlloc then [] else [("KK_MIMALLOC",show (sizePtr (platform flags)))])
                    ++ (if (buildType flags > DebugFull) then [] else [("KK_DEBUG_FULL","")])
                    ++ (if optctailCtxPath flags then [] else [("KK_CTAIL_NO_CONTEXT_PATH","")])
                    ++ (if platformHasCompressedFields (platform flags) then [("KK_INTB_SIZE",show (sizeField (platform flags)))] else [])
                    ++ (if not stdAlloc && mimallocStats flags then [("MI_STAT","2")] else [])

        triplet   = if (not (null (vcpkgTriplet flags))) then vcpkgTriplet flags
                      else if (isTargetWasm (target flags))
                        then ("wasm" ++ show (8*sizePtr (platform flags)) ++ "-emscripten")
                        else targetArch flags ++
                              (if onWindows
                                  then (if (ccName (ccomp flags) `startsWith` "mingw")
                                          then "-mingw-static"
                                          else "-windows-static-md")
                                  else ("-" ++ tripletOsName (targetOS flags)))

    in  flags{  outBaseName = if null (outBaseName flags) && not (null (outFinalPath flags))
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
                useStdAlloc = stdAlloc,
                vcpkgTriplet= triplet,
                ccompDefs   = cdefs,
                baseFlags   = Just defaultFlags
              }

processInitialOptions :: Flags -> [String] -> IO (Flags,Mode)
processInitialOptions flags0 opts
  = case parseOptions flags0 opts of
      Left err -> invokeError [err]
      Right (flags1,mode)
        -> do arch <- if (null (targetArch flags1)) then getTargetArch else return (targetArch flags1)
              let os     = if (null (targetOS flags1)) then hostOsName else targetOS flags1
              let flags2 = flags1{targetPlatform = (targetPlatform flags1){ tplArch = arch, tplOS = os } }
                  flags = case mode of
                            ModeInteractive _    -> flags2{evaluate = True}
                            ModeLanguageServer _ -> flags2{genRangeMap = True}
                            _                    -> flags2
              buildDir <- getKokaBuildDir (buildDir flags) (evaluate flags)
              buildTag <- if (null (buildTag flags)) then getDefaultBuildTag else return (buildTag flags)
              ed   <- if (null (editor flags))
                      then detectEditor
                      else return (editor flags)
              pkgs <- discoverPackages buildDir

              (localDir,localLibDir,localShareDir,localBinDir)
                  <- getKokaDirs (localLibDir flags) (localShareDir flags) buildDir

              normalizedIncludes <- mapM realPath ("." : (localShareDir ++ "/lib") : includePath flags)

              -- cc
              ccmd <- if (ccompPath flags == "") then detectCC (target flags)
                      else if (ccompPath flags == "mingw") then return "gcc"
                      else return (ccompPath flags)
              (cc,asan) <- ccFromPath flags ccmd
              ccCheckExist cc

              let flagsx  = flags{ packages    = pkgs,
                                  buildDir    = buildDir,
                                  buildTag    = buildTag,
                                  localBinDir = localBinDir,
                                  localDir    = localDir,
                                  localLibDir = localLibDir,
                                  localShareDir = localShareDir,
                                  ccompPath   = ccmd,
                                  ccomp       = cc,
                                  asan        = asan,
                                  editor      = ed,
                                  includePath = normalizedIncludes,
                                  genRangeMap = outHtml flags > 0 || genRangeMap flags
                              }
              return (flagsx,mode)

parseOptions :: Flags -> [String] -> Either String (Flags,Mode)
parseOptions flags0 opts
  = let (preOpts,postOpts) = span (/="--") opts
        flags1 = case postOpts of
                   []       -> flags0
                   (_:rest) -> flags0{ execOpts = rest }
        (options,files,errs0) = getOpt Permute optionsAll preOpts
        errs = errs0 ++ extractErrors options
    in if null errs
         then let mode = if (any isHelp options) then ModeHelp
                          else if (any isVersion options) then ModeVersion
                          else if (any isInteractive options) then ModeInteractive files
                          else if (any isLanguageServer options) then ModeLanguageServer files
                          else if (null files) then ModeInteractive files
                                              else ModeCompiler files
              in Right (extractFlags flags1 options,mode)
         else Left (concat errs)


getKokaBuildDir :: FilePath -> Bool -> IO FilePath
getKokaBuildDir "" eval
  = if (eval)
      then do exist <- doesDirectoryExist kkbuild
              if (exist)
                then return kkbuild
                else do -- avoid the tmp directory as it does not always have execute permissions
                        -- tmp <- getTemporaryDirectory
                        -- instead use `$HOME/.koka` if in the interpreter
                        home <- getHomeDirectory
                        return (joinPath home kkbuild)
      else return kkbuild
getKokaBuildDir buildDir _ = return buildDir


getDefaultBuildTag :: IO String
getDefaultBuildTag
  = do wslDistro <- getEnvVar "WSL_DISTRO_NAME"
       if not (null wslDistro)
         then return (map toLower wslDistro)
         else return ""

kkbuild :: String
kkbuild = ".koka"

getKokaDirs :: FilePath -> FilePath -> FilePath -> IO (FilePath,FilePath,FilePath,FilePath)
getKokaDirs libDir1 shareDir1 buildDir0
  = do bin        <- getProgramPath
       let binDir  = dirname bin
           rootDir = rootDirFrom binDir
       -- putStrLn ("rootdir: " ++ rootDir ++ ", bindir: " ++ binDir)
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
                  (_:"build":_:"dist":".stack-work":es)    -> joinPaths (reverse es)
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
              -- trace ("found eroot,root: " ++ show (eroot,root)) $
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
               ccObjFile    :: String -> FilePath,  -- make object file name
               ccFlagsOpt   :: Int -> String -> Args  -- optimize and cpuArch to extra build arguments
            }

instance Show CC where  -- for the hash
  show cc  = "CC{" ++ concat (intersperse "," [
                ccName cc, ccPath cc, show (ccFlags cc), show (ccFlagsBuild cc),
                show (ccFlagsCompile cc), show (ccFlagsLink cc) --, show (ccFlagsWarn cc)
             ]) ++ "}"

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

targetPlatformFromFlags :: Flags -> TargetPlatform
targetPlatformFromFlags flags
  = targetPlatform flags

targetFromFlags :: Flags -> Target
targetFromFlags flags
  = tplTarget (targetPlatform flags)

platformFromFlags :: Flags -> Platform
platformFromFlags flags
  = tplPlatform (targetPlatform flags)

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


buildVariant :: Flags -> String   -- for example: clang-debug-4ead5f
buildVariant flags
  = buildLibVariant flags ++
    (if useBuildDirHash flags then "-" ++ flagsHash flags else "")

buildLibVariant :: Flags -> String   -- for example: clang-debug, js-release
buildLibVariant flags
  = let pre  = case target flags of
                 C ctarget
                   -> ccName (ccomp flags) ++
                      (case ctarget of
                        Wasm   -> "-wasm" ++ show (8*sizePtr (platform flags))
                        WasmJs -> "-wasmjs"
                        WasmWeb-> "-wasmweb"
                        _      | platformHasCompressedFields (platform flags)
                               -> "-" ++ targetArch flags ++ "c"
                               | otherwise -> "")
                 JS _  -> "js"
                 _     -> show (target flags)
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
      Just s -> s ++ (ccFlagsOpt cc) (optimize flags) (targetArch flags)
      Nothing -> []

gnuWarn = words "-Wall -Wextra -Wpointer-arith -Wshadow -Wstrict-aliasing" ++
          words "-Wno-unknown-pragmas -Wno-missing-field-initializers" ++
          words "-Wno-unused-parameter -Wno-unused-variable -Wno-unused-value" ++
          words "-Wno-unused-but-set-variable"

ccGcc :: String -> FilePath -> Bool -> CC
ccGcc name path hasOptG
  = CC name path []
        ([(DebugFull,     ["-g","-O0","-fno-omit-frame-pointer"]),
          (Debug,         ["-g",if hasOptG then "-Og" else "-O1"]),
          (RelWithDebInfo,["-O2", "-g", "-DNDEBUG"]),
          (Release,       ["-O2", "-DNDEBUG"]) ]
        )
        (gnuWarn)
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
        (optArch)
  where
    optArch opt cpuArch
      = -- unfortunately, these flags are not as widely supported as one may hope so we only enable at -O2 or higher
        if (opt < 2) then []
        else if (cpuArch=="x64") then ["-march=haswell","-mtune=native"]    -- Haswell (2013) = x86-64-v3: popcnt, lzcnt, tzcnt, pdep, pext
        else if (cpuArch=="arm64") then ["-march=armv8.1-a+crypto+aes","-mtune=native"]  -- popcnt, simd, lse, pmull (+aes)
        else []

ccMsvc :: String -> FilePath -> CC
ccMsvc name path
  = CC name path ["-DWIN32","-nologo"]
         [(DebugFull,words "-MDd -Zi -FS -Od -RTC1"),
          (Debug,words "-MDd -Zi -FS -O1"),
          (Release,words "-MD -O2 -Ob2 -DNDEBUG"),
          (RelWithDebInfo,words "-MD -Zi -FS -O2 -Ob2 -DNDEBUG")]
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
         (optArch)
  where
    optArch opt cpuArch
      = if (opt < 2) then []
        else if (cpuArch == "x64") then ["-arch:AVX2"] -- popcnt, lzcnt, tzcnt, pdep, pext, clmul
        else if (cpuArch == "arm64") then ["-arch:armv8.1"] -- popcnt, simd, lse
        else []

ccFromPath :: Flags -> FilePath -> IO (CC,Bool {-asan-})
ccFromPath flags path
  = let name    = -- reverse $ dropWhile (not . isAlpha) $ reverse $
                  basename path
        gcc     = ccGcc name path True
        mingw   = gcc{ ccName = "mingw",
                       ccLibFile = \lib -> "lib" ++ lib ++ ".a",
                       ccFlagStack = (\stksize -> if stksize > 0 then ["-Wl,--stack," ++ show stksize] else [])
                     }
        emcc    = (ccGcc name path False)
                     { ccFlagsCompile = ccFlagsCompile gcc ++ ["-D__wasi__"],
                       ccFlagsLink = ccFlagsLink gcc ++ ["-sWASM_BIGINT=1","-sEXPORTED_RUNTIME_METHODS=ccall,cwrap"],
                       ccFlagStack = (\stksize -> if stksize == 0 then [] else ["-s","TOTAL_STACK=" ++ show stksize]),
                       ccFlagHeap  = (\hpsize -> if hpsize == 0 then [] else ["-s","TOTAL_MEMORY=" ++ show hpsize]),
                       ccTargetExe = (\out -> ["-o", out ++ targetExeExtension (target flags)]),
                       ccTargetObj = (\fname -> ["-o", (notext fname) ++ targetObjExtension (target flags)]),
                       ccObjFile   = (\fname -> fname ++ targetObjExtension (target flags)),
                       ccLibFile   = (\fname -> targetLibFile (target flags) fname)
                     }
        clang   = gcc{ ccFlagsWarn = gnuWarn
                                     ++ words "-Wno-cast-qual -Wno-undef -Wno-reserved-id-macro -Wno-unused-macros -Wno-cast-align"
                                     ++ (if onMacOS && targetArch flags == "arm64" then ["-Wno-unknown-warning-option"] else [])
                     }
        generic = gcc{ ccFlagsWarn = [] }
        msvc    = ccMsvc name path
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
            hPutStrLn stderr ("\nwarning: a wasm target should use the emscripten compiler (emcc),\n  but currently '"
                       ++ ccPath cc ++ "' is used."
                       ++ "\n  hint: specify the emscripten path using --cc=<emcc path>?")
          if (asan flags)
            then if (not (ccName cc `startsWith` "clang" || ccName cc `startsWith` "gcc" || ccName cc `startsWith` "g++"))
                    then do hPutStrLn stderr "warning: can only use address sanitizer with clang or gcc (--fasan is ignored)"
                            return (cc,False)
                    -- asan on Apple Silicon can't find leaks and throws an error
                    -- We can't check for arch, since GHC 8.10 runs on Rosetta and detects x86_64
                    else do let sanitize = if onMacOS then "-fsanitize=address,undefined" else "-fsanitize=address,undefined,leak"
                            return (cc{ ccName         = ccName cc ++ "-asan"
                                      , ccFlagsCompile = ccFlagsCompile cc ++ [sanitize,"-fno-omit-frame-pointer","-O0"]
                                      , ccFlagsLink    = ccFlagsLink cc ++ [sanitize] }
                                  ,True)
          else if (profile flags)
            then let isClang = ccName cc `startsWith` "clang"
                 in if isClang
                   then return (cc{ ccName         = ccName cc ++ "-profile"
                                  , ccFlagsCompile = ccFlagsCompile cc ++ ["-fprofile-instr-generate","-fcoverage-mapping","-g","-fno-omit-frame-pointer"]
                                  , ccFlagsLink    = ccFlagsLink cc ++ ["-fprofile-instr-generate"] }
                               ,True)
                   else return (cc{ ccName         = ccName cc ++ "-profile"
                                  , ccFlagsCompile = ccFlagsCompile cc ++ ["-pg","-g3","-fno-omit-frame-pointer"]
                                  , ccFlagsLink    = ccFlagsLink cc ++ ["-pg"] }
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
         Nothing -> do hPutStrLn stderr ("\nwarning: cannot find the C compiler: " ++ ccPath cc)
                       when (ccName cc == "cl") $
                         hPutStrLn stderr ("   hint: run in an x64 Native Tools command prompt? or use the --cc=clang-cl flag?")
                       when (ccName cc == "clang-cl") $
                         hPutStrLn stderr ("   hint: install clang for Windows from <https://github.com/llvm/llvm-project/releases/latest> ?")


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

systemLibDirs :: [String]
systemLibDirs
  = if onWindows then []
    else ["/usr/local/lib","/usr/lib","/lib"]
         ++ if onMacOS then ["/opt/homebrew/lib"]
                       else let triplet = System.Info.arch ++ "-" ++ System.Info.os ++ "-gnu"
                            in ["/usr/lib/" ++ triplet]

-- translate standard platform name to platforms used by vcpkg
tripletOsName :: String -> String
tripletOsName osName
  = case osName of
      "linux-android" -> "android"
      "mingw32"       -> "mingw-static"
      "darwin"        -> "osx"
      "macos"         -> "osx"
      os              -> os

hostOsName
  = case System.Info.os of
      "mingw32"       -> "windows"
      "darwin"        -> "macos"
      "linux-android" -> "android"
      os              -> os

getTargetArch :: IO String
getTargetArch
  = case hostOsName of
      "windows" -> -- on windows on arm, koka is a Haskell x64 exe but targets native arm64
                   do procId <- getEnvVar "PROCESSOR_IDENTIFIER"
                      if (any (\tgt -> procId `startsWith` tgt) ["ARMv8","ARMv9","ARM64","aarch64"])
                        then return "arm64"  -- windows on arm
                        else return hostArch
      _         -> return hostArch

hostArch :: String
hostArch
  = case map toLower System.Info.arch of
      "aarch64"     -> "arm64"
      "armv8"       -> "arm64"
      "armv9"       -> "arm64"
      "x86_64"      -> "x64"
      "x86-64"      -> "x64"
      "amd64"       -> "x64"
      "i386"        -> "x86"
      "powerpc"     -> "ppc"
      "powerpc64"   -> "ppc64"
      "powerpc64le" -> "ppc64le"
      arch          -> arch


detectCC :: Target -> IO String
detectCC target
  = do paths0 <- getEnvPaths "PATH"
       let extra = if (onWindows && not (any (isInfixOf "LLVM") paths0))
                     then ["C:\\Program Files\\LLVM\\bin"] else []       -- find LLVM after initial install when the LLVM path may not be set yet
           paths = extra ++ paths0
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
  = do let xpaths = paths ++
                    if onWindows && name `startsWith` "clang"
                      then ["C:/Program Files/LLVM/bin"]
                      else []
       mbPath <- searchPaths xpaths [exeExtension] name
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
  <-> text "flags  :" <+> pretty (flagsHash flags)
  <->
  (color Gray $ vcat $ map text
  [ "Copyright 2019-2024, Microsoft Research, Daan Leijen, and others."
  , "This program is free software; see the source for copying conditions."
  , "This program is distributed in the hope that it will be useful,"
  , "but without any warranty; without even the implied warranty"
  , "of merchantability or fitness for a particular purpose."
  ])
  where
    capitalize ""     = ""
    capitalize (c:cs) = toUpper c : cs

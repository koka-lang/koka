-----------------------------------------------------------------------------
-- Copyright 2012-2020 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Main module.
-}
-----------------------------------------------------------------------------
module Compiler.Options( -- * Command line options
                         getOptions, processOptions, Mode(..), Flags(..)
                       -- * Show standard messages
                       , showHelp, showEnv, showVersion, commandLineHelp, showIncludeInfo
                       -- * Utilities
                       , prettyEnvFromFlags
                       , colorSchemeFromFlags
                       , prettyIncludePath
                       , isValueFromFlags
                       , CC(..), BuildType(..), ccFlagsBuildFromFlags, buildType, unquote
                       ) where


import Data.Char              ( toUpper, isAlpha, isSpace )
import Data.List              ( intersperse )
import System.Environment     ( getArgs )
import System.Directory ( doesFileExist )
import Platform.GetOptions
import Platform.Config
import Lib.PPrint
import Lib.Printer
import Common.Failure         ( raiseIO )
import Common.ColorScheme
import Common.File
import Common.Syntax          ( Target (..), Host(..), Platform(..), platform32, platform64, platformJS, platformCS )
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
                 , TP.coreInlineMax   = optInlineMax flags
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
  | ModeCompiler    { files :: [FilePath] }
  | ModeInteractive { files :: [FilePath] }

data Option
  = Interactive
  | Version
  | Help
  | Flag (Flags -> Flags)
  | Error String

data Flags
  = Flags{ warnShadow       :: Bool
         , showKinds        :: Bool
         , showKindSigs     :: Bool
         , showSynonyms     :: Bool
         , showCore         :: Bool
         , showCoreTypes    :: Bool
         , showAsmCS        :: Bool
         , showAsmJS        :: Bool
         , showAsmC         :: Bool
         , showTypeSigs     :: Bool
         , showElapsed      :: Bool
         , evaluate         :: Bool
         , execOpts         :: String
         , library          :: Bool
         , target           :: Target
         , host             :: Host
         , platform         :: Platform
         , simplify         :: Int
         , simplifyMaxDup   :: Int
         , colorScheme      :: ColorScheme
         , outDir           :: FilePath
         , outBuildDir      :: FilePath
         , includePath      :: [FilePath]
         , csc              :: FileName
         , node             :: FileName
         , cmake            :: FileName
         , cmakeArgs        :: String
         , ccompPath        :: FilePath
         , ccompCompileArgs :: String
         , ccompLinkArgs    :: String
         , ccompLinkLibs    :: String
         , ccomp            :: CC
         , editor           :: String
         , redirectOutput   :: FileName
         , outHtml          :: Int
         , htmlBases        :: [(String,String)]
         , htmlCss          :: String
         , htmlJs           :: String
         , verbose          :: Int
         , exeName          :: String
         , showSpan         :: Bool
         , console          :: String
         , rebuild          :: Bool
         , genCore          :: Bool
         , coreCheck        :: Bool
         , enableMon        :: Bool
         , semiInsert       :: Bool
         , libDir           :: FilePath
         , binDir           :: FilePath
         , kklibDir         :: FilePath
         , stdlibDir        :: FilePath
         , packages         :: Packages
         , forceModule      :: FilePath
         , debug            :: Bool      -- emit debug info
         , optimize         :: Int       -- optimization level; 0 or less is off
         , optInlineMax     :: Int
         , optctail         :: Bool
         , optctailInline   :: Bool
         , parcReuse        :: Bool
         , parcSpecialize   :: Bool
         , parcReuseSpec    :: Bool
         , asan             :: Bool
         }

flagsNull :: Flags
flagsNull
  = Flags -- warnings
          True
          -- show
          False False  -- kinds kindsigs
          False False False -- synonyms core core-types
          False -- show asm
          False
          False
          False -- typesigs
          False -- show elapsed time
          True  -- executes
          ""    -- execution options
          False -- library
          C     -- target
          Node  -- js host
          platform64
          5     -- simplify passes
          10    -- simplify dup max (must be at least 10 to inline partial applications across binds)
          defaultColorScheme
          ("out/v" ++ version) -- out-dir
          ("")     -- build dir
          []
          "csc"
          "node"
          "cmake"
          ""       -- cmake args
          ""       -- ccompPath
          ""       -- ccomp args
          ""       -- clink args
          ""       -- clink libs
          (ccGcc "gcc" "gcc")
          ""       -- editor
          ""
          0        -- out html
          []
          ("styles/" ++ programName ++ ".css")
          ("")
          1        -- verbosity
          ""
          False
          "ansi"  -- console: ansi, html
          False -- rebuild
          False -- genCore
          False -- coreCheck
          True  -- enableMonadic
          True  -- semi colon insertion
          ""    -- bin dir
          ""    -- lib dir
          ""    -- kklib dir   <lib>/kklib
          ""    -- stdlib dir  <lib>/lib
          packagesEmpty -- packages
          "" -- forceModule
          True -- debug
          0    -- optimize
          10   -- inlineMax
          True -- optctail
          False -- optctailInline
          True -- parc reuse
          True -- parc specialize
          True -- parc reuse specialize
          False -- use asan

isHelp Help = True
isHelp _    = False

isVersion Version = True
isVersion _      = False

isInteractive Interactive = True
isInteractive _ = False

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
 , flag   ['e'] ["execute"]         (\b f -> f{evaluate= b})        "compile and execute (default)"
 , flag   ['c'] ["compile"]         (\b f -> f{evaluate= not b})    "only compile, do not execute"
 , option ['i'] ["include"]         (OptArg includePathFlag "dirs") "add <dirs> to search path (empty resets)"
 , option ['o'] ["outdir"]          (ReqArg outDirFlag "dir")       "output files go to <dir> ('out' by default)"
 , option []    ["outname"]         (ReqArg exeNameFlag "name")     "base name of the final executable"
 , numOption 1 "n" ['v'] ["verbose"] (\i f -> f{verbose=i})         "verbosity 'n' (0=quiet, 1=default, 2=trace)"
 , flag   ['r'] ["rebuild"]         (\b f -> f{rebuild = b})        "rebuild all"
 , flag   ['l'] ["library"]         (\b f -> f{library=b, evaluate=if b then False else (evaluate f) }) "generate a library"
 , numOption 0 "n" ['O'] ["optimize"]   (\i f -> f{optimize=i})     "optimize (0=default, 2=full)"
 , flag   ['D'] ["debug"]           (\b f -> f{debug=b})            "emit debug information (on by default)"

 , emptyline
 , flag   []    ["html"]            (\b f -> f{outHtml = if b then 2 else 0}) "generate documentation"
 , option []    ["htmlbases"]       (ReqArg htmlBasesFlag "bases")            "set link prefixes for documentation"
 , option []    ["htmlcss"]         (ReqArg htmlCssFlag "link")               "set link to the css documentation style"
 , config []    ["target"]          [("js",JS),("cs",CS),("c",C)] targetFlag  "generate C (default), javascript, or C#"
 , config []    ["host"]            [("node",Node),("browser",Browser)] (\h f -> f{ target=JS, host=h}) "specify host for running javascript"
 , config []    ["platform"]        [("x32",platform32),("x64",platform64)] (\p f -> f{platform=p})     "specify target platform (default=64-bit)"
 , emptyline
 , flag   []    ["showelapsed"]    (\b f -> f{ showElapsed = b})    "show elapsed time after evaluation"
 , flag   []    ["showspan"]       (\b f -> f{ showSpan = b})       "show ending row/column too on errors"
 -- , flag   []    ["showkinds"]      (\b f -> f{showKinds=b})        "show full kind annotations"
 , flag   []    ["showkindsigs"]   (\b f -> f{showKindSigs=b})      "show kind signatures of type definitions"
 , flag   []    ["showtypesigs"]   (\b f -> f{showTypeSigs=b})      "show type signatures of definitions"
 , flag   []    ["showsynonyms"]   (\b f -> f{showSynonyms=b})      "show expanded type synonyms in types"
 , flag   []    ["showcore"]       (\b f -> f{showCore=b})          "show core"
 , flag   []    ["showcoretypes"]  (\b f -> f{showCoreTypes=b})     "show full types in core"
 , flag   []    ["showcs"]         (\b f -> f{showAsmCS=b})         "show generated c#"
 , flag   []    ["showjs"]         (\b f -> f{showAsmJS=b})         "show generated javascript"
 , flag   []    ["showc"]          (\b f -> f{showAsmC=b})          "show generated C"
 , flag   []    ["core"]           (\b f -> f{genCore=b})           "generate a core file"
 , flag   []    ["checkcore"]      (\b f -> f{coreCheck=b})         "check generated core"
 -- , flag   []    ["show-coreF"]      (\b f -> f{showCoreF=b})        "show coreF"
 , emptyline
 , option []    ["builddir"]        (ReqArg buildDirFlag "dir")     "build into <dir> (default: <outdir>/<cfg>)"
 , option []    ["editor"]          (ReqArg editorFlag "cmd")       "use <cmd> as editor"
 , option []    ["cmake"]           (ReqArg cmakeFlag "cmd")        "use <cmd> to invoke cmake"
 , option []    ["cmakeargs"]       (ReqArg cmakeArgsFlag "args")   "pass <args> to cmake"
 , option []    ["cc"]              (ReqArg ccFlag "cmd")           "use <cmd> as the C backend compiler "
 , option []    ["ccargs"]          (ReqArg ccCompileArgs "args")   "pass <args> to C backend compiler "
 , option []    ["cclinkargs"]      (ReqArg ccLinkArgs "args")      "pass <args> to C backend linker "
 , option []    ["cclibs"]          (ReqArg ccLinkLibs "libs")      "link with comma separated <libs>"
 , option []    ["csc"]             (ReqArg cscFlag "cmd")          "use <cmd> as the csharp backend compiler "
 , option []    ["node"]            (ReqArg nodeFlag "cmd")         "use <cmd> to execute node"
 , option []    ["color"]           (ReqArg colorFlag "colors")     "set colors"
 , option []    ["redirect"]        (ReqArg redirectFlag "file")    "redirect output to <file>"
 , configstr [] ["console"]      ["ansi","html","raw"] (\s f -> f{console=s})   "console output format"
--  , option []    ["install-dir"]     (ReqArg installDirFlag "dir")       "set the install directory explicitly"

 , hide $ fflag       ["asan"]      (\b f -> f{asan=b})              "compile with address sanitizer (clang only)"
 , hide $ fnum 3 "n"  ["simplify"]  (\i f -> f{simplify=i})          "enable 'n' core simplification passes"
 , hide $ fnum 10 "n" ["maxdup"]    (\i f -> f{simplifyMaxDup=i})    "set 'n' as maximum code duplication threshold"
 , hide $ fnum 10 "n" ["inline"]    (\i f -> f{optInlineMax=i})      "set 'n' as maximum inline threshold (=10)"
 , hide $ fflag       ["monadic"]   (\b f -> f{enableMon=b})         "enable monadic translation"
 , hide $ flag []     ["semi"]      (\b f -> f{semiInsert=b})        "insert semicolons based on layout"
 , hide $ fflag       ["parcreuse"] (\b f -> f{parcReuse=b})         "enable in-place update analysis"
 , hide $ fflag       ["parcspec"]  (\b f -> f{parcSpecialize=b})    "enable drop specialization"
 , hide $ fflag       ["parcrspec"] (\b f -> f{parcReuseSpec=b})     "enable reuse specialization"
 , hide $ fflag       ["optctail"]  (\b f -> f{optctail=b})          "enable con-tail optimization (TRMC)"
 , hide $ fflag       ["optctailinline"]  (\b f -> f{optctailInline=b})  "enable con-tail inlining (increases code size)"
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

  config short long opts f desc
    = option short long (ReqArg validate valid) desc
    where
      valid = "(" ++ concat (intersperse "|" (map fst opts)) ++ ")"
      validate s
        = case lookup s opts of
            Just x -> Flag (\flags -> f x flags)
            Nothing -> Error ("invalid value for --" ++ head long ++ " option, expecting any of " ++ valid)

  configstr short long opts f desc
    = config short long (map (\s -> (s,s)) opts) f desc

  targetFlag t f
    = f{ target=t, platform=case t of
                              JS -> platformJS
                              CS -> platformCS
                              _  -> platform64  }

  colorFlag s
    = Flag (\f -> f{ colorScheme = readColorFlags s (colorScheme f) })

  consoleFlag s
    = Flag (\f -> f{ console = s })

  htmlBasesFlag s
    = Flag (\f -> f{ htmlBases = (htmlBases f) ++ readHtmlBases s })

  htmlCssFlag s
    = Flag (\f -> f{ htmlCss = s })

  includePathFlag mbs
    = Flag (\f -> f{ includePath = case mbs of
                                     Just s | not (null s) -> includePath f ++ undelimPaths s
                                     _ -> [] })

  outDirFlag s
    = Flag (\f -> f{ outDir = s })

  buildDirFlag s
    = Flag (\f -> f{ outBuildDir = s })

  exeNameFlag s
    = Flag (\f -> f{ exeName = s })

  ccFlag s
    = Flag (\f -> f{ ccompPath = s })

  ccCompileArgs s
    = Flag (\f -> f{ ccompCompileArgs = s })
  ccLinkArgs s
    = Flag (\f -> f{ ccompLinkArgs = s })
  ccLinkLibs s
    = Flag (\f -> f{ ccompLinkLibs = s })

  cscFlag s
    = Flag (\f -> f{ csc = s })

  nodeFlag s
    = Flag (\f -> f{ node = s })

  editorFlag s
    = Flag (\f -> f{ editor = s })

  redirectFlag s
    = Flag (\f -> f{ redirectOutput = s })

  cmakeFlag s
      = Flag (\f -> f{ cmake = s })

  cmakeArgsFlag s
      = Flag (\f -> f{ cmakeArgs = s })

{-
  installDirFlag s
    = Flag (\f -> f{ installDir = s, includePath = (includePath f) ++ [libd] })
    where
      libd = joinPath s "lib"
-}

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
  = [ -- ("koka-dir",     "dir",     dirEnv,       "The install directory")
      ("koka-options", "options", flagsEnv,     "Add <options> to the command line")
    , ("koka-editor",  "command", editorEnv,    "Use <cmd> as the editor (substitutes %l, %c, and %s)")
    ]
  where
    flagsEnv s      = [s]
    editorEnv s     = ["--editor=" ++ s]
    -- dirEnv s        = ["--install-dir=" ++ s]

{--------------------------------------------------------------------------
  Process options
--------------------------------------------------------------------------}
getOptions :: String -> IO (Flags,Mode)
getOptions extra
  = do env  <- getEnvOptions
       args <- getArgs
       processOptions flagsNull (env ++ words extra ++ args)

processOptions :: Flags -> [String] -> IO (Flags,Mode)
processOptions flags0 opts
  = let (preOpts,postOpts) = span (/="--") opts
        flags1 = case postOpts of
                   [] -> flags0
                   (_:rest) -> flags0{ execOpts = concat (map (++" ") rest) }
        (options,files,errs0) = getOpt Permute optionsAll preOpts
        errs = errs0 ++ extractErrors options
    in if (null errs)
        then let flags = extractFlags flags1 options
                 mode = if (any isHelp options) then ModeHelp
                        else if (any isVersion options) then ModeVersion
                        else if (any isInteractive options) then ModeInteractive files
                        else if (null files) then ModeInteractive files
                                             else ModeCompiler files
             in do pkgs <- discoverPackages (outDir flags)
                   (binDir,libDir,kklibDir,stdlibDir) <- getKokaDirs
                   ccmd <- if (ccompPath flags == "") then detectCC else return (ccompPath flags)
                   cc   <- ccFromPath flags ccmd
                   return (flags{ packages    = pkgs,
                                  binDir      = binDir,
                                  libDir      = libDir,
                                  kklibDir    = kklibDir,
                                  stdlibDir   = stdlibDir,
                                  ccompPath      = ccmd,
                                  ccomp       = cc,
                                  includePath = stdlibDir : includePath flags }
                          ,mode)
        else invokeError errs

getKokaDirs :: IO (FilePath,FilePath,FilePath, FilePath)
getKokaDirs
  = do bin        <- getProgramPath
       let binDir  = dirname bin
           rootDir = rootDirFrom binDir
       libDir0    <- getEnvVar "KOKA_LIB_DIR"
       libDir     <- if (not (null libDir0)) then return libDir0 else
                     do exist <- doesFileExist (joinPath rootDir "lib/toc.kk")
                        if (exist)
                          then return rootDir -- from local repo
                          else return (joinPath rootDir ("lib/koka/v" ++ version))  -- from install
       kklibDir0  <- getEnvVar "KOKA_KKLIB_DIR"
       let kklibDir = if (null kklibDir0) then joinPath libDir "kklib" else kklibDir0
       stdlibDir0 <- getEnvVar "KOKA_STDLIB_DIR"
       let stdlibDir = if (null stdlibDir0) then joinPath libDir "lib" else stdlibDir0
       return (normalizeWith '/' binDir,
               normalizeWith '/' libDir,
               normalizeWith '/' kklibDir,
               normalizeWith '/' stdlibDir)

rootDirFrom :: FilePath -> FilePath
rootDirFrom binDir
 = case reverse (splitPath binDir) of
     -- stack build
     ("bin":_:"install":".stack-work":es)     -> joinPaths (reverse es)
     ("bin":_:_:"install":".stack-work":es)   -> joinPaths (reverse es)
     ("bin":_:_:_:"install":".stack-work":es) -> joinPaths (reverse es)
     -- install
     ("bin":es)   -> joinPaths (reverse es)
     -- jake build
     (_:"out":es) -> joinPaths (reverse es)
     _            -> binDir


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
                                         ,joinPath froot "v2.0.50727"
                                         ,joinPath froot "v1.1.4322"]
                                         [exeExtension] "csc"
                    case mbcsc of
                      Nothing  -> return []
                      Just csc -> return ["--csc=" ++ csc ]
            else return ["--csc="++ joinPaths [fw,fv,"csc"]]


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
               ccIncludeDir :: FilePath -> Args,
               ccTargetObj  :: FilePath -> Args,
               ccTargetExe  :: FilePath -> Args,
               ccAddSysLib  :: String -> Args,
               ccAddLib     :: FilePath -> Args,
               ccAddDef     :: String -> Args,
               ccLibFile    :: String -> FilePath,
               ccObjFile    :: String -> FilePath
            }

data BuildType = Debug | Release | RelWithDebInfo
               deriving (Eq)

instance Show BuildType where
  show Debug          = "debug"
  show Release        = "release"
  show RelWithDebInfo = "drelease"

buildType :: Flags -> BuildType
buildType flags
  = if optimize flags <= 0
      then Debug
      else if debug flags
             then RelWithDebInfo
             else Release

ccFlagsBuildFromFlags :: CC -> Flags -> Args
ccFlagsBuildFromFlags cc flags
  = case lookup (buildType flags) (ccFlagsBuild cc) of
      Just s -> s
      Nothing -> []

gnuWarn = words "-Wall -Wextra -Wno-unknown-pragmas -Wno-unused-parameter -Wno-unused-variable -Wno-unused-value" ++
          words "-Wno-missing-field-initializers -Wpointer-arith -Wshadow -Wstrict-aliasing"

ccGcc,ccMsvc :: String -> FilePath -> CC
ccGcc name path
  = CC name path []
        [(Debug,         words "-g -O1"),
         (Release,       words "-O2 -flto -DNDEBUG"),
         (RelWithDebInfo,words "-O2 -flto -g -DNDEBUG")]
        (gnuWarn ++ ["-Wno-unused-but-set-variable"])
        (["-c"] ++ (if onWindows then [] else ["-D_GNU_SOURCE"]))
        []
        (\idir -> ["-I",idir])
        (\fname -> ["-o", (notext fname) ++ objExtension])
        (\out -> ["-o",out])
        (\syslib -> ["-l" ++ syslib])
        (\lib -> [lib])
        (\def -> ["-D" ++ def])
        (\lib -> libPrefix ++ lib ++ libExtension)
        (\obj -> obj ++ objExtension)

ccMsvc name path
  = CC name path ["-DWIN32","-nologo"]
         [(Debug,words "-MDd -Zi -Ob0 -Od -RTC1"),
          (Release,words "-MD -O2 -Ob2 -DNDEBUG"),
          (RelWithDebInfo,words "-MD -Zi -O2 -Ob1 -DNDEBUG")]
         ["-W3"]
         ["-TC","-c"]
         []
         (\idir -> ["-I",idir])
         (\fname -> ["-Fo" ++ ((notext fname) ++ objExtension)])
         (\out -> ["-Fe" ++  out ++ exeExtension])
         (\syslib -> [syslib ++ libExtension])
         (\lib -> [lib])
         (\def -> ["-D" ++ def])
         (\lib -> libPrefix ++ lib ++ libExtension)
         (\obj -> obj ++ objExtension)


ccFromPath :: Flags -> FilePath -> IO CC
ccFromPath flags path
  = let name    = -- reverse $ dropWhile (not . isAlpha) $ reverse $
                  basename path
        gcc     = ccGcc name path
        mingw   = gcc{ ccName = "mingw", ccLibFile = \lib -> "lib" ++ lib ++ ".a" }
        clang   = gcc{ ccFlagsWarn = gnuWarn ++ words "-Wno-cast-qual -Wno-undef -Wno-reserved-id-macro -Wno-unused-macros -Wno-cast-align" }
        generic = gcc{ ccFlagsWarn = [] }
        msvc    = ccMsvc name path
        clangcl = msvc{ ccFlagsWarn = ccFlagsWarn clang ++ words "-Wno-extra-semi-stmt -Wno-extra-semi -Wno-strict-prototypes -Wno-sign-conversion",
                        ccFlagsLink = ccFlagsLink clang ++ words "-Wno-unused-command-line-argument"
                      }

        cc0     | (name `startsWith` "clang-cl") = clangcl
                | (name `startsWith` "mingw") = mingw
                | (name `startsWith` "clang") = clang
                | (name `startsWith` "gcc")   = if onWindows then mingw
                                                else gcc
                | (name `startsWith` "cl")    = msvc
                | (name `startsWith` "icc")   = gcc
                | (name == "cc") = generic
                | otherwise      = gcc

        cc = cc0{ ccFlagsCompile = ccFlagsCompile cc0 ++ unquote (ccompCompileArgs flags)
                , ccFlagsLink    = ccFlagsLink cc0 ++ unquote (ccompLinkArgs flags) }

    in if (asan flags)
         then if (not (ccName cc `startsWith` "clang"))
                then do putStrLn "warning: can only use address sanitizer with clang (ignored)"
                        return cc
                else do return cc{ ccName         = ccName cc ++ "-asan"
                                 , ccFlagsCompile = ccFlagsCompile cc ++ ["-fsanitize=address"]
                                 , ccFlagsLink    = ccFlagsLink cc ++ ["-fsanitize=address"] }
         else return cc

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

detectCC :: IO String
detectCC
  = do paths <- getEnvPaths "PATH"
       (name,path) <- do envCC <- getEnvVar "CC"
                         findCC paths ((if (envCC=="") then [] else [envCC]) ++
                                       (if (onMacOS) then ["clang"] else []) ++
                                       ["gcc","cl","clang-cl","clang","icc","cc","g++","clang++"])
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
    (if null (compiler ++ buildVariant) then "" else " (" ++ compiler ++ " " ++ buildVariant ++ " version)")
  , ""
  ])
  <-> text "bin   :" <+> text (binDir flags)
  <-> text "lib   :" <+> text (libDir flags)
  <-> text "stdlib:" <+> text (stdlibDir flags)
  <-> text "kklib :" <+> text (kklibDir flags)
  <-> text "cc    :" <+> text (ccPath (ccomp flags))
  <->
  (color Gray $ vcat $ map text
  [ "Copyright (c) 2012-2020 Microsoft Corporation, by Daan Leijen."
  , "This program is free software; see the source for copying conditions."
  , "This program is distributed in the hope that it will be useful,"
  , "but without any warranty; without even the implied warranty"
  , "of merchantability or fitness for a particular purpose."
  ])
  where
    capitalize ""     = ""
    capitalize (c:cs) = toUpper c : cs

-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
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
                       ) where


import Data.Char              ( toUpper )
import Data.List              ( intersperse )
import System.Environment     ( getArgs )
import Platform.GetOptions
import Platform.Config        ( pathDelimiter, version, compiler, buildTime, buildVariant, exeExtension, programName )
import Lib.PPrint
import Lib.Printer
import Common.Failure         ( raiseIO )
import Common.ColorScheme
import Common.File
import Common.Syntax          ( Target (..), Host(..) )
import Compiler.Package

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
                 , TP.showCoreTypes   = showCoreTypes flags
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
         , showAsmCSharp    :: Bool
         , showAsmJavaScript :: Bool
         , showTypeSigs     :: Bool
         , evaluate         :: Bool
         , library          :: Bool
         , targets          :: [Target]
         , host             :: Host
         , simplify         :: Int
         , colorScheme      :: ColorScheme
         , outDir           :: FilePath
         , includePath      :: [FilePath]
         , csc              :: FileName
         , node             :: FileName
         , editor           :: String
         , redirectOutput   :: FileName
         , maxStructFields  :: Int
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
         -- , installDir       :: FilePath
         , semiInsert       :: Bool
         , packages         :: Packages
         , forceModule      :: FilePath
         , optimize         :: Int       -- optimization level; negative is off
         , debug            :: Bool
         }

flagsNull :: Flags
flagsNull
  = Flags -- warnings
          True
          -- show
          False False  -- kinds kindsigs
          False False False -- synonyms core core-types
          False -- show asmCSharp
          False -- show asmJavaScript
          False -- typesigs
          True -- executes
          False -- library
          [JS]
          Node
          5     -- simplify passes
          defaultColorScheme
          "out"    -- out-dir
          []
          "csc"
          "node"
          ""
          ""
          3
          0
          []
          ("styles/" ++ programName ++ ".css")
          ("")
          0
          ""
          False
          "ansi"  -- console: ansi, html
          False -- rebuild
          False -- genCore
          False -- coreCheck
          True  -- enableMonadic
          -- ""  -- install dir
          True  -- semi colon insertion
          packagesEmpty -- packages
          "" -- forceModule
          (-1) -- optimize
          True -- debug

isHelp Help = True
isHelp _    = False

isVersion Version = True
isVersion _      = False

isInteractive Interactive = True
isInteractive _ = False

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
 , option ['o'] ["outdir"]          (ReqArg outDirFlag "dir")       "put generated files in <dir> ('out' by default)"
 , option []    ["outname"]         (ReqArg exeNameFlag "name")     "name of the final executable"
 , flag   ['v'] ["verbose"]         (\b f -> f{verbose=if b then (verbose f)+1 else 0}) "run more verbose"
 , flag   ['r'] ["rebuild"]         (\b f -> f{rebuild = b})        "rebuild all"
 , flag   ['l'] ["library"]         (\b f -> f{library=b, evaluate=if b then False else (evaluate f) }) "generate a library"
 , flag   ['O'] ["optimize"]        (\b f -> f{optimize=if b then 1 else -1}) "optimize (off by default)"
 , flag   ['D'] ["debug"]           (\b f -> f{debug=b})            "emit debug information (on by default)"

 , emptyline
 , flag   []    ["html"]            (\b f -> f{outHtml = if b then 2 else 0}) "generate documentation"
 , option []    ["htmlbases"]       (ReqArg htmlBasesFlag "bases")  "set link prefixes for documentation"
 , option []    ["htmlcss"]         (ReqArg htmlCssFlag "link")     "set link to the css documentation style"
 , config []    ["target"]          [("js",[JS]),("cs",[CS])] (\t f -> f{targets=t}) "generate csharp or javascript (default)"
 , config []    ["host"]            [("node",Node),("browser",Browser)] (\h f -> f{ targets=[JS], host=h}) "specify host for running code"

 , emptyline
 , flag   []    ["showspan"]       (\b f -> f{ showSpan = b})      "show ending row/column too on errors"
 -- , flag   []    ["showkinds"]      (\b f -> f{showKinds=b})        "show full kind annotations"
 , flag   []    ["showkindsigs"]   (\b f -> f{showKindSigs=b})     "show kind signatures of type definitions"
 , flag   []    ["showtypesigs"]   (\b f -> f{showTypeSigs=b})     "show type signatures of definitions"
 , flag   []    ["showsynonyms"]   (\b f -> f{showSynonyms=b})     "show expanded type synonyms in types"
 , flag   []    ["showcore"]       (\b f -> f{showCore=b})         "show core"
 , flag   []    ["showcoretypes"]       (\b f -> f{showCoreTypes=b})         "show full types in core"
 , flag   []    ["showcs"]         (\b f -> f{showAsmCSharp=b})    "show generated c#"
 , flag   []    ["showjs"]         (\b f -> f{showAsmJavaScript=b}) "show generated javascript"
 , flag   []    ["core"]            (\b f -> f{genCore=b})           "generate a core file"
 , flag   []    ["checkcore"]      (\b f -> f{coreCheck=b})         "check generated core"
 -- , flag   []    ["show-coreF"]      (\b f -> f{showCoreF=b})        "show coreF"
 , emptyline
 , option []    ["editor"]          (ReqArg editorFlag "cmd")       "use <cmd> as editor"
 , option []    ["csc"]             (ReqArg cscFlag "cmd")          "use <cmd> as the csharp backend compiler"
 , option []    ["node"]            (ReqArg nodeFlag "cmd")         "use <cmd> to execute node"
 , option []    ["color"]           (ReqArg colorFlag "colors")     "set colors"
 , option []    ["redirect"]        (ReqArg redirectFlag "file")    "redirect output to <file>"
 , configstr [] ["console"]      ["ansi","html","raw"] (\s f -> f{console=s})   "console output format"
--  , option []    ["install-dir"]     (ReqArg installDirFlag "dir")       "set the install directory explicitly"

 , hiddenNumOption 3 "n" [] ["simplify"]  (\i f -> f{simplify=i})    "enable 'n' core simplification passes"
 , hiddenFlag   []    ["mon"]       (\b f -> f{enableMon=b})          "enable monadic translation"
 , hiddenFlag   []    ["structs"]   (\b f -> f{maxStructFields= if b then 3 else 0})  "pass constructors on stack"
 , hiddenFlag []      ["semi"]      (\b f -> f{semiInsert=b})     "insert semicolons based on layout"
 ]
 where
  emptyline
    = flag   []    [] (\b f -> f) ""

  option short long f desc
    = ([Option short long f desc],[])

  hiddenOption short long f desc
    = ([],[Option short long f desc])

  flag short long f desc
    = ([Option short long (NoArg (Flag (f True))) desc]
      ,[Option [] (map ("no-" ++) long) (NoArg (Flag (f False))) ""])

  hiddenFlag short long f desc
    = ([],[Option short long (NoArg (Flag (f True))) desc
          ,Option [] (map ("no-" ++) long) (NoArg (Flag (f False))) ""])

  hiddenNumOption def optarg short long f desc
    = ([],[Option short long (OptArg (\mbs -> Flag (numOption def f mbs)) optarg) desc
          ,Option [] (map ("no-" ++) long) (NoArg (Flag (f (-1)))) ""])

  numOption def f mbs
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

  exeNameFlag s
    = Flag (\f -> f{ exeName = s })

  cscFlag s
    = Flag (\f -> f{ csc = s })

  nodeFlag s
    = Flag (\f -> f{ node = s })

  editorFlag s
    = Flag (\f -> f{ editor = s })

  redirectFlag s
    = Flag (\f -> f{ redirectOutput = s })
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
  = let (options,files,errs0) = getOpt Permute optionsAll opts
        errs = errs0 ++ extractErrors options
    in if (null errs)
        then let flags = extractFlags flags0 options
                 mode = if (any isHelp options) then ModeHelp
                        else if (any isVersion options) then ModeVersion
                        else if (any isInteractive options) then ModeInteractive files
                        else if (null files) then ModeInteractive files
                                             else ModeCompiler files
             in do pkgs <- discoverPackages (outDir flags)
                   installDir <- getInstallDir
                   return (flags{ packages = pkgs,
                                  includePath = joinPath installDir "lib":includePath flags },mode)
        else invokeError errs

extractFlags :: Flags -> [Option] -> Flags
extractFlags flagsInit options
  = let flags = foldl extract flagsInit options
    in case (JS `elem` targets flags) of  -- the maxStructFields prevents us from generating CS and JS at the same time...
         True -> flags{ maxStructFields = -1 }
         _    -> case (CS `elem` targets flags) of
                   True | maxStructFields flags < 0 -> flags{ maxStructFields = 3 }
                   _    -> flags
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
       idir<- getInstallDir
       xss <- mapM getEnvOption environment
       return (concat ({- ["--install-dir=" ++ idir]:-} csc:xss))
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


showVersion :: Printer p => p -> IO ()
showVersion p
  = writePrettyLn p versionMessage

versionMessage :: Doc
versionMessage
  =
  (vcat $ map text $
  [ capitalize programName ++ " " ++ version ++ ", " ++ buildTime ++
    (if null (compiler ++ buildVariant) then "" else " (" ++ compiler ++ " " ++ buildVariant ++ " version)")
  , ""
  ])
  <->
  (color DarkGray $ vcat $ map text
  [ "Copyright (c) 2012-2016 Microsoft Corporation, by Daan Leijen."
  , "This program is free software; see the source for copying conditions."
  , "This program is distributed in the hope that it will be useful,"
  , "but without any warranty; without even the implied warranty"
  , "of merchantability or fitness for a particular purpose."
  ])
  where
    capitalize ""     = ""
    capitalize (c:cs) = toUpper c : cs

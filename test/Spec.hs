import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Char
import Numeric (showFFloat)
import Data.Array ((!))
import Data.IORef
import Data.List
import Data.List.Extra (replace, trim)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import qualified System.Info as Sys
import System.Process (readProcessWithExitCode)
import Text.Regex.TDFA
import Text.JSON
import Test.Hspec
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Formatters.V2 hiding (Error, Result)
import qualified Test.Hspec.Core.Formatters.V2 as F
import qualified NameSpec(spec)

commonFlags :: [String]
commonFlags = ["-c", "-v0", "--console=raw",
               "-O1", -- for cgen/specialize
               -- "--cc=clang",
               -- "--checkcore",
               "-itest",  -- "-ilib",
               "--buildtag=test", "--no-buildhash"]

data Mode = Test | New | Update
  deriving (Eq, Ord, Show)


data Options = Options{ mode :: Mode, cabal :: Bool, sysghc:: Bool, opt :: Int, target :: String, par :: Bool, rebuild :: Bool,
                        quick :: Bool, shard :: (Int,Int) }

optionsDefault = Options Test False False 0 "" True False False (1,1)

data Cfg = Cfg{ flags   :: [String],
                options :: Options,
                fexclude:: !(String -> String -> Bool),  -- target and file name
                fslow   :: !(String -> Bool)   -- skipped with --quick
              }

makeCfg :: [String] -> Options -> [String] -> [(String,[String])] -> [String] -> Cfg
makeCfg flags options exclude texclude slow
  = Cfg flags options (\t s -> fex s || any (\(k,m) -> k == t && m s) ftex) (matchAny slow)
  where
    fex  = matchAny exclude
    ftex = [(k, matchAny xs) | (k,xs) <- texclude]

matchAny :: [String] -> String -> Bool
matchAny []    = const False
matchAny items = \s -> any (\m -> m s) matchers
  where
    matchers = map matcher items
    matcher item | any (\c -> c `elem` "()[]+*?") item = let r = makeRegex item :: Regex
                                                         in (\s -> matchTest r s)
                 | otherwise = (\s -> s == item || s == (item ++ ".kk"))

normalizePlatformKey :: String -> String
normalizePlatformKey = map norm
  where
    norm c | c == '_' = '-'
           | otherwise = toLower c

instance JSON Cfg where
  showJSON cfg = JSObject (toJSObject [])
  readJSON val
    = case val of
        JSObject obj
          -> let flags = case valFromObj "flags" obj of
                           Ok s -> words s
                           _    -> []
                 exclude = case valFromObj "exclude" obj of
                             Ok xs -> xs
                             _     -> []
                 slow = case valFromObj "slow" obj of
                          Ok xs -> xs
                          _     -> []
                 pexclude = concat [xs | (platform,xs) <- keyedLists "exclude-platform", platform `elem` platformKeys]
                 texclude = keyedLists "exclude-target"
                 keyedLists key
                   = case valFromObj key obj of
                       Ok (JSObject kobj) -> [(normalizePlatformKey k, mapMaybe toString xs) | (k, JSArray xs) <- fromJSObject kobj]
                       _                  -> []
                 toString (JSString s) = Just (fromJSString s)
                 toString _            = Nothing
                 platformKeys = [os, arch, os ++ "-" ++ arch]
                 -- see also src/Compile/Option.hs
                 os   = case normalizePlatformKey Sys.os of
                          "darwin"  -> "macos"
                          "mingw32" -> "windows"
                          o         -> o
                 arch = case normalizePlatformKey Sys.arch of
                          "aarch64" -> "arm64"
                          "armv8"   -> "arm64"
                          "armv9"   -> "arm64"
                          "x86-64"  -> "x64"
                          "x86_64"  -> "x64"
                          "amd64"   -> "x64"
                          a          -> a
               in Ok (makeCfg flags optionsDefault (exclude ++ pexclude) texclude slow)
        JSNull     -> Ok (makeCfg [] optionsDefault [] [] [])
        JSString s -> Ok (makeCfg (words (fromJSString s)) optionsDefault [] [] [])
        _          -> Error ("invalid JSON object")

extendCfg :: Cfg -> Cfg -> Cfg
extendCfg (Cfg flags1 opts1 fexclude1 fslow1) (Cfg flags2 opts2 fexclude2 fslow2)
  = Cfg (flags1 ++ flags2) opts1
        (\t s -> fexclude1 t s || fexclude2 t s) (\s -> fslow1 s || fslow2 s)

initialCfg :: Options -> Cfg
initialCfg options
  = makeCfg (commonFlags ++ if (not (null (target options))) then ["--target=" ++ target options] else [])
            options [] [] []


readFlagsFile :: FilePath -> IO [String]
readFlagsFile fp
  = do exists <- doesFileExist fp
       if not exists
         then return []
         else words <$> readFile fp

testSanitize :: FilePath -> String -> String
testSanitize kokaDir
  = trim
  . sub "^compile *:.*kklib.*\n" ""
  . sub "^Up to date\n" ""
  . sub "\n[[:space:]]+at .*" ""
  . sub "(std_core\\.js:)[[:digit:]]+" "\\1"
  . sub "[\r\n]+" "\n"
  . sub "[[:blank:]]+" " "
  . sub "\\\\" "/"
   -- type variable names and box names
  . sub "\\@box-x[[:digit:]]+(-x[[:digit:]]+)?" "@box-xxx"
  . sub "(\\@[a-zA-Z])[[:digit:]]+" "\\1xxx"
  . sub "([a-zA-Z])@[[:digit:]@]+" "\\1@xxx"
  . sub "([a-zA-Z])\\.[[:digit:]]+\\.[[:digit:]]+" "\\1@xxx"
  . sub "<[[:digit:]]+>" "<0>"
  -- for tests using --showhiddentypesigs,
  -- @unroll-xxx
  -- e.g. .lift250-main => .lift000-main
  --      f: (z00.192 : a) -> a => f: (a.00.000 : a) -> a
  . sub "([[:alpha:]][[:alpha:]]+)[[:digit:]][[:digit:]][[:digit:]]+" "\\1xxx"  -- xs254 ~> xsxxx
  . sub "@unroll-[[:alnum:]@-]+" "@unroll-xxx"                   -- @unroll-main12 ~> @unroll-xxx
  . sub "(@m?)lift-[[:alnum:]@-]+" "\\1lift-xxx"                 -- @mlift-main33  ~> @mlift-xxx
  . sub "(^[[:alnum:]]+\\/.+:.*) [[:alpha:]]+[[:digit:]]+@[[:digit:]]+ :" "\\1 a00.000 :"
  -- . sub ": [[:digit:]]+([,\\)])" ": 0\\1"
  -- old style handlers
  . sub "@hnd-" ""
  -- using @ for hidden names now
  . sub "\\." "@"
  -- standard qualifiers
  . sub "(console|hnd|sslice|string|list)/" ""
 . if null kokaDir then id else replace xkokaDir "..."
  where
    xkokaDir = case map (\c -> if c == '\\' then '/' else c) kokaDir of
                 (c:':':rest) -> [toLower c] ++ ":" ++ rest
                 path -> path

    sub :: String -> String -> String -> String
    sub re rep s = subRegex (makeRegex re) s rep
    -- limitTo n s | length s > n = take n s ++ "... (and more)"
    --             | otherwise    = s

-- `subRegex re s rep` replaces each match of `re` in `s` by `rep`, in which `\n` stands for group n.
-- The regexes are regex-tdfa's, not the C library's (Text.Regex): macOS's regcomp is not
-- thread-safe, and the tests sanitize their output in parallel.
subRegex :: Regex -> String -> String -> String
subRegex re s rep = go 0 s (matchAllText re s)
  where
    go _ rest [] = rest
    go pos rest (m:ms)
      = let (off,len) = snd (m ! 0)
            (pre,post) = splitAt (off - pos) rest
        in pre ++ expand m rep ++ go (off + len) (drop len post) ms
    expand m ('\\':d:cs) | isDigit d = fst (m ! digitToInt d) ++ expand m cs
    expand m (c:cs) = c : expand m cs
    expand _ []     = []

expectedSanitize :: String -> String
expectedSanitize input
  = filter (/='\r') input   -- on windows \r still gets through sometimes

runKoka :: Cfg -> FilePath -> FilePath -> IO String
runKoka cfg kokaDir fp
  = do (_, out, _) <- runKokaProcess cfg kokaDir fp
       return (testSanitize kokaDir out)

-- precompile and stop on failure (as parallel tests would otherwise race to build the same modules)
precompile :: Cfg -> FilePath -> IO ()
precompile cfg fp
  = do (exitCode, out, err) <- runKokaProcess cfg "" fp
       when (exitCode /= ExitSuccess) $
         do hFlush stdout
            hPutStrLn stderr ("error: failed to precompile " ++ fp ++ " (" ++ show exitCode ++ ")")
            hPutStrLn stderr (out ++ err)
            exitFailure

runKokaProcess :: Cfg -> FilePath -> FilePath -> IO (ExitCode, String, String)
runKokaProcess cfg kokaDir fp
  = do caseFlags <- readFlagsFile (fp ++ ".flags")
       let relTest = makeRelative kokaDir fp
           optFlag   = if (opt (options cfg) /= 0) then ["-O" ++ show (opt (options cfg))] else []
           kokaFlags = optFlag ++ flags cfg ++ caseFlags
       if (cabal (options cfg))
         then readProcessWithExitCode "cabal" (["new-run", "koka", "--"] ++ kokaFlags ++ [relTest]) ""
         else do let stackFlags = if (sysghc (options cfg)) then ["--system-ghc","--skip-ghc-check"] else []
                 readProcessWithExitCode "stack" (["exec","koka"] ++ stackFlags ++ ["--"] ++ kokaFlags ++ [relTest]) ""



-- `counter` numbers the tests in discovery order; with `--shard=i/n` we only run every n-th test
makeTest :: IORef Int -> Cfg -> FilePath -> Spec
makeTest counter cfg fp
  | takeExtension fp == ".kk" || ".kk.md" `isSuffixOf` fp
      = do let expectedFile = fp ++ ".out"
           isTest <- runIO $ doesFileExist expectedFile
           let shouldRun = not isTest && mode (options cfg) == New || isTest && mode (options cfg) /= New
               (i,n)     = shard (options cfg)
           idx <- if shouldRun then runIO (atomicModifyIORef' counter (\k -> (k+1,k))) else return 0
           when (shouldRun && idx `mod` n == i - 1) $
             it (takeBaseName fp) $ do
               kokaDir <- getCurrentDirectory
               out <- runKoka cfg kokaDir fp
               unless (mode (options cfg) == Test) $ (withBinaryFile expectedFile WriteMode (\h -> hPutStr h out)) -- writeFile expectedFile out
               expected <- testSanitize kokaDir <$> readFile expectedFile
               out `shouldBe` expected
  | otherwise
      = return ()

discoverTests :: Cfg -> FilePath -> Spec
discoverTests cfg0 p = do counter <- runIO (newIORef 0)
                          discover counter cfg0 "" p
  where discover counter cfg cat p
          = do isDirectory <- runIO $ doesDirectoryExist p
               if not isDirectory
                 then makeTest counter cfg p
                 else do
                   fs0  <- runIO (sort <$> listDirectory p)
                   cfg' <- runIO (readConfigFile cfg p)
                   let tgt    = if null (target (options cfg')) then "c" else target (options cfg')
                       skip f = fexclude cfg' tgt f || (quick (options cfg') && fslow cfg' f)
                       fs   = filter (not . skip) fs0  -- todo: make ignoring the failing optional
                       with = if cat == "" then id else describe cat
                   with $ mapM_ (\f -> discover counter cfg' f (p </> f)) fs

readConfigFile :: Cfg -> FilePath -> IO Cfg
readConfigFile cfg dir
  = do let fname = dir </> "config.json"
       hasCfg <- doesFileExist fname
       if (not hasCfg) then return cfg
         else do txt <- readFile fname
                 case decode (trim (removeLineComments txt)) of
                   Ok cfg'   -> return (extendCfg cfg cfg')
                   Error err -> do putStrLn ("(warning: " ++ fname ++ ": " ++ err ++ ")")
                                   return cfg
  where
    -- allow limited form of line comments that cannot contain " or / characters
    removeLineComments txt
      = unlines (map removeLineComment (lines txt))
    removeLineComment cs
      = case dropWhile (\c -> not (c `elem` "/\"")) (reverse cs) of
          ('/':'/':rev) -> reverse rev
          _             -> cs




processOptions :: String -> (Options, [String]) -> (Options,[String])
processOptions arg (options,hargs)
  = if (take 7 arg == "--mode=")
      then let m = parseMode (drop 7 arg)
           in (options{ mode = m }, hargs)
    else if (arg == "--cabal")
      then (options{cabal=True}, hargs)
    else if (take 2 arg == "-O")
      then (options{opt=read (drop 2 arg)}, hargs)
    else if (arg == "--system-ghc")
      then (options{sysghc=True}, hargs)
    else if (take 9 arg == "--target-")
      then (options{target=drop 9 arg}, hargs)
    else if (arg == "--names")
      then (options{target="names"}, hargs)
    else if (arg == "--seq")
      then (options{par=False}, hargs)
    else if (arg == "--rebuild" || arg == "-r")
      then (options{rebuild=True}, hargs)
    else if (arg == "--quick")
      then (options{quick=True}, hargs)
    else if (take 8 arg == "--shard=")
      then (options{shard=parseShard (drop 8 arg)}, hargs)
      else (options, arg : hargs)
  where
    parseShard :: String -> (Int,Int)
    parseShard s = case break (=='/') s of
                     (i,'/':n) | all isDigit i && all isDigit n && not (null i) && not (null n)
                                 && read i >= 1 && read i <= (read n :: Int) -> (read i, read n)
                     _ -> error $ "Invalid shard (expecting --shard=<i>/<n> with 1 <= i <= n): " ++ show s

    parseMode :: String -> Mode
    parseMode "new" = New
    parseMode "update" = Update
    parseMode "test" = Test
    parseMode m = error $ "Unrecognized mode: " ++ show m

getOptions :: IO (Options , [String])
getOptions
  = do argv <- getArgs
       return (foldr processOptions (optionsDefault,[]) argv)

main :: IO ()
main = do
  setEnv "LC_ALL" "C"  -- for unicode
  pwd <- getCurrentDirectory
  (options0, args) <- getOptions
  cabalEnv <- do stackExe <- lookupEnv "STACK_EXE"
                 return (maybe "" id stackExe == "")
  let options = options0{ cabal = cabalEnv || cabal options0 }
  hcfg <- readConfig defaultConfig args
  putStrLn "pre-compiling standard libraries..."
  -- compile all standard libraries before testing so we can run in parallel
  let cfg = initialCfg options
      stdcfg = if rebuild options then cfg{ flags = flags cfg ++ ["-r", "-l"]} else cfg
  precompile stdcfg{flags = "-e" : flags stdcfg} (if target options == "wasm" then "util/link-wasm.kk" else "util/link-test.kk")
  -- precompile bench by compiling a dependent file (ensures correct module name resolution)
  precompile stdcfg "test/lazy/queue/bankers.kk"
  -- precompile mpat-lib so test/cgen/mpat.kk loads it from the .kki
  precompile stdcfg "test/cgen/mpat-lib.kk"
  -- precompile specbox-lib so test/cgen/specbox.kk loads it from the .kki
  -- (both carry a -O2 .flags file so they share one build directory)
  precompile stdcfg "test/cgen/specbox-lib.kk"
  -- precompile the syn-nested chain so syn-nested1 emits only its own core
  precompile stdcfg{flags = flags stdcfg ++ ["-l"]} "test/parc/syn-nested1c.kk"
  precompile stdcfg{flags = flags stdcfg ++ ["--target=js"]} "util/link-test.kk" -- precompiled js libraries as well
  putStrLn "ok."
  let spec = if target options == "names"
               then NameSpec.spec
               else (if (target options == "js" || not (par options)) then id else parallel) $
                    discoverTests cfg (pwd </> "test")
  timeSum <- newIORef 0
  summary <- withArgs [] (runSpec spec hcfg{configFormat=Just (formatterToFormat (specProgress timeSum))})
  evaluateSummary summary


specProgress :: IORef Double -> Formatter
specProgress timeSum = specdoc {
    formatterItemDone = \(nesting, requirement) item -> do
      let Seconds secs = itemDuration item
      liftIO $ modifyIORef' timeSum (+ secs)
      total <- getTotalCount
      let info = itemInfo item
          line = showTotal total nesting requirement ++ showDuration (itemDuration item)
      case itemResult item of
        F.Success -> withSuccessColor $ do
          writeLine line
          forM_ (lines info) $ \ s -> writeLine $ indentationFor ("" : nesting) ++ s
        F.Failure _ _ -> withFailColor $ do
          n <- getFailCount
          writeLine $ line ++ ": FAILED [" ++ show n ++ "]"
          forM_ (take 10 (lines info)) $ \ s ->
            writeLine $ indentationFor nesting ++ (take (400) s)
        F.Pending _ reason -> withPendingColor $ do
          writeLine line
          forM_ (lines info) $ \ s ->
            writeLine $ indentationFor nesting ++ s
          writeLine $ indentationFor nesting ++ "# PENDING: " ++ fromMaybe "No reason given" reason
  , formatterDone = do
      formatterDone specdoc
      secs <- liftIO $ readIORef timeSum
      writeLine $ "Sum of test times: " ++ showFFloat (Just 4) secs " seconds"
} where
    showTotal total nesting req
      = let nest = (length nesting + 1) * 2
            t    = " " ++ show total
        in replicate nest ' ' ++ req ++ replicate (12 - length req) ' ' ++ t
    showDuration (Seconds secs)
      = " (" ++ show (floor (secs * 1000) :: Int) ++ "ms)"
    indentationFor nesting
      = replicate ((length nesting + 1) * 2) ' '



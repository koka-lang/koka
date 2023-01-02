import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import Data.List.Extra (replace, trim)
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process (readProcess)
import Text.Regex
import Text.JSON
import Test.Hspec
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Formatters hiding (Error)

commonFlags :: [String]
commonFlags = ["-c", "-v0", "--console=raw",
               "-O1", -- for cgen/specialize
               -- "--cc=clang",
               -- "--checkcore",
               "-ilib", "-itest",
               "--buildtag=test"]

data Mode = Test | New | Update
  deriving (Eq, Ord, Show)


data Options = Options{ mode :: Mode, cabal :: Bool, sysghc:: Bool, opt :: Int, target :: String, par :: Bool }

optionsDefault = Options Test False False 0 "" True

data Cfg = Cfg{ flags   :: [String],
                options :: Options,
                exclude :: [String],
                fexclude:: !(String -> Bool)
              }  

makeCfg :: [String] -> Options -> [String] -> Cfg
makeCfg flags options []
  = Cfg flags options [] (\s -> False)
makeCfg flags options exclude
  = Cfg flags options exclude fexclude
  where
    matcher item | any (\c -> c `elem` "()[]+*?") item = let r = mkRegex item
                                                         in (\s -> isJust (matchRegex r s))
                 | otherwise = (\s -> s == item || s == (item ++ ".kk"))
    matchers   = map matcher exclude
    fexclude s = any (\m -> m s) matchers
                   
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
             in Ok (makeCfg flags optionsDefault exclude)
        JSNull     -> Ok (makeCfg [] optionsDefault [])
        JSString s -> Ok (makeCfg (words (fromJSString s)) optionsDefault [])
        _          -> Error ("invalid JSON object")

extendCfg :: Cfg -> Cfg -> Cfg
extendCfg (Cfg flags1 opts1 exclude1 fexclude1) (Cfg flags2 opts2 exclude2 fexclude2)
  = Cfg (flags1 ++ flags2) opts1
        (exclude1 ++ exclude2) (\s -> fexclude1 s || fexclude2 s)

initialCfg :: Options -> Cfg
initialCfg options 
  = makeCfg (commonFlags ++ if (not (null (target options))) then ["--target=" ++ target options] else [])
            options [] 
                  

readFlagsFile :: FilePath -> IO [String]
readFlagsFile fp
  = do exists <- doesFileExist fp
       if not exists
         then return []
         else words <$> readFile fp

testSanitize :: FilePath -> String -> String
testSanitize kokaDir
  = trim
  . sub "^Up to date\n" ""
  . sub "\n[[:space:]]+at .*" ""
  . sub "(std_core\\.js:)[[:digit:]]+" "\\1"
  . sub "[\r\n]+" "\n"
  . sub "[[:blank:]]+" " "
  . sub "\\\\" "/"
  -- type variable names and box names
  . sub "\\.box-x[[:digit:]]+(-x[[:digit:]]+)?" ".box"
  . sub "(\\.[a-zA-Z])[[:digit:]]+" "\\1"
  . sub "([a-zA-Z])\\.[[:digit:]]+" "\\1"
  -- . sub "([a-zA-Z])\\.[[:digit:]]+\\.[[:digit:]]+" "\\1"
  . sub "<[[:digit:]]+>" "<0>"
  -- for tests using --showhiddentypesigs,
  -- e.g. .lift250-main => .lift000-main
  --      f: (z00.192 : a) -> a => f: (a.00.000 : a) -> a
  . sub "(\\.m?)lift[[:digit:]]+" "\\1lift000"
  . sub "(^[[:alnum:]]+\\/.+:.*) [[:alpha:]]+[[:digit:]]+\\.[[:digit:]]+ :" "\\1 a00.000 :"
  -- . sub ": [[:digit:]]+([,\\)])" ": 0\\1"
  . if null kokaDir then id else replace xkokaDir "..."
  where 
    xkokaDir = map (\c -> if c == '\\' then '/' else c) kokaDir
    sub re = flip (subRegex (mkRegex re))
    -- limitTo n s | length s > n = take n s ++ "... (and more)"
    --             | otherwise    = s

expectedSanitize :: String -> String  
expectedSanitize input
  = filter (/='\r') input   -- on windows \r still gets through sometimes

runKoka :: Cfg -> FilePath -> FilePath -> IO String
runKoka cfg kokaDir fp
  = do caseFlags <- readFlagsFile (fp ++ ".flags")
       let relTest = makeRelative kokaDir fp
           optFlag   = if (opt (options cfg) /= 0) then ["-O" ++ show (opt (options cfg))] else []
           kokaFlags = optFlag ++ flags cfg ++ caseFlags 
       if (cabal (options cfg))
         then do let argv = ["new-run", "koka", "--"] ++ kokaFlags ++ [relTest]
                 testSanitize kokaDir <$> readProcess "cabal" argv ""       
         else do let stackFlags = if (sysghc (options cfg)) then ["--system-ghc","--skip-ghc-check"] else []
                     argv = ["exec","koka"] ++ stackFlags ++ ["--"] ++ kokaFlags ++ [relTest]
                 testSanitize kokaDir <$> readProcess "stack" argv ""
       

makeTest :: Cfg -> FilePath -> Spec
makeTest cfg fp
  | takeExtension fp == ".kk"
      = do let expectedFile = fp ++ ".out"
           isTest <- runIO $ doesFileExist expectedFile
           let shouldRun = not isTest && mode (options cfg) == New || isTest && mode (options cfg) /= New
           when shouldRun $
             it (takeBaseName fp) $ do
               kokaDir <- getCurrentDirectory       
               out <- runKoka cfg kokaDir fp
               unless (mode (options cfg) == Test) $ (withBinaryFile expectedFile WriteMode (\h -> hPutStr h out)) -- writeFile expectedFile out
               expected <- testSanitize kokaDir <$> readFile expectedFile
               out `shouldBe` expected
  | otherwise
      = return ()

discoverTests :: Cfg -> FilePath -> Spec
discoverTests cfg0 p = discover cfg0 "" p
  where discover cfg cat p
          = do isDirectory <- runIO $ doesDirectoryExist p
               if not isDirectory
                 then makeTest cfg p
                 else do
                   fs0  <- runIO (sort <$> listDirectory p)
                   cfg' <- runIO (readConfigFile cfg p)
                   let fs   = filter (not . fexclude cfg') fs0  -- todo: make ignoring the failing optional
                       with = if cat == "" then id else describe cat
                   with $ mapM_ (\f -> discover cfg' f (p </> f)) fs

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
    else if (arg == "--target-js")
      then (options{target="js"}, hargs)
    else if (arg == "--target-c64c")
      then (options{target="c64c"}, hargs)      
    else if (arg == "--seq")
      then (options{par=False}, hargs)
      else (options, arg : hargs)
  where
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
  pwd <- getCurrentDirectory
  (options0, args) <- getOptions
  cabalEnv <- do stackExe <- lookupEnv "STACK_EXE"
                 return (maybe "" id stackExe == "")
  let options = options0{ cabal = cabalEnv || cabal options0 }
  hcfg <- readConfig defaultConfig args
  putStrLn "pre-compiling standard libraries..."
  -- compile all standard libraries before testing so we can run in parallel
  let cfg = initialCfg options
  runKoka cfg "" "util/link-test.kk" 
  putStrLn "ok."
  let spec = (if (target options == "js" || not (par options)) then id else parallel) $ 
             discoverTests cfg (pwd </> "test")
  summary <- withArgs [] (runSpec spec hcfg{configFormatter=Just specProgress})
  evaluateSummary summary


specProgress = specdoc {
    exampleSucceeded = \(nesting, requirement) info -> withSuccessColor $ do
      total  <- getTotalCount
      writeLine $ showTotal total nesting requirement
      forM_ (lines info) $ \ s -> writeLine $ indentationFor ("" : nesting) ++ s

  , exampleFailed = \(nesting, requirement) info _ -> withFailColor $ do
      n <- getFailCount
      total  <- getTotalCount
      writeLine $ showTotal total nesting requirement ++ ": FAILED [" ++ show n ++ "]"
      forM_ (take 10 (lines info)) $ \ s ->
        writeLine $ indentationFor nesting ++ (take (400) s)

  , examplePending = \(nesting, requirement) info reason -> withPendingColor $ do
      total  <- getTotalCount    
      writeLine $ showTotal total nesting requirement
      forM_ (lines info) $ \ s ->
        writeLine $ indentationFor nesting ++ s
      writeLine $ indentationFor nesting ++ "# PENDING: " ++ fromMaybe "No reason given" reason
} where
    showTotal total nesting req
      = let nest = (length nesting + 1) * 2
            t    = " " ++ show total
        in replicate nest ' ' ++ req ++ replicate (12 - length req) ' ' ++ t
    indentationFor nesting 
      = replicate ((length nesting + 1) * 2) ' '

    

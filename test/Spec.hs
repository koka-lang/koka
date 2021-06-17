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
               -- "--checkcore",
               "-ilib", "-itest",
               "--outtag=test"]

data Mode = Test | New | Update
  deriving (Eq, Ord, Show)

data Cfg = Cfg{ flags   :: [String],
                exclude :: [String],
                fexclude:: !(String -> Bool),
                cabal :: Bool
              }  

makeCfg flags []
  = Cfg flags [] (\s -> False)
makeCfg flags exclude
  = Cfg flags exclude fexclude
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
             in Ok (makeCfg flags exclude False)
        JSNull     -> Ok (makeCfg [] [] False)
        JSString s -> Ok (makeCfg (words (fromJSString s)) [] False)
        _          -> Error ("invalid JSON object")

extendCfg :: Cfg -> Cfg -> Cfg
extendCfg (Cfg flags1 exclude1 fexclude1 cabal1) (Cfg flags2 exclude2 fexclude2 cabal2)
  = Cfg (flags1 ++ flags2) (exclude1 ++ exclude2) (\s -> fexclude1 s || fexclude2 s) (cabal1 || cabal2)

initialCfg :: Bool -> Cfg
initialCfg cabal = makeCfg commonFlags [] cabal
                        
                  

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
  . replace kokaDir "..."
  where sub re = flip (subRegex (mkRegex re))

expectedSanitize :: String -> String  
expectedSanitize input
  = filter (/='\r') input   -- on windows \r still gets through sometimes

runKoka :: Cfg -> FilePath -> IO String
runKoka cfg fp
  = do caseFlags <- readFlagsFile (fp ++ ".flags")
       kokaDir <- getCurrentDirectory
       let relTest = makeRelative kokaDir fp
       if (cabal cfg)
         then do let argv = ["run", "koka", "--"] ++ flags cfg ++ caseFlags ++ [relTest]
                 testSanitize kokaDir <$> readProcess "cabal" argv ""       
         else do let argv = ["exec", "koka", "--"] ++ flags cfg ++ caseFlags ++ [relTest]
                 testSanitize kokaDir <$> readProcess "stack" argv ""
       

makeTest :: Mode -> Cfg -> FilePath -> Spec
makeTest mode cfg fp
  | takeExtension fp == ".kk"
      = do let expectedFile = fp ++ ".out"
           isTest <- runIO $ doesFileExist expectedFile
           let shouldRun = not isTest && mode == New || isTest && mode /= New
           when shouldRun $
             it (takeBaseName fp) $ do
               out <- runKoka cfg fp
               unless (mode == Test) $ (withBinaryFile expectedFile WriteMode (\h -> hPutStr h out)) -- writeFile expectedFile out
               expected <- expectedSanitize <$> readFile expectedFile
               out `shouldBe` expected
  | otherwise
      = return ()

discoverTests :: Bool -> Mode -> FilePath -> Spec
discoverTests cabal mode p = discover (initialCfg cabal) "" p
  where discover cfg cat p
          = do isDirectory <- runIO $ doesDirectoryExist p
               if not isDirectory
                 then makeTest mode cfg p
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

parseMode :: String -> Mode
parseMode "new" = New
parseMode "update" = Update
parseMode "test" = Test
parseMode m = error $ "Unrecognized mode: " ++ show m

getOpts :: [String] -> (Bool, Mode, [String])
getOpts ("--mode":mode:args) =
  let (cabal',mode',args') = getOpts args
   in (cabal', max (parseMode mode) mode', args')
getOpts ("--cabal":args) =
  let (_,mode',args') = getOpts args
  in (True,mode',args')
getOpts (x:args) =
  let (cabal,mode, args') = getOpts args
   in (cabal,mode, x:args')
getOpts [] = (False, Test, [])

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  (cabalArg, mode, args) <- getOpts <$> getArgs  
  cabalEnv <- do stackExe <- lookupEnv "STACK_EXE"
                 return (maybe "" id stackExe == "")
  let cabal = cabalArg || cabalEnv
  hcfg <- readConfig defaultConfig args
  putStrLn "pre-compiling standard libraries..."
  -- compile all standard libraries before testing so we can run in parallel
  runKoka (initialCfg cabal) "util/link-test.kk" 
  putStrLn "ok."
  let spec = parallel $ discoverTests cabal mode (pwd </> "test")
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
      forM_ (lines info) $ \ s ->
        writeLine $ indentationFor nesting ++ s

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

    

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
import Test.Hspec
import Test.Hspec.Core.Runner
import Text.Regex

data Mode = Test | New | Update
  deriving (Eq, Ord, Show)

readFlags :: FilePath -> IO [String]
readFlags fp
  = do exists <- doesFileExist fp
       if not exists
         then return []
         else words <$> readFile fp

commonFlags :: [String]
commonFlags = ["-c", "-v0", "--console=raw",
               -- "--checkcore",
               "-ilib", "-itest",
               "--outdir=" ++ "out" </> "test"]

testSanitize :: FilePath -> String -> String
testSanitize kokaDir
  = trim
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

runKoka :: FilePath -> IO String
runKoka fp
  = do dirFlags <- readFlags (takeDirectory fp </> ".flags")
       caseFlags <- readFlags (fp ++ ".flags")
       kokaDir <- getCurrentDirectory
       let relTest = makeRelative kokaDir fp
       let argv = ["exec", "koka", "--"] ++ commonFlags ++ dirFlags ++ caseFlags ++ [relTest]
       testSanitize kokaDir <$> readProcess "stack" argv ""

makeTest :: Mode -> FilePath -> Spec
makeTest mode fp
  | takeExtension fp == ".kk"
      = do let expectedFile = fp ++ ".out"
           isTest <- runIO $ doesFileExist expectedFile
           let shouldRun = not isTest && mode == New || isTest && mode /= New
           when shouldRun $
             it (takeBaseName fp) $ do
               out <- runKoka fp
               unless (mode == Test) $ (withBinaryFile expectedFile WriteMode (\h -> hPutStr h out)) -- writeFile expectedFile out
               expected <- expectedSanitize <$> readFile expectedFile
               out `shouldBe` expected
  | otherwise
      = return ()

discoverTests :: Mode -> FilePath -> Spec
discoverTests mode = discover ""
  where discover cat p
          = do isDirectory <- runIO $ doesDirectoryExist p
               if not isDirectory
                 then makeTest mode p
                 else do
                   fs0 <- runIO (sort <$> listDirectory p)
                   inFailing <- readFailing p
                   let fs   = filter (not . inFailing) fs0  -- todo: make ignoring the failing optional
                       with = if cat == "" then id else describe cat
                   with $ mapM_ (\f -> discover f (p </> f)) fs

readFailing dir 
  = runIO $
    do let fname = dir </> "failing.txt"
       hasFailing <- doesFileExist fname
       if (hasFailing) 
         then do txt <- readFile fname
                 let items = filter (\s -> not ("#" `isPrefixOf` s)) $ map trim $ lines txt
                     matcher item | any (\c -> c `elem` "([]+*?)") item = let r = mkRegex item
                                                                          in (\s -> isJust (matchRegex r s))
                                  | otherwise = (\s -> s == item)
                     matchers = map matcher items                     
                 return (\s -> any (\m -> m s) matchers) 
         else return (\s -> False)

parseMode :: String -> Mode
parseMode "new" = New
parseMode "update" = Update
parseMode "test" = Test
parseMode m = error $ "Unrecognized mode: " ++ show m

getMode :: [String] -> (Mode, [String])
getMode ("--mode":mode:args) =
  let (mode',args') = getMode args
   in (max (parseMode mode) mode', args')
getMode (x:args) =
  let (mode, args') = getMode args
   in (mode, x:args')
getMode [] = (Test, [])

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  (mode, args) <- getMode <$> getArgs
  hcfg <- readConfig defaultConfig args
  let spec = discoverTests mode (pwd </> "test")
  summary <- withArgs [] (runSpec spec hcfg)
  evaluateSummary summary

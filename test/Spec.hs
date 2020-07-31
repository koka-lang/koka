import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.List.Extra (replace, trim)
import System.Directory
import System.FilePath
import System.IO
import System.Process (readProcess)
import Test.Hspec
import Text.Regex

readFlags :: FilePath -> IO [String]
readFlags fp
  = do exists <- doesFileExist fp
       if not exists
         then return []
         else words <$> readFile fp

commonFlags :: [String]
commonFlags = ["-c", "--console=raw",
               "--checkcore",
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

runKoka :: [String] -> FilePath -> IO String
runKoka flags file
  = readProcess "stack" argv ""
  where argv = ["exec", "koka", "--"] ++ commonFlags ++ flags ++ [file]

makeTest :: FilePath -> Spec
makeTest fp
  | takeExtension fp == ".kk"
      = do let expectedFile = fp ++ ".out"
           isTest <- runIO $ doesFileExist expectedFile
           when isTest $
             it (takeBaseName fp) $ do
               expected <- readFile expectedFile
               dirFlags <- readFlags (takeDirectory fp </> ".flags")
               caseFlags <- readFlags (fp ++ ".flags")
               kokaDir <- getCurrentDirectory
               let relTest = makeRelative kokaDir fp
               out <- runKoka (dirFlags ++ caseFlags) relTest
               testSanitize kokaDir out `shouldBe` expected
  | otherwise
      = return ()

discoverTests :: FilePath -> Spec
discoverTests = discover ""
  where discover cat p
          = do isDirectory <- runIO $ doesDirectoryExist p
               if not isDirectory
                 then makeTest p
                 else do
                   fs <- runIO (sort <$> listDirectory p)
                   let with = if cat == "" then id else describe cat
                   with $ mapM_ (\f -> discover f (p </> f)) fs

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  hspec $ discoverTests (pwd </> "test")

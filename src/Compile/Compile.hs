module Compile.Compile() where

import Data.Char
import Control.Exception
import Control.Applicative
import Control.Monad          ( ap, when )
import qualified Control.Monad.Fail as F
import System.Directory ( doesFileExist )
import Lib.PPrint
import Platform.Config        ( version, exeExtension, dllExtension, libPrefix, libExtension, pathSep, sourceExtension )
import Common.Error
import Common.File
import Common.ColorScheme
import Common.Range
import Type.Type
import Compiler.Options
import Compile.Module



{---------------------------------------------------------------
  Create initial empty modules
  from a source path or module name (import)
---------------------------------------------------------------}

moduleFromSource :: FilePath -> Compile Module
moduleFromSource fpath0
  = do let fpath = normalize fpath0
       mbpath <- searchSourceFile "" fpath
       case mbpath of
         Nothing          -> throwFileNotFound fpath
         Just (root,stem) -> do let stemParts  = splitPath (noexts stem)
                                    sourcePath = joinPath root stem
                                modName <- if isAbsolute stem || any (not . isValidId) stemParts
                                            then case reverse stemParts of
                                                    (base:_)  | isValidId base
                                                      -> return (newModuleName base)  -- module may not be found if imported
                                                    _ -> throwError (ErrorIO (text ("file path cannot be mapped to a valid module name: " ++ sourcePath)))
                                            else return (newModuleName (noexts stem))
                                ifacePath <- outputName (moduleNameToPath modName ++ ifaceExtension)
                                return $ moduleCreateEmpty modName sourcePath ifacePath
  where
    isValidId :: String -> Bool  -- todo: make it better
    isValidId ""      = False
    isValidId (c:cs)  = isLower c && all (\c -> isAlphaNum c || c `elem` "_-") cs


moduleFromModuleName :: FilePath -> Name -> Compile Module
moduleFromModuleName relativeDir modName
  = do mbSourceName <- searchSourceFile relativeDir (nameToPath modName ++ sourceExtension)
       ifacePath    <- outputName (moduleNameToPath modName ++ ifaceExtension)
       case mbSourceName of
         Just (root,stem)
            -> return $ moduleCreateEmpty modName (joinPath root stem) ifacePath
         Nothing
            -> do ifaceExist <- liftIO $ doesFileExist ifacePath
                  if ifaceExist
                    then do warningMessage (\cs -> text "interface" <+> color (colorModule cs) (pretty modName) <+> text "found but no corresponding source module")
                            return $ moduleCreateEmpty modName "" ifacePath
                    else throwModuleNotFound rangeNull modName


searchSourceFile :: FilePath -> FilePath -> Compile (Maybe (FilePath,FilePath))
searchSourceFile currentDir fname
  = do -- trace ("search source: " ++ fname ++ " from " ++ concat (intersperse ", " (currentDir:includePath flags))) $ return ()
       -- currentDir is set when importing from a module so a module name is first resolved relative to the current module
       flags <- getFlags
       extra <- if null currentDir then return []
                                   else do{ d <- liftIO $ realPath currentDir; return [d] }
       mbP <- liftIO $ searchPathsCanonical (extra ++ includePath flags) [sourceExtension,sourceExtension++".md"] [] fname
       case mbP of
         Just (root,stem) | root == currentDir  -- make a relative module now relative to the include path
           -> return $ Just (makeRelativeToPaths (includePath flags) (joinPath root stem))
         _ -> return mbP




{---------------------------------------------------------------
  Helpers
---------------------------------------------------------------}

throwModuleNotFound :: Range -> Name -> Compile a
throwModuleNotFound range name
  = do flags <- getFlags
       throw (ErrorGeneral range (errorNotFound flags colorModule "module" (pretty name)))

throwFileNotFound :: FilePath -> Compile a
throwFileNotFound name
  = do flags <- getFlags
       throw (ErrorIO $ text "error:" <+> errorNotFound flags colorSource "" (text name))

errorNotFound flags clr kind namedoc
  = text ("could not find" ++ (if null kind then "" else (" " ++ kind)) ++ ":") <+> color (clr cscheme) namedoc <->
    text "search path:" <+> prettyIncludePath flags
  where
    cscheme = colorSchemeFromFlags flags

-- name to path preserves '/' while moduleNameToPath uses '_' for '/'
nameToPath :: Name -> FilePath
nameToPath name
  = show name

outputName :: FilePath -> Compile FilePath
outputName fpath
  = do flags <- getFlags
       return (outName flags fpath)

ifaceExtension :: FilePath
ifaceExtension
  = sourceExtension ++ "i"




{---------------------------------------------------------------
  Compilation monad
  carries flags and terminal and catches errors
---------------------------------------------------------------}
data Terminal = Terminal{ termError :: ErrorMessage -> IO ()
                        , termPhase :: String -> IO ()
                        , termPhaseDoc :: Doc -> IO ()
                        , termType  :: Scheme -> IO ()
                        , termDoc   :: Doc -> IO ()
                        }


data Compile a = Compile (Env -> IO a)

data Env = Env { envTerminal :: Terminal, envFlags :: Flags }


runCompile :: Terminal -> Flags -> Compile a -> IO (Maybe a)
runCompile term flags (Compile ie)
  = catch (do x <- ie (Env term flags)
              return (Just x))
          (\err ->
           do termError term err
              return Nothing)

liftIO :: IO a -> Compile a
liftIO io = Compile (\env -> io)

instance Functor Compile where
  fmap f (Compile ie)  = Compile (\env -> fmap f (ie env))

instance Applicative Compile where
  pure x = Compile (\env -> return x)
  (<*>)  = ap

instance Monad Compile where
  -- return = pure
  (Compile ie) >>= f
    = Compile (\env -> do x <- ie env
                          case (f x) of
                            Compile ie' -> ie' env)

instance F.MonadFail Compile where
  fail msg = throw (ErrorIO (text msg))

throwError :: ErrorMessage -> Compile a
throwError msg
  = liftIO $ throw msg

getFlags :: Compile Flags
getFlags
  = Compile (\env -> return (envFlags env))

getTerminal :: Compile Terminal
getTerminal
  = Compile (\env -> return (envTerminal env))

getColorScheme :: Compile ColorScheme
getColorScheme
  = do flags <- getFlags
       return (colorSchemeFromFlags flags)



warningMessage :: (ColorScheme -> Doc) -> Compile ()
warningMessage doc
  = do term    <- getTerminal
       cscheme <- getColorScheme
       liftIO $ termDoc term $ color (colorWarning (cscheme)) $ (text "warning:" <+> doc cscheme)


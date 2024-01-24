module Compile.Compile() where

import Data.Char
import Data.List
import Data.Either
import Control.Exception
import Control.Applicative
import Control.Monad          ( ap, when )
import qualified Control.Monad.Fail as F
import Control.Concurrent.Async (mapConcurrently)
import System.Directory ( doesFileExist )

import Lib.Scc( scc )
import Lib.PPrint
import Platform.Config        ( version, exeExtension, dllExtension, libPrefix, libExtension, pathSep, sourceExtension )
import Common.Error
import Common.File
import Common.ColorScheme
import Common.Range
import Common.NamePrim        (isPrimitiveModule)
import Syntax.Syntax
import Syntax.Parse           (parseProgramFromFile)
import Type.Type
import Compiler.Options
import Compile.Module

{---------------------------------------------------------------
  Given a set of modules,
  return all required modules in build order
---------------------------------------------------------------}

resolveDependencies :: [Module] -> Compile [Module]
resolveDependencies modules
  = do pmodules   <- mapConcurrent ensureParsed modules
       newimports <- concat <$> mapM (addImports pmodules) pmodules
       if (null newimports)
         then toBuildOrder pmodules
         else resolveDependencies (newimports ++ pmodules)
  where
    addImports pmodules mod
      = concat <$> mapM addImport (modImports mod)
      where
        addImport :: ModuleName -> Compile [Module]
        addImport impName = if any (\m -> modName m == impName) pmodules
                             then return []
                             else do let relativeDir = dirname (modSourcePath mod)
                                     m <- moduleFromModuleName relativeDir impName  -- todo: set error, prevent parsing?
                                     return [m]

toBuildOrder :: [Module] -> Compile [Module]
toBuildOrder modules
  = -- todo: might be faster to check if the modules are already in build order before doing a full `scc` ?
    let deps    = [(modName mod, modImports mod) | mod <- modules]
        ordered = scc deps
        ungroup [mname]  | Just mod <- find (\m -> modName m == mname) modules  = return [mod]
        ungroup grp      = do throwError (errorMessageKind ErrBuild rangeNull (text ("recursive imports: " ++ show grp))) -- todo: nice error
                              return []
    in concat <$> mapM ungroup ordered


{---------------------------------------------------------------
  Parse modules
---------------------------------------------------------------}

ensureParsed :: Module -> Compile Module
ensureParsed mod
  = if modPhase mod >= ModParsed
      then return mod
      else do res <- checked (moduleParse mod)
              case res of
                Right mod' -> return mod'
                Left errs  -> return mod{ modErrors = mergeErrors errs (modErrors mod) }

moduleParse :: Module -> Compile Module
moduleParse mod
  = do flags <- getFlags
       let allowAt = isPrimitiveModule (modName mod)
       prog <- liftIOError $ parseProgramFromFile allowAt (semiInsert flags) (modSourcePath mod)
       return mod{ modPhase = ModParsed
                 , modLexemes = programLexemes prog
                 , modProgram = Just prog
                 , modImports = nub (map importFullName (programImports prog))
                 }

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
                                                    _ -> throwError (errorMessageKind ErrBuild rangeNull (text ("file path cannot be mapped to a valid module name: " ++ sourcePath)))
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
                    then do cs <- getColorScheme
                            addWarningMessage (warningMessageKind ErrBuild rangeNull (text "interface" <+> color (colorModule cs) (pretty modName) <+> text "found but no corresponding source module"))
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
       throwError (errorMessageKind ErrBuild range (errorNotFound flags colorModule "module" (pretty name)))

throwFileNotFound :: FilePath -> Compile a
throwFileNotFound name
  = do flags <- getFlags
       throwError (errorMessageKind ErrBuild rangeNull (errorNotFound flags colorSource "" (text name)))

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
runCompile term flags cmp
  = do res <- runCompileEnv (Env term flags) cmp
       case res of
         Right x  -> return (Just x)
         Left err -> do mapM_ (termError term) (errors err)
                        return Nothing


runCompileEnv :: Env -> Compile a -> IO (Either Errors a)
runCompileEnv env (Compile cmp)
  = catch (do x <- cmp env
              return (Right x))
          (\errs -> return (Left errs))


liftIO :: IO a -> Compile a
liftIO io = Compile (\env -> io)

liftIOError :: IO (Error () a) -> Compile a
liftIOError io
  = do res <- liftIO io
       case checkError res of
         Left errs          -> throw errs
         Right (x,Errors warnings) -> do mapM_ addWarningMessage warnings
                                         return x


mapConcurrent :: (a -> Compile b) -> [a] -> Compile [b]
mapConcurrent f xs
  = do env <- getEnv
       ys  <- liftIO $ mapConcurrently (\x -> runCompileEnv env (f x)) xs
       let errs = lefts ys
       if null errs
         then return (rights ys)
         else throw (foldr mergeErrors errorsNil errs)




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
  fail msg = throwError (errorMessageKind ErrGeneral rangeNull (text msg))

checked :: Compile a -> Compile (Either Errors a)
checked (Compile cmp)
  = Compile (\env -> catch (do x <- cmp env
                               return (Right x))
                           (\errs -> return (Left errs)))

throwError :: ErrorMessage -> Compile a
throwError msg
  = liftIO $ throw (errorsSingle msg)

getEnv :: Compile Env
getEnv
  = Compile (\env -> return env)

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



addWarningMessage :: ErrorMessage -> Compile ()
addWarningMessage warn
  = do term    <- getTerminal
       cscheme <- getColorScheme
       liftIO $ termDoc term $ ppErrorMessage "" True cscheme warn


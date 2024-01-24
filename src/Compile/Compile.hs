module Compile.Compile( Compile
                      , VFS(..), noVFS
                      , runCompileIO
                      , resolveDependencies
                      , moduleFromSource
                      ) where

import Data.Char
import Data.Maybe
import Data.List
import Data.Either
import Data.IORef
import Control.Exception
import Control.Applicative
import Control.Monad          ( ap, when )
import qualified Control.Monad.Fail as F
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.Chan
import Control.Concurrent
import System.Directory ( doesFileExist )

import Lib.Scc( scc )
import Lib.PPrint
import Platform.Config        ( version, exeExtension, dllExtension, libPrefix, libExtension, pathSep, sourceExtension )
import Common.Syntax          ( Target(..))
import Common.Error
import Common.File   hiding (getFileTime)
import qualified Common.File as F
import Common.ColorScheme
import Common.Range
import Common.NamePrim        (isPrimitiveModule)
import Syntax.Syntax
import Syntax.Parse           (parseProgramFromFile)
import Type.Type
import qualified Core.Core as Core
import Core.Parse
import Compiler.Options
import Compile.Module

{---------------------------------------------------------------
  Given a set of modules,
  return all required modules in build order
---------------------------------------------------------------}

-- given a root set of modules, load- or parse all required modules
resolveDependencies :: [Module] -> Compile [Module]
resolveDependencies modules
  = do ordered <- phaseTimed "resolve" (list (map (pretty . modName) modules)) $
                  resolveDeps modules
       mapM moduleFlushErrors ordered

resolveDeps modules
  = do pmodules   <- mapConcurrent ensureLoaded modules           -- we can concurrently load/parse modules
       newimports <- nubBy (\m1 m2 -> modName m1 == modName m2) <$> concat <$> mapM (addImports pmodules) pmodules
       if (null newimports)
         then toBuildOrder pmodules
         else do phase "resolve" (list (map (pretty . modName) newimports))
                 resolveDeps (newimports ++ pmodules)
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

-- order the loaded modules in build order (by using scc)
toBuildOrder :: [Module] -> Compile [Module]
toBuildOrder modules
  = -- todo: might be faster to check if the modules are already in build order before doing a full `scc` ?
    let deps    = [(modName mod, modImports mod) | mod <- modules]
        ordered = scc deps
        ungroup [mname]  | Just mod <- find (\m -> modName m == mname) modules  = return [mod]
        ungroup grp      = do throwError (errorMessageKind ErrBuild rangeNull (text ("recursive imports: " ++ show grp))) -- todo: nice error
                              return []
    in do phaseVerbose "build order" (list (map (\grp -> hsep (map (pretty) grp)) ordered))
          concat <$> mapM ungroup ordered


moduleFlushErrors :: Module -> Compile Module
moduleFlushErrors mod
  = do addErrors (modErrors mod)
       return mod{ modErrors = errorsNil }

{---------------------------------------------------------------
  Parse modules from source, or load from an interface file
  After this, `modImports` should be valid
---------------------------------------------------------------}

ensureLoaded :: Module -> Compile Module
ensureLoaded mod
  = if modPhase mod >= ModLoaded
      then return mod
      else do (mod',errs) <- checkedDefault mod $ -- on error, return the original module
                             if not (null (modLibIfacePath mod)) && modLibIfaceTime mod > modIfaceTime mod
                               then moduleLoadLibIface mod
                               else if modSourceTime mod > modIfaceTime mod
                                 then moduleParse mod
                                 else moduleLoadIface mod
              return mod'{ modErrors = mergeErrors errs (modErrors mod') }

moduleParse :: Module -> Compile Module
moduleParse mod
  = do phaseVerbose "parsing" (text (modSourcePath mod))
       flags <- getFlags
       let allowAt = isPrimitiveModule (modName mod)
       prog <- liftIOError $ parseProgramFromFile allowAt (semiInsert flags) (modSourcePath mod)
       return mod{ modPhase = ModParsed
                 , modLexemes = programLexemes prog
                 , modProgram = Just prog
                 , modImports = nub (map importFullName (programImports prog))
                 }

moduleLoadIface :: Module -> Compile Module
moduleLoadIface mod
  = do phaseVerbose "loading" (text (modIfacePath mod))
       (core,parseInlines) <- liftIOError $ parseCore (modIfacePath mod) (modSourcePath mod)
       return mod{ modPhase   = ModCompiled
                 , modImports = map Core.importName (Core.coreProgImports core)
                 , modCore    = Just core
                 , modInlines = Left parseInlines
                 }

moduleLoadLibIface :: Module -> Compile Module
moduleLoadLibIface mod
  = do phaseVerbose "loading" (text (modLibIfacePath mod))
       (core,parseInlines) <- liftIOError $ parseCore (modLibIfacePath mod) (modSourcePath mod)
       flags <- getFlags
       liftIO $ copyLibIfaceToOutput flags (modLibIfacePath mod) (modIfacePath mod) core
       return mod{ modPhase   = ModCompiled
                 , modImports = map Core.importName (Core.coreProgImports core)
                 , modCore    = Just core
                 , modInlines = Left parseInlines
                 }

copyLibIfaceToOutput :: Flags -> FilePath -> FilePath -> Core.Core -> IO ()
copyLibIfaceToOutput flags libIfacePath ifacePath core  {- core is needed to know imported clibs etc. -}
  = do let withext fname ext = notext fname ++ ext
           force = rebuild flags
       copyTextIfNewer force libIfacePath ifacePath
       case target flags of
        CS    -> do copyBinaryIfNewer force (withext libIfacePath dllExtension) (withext ifacePath dllExtension)
        JS _  -> do copyTextIfNewer force (withext libIfacePath ".mjs") (withext ifacePath ".mjs")
        C _   -> do copyTextIfNewer force (withext libIfacePath ".c") (withext ifacePath ".c")
                    copyTextIfNewer force (withext libIfacePath ".h") (withext ifacePath ".h")
                    let cc = ccomp flags
                    copyBinaryIfNewer force (ccObjFile cc (notext libIfacePath)) (ccObjFile cc (notext ifacePath))
                    mapM_ (\clib -> do let libFile = ccLibFile cc clib
                                       -- todo: only copy if it exists?
                                       copyBinaryIfNewer force (joinPath (dirname libIfacePath) libFile)
                                                               (joinPath (dirname ifacePath) libFile)
                          ) (clibsFromCore flags core)


clibsFromCore flags core
  = externalImportKeyFromCore (target flags) (buildType flags) core "library"

csyslibsFromCore flags core
  = externalImportKeyFromCore (target flags) (buildType flags) core "syslib"


externalImportKeyFromCore :: Target -> BuildType -> Core.Core -> String -> [String]
externalImportKeyFromCore target buildType core key
  = catMaybes [Core.eimportLookup buildType key keyvals  | keyvals <- externalImportsFromCore target core]

externalImportsFromCore :: Target -> Core.Core -> [[(String,String)]]
externalImportsFromCore target core
  = [keyvals  | Core.ExternalImport imports _ <- Core.coreProgExternals core, (target,keyvals) <- imports]


{---------------------------------------------------------------
  Create initial empty modules
  from a source path (from IDE) or module name (from an import)
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
                                moduleValidate $ moduleCreateInitial modName sourcePath ifacePath ""
  where
    isValidId :: String -> Bool  -- todo: make it better
    isValidId ""      = False
    isValidId (c:cs)  = isLower c && all (\c -> isAlphaNum c || c `elem` "_-") cs


moduleFromModuleName :: FilePath -> Name -> Compile Module
moduleFromModuleName relativeDir modName
  = do mbSourceName <- searchSourceFile relativeDir (nameToPath modName ++ sourceExtension)
       ifacePath    <- outputName (moduleNameToPath modName ++ ifaceExtension)
       libIfacePath <- searchLibIfaceFile (moduleNameToPath modName ++ ifaceExtension)
       case mbSourceName of
         Just (root,stem)
            -> moduleValidate $ moduleCreateInitial modName (joinPath root stem) ifacePath libIfacePath
         Nothing
            -> do ifaceExist <- liftIO $ doesFileExistAndNotEmpty ifacePath
                  if ifaceExist
                    then do cs <- getColorScheme
                            addWarningMessage (warningMessageKind ErrBuild rangeNull (text "interface" <+> color (colorModule cs) (pretty modName) <+> text "found but no corresponding source module"))
                            moduleValidate $ moduleCreateInitial modName "" ifacePath libIfacePath
                    else throwModuleNotFound rangeNull modName

-- Find a source file and resolve it
-- with a `(root,stem)` where `stem` is the minimal module path relative to the include roots.
-- The root is either in the include paths or the full directory for absolute paths.
-- (and absolute file paths outside the include roots always have a single module name corresponding to the file)
-- relativeDir is set when importing from a module so a module name is first resolved relative to the current module
searchSourceFile :: FilePath -> FilePath -> Compile (Maybe (FilePath,FilePath))
searchSourceFile relativeDir fname
  = do -- trace ("search source: " ++ fname ++ " from " ++ concat (intersperse ", " (relativeDir:includePath flags))) $ return ()
       flags <- getFlags
       extra <- if null relativeDir then return []
                                   else do{ d <- liftIO $ realPath relativeDir; return [d] }
       mbP <- liftIO $ searchPathsCanonical (extra ++ includePath flags) [sourceExtension,sourceExtension++".md"] [] fname
       case mbP of
         Just (root,stem) | root == relativeDir  -- make a relative module now relative to the include path
           -> return $ Just (makeRelativeToPaths (includePath flags) (joinPath root stem))
         _ -> return mbP

-- find a pre-compiled libary interface
-- in the future we can support packages as well
searchLibIfaceFile :: FilePath -> Compile FilePath  -- can be empty
searchLibIfaceFile fname
  = do flags <- getFlags
       let libIfacePath = joinPaths [localLibDir flags, buildVariant flags, fname]
       exist <- liftIO $ doesFileExist libIfacePath
       return (if exist then libIfacePath else "")


{---------------------------------------------------------------
  Validate if modules are still valid
---------------------------------------------------------------}

revalidate :: [Module] -> Compile [Module]
revalidate modules
  = mapM moduleValidate modules

moduleValidate :: Module -> Compile Module
moduleValidate mod
  = do ftSource   <- getFileTime (modSourcePath mod)
       ftIface    <- getFileTime (modIfacePath mod)
       ftLibIface <- getFileTime (modLibIfacePath mod)
       let mod' = mod{ modSourceTime = ftSource
                     , modIfaceTime  = ftIface
                     , modLibIfaceTime = ftLibIface
                     }
       -- phaseVerbose "trace" (pretty (modName mod) <.> text (": times: " ++ show (ftSource,ftIface)))
       if (modSourceTime mod' > modIfaceTime mod' || modLibIfaceTime mod' > modIfaceTime mod')
         then return mod'{ modPhase = ModInit, modErrors = errorsNil }
         else return mod'



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
       return $ joinPath (fullBuildDir flags ++ "-" ++ flagsHash flags) fpath

ifaceExtension :: FilePath
ifaceExtension
  = sourceExtension ++ "i"




{---------------------------------------------------------------
  Compilation monad
  carries flags and terminal and catches errors
---------------------------------------------------------------}
data VFS = VFS { vfsFind :: FilePath -> Maybe (BString,FileTime) }

data Compile a = Compile (Env -> IO a)

data Env = Env { envTerminal :: Terminal, envFlags :: Flags, envErrors :: IORef Errors, envVFS :: VFS }

noVFS :: VFS
noVFS = VFS (\fpath -> Nothing)


runCompileIO :: Terminal -> Flags -> VFS -> Compile a -> IO (Maybe a)
runCompileIO term flags vfs cmp
  = do res <- runCompile term flags vfs cmp
       case res of
         Right (x,errs) -> do mapM_ (termError term) (errors errs)
                              return (Just x)
         Left errs      -> do mapM_ (termError term) (errors errs)
                              return Nothing

runCompile :: Terminal -> Flags -> VFS -> Compile a -> IO (Either Errors (a,Errors))
runCompile term flags vfs cmp
  = do errs <- newIORef errorsNil
       (termProxy,stop) <- forkTerminal term
       finally (runCompileEnv (Env termProxy flags errs vfs) cmp) (stop)


-- Fork off a thread to handle output from other threads so it is properly interleaved
forkTerminal :: Terminal -> IO (Terminal, IO ())
forkTerminal term
  = do ch <- newChan
       forkIO (handleOutput ch)
       let termProxy = Terminal (writeChan ch . Just . termError term)
                                (writeChan ch . Just . termPhase term)
                                (writeChan ch . Just . termPhaseDoc term)
                                (writeChan ch . Just . termType term)
                                (writeChan ch . Just . termDoc term)
       return (termProxy, writeChan ch Nothing)
  where
    handleOutput :: Chan (Maybe (IO ())) -> IO ()
    handleOutput ch
      = do mbf <- readChan ch
           case mbf of
             Nothing -> return ()
             Just io -> do io
                           handleOutput ch



runCompileEnv :: Env -> Compile a -> IO (Either Errors (a,Errors))
runCompileEnv env action
  = case checked action of
      Compile cmp -> cmp env

checked :: Compile a -> Compile (Either Errors (a,Errors))
checked (Compile cmp)
  = Compile (\env -> do res <- do{ x <- cmp env; return (Right x) }
                               `catch` (\errs -> return (Left errs))
                               `catchIO` (\exn -> return $ Left $ errorsSingle $ errorMessageKind ErrBuild rangeNull (text (show exn)))
                        errsw <- readIORef (envErrors env)
                        writeIORef (envErrors env) errorsNil
                        case res of
                          Right x    -> return (Right (x,errsw))
                          Left errs  -> return (Left (mergeErrors errsw errs))
            )

checkedDefault :: a -> Compile a -> Compile (a,Errors)
checkedDefault def action
  = do res <- checked action
       case res of
         Left errs      -> return (def,errs)
         Right (x,errs) -> return (x,errs)

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO io f
  = io `catch` f

liftIO :: IO a -> Compile a
liftIO io = Compile (\env -> io)

liftIOError :: IO (Error () a) -> Compile a
liftIOError io
  = do res <- liftIO io
       case checkError res of
         Left errs -> throw errs
         Right (x,warns)
          -> do addErrors warns
                return x

mapConcurrent :: (a -> Compile b) -> [a] -> Compile [b]
mapConcurrent f xs
  = do env <- getEnv
       ys  <- liftIO $ mapConcurrently (\x -> runCompileEnv env (f x)) xs
       let errs = lefts ys
       if null errs
         then do let (zs,warns) = unzip (rights ys)
                 mapM_ addErrors warns
                 return zs
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

addErrors :: Errors -> Compile ()
addErrors errs0
  = do env <- getEnv
       liftIO $ modifyIORef (envErrors env) (\errs1 -> mergeErrors errs0 errs1)

addWarningMessage :: ErrorMessage -> Compile ()
addWarningMessage warn
  = addErrors (errorsSingle warn)

addErrorMessage :: ErrorMessage -> Compile ()
addErrorMessage err
  = addErrors (errorsSingle err)

addErrorMessages :: [ErrorMessage] -> Compile ()
addErrorMessages errs
  = addErrors (Errors errs)

phaseTimed :: String -> Doc -> Compile a -> Compile a
phaseTimed p doc action
  = do t0 <- liftIO $ getCurrentTime
       phaseVerbose p doc
       x  <- action
       t1 <- liftIO $ getCurrentTime
       phaseVerbose p (doc <.> text ": elapsed:" <+> text (showTimeDiff t1 t0))
       return x


phaseVerbose :: String -> Doc -> Compile ()
phaseVerbose p doc
  = do term <- getTerminal
       cscheme <- getColorScheme
       liftIO $ termPhaseDoc term (color (colorInterpreter cscheme) (text (sfill 8 p ++ ":")) <+> (color (colorSource cscheme) doc))

phase :: String -> Doc -> Compile ()
phase p doc
  = do term <- getTerminal
       cscheme <- getColorScheme
       liftIO $ termPhaseDoc term (color (colorInterpreter cscheme) (text (sfill 8 p ++ ":")) <+> (color (colorSource cscheme) doc))

sfill n s = s ++ replicate (n - length s) ' '


getFileTime :: FilePath -> Compile FileTime
getFileTime "" = return fileTime0
getFileTime fpath0
  = do env <- getEnv
       let fpath = normalize fpath0
       case vfsFind (envVFS env) fpath of
         Just (_, t) -> return t
         Nothing     -> liftIO $ F.getFileTime fpath

maybeGetFileTime :: FilePath -> Compile (Maybe FileTime)
maybeGetFileTime fpath
  = do ft <- getFileTime fpath
       return (if ft == fileTime0 then Nothing else Just ft)


getFileContents :: FilePath -> Compile BString
getFileContents fpath0
  = do env <- getEnv
       let fpath = normalize fpath0
       case vfsFind (envVFS env) fpath of
         Just (content, _) -> return content
         Nothing           -> liftIO $ readInput fpath

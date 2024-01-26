-----------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Compile.Build( Build
                      , VFS(..), noVFS
                      , runBuildIO
                      , modulesResolveDependencies
                      , modulesCompile
                      , modulesTypeCheck
                      , moduleFromSource
                      ) where

import Debug.Trace
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
import Common.Syntax          ( Target(..), isPublic, Visibility(..))
import Common.Error
import Common.File   hiding (getFileTime)
import qualified Common.File as F
import qualified Common.NameMap as M
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
import Compile.Compile        ( typeCheck, compileCore )

modulesCompile :: [Module] -> Build [Module]
modulesCompile roots
  = phaseTimed "compiling" Lib.PPrint.empty $ -- (list (map (pretty . modName) modules))
    do rootsv      <- modulesValidate roots
       modules     <- modulesResolveDependencies rootsv
       tcheckedMap <- modmapCreate modules
       compiledMap <- modmapCreate modules
       compiled    <- mapConcurrent (moduleCoreCompile tcheckedMap compiledMap) modules
       mapM moduleFlushErrors compiled


{---------------------------------------------------------------
  Compile a set of modules
---------------------------------------------------------------}

moduleCoreCompile :: ModuleMap -> ModuleMap -> Module -> Build Module
moduleCoreCompile tcheckedMap compiledMap mod0
  = onBuildException (done mod0) $ -- ensure the mvar is always set so progress is made
    withModule (modName mod0) $
    do mod <- moduleTypeCheck tcheckedMap mod0
       if (modPhase mod < ModTyped || modPhase mod >= ModCoreCompiled)
         then done mod
         else do -- wait for direct (user+pub) imports to be compiled
                 let pubImportNames = map Core.importName (modCoreImports mod)
                 -- phase "compile imports" $ pretty (modName mod) <.> colon <+> list (map pretty pubImportNames)
                 imports <- moduleGetCompilerImports compiledMap [] pubImportNames
                 if any (\m -> modPhase m < ModCoreCompiled) imports
                   then done mod  -- dependencies had errors (todo: we could keep going if the import has (previously computed) core?)
                   else -- core compile
                        do  phase "compile" $ pretty (modName mod) -- <.> text ": imported:" <+> list (map (pretty . modName) imports)
                            flags <- getFlags
                            let defs    = defsFromModules (mod:imports)  -- todo: optimize by reusing the defs from the type check?
                                inlines = inlinesFromModules imports
                            (core,inlineDefs) <- liftError $ compileCore flags (defsNewtypes defs) (defsGamma defs) inlines (fromJust (modCore mod))
                            let mod' = mod{ modPhase = ModCoreCompiled
                                          , modFinalCore = Just core
                                          , modInlines = Right inlineDefs
                                          }
                            -- phaseVerbose "compiled" (pretty (modName mod))
                            done mod'

  where
    done mod' = do modmapPut compiledMap mod'
                   return mod'



-- Import also modules required for checking inlined definitions from direct imports.
moduleGetCompilerImports :: ModuleMap -> [ModuleName] -> [ModuleName] -> Build [Module]
moduleGetCompilerImports compiledMap alreadyDone0 importNames
  = do -- wait for imported modules to be compiled
       imports <- mapM (modmapRead compiledMap) importNames
       let alreadyDone = alreadyDone0 ++ importNames
           extras = nub $ [Core.importName imp | mod <- imports, hasInlines (modInlines mod),
                                                  -- consider all of its imports to ensure we can check its inline definitions
                                                  imp <- modCoreImports mod,
                                                  not (Core.importName imp `elem` alreadyDone)]
       extraImports <- mapM (modmapRead compiledMap) extras
       return (extraImports ++ imports)
  where
    hasInlines (Right []) = False
    hasInlines _          = True


{---------------------------------------------------------------
  Given a set of modules that are in build order,
  type check them all
---------------------------------------------------------------}

type ModuleMap = M.NameMap (MVar Module)

modmapPut :: ModuleMap -> Module -> Build ()
modmapPut modmap mod
  = liftIO $ putMVar ((M.!) modmap (modName mod)) mod

modmapRead :: ModuleMap -> ModuleName -> Build Module
modmapRead modmap modname
  = liftIO $ readMVar ((M.!) modmap modname)

modmapCreate :: [Module] -> Build ModuleMap
modmapCreate modules
   = M.fromList <$> mapM (\mod -> do v <- liftIO $ newEmptyMVar
                                     return (modName mod, v)) modules

modulesTypeCheck :: [Module] -> Build [Module]
modulesTypeCheck modules
  = phaseTimed "type checking" Lib.PPrint.empty $ -- (list (map (pretty . modName) modules))
    do tcheckedMap <- M.fromList <$> mapM (\mod -> do v <- liftIO $ newEmptyMVar
                                                      return (modName mod, v)) modules
       tchecked <- mapConcurrent (moduleTypeCheck tcheckedMap) modules
       mapM moduleFlushErrors tchecked

moduleTypeCheck :: ModuleMap -> Module -> Build Module
moduleTypeCheck tcheckedMap mod
  = onBuildException (modmapPut tcheckedMap mod) $ -- ensure the mvar is always set so progress is made
    withModule (modName mod) $
    if (modPhase mod < ModParsed || modPhase mod >= ModTyped)
      then done mod
      else do -- wait for direct imports to be type checked
              imports <- moduleGetPubImports tcheckedMap [] (modImports mod)
              if any (\m -> modPhase m < ModTyped) imports
                then done mod  -- dependencies had errors (todo: we could keep going if the import has (previously computed) core?)
                else -- type check
                     do flags <- getFlags
                        let defs = defsFromModules imports
                            program = fromJust (modProgram mod)
                            cimports = coreImportsFromModules (modImports mod) (programImports program) imports
                        phase "check" $ pretty (modName mod) -- <.> text ": imported:" <+> list (map (pretty . Core.importName) cimports)
                        (core,mbRangeMap) <- liftError $ typeCheck flags defs cimports program
                        let mod' = mod{ modPhase = ModTyped
                                      , modCore = Just core
                                      , modCoreImports = cimports
                                      , modRangeMap = mbRangeMap
                                      , modDefinitions = Just (defsFromCore core)
                                      }
                        -- phaseVerbose "checked" (pretty (modName mod))
                        done mod'

  where
    done mod' = do modmapPut tcheckedMap mod'
                   return mod'


-- Recursively load public imports from imported modules
moduleGetPubImports :: ModuleMap -> [ModuleName] -> [ModuleName] -> Build [Module]
moduleGetPubImports tcheckedMap alreadyDone0 importNames
  = do -- wait for imported modules to be type checked
       imports <- mapM (modmapRead tcheckedMap) importNames
       let alreadyDone = alreadyDone0 ++ importNames
           extras = nub $ [Core.importName imp | mod <- imports, imp <- modCoreImports mod,
                                                  isPublic (Core.importVis imp),  -- never ImportCompiler
                                                  not (Core.importName imp `elem` alreadyDone)]
       if null extras
         then return imports
         else do extraImports <- moduleGetPubImports tcheckedMap alreadyDone extras
                 return (extraImports ++ imports)

-- Return all (user and pub) core imports for a list of user imported modules;
-- - needs the original imports as well to determine provenance
-- - needs the program imports as well to determine visibility
coreImportsFromModules :: [ModuleName] -> [Import] -> [Module] -> [Core.Import]
coreImportsFromModules userImports progImports modules
  = [Core.Import (modName mod) ""
      (getProvenance (modName mod))
      (getVisibility (modName mod))
      (fromMaybe "" (fmap Core.coreProgDoc (modCore mod)))
    | mod <- modules ]
  where
    getVisibility modname
      = case find (\imp -> importFullName imp == modname) progImports of
          Just imp -> importVis imp  -- todo: get max?
          _        -> Private

    getProvenance modname
      = if modname `elem` userImports then Core.ImportUser else Core.ImportPub



{---------------------------------------------------------------
  Given a set of modules,
  return all required modules in build order
---------------------------------------------------------------}

-- given a root set of modules, load- or parse all required modules
modulesResolveDependencies :: [Module] -> Build [Module]
modulesResolveDependencies modules
  = do ordered <- phaseTimed "resolving" (list (map (pretty . modName) modules)) $
                  modulesResolveDeps modules
       mapM moduleFlushErrors ordered

modulesResolveDeps modules
  = do pmodules   <- mapConcurrent ensureLoaded modules           -- we can concurrently load/parse modules
       newimports <- nubBy (\m1 m2 -> modName m1 == modName m2) <$> concat <$> mapM (addImports pmodules) pmodules
       if (null newimports)
         then toBuildOrder pmodules
         else do phaseVerbose "resolve" (list (map (pretty . modName) newimports))
                 modulesResolveDeps (newimports ++ pmodules)
  where
    addImports pmodules mod
      = concat <$> mapM addImport (modImports mod)
      where
        addImport :: ModuleName -> Build [Module]
        addImport impName = if any (\m -> modName m == impName) pmodules
                             then return []
                             else do let relativeDir = dirname (modSourcePath mod)
                                     m <- moduleFromModuleName relativeDir impName  -- todo: set error, prevent parsing?
                                     return [m]

-- order the loaded modules in build order (by using scc)
toBuildOrder :: [Module] -> Build [Module]
toBuildOrder modules
  = -- todo: might be faster to check if the modules are already in build order before doing a full `scc` ?
    let deps    = [(modName mod, modImports mod) | mod <- modules]
        ordered = scc deps
        ungroup [mname]  | Just mod <- find (\m -> modName m == mname) modules  = return [mod]
        ungroup grp      = do throwError (errorMessageKind ErrBuild rangeNull (text ("recursive imports: " ++ show grp))) -- todo: nice error
                              return []
    in do phaseVerbose "build order" (list (map (\grp -> hsep (map (pretty) grp)) ordered))
          concat <$> mapM ungroup ordered


moduleFlushErrors :: Module -> Build Module
moduleFlushErrors mod
  = do addErrors (modErrors mod)
       return mod{ modErrors = errorsNil }

{---------------------------------------------------------------
  Parse modules from source, or load from an interface file
  After this, `modImports` should be valid
---------------------------------------------------------------}

ensureLoaded :: Module -> Build Module
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

moduleParse :: Module -> Build Module
moduleParse mod
  = do phase "parsing" (text (modSourceRelativePath mod))
       flags <- getFlags
       let allowAt = isPrimitiveModule (modName mod)
       prog <- liftIOError $ parseProgramFromFile allowAt (semiInsert flags) (modSourcePath mod)
       return mod{ modPhase = ModParsed
                 , modLexemes = programLexemes prog
                 , modProgram = Just $! prog
                 , modImports = nub (map importFullName (programImports prog))
                 }

moduleLoadIface :: Module -> Build Module
moduleLoadIface mod
  = do phase "loading" (text (modIfacePath mod))
       (core,parseInlines) <- liftIOError $ parseCore (modIfacePath mod) (modSourcePath mod)
       return mod{ modPhase   = ModCompiled
                 , modImports = map Core.importName (filter (not . Core.isCompilerImport) (Core.coreProgImports core))
                 , modCore    = Just $! core
                 , modDefinitions = Just $! (defsFromCore core)
                 , modInlines = case parseInlines of
                                  Nothing -> Right []
                                  Just f  -> Left f
                 }

moduleLoadLibIface :: Module -> Build Module
moduleLoadLibIface mod
  = do phaseVerbose "loading" (text (modLibIfacePath mod))
       (core,parseInlines) <- liftIOError $ parseCore (modLibIfacePath mod) (modSourcePath mod)
       flags <- getFlags
       liftIO $ copyLibIfaceToOutput flags (modLibIfacePath mod) (modIfacePath mod) core
       return mod{ modPhase   = ModCompiled
                 , modImports = map Core.importName  (filter (not . Core.isCompilerImport) (Core.coreProgImports core))
                 , modCore    = Just $! core
                 , modDefinitions = Just $! (defsFromCore core)
                 , modInlines = case parseInlines of
                                  Nothing -> Right []
                                  Just f  -> Left f
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

moduleFromSource :: FilePath -> Build Module
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
                                moduleValidate $ (moduleCreateInitial modName sourcePath ifacePath ""){ modSourceRelativePath = stem }
  where
    isValidId :: String -> Bool  -- todo: make it better
    isValidId ""      = False
    isValidId (c:cs)  = isLower c && all (\c -> isAlphaNum c || c `elem` "_-") cs


moduleFromModuleName :: FilePath -> Name -> Build Module
moduleFromModuleName relativeDir modName
  = do mbSourceName <- searchSourceFile relativeDir (nameToPath modName ++ sourceExtension)
       ifacePath    <- outputName (moduleNameToPath modName ++ ifaceExtension)
       libIfacePath <- searchLibIfaceFile (moduleNameToPath modName ++ ifaceExtension)
       case mbSourceName of
         Just (root,stem)
            -> moduleValidate $ (moduleCreateInitial modName (joinPath root stem) ifacePath libIfacePath){ modSourceRelativePath = stem }
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
searchSourceFile :: FilePath -> FilePath -> Build (Maybe (FilePath,FilePath))
searchSourceFile relativeDir fname
  = do -- trace ("search source: " ++ fname ++ " from " ++ concat (intersperse ", " (relativeDir:includePath flags))) $ return ()
       flags <- getFlags
       liftIO $ searchPathsCanonical relativeDir (includePath flags) [sourceExtension,sourceExtension++".md"] [] fname

-- find a pre-compiled libary interface
-- in the future we can support packages as well
searchLibIfaceFile :: FilePath -> Build FilePath  -- can be empty
searchLibIfaceFile fname
  = do flags <- getFlags
       let libIfacePath = joinPaths [localLibDir flags, buildVariant flags, fname]
       exist <- liftIO $ doesFileExist libIfacePath
       return (if exist then libIfacePath else "")


{---------------------------------------------------------------
  Validate if modules are still valid
---------------------------------------------------------------}

modulesValidate :: [Module] -> Build [Module]
modulesValidate modules
  = mapM moduleValidate modules

moduleValidate :: Module -> Build Module
moduleValidate mod
  = do flags <- getFlags
       ftSource   <- getFileTime (modSourcePath mod)
       ftIface    <- getFileTime (modIfacePath mod)
       ftLibIface <- getFileTime (modLibIfacePath mod)
       let stale = (ftSource > modSourceTime mod || ftIface > modIfaceTime mod || ftLibIface > modLibIfaceTime mod)
       let mod' = mod{ modSourceTime = ftSource
                     , modIfaceTime  = ftIface
                     , modLibIfaceTime = ftLibIface
                     }
       -- phaseVerbose "trace" (pretty (modName mod) <.> text (": times: " ++ show (ftSource,ftIface)))
       if (stale || rebuild flags || forceModule flags == modSourcePath mod)
         then return mod'{ modPhase = ModInit, modErrors = errorsNil,
                           -- reset fields that are not used by an IDE to reduce memory pressure
                           -- leave lexemes and definitions
                           modFinalCore = Nothing, modInlines = Right [],
                           modCore = Nothing, modProgram = Nothing
                         }
         else return mod'



{---------------------------------------------------------------
  Helpers
---------------------------------------------------------------}

throwModuleNotFound :: Range -> Name -> Build a
throwModuleNotFound range name
  = do flags <- getFlags
       throwError (errorMessageKind ErrBuild range (errorNotFound flags colorModule "module" (pretty name)))

throwFileNotFound :: FilePath -> Build a
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

outputName :: FilePath -> Build FilePath
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

data Build a = Build (Env -> IO a)

data Env = Env { envTerminal :: Terminal,
                 envFlags    :: Flags,
                 envErrors   :: IORef Errors,
                 envRange    :: IORef Range,
                 envVFS      :: VFS }

noVFS :: VFS
noVFS = VFS (\fpath -> Nothing)

maxErrors :: Int
maxErrors = 25

runBuildIO :: Terminal -> Flags -> VFS -> Build a -> IO (Maybe a)
runBuildIO term flags vfs cmp
  = do res <- runBuild term flags vfs cmp
       case res of
         Right (x,errs) -> do mapM_ (termError term) (take maxErrors (errors errs))
                              return (Just x)
         Left errs      -> do mapM_ (termError term) (take maxErrors (errors errs))
                              return Nothing

runBuild :: Terminal -> Flags -> VFS -> Build a -> IO (Either Errors (a,Errors))
runBuild term flags vfs cmp
  = do errs <- newIORef errorsNil
       rng  <- newIORef rangeNull
       (termProxy,stop) <- forkTerminal term
       finally (runBuildEnv (Env termProxy flags errs rng vfs) cmp) (stop)


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



runBuildEnv :: Env -> Build a -> IO (Either Errors (a,Errors))
runBuildEnv env action
  = case checked action of
      Build cmp -> cmp env


checked :: Build a -> Build (Either Errors (a,Errors))
checked (Build cmp)
  = Build   (\env -> do res <- do{ x <- cmp env; return (Right x) }
                               `catch` (\errs -> return (Left errs))
                               `catchError` (\err -> makeErr env ErrInternal (show err))
                               `catchIO` (\exn -> makeErr env ErrBuild (show exn))
                        errsw <- readIORef (envErrors env)
                        writeIORef (envErrors env) errorsNil
                        case res of
                          Right x    -> return (Right (x,errsw))
                          Left errs  -> return (Left (mergeErrors errsw errs))
            )
  where
    makeErr env errKind msg
      = do rng <- readIORef (envRange env)
           return (Left (errorsSingle (errorMessageKind errKind rng (text msg))))

checkedDefault :: a -> Build a -> Build (a,Errors)
checkedDefault def action
  = do res <- checked action
       case res of
         Left errs      -> return (def,errs)
         Right (x,errs) -> return (x,errs)


catchError :: IO a -> (ErrorCall -> IO a) -> IO a
catchError io f
  = io `catch` f


catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO io f
  = io `catch` f

liftIO :: IO a -> Build a
liftIO io = Build (\env -> io)

liftIOError :: IO (Error () a) -> Build a
liftIOError io
  = do res <- liftIO io
       liftError res

liftError :: Error () a -> Build a
liftError res
  = case checkError res of
      Left errs       -> throw errs
      Right (x,warns) -> do addErrors warns
                            return x

mapConcurrent :: (a -> Build b) -> [a] -> Build [b]
mapConcurrent f xs
  = do env <- getEnv
       ys  <- liftIO $ mapConcurrently (\x -> runBuildEnv env (f x)) xs
       let errs = lefts ys
       if null errs
         then do let (zs,warns) = unzip (rights ys)
                 mapM_ addErrors warns
                 return zs
         else throw (foldr mergeErrors errorsNil errs)

instance Functor Build where
  fmap f (Build ie)  = Build (\env -> fmap f (ie env))

instance Applicative Build where
  pure x = Build (\env -> return x)
  (<*>)  = ap

instance Monad Build where
  -- return = pure
  (Build ie) >>= f
    = Build (\env -> do x <- ie env
                        case (f x) of
                          Build ie' -> ie' env)

instance F.MonadFail Build where
  fail msg = throwError (errorMessageKind ErrGeneral rangeNull (text msg))

onBuildException :: Build b -> Build a -> Build a
onBuildException (Build onExn) (Build b)
  = Build (\env -> (b env) `onException` (onExn env))

buildFinally :: Build a -> Build () -> Build a
buildFinally (Build b) (Build fin)
  = Build (\env -> finally (b env) (fin env))

throwError :: ErrorMessage -> Build a
throwError msg
  = liftIO $ throw (errorsSingle msg)

getEnv :: Build Env
getEnv
  = Build (\env -> return env)

getFlags :: Build Flags
getFlags
  = Build (\env -> return (envFlags env))

getTerminal :: Build Terminal
getTerminal
  = Build (\env -> return (envTerminal env))

getColorScheme :: Build ColorScheme
getColorScheme
  = do flags <- getFlags
       return (colorSchemeFromFlags flags)

addErrors :: Errors -> Build ()
addErrors errs0
  = do env <- getEnv
       liftIO $ modifyIORef (envErrors env) (\errs1 -> mergeErrors errs0 errs1)

addWarningMessage :: ErrorMessage -> Build ()
addWarningMessage warn
  = addErrors (errorsSingle warn)

addErrorMessage :: ErrorMessage -> Build ()
addErrorMessage err
  = addErrors (errorsSingle err)

addErrorMessages :: [ErrorMessage] -> Build ()
addErrorMessages errs
  = addErrors (Errors errs)

phaseTimed :: String -> Doc -> Build a -> Build a
phaseTimed p doc action
  = do t0 <- liftIO $ getCurrentTime
       phaseVerbose p doc
       buildFinally action $ do t1 <- liftIO $ getCurrentTime
                                phaseVerbose p (text "elapsed:" <+> text (showTimeDiff t1 t0))


phaseVerbose :: String -> Doc -> Build ()
phaseVerbose p doc
  = do term <- getTerminal
       cscheme <- getColorScheme
       liftIO $ termPhaseDoc term (color (colorInterpreter cscheme) (text (sfill 8 p ++ ":")) <+> (color (colorSource cscheme) doc))

phase :: String -> Doc -> Build ()
phase p doc
  = do term <- getTerminal
       cscheme <- getColorScheme
       liftIO $ termPhaseDoc term (color (colorInterpreter cscheme) (text (sfill 8 p ++ ":")) <+> (color (colorSource cscheme) doc))

sfill n s = s ++ replicate (n - length s) ' '


getFileTime :: FilePath -> Build FileTime
getFileTime "" = return fileTime0
getFileTime fpath0
  = do env <- getEnv
       let fpath = normalize fpath0
       case vfsFind (envVFS env) fpath of
         Just (_, t) -> return t
         Nothing     -> liftIO $ F.getFileTime fpath

maybeGetFileTime :: FilePath -> Build (Maybe FileTime)
maybeGetFileTime fpath
  = do ft <- getFileTime fpath
       return (if ft == fileTime0 then Nothing else Just ft)


getFileContents :: FilePath -> Build BString
getFileContents fpath0
  = do env <- getEnv
       let fpath = normalize fpath0
       case vfsFind (envVFS env) fpath of
         Just (content, _) -> return content
         Nothing           -> liftIO $ readInput fpath

getCurrentRange :: Build Range
getCurrentRange
  = do env <- getEnv
       liftIO $ readIORef (envRange env)

withModule :: ModuleName -> Build a -> Build a
withModule mname action
  = do env <- getEnv
       old <- liftIO $ readIORef (envRange env)
       liftIO $ writeIORef (envRange env) (makeSourceRange (show mname) 1 1 1 1)
       x <- action -- do not use finally so the error uses the range when it was thrown
       liftIO $ writeIORef (envRange env) old
       return x
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
import Type.Assumption        ( Gamma )
import qualified Core.Core as Core
import Core.Parse
import Compiler.Options
import Compile.Module
import Compile.TypeCheck      ( typeCheck )
import Compile.Optimize       ( coreOptimize )
import Compile.CodeGen        ( codeGen, Link, LinkResult(..), noLink )

{---------------------------------------------------------------
  Concurrently compile a list of root modules.
  Returns all required modules as compiled in build order
---------------------------------------------------------------}
modulesCompile :: [Module] -> Build [Module]
modulesCompile roots
  = phaseTimed "compiling" Lib.PPrint.empty $ -- (list (map (pretty . modName) modules))
    do rootsv      <- modulesValidate roots
       modules     <- modulesResolveDependencies rootsv
       tcheckedMap <- modmapCreate modules
       compiledMap <- modmapCreate modules
       codegenMap  <- modmapCreate modules
       linkedMap   <- modmapCreate modules
       compiled    <- mapConcurrentModules (moduleCompile tcheckedMap compiledMap codegenMap linkedMap) modules
       mapM moduleFlushErrors compiled

{---------------------------------------------------------------
  Module map
  We build concurrently using `mapConcurrentModules`

  Each module has 4 compilation phases that can be done concurrently with others. The 3 module maps
  (tcheckedMap, compiledMap, and codegenMap) synchronize between these phases through mvar's.

  - resolve     : Parse each module, or load it from a previously compiled interface file.
                  This can be completely concurrent, but each load leads to more imports that are resolved
                  concurrently in a fixpoint.
                  This produces the user program (`modProgram`), the lexemes (`modLexemes`) and
                  a rangemap (`modRangeMap`).

  - type check  : As soon as the user+pub imports are type checked, a module can be checked as well.
                  This produces the initial core (`modCore`) and the "definitions" (`modDefinitions`)
                  that contain the gamma, kind gamma, etc, and the user+pub imports (`modCoreImports`).

  - core compile: As soon as the user+pub imports, and the imports due to inline definitions are compiled,
                  the core of can be compiled and optimized as well (and use exported inline definitions)
                  This gives final core (`modFinalCore`) and exported inline definitions (`modInlines`).

  - codegen     : As soon as all imports are codegen'd,
                  the compiled core can be backend compiled (as it depends on the other's header files, libs, etc).
                  This produces an interface file (.kki) and compiled backend object files.
---------------------------------------------------------------}

type ModuleMap = M.NameMap (MVar Module)

-- signal a module is done with a compilation phase and unblock all pending reads.
modmapPut :: ModuleMap -> Module -> Build ()
modmapPut modmap mod
  = liftIO $ putMVar ((M.!) modmap (modName mod)) mod

-- blocks until a `modmapPut` happens (at which point it returns the module definition at that phase).
-- (all further reads are non-blocking)
modmapRead :: ModuleMap -> ModuleName -> Build Module
modmapRead modmap modname
  = liftIO $ readMVar ((M.!) modmap modname)

-- create an initial module map
modmapCreate :: [Module] -> Build ModuleMap
modmapCreate modules
   = M.fromList <$> mapM (\mod -> do v <- liftIO $ newEmptyMVar
                                     return (modName mod, v)) modules



{---------------------------------------------------------------
  Compile a module (type check, core compile, codegen, and link)
---------------------------------------------------------------}

moduleCompile :: ModuleMap -> ModuleMap -> ModuleMap -> ModuleMap -> Module -> Build Module
moduleCompile tcheckedMap compiledMap codegenMap linkedMap mod0
  = onBuildException (done mod0) $ -- ensure the mvar is always set so progress is made
    do (link,mod) <- moduleCodeGen tcheckedMap compiledMap codegenMap mod0
       if (modPhase mod < ModTyped || modPhase mod >= ModLinked)
         then done mod
         else do -- wait for all required imports to be linked (todo: only needed for the final exe?)
                 let pubImportNames = [] -- map Core.importName (modCoreImports mod)
                 imports <- moduleGetCompilerImports linkedMap [] pubImportNames
                 if any (\m -> modPhase m < ModLinked) imports
                   then done mod  -- dependencies had errors (todo: we could keep going if the import has (previously computed) core?)
                   else do mbEntry <- liftIO $ link   -- link it!
                           let mod' = mod{ modPhase = ModLinked }
                           done mod'

  where
    done mod' = do modmapPut linkedMap mod'
                   return mod'


{---------------------------------------------------------------
  Code generation (.c,.js)
---------------------------------------------------------------}

moduleCodeGen :: ModuleMap -> ModuleMap -> ModuleMap -> Module -> Build (Link, Module)
moduleCodeGen tcheckedMap compiledMap codegenMap mod0
  = onBuildException (done noLink mod0) $ -- ensure the mvar is always set so progress is made
    do mod <- moduleOptimize tcheckedMap compiledMap mod0
       if (modPhase mod < ModTyped || modPhase mod >= ModCodeGen)
         then done noLink mod
         else do -- wait for all required imports to be fully compiled
                 let pubImportNames = map Core.importName (modCoreImports mod)
                 imports <- moduleGetCompilerImports codegenMap [] pubImportNames
                 if any (\m -> modPhase m < ModCodeGen) imports
                   then done noLink mod  -- dependencies had errors (todo: we could keep going if the import has (previously computed) core?)
                   else codegen imports mod

  where
    done link mod' = do modmapPut codegenMap mod'
                        return (link,mod')

    codegen :: [Module] -> Module -> Build (Link,Module)
    codegen imports mod
      =  do phase "codegen" $ pretty (modName mod) -- <.> text ": imported:" <+> list (map (pretty . modName) imports)
            flags <- getFlags
            term  <- getTerminal
            let defs    = defsFromModules (mod:imports)  -- todo: optimize by reusing the defs from the compile?
                inlines = inlinesFromModules imports
            link <- liftIO $ codeGen term flags (defsNewtypes defs) (defsBorrowed defs) (defsKGamma defs) (defsGamma defs)
                              Nothing {-mbentry-} imports mod
            let mod' = mod{ modPhase = ModCodeGen }
            phaseVerbose 2 "codegen done" (pretty (modName mod))
            done link mod'
            -- and now link it


{---------------------------------------------------------------
  Core optimize a module
  (not just optimization, some transformations are essential
  like perceus ref counting etc.)
---------------------------------------------------------------}

moduleOptimize :: ModuleMap -> ModuleMap -> Module -> Build Module
moduleOptimize tcheckedMap compiledMap mod0
  = onBuildException (done mod0) $ -- ensure the mvar is always set so progress is made
    do mod <- moduleTypeCheck tcheckedMap mod0
       if (modPhase mod < ModTyped || modPhase mod >= ModOptimized)
         then done mod
         else do -- wait for direct (user+pub) imports to be compiled
                 let pubImportNames = map Core.importName (modCoreImports mod)
                 -- phase "compile imports" $ pretty (modName mod) <.> colon <+> list (map pretty pubImportNames)
                 imports <- moduleGetCompilerImports compiledMap [] pubImportNames
                 if any (\m -> modPhase m < ModOptimized) imports
                   then done mod  -- dependencies had errors (todo: we could keep going if the import has (previously computed) core?)
                   else -- core compile
                        do  phase "compile" $ pretty (modName mod) -- <.> text ": imported:" <+> list (map (pretty . modName) imports)
                            flags <- getFlags
                            let defs    = defsFromModules (mod:imports)  -- todo: optimize by reusing the defs from the type check?
                                inlines = inlinesFromModules imports
                            (core,inlineDefs) <- liftError $ coreOptimize flags (defsNewtypes defs) (defsGamma defs) inlines (fromJust (modCore mod))
                            let mod' = mod{ modPhase   = ModOptimized
                                          , modCore    = Just $! core
                                          , modInlines = Right $! inlineDefs
                                          }
                            phaseVerbose 2 "compile done" (pretty (modName mod))
                            done mod'

  where
    done mod' = do modmapPut compiledMap mod'
                   return mod'



-- Import also modules required for checking inlined definitions from direct imports.
moduleGetCompilerImports :: ModuleMap -> [ModuleName] -> [ModuleName] -> Build [Module]
moduleGetCompilerImports modmap alreadyDone0 importNames
  = do -- wait for imported modules to be compiled
       imports <- mapM (modmapRead modmap) importNames
       let alreadyDone = alreadyDone0 ++ importNames
           extras = nub $ [Core.importName imp | mod <- imports, hasInlines (modInlines mod),
                                                  -- consider all of its imports to ensure we can check its inline definitions
                                                  imp <- modCoreImports mod,
                                                  not (Core.importName imp `elem` alreadyDone)]
       extraImports <- mapM (modmapRead modmap) extras
       return (extraImports ++ imports)
  where
    hasInlines (Right []) = False
    hasInlines _          = True


{---------------------------------------------------------------
  Type check a module
---------------------------------------------------------------}

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
    if (modPhase mod < ModParsed || modPhase mod >= ModTyped)
      then done mod
      else do -- wait for direct imports to be type checked
              imports <- moduleGetPubImports tcheckedMap [] (modDeps mod)
              if any (\m -> modPhase m < ModTyped) imports
                then done mod  -- dependencies had errors (todo: we could keep going if the import has (previously computed) core?)
                else -- type check
                     do flags <- getFlags
                        let defs = defsFromModules imports
                            program = fromJust (modProgram mod)
                            cimports = coreImportsFromModules (modDeps mod) (programImports program) imports
                        phase "check" $ pretty (modName mod) -- <.> text ": imported:" <+> list (map (pretty . Core.importName) cimports)
                        (core,mbRangeMap) <- liftError $ typeCheck flags defs cimports program
                        -- let diff = [name | name <- map Core.importName (Core.coreProgImports core), not (name `elem` map Core.importName cimports)]
                        -- when (not (null diff)) $
                        --   phaseVerbose 1 "checked" $ pretty (modName mod) <.> text ": EXTRA imported:" <+> list (map pretty diff)
                        let mod' = mod{ modPhase = ModTyped
                                      , modCore = Just $! core
                                      , modRangeMap = mbRangeMap
                                      , modDefinitions = Just $! (defsFromCore core)
                                      }
                        phaseVerbose 2 "check done" (pretty (modName mod))
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
                                                  isPublic (Core.importVis imp),
                                                  not (Core.isCompilerImport imp),
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
  = do ordered <- -- phaseTimed "resolving" (list (map (pretty . modName) modules)) $
                  modulesResolveDeps modules
       mapM moduleFlushErrors ordered

modulesResolveDeps modules
  = do pmodules   <- mapConcurrentModules moduleLoad modules           -- we can concurrently load/parse modules
       newimports <- nubBy (\m1 m2 -> modName m1 == modName m2) <$> concat <$> mapM (addImports pmodules) pmodules
       if (null newimports)
         then toBuildOrder pmodules
         else do phaseVerbose 2 "resolve" (list (map (pretty . modName) newimports))
                 modulesResolveDeps (newimports ++ pmodules)
  where
    addImports pmodules mod
      = concat <$> mapM addImport (modDeps mod)
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
    let deps    = [(modName mod, modDeps mod) | mod <- modules]
        ordered = scc deps
        ungroup [mname]  | Just mod <- find (\m -> modName m == mname) modules  = return [mod]
        ungroup grp      = do throwError (errorMessageKind ErrBuild rangeNull (text ("recursive imports: " ++ show grp))) -- todo: nice error
                              return []
    in do -- phaseVerbose "build order" (list (map (\grp -> hsep (map (pretty) grp)) ordered))
          concat <$> mapM ungroup ordered


moduleFlushErrors :: Module -> Build Module
moduleFlushErrors mod
  = do addErrors (modErrors mod)
       return mod{ modErrors = errorsNil }


{---------------------------------------------------------------
  Parse modules from source, or load from an interface file
  After this, `modDeps` should be valid
---------------------------------------------------------------}

moduleLoad :: Module -> Build Module
moduleLoad mod
  = if modPhase mod >= ModLoaded
      then return mod
      else do (mod',errs) <- checkedDefault mod $ -- on error, return the original module
                             if not (null (modLibIfacePath mod)) && modLibIfaceTime mod > modIfaceTime mod
                               then moduleLoadLibIface mod
                               else if (modSourceTime mod > modIfaceTime mod)
                                 then moduleParse mod
                                 else moduleLoadIface mod
              return mod'{ modErrors = mergeErrors errs (modErrors mod') }

moduleParse :: Module -> Build Module
moduleParse mod
  = do phase "parse" (text (modSourcePath mod))
       flags <- getFlags
       let allowAt = isPrimitiveModule (modName mod)
       prog <- liftIOError $ parseProgramFromFile allowAt (semiInsert flags) (modSourcePath mod)
       return mod{ modPhase = ModParsed
                 , modLexemes = programLexemes prog
                 , modProgram = Just $! prog{ programName = modName mod }  -- todo: test suffix!
                 , modDeps = nub (map importFullName (programImports prog))
                 }

moduleLoadIface :: Module -> Build Module
moduleLoadIface mod
  = do phase "load" (pretty (modName mod))
       (core,parseInlines) <- liftIOError $ parseCore (modIfacePath mod) (modSourcePath mod)
       return (modFromIface core parseInlines mod)

moduleLoadLibIface :: Module -> Build Module
moduleLoadLibIface mod
  = do cscheme <- getColorScheme
       phase "load" (pretty (modName mod) <+> color (colorInterpreter cscheme) (text "from") <+> text (modLibIfacePath mod))
       (core,parseInlines) <- liftIOError $ parseCore (modLibIfacePath mod) (modSourcePath mod)
       flags <- getFlags
       liftIO $ copyLibIfaceToOutput flags (modLibIfacePath mod) (modIfacePath mod) core
       return (modFromIface core parseInlines mod)

modFromIface :: Core.Core -> Maybe (Gamma -> Error () [Core.InlineDef]) -> Module -> Module
modFromIface core parseInlines mod
  =  mod{ modPhase       = ModLinked
        , modDeps        = map Core.importName (filter (not . Core.isCompilerImport) (Core.coreProgImports core))
        , modCore        = Just $! core
        , modDefinitions = Just $! (defsFromCore core)
        , modInlines     = case parseInlines of
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
  = do flags    <- getFlags
       ftSource <- getFileTime (modSourcePath mod)
       (stale,mod') <- if (rebuild flags || forceModule flags == modSourcePath mod || forceModule flags == moduleNameToPath (modName mod))
                         then return (True,  mod{ modSourceTime = ftSource
                                                , modIfaceTime = fileTime0
                                                , modLibIfaceTime = fileTime0
                                                })
                         else  do ftIface    <- getFileTime (modIfacePath mod)
                                  ftLibIface <- getFileTime (modLibIfacePath mod)
                                  let stale = (ftSource > modSourceTime mod ||
                                               ftIface > modIfaceTime mod ||
                                               ftLibIface > modLibIfaceTime mod)
                                  return (stale, mod{ modSourceTime = ftSource
                                                    , modIfaceTime  = ftIface
                                                    , modLibIfaceTime = ftLibIface
                                                    })
       -- phaseVerbose "trace" (pretty (modName mod) <.> text (": times: " ++ show (ftSource,ftIface)))
       if stale
         then return mod'{ modPhase = ModInit, modErrors = errorsNil,
                           -- reset fields that are not used by an IDE to reduce memory pressure
                           -- leave lexemes, rangeMap, and definitions.
                           modProgram = Nothing,
                           modCore = Nothing,
                           modInlines = Right []
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
  = do res <- runBuild term flags{rebuild=True} vfs cmp
       case res of
         Right (x,errs) -> do mapM_ (termError term) (take maxErrors (errors errs))
                              return (Just x)
         Left errs      -> do mapM_ (termError term) (take maxErrors (errors errs))
                              return Nothing

runBuild :: Terminal -> Flags -> VFS -> Build a -> IO (Either Errors (a,Errors))
runBuild term flags vfs cmp
  = do errs <- newIORef errorsNil
       rng  <- newIORef rangeNull
       termProxyDone <- newEmptyMVar
       (termProxy,stop) <- forkTerminal term termProxyDone
       finally (runBuildEnv (Env termProxy flags errs rng vfs) cmp)
               (do stop
                   readMVar termProxyDone)


-- Fork off a thread to handle output from other threads so it is properly interleaved
forkTerminal :: Terminal -> MVar () -> IO (Terminal, IO ())
forkTerminal term termProxyDone
  = do ch <- newChan
       forkIO (handleOutput ch `finally` putMVar termProxyDone ())
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
             Nothing -> do return ()
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

-- map concurrently and merge (and rethrow) all errors
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

-- map concurrently and keep errors in the processed module
mapConcurrentModules :: (Module -> Build Module) -> [Module] -> Build [Module]
mapConcurrentModules f modules
  = mapConcurrent (\mod -> withCheckedModule mod (f mod)) modules


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
       phaseVerbose 1 p doc
       buildFinally action $ do t1 <- liftIO $ getCurrentTime
                                phaseVerbose 1 p (text "elapsed:" <+> text (showTimeDiff t1 t0) <.> linebreak)


phaseVerbose :: Int -> String -> Doc -> Build ()
phaseVerbose vlevel p doc
  = do flags <- getFlags
       when (verbose flags >= vlevel) $
         do term <- getTerminal
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

-- attribute exceptions to a certain module
withModuleName :: ModuleName -> Build a -> Build a
withModuleName mname action
  = do env <- getEnv
       old <- liftIO $ readIORef (envRange env)
       liftIO $ writeIORef (envRange env) (makeSourceRange (show mname) 1 1 1 1)
       x   <- action
       liftIO $ writeIORef (envRange env) old  -- do not use finally so any errors use the range as it was thrown
       return x

-- catch all errors and keep to them to the module
withCheckedModule :: Module -> Build Module -> Build Module
withCheckedModule mod action
  = withModuleName (modName mod) $
    do res <- checked action
       case res of
         Left errs          -> return mod{ modErrors = mergeErrors errs (modErrors mod) }
         Right (mod',warns) -> return mod'{ modErrors = mergeErrors warns (modErrors mod') }
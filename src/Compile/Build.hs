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

                      , modulesFullBuild
                      , modulesBuild
                      , modulesTypeCheck

                      , modulesResolveDependencies
                      , modulesReValidate
                      , moduleFromSource, moduleFromModuleName
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
import Control.Concurrent.QSem
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
import Syntax.Parse           (parseProgramFromString)
import Type.Type
import Type.Assumption        ( Gamma, gammaLookupQ, NameInfo(..) )
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
modulesFullBuild :: VFS -> Bool -> [ModuleName] -> [Name] -> [Module] -> [Module] -> Build [Module]
modulesFullBuild vfs rebuild forced mainEntries cachedImports roots
  = do modules <- modulesReValidate vfs rebuild forced cachedImports roots
       modulesBuild mainEntries modules

modulesFlushErrors :: [Module] -> Build [Module]
modulesFlushErrors modules
  = mapM moduleFlushErrors modules


modulesBuild :: [Name] -> [Module] -> Build [Module]
modulesBuild mainEntries modules
  = phaseTimed "build" Lib.PPrint.empty $ -- (list (map (pretty . modName) modules))
    do tcheckedMap <- modmapCreate modules
       optimizedMap<- modmapCreate modules
       codegenMap  <- modmapCreate modules
       linkedMap   <- modmapCreate modules
       compiled    <- mapConcurrentModules
                       (moduleCompile mainEntries tcheckedMap optimizedMap codegenMap linkedMap)
                       modules
       modulesFlushErrors compiled

modulesTypeCheck :: [Module] -> Build [Module]
modulesTypeCheck modules
  = phaseTimed "check" Lib.PPrint.empty $
    do tcheckedMap <- modmapCreate modules
       tchecked    <- mapConcurrentModules (moduleTypeCheck tcheckedMap) modules
       modulesFlushErrors tchecked

modulesReValidate :: VFS -> Bool -> [ModuleName] -> [Module] -> [Module] -> Build [Module]
modulesReValidate vfs rebuild forced cachedImports roots
  = phaseTimed "resolve" Lib.PPrint.empty $
    let rootNames = map modName roots in seqList rootNames $
    do rootsv   <- modulesValidate vfs roots
       resolved <- modulesResolveDependencies vfs rebuild forced cachedImports rootsv
       modulesFlushErrors resolved


{---------------------------------------------------------------
  Module map
  We build concurrently using `mapConcurrentModules`

  Each module has 5 compilation phases that can be done concurrently with others. The 5 module maps
  (tcheckedMap, optimizedMap, codegenMap, and linkedMap) synchronize between these phases through mvar's.

  - resolve     : Parse each module, or load it from a previously compiled interface file.
                  This can be completely concurrent, but each load leads to more imports that are resolved
                  concurrently in a fixpoint.
                  This produces the user program (`modProgram`), the lexemes (`modLexemes`) and
                  a rangemap (`modRangeMap`).

  - type check  : As soon as the user+pub imports are type checked, a module can be checked as well.
                  This produces the initial core (`modCore`) and the "definitions" (`modDefinitions`)
                  that contain the gamma, kind gamma, etc.

  - optimize    : As soon as the user+pub imports, and the imports due to inline definitions are optimized,
                  the initial core be optimized as well (and use exported inline definitions)
                  This gives final core (updated `modCore`) and exported inline definitions (`modInlines`).
                  Note: optimization cannot be skipped as it also includes essential transformations
                  (like monadic lifting for effectful code).

  - codegen     : As soon as all imports are optimized, (no need to wait for the imports to be codegen'd!)
                  the optimized core can be translated to the backend (.c, .js, etc)

  - link        : As soon as all imports are codegen'd, the generated backend files can
                  be compiled/linked into object files (as they depend on the other's header files, etc).
                  An exe needs to wait until all imports are _linked_ though.
                  This produces an interface file (.kki) and backend object and executable files.
---------------------------------------------------------------}

type ModuleMap = M.NameMap (MVar Module)

-- signal a module is done with a compilation phase and unblock all pending reads.
modmapPut :: ModuleMap -> Module -> Build ()
modmapPut modmap mod
  = liftIO $ putMVar ((M.!) modmap (modName mod)) mod

-- signal a module is done with a compilation phase and unblock all pending reads.
-- only the first call succeeds but subsequent ones will not block (useful for exception handling)
modmapTryPut :: ModuleMap -> Module -> Build Bool
modmapTryPut modmap mod
  = liftIO $ tryPutMVar ((M.!) modmap (modName mod)) mod

-- blocks until a `modmapTryPut` happens (at which point it returns the module definition at that phase).
-- (all further reads are non-blocking)
modmapRead :: ModuleMap -> ModuleName -> Build Module
modmapRead modmap modname
  = liftIO $ readMVar ((M.!) modmap modname)

-- create an initial module map
modmapCreate :: [Module] -> Build ModuleMap
modmapCreate modules
   = M.fromList <$> mapM (\mod -> do v <- liftIO $ newEmptyMVar
                                     return (modName mod, v)) modules


-- ensure a phase always sets its mvar to guarantee progress
moduleGuard :: ModulePhase -> ModulePhase -> ModuleMap -> (a -> Module) -> (Module -> b)
                -> (Module -> Build a) -> ((Module -> Build b) -> a -> Build b) -> (Module -> Build b)
moduleGuard expectPhase targetPhase modmap fromRes toRes initial action mod0
  = do res <- onBuildException (edone mod0) (initial mod0)
       let mod = fromRes res
       buildFinally (edone mod) $
        if (modPhase mod < expectPhase || modPhase mod >= targetPhase)
          then done mod
          else action done res
  where
    edone mod = do modmapTryPut modmap mod
                   return ()

    done mod  = do modmapPut modmap mod
                   return (toRes mod)


{---------------------------------------------------------------
  Compile a module (type check, core compile, codegen, and link)
---------------------------------------------------------------}

moduleCompile :: [Name] -> ModuleMap -> ModuleMap -> ModuleMap -> ModuleMap -> Module -> Build Module
moduleCompile mainEntries tcheckedMap optimizedMap codegenMap linkedMap
  = moduleGuard PhaseCodeGen PhaseLinked linkedMap (\(_,_,mod) -> mod) id
                (moduleCodeGen mainEntries tcheckedMap optimizedMap codegenMap)
    $ \done (full,link,mod) ->
     do -- wait for all required imports to be codegen'd
        -- However, for a final exe we need to wait for the imports to be _linked_ as well.
        imports <- moduleGetFullImports (if full then linkedMap else codegenMap) [] (modImportNames mod)
        if any (\m -> modPhase m < PhaseCodeGen) imports
          then done mod  -- dependencies had errors (todo: we could keep going if the import has (previously computed) core?)
          else do phase "link" $ pretty (modName mod)
                  mbEntry <- pooledIO $ link   -- link it!
                  let mod' = mod{ modPhase = PhaseLinked }
                  done mod'


{---------------------------------------------------------------
  Code generation (.c,.js)
---------------------------------------------------------------}

moduleCodeGen :: [Name] -> ModuleMap -> ModuleMap -> ModuleMap -> Module -> Build (Bool, Link, Module)
moduleCodeGen mainEntries tcheckedMap optimizedMap codegenMap
  = moduleGuard PhaseOptimized PhaseCodeGen codegenMap (\mod -> mod) (\mod -> (False,noLink,mod))
                (moduleOptimize tcheckedMap optimizedMap) $ \done mod ->
    do -- wait for all required imports to be optimized (no need to wait for codegen!)
       imports <- moduleGetFullImports optimizedMap [] (modImportNames mod)
       if any (\m -> modPhase m < PhaseOptimized) imports
         then done mod
         else do  phase "codegen" $ pretty (modName mod) -- <.> text ": imported:" <+> list (map (pretty . modName) imports)
                  flags <- getFlags
                  term  <- getTerminal
                  let defs    = defsFromModules (mod:imports)  -- todo: optimize by reusing the defs from the compile?
                      inlines = inlinesFromModules imports
                  mbEntry <- getMainEntry (defsGamma defs) mainEntries mod
                  seqIO   <- sequentialIO
                  link    <- pooledIO $ codeGen term flags seqIO
                                                (defsNewtypes defs) (defsBorrowed defs) (defsKGamma defs) (defsGamma defs)
                                                mbEntry imports mod
                  let mod' = mod{ modPhase = PhaseCodeGen }
                  phaseVerbose 2 "codegen done" (pretty (modName mod))
                  done mod'
                  return (isJust mbEntry,link,mod')


getMainEntry :: Gamma -> [Name] -> Module -> Build (Maybe (Name,Type))
getMainEntry gamma mainEntries mod
  = case find (\name -> qualifier name == modName mod) mainEntries of
      Nothing   -> return Nothing
      Just main -> case gammaLookupQ main gamma of
                    [InfoFun{infoType=tp}] -> return $ Just (main,tp)
                    []     -> do addErrorMessageKind ErrBuild (text "unable to find main function:" <+> pretty main)
                                 return Nothing
                    [info] -> do addErrorMessageKind ErrBuild (text "main function " <+> pretty main <+> text "must be a function")
                                 return Nothing
                    _      -> do addErrorMessageKind ErrBuild (text "ambiguous main function:" <+> pretty main)
                                 return Nothing


{---------------------------------------------------------------
  Core optimize a module
  (not just optimization, some transformations are essential
  like perceus ref counting etc.)
---------------------------------------------------------------}

moduleOptimize :: ModuleMap -> ModuleMap -> Module -> Build Module
moduleOptimize tcheckedMap optimizedMap
  = moduleGuard PhaseTyped PhaseOptimized optimizedMap id id (moduleTypeCheck tcheckedMap) $ \done mod ->
     do -- wait for direct (user+pub) imports to be compiled
        imports <- moduleGetFullImports optimizedMap [] (modImportNames mod)
        if any (\m -> modPhase m < PhaseOptimized) imports
          then done mod  -- dependencies had errors (todo: we could keep going if the import has (previously computed) core?)
          else -- core compile
              do  phase "optimize" $ pretty (modName mod) -- <.> text ": imported:" <+> list (map (pretty . modName) imports)
                  flags <- getFlags
                  let defs    = defsFromModules (mod:imports)  -- todo: optimize by reusing the defs from the type check?
                      inlines = inlinesFromModules imports
                  (core,inlineDefs) <- liftError $ coreOptimize flags (defsNewtypes defs) (defsGamma defs) inlines (fromJust (modCore mod))
                  let mod' = mod{ modPhase   = PhaseOptimized
                                , modCore    = Just $! core
                                , modInlines = Right $! inlineDefs
                                }
                  phaseVerbose 2 "optimize done" (pretty (modName mod))
                  done mod'


-- Import also modules required for checking inlined definitions from direct imports.
moduleGetFullImports :: ModuleMap -> [ModuleName] -> [ModuleName] -> Build [Module]
moduleGetFullImports modmap alreadyDone0 importNames
  = do -- wait for imported modules to be compiled
       imports <- mapM (modmapRead modmap) importNames
       let alreadyDone = alreadyDone0 ++ importNames
           extras = nub $ [Core.importName imp | mod <- imports, hasInlines (modInlines mod),
                                                  -- consider all of its imports too to ensure we can check its inline definitions
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

moduleTypeCheck :: ModuleMap -> Module -> Build Module
moduleTypeCheck tcheckedMap
  = moduleGuard PhaseParsed PhaseTyped tcheckedMap id id return $ \done mod ->
     do -- wait for direct imports to be type checked
        imports <- moduleGetPubImports tcheckedMap [] (modDeps mod)
        if any (\m -> modPhase m < PhaseTyped) imports
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
                  let mod' = mod{ modPhase = PhaseTyped
                                , modCore = Just $! core
                                , modRangeMap = mbRangeMap
                                , modDefinitions = Just $! (defsFromCore core)
                                }
                  phaseVerbose 2 "check done" (pretty (modName mod))
                  done mod'


-- Recursively load public imports from imported modules in a fixpoint
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
-- - needs the original user imports as well to determine provenance
-- - needs the user program imports as well to determine visibility
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

-- given a root set of modules, and a list of previously loaded modules (for reuse),
-- load- or parse all required modules to compile the root set.
-- needs also `rebuild` and `forced` to know whether re-parse or load from an interface file
-- to determine dependencies
modulesResolveDependencies :: VFS -> Bool -> [ModuleName] -> [Module] -> [Module] -> Build [Module]
modulesResolveDependencies vfs rebuild forced cached roots
  = do ordered <- -- phaseTimed "resolving" (list (map (pretty . modName) roots)) $
                  modulesResolveDeps vfs rebuild forced cached roots []
       return ordered

modulesResolveDeps :: VFS -> Bool -> [ModuleName] -> [Module] -> [Module] -> [Module] -> Build [Module]
modulesResolveDeps vfs rebuild forced cached roots acc
  = do lroots     <- mapConcurrentModules (moduleLoad vfs rebuild forced) roots    -- we can concurrently load/parse modules
       let loaded = lroots ++ acc
       newimports <- nubBy (\m1 m2 -> modName m1 == modName m2) <$> concat <$>
                     mapM (addImports loaded) lroots
       if (null newimports)
         then toBuildOrder loaded
         else do phaseVerbose 2 "resolve" (list (map (pretty . modName) newimports))
                 modulesResolveDeps vfs rebuild forced cached newimports loaded  -- keep resolving until all have been loaded
  where
    addImports loaded mod
      = catMaybes <$> mapM addImport (modDeps mod)
      where
        addImport :: ModuleName -> Build (Maybe Module)
        addImport impName
          = if any (\m -> modName m == impName) loaded
              then return Nothing
              else case find (\m -> modName m == impName) cached of
                     Just mod  -> do -- m <- moduleValidate vfs mod
                                     return (Just mod)
                     _         -> do let relativeDir = dirname (modSourcePath mod)
                                     m <- moduleFromModuleName vfs relativeDir impName
                                     return (Just m)

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

moduleLoad :: VFS -> Bool -> [ModuleName] -> Module -> Build Module
moduleLoad vfs rebuild forced mod
  = let force = (rebuild || modName mod `elem` forced) in
    if (modPhase mod >= PhaseLoaded) && not force
      then return mod
      else do (mod',errs) <- checkedDefault mod $ -- on error, return the original module
                             if not (null (modLibIfacePath mod)) && (modIfaceTime mod < modLibIfaceTime mod) && not force
                               then moduleLoadLibIface mod
                               else if (modSourceTime mod < modIfaceTime mod) && not force
                                 then moduleLoadIface mod
                                 else moduleParse vfs mod
              return mod'{ modErrors = mergeErrors errs (modErrors mod') }

moduleParse :: VFS -> Module -> Build Module
moduleParse vfs mod
  = do phase "parse" (text (modSourcePath mod))
       flags <- getFlags
       let allowAt = isPrimitiveModule (modName mod)
       input <- getFileContents vfs (modSourcePath mod)
       prog  <- liftError $ parseProgramFromString allowAt (semiInsert flags) input (modSourcePath mod)
       return mod{ modPhase = PhaseParsed
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
       phase "load" (pretty (modName mod) <+> color (colorInterpreter cscheme) (text "from:") <+> text (modLibIfacePath mod))
       (core,parseInlines) <- liftIOError $ parseCore (modLibIfacePath mod) (modSourcePath mod)
       flags <- getFlags
       pooledIO $ copyLibIfaceToOutput flags (modLibIfacePath mod) (modIfacePath mod) core
       return (modFromIface core parseInlines mod)

modFromIface :: Core.Core -> Maybe (Gamma -> Error () [Core.InlineDef]) -> Module -> Module
modFromIface core parseInlines mod
  =  mod{ modPhase       = PhaseLinked
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

moduleFromSource :: VFS -> FilePath -> Build Module
moduleFromSource vfs fpath0
  = do let fpath = normalize fpath0
       mbpath <- searchSourceFile vfs "" fpath
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
                                moduleValidate vfs $ (moduleCreateInitial modName sourcePath ifacePath ""){ modSourceRelativePath = stem }
  where
    isValidId :: String -> Bool  -- todo: make it better
    isValidId ""      = False
    isValidId (c:cs)  = isLower c && all (\c -> isAlphaNum c || c `elem` "_-") cs


moduleFromModuleName :: VFS -> FilePath -> Name -> Build Module
moduleFromModuleName vfs relativeDir modName
  = do mbSourceName <- searchSourceFile vfs relativeDir (nameToPath modName ++ sourceExtension)
       ifacePath    <- outputName (moduleNameToPath modName ++ ifaceExtension)
       libIfacePath <- searchLibIfaceFile vfs (moduleNameToPath modName ++ ifaceExtension)
       case mbSourceName of
         Just (root,stem)
            -> moduleValidate vfs $ (moduleCreateInitial modName (joinPath root stem) ifacePath libIfacePath){ modSourceRelativePath = stem }
         Nothing
            -> do ifaceExist <- buildDoesFileExistAndNotEmpty vfs ifacePath
                  if ifaceExist
                    then do cs <- getColorScheme
                            addWarningMessage (warningMessageKind ErrBuild rangeNull (text "interface" <+> color (colorModule cs) (pretty modName) <+> text "found but no corresponding source module"))
                            moduleValidate vfs $ moduleCreateInitial modName "" ifacePath libIfacePath
                    else throwModuleNotFound rangeNull modName

-- Find a source file and resolve it
-- with a `(root,stem)` where `stem` is the minimal module path relative to the include roots.
-- The root is either in the include paths or the full directory for absolute paths.
-- (and absolute file paths outside the include roots always have a single module name corresponding to the file)
-- relativeDir is set when importing from a module so a module name is first resolved relative to the current module
searchSourceFile :: VFS -> FilePath -> FilePath -> Build (Maybe (FilePath,FilePath))
searchSourceFile vfs relativeDir fname
  = do -- trace ("search source: " ++ fname ++ " from " ++ concat (intersperse ", " (relativeDir:includePath flags))) $ return ()
       flags <- getFlags
       case vfsFind vfs fname of  -- must match exactly; we may improve this later on and search relative files as well?
         Just _ -> return $! Just $! (getMaximalPrefixPath (includePath flags) fname)
         _      -> liftIO $ searchPathsCanonical relativeDir (includePath flags) [sourceExtension,sourceExtension++".md"] [] fname

-- find a pre-compiled libary interface
-- in the future we can support packages as well
searchLibIfaceFile :: VFS -> FilePath -> Build FilePath  -- can be empty
searchLibIfaceFile vfs fname
  = do flags <- getFlags
       let libIfacePath = joinPaths [localLibDir flags, buildVariant flags, fname]
       exist <- buildDoesFileExist vfs libIfacePath
       return (if exist then libIfacePath else "")


{---------------------------------------------------------------
  Validate if modules are still valid
---------------------------------------------------------------}

modulesValidate :: VFS -> [Module] -> Build [Module]
modulesValidate vfs modules
  = mapM (moduleValidate vfs) modules

moduleValidate :: VFS -> Module -> Build Module
moduleValidate vfs mod
  = do ftSource   <- getFileTime vfs (modSourcePath mod)
       ftIface    <- getFileTime vfs (modIfacePath mod)
       ftLibIface <- getFileTime vfs (modLibIfacePath mod)
       let stale = (ftSource > modSourceTime mod ||
                    ftIface > modIfaceTime mod ||
                    ftLibIface > modLibIfaceTime mod)
           mod'  = mod{ modSourceTime = ftSource
                      , modIfaceTime  = ftIface
                      , modLibIfaceTime = ftLibIface
                      }
       -- phaseVerbose 2 "validate" (pretty (modName mod') <.> text (": times: " ++ show (force,stale,ftSource,modIfaceTime mod')))
       if stale
         then return mod'{ modPhase = PhaseInit, modErrors = errorsNil,
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
                 envSemPooled     :: QSem,    -- limit I/O concurrency
                 envSemSequential :: QSem     -- limit some I/O to be atomic
               }

noVFS :: VFS
noVFS = VFS (\fpath -> Nothing)


runBuildIO :: Terminal -> Flags -> Build a -> IO (Maybe a)
runBuildIO term flags cmp
  = do res <- runBuild term flags cmp
       case res of
         Right (x,errs) -> do mapM_ (termError term) (take (maxErrors flags) (errors errs))
                              return (Just x)
         Left errs      -> do mapM_ (termError term) (take (maxErrors flags) (errors errs))
                              return Nothing

runBuild :: Terminal -> Flags -> Build a -> IO (Either Errors (a,Errors))
runBuild term flags cmp
  = do errs <- newIORef errorsNil
       rng  <- newIORef rangeNull
       semPooled     <- newQSem (min 64 (max 1 (maxConcurrency flags)))
       semSequential <- newQSem 1
       termProxyDone <- newEmptyMVar
       (termProxy,stop) <- forkTerminal term termProxyDone
       finally (runBuildEnv (Env termProxy flags errs rng semPooled semSequential) cmp)
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
       flags <- getFlags
       ys  <- if (maxConcurrency flags <= 1)
                then liftIO $ mapM (\x -> runBuildEnv env (f x)) xs
                else liftIO $ mapConcurrently (\x -> runBuildEnv env (f x)) xs
       let errs = lefts ys
       if null errs
         then do let (zs,warns) = unzip (rights ys)
                 mapM_ addErrors warns
                 return zs
         else throw (foldr mergeErrors errorsNil errs)

-- pooled is used for I/O operations that need to be limited in total concurrency
pooledIO :: IO a -> Build a
pooledIO io
  = do env <- getEnv
       liftIO $ withSem (envSemPooled env) io

sequentialIO :: Build (IO a -> IO a)
sequentialIO
  = do env <- getEnv
       return (withSem (envSemSequential env))

withSem :: QSem -> IO a -> IO a
withSem sem io
  = do waitQSem sem
       io `finally` signalQSem sem


-- map concurrently and keep errors in the processed module
mapConcurrentModules :: (Module -> Build Module) -> [Module] -> Build [Module]
mapConcurrentModules f modules
  = mapConcurrent (\mod -> withCheckedModule mod (f mod)) modules


instance Functor Build where
  fmap f (Build ie)  = Build (\env -> fmap f (ie env))

instance Applicative Build where
  pure x = seq x (Build (\env -> return x))
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

buildFinally :: Build b -> Build a -> Build a
buildFinally (Build fin) (Build b)
  = Build (\env -> finally (b env) (fin env))

throwError :: ErrorMessage -> Build a
throwError msg
  = liftIO $ throw (errorsSingle msg)

throwErrorKind :: ErrorKind -> Doc -> Build a
throwErrorKind ekind doc
  = do rng <- getCurrentRange
       throwError (errorMessageKind ekind rng doc)

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

hasBuildError :: Build Bool
hasBuildError
  = do env  <- getEnv
       errs <- liftIO $ readIORef (envErrors env)
       return $! any (\err -> errSeverity err >= SevError) (errors errs)

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

addErrorMessageKind :: ErrorKind -> Doc -> Build ()
addErrorMessageKind ekind doc
  = do rng <- getCurrentRange
       addErrorMessage (errorMessageKind ekind rng doc)

phaseTimed :: String -> Doc -> Build a -> Build a
phaseTimed p doc action
  = do t0 <- liftIO $ getCurrentTime
       phaseVerbose 1 (p ++ " start") doc
       buildFinally (do t1 <- liftIO $ getCurrentTime
                        phaseVerbose 1 (p ++ " elapsed") (text (showTimeDiff t1 t0) <.> linebreak))
                    action


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

buildDoesFileExist :: VFS -> FilePath -> Build Bool
buildDoesFileExist vfs fpath
  = case vfsFind vfs fpath of
      Just _ -> return True
      _      -> liftIO $ doesFileExist fpath

buildDoesFileExistAndNotEmpty :: VFS -> FilePath -> Build Bool
buildDoesFileExistAndNotEmpty vfs fpath
  = case vfsFind vfs fpath of
      Just (content,_) -> return $! not (bstringIsEmpty content)
      _                -> liftIO $ doesFileExistAndNotEmpty fpath

getFileTime :: VFS -> FilePath -> Build FileTime
getFileTime vfs "" = return fileTime0
getFileTime vfs fpath0
  = do let fpath = normalize fpath0
       case vfsFind vfs fpath of
         Just (_, t) -> return t
         Nothing     -> liftIO $ F.getFileTime fpath

maybeGetFileTime :: VFS -> FilePath -> Build (Maybe FileTime)
maybeGetFileTime vfs fpath
  = do ft <- getFileTime vfs fpath
       return (if ft == fileTime0 then Nothing else Just ft)

getFileContents :: VFS -> FilePath -> Build BString
getFileContents vfs fpath0
  = do env <- getEnv
       let fpath = normalize fpath0
       case vfsFind vfs fpath of
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

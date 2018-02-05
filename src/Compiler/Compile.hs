-----------------------------------------------------------------------------
-- Copyright 2012-2017 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Main module.
-}
-----------------------------------------------------------------------------
module Compiler.Compile( -- * Compile
                         compileFile
                       , compileModule
                       , compileModuleOrFile

                         -- * Interpreter
                       , compileExpression, compileType
                       , compileValueDef, compileTypeDef 
                       , compileProgram                       
                       , gammaFind

                         -- * Types
                       , Module(..)
                       , Loaded(..), initialLoaded, loadedName
                       , Terminal(..)
                       , searchSource
                       , CompileTarget(..)
                       ) where

import Lib.Trace              ( trace )
import Data.Char              ( isAlphaNum )

import System.Directory       ( createDirectoryIfMissing, canonicalizePath )
import Data.List              ( isPrefixOf )
import Control.Applicative 
import Control.Monad          ( ap, when )
import Common.Failure
import Lib.Printer            ( withNewFilePrinter )
import Common.Range           -- ( Range, sourceName )
import Common.Name            -- ( Name, newName, qualify, asciiEncode )
import Common.NamePrim        ( nameExpr, nameType, nameInteractiveModule, nameSystemCore, nameMain, nameTpWrite, nameTpIO, nameTpCps, nameTpAsync )
import Common.Error
import Common.File            
import Common.ColorScheme
import Common.Message         ( table )
import Common.Syntax
import Syntax.Syntax          
-- import Syntax.Lexer           ( readInput )
import Syntax.Parse           ( parseProgramFromFile, parseValueDef, parseExpression, parseTypeDef, parseType )

import Syntax.RangeMap
import Syntax.Colorize        ( colorize )
import Core.GenDoc            ( genDoc )
import Core.Check             ( checkCore )
import Core.UnReturn          ( unreturn )
import Core.Monadic           ( monTransform )

import Static.BindingGroups   ( bindingGroups )
import Static.FixityResolve   ( fixityResolve, fixitiesNew, fixitiesCompose )

import Kind.ImportMap       
import Kind.Newtypes          ( newtypesCompose )
import Kind.Infer             ( inferKinds )
import Kind.Kind              ( kindEffect )

import Type.Type              
import Type.Kind              ( containsHandledEffect )
import Type.Assumption        ( gammaLookupQ, extractGamma, infoType, gammaUnions, extractGammaImports, gammaLookup, gammaMap )
import Type.Infer             ( inferTypes )
import Type.Pretty hiding     ( verbose )            
import Compiler.Options       ( Flags(..), prettyEnvFromFlags, colorSchemeFromFlags, prettyIncludePath )

import Compiler.Module

-- needed for code generation
import Data.Char              ( toUpper )
import Lib.PPrint             hiding (dquote)
import Platform.Config        ( exeExtension, pathSep, sourceExtension )

import Backend.CSharp.FromCore( csharpFromCore )
import Backend.JavaScript.FromCore( javascriptFromCore )

import qualified Core.Core as Core
import Core.Simplify( simplifyDefs )
import Core.Uniquefy( uniquefy )
import qualified Core.Pretty
import Core.Parse(  parseCore )

import System.Directory ( doesFileExist )
import Compiler.Package
-- import qualified Core.Check

-- Debugging
-- import Lib.Trace

{--------------------------------------------------------------------------
  Compilation
--------------------------------------------------------------------------}

data Terminal = Terminal{ termError :: ErrorMessage -> IO ()
                        , termPhase :: String -> IO ()
                        , termPhaseDoc :: Doc -> IO ()
                        , termType  :: Scheme -> IO ()
                        , termDoc   :: Doc -> IO ()
                        }


data IOErr a = IOErr (IO (Error a))

runIOErr :: IOErr a -> IO (Error a)
runIOErr (IOErr ie) = ie

liftError :: Error a -> IOErr a
liftError err = IOErr (return err)

liftIO :: IO a -> IOErr a
liftIO io = IOErr (do x <- io
                      return (return x))

lift :: IO (Error a) -> IOErr a
lift ie   = IOErr ie

instance Functor IOErr where
  fmap f (IOErr ie)  = IOErr (fmap (fmap f) ie)

instance Applicative IOErr where
  pure  = return
  (<*>) = ap

instance Monad IOErr where
  return x          = IOErr (return (return x))
  (IOErr ie) >>= f  = IOErr (do err <- ie
                                case checkError err of
                                   Right (x,w) -> case f x of
                                                   IOErr ie' -> do err <- ie'
                                                                   return (addWarnings w err)
                                   Left msg  -> return (errorMsg msg  ))


bindIO :: IO (Error a) -> (a -> IO (Error b)) -> IO (Error b)
bindIO io f
  = do err <- io
       case checkError err of
         Left msg -> return (errorMsg msg)
         Right (x,w)  -> fmap (addWarnings w) (f x)

gammaFind name g
  = case (gammaLookupQ name g) of
      [tp] -> tp
      []   -> failure ("Compiler.Compile.gammaFind: can't locate " ++ show name)
      _    -> failure ("Compiler.Compile.gammaFind: multiple definitions for " ++ show name)




compileExpression :: Terminal -> Flags -> Loaded -> CompileTarget () -> UserProgram -> Int -> String -> IO (Error Loaded)
compileExpression term flags loaded compileTarget program line input
  = runIOErr $ 
    do let qnameExpr = (qualify (getName program) nameExpr)
       def <- liftError (parseExpression (semiInsert flags) (show nameInteractiveModule) line qnameExpr input)
       let programDef = programAddDefs program [] [def]
       -- specialized code: either just call the expression, or wrap in a show function
       case compileTarget of
         -- run a particular entry point
         Executable name ()  | name /= nameExpr
           -> compileProgram' term flags (loadedModules loaded) compileTarget "<interactive>" programDef       

         -- entry point is the expression: compile twice: 
         --  first to get the type of the expression and create a 'show' wrapper,
         --  then to actually run the program
           | otherwise
           -> do ld <- compileProgram' term flags{ evaluate = False } (loadedModules loaded) compileTarget  "<interactive>" programDef
                 let tp = infoType (gammaFind qnameExpr (loadedGamma ld))
                     (_,_,rho) = splitPredType tp
                 liftError $ checkUnhandledEffects flags nameExpr rangeNull rho
                 case splitFunType rho of
                   -- return unit: just run the expression (for its assumed side effect)
                   Just (_,_,tres)  | isTypeUnit tres
                      -> compileProgram' term flags (loadedModules ld) compileTarget  "<interactive>" programDef       
                   -- check if there is a show function, or use generic print if not.
                   Just (_,_,tres)  
                      -> do -- ld <- compileProgram' term flags (loadedModules ld0) Nothing "<interactive>" programDef
                            let matchShow (_,info) = let (_,_,itp) = splitPredType (infoType info)
                                                     in case splitFunType itp of
                                                          Just (targ:targs,_,_) | tres == snd targ && all (isOptional . snd) targs
                                                            -> True
                                                          _ -> False
                                r = rangeNull
                                mkApp e es = App e [(Nothing,x) | x <- es] r
                            case filter matchShow (gammaLookup (newName "show") (loadedGamma ld)) of
                              [(qnameShow,_)] 
                                -> do let expression = mkApp (Var (qualify nameSystemCore (newName "println")) False r) 
                                                        [mkApp (Var qnameShow False r) [mkApp (Var qnameExpr False r) []]]
                                      let defMain = Def (ValueBinder (qualify (getName program) nameMain) () (Lam [] expression r) r r)  r Public (DefFun PolyMon)  ""                            
                                      let programDef' = programAddDefs programDef [] [defMain]
                                      compileProgram' term flags (loadedModules ld) (Executable nameMain ()) "<interactive>" programDef'
                                      return ld
                              
                              _  -> liftError $ errorMsg (ErrorGeneral rangeNull (text "no 'show' function defined for values of type:" <+> ppType (prettyEnvFromFlags flags) tres)) 
                                                     -- mkApp (Var (qualify nameSystemCore (newName "gprintln")) False r) 
                                                     --   [mkApp (Var nameExpr False r) []]
                   Nothing
                    -> failure ("Compile.Compile.compileExpression: should not happen")
         -- no evaluation
         _ -> compileProgram' term flags (loadedModules loaded) compileTarget "<interactive>" programDef       

         
errorModuleNotFound :: Flags -> Range -> Name -> ErrorMessage 
errorModuleNotFound flags range name
  = ErrorGeneral range $ errorNotFound flags colorModule "module" (pretty name)

errorFileNotFound :: Flags -> FilePath -> ErrorMessage 
errorFileNotFound flags name
  = ErrorIO $ text "error:" <+> errorNotFound flags colorSource "" (text name)

errorNotFound flags clr kind namedoc
  = text ("could not find" ++ (if null kind then "" else (" " ++ kind)) ++ ":") <+> color (clr cscheme) namedoc <->
    text "search path:" <+> prettyIncludePath flags 
  where
    cscheme = colorSchemeFromFlags flags


prettyEnv loaded flags
  = (prettyEnvFromFlags flags){ context = loadedName loaded, importsMap = loadedImportMap loaded }


compileType :: Terminal -> Flags -> Loaded -> UserProgram -> Int -> String -> IO (Error Loaded)
compileType term flags loaded program line input
  = runIOErr $
    do let qnameType = qualify (getName program) nameType
       tdef <- liftError $ parseType (semiInsert flags) (show nameInteractiveModule) line nameType input
       let programDef = programAddDefs (programRemoveAllDefs program) [tdef] [] 
       -- typeCheck (loaded) flags line programDef
       compileProgram' term flags (loadedModules loaded) Object "<interactive>" programDef


compileValueDef :: Terminal -> Flags -> Loaded -> UserProgram -> Int -> String -> IO (Error (Name,Loaded))
compileValueDef term flags loaded program line input
  = runIOErr $
    do def <- liftError $ parseValueDef (semiInsert flags) (show nameInteractiveModule) line input
       let programDef = programAddDefs program [] [def]
       ld <- compileProgram' term flags (loadedModules loaded) Object "<interactive>" programDef
       return (qualify (getName program) (defName def),ld)

compileTypeDef :: Terminal -> Flags -> Loaded -> UserProgram -> Int -> String -> IO (Error (Name,Loaded))
compileTypeDef term flags loaded program line input
  = runIOErr $
    do (tdef,cdefs) <- liftError $ parseTypeDef (semiInsert flags) (show nameInteractiveModule) line input
       let programDef = programAddDefs program [tdef] cdefs
       ld <- compileProgram' term flags (loadedModules loaded) Object "<interactive>" programDef
       return (qualify (getName program) (typeDefName tdef),ld)


{---------------------------------------------------------------
  compileFile/Module
  These are meant to be called from the interpreter/main compiler
---------------------------------------------------------------}

compileModuleOrFile :: Terminal -> Flags -> Modules -> String -> Bool -> IO (Error Loaded)
compileModuleOrFile term flags modules fname force
  | any (not . validModChar) fname = compileFile term flags modules Object fname
  | otherwise
    = do let modName = newName fname
         exist <- searchModule flags "" modName
         case (exist) of
          Just fpath -> compileModule term (if force then flags{ forceModule = fpath } else flags) 
                                      modules modName 
          Nothing -> do fexist <- searchSourceFile flags "" fname
                        runIOErr $
                         case (fexist) of
                          Just (root,stem)  
                            -> compileProgramFromFile term flags modules Object root stem 
                          Nothing 
                            -> liftError $ errorMsg $ errorFileNotFound flags fname 
  where
    validModChar c
      = isAlphaNum c || c `elem` "/_"

compileFile :: Terminal -> Flags -> Modules -> CompileTarget () -> FilePath -> IO (Error Loaded)
compileFile term flags modules compileTarget fpath 
  = runIOErr $ 
    do mbP <- liftIO $ searchSourceFile flags "" fpath 
       case mbP of
         Nothing -> liftError $ errorMsg (errorFileNotFound flags fpath) 
         Just (root,stem)
           -> compileProgramFromFile term flags modules compileTarget root stem 
  
-- | Make a file path relative to a set of given paths: return the (maximal) root and stem
-- if it is not relative to the paths, return dirname/notdir
makeRelativeToPaths :: [FilePath] -> FilePath -> (FilePath,FilePath)
makeRelativeToPaths paths fname
  = case findMaximalPrefix paths fname of
      Just (n,root) -> (root,drop n fname)
      _             -> ("", fname)


compileModule :: Terminal -> Flags -> Modules -> Name -> IO (Error Loaded)
compileModule term flags modules name  -- todo: take force into account
  = runIOErr $ -- trace ("compileModule: " ++ show name) $
    do let imp = ImpProgram (Import name name rangeNull Private) 
       (loaded,((mod:_):_)) <- resolveImports term flags "" initialLoaded{ loadedModules = modules } [imp]
       return loaded{ loadedModule = mod }
  
{---------------------------------------------------------------
  Internal compilation
---------------------------------------------------------------}
compileProgram :: Terminal -> Flags -> Modules -> CompileTarget () -> FilePath -> UserProgram -> IO (Error Loaded)
compileProgram term flags modules compileTarget fname program
  = runIOErr $ compileProgram' term flags modules compileTarget  fname program
  

compileProgramFromFile :: Terminal -> Flags -> Modules -> CompileTarget () -> FilePath -> FilePath -> IOErr Loaded
compileProgramFromFile term flags modules compileTarget rootPath stem 
  = do let fname = joinPath rootPath stem
       liftIO $ termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "compile:") <+> color (colorSource (colorScheme flags)) (text (normalizeWith '/' fname)))
       liftIO $ termPhase term ("parsing " ++ fname)
       exist <- liftIO $ doesFileExist fname
       if (exist) then return () else liftError $ errorMsg (errorFileNotFound flags fname)
       program <- lift $ parseProgramFromFile (semiInsert flags) fname
       let isSuffix = map (\c -> if isPathSep c then '/' else c) (noexts stem)
                       `endsWith` show (programName program)                        
           ppcolor c doc = color (c (colors (prettyEnvFromFlags flags))) doc
       if (isExecutable compileTarget || isSuffix) then return ()
        else liftError $ errorMsg (ErrorGeneral (programNameRange program) 
                                     (text "module name" <+> 
                                      ppcolor colorModule (pretty (programName program)) <+> 
                                      text "is not a suffix of the file path" <+>
                                      parens (ppcolor colorSource $ text $ dquote $ stem)))
       let stemName = nameFromFile stem
       compileProgram' term flags modules compileTarget fname program{ programName = stemName }
  
nameFromFile :: FilePath -> Name
nameFromFile fname
  = newName $ map (\c -> if isPathSep c then '/' else c) $ 
    dropWhile isPathSep $ noexts fname

data CompileTarget a
  = Object
  | Library
  | Executable { entry :: Name, info :: a }

isExecutable (Executable _ _) = True
isExecutable _ = False

compileProgram' :: Terminal -> Flags -> Modules -> CompileTarget () -> FilePath -> UserProgram -> IOErr Loaded
compileProgram' term flags modules compileTarget fname program
  = do -- liftIO $ termPhase term ("resolve " ++ show (getName program))
       ftime <- liftIO (getFileTimeOrCurrent fname)
       let name   = getName program
           outIFace = outName flags (showModName name) ++ ifaceExtension
           mod    = Module name outIFace fname "" "" [] (Just program) (failure "Compiler.Compile.compileProgram: recursive module import") Nothing ftime
           allmods = addOrReplaceModule mod modules
           loaded = initialLoaded { loadedModule = mod 
                                  , loadedModules = allmods
                                  }
       -- trace ("compile file: " ++ show fname ++ "\n time: "  ++ show ftime ++ "\n latest: " ++ show (loadedLatest loaded)) $ return ()
       liftIO $ termPhase term ("resolve imports " ++ show (getName program))
       (loaded1,imported) <- resolveImports term flags (dirname fname) loaded (map ImpProgram (programImports program))
       if (name /= nameInteractiveModule || verbose flags > 0)
        then liftIO $ termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "check  :") <+> 
                                           color (colorSource (colorScheme flags)) (pretty (name)))
        else return ()
       let coreImports = concatMap toCoreImport (zip imported (programImports program))
           toCoreImport (mods,imp) = map (\mod -> Core.Import (modName mod) (modPackagePath mod) 
                                                  (importVis imp) (Core.coreProgDoc (modCore mod))) mods
       loaded2 <- liftError $ typeCheck loaded1 flags 0 coreImports program
       liftIO $ termPhase term ("codegen " ++ show (getName program))
       newTarget <- liftError $ 
           case compileTarget of
             Executable entryName _
               -> let mainName = if (isQualified entryName) then entryName else qualify (getName program) (entryName) in
                  case map infoType (gammaLookupQ mainName (loadedGamma loaded2)) of
                     []   -> errorMsg (ErrorGeneral rangeNull (text "there is no 'main' function defined" <-> text "hint: use the '-l' flag to generate a library?"))
                     tps  -> let mainType = TFun [] (TCon (TypeCon nameTpIO kindEffect)) typeUnit  -- just for display, so IO can be TCon
                                 isMainType tp = case expandSyn tp of
                                                   TFun [] eff resTp  -> True -- resTp == typeUnit
                                                   _                  -> False
                             in case filter isMainType tps of
                               [tp] -> do checkUnhandledEffects flags mainName rangeNull tp
                                          return (Executable mainName tp)
                               []   -> errorMsg (ErrorGeneral rangeNull (text "the type of 'main' must be a function without arguments" <-> 
                                                                                      table [(text "expected type", ppType (prettyEnvFromFlags flags) mainType)
                                                                                            ,(text "inferred type", ppType (prettyEnvFromFlags flags) (head tps))]))
                               _    -> errorMsg (ErrorGeneral rangeNull (text "found multiple definitions for the 'main' function"))
             Object -> return Object
             Library -> return Library                   
       -- try to set the right host depending on the main type                                     
       let flags' = case newTarget of
                      Executable _ tp 
                          -> -- trace ("type to analyse: " ++ show (pretty tp) ++ "\n\n") $
                             case expandSyn tp of
                               TFun [] eff resTp -> let isWrite (TApp (TCon tc) [_]) = typeconName tc == nameTpWrite
                                                        isWrite _                    = False
                                                    in -- trace ("effect: " ++ show (extractEffectExtend eff) ++ "\n\n") $
                                                       case filter isWrite (fst (extractEffectExtend eff)) of
                                                          (TApp _ [TCon tc]):_  | typeconName tc == newQualified "sys/dom/types" "hdom"
                                                            -> flags{ host = Browser }
                                                          _ -> flags
                               _ -> flags
                      _ -> flags

       loaded3 <- liftIO $ codeGen term flags' newTarget loaded2 
       -- liftIO $ termDoc term (text $ show (loadedGamma loaded3))
       return loaded3{ loadedModules = addOrReplaceModule (loadedModule loaded3) (loadedModules loaded3) }

checkUnhandledEffects flags name range tp
  = case expandSyn tp of
      TFun _ eff _  | containsHandledEffect [nameTpCps,nameTpAsync] eff 
        -> errorMsg (ErrorGeneral range (text "there are unhandled effects for the main expression" <-->
                                         text " inferred effect:" <+> ppType (prettyEnvFromFlags flags) eff <-->
                                         text " hint           : wrap the main function in a handler"))
      _ -> return ()

data ModImport = ImpProgram Import
               | ImpCore    Core.Import 

instance Ranged ModImport where
  getRange (ImpProgram imp) = importRange imp
  getRange _                = rangeNull

impName :: ModImport -> Name
impName (ImpProgram imp)  = importName imp
impName (ImpCore cimp)    = Core.importName cimp  

impFullName :: ModImport -> Name
impFullName (ImpProgram imp)  = importFullName imp
impFullName (ImpCore cimp)    = Core.importName cimp  

resolveImports :: Terminal -> Flags -> FilePath -> Loaded -> [ModImport] -> IOErr (Loaded,[[Module]])
resolveImports term flags currentDir loaded []
  = return (loaded,[])
resolveImports term flags currentDir loaded (imp:imps)
  = do (mod,modules) <- resolveModule term flags currentDir (loadedModules loaded) imp
       let (loaded1,errs) = loadedImportModule (maxStructFields flags) loaded mod (getRange imp) (impName imp) 
           loaded2        = loaded1{ loadedModules = modules }
           -- impsPub        = filter (\imp -> Core.importVis imp == Public) $ Core.coreProgImports $ modCore mod
       mapM_ (\err -> liftError (errorMsg err)) errs   
       -- trace ("impsPub: " ++ show [show (Core.importName imp) ++ ": " ++ Core.importPackage imp | imp <- impsPub]) $ return ()
       (loaded3,mods3) <- resolveImports term flags currentDir loaded2 imps
       (loaded4,mods) <- resolvePubImports flags loaded3 mod
       return (loaded4,mods:mods3)

resolvePubImports :: Flags -> Loaded -> Module -> IOErr (Loaded,[Module])
resolvePubImports flags loaded0 mod  
  = do let imports = Core.coreProgImports $ modCore mod
           pubImports = filter (\imp -> Core.importVis imp == Public) imports
       ifaces <- mapM findImp pubImports
       let (loaded1,imps,errs) = foldl loadPubImport (loaded0,[],[])  (zip ifaces pubImports)
       mapM_ (\err -> liftError (errorMsg err)) errs
       return (loaded1,mod:imps)
  where
    modules = loadedModules loaded0
    modNames = map modName modules
           
    findImp imp
      = do mbiface <- liftIO $ searchOutputIface flags (Core.importName imp)
           case mbiface of
             Just iface -> return iface
             Nothing -> liftError $ 
                        errorMsg (ErrorGeneral rangeNull (text "Unable to find interface file for:" <+> pretty (Core.importName imp)))

    loadPubImport :: (Loaded,[Module],[ErrorMessage]) -> (FilePath,Core.Import) -> (Loaded,[Module],[ErrorMessage])            
    loadPubImport (loaded,imps,errs0) (iface,imp)
      = -- trace ("lookup pub import: " ++ iface ++ ", name: " ++ show (Core.importName imp) ++ "\n  " ++ show (map modName modules)) $
        case lookupImport iface modules of
          Just impMod | True -- modName impMod /= Core.importName imp
                      -> let (loadedImp,errs1) = loadedImportModule (maxStructFields flags) loaded impMod rangeNull (Core.importName imp)
                         in (loadedImp,impMod:imps,errs0) --  ++ errs1)
          _ -> (loaded,imps,errs0)


searchModule :: Flags -> FilePath -> Name -> IO (Maybe FilePath)
searchModule flags currentDir name
  = do mbSource <- searchSource flags currentDir name
       case mbSource of
         Just (root,stem) -> return (Just (joinPath root stem))
         Nothing -> do mbIface <- searchIncludeIface flags currentDir name
                       case mbIface of
                         Nothing -> searchPackageIface flags currentDir Nothing name
                         Just iface -> return (Just iface)


resolveModule :: Terminal -> Flags -> FilePath -> [Module] -> ModImport -> IOErr (Module,[Module])
resolveModule term flags currentDir modules mimp
  = case mimp of
      -- program import
      ImpProgram imp ->
        do -- local or in package?
           mbSource <- liftIO $ searchSource flags currentDir name
           case mbSource of
             Nothing -> -- no source, search first the include directories for an interface
               do mbIface <- liftIO $ searchIncludeIface flags currentDir name
                  -- trace ("load from program: " ++ show (mbSource,mbIface)) $ return ()
                  case mbIface of
                    Nothing -> -- still nothing: search the packages with an unknown package name
                               do mbIface <- liftIO $ searchPackageIface flags currentDir Nothing name
                                  case mbIface of
                                    Nothing -> liftError $ errorMsg $ errorModuleNotFound flags (importRange imp) name  
                                    Just iface -> -- it is a package interface                          
                                      do -- TODO: check there is no (left-over) iface in the outputDir?
                                         loadFromIface iface "" ""
                    Just iface -> do loadFromIface iface "" ""
            
             Just (root,stem) -> -- source found, search output iface
               do mbIface <- liftIO $ searchOutputIface flags name
                  -- trace ("load from program: " ++ show (mbSource,mbIface)) $ return ()
                  case mbIface of
                    Nothing    -> loadFromSource modules root stem
                    Just iface -> loadDepend iface root stem 
      
      -- core import in source                      
      ImpCore cimp | (null (Core.importPackage cimp)) && (currentDir == outDir flags) -> 
        do mbSource <- liftIO $ searchSource flags "" name
           mbIface  <- liftIO $ searchOutputIface flags name
           -- trace ("source core: found: " ++ show (mbIface,mbSource)) $ return ()
           case (mbIface,mbSource) of
             (Nothing,Nothing) 
                -> liftError $ errorMsg $ errorModuleNotFound flags rangeNull name
             (Nothing,Just (root,stem)) 
                -> loadFromSource modules root stem
             (Just iface,Nothing) 
                -> do let cscheme = (colorSchemeFromFlags flags)
                      liftIO $ termDoc term $ color (colorWarning cscheme) $ 
                         text "warning: interface" <+> color (colorModule cscheme) (pretty name) <+> text "found but no corresponding source module"
                      loadFromIface iface "" ""
             (Just iface,Just (root,stem)) 
                -> loadDepend iface root stem 

      -- core import of package
      ImpCore cimp ->          
        do mbIface <- liftIO $ searchPackageIface flags currentDir (Just (Core.importPackage cimp)) name
           -- trace ("core import pkg: " ++ Core.importPackage cimp ++ "/" ++ show name ++ ": found: " ++ show (mbIface)) $ return ()
           case mbIface of
             Nothing    -> liftError $ errorMsg $ errorModuleNotFound flags rangeNull name  
             Just iface -> loadFromIface iface "" ""

    where
      name = impFullName mimp 
    
      loadDepend iface root stem 
         = do let srcpath = joinPath root stem
              ifaceTime <- liftIO $ getFileTimeOrCurrent iface
              sourceTime <- liftIO $ getFileTimeOrCurrent srcpath
              case lookupImport iface modules of
                Just mod ->
                  if (srcpath /= forceModule flags && modTime mod >= sourceTime)
                   then -- trace ("module " ++ show (name) ++ " already loaded") $ 
                        -- loadFromModule iface root stem mod
                        return (mod,modules) -- TODO: revise! do proper dependency checking instead..
                   else -- trace ("module " ++ show ( name) ++ " already loaded but not recent enough..\n " ++ show (modTime mod, sourceTime)) $
                        loadFromSource modules root stem  
                Nothing ->
                  -- trace ("module " ++ show (name) ++ " not yet loaded") $ 
                  if (not (rebuild flags) && srcpath /= forceModule flags && ifaceTime > sourceTime)
                    then loadFromIface iface root stem
                    else loadFromSource modules root stem

      loadFromSource modules1 root fname
        = -- trace ("loadFromSource: " ++ root ++ "/" ++ fname) $
          do loadedImp <- compileProgramFromFile term flags modules1 Object root fname 
             return (loadedModule loadedImp, loadedModules loadedImp)
      
      loadFromIface iface root stem 
        = -- trace ("loadFromIFace: " ++  iface ++ ": " ++ root ++ "/" ++ stem) $
          do let (pkgQname,pkgLocal) = packageInfoFromDir (packages flags) (dirname iface)             
                 loadMessage msg = liftIO $ termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text msg) <+> 
                                       color (colorSource (colorScheme flags)) 
                                         (pretty (if null pkgQname then "" else pkgQname ++ "/") <>  pretty (name)))
             mod <- case lookupImport iface modules of
                      Just mod 
                       -> do loadMessage "reusing:"
                             return mod 
                      Nothing
                       -> do loadMessage "loading:"
                             ftime  <- liftIO $ getFileTime iface                             
                             core <- lift $ parseCore iface                             
                             -- liftIO $ copyIFaceToOutputDir term flags iface
                             let mod = Module (Core.coreName core) iface (joinPath root stem) pkgQname pkgLocal [] 
                                                Nothing -- (error ("getting program from core interface: " ++ iface))
                                                  core Nothing ftime
                             return mod
             loadFromModule iface root stem mod

      loadFromModule iface root source mod
        = -- trace ("loadFromModule: " ++ iface ++ ": " ++ root ++ "/" ++ source) $
          do let allmods = addOrReplaceModule mod modules 
                 loaded = initialLoaded { loadedModule = mod 
                                        , loadedModules = allmods
                                        }
             (loadedImp,impss) <- resolveImports term flags (dirname iface) loaded (map ImpCore (Core.coreProgImports (modCore mod)))
             let latest = maxFileTimes (map modTime (concat impss))
             -- trace ("loaded iface: " ++ show iface ++ "\n time: "  ++ show (modTime mod) ++ "\n latest: " ++ show (latest)) $ return ()
             if (latest > modTime mod 
                  && not (null source)) -- happens if no source is present but (package) depencies have updated... 
               then loadFromSource (loadedModules loadedImp) root source -- load from source after all
               else do liftIO $ copyIFaceToOutputDir term flags iface (modPackageQPath mod) (concat impss)
                       return ((loadedModule loadedImp){ modSourcePath = joinPath root source }, loadedModules loadedImp)


lookupImport :: FilePath {- interface name -} -> Modules -> Maybe Module
lookupImport imp [] = Nothing
lookupImport imp (mod:mods)
  = if (modPath mod == imp) 
     then Just (mod)
     else lookupImport imp mods


searchPackageIface :: Flags -> FilePath -> Maybe PackageName -> Name -> IO (Maybe FilePath)
searchPackageIface flags currentDir mbPackage name
  = do let postfix = showModName name ++ ifaceExtension -- map (\c -> if c == '.' then '_' else c) (show name) 
       case mbPackage of
         Nothing 
          -> searchPackages (packages flags) currentDir "" postfix
         Just ""
          -> do let reliface = joinPath currentDir postfix
                exist <- doesFileExist reliface  
                if exist then return (Just reliface) 
                 else return Nothing
         Just package
          -> searchPackages (packages flags) currentDir (head (splitPath package)) postfix 
       

searchOutputIface :: Flags -> Name -> IO (Maybe FilePath)
searchOutputIface flags name
  = do let postfix = showModName name ++ ifaceExtension -- map (\c -> if c == '.' then '_' else c) (show name) 
           iface = joinPath (outDir flags) postfix
       -- trace ("search output iface: " ++ show name ++ ": " ++ iface) $ return ()
       exist <- doesFileExist iface
       return (if exist then (Just iface) else Nothing) 
                              
searchSource :: Flags -> FilePath -> Name -> IO (Maybe (FilePath,FilePath))
searchSource flags currentDir name
  = searchSourceFile flags currentDir (show name)

searchSourceFile :: Flags -> FilePath -> FilePath -> IO (Maybe (FilePath,FilePath))
searchSourceFile flags currentDir fname
  = do -- trace ("search source: " ++ postfix ++ " from " ++ currentDir) $ return ()
       mbP <- searchPathsEx (currentDir : includePath flags) [sourceExtension,sourceExtension++".md"] fname 
       case mbP of
         Just (root,stem) | root == currentDir 
           -> return $ Just (makeRelativeToPaths (includePath flags) (joinPath root stem))
         _ -> return mbP 

searchIncludeIface :: Flags -> FilePath -> Name -> IO (Maybe FilePath)
searchIncludeIface flags currentDir name
  = do -- trace ("search include iface: " ++ showModName name ++ " from " ++ currentDir) $ return ()
       mbP <- searchPathsEx (currentDir : includePath flags) [] (showModName name ++ ifaceExtension) 
       case mbP of
         Just (root,stem) 
           -> return $ Just (joinPath root stem)
         Nothing -> return Nothing 

{---------------------------------------------------------------
  
---------------------------------------------------------------}
typeCheck :: Loaded -> Flags -> Int -> [Core.Import] -> UserProgram -> Error Loaded
typeCheck loaded flags line coreImports program
  = do -- static checks
       -- program1 <- {- mergeSignatures (colorSchemeFromFlags flags) -} (implicitPromotion program)
       let program0 = -- implicitPromotion (loadedConstructors loaded) 
                      (bindingGroups program)
           -- (warnings1,(fixities1,defs1,program1)) <- staticCheck (colorSchemeFromFlags flags) 
           -- (loadedFixities loaded) (loadedDefinitions loaded) program0
           program1  = program0
           warnings1 = []

           fixities0 = fixitiesNew [(name,fix) | FixDef name fix rng <- programFixDefs program0] 
           fixities1 = fixitiesCompose (loadedFixities loaded) (fixities0)

       (program2,fixities2) <- fixityResolve (colorSchemeFromFlags flags) fixities1 program1

       let warnings = warnings1 
           fname   = sourceName (programSource program)
           module1 = (moduleNull (getName program))
                               { modSourcePath = fname
                               , modPath = outName flags (showModName (getName program)) ++ ifaceExtension
                               , modProgram    = Just program
                               , modWarnings   = warnings
                               }
           -- module0 = loadedModule loaded
           loaded1 = loaded{ loadedModule      = module1
                           -- , loadedDefinitions = defs1
                           , loadedFixities    = fixities2
--                           , loadedModules     = (loadedModules loaded) ++ 
--                                                   (if null (modPath module1) then [] else [loadedModule loaded])
                           }

       addWarnings warnings (inferCheck loaded1 flags line coreImports program2 )



inferCheck :: Loaded -> Flags -> Int -> [Core.Import] -> UserProgram -> Error Loaded
inferCheck loaded flags line coreImports program1 
  = do -- kind inference 
       (defs, {- conGamma, -} kgamma, synonyms, newtypes, constructors, {- coreTypeDefs, coreExternals,-} coreProgram1, unique3, mbRangeMap1) 
         <- inferKinds 
              (colorSchemeFromFlags flags) (maxStructFields flags) 
              (if (outHtml flags > 0) then Just rangeMapNew else Nothing)
              (loadedImportMap loaded)
              (loadedKGamma loaded) 
              (loadedSynonyms loaded)
              (loadedUnique loaded) 
              program1

       let  gamma0  = gammaUnions [loadedGamma loaded
                                  ,extractGamma False (maxStructFields flags) coreProgram1
                                  ,extractGammaImports (importsList (loadedImportMap loaded)) (getName program1)
                                  ]
       
            loaded3 = loaded { loadedKGamma  = kgamma
                            , loadedGamma   = gamma0
                            , loadedSynonyms= synonyms
                            , loadedNewtypes= newtypesCompose (loadedNewtypes loaded) newtypes
                            , loadedConstructors=constructors
                            , loadedUnique  = unique3 
                            }
{-
       let coreImports = map toCoreImport (programImports program1)
           toCoreImport imp
            -- TODO: we cannot lookup an import this way due to clashing or relative module names 
            = case lookupImport (importFullName imp) (loadedModules loaded3) of
                Just mod  -> Core.Import (importFullName imp) (packageName (packageMap flags) (modPath mod)) (importVis imp) (Core.coreProgDoc (modCore mod))
                Nothing   -> failure ("Compiler.Compile.codeGen: unable to find module: " ++ show (importFullName imp))
-}
           

       -- type inference
       let penv = prettyEnv loaded3 flags

       (gamma,coreDefs0,unique4,mbRangeMap2) 
         <- inferTypes 
              penv
              mbRangeMap1
              (loadedSynonyms loaded3)
              (loadedNewtypes loaded3)
              (loadedConstructors loaded3)
              (loadedImportMap loaded3)
              (loadedGamma loaded3)              
              (getName program1)
              (loadedUnique loaded3) 
              defs

       -- make sure generated core is valid
       if (not (coreCheck flags)) then return () 
        else Core.Check.checkCore False penv unique4 gamma coreDefs0 

       -- remove return statements
       coreDefsUR <- unreturn penv coreDefs0

       -- do monadic effect translation (i.e. insert binds)
       (isCps,coreDefsMon)
           <- if (not (enableMon flags)) -- CS `elem` targets flags || 
               then return (False,coreDefsUR)
               else do cdefs <- Core.Monadic.monTransform penv coreDefsUR
                       -- recheck cps transformed core
                       when (coreCheck flags) $
                          Core.Check.checkCore True penv unique4 gamma cdefs
                       return (True,cdefs)

       

       -- simplify coreF if enabled
       (coreDefsSimp,unique5)
                  <- if simplify flags < 0  -- if zero, we still run one simplify step to remove open applications 
                      then return (coreDefsMon,unique4)
                      else -- trace "simplify" $ 
                           do let (cdefs,unique4a) -- Core.Simplify.simplify $ 
                                          -- Core.Simplify.simplify 
                                     = simplifyDefs False (simplify flags) unique4 penv coreDefsMon
                              -- recheck simplified core
                              when (not isCps && coreCheck flags) $
                                Core.Check.checkCore isCps penv unique4a gamma cdefs
                              -- and one more unsafe simplify to remove open calls etc.
                              return $ simplifyDefs True 1 unique4a penv cdefs
                              
       -- Assemble core program and return
       let coreProgram2 = -- Core.Core (getName program1) [] [] coreTypeDefs coreDefs0 coreExternals
                          uniquefy $
                          coreProgram1{ Core.coreProgImports = coreImports 
                                      , Core.coreProgDefs = coreDefsSimp
                                      , Core.coreProgFixDefs = [Core.FixDef name fix | FixDef name fix rng <- programFixDefs program1]
                                      }
           loaded4 = loaded3{ loadedGamma = gamma
                            , loadedUnique = unique4
                            , loadedModule = (loadedModule loaded3){ modCore = coreProgram2, modRangeMap = mbRangeMap2 }
                            }

       -- for now, generate C# code here
       return loaded4

  
modulePath mod
  = let path = maybe "" (sourceName . programSource) (modProgram mod)
    in if null path 
        then show nameInteractiveModule
        else path


outBaseName outDir fname
  = if (null outDir)
     then basename fname
     else joinPath outDir (notdir (basename fname))

capitalize s
  = case s of
      c:cs -> toUpper c : cs
      _    -> s

codeGen :: Terminal -> Flags -> CompileTarget Type -> Loaded -> IO Loaded
codeGen term flags compileTarget loaded   
  = do let mod         = loadedModule loaded
           outBase     = outName flags (showModName (modName mod))
               
       let env      = (prettyEnvFromFlags flags){ context = loadedName loaded, importsMap = loadedImportMap loaded }
           outIface = outBase ++ ifaceExtension
           ifaceDoc = Core.Pretty.prettyCore env{ coreIface = True } (modCore mod) <-> Lib.PPrint.empty
       
       -- create output directory if it does not exist
       createDirectoryIfMissing True (dirname outBase)

       -- core
       let outCore  = outBase ++ ".core"
           coreDoc  = Core.Pretty.prettyCore env{ coreIface = False } (modCore mod) <-> Lib.PPrint.empty                 
       when (genCore flags)  $
         do termPhase term "generate core"
            writeDocW 10000 outCore coreDoc  -- just for debugging
       when (showCore flags) $
         do termDoc term coreDoc

       -- write documentation
       let fullHtml = outHtml flags > 1
           outHtmlFile  = outBase ++ "-source.html"
           source   = maybe sourceNull programSource (modProgram mod)
       if (isLiteralDoc (sourceName source)) -- .kk.md
        then do termPhase term "write html document"
                withNewFilePrinter (outBase ++ ".md") $ \printer ->
                 colorize (modRangeMap mod) env (loadedKGamma loaded) (loadedGamma loaded) fullHtml (sourceName source) 1 (sourceBString source) printer
        else when (outHtml flags > 0) $
              do termPhase term "write html source" 
                 withNewFilePrinter outHtmlFile $ \printer ->
                  colorize (modRangeMap mod) env (loadedKGamma loaded) (loadedGamma loaded) fullHtml (sourceName source) 1 (sourceBString source) printer
                 termPhase term "write html documentation" 
                 withNewFilePrinter (outBase ++ ".xmp.html") $ \printer ->
                  genDoc env (loadedKGamma loaded) (loadedGamma loaded) (modCore mod) printer

       mbRuns <- sequence [backendCodeGen term flags (loadedModules loaded)  compileTarget  outBase (modCore mod)  | backendCodeGen <- backends] 

       -- write interface file last so on any error it will not be written
       writeDocW 10000 outIface ifaceDoc
       ftime <- getFileTimeOrCurrent outIface
       let mod1 = (loadedModule loaded){ modTime = ftime }
           loaded1 = loaded{ loadedModule = mod1  }

       -- run the program
       when ((evaluate flags && isExecutable compileTarget)) $
        compilerCatch "program" term () $ 
          case concatMaybe mbRuns of
            (run:_)  -> do termPhase term "run"
                           run
            _        -> return ()

       return loaded1 -- { loadedArities = arities, loadedExternals = externals }
  where
    concatMaybe :: [Maybe a] -> [a]
    concatMaybe mbs  = concatMap (maybe [] (\x -> [x])) mbs

    backends = [codeGenCS, codeGenJS]


-- CS code generation via libraries; this catches bugs in C# generation early on but doesn't take a transitive closure of dll's 
codeGenCSDll:: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (IO()))
codeGenCSDll term flags modules compileTarget outBase core   | not (CS `elem` targets flags)
  = return Nothing
codeGenCSDll term flags modules compileTarget outBase core
  = compilerCatch "csharp" term Nothing $
    do let (mbEntry,isAsync) = case compileTarget of
                                 Executable name tp -> (Just (name,tp), isAsyncFunction tp)
                                 _ -> (Nothing, False)
           cs  = csharpFromCore (maxStructFields flags) (enableMon flags) mbEntry core
           outcs       = outBase ++ ".cs"
           searchFlags = "" -- concat ["-lib:" ++ dquote dir ++ " " | dir <- [outDir flags] {- : includePath flags -}, not (null dir)] ++ " "

       termPhase term $ "generate csharp" ++ maybe "" (\(name,_) -> ": entry: " ++ show name) mbEntry 
       writeDoc outcs cs
       when (showAsmCSharp flags) (termDoc term cs)

       let linkFlags  = concat ["-r:" ++ outName flags (showModName (Core.importName imp)) ++ dllExtension ++ " " 
                                    | imp <- Core.coreProgImports core] -- TODO: link to correct package!
                        ++ "-r:System.Numerics.dll " ++ (if isAsync then "-r:" ++ outName flags "std_async.dll " else "")
           targetName = case compileTarget of
                          Executable _ _ -> dquote ((if null (exeName flags) then outBase else outName flags (exeName flags)) ++ exeExtension)
                          _              -> dquote (outBase ++ dllExtension)
           targetFlags= case compileTarget of
                          Executable _ _ -> "-t:exe -out:" ++ targetName
                          _              -> "-t:library -out:" ++ targetName  
           debugFlags = (if (debug flags) then "-debug " else "") ++ (if (optimize flags >= 0) then "-optimize " else "")
       let cmd = (csc flags ++ " " ++ debugFlags ++ targetFlags ++ " -nologo -warn:4 " ++ searchFlags ++ linkFlags ++ dquote outcs)        
       -- trace cmd $ return () 
       runSystem cmd
       -- run the program
       return (Just (runSystem targetName))

-- Generate C# through CS files without generating dll's
codeGenCS :: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (IO()))
codeGenCS term flags modules compileTarget outBase core   | not (CS `elem` targets flags)
  = return Nothing
codeGenCS term flags modules compileTarget outBase core
  = compilerCatch "csharp" term Nothing $
    do let (mbEntry,isAsync) = case compileTarget of
                                 Executable name tp -> (Just (name,tp), isAsyncFunction tp)
                                 _ -> (Nothing, False)
           cs  = csharpFromCore (maxStructFields flags) (enableMon flags) mbEntry core
           outcs       = outBase ++ ".cs"
           searchFlags = "" -- concat ["-lib:" ++ dquote dir ++ " " | dir <- [outDir flags] {- : includePath flags -}, not (null dir)] ++ " "

       termPhase term $ "generate csharp" ++ maybe "" (\(name,_) -> ": entry: " ++ show name) mbEntry 
       writeDoc outcs cs
       when (showAsmCSharp flags) (termDoc term cs)

       case mbEntry of
         Nothing -> return Nothing
         Just entry -> 
          do let linkFlags  = "-r:System.Numerics.dll " -- ++ (if isAsync then "-r:" ++ outName flags "std_async.dll ")
                 sources    = concat [dquote (outName flags (showModName (modName mod)) ++ ".cs") ++ " " | mod <- modules]
                 targetName = dquote ((if null (exeName flags) then outBase else outName flags (exeName flags)) ++ exeExtension)
                 targetFlags= "-t:exe -out:" ++ targetName ++ " "
                 debugFlags = (if (debug flags) then "-debug -define:DEBUG " else "") ++ (if (optimize flags >= 0) then "-optimize " else "")
             let cmd = (csc flags ++ " " ++ targetFlags ++ debugFlags ++ " -nologo -warn:4 " ++ searchFlags ++ linkFlags ++ sources)        
             trace cmd $ return () 
             runSystem cmd
             -- run the program
             return (Just (runSystem targetName))


codeGenJS :: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (IO ()))
codeGenJS term flags modules compileTarget outBase core  | not (JS `elem` targets flags)
  = return Nothing
codeGenJS term flags modules compileTarget outBase core
  = do let outjs = outBase ++ ".js"
       let mbEntry = case compileTarget of 
                       Executable name tp -> Just (name,isAsyncFunction tp)
                       _                  -> Nothing
       let js    = javascriptFromCore (maxStructFields flags) mbEntry core
       termPhase term ( "generate javascript: " ++ outjs )
       writeDocW 80 outjs js 
       when (showAsmJavaScript flags) (termDoc term js)

       case mbEntry of
        Nothing -> return Nothing
        Just (name) ->
         do -- always generate an index.html file                
            let outHtml = outName flags ((if (null (exeName flags)) then "index" else (exeName flags)) ++ ".html")
                contentHtml = text $ unlines $ [
                                "<!DOCTYPE html>",
                                "<html>",
                                "  <head>",
                                "    <script data-main='" ++ notdir (basename outjs) ++ "' src='require.js'></script>",
                                "  </head>",
                                "  <body>",
                                "  </body>",
                                "</html>"
                              ]  
            termPhase term ("generate index html: " ++ outHtml)
            writeDoc outHtml contentHtml  
            -- try to ensure require.js is there
            -- TODO: we should search along the node_modules search path
            mbReq <- searchPackages (packages flags) (outDir flags) "requirejs" "require.js"
            case mbReq of
              Just reqPath -> copyTextIfNewer (rebuild flags) reqPath (outName flags "require.js")
              Nothing      -> trace "could not find requirejs" $ return () -- TODO: warning?
            
            case host flags of
              Browser ->
               do return (Just (runSystem (dquote outHtml ++ " &")))
              Node ->
               do return (Just (runSystem ("node " ++ outjs))) 



  
copyIFaceToOutputDir :: Terminal -> Flags -> FilePath -> PackageName -> [Module] -> IO ()
copyIFaceToOutputDir term flags iface targetPath imported
  -- | host flags == Node && target flags == JS = return ()
  -- | otherwise  
  = do let outName = joinPaths [outDir flags, targetPath, notdir iface]
       copyTextIfNewer (rebuild flags) iface outName
       if (CS `elem` targets flags)
        then do let libSrc = notext iface ++ dllExtension
                let libOut = notext outName ++ dllExtension
                copyBinaryIfNewer (rebuild flags) libSrc libOut
        else return ()
       if (JS `elem` targets flags)
        then do let jsSrc = notext iface ++ ".js"
                let jsOut = notext outName ++ ".js"
                copyTextFileWith  jsSrc jsOut (packagePatch iface (targetPath) imported)
        else return ()

packagePatch :: FilePath -> PackageName -> [Module] -> (String -> String)
packagePatch iface current imported source
  = let mapping = [("'" ++ modPackagePath imp ++ "/" ++ basename (modPath imp) ++ "'", 
                      "'" ++ makeRelativeToDir (modPackageQPath imp) current ++ "/" ++ basename (modPath imp) ++ "'") 
                    | imp <- imported, not (null (modPackageName imp))]
    in -- trace ("patch: " ++ current ++ "/" ++ notdir iface ++ ":\n  " ++ show mapping) $
       case span (\l -> not (startsWith l "define([")) (lines source) of
         (pre,line:post)
           -> let rline = replaceWith mapping line
              in -- trace ("replace with: " ++ show mapping ++ "\n" ++ line ++ "\n" ++ rline) $
                  unlines $ pre ++ (rline : post)
         _ -> source  
  where
    makeRelativeToDir :: FilePath -> FilePath -> FilePath
    makeRelativeToDir  target current
      = let pre         = commonPathPrefix current target        
            currentStem = drop (length pre) current
            targetStem  = drop (length pre) target
            relative    = concatMap (const "../") $ filter (not . null) $ splitPath currentStem      
        in (if null relative then "./" else relative) ++ targetStem

    replaceWith mapping "" = ""
    replaceWith mapping s
      = case filter (startsWith s . fst) mapping of
          ((pattern,replacement):_) -> replacement ++ replaceWith mapping (drop (length pattern) s)
          _                         -> head s : replaceWith mapping (tail s) 


showModName name
  = moduleNameToPath name -- asciiEncode True (show name)

dquote s
  = "\"" ++ s ++ "\""

outName flags s
  = if (null (outDir flags))
     then s
     else joinPath (outDir flags) s
       

dllExtension 
  = ".dll"

ifaceExtension
  = sourceExtension ++ "i"


compilerCatch comp term defValue io 
  = io `catchSystem` \msg -> 
    do (termError term) (ErrorIO (hang 2 $ text ("failure while running " ++ comp ++ ":")
                                           <-> (fillSep $ map string $ words msg)))
       return defValue


isJust (Just _) = True
isJust _        = False

catchSystem io f
  = io `catchIO` (\msg -> let cmdFailed = "command failed:" in
                          if (cmdFailed `isPrefixOf` msg)
                           then f (drop (length cmdFailed) msg)
                           else f msg)


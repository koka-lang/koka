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
import Data.Char              ( isAlphaNum, toLower, isSpace )

import System.Directory       ( createDirectoryIfMissing, canonicalizePath, getCurrentDirectory )
import Data.List              ( isPrefixOf, intersperse )
import qualified Data.Set as S
import Control.Applicative
import Control.Monad          ( ap, when )
import Common.Failure
import Lib.Printer            ( withNewFilePrinter )
import Common.Range           -- ( Range, sourceName )
import Common.Name            -- ( Name, newName, qualify, asciiEncode )
import Common.NamePrim        ( nameExpr, nameType, nameInteractiveModule, nameSystemCore, nameMain, nameTpWrite, nameTpIO, nameTpCps, nameTpAsync, nameTpNamed )
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
import Core.OpenResolve       ( openResolve )
import Core.FunLift           ( liftFunctions )
import Core.Monadic           ( monTransform )
import Core.MonadicLift       ( monadicLift )
import Core.Inlines           ( inlinesExtends, extractInlineDefs )
import Core.Inline            ( inlineDefs )

import Static.BindingGroups   ( bindingGroups )
import Static.FixityResolve   ( fixityResolve, fixitiesNew, fixitiesCompose )

import Kind.ImportMap
import Kind.Newtypes          ( Newtypes, newtypesCompose )
import Kind.Infer             ( inferKinds )
import Kind.Kind              ( kindEffect )

import Type.Type
import Type.Kind              ( containsHandledEffect, getHandledEffectX, extractHandledEffect )
import Type.Assumption        ( gammaLookupQ, extractGamma, infoType, NameInfo(..), gammaUnions, extractGammaImports, gammaLookup, gammaMap )
import Type.Infer             ( inferTypes )
import Type.Pretty hiding     ( verbose )
import Compiler.Options       ( Flags(..), CC(..), BuildType(..), buildType, ccFlagsBuildFromFlags, unquote,
                                prettyEnvFromFlags, colorSchemeFromFlags, prettyIncludePath, isValueFromFlags,
                                buildDir, outName, configType )

import Compiler.Module

-- needed for code generation
import Data.Char              ( toUpper )
import Lib.PPrint             hiding (dquote)
import Platform.Config        ( version, exeExtension, dllExtension, libPrefix, libExtension, objExtension, pathSep, sourceExtension )

import Backend.CSharp.FromCore    ( csharpFromCore )
import Backend.JavaScript.FromCore( javascriptFromCore )
import Backend.C.FromCore         ( cFromCore )

import qualified Core.Core as Core
import Core.Simplify( simplifyDefs )
import Core.Uniquefy( uniquefy )
import qualified Core.Pretty
import Core.Parse(  parseCore )
import Core.CTail( ctailOptimize )

import System.Directory ( doesFileExist )
import Compiler.Package
-- import qualified Core.Check

-- Debugging
import Lib.Trace


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

instance MonadFail IOErr where
  fail = liftError . fail


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
           -> do ld <- compileProgram' term flags{ evaluate = False } (loadedModules loaded) Object {-compileTarget-}  "<interactive>" programDef
                 let tp = infoType (gammaFind qnameExpr (loadedGamma ld))
                     (_,_,rho) = splitPredType tp
                 -- _ <- liftError $ checkUnhandledEffects flags loaded nameExpr rangeNull rho
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
                                      let defMain = Def (ValueBinder (qualify (getName program) nameMain) () (Lam [] expression r) r r)  r Public (DefFun []) InlineNever ""
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
    = -- trace ("compileModuleOrFile: " ++ show fname ++ ", modules: " ++ show (map modName modules)) $
      do let modName = newName fname
         exist <- searchModule flags "" modName
         case (exist) of
          Just (fpath) -> compileModule term (if force then flags{ forceModule = fpath } else flags)
                                      modules modName
          _       -> do fexist <- searchSourceFile flags "" fname
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
  = let (root,stem) = case findMaximalPrefix paths fname of
                        Just (n,root) -> (root,drop n fname)
                        _             -> ("", fname)
    in -- trace ("relative path of " ++ fname ++ " = " ++ show (root,stem)) $
       (root,stem)


compileModule :: Terminal -> Flags -> Modules -> Name -> IO (Error Loaded)
compileModule term flags modules name  -- todo: take force into account
  = runIOErr $
    do let imp = ImpProgram (Import name name rangeNull Private)
       loaded <- resolveImports name term flags "" initialLoaded{ loadedModules = modules } [imp]
       -- trace ("compileModule: loaded modules: " ++ show (map modName (loadedModules loaded))) $ return ()
       case filter (\m -> modName m == name) (loadedModules loaded) of
         (mod:_) -> return loaded{ loadedModule = mod }
         []      -> fail $ "Compiler.Compile.compileModule: module not found in imports: " ++ show name ++ " not in " ++ show (map (show . modName) (loadedModules loaded))


{---------------------------------------------------------------
  Internal compilation
---------------------------------------------------------------}
compileProgram :: Terminal -> Flags -> Modules -> CompileTarget () -> FilePath -> UserProgram -> IO (Error Loaded)
compileProgram term flags modules compileTarget fname program
  = runIOErr $ compileProgram' term flags modules compileTarget  fname program


compileProgramFromFile :: Terminal -> Flags -> Modules -> CompileTarget () -> FilePath -> FilePath -> IOErr Loaded
compileProgramFromFile term flags modules compileTarget rootPath stem
  = do let fname = joinPath rootPath stem
       -- trace ("compileProgramFromFile: " ++ show fname ++ ", modules: " ++ show (map modName modules)) $ return ()
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
  = do liftIO $ termPhase term ("compile program' " ++ show (getName program))
       ftime <- liftIO (getFileTimeOrCurrent fname)
       let name   = getName program
           outIFace = outName flags (showModName name) ++ ifaceExtension
           mod    = (moduleNull name){
                      modPath = outIFace,
                      modSourcePath = fname,
                      modProgram = (Just program),
                      modCore = failure "Compiler.Compile.compileProgram: recursive module import",
                      modTime = ftime
                    }
           allmods = addOrReplaceModule mod modules
           loaded = initialLoaded { loadedModule = mod
                                  , loadedModules = allmods
                                  }
       -- trace ("compile file: " ++ show fname ++ "\n time: "  ++ show ftime ++ "\n latest: " ++ show (loadedLatest loaded)) $ return ()
       liftIO $ termPhase term ("resolve imports " ++ show (getName program))
       loaded1 <- resolveImports (getName program) term flags (dirname fname) loaded (map ImpProgram (programImports program))
       -- trace (" loaded modules: " ++ show (map modName (loadedModules loaded1))) $ return ()
       -- trace ("inlines: "  ++ show (loadedInlines loaded1)) $ return ()
       if (name /= nameInteractiveModule || verbose flags > 0)
        then liftIO $ termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "check  :") <+>
                                           color (colorSource (colorScheme flags)) (pretty (name)))
        else return ()
       let coreImports = concatMap toCoreImport (loadedModules loaded1) -- (programImports program)
           toCoreImport mod = let vis = case filter (\imp -> modName mod == importFullName imp) (programImports program) of
                                          []      -> Private -- failure $ "Compiler.Compile.compileProgram': cannot find import: " ++ show (modName mod) ++ " in " ++ show (map (show . importName) (programImports program))
                                          (imp:_) -> importVis imp -- TODO: get max
                              in if (modName mod == name) then []
                                  else [Core.Import (modName mod) (modPackagePath mod) vis (Core.coreProgDoc (modCore mod))]
       loaded2a <- liftError $ typeCheck loaded1 flags 0 coreImports program

       -- cull imports to only the real dependencies
       let mod = loadedModule loaded2a
           inlineDefs = case (modInlines mod) of
                          Right defs -> defs
                          Left _     -> []
           deps  = Core.dependencies inlineDefs (modCore mod)
           imps  = filter (\imp -> isPublic (Core.importVis imp) || Core.importName imp == nameSystemCore
                                    || S.member (Core.importName imp) deps) (Core.coreProgImports (modCore mod))
           mod'  = mod{ modCore = (modCore mod){ Core.coreProgImports = imps } }
           loaded2 = loaded2a{ loadedModule = mod' }

       -- codegen
       liftIO $ termPhase term ("codegen " ++ show (getName program))
       (newTarget,loaded3) <- liftError $
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
                               [tp] -> do mbF <- checkUnhandledEffects flags loaded2 mainName rangeNull tp
                                          case mbF of
                                           Nothing -> return (Executable mainName tp, loaded2)
                                           Just f  ->
                                            let mainName2  = qualify (getName program) (newHiddenName "hmain")
                                                r          = rangeNull
                                                expression = App (Var (if (isHiddenName mainName) then mainName -- .expr
                                                                                                  else unqualify mainName -- main
                                                                      ) False r) [] r
                                                defMain    = Def (ValueBinder (unqualify mainName2) () (Lam [] (f expression) r) r r)  r Public (DefFun []) InlineNever  ""
                                                program2   = programAddDefs program [] [defMain]
                                            in do loaded3 <- typeCheck loaded1 flags 0 coreImports program2
                                                  return (Executable mainName2 tp, loaded3) -- TODO: refine the type of main2
                               []   -> errorMsg (ErrorGeneral rangeNull (text "the type of 'main' must be a function without arguments" <->
                                                                                      table [(text "expected type", ppType (prettyEnvFromFlags flags) mainType)
                                                                                            ,(text "inferred type", ppType (prettyEnvFromFlags flags) (head tps))]))
                               _    -> errorMsg (ErrorGeneral rangeNull (text "found multiple definitions for the 'main' function"))
             Object -> return (Object,loaded2)
             Library -> return (Library,loaded2)
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

       loaded4 <- liftIO $ codeGen term flags' newTarget loaded3
       -- liftIO $ termDoc term (text $ show (loadedGamma loaded3))
       -- trace (" final loaded modules: " ++ show (map modName (loadedModules loaded4))) $ return ()
       return loaded4{ loadedModules = addOrReplaceModule (loadedModule loaded4) (loadedModules loaded4) }

checkUnhandledEffects :: Flags -> Loaded -> Name -> Range -> Type -> Error (Maybe (UserExpr -> UserExpr))
checkUnhandledEffects flags loaded name range tp
  = case expandSyn tp of
      TFun _ eff _
        -> let (ls,_) = extractHandledEffect eff
           in -- trace ("extract effects: " ++ show ls) $
              combine eff Nothing ls
      _ -> return Nothing
  where
    exclude = [nameTpCps,nameTpNamed] -- nameTpAsync

    combine :: Effect -> Maybe (UserExpr -> UserExpr) -> [Effect] -> Error (Maybe (UserExpr -> UserExpr))
    combine eff mf [] = return mf
    combine eff mf (l:ls) = case getHandledEffectX exclude l of
                             Nothing -> combine eff mf ls
                             Just (_,effName)
                               -> let defaultHandlerName = makeHiddenName "default" effName
                                  in -- trace ("looking up: " ++ show defaultHandlerName) $
                                     case gammaLookupQ defaultHandlerName (loadedGamma loaded) of
                                        [InfoFun _ dname _ _ _]
                                          -> trace ("add default effect for " ++ show effName) $
                                             let g mfx expr = let r = getRange expr
                                                              in App (Var dname False r) [(Nothing,Lam [] (maybe expr (\f -> f expr) mfx) r)] r
                                             in if (effName == nameTpAsync)  -- always put async as the most outer effect
                                                 then do mf' <- combine eff mf ls
                                                         return (Just (g mf'))
                                                 else combine eff (Just (g mf)) ls
                                        infos
                                          -> -- trace ("not found: " ++ show (loadedGamma loaded)) $
                                             do errorMsg (ErrorGeneral range (text "there are unhandled effects for the main expression" <-->
                                                           text " inferred effect :" <+> ppType (prettyEnvFromFlags flags) eff <-->
                                                           text " unhandled effect:" <+> ppType (prettyEnvFromFlags flags) l <-->
                                                           text " hint            : wrap the main function in a handler"))
                                                combine eff mf ls


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


resolveImports :: Name -> Terminal -> Flags -> FilePath -> Loaded -> [ModImport] -> IOErr (Loaded)
resolveImports mname term flags currentDir loaded0 imports0
  = do -- trace (show mname ++ ": resolving imports: current modules: " ++ show (map (show . modName) (loadedModules loaded0)) ++ "\n") $ return ()
       (imports,resolved) <- resolveImportModules mname term flags currentDir (removeModule mname (loadedModules loaded0)) imports0
       -- trace (show mname ++ ": resolved imports, imported: " ++ show (map (show . modName) imports) ++ "\n  resolved to: " ++ show (map (show . modName) resolved) ++ "\n") $ return ()
       let load msg loaded []
             = return loaded
           load msg loaded (mod:mods)
             = do let (loaded1,errs) = loadedImportModule (isValueFromFlags flags) loaded mod (rangeNull) (modName mod)
                  -- trace ("loaded " ++ msg ++ " module: " ++ show (modName mod)) $ return ()
                  mapM_ (\err -> liftError (errorMsg err)) errs
                  load msg loaded1 mods

           loadInlines :: Loaded -> Module -> IOErr [Core.InlineDef]
           loadInlines loaded mod
             = case modInlines mod of
                 Right idefs -> return idefs
                 Left parseInlines ->
                   do -- trace ("load module inline defs: " ++ show (modName mod)) $ return ()
                      liftError $ parseInlines (loadedGamma loaded) -- process inlines after all have been loaded


       loadedImp  <- load "import" loaded0 imports
       loadedFull <- load "inline import" loaded0 resolved
       inlineDefss   <- mapM (loadInlines loadedFull) resolved
       let modsFull   = zipWith (\mod idefs -> mod{ modInlines = Right idefs }) resolved inlineDefss
           inlineDefs = concat inlineDefss
           inlines    = inlinesExtends inlineDefs (loadedInlines loadedImp)
       -- trace ("resolved inlines: " ++ show (length inlineDefss, length inlineDefs)) $ return ()
       return loadedImp{ loadedModules = modsFull, loadedInlines = inlines }

resolveImportModules :: Name -> Terminal -> Flags -> FilePath -> [Module] -> [ModImport] -> IOErr ([Module],[Module])
resolveImportModules mname term flags currentDir resolved []
  = return ([],resolved)
resolveImportModules mname term flags currentDir resolved0 (imp:imps)
  = do -- trace (show mname ++ ": resolving imported modules: " ++ show (impName imp) ++ ", resolved: " ++ show (map (show . modName) resolved0)) $ return ()
       (mod,resolved1) <- case filter (\m -> impName imp == modName m) resolved0 of
                            (mod:_) -> return (mod,resolved0)
                            _       -> resolveModule term flags currentDir resolved0 imp
       -- trace (" newly resolved from " ++ show (modName mod) ++ ": " ++ show (map (show . modName) resolved1)) $ return ()
       let imports    = Core.coreProgImports $ modCore mod
           pubImports = map ImpCore (filter (\imp -> Core.importVis imp == Public) imports)
       -- trace (" resolve further imports (from " ++ show (modName mod) ++ ") (added module: " ++ show (impName imp) ++ " public imports: " ++ show (map (show . impName) pubImports) ++ ")") $ return ()
       (needed,resolved2) <- resolveImportModules mname term flags currentDir resolved1 (pubImports ++ imps)
       let needed1 = filter (\m -> modName m /= modName mod) needed -- no dups
       return (mod:needed1,resolved2)
       -- trace ("\n\n--------------------\nmodule " ++ show (impName imp) ++ ":\n " ++ show (loadedGamma loaded4)) $ return ()
       -- inlineDefs <- liftError $ (modInlines mod) (loadedGamma loaded4) -- process inlines after pub imports


searchModule :: Flags -> FilePath -> Name -> IO (Maybe FilePath)
searchModule flags currentDir name
  = do mbSource <- searchSource flags currentDir name
       case mbSource of
         Just (root,stem,mname) -> return (Just (joinPath root stem))
         Nothing -> do mbIface <- searchIncludeIface flags currentDir name
                       case mbIface of
                         Nothing -> searchPackageIface flags currentDir Nothing name
                         Just iface -> return (Just iface)


resolveModule :: Terminal -> Flags -> FilePath -> [Module] -> ModImport -> IOErr (Module,[Module])
resolveModule term flags currentDir modules mimp
  = -- trace ("resolve module: " ++ show (impFullName mimp) ++ ", resolved: " ++ show (map (show . modName) modules) ++ ", in " ++ show currentDir) $
    case mimp of
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

             Just (root,stem,mname) -> -- source found, search output iface
               do mbIface <- liftIO $ searchOutputIface flags mname
                  -- trace ("load from program: " ++ show (mbSource,mbIface)) $ return ()
                  case mbIface of
                    Nothing    -> loadFromSource modules root stem
                    Just iface -> loadDepend iface root stem

      -- core import in source
      ImpCore cimp | (null (Core.importPackage cimp)) && (currentDir == buildDir flags) ->
        do mbSource <- liftIO $ searchSource flags "" name
           mbIface  <- liftIO $ searchOutputIface flags name
           -- trace ("source core: found: " ++ show (mbIface,mbSource)) $ return ()
           case (mbIface,mbSource) of
             (Nothing,Nothing)
                -> liftError $ errorMsg $ errorModuleNotFound flags rangeNull name
             (Nothing,Just (root,stem,mname))
                -> loadFromSource modules root stem
             (Just iface,Nothing)
                -> do let cscheme = (colorSchemeFromFlags flags)
                      liftIO $ termDoc term $ color (colorWarning cscheme) $
                         text "warning: interface" <+> color (colorModule cscheme) (pretty name) <+> text "found but no corresponding source module"
                      loadFromIface iface "" ""
             (Just iface,Just (root,stem,mname))
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
                  if (not (rebuild flags) && srcpath /= forceModule flags && ifaceTime >= sourceTime)
                    then loadFromIface iface root stem
                    else loadFromSource modules root stem

      loadFromSource modules1 root fname
        = -- trace ("loadFromSource: " ++ root ++ "/" ++ fname) $
          do loadedImp <- compileProgramFromFile term flags modules1 Object root fname
             let mod = loadedModule loadedImp
                 allmods = addOrReplaceModule mod modules
             return (mod, loadedModules loadedImp)

      loadFromIface iface root stem
        = -- trace ("loadFromIFace: " ++  iface ++ ": " ++ root ++ "/" ++ stem ++ "\n in modules: " ++ show (map modName modules)) $
          do let (pkgQname,pkgLocal) = packageInfoFromDir (packages flags) (dirname iface)
                 loadMessage msg = liftIO $ termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text msg) <+>
                                       color (colorSource (colorScheme flags))
                                         (pretty (if null pkgQname then "" else pkgQname ++ "/") <.>  pretty (name)))
             mod <- case lookupImport iface modules of
                      Just mod
                       -> do loadMessage "reusing:"
                             return mod
                      Nothing
                       -> do loadMessage "loading:"
                             ftime  <- liftIO $ getFileTime iface
                             (core,parseInlines) <- lift $ parseCore iface
                             outIFace <- liftIO $ copyIFaceToOutputDir term flags iface core
                             let mod = Module (Core.coreName core) outIFace (joinPath root stem) pkgQname pkgLocal []
                                                Nothing -- (error ("getting program from core interface: " ++ iface))
                                                  core (Left parseInlines) Nothing ftime
                             return mod
             loadFromModule (modPath mod){-iface-} root stem mod

      loadFromModule iface root source mod
        = -- trace ("load from module: " ++ iface ++ ": " ++ root ++ "/" ++ source) $
          do --     loaded = initialLoaded { loadedModule = mod
             --                            , loadedModules = allmods
             --                            }
             -- (loadedImp,impss) <- resolveImports term flags (dirname iface) loaded (map ImpCore (Core.coreProgImports (modCore mod)))
             (imports,resolved1) <- resolveImportModules name term flags (dirname iface) modules (map ImpCore (Core.coreProgImports (modCore mod)))
             let latest = maxFileTimes (map modTime imports)
             -- trace ("loaded iface: " ++ show iface ++ "\n time: "  ++ show (modTime mod) ++ "\n latest: " ++ show (latest)) $ return ()
             if (latest >= modTime mod
                  && not (null source)) -- happens if no source is present but (package) depencies have updated...
               then loadFromSource resolved1 root source -- load from source after all
               else do liftIO $ copyPkgIFaceToOutputDir term flags iface (modCore mod) (modPackageQPath mod) imports
                       let allmods = addOrReplaceModule mod resolved1
                       return (mod{ modSourcePath = joinPath root source }, allmods)


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
          -> do let reliface = joinPath {- currentDir -} (buildDir flags) postfix  -- TODO: should be currentDir?
                exist <- doesFileExist reliface
                if exist then return (Just reliface)
                 else trace ("no iface at: " ++ reliface) $ return Nothing
         Just package
          -> searchPackages (packages flags) currentDir (head (splitPath package)) postfix


searchOutputIface :: Flags -> Name -> IO (Maybe FilePath)
searchOutputIface flags name
  = do let postfix = showModName name ++ ifaceExtension -- map (\c -> if c == '.' then '_' else c) (show name)
           iface = joinPath (buildDir flags) postfix
       exist <- doesFileExist iface
       -- trace ("search output iface: " ++ show name ++ ": " ++ iface ++ " (" ++ (if exist then "found" else "not found" ) ++ ")") $ return ()
       if exist then return (Just iface)
         else do let libIface = joinPaths [localLibDir flags, configType flags, postfix]
                 libExist <- doesFileExist libIface
                 return (if (libExist) then ({-trace ("found lib iface: " ++ libIface) $ -} Just libIface) else Nothing)

searchSource :: Flags -> FilePath -> Name -> IO (Maybe (FilePath,FilePath,Name {-full mod name relative to root-}))
searchSource flags currentDir name
  = do mbFile <- searchSourceFile flags currentDir (show name)
       case mbFile of
        Just (root,stem)
          -> let mname = case dirname stem of
                           ""  -> name
                           pre -> -- trace ("found source: " ++ showTupled name ++ ", root: " ++ root ++ ", " ++ show pre ++ ", " ++ show (pathToModuleName pre)) $
                                  mergeCommonPath (pathToModuleName pre) name
             in return (Just (root,stem,mname))
        _ -> return Nothing

searchSourceFile :: Flags -> FilePath -> FilePath -> IO (Maybe (FilePath,FilePath))
searchSourceFile flags currentDir fname
  = do -- trace ("search source: " ++ fname ++ " from " ++ concat (intersperse ", " (currentDir:includePath flags))) $ return ()
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
  = -- trace ("typecheck: imports: " ++ show ((map Core.importName) coreImports)) $
    do -- kind inference
       (defs, {- conGamma, -} kgamma, synonyms, newtypes, constructors, {- coreTypeDefs, coreExternals,-} coreProgram1, unique3, mbRangeMap1)
         <- inferKinds
              (isValueFromFlags flags)
              (colorSchemeFromFlags flags)
              (platform flags)
              (if (outHtml flags > 0) then Just rangeMapNew else Nothing)
              (loadedImportMap loaded)
              (loadedKGamma loaded)
              (loadedSynonyms loaded)
              (loadedNewtypes loaded)
              (loadedUnique loaded)
              program1

       let  gamma0  = gammaUnions [loadedGamma loaded
                                  ,extractGamma (isValueFromFlags flags) True coreProgram1
                                  ,extractGammaImports (importsList (loadedImportMap loaded)) (getName program1)
                                  ]

            loaded3 = loaded { loadedKGamma  = kgamma
                            , loadedGamma   = gamma0
                            , loadedSynonyms= synonyms
                            , loadedNewtypes= newtypes -- newtypesCompose (loadedNewtypes loaded) newtypes
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
        else -- trace "initial core check" $
             Core.Check.checkCore False False penv unique4 gamma coreDefs0

       -- remove return statements
       coreDefsUR <- unreturn penv coreDefs0
       -- let coreDefsUR = coreDefs0
       when (coreCheck flags) $ -- trace "return core check" $
                                Core.Check.checkCore False False penv unique4 gamma coreDefsUR

       let showDef def = show (Core.Pretty.prettyDef ((prettyEnvFromFlags flags){coreShowDef=True}) def)
           traceDefGroups title dgs = trace (unlines (["","-----------------", title, "---------------"] ++ map showDef (Core.flattenDefGroups dgs))) $ return ()

       -- traceDefGroups "unreturn" coreDefsUR

       -- lifting recursive functions to top level
       let (coreDefsLifted,uniqueLift) = liftFunctions penv unique4 coreDefsUR
       when (coreCheck flags) $ -- trace "lift functions core check" $
                                Core.Check.checkCore True True penv uniqueLift gamma coreDefsLifted

       -- simplify core
       let ndebug = optimize flags > 0
           (coreDefsSimp0,uniqueSimp0) = simplifyDefs False ndebug (simplify flags) (0) uniqueLift penv coreDefsLifted

       -- traceDefGroups "lifted" coreDefsSimp0


       -- constructor tail optimization
       let (coreDefsCTail,uniqueCTail)
                  = if (optctail flags)
                     then ctailOptimize penv (platform flags) newtypes gamma (optctailInline flags) coreDefsSimp0 uniqueSimp0
                     else (coreDefsSimp0,uniqueSimp0)

       -- traceDefGroups "ctail" coreDefsCTail

       -- do monadic effect translation (i.e. insert binds)
       let uniqueMon = uniqueCTail
       let isPrimitiveModule = Core.coreProgName coreProgram1 == newName "std/core/types" ||
                               Core.coreProgName coreProgram1 == newName "std/core/hnd"
       coreDefsMon
           <- if (not (enableMon flags) || isPrimitiveModule)
               then return (coreDefsCTail)
               else do cdefs <- Core.Monadic.monTransform penv coreDefsCTail
                       -- recheck cps transformed core
                       when (coreCheck flags) $
                          -- trace "monadic core check" $
                          Core.Check.checkCore False False penv uniqueCTail gamma cdefs
                       return (cdefs)

       -- traceDefGroups "monadic" coreDefsMon

       -- resolve phantom .open
       let coreDefsOR = if isPrimitiveModule then coreDefsMon
                         else openResolve penv gamma coreDefsMon
           uniqueOR   = uniqueMon
       when (coreCheck flags) $ -- trace "open resolve core check" $
                                Core.Check.checkCore True False penv uniqueOR gamma coreDefsOR

       -- traceDefGroups "open resolve" coreDefsOR

       -- simplify coreF if enabled
       (coreDefsSimp,uniqueSimp)
                  <- if simplify flags < 0  -- if zero, we still run one simplify step to remove open applications
                      then return (coreDefsOR,uniqueOR)
                      else -- trace "simplify" $
                           do let (cdefs0,unique0) -- Core.Simplify.simplify $
                                          -- Core.Simplify.simplify
                                     = simplifyDefs False ndebug (simplify flags) (simplifyMaxDup flags) uniqueOR penv coreDefsOR
                              -- recheck simplified core
                              when (coreCheck flags) $
                                -- trace "after simplify core check 1" $
                                Core.Check.checkCore True False penv unique0 gamma cdefs0
                              return (cdefs0,unique0) -- $ simplifyDefs False 1 unique4a penv cdefs

       -- traceDefGroups "open resolve simplified" coreDefsSimp

       {-
       -- do monadic effect translation (i.e. insert binds)
       let uniqueMon = uniqueSimp
       coreDefsMon
           <- if (not (enableMon flags) ||
                  Core.coreProgName coreProgram1 == newName "std/core/types" ||
                  Core.coreProgName coreProgram1 == newName "std/core/hnd" )
               then return (coreDefsSimp)
               else do cdefs <- Core.Monadic.monTransform penv coreDefsSimp
                       -- recheck cps transformed core
                       when (coreCheck flags) $
                          trace "monadic core check" $ Core.Check.checkCore False False penv uniqueLift gamma cdefs
                       return (cdefs)

       traceDefGroups "monadic" coreDefsMon
       -}

       let (coreDefsMonL,uniqueMonL) = monadicLift penv uniqueSimp coreDefsSimp
       when (coreCheck flags) $ -- trace "monadic lift core check" $
                                Core.Check.checkCore True True penv uniqueMonL gamma coreDefsMonL
       -- traceDefGroups "monadic lift" coreDefsMonL

       -- do an inlining pass
       let (coreDefsInl,uniqueInl) = inlineDefs penv uniqueMonL (loadedInlines loaded3) coreDefsMonL
       when (coreCheck flags) $ -- trace "inlined functions core check" $
                                Core.Check.checkCore True True penv uniqueInl gamma coreDefsInl

       -- and one more simplify
       (coreDefsSimp2,uniqueSimp2)
                  <- if simplify flags < 0  -- if zero, we still run one simplify step to remove open applications
                      then return (coreDefsInl,uniqueInl)
                      else -- trace "simplify" $
                           do let (cdefs0,unique0) -- Core.Simplify.simplify $
                                          -- Core.Simplify.simplify
                                     = simplifyDefs False ndebug (simplify flags) (simplifyMaxDup flags) uniqueInl penv coreDefsInl
                              -- recheck simplified core
                              when (coreCheck flags) $
                                -- trace "after simplify core check 2" $
                                Core.Check.checkCore True True penv unique0 gamma cdefs0
                              return (cdefs0,unique0) -- $ simplifyDefs False 1 unique4a penv cdefs

       -- traceDefGroups "inlined simplified" coreDefsSimp2

{-
       -- and one more simplify
       (coreDefsSimp3,uniqueSimp3)
                  <- if simplify flags < 0  -- if zero, we still run one simplify step to remove open applications
                      then return (coreDefsLifted,uniqueLift)
                      else -- trace "simplify" $
                           do let (cdefs0,unique0) -- Core.Simplify.simplify $
                                          -- Core.Simplify.simplify
                                     = simplifyDefs False (simplify flags) (simplifyMaxDup flags) uniqueLift penv coreDefsLifted
                              -- recheck simplified core
                              when (coreCheck flags) $
                                trace "after simplify core check 3" $Core.Check.checkCore True True penv unique0 gamma cdefs0
                              return (cdefs0,unique0) -- $ simplifyDefs False 1 unique4a penv cdefs
-}
       -- Assemble core program and return
       let coreDefsLast = coreDefsSimp2
           uniqueLast   = uniqueSimp2
           inlineDefs   = extractInlineDefs (coreInlineMax penv) coreDefsLast

           coreProgram2 = -- Core.Core (getName program1) [] [] coreTypeDefs coreDefs0 coreExternals
                          uniquefy $
                          coreProgram1{ Core.coreProgImports = coreImports
                                      , Core.coreProgDefs = coreDefsLast  -- coreDefsSimp
                                      , Core.coreProgFixDefs = [Core.FixDef name fix | FixDef name fix rng <- programFixDefs program1]
                                      }

           loaded4 = loaded3{ loadedGamma = gamma
                            , loadedUnique = uniqueLast
                            , loadedModule = (loadedModule loaded3){
                                                modCore = coreProgram2,
                                                modRangeMap = mbRangeMap2,
                                                modInlines  = Right inlineDefs
                                              }
                            , loadedInlines = inlinesExtends inlineDefs (loadedInlines loaded3)
                            }

       return loaded4


modulePath mod
  = let path = maybe "" (sourceName . programSource) (modProgram mod)
    in if null path
        then show nameInteractiveModule
        else path


capitalize s
  = case s of
      c:cs -> toUpper c : cs
      _    -> s


codeGen :: Terminal -> Flags -> CompileTarget Type -> Loaded -> IO Loaded
codeGen term flags compileTarget loaded
  = compilerCatch "code generation" term loaded $
    do let mod         = loadedModule loaded
           outBase     = outName flags (showModName (modName mod))

       let env      = (prettyEnvFromFlags flags){ context = loadedName loaded, importsMap = loadedImportMap loaded }
           outIface = outBase ++ ifaceExtension
           inlineDefs = case modInlines mod of
                          Right defs -> defs
                          Left _     -> []
           ifaceDoc = Core.Pretty.prettyCore env{ coreIface = True } inlineDefs (modCore mod) <-> Lib.PPrint.empty

       -- create output directory if it does not exist
       createDirectoryIfMissing True (dirname outBase)

       -- remove existing kki file in case of errors
       removeFileIfExists outIface

       -- core
       let outCore  = outBase ++ ".core"
           coreDoc  = Core.Pretty.prettyCore env{ coreIface = False, coreShowDef = (showCore flags) } inlineDefs (modCore mod)
                        <-> Lib.PPrint.empty
       when (genCore flags)  $
         do termPhase term "generate core"
            writeDocW 10000 outCore coreDoc  -- just for debugging
       when (showCore flags && target flags /= C) $
         do termDoc term coreDoc

       -- write documentation
       let fullHtml = outHtml flags > 1
           outHtmlFile  = outBase ++ "-source.html"
           source   = maybe sourceNull programSource (modProgram mod)
           cenv     = env{ colorizing=True }
       if (isLiteralDoc (sourceName source)) -- .kk.md
        then do termPhase term "write html document"
                withNewFilePrinter (outBase ++ ".md") $ \printer ->
                 colorize (modRangeMap mod) cenv (loadedKGamma loaded) (loadedGamma loaded) fullHtml (sourceName source) 1 (sourceBString source) printer
        else when (outHtml flags > 0) $
              do termPhase term "write html source"
                 withNewFilePrinter outHtmlFile $ \printer ->
                  colorize (modRangeMap mod) cenv (loadedKGamma loaded) (loadedGamma loaded) fullHtml (sourceName source) 1 (sourceBString source) printer
                 termPhase term "write html documentation"
                 withNewFilePrinter (outBase ++ ".xmp.html") $ \printer ->
                  genDoc cenv (loadedKGamma loaded) (loadedGamma loaded) (modCore mod) printer

       mbRun <- backend term flags (loadedModules loaded)  compileTarget  outBase (modCore mod)

       -- write interface file last so on any error it will not be written
       writeDocW 10000 outIface ifaceDoc
       ftime <- getFileTimeOrCurrent outIface
       let mod1 = (loadedModule loaded){ modTime = ftime }
           loaded1 = loaded{ loadedModule = mod1  }

       -- run the program
       when ((evaluate flags && isExecutable compileTarget)) $
        compilerCatch "program run" term () $
          case mbRun of
            Just run -> do termPhase term $ "evaluate"
                           termDoc term $ space
                           run
            _        -> termDoc term $ space

       return loaded1 -- { loadedArities = arities, loadedExternals = externals }
  where
    concatMaybe :: [Maybe a] -> [a]
    concatMaybe mbs  = concatMap (maybe [] (\x -> [x])) mbs

    backend :: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (IO()))
    backend  = case target flags of
                 CS -> codeGenCS
                 JS -> codeGenJS
                 _  -> codeGenC (modSourcePath (loadedModule loaded)) (loadedNewtypes loaded) (loadedUnique loaded)


-- CS code generation via libraries; this catches bugs in C# generation early on but doesn't take a transitive closure of dll's
codeGenCSDll:: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (IO()))
codeGenCSDll term flags modules compileTarget outBase core
  = compilerCatch "csharp compilation" term Nothing $
    do let (mbEntry,isAsync) = case compileTarget of
                                 Executable name tp -> (Just (name,tp), isAsyncFunction tp)
                                 _ -> (Nothing, False)
           cs  = csharpFromCore (enableMon flags) mbEntry core
           outcs       = outBase ++ ".cs"
           searchFlags = "" -- concat ["-lib:" ++ dquote dir ++ " " | dir <- [buildDir flags] {- : includePath flags -}, not (null dir)] ++ " "

       termPhase term $ "generate csharp" ++ maybe "" (\(name,_) -> ": entry: " ++ show name) mbEntry
       writeDoc outcs cs
       when (showAsmCS flags) (termDoc term cs)

       let linkFlags  = concat ["-r:" ++ outName flags (showModName (Core.importName imp)) ++ dllExtension ++ " "
                                    | imp <- Core.coreProgImports core] -- TODO: link to correct package!
                        ++ "-r:System.Numerics.dll " ++ (if isAsync then "-r:" ++ outName flags "std_async.dll " else "")
           targetName = case compileTarget of
                          Executable _ _ -> dquote ((if null (exeName flags) then outBase else outName flags (exeName flags)) ++ exeExtension)
                          _              -> dquote (outBase ++ dllExtension)
           targetFlags= case compileTarget of
                          Executable _ _ -> "-t:exe -out:" ++ targetName
                          _              -> "-t:library -out:" ++ targetName
           debugFlags = (if (debug flags) then "-debug " else "") ++ (if (optimize flags >= 1) then "-optimize " else "")
       let cmd = (csc flags ++ " " ++ debugFlags ++ targetFlags ++ " -nologo -warn:4 " ++ searchFlags ++ linkFlags ++ dquote outcs)
       -- trace cmd $ return ()
       runSystemEcho term flags cmd
       -- run the program
       return (Just (runSystemEcho term flags targetName))

-- Generate C# through CS files without generating dll's
codeGenCS :: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (IO()))
codeGenCS term flags modules compileTarget outBase core
  = compilerCatch "csharp compilation" term Nothing $
    do let (mbEntry,isAsync) = case compileTarget of
                                 Executable name tp -> (Just (name,tp), isAsyncFunction tp)
                                 _ -> (Nothing, False)
           cs  = csharpFromCore (enableMon flags) mbEntry core
           outcs       = outBase ++ ".cs"
           searchFlags = "" -- concat ["-lib:" ++ dquote dir ++ " " | dir <- [buildDir flags] {- : includePath flags -}, not (null dir)] ++ " "

       termPhase term $ "generate csharp" ++ maybe "" (\(name,_) -> ": entry: " ++ show name) mbEntry
       writeDoc outcs cs
       when (showAsmCS flags) (termDoc term cs)

       case mbEntry of
         Nothing -> return Nothing
         Just entry ->
          do let linkFlags  = "-r:System.Numerics.dll " -- ++ (if isAsync then "-r:" ++ outName flags "std_async.dll ")
                 sources    = concat [dquote (outName flags (showModName (modName mod)) ++ ".cs") ++ " " | mod <- modules]
                 targetName = dquote ((if null (exeName flags) then outBase else outName flags (exeName flags)) ++ exeExtension)
                 targetFlags= "-t:exe -out:" ++ targetName ++ " "
                 debugFlags = (if (debug flags) then "-debug -define:DEBUG " else "") ++ (if (optimize flags >= 1) then "-optimize " else "")
             let cmd = (csc flags ++ " " ++ targetFlags ++ debugFlags ++ " -nologo -warn:4 " ++ searchFlags ++ linkFlags ++ sources)
             runSystemEcho term flags cmd
             -- run the program
             return (Just (runSystemEcho term flags targetName))


codeGenJS :: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (IO ()))
codeGenJS term flags modules compileTarget outBase core
  = do let outjs = outBase ++ ".js"
       let mbEntry = case compileTarget of
                       Executable name tp -> Just (name,isAsyncFunction tp)
                       _                  -> Nothing
           extractImport mod = Core.Import (modName mod) (modPackageQName mod) Public ""
       let js    = javascriptFromCore mbEntry (map extractImport modules) core
       termPhase term ( "generate javascript: " ++ outjs )
       writeDocW 80 outjs js
       when (showAsmJS flags) (termDoc term js)

       case mbEntry of
        Nothing -> return Nothing
        Just (name) ->
         do -- always generate an index.html file
            let outHtml = outName flags ((if (null (exeName flags)) then "index" else (exeName flags)) ++ ".html")
                contentHtml = text $ unlines $ [
                                "<!DOCTYPE html>",
                                "<html>",
                                "  <head>",
                                "    <meta charset=\"utf-8\">",
                                "    <script data-main='" ++ notdir (basename outjs) ++ "' src=\"node_modules/requirejs/require.js\"></script>",
                                "  </head>",
                                "  <body>",
                                "  </body>",
                                "</html>"
                              ]
            termPhase term ("generate index html: " ++ outHtml)
            writeDoc outHtml contentHtml
            -- copy amdefine
            let copyNodeModules fname
                  = let nname = "node_modules/" ++ fname
                    in copyTextIfNewer (rebuild flags) (joinPath (localShareDir flags) nname) (outName flags nname)
            mapM_ copyNodeModules ["amdefine/amdefine.js","amdefine/package.json",
                                   "requirejs/require.js","requirejs/package.json"]


            -- try to ensure require.js is there
            -- TODO: we should search along the node_modules search path
            {-mbReq <- searchPackages (packages flags) (buildDir flags) "requirejs" "require.js"
            case mbReq of
              Just reqPath -> copyTextIfNewer (rebuild flags) reqPath (outName flags "require.js")
              Nothing      -> trace "could not find requirejs" $ return () -- TODO: warning?
            -}
            case host flags of
              Browser ->
               do return (Just (runSystemEcho term flags (dquote outHtml ++ " &")))
              _ ->
               do return (Just (runCommand term flags ["node","--stack-size=100000",outjs]))




codeGenC :: FilePath -> Newtypes -> Int -> Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (IO ()))
codeGenC sourceFile newtypes unique0 term flags modules compileTarget outBase core0
 = -- compilerCatch "c compilation" term Nothing $
   do let outC = outBase ++ ".c"
          outH = outBase ++ ".h"
          sourceDir = dirname sourceFile
      let mbEntry = case compileTarget of
                      Executable name tp -> Just (name,isAsyncFunction tp)
                      _                  -> Nothing
      let -- (core,unique) = parcCore (prettyEnvFromFlags flags) newtypes unique0 core0
          (cdoc,hdoc,bcore) = cFromCore sourceDir (prettyEnvFromFlags flags) (platform flags)
                                newtypes unique0 (parcReuse flags) (parcSpecialize flags) (parcReuseSpec flags)
                                mbEntry core0
          bcoreDoc  = Core.Pretty.prettyCore (prettyEnvFromFlags flags){ coreIface = False, coreShowDef = True } [] bcore
      -- writeDocW 120 (outBase ++ ".c.core") bcoreDoc
      when (showCore flags) $
        do termDoc term bcoreDoc

      termPhase term ( "generate c: " ++ outBase )
      writeDocW 120 outC (cdoc <.> linebreak)
      writeDocW 120 outH (hdoc <.> linebreak)
      when (showAsmC flags) (termDoc term (hdoc <//> cdoc))

      -- copy libraries
      let cc       = ccomp flags
          eimports = externalImportsFromCore bcore
          clibs    = clibsFromCore bcore 
      mapM_ (copyCLibrary term flags cc) eimports

      -- compile
      let ccompile = concat $
                     [ [ccPath cc]
                     , ccFlags cc
                     , ccFlagsWarn cc
                     , ccFlagsCompile cc
                     , ccFlagsBuildFromFlags cc flags
                     , ccIncludeDir cc (localShareDir flags ++ "/kklib/include")
                     ]
                     ++
                     map (ccIncludeDir cc) (ccompIncludeDirs flags)
                     ++
                     map (ccAddDef cc) ((if (asan flags) then [] else ["KK_MIMALLOC","MI_MAX_ALIGN_SIZE=8"])
                                        ++ ["KK_STATIC_LIB"])
                     ++
                     [ ccTargetObj cc outBase
                     , [outC]
                     ]

      runCommand term flags ccompile

      -- compile and link?
      case mbEntry of
       Nothing -> return Nothing
       Just _ ->
         do cmakeGeneratorFlag  -- prefer Ninja if available
              <- if onWindows then return ["-G","Ninja"]  -- we must use Ninja on windows (as we cannot handle multi-config cmake)
                  else do paths   <- getEnvPaths "PATH"
                          mbNinja <- searchPaths paths [exeExtension] "ninja"
                          case mbNinja of
                            Just ninja -> do -- termDoc term $ text "found ninja:" <+> pretty ninja
                                             return ["-G","Ninja"]
                            Nothing    -> return []

            checkCMake term flags
            currentDir <- getCurrentDirectory
            -- kklibInstallDir = joinPath kklibDir "out/install"
            -- installKKLib term flags kklibDir kklibInstallDir cmakeGeneratorFlag cmakeConfigTypeFlag configType

            let mainModName= showModName (Core.coreProgName core0)
                mainName   = if null (exeName flags) then mainModName else exeName flags
                mainExe    = outName flags mainName

            -- build kklib for the specified build variant
            cmakeLib term flags cc "kklib" (ccLibFile cc "kklib") cmakeGeneratorFlag

            let objs   = [outName flags (ccObjFile cc (showModName mname)) | mname <- (map modName modules ++ [Core.coreProgName core0])]
                syslibs= concat [csyslibsFromCore mcore | mcore <- map modCore modules]
                         ++ ccompLinkSysLibs flags
                         ++ (if onWindows then ["bcrypt","psapi","advapi32"]
                                          else ["m","pthread"])
                libs   = ["kklib"] -- [normalizeWith '/' (outName flags (ccLibFile cc "kklib"))] ++ ccompLinkLibs flags
                         ++ 
                         clibs 
                         ++
                         concat [clibsFromCore mcore | mcore <- map modCore modules]

                libpaths = map (\lib -> outName flags (ccLibFile cc lib)) libs
 
                clink  = concat $
                         [ [ccPath cc]
                         , ccFlags cc
                         , ccFlagsBuildFromFlags cc flags
                         , ccTargetExe cc mainExe
                         ]
                         ++ [objs]
                         ++ [ccFlagsLink cc]  -- must be last due to msvc
                         -- ++ [ccAddLibraryDir cc (buildDir flags)]
                         ++ map (ccAddLib cc) libpaths  -- libs
                         ++ map (ccAddSysLib cc) syslibs
                         

            termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "linking:") <+>
                               color (colorSource (colorScheme flags)) (text mainName))
            runCommand term flags clink

            termPhaseDoc term $ color (colorInterpreter (colorScheme flags)) (text "created:") <+>
                                  color (colorSource (colorScheme flags)) (text (normalizeWith pathSep mainExe))
            let cmdflags = if (showElapsed flags) then " --kktime" else ""
            return (Just (runSystemEcho term flags (dquote mainExe ++ cmdflags ++ " " ++ execOpts flags))) -- use shell for proper rss accounting


copyCLibrary :: Terminal -> Flags -> CC -> [(String,String)] -> IO ()
copyCLibrary term flags cc eimport
  = do let clib = case lookup "library" eimport of
                    Just lib -> lib
                    Nothing  -> case lookup "vcpkg" eimport of
                                  Just pkg -> pkg
                                  Nothing  -> ""
       if (null clib) then nosuccess clib else 
        do mbPath <- searchPaths (ccompLibDirs flags) [] (ccLibFile cc clib)
           case mbPath of
              Nothing   -> do fname <- vcpkgInstall term flags cc eimport clib
                              if (null fname)
                                then nosuccess clib
                                else do exist <- doesFileExist fname
                                        if (not exist) 
                                          then nosuccess fname
                                          else copyLibFile fname
              Just fname -> copyLibFile fname
  where
    nosuccess clib
      = raiseIO ("unable to find C library " ++ clib)
    copyLibFile fname
      = do termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "library:") <+>
              color (colorSource (colorScheme flags)) (text fname))          
           copyBinaryIfNewer False fname (outName flags (notdir fname))
                         
           

termWarning term flags doc
  = termDoc term $ color (colorWarning (colorSchemeFromFlags flags)) (text "warning:" <+> doc)

vcpkgInstall :: Terminal -> Flags -> CC -> [(String,String)] -> FilePath -> IO FilePath
vcpkgInstall term flags cc eimport clib | onWindows && (ccName cc `startsWith` "mingw")
  = do termWarning term flags $
        text "unable to find C library:" <+> color (colorSource (colorScheme flags)) (text clib) <->
        text "   hint: currently using the \"mingw\" compiler but to use external \".lib\" libraries" <->
        text "         on Windows you need to use the \"clang-cl\" or \"cl\" (msvc) compiler." <->
        text "         run from an 'x64 Native Tools Command' window and install clang-cl from" <-> 
        text "         <https://releases.llvm.org/download.html>"
       return ""

vcpkgInstall term flags cc eimport clib
  = case lookup "vcpkg" eimport of
      Nothing  -> 
        do termWarning term flags $
              text "unable to find C library:" <+> color (colorSource (colorScheme flags)) (text clib) <->
              text "   hint: provide \"--cclibdir\" as an option, or use \"syslib\" in an extern import?"
           return ""
      Just pkg -> 
        do vcpkgExist <- doesFileExist (vcpkg flags)
           if (not vcpkgExist)
             then do termWarning term flags ( 
                       text ("this module requires vcpkg to install: " ++ clib) <->
                       text ("   hint: specify the root directory of vcpkg using the \"--vcpkg=<dir>\" option") <->
                       text ("         and/or install \"vcpkg\" from <https://github.com/microsoft/vcpkg#getting-started>") <->
                       text ("         (install in \"~/vcpkg\" to be found automatically by the koka compiler)")
                       )
                     return ""
             else do pkgExist <- doesFileExist (joinPaths [vcpkgRoot flags,"packages",pkg ++ "_" ++ vcpkgTriplet flags])
                     if (pkgExist)  
                       then termWarning term flags $ text ("vcpkg \"" ++ pkg ++ "\" is installed but the library \"" ++ clib ++ "\" is not found.")
                       else return ()
                     let install = [vcpkg flags,
                                    "install",
                                    pkg ++ ":" ++ vcpkgTriplet flags,
                                    "--disable-metrics"]                               
                     if (not (vcpkgAutoInstall flags))
                       then do termWarning term flags (text "this module requires the vcpkg package" 
                                                        <+> color (colorSource (colorScheme flags)) (text pkg) 
                                                        <+> text "-- install the package as:" 
                                        <-> text "         >" <+> color (colorSource (colorScheme flags)) (text (unwords install))
                                        <-> text "         to install the required C library and header files")
                               return ""
                       else do termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "install: vcpkg package:") <+>
                                 color (colorSource (colorScheme flags)) (text pkg))
                               runCommand term flags install
                               return (joinPaths [vcpkgLibDir flags,ccLibFile cc clib])
                    

clibsFromCore core    = externalImportKeyFromCore core "library"
csyslibsFromCore core = externalImportKeyFromCore core "syslib"

externalImportKeyFromCore :: Core.Core -> String -> [String]
externalImportKeyFromCore core key
  = [val  | (key',val) <- concat (externalImportsFromCore core), key == key']

externalImportsFromCore :: Core.Core -> [[(String,String)]]
externalImportsFromCore core 
  = [keyvals  | Core.ExternalImport imports _ <- Core.coreProgExternals core, (C,keyvals) <- imports]


cmakeLib :: Terminal -> Flags -> CC -> String -> FilePath -> [String] -> IO ()
cmakeLib term flags cc libName {-kklib-} libFile {-libkklib.a-} cmakeGeneratorFlag
  = do let libPath = outName flags libFile  {-out/v2.x.x/clang-debug/libkklib.a-}
       exist <- doesFileExist libPath
       let binLibPath = joinPath (localLibDir flags) (configType flags ++ "/" ++ libFile)
       let srcLibDir  = joinPath (localShareDir flags) (libName)
       binExist <- doesFileExist binLibPath
       binNewer <- if (not binExist) then return False
                   else if (not exist) then return True
                   else do cmp <- fileTimeCompare binLibPath libPath
                           return (cmp==GT)
       srcNewer <- if (binNewer) then return False -- no need to check
                   else if (not exist) then return True
                   else do cmp <- fileTimeCompare (srcLibDir ++ "/include/kklib.h") libPath
                           return (cmp==GT)
       -- putStrLn ("binLibPath: " ++ binLibPath ++ ", newer: " ++ show binNewer)
       if (not binNewer && not srcNewer && not (rebuild flags)) then return ()
         else if (binNewer)
           then -- use pre-compiled installed binary
                copyBinaryFile binLibPath libPath
           else -- todo: check for installed binaries for the library
                do termDoc term $ color (colorInterpreter (colorScheme flags)) (text ("cmake  :")) <+>
                                   color (colorSource (colorScheme flags)) (text libName) <+>
                                    color (colorInterpreter (colorScheme flags)) (text "from:") <+>
                                     color (colorSource (colorScheme flags)) (text srcLibDir)
                   let cmakeDir    = outName flags libName
                       cmakeConfigType = "-DCMAKE_BUILD_TYPE=" ++
                                         (case (buildType flags) of
                                             Debug -> "Debug"
                                             RelWithDebInfo -> "RelWithDebInfo"
                                             Release -> "Release")
                       cmakeConfig =  [ cmake flags
                                      , "-E", "chdir", cmakeDir   -- see above for chdir
                                      , cmake flags
                                      ]
                                      ++ cmakeGeneratorFlag ++
                                      [ cmakeConfigType
                                      , "-DCMAKE_C_COMPILER=" ++ (basename (ccPath cc))
                                      , "-DCMAKE_INSTALL_PREFIX=" ++ (buildDir flags)
                                      , "-DKK_COMP_VERSION=" ++ version
                                      , (if (asan flags) then "-DKK_DEBUG_SAN=address" else "")
                                      ]
                                      ++ unquote (cmakeArgs flags) ++
                                      [ srcLibDir ]

                       cmakeBuild  = [cmake flags, "--build", cmakeDir]
                       -- cmakeInstall= cmake flags ++ " --build " ++ dquote cmakeDir ++ " --target install"   -- switch "--install" is not available before cmake 3.15
                   createDirectoryIfMissing True cmakeDir
                   runCommand term flags cmakeConfig
                   runCommand term flags cmakeBuild
                   copyBinaryFile (cmakeDir ++ "/" ++ libFile) libPath


-- emit helpful messages if dependencies are not installed (cmake etc)
checkCMake :: Terminal -> Flags -> IO ()
checkCMake term flags
  = do paths   <- getEnvPaths "PATH"
       mbCMake <- searchPaths paths [exeExtension] (cmake flags)
       case mbCMake of
         Nothing -> do termDoc term (text ("error: '" ++ cmake flags ++ "' command cannot be found.") <->
                                     text " hint: install from <https://cmake.org/download/> or using 'sudo apt-get install cmake'")
                       return ()
         Just _  -> if (not (onWindows))
                     then return ()
                     else do vsDir <- getEnvVar "VSINSTALLDIR"
                             if (null vsDir)
                              then -- bare windows prompt: check for Ninja, and if using clang
                                   do mbNinja <- searchPaths paths [exeExtension] "ninja"
                                      case mbNinja of
                                        Nothing -> do termDoc term (text "error: 'ninja' build system not found" <->
                                                                    text " hint: run from a Visual Studio 'x64 Native Tools Command Prompt'" <->
                                                                    text "       or install 'ninja' from <https://github.com/ninja-build/ninja/releases>")
                                                      return ()
                                        Just _  -> return ()
                                                   {-do cc <- getEnvVar "CC"
                                                      if (cc=="clang")
                                                       then do termDoc term (text "When using 'clang' you must run in a 'x64 Native Tools Command Prompt'")
                                                               return ()
                                                       else return ()-}
                              else -- visual studio prompt
                                   return ()



onWindows :: Bool
onWindows
  = (exeExtension == ".exe")

runSystemEcho :: Terminal -> Flags -> String -> IO ()
runSystemEcho term flags cmd
  = do when (verbose flags >= 2) $
         termPhase term ("shell> " ++ cmd)
       runSystem cmd

runCommand :: Terminal -> Flags -> [String] -> IO ()
runCommand term flags cargs@(cmd:args)
  = do when (verbose flags >= 2) $
         termPhase term ("command> " ++ cmd ++ " [" ++ concat (intersperse "," args) ++ "]")
       runCmd cmd (filter (not . null) args)


joinWith sep xs
  = concat (intersperse sep xs)

copyIFaceToOutputDir :: Terminal -> Flags -> FilePath -> Core.Core -> IO FilePath
copyIFaceToOutputDir term flags iface core
  -- | host flags == Node && target flags == JS = return ()
  -- | otherwise
  = do let outIFace = outName flags (notdir iface)
           withExt fname ext = notext fname ++ ext
       -- trace ("copy iface: " ++ iface ++ " to " ++ outIFace) $ return ()
       copyTextIfNewer (rebuild flags) iface outIFace
       if (CS == target flags)
        then do let libSrc = notext iface ++ dllExtension
                let libOut = notext outIFace ++ dllExtension
                copyBinaryIfNewer (rebuild flags) libSrc libOut
        else return ()
       if (JS == target flags)
        then do let jsSrc = notext iface ++ ".js"
                let jsOut = notext outIFace ++ ".js"
                -- copyTextFileWith  jsSrc jsOut (packagePatch iface (targetPath) imported)
                copyTextIfNewer (rebuild flags) jsSrc jsOut
        else return ()
       if (C == target flags)
        then do copyTextIfNewer (rebuild flags) (withExt iface ".c") (withExt outIFace ".c")
                copyTextIfNewer (rebuild flags) (withExt iface ".h") (withExt outIFace ".h")
                let cc = ccomp flags
                    srcDir = dirname iface
                copyBinaryIfNewer (rebuild flags) (ccObjFile cc (notext iface)) (ccObjFile cc (notext outIFace))
                mapM_ (\clib -> 
                  do let libFile = ccLibFile cc clib
                     -- todo: only copy if it exists?
                     copyBinaryIfNewer (rebuild flags) (joinPath srcDir libFile) (outName flags libFile)
                 ) (clibsFromCore core)
        else return ()
       return outIFace

copyPkgIFaceToOutputDir :: Terminal -> Flags -> FilePath -> Core.Core -> PackageName -> [Module] -> IO ()
copyPkgIFaceToOutputDir term flags iface core targetPath imported
  -- | host flags == Node && target flags == JS = return ()
  -- | otherwise
  = do outIFace <- copyIFaceToOutputDir term flags iface core
       if (JS == target flags)
        then do let outJs = notext outIFace ++ ".js"
                content <- readTextFile outJs
                case content of
                  Nothing -> return ()
                  Just content -> writeTextFile outJs (packagePatch iface (targetPath) imported content)
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


posixOutName flags s
 = normalizeWith '/' (outName flags s)


ifaceExtension
  = sourceExtension ++ "i"


compilerCatch comp term defValue io
  = io `catchSystem` \msg ->
    do (termError term) (ErrorIO (hang 2 $ text ("failure during " ++ comp ++ ":")
                                           <-> (fillSep $ map string $ words msg)))
       return defValue


isJust (Just _) = True
isJust _        = False

catchSystem io f
  = io `catchIO` (\msg -> let cmdFailed = "command failed:" in
                          if (cmdFailed `isPrefixOf` msg)
                           then f (drop (length cmdFailed) msg)
                           else f msg)

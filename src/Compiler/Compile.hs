-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Main module.
-}
-----------------------------------------------------------------------------
{-# LANGUAGE InstanceSigs #-}
module Compiler.Compile( -- * Compile
                         compileFile
                       , compileModule
                       , compileModuleOrFile

                         -- * Interpreter
                       , compileExpression, compileType
                       , compileValueDef, compileTypeDef
                       , compileProgram
                       , gammaFind
                       , codeGen

                         -- * Types
                       , Module(..)
                       , Loaded(..), initialLoaded, loadedName
                       , Terminal(..)
                       , searchSource
                       , CompileTarget(..)
                       ) where

import Lib.Trace              ( trace )
import Data.Char              ( isAlphaNum, toLower, isSpace )

import System.Directory       ( createDirectoryIfMissing, canonicalizePath, getCurrentDirectory, doesDirectoryExist )
import Data.Maybe             ( catMaybes, fromJust )
import Data.List              ( isPrefixOf, intersperse, intercalate )
import qualified Data.Set as S
import Control.Applicative
import Control.Monad          ( ap, when )
import qualified Control.Monad.Fail as F
import Common.Failure
import Lib.Printer            ( withNewFilePrinter )
import Common.Range           -- ( Range, sourceName )
import Common.Name            -- ( Name, newName, qualify, asciiEncode )
import Common.NamePrim        ( nameCoreHnd, isPrimitiveModule, nameExpr, nameType, nameInteractiveModule, nameSystemCore, nameMain, nameTpWrite, nameTpIO, nameTpCps, nameTpAsync, nameTpNamed, isPrimitiveName )
import Common.Error
import Common.File
import Common.ColorScheme
import Common.Message         ( table )
import Common.Syntax
import Common.Unique
import Syntax.Syntax
-- import Syntax.Lexer           ( readInput )
import Syntax.Parse           ( parseProgramFromFile, parseValueDef, parseExpression, parseTypeDef, parseType, parseProgramFromString )

import Syntax.RangeMap
import Syntax.Colorize        ( colorize )
import Core.GenDoc            ( genDoc )
import Core.Check             ( checkCore )
import Core.UnReturn          ( unreturn )
import Core.CheckFBIP         ( checkFBIP )
import Core.OpenResolve       ( openResolve )
import Core.FunLift           ( liftFunctions )
import Core.Monadic           ( monTransform )
import Core.MonadicLift       ( monadicLift )
import Core.Inlines           ( inlinesExtends, extractInlineDefs, inlinesMerge, inlinesToList, inlinesFilter, inlinesNew )
import Core.Borrowed          ( Borrowed, borrowedExtendICore )
import Core.Inline            ( inlineDefs )
import Core.Specialize
import Core.Unroll            ( unrollDefs )

import Static.BindingGroups   ( bindingGroups )
import Static.FixityResolve   ( fixityResolve, fixitiesNew, fixitiesCompose )

import Kind.ImportMap
import Kind.Newtypes          ( Newtypes, newtypesCompose, extractNewtypes )
import Kind.Infer             ( inferKinds )
import Kind.Kind              ( kindEffect )

import Type.Type
import Type.Kind              ( containsHandledEffect, getHandledEffectX, extractHandledEffect )
import Type.Assumption        ( gammaLookupQ, extractGamma, infoType, NameInfo(..), gammaUnions, extractGammaImports, gammaLookup, gammaMap )
import Type.Infer             ( inferTypes )
import Type.Pretty hiding     ( verbose )
import Compiler.Options       ( Flags(..), CC(..), BuildType(..), buildType, ccFlagsBuildFromFlags, unquote,
                                prettyEnvFromFlags, colorSchemeFromFlags, prettyIncludePath, isValueFromFlags,
                                fullBuildDir, outName, buildVariant, osName, targetExeExtension,
                                conanSettingsFromFlags, vcpkgFindRoot, onWindows, onMacOS, Mode (ModeLanguageServer))

import Compiler.Module

-- needed for code generation
import Data.Char              ( toUpper )
import Lib.PPrint             hiding (dquote)
import Platform.Config        ( version, exeExtension, dllExtension, libPrefix, libExtension, pathSep, sourceExtension )

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

import qualified Data.Map
import Core.Core (Core(coreProgImports))


{--------------------------------------------------------------------------
  Compilation
--------------------------------------------------------------------------}

data Terminal = Terminal{ termError :: ErrorMessage -> IO ()
                        , termPhase :: String -> IO ()
                        , termPhaseDoc :: Doc -> IO ()
                        , termType  :: Scheme -> IO ()
                        , termDoc   :: Doc -> IO ()
                        }


data IOErr b a = IOErr (IO (Error b a))

runIOErr :: IOErr b a -> IO (Error b a)
runIOErr (IOErr ie) = ie

liftErrorPartial :: c -> Error b a -> IOErr c a
liftErrorPartial partialT err = IOErr (return $ setPartial (Just partialT) err)

liftError :: Error c a -> IOErr c a
liftError err = IOErr (return err)

liftIO :: IO a -> IOErr b a
liftIO io = IOErr (do x <- io
                      return (return x))

lift :: IO (Error b a) -> IOErr b a
lift ie   = IOErr ie

instance Functor (IOErr b) where
  fmap f (IOErr ie)  = IOErr (fmap (fmap f) ie)

instance Applicative (IOErr b) where
  pure x = IOErr (return (return x))
  (<*>) = ap

instance Monad (IOErr b) where
  -- return = pure
  (IOErr ie) >>= f  = IOErr (do err <- ie
                                case checkPartial err of
                                   Right (x,w,b) -> case f x of
                                                   IOErr ie' -> do err <- ie'
                                                                   return (addPartialResult (addWarnings w err) b)
                                   Left (msg, b)  -> return (errorMsgPartial msg b))

instance F.MonadFail (IOErr b) where
  fail = liftError . fail


bindIO :: IO (Error b a) -> (a -> IO (Error b c)) -> IO (Error b c)
bindIO io f
  = do err <- io
       case checkPartial err of
         Left (msg, b) -> return (errorMsgPartial msg b)
         Right (x,w,b)  -> fmap (flip addPartialResult b . addWarnings w) (f x)

gammaFind name g
  = case (gammaLookupQ name g) of
      [tp] -> tp
      []   -> failure ("Compiler.Compile.gammaFind: can't locate " ++ show name)
      _    -> failure ("Compiler.Compile.gammaFind: multiple definitions for " ++ show name)

compileExpression :: (FilePath -> Maybe (BString, FileTime)) -> Terminal -> Flags -> Loaded -> CompileTarget () -> UserProgram -> Int -> String -> IO (Error Loaded (Loaded, Maybe FilePath))
compileExpression maybeContents term flags loaded compileTarget program line input
  = runIOErr $
    do let qnameExpr = (qualify (getName program) nameExpr)
       def <- liftErrorPartial loaded (parseExpression (semiInsert flags) (show nameInteractiveModule) line qnameExpr input)
       let programDef = programAddDefs program [] [def]
       -- specialized code: either just call the expression, or wrap in a show function
       case compileTarget of
         -- run a particular entry point
         Executable name ()  | name /= nameExpr
           -> compileProgram' maybeContents term flags (loadedModules loaded) compileTarget "<interactive>" programDef []
         -- entry point is the expression: compile twice:
         --  first to get the type of the expression and create a 'show' wrapper,
         --  then to actually run the program
           | otherwise
           -> do (ld, f) <- compileProgram' maybeContents term flags{ evaluate = False } (loadedModules loaded) Object {-compileTarget-}  "<interactive>" programDef []
                 let tp = infoType (gammaFind qnameExpr (loadedGamma ld))
                     (_,_,rho) = splitPredType tp
                 -- _ <- liftError $ checkUnhandledEffects flags loaded nameExpr rangeNull rho
                 case splitFunType rho of
                   -- return unit: just run the expression (for its assumed side effect)
                   Just (_,_,tres)  | isTypeUnit tres
                      -> compileProgram' maybeContents term flags (loadedModules ld) compileTarget  "<interactive>" programDef []
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
                                      let defMain = Def (ValueBinder (qualify (getName program) nameMain) () (Lam [] expression r) r r)  r Public (defFun []) InlineNever ""
                                      let programDef' = programAddDefs programDef [] [defMain]
                                      compileProgram' maybeContents term flags (loadedModules ld) (Executable nameMain ()) "<interactive>" programDef' []

                              _  -> liftErrorPartial loaded $ errorMsg (ErrorGeneral rangeNull (text "no 'show' function defined for values of type:" <+> ppType (prettyEnvFromFlags flags) tres))
                                                     -- mkApp (Var (qualify nameSystemCore (newName "gprintln")) False r)
                                                     --   [mkApp (Var nameExpr False r) []]
                   Nothing
                    -> failure ("Compile.Compile.compileExpression: should not happen")
         -- no evaluation
         _ -> compileProgram' (const Nothing)  term flags (loadedModules loaded) compileTarget "<interactive>" programDef []


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


compileType :: Terminal -> Flags -> Loaded -> UserProgram -> Int -> String -> IO (Error Loaded Loaded)
compileType term flags loaded program line input
  = runIOErr $
    do let qnameType = qualify (getName program) nameType
       tdef <- liftErrorPartial loaded $ parseType (semiInsert flags) (show nameInteractiveModule) line nameType input
       let programDef = programAddDefs (programRemoveAllDefs program) [tdef] []
       -- typeCheck (loaded) flags line programDef
       (ld, _) <- compileProgram' (const Nothing) term flags (loadedModules loaded) Object "<interactive>" programDef []
       return ld


compileValueDef :: Terminal -> Flags -> Loaded -> UserProgram -> Int -> String -> IO (Error Loaded (Name,Loaded))
compileValueDef term flags loaded program line input
  = runIOErr $
    do def <- liftErrorPartial loaded $ parseValueDef (semiInsert flags) (show nameInteractiveModule) line input
       let programDef = programAddDefs program [] [def]
       (ld, _) <- compileProgram' (const Nothing) term flags (loadedModules loaded) Object "<interactive>" programDef []
       return (qualify (getName program) (defName def),ld)

compileTypeDef :: Terminal -> Flags -> Loaded -> UserProgram -> Int -> String -> IO (Error Loaded (Name,Loaded))
compileTypeDef term flags loaded program line input
  = runIOErr $
    do (tdef,cdefs) <- liftErrorPartial loaded $ parseTypeDef (semiInsert flags) (show nameInteractiveModule) line input
       let programDef = programAddDefs program [tdef] cdefs
       (ld, _) <- compileProgram' (const Nothing) term flags (loadedModules loaded) Object "<interactive>" programDef []
       return (qualify (getName program) (typeDefName tdef),ld)


{---------------------------------------------------------------
  compileFile/Module
  These are meant to be called from the interpreter/main compiler
---------------------------------------------------------------}

compileModuleOrFile :: (FilePath -> Maybe (BString, FileTime)) -> Maybe BString -> Terminal -> Flags -> Modules -> String -> Bool -> CompileTarget () -> [Name] -> IO (Error Loaded (Loaded, Maybe FilePath))
compileModuleOrFile maybeContents contents term flags modules fname force compileTarget importPath
  | any (not . validModChar) fname = compileFile maybeContents contents term flags modules compileTarget importPath fname
  | otherwise
    = -- trace ("compileModuleOrFile: " ++ show fname ++ ", modules: " ++ show (map modName modules)) $
      do
        let modName = pathToModuleName fname
        exist <- searchModule flags "" modName
        case (exist) of
          Just (fpath) -> compileModule term (if force then flags{ forceModule = fpath } else flags)
                                      modules modName compileTarget importPath
          _       -> do
            fexist <- searchSourceFile flags "" fname
            runIOErr $
              case (fexist) of
                Just (root,stem)
                  -> compileProgramFromFile maybeContents contents term flags modules Object importPath root stem
                Nothing
                  -> liftError $ errorMsg $ errorFileNotFound flags fname
  where
    validModChar c
      = isAlphaNum c || c `elem` "/_"

compileFile :: (FilePath -> Maybe (BString, FileTime)) -> Maybe BString -> Terminal -> Flags -> Modules -> CompileTarget () -> [Name] -> FilePath -> IO (Error Loaded (Loaded, Maybe FilePath))
compileFile maybeContents contents term flags modules compileTarget importPath fpath0
  = runIOErr $
    do file1 <- liftIO $ realPath fpath0
       let fpath = normalize file1
       mbP <- liftIO $ searchSourceFile flags "" fpath
       case mbP of
         Nothing -> liftError $ errorMsg (errorFileNotFound flags fpath)
         Just (root,stem)
           -> do
                -- trace ("Includes : " ++ show (includePath flags)) $ return ()
                -- trace ("Root: " ++ root ++ " STEM: " ++ stem) $ return ()
                compileProgramFromFile maybeContents contents term flags modules compileTarget importPath root stem

-- | Make a file path relative to a set of given paths: return the (maximal) root and stem
-- if it is not relative to the paths, return dirname/notdir
makeRelativeToPaths :: [FilePath] -> FilePath -> (FilePath,FilePath)
makeRelativeToPaths paths fname
  = let (root,stem) = case findMaximalPrefix paths fname of
                        Just (n,root) -> (root,drop n fname)
                        _             -> ("", fname)
    in -- trace ("relative path of " ++ fname ++ " with paths " ++ show paths ++ " = " ++ show (root,stem)) $
       (root,stem)


compileModule :: Terminal -> Flags -> Modules -> Name -> CompileTarget () -> [Name] -> IO (Error Loaded (Loaded, Maybe FilePath))
compileModule term flags modules name compileTarget importPath -- todo: take force into account
  = runIOErr $
    do let imp = ImpProgram (Import name name rangeNull Private)
       loaded <- resolveImports (const Nothing) (newName "") term flags "" initialLoaded{ loadedModules = modules } importPath [imp]
       -- trace ("compileModule: loaded modules: " ++ show (map modName (loadedModules loaded))) $ return ()
       case filter (\m -> modName m == name) (loadedModules loaded) of
         (mod:_) -> return (loaded{ loadedModule = mod }, Nothing)
         []      -> fail $ "Compiler.Compile.compileModule: module not found in imports: " ++ show name ++ " not in " ++ show (map (show . modName) (loadedModules loaded))


{---------------------------------------------------------------
  Internal compilation
---------------------------------------------------------------}
compileProgram :: Terminal -> Flags -> Modules -> CompileTarget () -> FilePath -> UserProgram -> [Name] -> IO (Error Loaded (Loaded, Maybe FilePath))
compileProgram term flags modules compileTarget fname program importPath
  = runIOErr $ compileProgram' (const Nothing) term flags modules compileTarget fname program importPath

compileProgramFromFile :: (FilePath -> Maybe (BString, FileTime)) -> Maybe BString -> Terminal -> Flags -> Modules -> CompileTarget () -> [Name] -> FilePath -> FilePath -> IOErr Loaded (Loaded, Maybe FilePath)
compileProgramFromFile maybeContents contents term flags modules compileTarget importPath rootPath stem
  = do fname0 <- liftIO $ realPath $ joinPath rootPath stem
       let fname = normalize fname0
       -- trace ("compileProgramFromFile: " ++ show fname ++ ", modules: " ++ show (map modName modules)) $ return ()
       liftIO $ termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "compile:") <+> color (colorSource (colorScheme flags)) (text (normalizeWith '/' fname)))
       exist <- liftIO $ doesFileExist fname
       if (exist) then return () else liftError $ errorMsg (errorFileNotFound flags fname)
       program <- lift $ case contents of { Just x -> return $ parseProgramFromString (semiInsert flags) x fname; _ -> parseProgramFromFile (semiInsert flags) fname}
       let isSuffix = -- asciiEncode True (noexts stem) `endsWith` asciiEncode True (show (programName program))
                      -- map (\c -> if isPathSep c then '/' else c) (noexts stem)
                      show (pathToModuleName (noexts stem)) `endsWith` show (programName program)
                      -- map (\c -> if isPathSep c then '/' else c) (noexts stem)
                      --  `endsWith` moduleNameToPath (programName program)
           ppcolor c doc = color (c (colors (prettyEnvFromFlags flags))) doc
       if (isExecutable compileTarget || isSuffix) then return ()
        else liftError $ errorMsg (ErrorGeneral (programNameRange program)
                                     (text "module name" <+>
                                      ppcolor colorModule (pretty (programName program)) <+>
                                      text "is not a suffix of the file path" <+>
                                      parens (ppcolor colorSource $ text $ dquote $ stem)
                                     ))
       let stemName = nameFromFile stem
      --  let flags2 = flags{forceModule = fname}
       compileProgram' maybeContents term flags modules compileTarget fname program{ programName = stemName } importPath

nameFromFile :: FilePath -> Name
nameFromFile fname
  = pathToModuleName $ dropWhile isPathSep $ noexts fname

data CompileTarget a
  = Object
  | Library
  | Executable { entry :: Name, info :: a } deriving Show

isExecutable (Executable _ _) = True
isExecutable _ = False

compileProgram' :: (FilePath -> Maybe (BString, FileTime)) -> Terminal -> Flags -> Modules -> CompileTarget () -> FilePath -> UserProgram -> [Name] -> IOErr Loaded (Loaded, Maybe FilePath)
compileProgram' maybeContents term flags modules compileTarget fname program importPath
  = do liftIO $ termPhase term ("compile program' " ++ show (getName program))
       let name   = getName program
           outIFace = outName flags (showModName name) ++ ifaceExtension
       ftime <- liftIO (getCurrentFileTime fname maybeContents)
       let mod    = (moduleNull name){
                      modPath = outIFace,
                      modSourcePath = fname,
                      modProgram = (Just program),
                      modCore = failure ("Compiler.Compile.compileProgram: recursive module import (" ++ fname ++ ")"),
                      modTime = ftime
                    }
           allmods = addOrReplaceModule mod modules
           loaded = initialLoaded { loadedModule = mod
                                  , loadedModules = allmods
                                  }
       -- trace ("compile file: " ++ show fname ++ "\n time: "  ++ show ftime ++ "\n latest: " ++ show (loadedLatest loaded)) $ return ()
       liftIO $ termPhase term ("resolve imports " ++ show (getName program))
       loaded1 <- resolveImports maybeContents (getName program) term flags (dirname fname) loaded importPath (map ImpProgram (programImports program))
       -- trace (" loaded modules: " ++ show (map modName (loadedModules loaded1))) $ return ()
       --trace ("------\nloaded1:\n" ++ show (loadedNewtypes loaded1) ++ "\n----") $ return ()
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

       (loaded2a, coreDoc) <- liftErrorPartial loaded1 $ typeCheck loaded1 flags 0 coreImports program
       when (showCore flags) $
         liftIO (termDoc term (vcat [
           text "-------------------------",
           text "core",
           text "-------------------------",
           coreDoc,
           text "-------------------------"
         ]))

       -- use time of type check as modTime
       time <- liftIO getCurrentTime
       -- cull imports to only the real dependencies
       let mod = loadedModule loaded2a
           inlineDefs = case (modInlines mod) of
                          Right defs -> defs
                          Left _     -> []
           deps  = Core.dependencies inlineDefs (modCore mod)
           imps  = filter (\imp -> isPublic (Core.importVis imp) || Core.importName imp == nameSystemCore
                                    || S.member (Core.importName imp) deps) (Core.coreProgImports (modCore mod))
           mod'  = mod{ modCore = (modCore mod){ Core.coreProgImports = imps }, modTime = time }
           loaded2 = loaded2a{ loadedModule = mod' }

       -- codegen
       liftIO $ termPhase term ("codegen " ++ show (getName program))
       (newTarget,loaded3) <- wrapMain term flags loaded2 loaded1 compileTarget program coreImports
       (loaded4, outFile) <- liftIO $ do
              (loadedNew, mbRun) <- codeGen term flags newTarget loaded3
              -- run the program
              when (evaluate flags && isExecutable newTarget) $
                compilerCatch "program run" term () $
                  case mbRun of
                    Just (_,run) -> do termPhase term "evaluate"
                                       termDoc term space
                                       run
                    _  -> termDoc term space
              return (loadedNew, fmap fst mbRun)
       -- liftIO $ termDoc term (text $ show (loadedGamma loaded4))
       --trace (" final loaded modules: " ++ show (map modName (loadedModules loaded4))) $ return ()
       return (loaded4{ loadedModules = addOrReplaceModule (loadedModule loaded4) (loadedModules loaded4) }, outFile)

wrapMain :: Terminal -> Flags -> Loaded -> Loaded -> CompileTarget a -> Program UserType UserKind -> [Core.Import] -> IOErr Loaded (CompileTarget Scheme, Loaded)
wrapMain  term flags loaded0 loaded1 compileTarget program coreImports = do
  liftIO $ termPhase term ("wrapping main " ++ show (getName program))
  liftErrorPartial loaded1 $
      case compileTarget of
        Executable entryName _
          -> let mainName = if (isQualified entryName) then entryName else qualify (getName program) (entryName) in
            case gammaLookupQ mainName (loadedGamma loaded0) of
                []   -> errorMsg (ErrorGeneral rangeNull (text "there is no 'main' function defined" <-> text "hint: use the '-l' flag to generate a library?"))
                infos-> let mainType = TFun [] (TCon (TypeCon nameTpIO kindEffect)) typeUnit  -- just for display, so IO can be TCon
                            isMainType tp = case expandSyn tp of
                                              TFun [] eff resTp  -> True -- resTp == typeUnit
                                              _                  -> False
                        in case filter (isMainType . infoType) infos of
                          [InfoFun{infoType=tp,infoRange=r}]
                            -> do mbF <- checkUnhandledEffects flags loaded0 mainName r tp
                                  case mbF of
                                    Nothing -> return (Executable mainName tp, loaded0)
                                    Just f  ->
                                      let mainName2  = qualify (getName program) (newHiddenName "hmain")
                                          expression = App (Var (if (isHiddenName mainName) then mainName -- .expr
                                                                                            else unqualify mainName -- main
                                                                ) False r) [] r
                                          defMain    = Def (ValueBinder (unqualify mainName2) () (Lam [] (f expression) r) r r)  r Public (defFun []) InlineNever ""
                                          program2   = programAddDefs program [] [defMain]
                                      in do (loaded3,_) <- ignoreWarnings $ typeCheck loaded1 flags 0 coreImports program2
                                            return (Executable mainName2 tp, loaded3) -- TODO: refine the type of main2
                          [info]
                            -> errorMsg (ErrorGeneral (infoRange info) (text "'main' must be declared as a function (fun)"))
                          [] -> errorMsg (ErrorGeneral rangeNull (text "the type of 'main' must be a function without arguments" <->
                                                                                table [(text "expected type", ppType (prettyEnvFromFlags flags) mainType)
                                                                                      ,(text "inferred type", ppType (prettyEnvFromFlags flags) (head (map infoType infos)))]))
                          _  -> errorMsg (ErrorGeneral rangeNull (text "found multiple definitions for the 'main' function"))
        Object -> return (Object,loaded0)
        Library -> return (Library,loaded0)

checkUnhandledEffects :: Flags -> Loaded -> Name -> Range -> Type -> Error Loaded (Maybe (UserExpr -> UserExpr))
checkUnhandledEffects flags loaded name range tp
  = case expandSyn tp of
      TFun _ eff _
        -> let (ls,_) = extractHandledEffect eff
           in -- trace ("extract effects: " ++ show ls) $
              combine eff Nothing ls
      _ -> return Nothing
  where
    exclude = [nameTpCps,nameTpNamed] -- nameTpAsync

    combine :: Effect -> Maybe (UserExpr -> UserExpr) -> [Effect] -> Error Loaded (Maybe (UserExpr -> UserExpr))
    combine eff mf [] = return mf
    combine eff mf (l:ls) = case getHandledEffectX exclude l of
                             Nothing -> combine eff mf ls
                             Just (_,effName)
                               -> let defaultHandlerName = makeHiddenName "default" effName
                                  in -- trace ("looking up: " ++ show defaultHandlerName) $
                                     case gammaLookupQ defaultHandlerName (loadedGamma loaded) of
                                        [fun@InfoFun{}]
                                          -> trace ("add default effect for " ++ show effName) $
                                             let dname = infoCName fun
                                                 g mfx expr = let r = getRange expr
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


resolveImports :: (FilePath -> Maybe (BString, FileTime)) -> Name -> Terminal -> Flags -> FilePath -> Loaded -> [Name] -> [ModImport] -> IOErr Loaded Loaded
resolveImports maybeContents mname term flags currentDir loaded0 importPath imports0
  = do -- trace (show mname ++ ": resolving imports: current modules: " ++ show (map (show . modName) (loadedModules loaded0)) ++ "\n") $ return ()
       (imports,resolved) <- resolveImportModules maybeContents mname term flags currentDir (removeModule mname (loadedModules loaded0)) (mname:importPath) imports0
       -- trace (show mname ++ ": resolved imports, imported: " ++ show (map (show . modName) imports) ++ "\n  resolved to: " ++ show (map (show . modName) resolved) ++ "\n") $ return ()
       let load msg loaded []
             = return loaded
           load msg loaded (mod:mods)
             = do let (loaded1,errs) = loadedImportModule (isValueFromFlags flags) loaded mod (rangeNull) (modName mod)
                  -- trace ("loaded " ++ msg ++ " module: " ++ show (modName mod)) $ return ()
                  mapM_ (\err -> liftErrorPartial loaded0 (errorMsg err)) errs
                  load msg loaded1 mods

           loadInlines :: Loaded -> Module -> IOErr Loaded [Core.InlineDef]
           loadInlines loaded mod
             = case modInlines mod of
                 Right idefs -> return idefs
                 Left parseInlines ->
                   do -- trace ("load module inline defs: " ++ show (modName mod)) $ return ()
                      liftErrorPartial loaded $ parseInlines (loadedGamma loaded) -- process inlines after all have been loaded


       loadedImp  <- load "import" loaded0 imports
       loadedFull <- load "inline import" loaded0 resolved
       inlineDefss   <- mapM (loadInlines loadedFull) resolved
       let modsFull   = zipWith (\mod idefs -> mod{ modInlines = Right idefs }) resolved inlineDefss
           inlineDefs = concat inlineDefss
           inlines    = inlinesExtends inlineDefs (loadedInlines loadedImp)
       -- trace ("resolved inlines: " ++ show (length inlineDefss, length inlineDefs)) $ return ()
       return loadedImp{ loadedModules = modsFull, loadedInlines = inlines }

resolveImportModules :: (FilePath -> Maybe (BString, FileTime)) -> Name -> Terminal -> Flags -> FilePath -> [Module] -> [Name] -> [ModImport] -> IOErr Loaded ([Module],[Module])
resolveImportModules maybeContents mname term flags currentDir resolved importPath []
  = return ([],resolved)
resolveImportModules maybeContents mname term flags currentDir resolved0 importPath (imp:imps)
  = if impName imp `elem` importPath then do
        liftError $ errorMsg $ ErrorStatic [(getRange imp, text "cyclic module dependency detected when importing: " <+> ppName (prettyEnvFromFlags flags) mname <+> text " import path: " <-> vsep (reverse (map (ppName (prettyEnvFromFlags flags)) importPath)))]
        return (resolved0,resolved0)
      else
    do -- trace ("\t" ++ show mname ++ ": resolving imported modules: " ++ show (impName imp) ++ ", resolved: " ++ show (map (show . modName) resolved0) ++ ", path:" ++ show importPath) $ return ()
       (mod,resolved1) <- case filter (\m -> impName imp == modName m) resolved0 of
                            (mod:_) -> return (mod,resolved0)
                            _       -> resolveModule maybeContents term flags currentDir resolved0 importPath imp
       -- trace ("\tnewly resolved from " ++ show (modName mod) ++ ": " ++ show (map (show . modName) resolved1)) $ return ()
       let imports    = Core.coreProgImports $ modCore mod
           pubImports = map ImpCore (filter (\imp -> Core.importVis imp == Public) imports)
       -- trace (" resolve further imports (from " ++ show (modName mod) ++ ") (added module: " ++ show (impName imp) ++ " public imports: " ++ show (map (show . impName) pubImports) ++ ")") $ return ()
       (needed,resolved2) <- resolveImportModules maybeContents mname term flags currentDir resolved1 importPath (pubImports ++ imps)
       let needed1 = filter (\m -> modName m /= modName mod) needed -- no dups
       return (mod:needed1,resolved2)


searchModule :: Flags -> FilePath -> Name -> IO (Maybe FilePath)
searchModule flags currentDir name
  = do mbSource <- searchSource flags currentDir name
       case mbSource of
         Just (root,stem,mname) -> return (Just (joinPath root stem))
         Nothing -> do mbIface <- searchIncludeIface flags currentDir name
                       case mbIface of
                         Nothing -> searchPackageIface flags currentDir Nothing name
                         Just iface -> return (Just iface)

getCurrentFileTime :: FilePath ->  (FilePath -> Maybe (BString, FileTime)) -> IO FileTime
getCurrentFileTime fp maybeContents = do
  f <- realPath fp
  case maybeContents (normalize f) of
    Just (_, t) -> return t
    Nothing -> getFileTimeOrCurrent fp

maybeGetCurrentFileTime :: FilePath ->  (FilePath -> Maybe (BString, FileTime)) -> IO (Maybe FileTime)
maybeGetCurrentFileTime fp maybeContents = do
  f <- realPath fp
  case maybeContents (normalize f) of
    Just (_, t) -> return $ Just t
    Nothing -> do
      -- trace ("File " ++ show fp ++ " not in virtual filesystem") $ return ()
      ft <- getFileTime fp
      if ft == fileTime0 then return Nothing else return $ -- (trace $ "Get maybe " ++ show ft) $
        Just ft

resolveModule :: (FilePath -> Maybe (BString, FileTime)) -> Terminal -> Flags -> FilePath -> [Module] -> [Name] -> ModImport -> IOErr Loaded (Module,[Module])
resolveModule maybeContents term flags currentDir modules importPath mimp
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
      ImpCore cimp | (null (Core.importPackage cimp)) && (currentDir == fullBuildDir flags) ->
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
         = -- trace ("loadDepend " ++ iface ++ " root: " ++ root ++ " stem: " ++ stem) $
           do let srcpath = joinPath root stem
              ifaceTime <- liftIO $ getCurrentFileTime iface maybeContents
              sourceTime <- liftIO $ getCurrentFileTime srcpath maybeContents
              -- trace ("loadDepend: " ++ show (ifaceTime, sourceTime)) $ return ()
              case lookupImport iface modules of
                  Just mod ->
                    if (srcpath /= forceModule flags && modTime mod >= sourceTime)
                     then -- trace ("module " ++ show (name) ++ " already loaded") $
                          return (mod,modules) -- TODO: revise! do proper dependency checking instead..
                    else if (not (rebuild flags) && srcpath /= forceModule flags && ifaceTime >= sourceTime)
                        then loadFromIface iface root stem
                    else loadFromSource modules root stem
                  Nothing ->
                    -- trace ("module " ++ show (name) ++ " not yet loaded") $
                    if (not (rebuild flags) && srcpath /= forceModule flags && ifaceTime >= sourceTime)
                      then loadFromIface iface root stem
                      else loadFromSource modules root stem

      loadFromSource modules1 root fname
        = -- trace ("loadFromSource: " ++ show force ++ " " ++ " update " ++ show genUpdate ++ " root:" ++ root ++ " stem:" ++ stem) $
        do
          (loadedImp, _) <- compileProgramFromFile maybeContents (fst <$> maybeContents (normalize fname)) term flags modules1 Object importPath root fname
          let mod = loadedModule loadedImp
              allmods = addOrReplaceModule mod modules
          return (mod, loadedModules loadedImp)

      loadFromIface iface root stem
        = -- trace ("loadFromIFace: " ++  iface ++ ": root:" ++ root ++ " stem:" ++ stem ++ "\n in modules: " ++ show (map modName modules)) $
          do let (pkgQname,pkgLocal) = packageInfoFromDir (packages flags) (dirname iface)
                 loadMessage msg = liftIO $ termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text msg) <+>
                                       color (colorSource (colorScheme flags))
                                         (pretty (if null pkgQname then "" else pkgQname ++ "/") <.>  pretty (name)))
             mod <- case lookupImport iface modules of
                      Just mod
                       -> do loadMessage "reusing:"
                             return mod
                      Nothing
                       -> do
                          loadMessage "loading:"
                          ftime <- liftIO $ getFileTime iface
                          mbCore <- liftIO $ parseCore iface
                          (core,parseInlines) <- liftError mbCore
                          -- let core = uniquefy core0
                          outIFace <- liftIO $ copyIFaceToOutputDir term flags iface core
                          let mod = Module (Core.coreName core) outIFace (joinPath root stem) pkgQname pkgLocal []
                                              Nothing -- (error ("getting program from core interface: " ++ iface))
                                                core (Left parseInlines) True Nothing ftime
                          return mod
             loadFromModule (modPath mod){-iface-} root stem mod

      loadFromModule iface root source mod
        = -- trace ("load from module: " ++ iface ++ ": " ++ root ++ "/" ++ source) $
          do --     loaded = initialLoaded { loadedModule = mod
             --                            , loadedModules = allmods
             --                            }
             -- (loadedImp,impss) <- resolveImports term flags (dirname iface) loaded (map ImpCore (Core.coreProgImports (modCore mod)))
             (imports,resolved1) <- resolveImportModules maybeContents name term flags (dirname iface) modules (name:importPath) (map ImpCore (Core.coreProgImports (modCore mod)))
             let latest = maxFileTimes (map modTime imports)

             -- trace ("loaded iface: " ++ show iface ++ "\n time: "  ++ show (modTime mod) ++ "\n latest: " ++ show (latest)) $ return ()
             if (latest >= modTime mod
                  && not (null source)) -- happens if no source is present but (package) depencies have updated...
               then loadFromSource resolved1 root source -- load from source after all
               else do liftIO $ copyPkgIFaceToOutputDir term flags iface (modCore mod) (modPackageQPath mod) imports
                       let allmods = addOrReplaceModule mod resolved1
                       return (mod{ modSourcePath = normalize $ joinPath root source }, allmods)


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
          -> do let reliface = joinPath {- currentDir -} (fullBuildDir flags) postfix  -- TODO: should be currentDir?
                exist <- doesFileExistAndNotEmpty reliface
                if exist then return (Just reliface)
                 else trace ("no iface at: " ++ reliface) $ return Nothing
         Just package
          -> searchPackages (packages flags) currentDir (head (splitPath package)) postfix


searchOutputIface :: Flags -> Name -> IO (Maybe FilePath)
searchOutputIface flags name
  = do let postfix = showModName name ++ ifaceExtension -- map (\c -> if c == '.' then '_' else c) (show name)
           iface = joinPath (fullBuildDir flags) postfix
       exist <- doesFileExistAndNotEmpty iface
       -- trace ("search output iface: " ++ show name ++ ": " ++ iface ++ " (" ++ (if exist then "found" else "not found" ) ++ ")") $ return ()
       if exist then return (Just iface)
         else do let libIface = joinPaths [localLibDir flags, buildVariant flags, postfix]
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
       mbP <- searchPathsEx (currentDir : includePath flags) [sourceExtension,sourceExtension++".md"] [] fname
       case mbP of
         Just (root,stem) | root == currentDir
           -> return $ Just (makeRelativeToPaths (includePath flags) (joinPath root stem))
         _ -> return mbP

searchIncludeIface :: Flags -> FilePath -> Name -> IO (Maybe FilePath)
searchIncludeIface flags currentDir name
  = do -- trace ("search include iface: " ++ showModName name ++ " from " ++ currentDir) $ return ()
       mbP <- searchPathsEx (currentDir : includePath flags) [] [] (showModName name ++ ifaceExtension)
       case mbP of
         Just (root,stem)
           -> return $ Just (joinPath root stem)
         Nothing -> return Nothing

{---------------------------------------------------------------

---------------------------------------------------------------}
typeCheck :: Loaded -> Flags -> Int -> [Core.Import] -> UserProgram -> Error Loaded (Loaded,Doc)
typeCheck loaded flags line coreImports program
  = do -- static checks
       -- program1 <- {- mergeSignatures (colorSchemeFromFlags flags) -} (implicitPromotion program)
       let program0 = -- implicitPromotion (loadedConstructors loaded)
                      (bindingGroups program)
           -- (warnings1,(fixities1,defs1,program1)) <- staticCheck (colorSchemeFromFlags flags)
           -- (loadedFixities loaded) (loadedDefinitions loaded) program0
           program1  = program0
           warnings1 = []

           fixitiesAll = fixitiesNew [(name,fix) | FixDef name fix rng vis <- programFixDefs program0]

       (program2,_) <- fixityResolve (colorSchemeFromFlags flags) (fixitiesCompose (loadedFixities loaded) fixitiesAll) program0
       let warnings = warnings1
           fname   = sourceName (programSource program)
           module1 = (moduleNull (getName program))
                               { modSourcePath = normalize fname
                               , modPath = outName flags (showModName (getName program)) ++ ifaceExtension
                               , modProgram    = Just program
                               , modWarnings   = warnings
                               }
           -- module0 = loadedModule loaded
           fixitiesPub = fixitiesNew [(name,fix) | FixDef name fix rng vis <- programFixDefs program0, vis == Public]
           loaded1 = loaded{ loadedModule      = module1
                           -- , loadedDefinitions = defs1
                           , loadedFixities    = fixitiesCompose (loadedFixities loaded) fixitiesPub
--                           , loadedModules     = (loadedModules loaded) ++
--                                                   (if null (modPath module1) then [] else [loadedModule loaded])
                           }

       addWarnings warnings (inferCheck loaded1 flags line coreImports program2 )


inferCheck :: Loaded -> Flags -> Int -> [Core.Import] -> UserProgram -> Error Loaded (Loaded,Doc)
inferCheck loaded0 flags line coreImports program
  = Core.runCorePhase (loadedUnique loaded0) $
    do -- kind inference

       (defs, kgamma, synonyms, newtypes, constructors, coreProgram, mbRangeMap0)
         <- inferKinds
              (isValueFromFlags flags)
              (colorSchemeFromFlags flags)
              (platform flags)
              (if (outHtml flags > 0 || genRangeMap flags) then Just rangeMapNew else Nothing)
              (loadedImportMap loaded0)
              (loadedKGamma loaded0)
              (loadedSynonyms loaded0)
              (loadedNewtypes loaded0)
              program

       let  gamma0  = gammaUnions [loadedGamma loaded0
                                  ,extractGamma (isValueFromFlags flags) True coreProgram
                                  ,extractGammaImports (importsList (loadedImportMap loaded0)) (getName program)
                                  ]
            loaded  = loaded0 { loadedKGamma  = kgamma
                              , loadedGamma   = gamma0
                              , loadedSynonyms= synonyms
                              , loadedNewtypes= newtypes -- newtypesCompose (loadedNewtypes loaded0) newtypes
                              , loadedConstructors=constructors
                              }
            penv    = prettyEnv loaded flags

            traceDefGroups title
              = do dgs <- Core.getCoreDefs
                   -- let doc = Core.Pretty.prettyCore (prettyEnvFromFlags flags){ coreIface = False, coreShowDef = True } C []
                   --            (coreProgram{ Core.coreProgDefs = dgs })
                   trace (unlines (["","/* -----------------", title, "--------------- */"] ++ -- ++ [show doc])) $ return ()
                           map showDef (Core.flattenDefGroups dgs))) $ return ()
              where
                showDef def = show (Core.Pretty.prettyDef (penv{coreShowDef=True}) def)

       -- Type inference
       (gamma,cdefs,mbRangeMap)
         <- inferTypes
              penv
              mbRangeMap0
              (loadedSynonyms loaded)
              (loadedNewtypes loaded)
              (loadedConstructors loaded)
              (loadedImportMap loaded)
              (loadedGamma loaded)
              (getName program)
              defs
       Core.setCoreDefs cdefs

       -- check generated core
       let checkCoreDefs title = when (coreCheck flags) (trace ("checking " ++ title) $
                                                         Core.Check.checkCore False False penv gamma)
       -- checkCoreDefs "initial"
       -- traceDefGroups "initial"

       -- remove return statements
       unreturn penv
       -- checkCoreDefs "unreturn"
       let borrowed = borrowedExtendICore (coreProgram{ Core.coreProgDefs = cdefs }) (loadedBorrowed loaded)
       checkFBIP penv (platform flags) (loadedNewtypes loaded) borrowed gamma

       -- initial simplify
       let ndebug  = optimize flags > 0
           simplifyX dupMax = simplifyDefs penv False {-unsafe-} ndebug (simplify flags) dupMax
           simplifyDupN     = when (simplify flags >= 0) $
                              simplifyX (simplifyMaxDup flags)
           simplifyNoDup    = simplifyX 0
       simplifyNoDup
       -- traceDefGroups "simplify1"

       -- lift recursive functions to top-level before specialize (so specializeDefs do not contain local recursive definitions)
       liftFunctions penv
       checkCoreDefs "lifted"
       -- traceDefGroups "lifted"

       -- unroll recursive definitions (before inline so generated wrappers can be inlined)
       when (optUnroll flags > 0) $
         do unrollDefs penv (optUnroll flags)
            -- traceDefGroups "unrolled"

       -- inline: inline local definitions more aggressively (2x)
       when (optInlineMax flags > 0) $
         do let inlines = if (isPrimitiveModule (Core.coreProgName coreProgram)) then loadedInlines loaded
                           else inlinesFilter (\name -> nameId nameCoreHnd /= nameModule name) (loadedInlines loaded)
            inlineDefs penv (2*(optInlineMax flags)) inlines
            -- checkCoreDefs "inlined"

       simplifyDupN
       -- traceDefGroups "inlined"

       -- specialize
       specializeDefs <- if (isPrimitiveModule (Core.coreProgName coreProgram)) then return [] else
                         Core.withCoreDefs (\defs -> extractSpecializeDefs (loadedInlines loaded) defs)
       -- traceM ("Spec defs:\n" ++ unlines (map show specializeDefs))

       when (optSpecialize flags && not (isPrimitiveModule (Core.coreProgName coreProgram))) $
         do
            -- simplifyDupN
            -- traceDefGroups "beforespec"
            specialize (inlinesExtends specializeDefs (loadedInlines loaded)) penv
            -- traceDefGroups "specialized"
            simplifyDupN
            -- traceDefGroups "simplified"
            -- lifting remaining recursive functions to top level (must be after specialize as that can generate local recursive definitions)
            liftFunctions penv
            checkCoreDefs "specialized"
            -- traceDefGroups "specialized and lifted"

       -- simplify once more
       simplifyDupN
       coreDefsInlined <- Core.getCoreDefs
       -- traceDefGroups "simplified"


       ------------------------------
       -- backend optimizations

       -- tail-call-modulo-cons optimization
       when (optctail flags) $
         ctailOptimize penv newtypes gamma (optctailCtxPath flags)

       -- transform effects to explicit monadic binding (and resolve .open calls)
       when (enableMon flags && not (isPrimitiveModule (Core.coreProgName coreProgram))) $
          -- trace "monadic transform" $
          do Core.Monadic.monTransform penv
             openResolve penv gamma           -- must be after monTransform
       checkCoreDefs "monadic transform"

       -- simplify open applications (needed before inlining open defs)
       simplifyNoDup
       -- traceDefGroups "open resolved"

       -- monadic lifting to create fast inlined paths
       monadicLift penv
       checkCoreDefs "monadic lifting"
       -- traceDefGroups "monadic lift"

      -- now inline primitive definitions (like yield-bind)
       let inlinesX = inlinesFilter isPrimitiveName (loadedInlines loaded)
       -- trace ("inlines2: " ++ show (map Core.inlineName (inlinesToList inlinesX))) $
       inlineDefs penv (2*optInlineMax flags) inlinesX -- (loadedInlines loaded)

       -- remove remaining open calls; this may change effect types
       simplifyDefs penv True {-unsafe-} ndebug (simplify flags) 0 -- remove remaining .open

       -- final simplification
       simplifyDupN
       checkCoreDefs "final"
       -- traceDefGroups "simplify final"

       -- Assemble core program and return
       coreDefsFinal <- Core.getCoreDefs
       uniqueFinal   <- unique
       -- traceM ("final: " ++ show uniqueFinal)
       let -- extract inline definitions to export
           localInlineDefs  = extractInlineDefs (optInlineMax flags) coreDefsInlined
           -- give priority to specializeDefs, since inlining can prevent specialize opportunities
           allInlineDefs    = specializeDefs ++ localInlineDefs

           coreProgramFinal
            = uniquefy $
              coreProgram { Core.coreProgImports = coreImports
                          , Core.coreProgDefs = coreDefsFinal
                          , Core.coreProgFixDefs = [Core.FixDef name fix | FixDef name fix rng vis <- programFixDefs program, vis == Public]
                          }

           loadedFinal = loaded { loadedGamma = gamma
                                , loadedUnique = uniqueFinal
                                , loadedModule = (loadedModule loaded){
                                                    modCore     = coreProgramFinal,
                                                    modRangeMap = mbRangeMap,
                                                    modInlines  = Right allInlineDefs
                                                  }
                                , loadedInlines = inlinesExtends allInlineDefs (loadedInlines loaded)
                                }

           coreDoc = Core.Pretty.prettyCore (prettyEnvFromFlags flags){ coreIface = False, coreShowDef = True } (C CDefault) []
                       (coreProgram{ Core.coreProgDefs = coreDefsInlined })

       return (loadedFinal, coreDoc)

modulePath mod
  = let path = maybe "" (sourceName . programSource) (modProgram mod)
    in if null path
        then show nameInteractiveModule
        else path


capitalize s
  = case s of
      c:cs -> toUpper c : cs
      _    -> s


codeGen :: Terminal -> Flags -> CompileTarget Type -> Loaded -> IO (Loaded, Maybe (FilePath, IO()))
codeGen term flags compileTarget loaded
  = compilerCatch "code generation" term (loaded, Nothing) $
    do let mod         = loadedModule loaded
           outBase     = outName flags (showModName (modName mod))

       let env      = (prettyEnvFromFlags flags){ context = loadedName loaded, importsMap = loadedImportMap loaded }
           outIface = outBase ++ ifaceExtension
           inlineDefs = case modInlines mod of
                          Right defs -> defs
                          Left _     -> []
           ifaceDoc = Core.Pretty.prettyCore env{ coreIface = True } (target flags) inlineDefs (modCore mod) <-> Lib.PPrint.empty

       -- create output directory if it does not exist
       createDirectoryIfMissing True (dirname outBase)

       -- remove existing kki file in case of errors
       removeFileIfExists outIface

       -- core
       let outCore  = outBase ++ ".kkc"
           coreDoc  = Core.Pretty.prettyCore env{ coreIface = False, coreShowDef = (showCore flags) } (target flags) inlineDefs (modCore mod)
                        <-> Lib.PPrint.empty
       when (genCore flags)  $
         do termPhase term "generate core"
            writeDocW 10000 outCore coreDoc  -- just for debugging
       when (showFinalCore flags && not (isTargetC (target flags))) $
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

       mbRun <- backend term flags (loadedModules loaded) compileTarget outBase (modCore mod)

       -- write interface file last so on any error it will not be written
       writeDocW 10000 outIface ifaceDoc
       ftime <- getFileTimeOrCurrent outIface
       let mod1 = (loadedModule loaded){ modTime = ftime, modCompiled = True }
           loaded1 = loaded{ loadedModule = mod1  }

       -- copy final exe if -o is given
       case mbRun of
         Just (out,_)
           -> do let finalOut = outFinalPath flags
                 exe <- if (not (null finalOut))
                          then do let targetOut = ensureExt finalOut (targetExeExtension (target flags))
                                  when (osName == "macos") $
                                    removeFileIfExists targetOut  -- needed on macOS due to code signing issues (see https://developer.apple.com/forums/thread/669145)
                                  copyBinaryFile out targetOut
                                  return finalOut
                          else return out
                 termPhaseDoc term $ color (colorInterpreter (colorScheme flags)) (text "created:") <+>
                    color (colorSource (colorScheme flags)) (text (normalizeWith pathSep exe))
         _ -> return ()

       return (loaded1, mbRun) -- { loadedArities = arities, loadedExternals = externals }
  where
    concatMaybe :: [Maybe a] -> [a]
    concatMaybe mbs  = concatMap (maybe [] (\x -> [x])) mbs

    backend :: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (FilePath, IO()))
    backend  = case target flags of
                 CS   -> codeGenCS
                 JS _ -> codeGenJS
                 _    -> let -- for Perceus (Parc) we analyze types inside abstract types and thus need
                             -- access to all defined types; here we freshly extract all type definitions from all
                             -- imported modules.
                             newtypesAll = foldr1 newtypesCompose (map (extractNewtypes . modCore) (loadedModule loaded : loadedModules loaded))
                         in codeGenC (modSourcePath (loadedModule loaded))
                                  -- (loadedNewtypes loaded)
                                     newtypesAll (loadedBorrowed loaded) (loadedUnique loaded)


-- CS code generation via libraries; this catches bugs in C# generation early on but doesn't take a transitive closure of dll's
codeGenCSDll:: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (IO()))
codeGenCSDll term flags modules compileTarget outBase core
  = compilerCatch "csharp compilation" term Nothing $
    do let (mbEntry,isAsync) = case compileTarget of
                                 Executable name tp -> (Just (name,tp), isAsyncFunction tp)
                                 _ -> (Nothing, False)
           cs  = csharpFromCore (buildType flags) (enableMon flags) mbEntry core
           outcs       = outBase ++ ".cs"
           searchFlags = "" -- concat ["-lib:" ++ dquote dir ++ " " | dir <- [fullBuildDir flags] {- : includePath flags -}, not (null dir)] ++ " "

       termPhase term $ "generate csharp" ++ maybe "" (\(name,_) -> ": entry: " ++ show name) mbEntry
       writeDoc outcs cs
       when (showAsmCS flags) (termDoc term cs)

       let linkFlags  = concat ["-r:" ++ outName flags (showModName (Core.importName imp)) ++ dllExtension ++ " "
                                    | imp <- Core.coreProgImports core] -- TODO: link to correct package!
                        ++ "-r:System.Numerics.dll " ++ (if isAsync then "-r:" ++ outName flags "std_async.dll " else "")
           targetName = case compileTarget of
                          Executable _ _ -> dquote ((if null (outBaseName flags) then outBase else outName flags (outBaseName flags)) ++ exeExtension)
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
codeGenCS :: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (FilePath, IO()))
codeGenCS term flags modules compileTarget outBase core
  = compilerCatch "csharp compilation" term Nothing $
    do let (mbEntry,isAsync) = case compileTarget of
                                 Executable name tp -> (Just (name,tp), isAsyncFunction tp)
                                 _ -> (Nothing, False)
           cs  = csharpFromCore (buildType flags) (enableMon flags) mbEntry core
           outcs       = outBase ++ ".cs"
           searchFlags = "" -- concat ["-lib:" ++ dquote dir ++ " " | dir <- [fullBuildDir flags] {- : includePath flags -}, not (null dir)] ++ " "

       termPhase term $ "generate csharp" ++ maybe "" (\(name,_) -> ": entry: " ++ show name) mbEntry
       writeDoc outcs cs
       when (showAsmCS flags) (termDoc term cs)

       case mbEntry of
         Nothing -> return Nothing
         Just entry ->
          do let linkFlags  = "-r:System.Numerics.dll " -- ++ (if isAsync then "-r:" ++ outName flags "std_async.dll ")
                 sources    = concat [dquote (outName flags (showModName (modName mod)) ++ ".cs") ++ " " | mod <- modules]
                 targetExe  = (if null (outBaseName flags) then outBase else outName flags (outBaseName flags)) ++ exeExtension
                 targetName = dquote targetExe
                 targetFlags= "-t:exe -out:" ++ targetName ++ " "
                 debugFlags = (if (debug flags) then "-debug -define:DEBUG " else "") ++ (if (optimize flags >= 1) then "-optimize " else "")
             let cmd = (csc flags ++ " " ++ targetFlags ++ debugFlags ++ " -nologo -warn:4 " ++ searchFlags ++ linkFlags ++ sources)
             runSystemEcho term flags cmd
             -- run the program
             return (Just (targetExe, runSystemEcho term flags targetName))


codeGenJS :: Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (FilePath, IO ()))
codeGenJS term flags modules compileTarget outBase core
  = do let outjs = outBase ++ ".mjs"
       let mbEntry = case compileTarget of
                       Executable name tp -> Just (name,isAsyncFunction tp)
                       _                  -> Nothing
           extractImport mod = Core.Import (modName mod) (modPackageQName mod) Public ""
       let js    = javascriptFromCore (buildType flags) mbEntry (map extractImport modules) core
       termPhase term ( "generate javascript: " ++ outjs )
       writeDocW 80 outjs js
       when (showAsmJS flags) (termDoc term js)

       case mbEntry of
        Nothing -> return Nothing
        Just (name) ->
         do -- always generate an index.html file
            let outHtml = outName flags ((if (null (outBaseName flags)) then "index" else (outBaseName flags)) ++ ".html")
                contentHtml = text $ unlines $ [
                                "<!DOCTYPE html>",
                                "<html>",
                                "  <head>",
                                "    <meta charset=\"utf-8\">",
                                "    <script type='module' src='./" ++ notdir outjs ++ "'></script>",
                                "  </head>",
                                "  <body>",
                                "  </body>",
                                "</html>"
                              ]
            termPhase term ("generate index html: " ++ outHtml)
            writeDoc outHtml contentHtml
            -- copy amdefine
            {-
            let copyNodeModules fname
                  = let nname = "node_modules/" ++ fname
                    in copyTextIfNewer (rebuild flags) (joinPath (localShareDir flags) nname) (outName flags nname)
            mapM_ copyNodeModules ["amdefine/amdefine.js","amdefine/package.json",
                                   "requirejs/require.js","requirejs/package.json"]
            -}


            -- try to ensure require.js is there
            -- TODO: we should search along the node_modules search path
            {-mbReq <- searchPackages (packages flags) (fullBuildDir flags) "requirejs" "require.js"
            case mbReq of
              Just reqPath -> copyTextIfNewer (rebuild flags) reqPath (outName flags "require.js")
              Nothing      -> trace "could not find requirejs" $ return () -- TODO: warning?
            -}
            case target flags of
              JS JsWeb ->
               do return (Just (outHtml, runSystemEcho term flags (dquote outHtml ++ " &")))
              _ ->
               do let stksize = if (stackSize flags == 0) then 100000 else (stackSize flags `div` 1024)
                  return (Just (outjs, runCommand term flags [node flags,"--stack-size=" ++ show stksize,outjs]))

codeGenC :: FilePath -> Newtypes -> Borrowed -> Int -> Terminal -> Flags -> [Module] -> CompileTarget Type -> FilePath -> Core.Core -> IO (Maybe (FilePath,IO ()))
codeGenC sourceFile newtypes borrowed0 unique0 term flags modules compileTarget outBase core0
 = -- compilerCatch "c compilation" term Nothing $
   do let outC = outBase ++ ".c"
          outH = outBase ++ ".h"
          sourceDir = dirname sourceFile
      let mbEntry = case compileTarget of
                      Executable name tp -> Just (name,isAsyncFunction tp)
                      _                  -> Nothing
      let -- (core,unique) = parcCore (prettyEnvFromFlags flags) newtypes unique0 core0
          ctarget = case target flags of
                      C ctarget -> ctarget
                      _         -> CDefault
          (cdoc,hdoc,mbMainDoc,bcore) = cFromCore ctarget (buildType flags) sourceDir (prettyEnvFromFlags flags) (platform flags)
                                          newtypes borrowed0 unique0 (parcReuse flags) (parcSpecialize flags) (parcReuseSpec flags)
                                          (parcBorrowInference flags) (optEagerPatBind flags) (stackSize flags) mbEntry core0
          bcoreDoc  = Core.Pretty.prettyCore (prettyEnvFromFlags flags){ coreIface = False, coreShowDef = True } (C CDefault) [] bcore
      -- writeDocW 120 (outBase ++ ".c.kkc") bcoreDoc
      when (showFinalCore flags) $
        do termDoc term bcoreDoc

      termPhase term ( "generate c: " ++ outBase )
      writeDocW 120 outC (cdoc <.> linebreak)
      writeDocW 120 outH (hdoc <.> linebreak)
      when (showAsmC flags) (termDoc term (hdoc <//> cdoc))

      -- copy libraries
      let cc       = ccomp flags
          eimports = externalImportsFromCore (target flags) bcore
          clibs    = clibsFromCore flags bcore
      extraIncDirs <- concat <$> mapM (copyCLibrary term flags cc) eimports

      -- compile
      ccompile term flags cc outBase extraIncDirs [outC]

      -- compile and link?
      case mbMainDoc of   -- like mbEntry
       Nothing -> return Nothing
       Just mainDoc ->
         do -- compile main C entry point (`main`)
            let outMainBase = outBase ++ "__main"
                outMainC    = outMainBase ++ ".c"
                mainObj     = ccObjFile cc outMainBase
            writeDocW 120 outMainC (mainDoc <.> linebreak)
            ccompile term flags cc outMainBase  extraIncDirs [outMainC]

            currentDir <- getCurrentDirectory

            let mainModName= showModName (Core.coreProgName core0)
                mainName   = if null (outBaseName flags) then mainModName else outBaseName flags
                mainExe    = outName flags mainName


            -- build kklib for the specified build variant
            -- cmakeLib term flags cc "kklib" (ccLibFile cc "kklib") cmakeGeneratorFlag
            kklibObj <- kklibBuild term flags cc "kklib" (ccObjFile cc "kklib")

            let objs   = [kklibObj] ++
                         [outName flags (ccObjFile cc (showModName mname))
                            | mname <- map modName modules ++ [Core.coreProgName core0]] ++
                         [mainObj]
                syslibs= concat [csyslibsFromCore flags mcore | mcore <- map modCore modules]
                         ++ ccompLinkSysLibs flags
                         ++ (if onWindows && not (isTargetWasm (target flags))
                              then ["bcrypt","psapi","advapi32"]
                              else ["m","pthread"])
                libs   = -- ["kklib"] -- [normalizeWith '/' (outName flags (ccLibFile cc "kklib"))] ++ ccompLinkLibs flags
                         -- ++
                         clibs
                         ++
                         concat [clibsFromCore flags mcore | mcore <- map modCore modules]

                libpaths = map (\lib -> outName flags (ccLibFile cc lib)) libs

                stksize = if (stackSize flags == 0 && (onWindows || isTargetWasm (target flags)))
                            then 8*1024*1024    -- default to 8Mb on windows and wasi
                            else stackSize flags
                hpsize  = if (heapSize flags == 0 && isTargetWasm (target flags))
                            then 1024*1024*1024 -- default to 1Gb on wasi
                            else heapSize flags

                clink  = concat $
                         [ [ccPath cc]
                         , ccFlags cc
                         , ccFlagsBuildFromFlags cc flags
                         , ccTargetExe cc mainExe
                         ]
                         ++ [objs]
                         ++ [ccFlagsLink cc]  -- must be last due to msvc
                         ++ [ccFlagStack cc stksize,ccFlagHeap cc hpsize]
                         -- ++ [ccAddLibraryDir cc (fullBuildDir flags)]
                         ++ map (ccAddLib cc) libpaths  -- libs
                         ++ map (ccAddSysLib cc) syslibs


            termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "linking:") <+>
                               color (colorSource (colorScheme flags)) (text mainName))
            runCommand term flags clink

            let mainTarget = mainExe ++ targetExeExtension (target flags)
            when (not (null (outFinalPath flags))) $
              termPhaseDoc term $ color (colorInterpreter (colorScheme flags)) (text "created:") <+>
                                    color (colorSource (colorScheme flags)) (text (normalizeWith pathSep mainTarget))
            let cmdflags = if (showElapsed flags) then " --kktime" else ""

            case target flags of
              C Wasm
                -> do return (Just (mainTarget,
                               runSystemEcho term flags (wasmrun flags ++ " " ++ dquote mainTarget ++ " -- " ++ cmdflags ++ " " ++ execOpts flags)))
              C WasmWeb
                -> do return (Just (mainTarget, runSystemEcho term flags (dquote mainTarget ++ " &")))
              C WasmJs
                -> do let nodeStack = if (stksize == 0) then 100000 else (stksize `div` 1024)
                      return (Just (mainTarget,
                               runCommand term flags [node flags,"--stack-size=" ++ show nodeStack,mainTarget]))
              _ -> do return (Just (mainTarget,
                               runSystemEcho term flags (dquote mainExe ++ cmdflags ++ " " ++ execOpts flags))) -- use shell for proper rss accounting


ccompile :: Terminal -> Flags -> CC -> FilePath -> [FilePath] -> [FilePath] -> IO ()
ccompile term flags cc ctargetObj extraIncDirs csources
  = do let cmdline = concat $
                      [ [ccPath cc]
                      , ccFlags cc
                      , ccFlagsWarn cc
                      , ccFlagsBuildFromFlags cc flags
                      , ccFlagsCompile cc
                      , ccIncludeDir cc (localShareDir flags ++ "/kklib/include")
                      ]
                      ++
                      map (ccIncludeDir cc) (extraIncDirs ++ ccompIncludeDirs flags)
                      ++
                      map (ccAddDef cc) (ccompDefs flags)
                      ++
                      [ ccTargetObj cc (notext ctargetObj)
                      , csources
                      ]
       runCommand term flags cmdline


-- copy static C library to the output directory (so we can link and/or bundle) and
-- return needed include paths for imported C code
copyCLibrary :: Terminal -> Flags -> CC -> [(String,String)] -> IO [FilePath] {-include paths-}
copyCLibrary term flags cc eimport
  = case Core.eimportLookup (buildType flags) "library" eimport of
      Nothing -> return []
      Just clib
        -> do mb  <- do mbSearch <- search [] [ searchCLibrary flags cc clib (ccompLibDirs flags)
                                              , case lookup "conan" eimport of
                                                  Just pkg | not (null (conan flags))
                                                    -> conanCLibrary term flags cc eimport clib pkg
                                                  _ -> return (Left [])
                                              , case lookup "vcpkg" eimport of
                                                  Just pkg
                                                    -> vcpkgCLibrary term flags cc eimport clib pkg
                                                  _ -> return (Left [])
                                              ]
                        case mbSearch of
                          Right res -> return (Just res)
                          Left warn -> do termWarning term flags warn
                                          return Nothing
              case mb of
                Just (libPath,includes)
                  -> do termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "library:") <+>
                          color (colorSource (colorScheme flags)) (text libPath))
                        -- this also renames a suffixed libname to a canonical name (e.g. <vcpkg>/pcre2-8d.lib -> <out>/pcre2-8.lib)
                        copyBinaryIfNewer (rebuild flags) libPath (outName flags (ccLibFile cc clib))
                        return includes
                Nothing
                  -> -- TODO: suggest conan and/or vcpkg install?
                     do termWarning term flags $
                          text "unable to find C library:" <+> color (colorSource (colorScheme flags)) (text clib) <->
                          text "   hint: provide \"--cclibdir\" as an option, or use \"syslib\" in an extern import?"
                        raiseIO ("unable to find C library " ++ clib ++
                                 "\nlibrary search paths: " ++ show (ccompLibDirs flags))
  where
    search :: [Doc] -> [IO (Either [Doc] (FilePath,[FilePath]))] -> IO (Either Doc (FilePath,[FilePath]))
    search warns [] = return (Left (vcat (intersperse (text "or") warns)))
    search warns (io:ios)
      = do mbRes <- io
           case mbRes of
             Right res   -> return (Right res)
             Left warns' -> search (warns ++ warns') ios

searchCLibrary :: Flags -> CC -> FilePath -> [FilePath] -> IO (Either [Doc] (FilePath {-libPath-},[FilePath] {-include paths-}))
searchCLibrary flags cc clib searchPaths
  = do mbPath <- -- looking for specific suffixes is not ideal but it differs among plaforms (e.g. pcre2-8 is only pcre2-8d on Windows)
                 -- and the actual name of the library is not easy to extract from vcpkg (we could read
                 -- the lib/config/<lib>.pc information and parse the Libs field but that seems fragile as well)
                 do let suffixes = (if (buildType flags <= Debug) then ["d","_d","-d","-debug","_debug","-dbg","_dbg"] else [])
                    -- trace ("search in: " ++ show searchPaths) $
                    searchPathsSuffixes searchPaths [] suffixes (ccLibFile cc clib)
       case mbPath of
        Just fname
          -> case reverse (splitPath fname) of
               (_:"lib":"debug":rbase) -> return (Right (fname, [joinPaths (reverse rbase ++ ["include"])])) -- for vcpkg
               (_:"lib":rbase)         -> return (Right (fname, [joinPaths (reverse rbase ++ ["include"])])) -- e.g. /usr/local/lib
               _                       -> return (Right (fname, []))
        _ -> return (Left [])


conanCLibrary :: Terminal -> Flags -> CC -> [(String,String)] -> FilePath -> String -> IO (Either [Doc] (FilePath,[FilePath]))
conanCLibrary term flags cc eimport clib pkg
  = do mbConanCmd <- searchProgram (conan flags)
       case mbConanCmd of
         Nothing
          -> do return $ Left [text "this module requires a conan package but \"" <.> clrSource (text (conan flags)) <.> text "\" is not installed."
                                     <-> text "         install conan as:"
                                     <-> text "         >" <+> clrSource (text "pip3 install conan")
                                     <-> text "         or see <" <.> clrSource (text "https://docs.conan.io/en/latest/installation.html") <.> text ">"]
         Just conanCmd | onWindows && not (any (\pre -> ccName cc `startsWith` pre) ["cl","clang-cl"])
          -> do return $ Left [text "conan can only be used with the 'cl' or 'clang-cl' compiler on Windows"]
         Just conanCmd | isTargetWasm (target flags)
          -> do return $ Left [text "conan can not be used with a wasm target"]
         Just conanCmd
          -> do mbPkgDir <- getPackageDir conanCmd
                case mbPkgDir of
                  Nothing
                    -> do termWarning term flags $
                            text "unable to resolve conan package:" <+> clrSource (text pkg)
                          return (Left [])
                  Just (pkgName,pkgDir)
                    -> do termPhaseDoc term $ color (colorInterpreter (colorScheme flags)) $
                            text "package: conan" <+> clrSource (text pkgName) -- <.> colon <+> clrSource (text pkgDir)
                          let libDir = pkgDir ++ "/lib"
                          searchCLibrary flags cc clib [libDir]

  where
    pkgBase
      = takeWhile (/='/') pkg

    (baseSettings,conanEnv)
      = conanSettingsFromFlags flags cc

    settings
      = baseSettings ++ ["-o",pkgBase ++ "/*:shared=False","-o","shared=False"]

    clrSource doc
      = color (colorSource (colorScheme flags)) doc

    getPackageDir conanCmd
      = do mbPkgDir <- findPackagDir conanCmd
           case mbPkgDir of
             Just _   -> return mbPkgDir
             Nothing  -> do installPackage conanCmd
                            findPackagDir conanCmd

    findPackagDir conanCmd
      = do -- find out latest installed version
           let infoCmd = [conanCmd, "list", "-c", pkg]
           out <- runCommandRead term flags conanEnv infoCmd `catchIO` (\msg -> return "")
           -- termPhase term out
           let cachepkg = dropWhile isSpace $ concat $ take 1 $ dropWhile (all isSpace) $ reverse (lines out)
           if (not (cachepkg `startsWith` pkgBase))
             then return Nothing
             else -- and get the binary package location
                  do let queryCmd = [conanCmd, "install", "--requires", cachepkg] ++ settings
                     (_,out) <- runCommandReadAll term flags conanEnv queryCmd `catchIO` (\msg -> return ("",""))
                     -- termPhase term out
                     let prefix = cachepkg ++ ": Appending PATH environment variable: "
                         ppaths = [reverse $ drop 4 {- /bin -} $ reverse $
                                    drop (length prefix) line | line <- lines out, line `startsWith` prefix]
                     termPhase term (show (lines out))
                     termPhase term (unlines ppaths)
                     case ppaths of
                       [ppath] -> do exist <- doesDirectoryExist ppath
                                     return (if exist then Just (cachepkg,ppath) else Nothing)
                       _       -> return Nothing


    installPackage conanCmd
      = do let installCmd = [conanCmd, "install", "--build", "missing", "--requires", pkg] ++ settings
           if not (autoInstallLibs flags)
             then termWarning term flags (text "this module requires the conan package"
                    <+> clrSource (text pkg)
                    <+> text "         enable auto install using the \"--autoinstall\" option to koka,"
                    <+> text "         or install the package manually as:"
                    <-> text "         >" <+> clrSource (text (unwords installCmd))
                    <-> text "         to install the required C library and header files")
             else do let profileCmd = [conanCmd, "profile", "detect"] -- ensure default profile exists
                     runCommandReadAll term flags conanEnv profileCmd `catchIO` (\msg -> return ("",""))
                     termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "install: conan package:") <+> clrSource (text pkg))
                     runCommandEnv term flags conanEnv installCmd


vcpkgCLibrary :: Terminal -> Flags -> CC -> [(String,String)] -> FilePath -> String -> IO (Either [Doc] (FilePath,[FilePath]))
vcpkgCLibrary term flags cc eimport clib pkg
  = do (root,vcpkg) <- vcpkgFindRoot (vcpkgRoot flags)
       exist <- doesFileExist vcpkg
       if (not exist)
         then do return $ Left [
                    text "this module requires vcpkg to install the" <+> clrSource (text clib) <+> text "library." <->
                    text "   hint: specify the root directory of vcpkg using the" <+> clrSource (text "--vcpkg=<dir>") <+> text "option" <->
                    text "         or the" <+> clrSource (text "VCPKG_ROOT") <+> text "environment variable," <->
                  (if onMacOS then
                   (text "         or install vcpkg as:" <->
                    text "         > brew install vcpkg")
                   else
                   (text "         or install vcpkg from <" <.> clrSource (text "https://vcpkg.io/en/getting-started.html") <.> text ">"))
                  ]
         else do let libDir = root ++ "/installed/" ++ (vcpkgTriplet flags)
                                ++ (if buildType flags <= Debug then "/debug/lib" else "/lib")
                 termPhaseDoc term $ color (colorInterpreter (colorScheme flags)) $
                    text "package: vcpkg" <+> clrSource (text pkg)
                 mbInstalled <- searchCLibrary flags cc clib [libDir]
                 case mbInstalled of
                   Right _ -> return mbInstalled
                   Left _  -> install root libDir vcpkg
  where
    clrSource doc
      = color (colorSource (colorScheme flags)) doc

    install rootDir libDir vcpkgCmd
      = do  let packageDir = joinPaths [rootDir,"packages",pkg ++ "_" ++ vcpkgTriplet flags]
            pkgExist <- doesDirectoryExist packageDir
            when (pkgExist) $
              termWarning term flags $
                text "vcpkg" <+> clrSource (text pkg) <+>
                text "is installed but the library" <+> clrSource (text clib) <+>
                text "is not found in" <+> clrSource (text libDir)
            let installCmd = [vcpkgCmd, "install", pkg ++ ":" ++ vcpkgTriplet flags, "--disable-metrics"]
            if (not (autoInstallLibs flags))
              then do termWarning term flags (text "this module requires vcpkg package"
                                              <+> clrSource (text pkg)
                                              <-> text "   hint: enable auto install using the \"--autoinstall\" option to koka,"
                                              <-> text "         or install the package manually as:"
                                              <-> text "         >" <+> clrSource (text (unwords installCmd))
                                              <-> text "         to install the required C library and header files")
                      return (Left [])
              else do termPhaseDoc term (color (colorInterpreter (colorScheme flags)) (text "install: vcpkg package:") <+> clrSource (text pkg))
                      runCommand term flags installCmd
                      searchCLibrary flags cc clib [libDir] -- try to find again after install


termWarning term flags doc
  = termDoc term $ color (colorWarning (colorSchemeFromFlags flags)) (text "warning:" <+> doc)

clibsFromCore flags core    = externalImportKeyFromCore (target flags) (buildType flags) core "library"
csyslibsFromCore flags core = externalImportKeyFromCore (target flags) (buildType flags) core "syslib"

externalImportKeyFromCore :: Target -> BuildType -> Core.Core -> String -> [String]
externalImportKeyFromCore target buildType core key
  = catMaybes [Core.eimportLookup buildType key keyvals  | keyvals <- externalImportsFromCore target core]

externalImportsFromCore :: Target -> Core.Core -> [[(String,String)]]
externalImportsFromCore target core
  = [keyvals  | Core.ExternalImport imports _ <- Core.coreProgExternals core, (target,keyvals) <- imports]


kklibBuild :: Terminal -> Flags -> CC -> String -> FilePath -> IO FilePath
kklibBuild term flags cc name {-kklib-} objFile {-libkklib.o-}
  = do let objPath = outName flags objFile  {-out/v2.x.x/clang-debug/libkklib.o-}
       exist <- doesFileExist objPath
       let binObjPath = joinPath (localLibDir flags) (buildVariant flags ++ "/" ++ objFile)
       let srcLibDir  = joinPath (localShareDir flags) (name)
       binExist <- doesFileExist binObjPath
       binNewer <- if (not binExist) then return False
                   else if (not exist) then return True
                   else do cmp <- fileTimeCompare binObjPath objPath
                           return (cmp==GT)
       srcNewer <- if (binNewer) then return False -- no need to check
                   else if (not exist) then return True
                   else do cmp <- fileTimeCompare (srcLibDir ++ "/include/kklib.h") objPath
                           return (cmp==GT)
       -- putStrLn ("binObjPath: " ++ binObjPath ++ ", newer: " ++ show binNewer)
       if (not binNewer && not srcNewer && not (rebuild flags))
        then return ()
         else if (binNewer)
           then -- use pre-compiled installed binary
                copyBinaryFile binObjPath objPath
           else -- todo: check for installed binaries for the library
                -- compile kklib from sources
                do termDoc term $ color (colorInterpreter (colorScheme flags)) (text ("compile:")) <+>
                                   color (colorSource (colorScheme flags)) (text name) <+>
                                    color (colorInterpreter (colorScheme flags)) (text "from:") <+>
                                     color (colorSource (colorScheme flags)) (text srcLibDir)
                   let flags0 = if (useStdAlloc flags) then flags
                                  else flags{ ccompIncludeDirs = ccompIncludeDirs flags ++ [localShareDir flags ++ "/kklib/mimalloc/include"] }
                       flags1 = flags0{ ccompDefs = ccompDefs flags ++
                                                    [("KK_COMP_VERSION","\"" ++ version ++ "\""),
                                                     ("KK_CC_NAME", "\"" ++ ccName cc ++ "\"")] }
                   ccompile term flags1 cc objPath [] [joinPath srcLibDir "src/all.c"]
       return objPath


cmakeLib :: Terminal -> Flags -> CC -> String -> FilePath -> [String] -> IO ()
cmakeLib term flags cc libName {-kklib-} libFile {-libkklib.a-} cmakeGeneratorFlag
  = do let libPath = outName flags libFile  {-out/v2.x.x/clang-debug/libkklib.a-}
       exist <- doesFileExist libPath
       let binLibPath = joinPath (localLibDir flags) (buildVariant flags ++ "/" ++ libFile)
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
                                             DebugFull -> "Debug"
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
                                      , "-DCMAKE_INSTALL_PREFIX=" ++ (fullBuildDir flags)
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


runSystemEcho :: Terminal -> Flags -> String -> IO ()
runSystemEcho term flags cmd
  = do when (verbose flags >= 2) $
         termPhase term ("shell> " ++ cmd)
       runSystem cmd

runCommand :: Terminal -> Flags -> [String] -> IO ()
runCommand term flags cargs@(cmd:args)
  = do let command = unwords (shellQuote cmd : map shellQuote args)
       if (osName == "windows" && cmd `endsWith` "emcc") -- hack to run emcc correctly on windows (due to Python?)
         then runSystemEcho term flags command
         else  do when (verbose flags >= 2) $
                    termPhase term ("command> " ++ command) -- cmd ++ " [" ++ concat (intersperse "," args) ++ "]")
                  runCmd cmd (filter (not . null) args)
                    `catchIO` (\msg -> raiseIO ("error  : " ++ msg ++ "\ncommand: " ++ command))

runCommandRead :: Terminal -> Flags -> [(String,String)] -> [String] -> IO String
runCommandRead term flags env cargs
  = do (out,_) <- runCommandReadAll term flags env cargs
       return out

runCommandReadAll :: Terminal -> Flags -> [(String,String)] -> [String] -> IO (String,String)
runCommandReadAll term flags env cargs@(cmd:args)
  = do let command = unwords (shellQuote cmd : map shellQuote args)
       when (verbose flags >= 2) $
         termPhase term ("command> " ++ command) -- cmd ++ " [" ++ concat (intersperse "," args) ++ "]")
       runCmdRead env cmd (filter (not . null) args)
         `catchIO` (\msg -> raiseIO ("error  : " ++ msg ++ "\ncommand: " ++ command))

runCommandEnv :: Terminal -> Flags -> [(String,String)] -> [String] -> IO ()
runCommandEnv term flags env cargs@(cmd:args)
  = do let command = unwords (shellQuote cmd : map shellQuote args)
       when (verbose flags >= 2) $
         termPhase term ("command> " ++ command) -- cmd ++ " [" ++ concat (intersperse "," args) ++ "]")
       runCmdEnv env  cmd (filter (not . null) args)
         `catchIO` (\msg -> raiseIO ("error  : " ++ msg ++ "\ncommand: " ++ command))


shellQuote s
  = if (all (\c -> isAlphaNum c || c `elem` ":/-_.=") s) then s
     else "\"" ++ concatMap quote s ++ "\""
  where
    quote '"'  = "\\\""
    quote '\'' = "\\'"
    quote c    = [c]

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
       case target flags of
        CS
          -> do let libSrc = notext iface ++ dllExtension
                let libOut = notext outIFace ++ dllExtension
                copyBinaryIfNewer (rebuild flags) libSrc libOut
        JS _
          -> do let jsSrc = notext iface ++ ".mjs"
                let jsOut = notext outIFace ++ ".mjs"
                -- copyTextFileWith  jsSrc jsOut (packagePatch iface (targetPath) imported)
                copyTextIfNewer (rebuild flags) jsSrc jsOut
        C _
          -> do copyTextIfNewer (rebuild flags) (withExt iface ".c") (withExt outIFace ".c")
                copyTextIfNewer (rebuild flags) (withExt iface ".h") (withExt outIFace ".h")
                let cc = ccomp flags
                    srcDir = dirname iface
                copyBinaryIfNewer (rebuild flags) (ccObjFile cc (notext iface)) (ccObjFile cc (notext outIFace))
                mapM_ (\clib ->
                  do let libFile = ccLibFile cc clib
                     -- todo: only copy if it exists?
                     copyBinaryIfNewer (rebuild flags) (joinPath srcDir libFile) (outName flags libFile)
                 ) (clibsFromCore flags core)
       return outIFace

copyPkgIFaceToOutputDir :: Terminal -> Flags -> FilePath -> Core.Core -> PackageName -> [Module] -> IO ()
copyPkgIFaceToOutputDir term flags iface core targetPath imported
  -- | host flags == Node && target flags == JS = return ()
  -- | otherwise
  = do outIFace <- copyIFaceToOutputDir term flags iface core
       if (isTargetJS (target flags))
        then do let outJs = notext outIFace ++ ".mjs"
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
                                           <-> string msg)) -- (fillSep $ map string $ words msg)))
       return defValue


isJust (Just _) = True
isJust _        = False

catchSystem io f
  = io `catchIO` (\msg -> let cmdFailed = "command failed:" in
                          if (cmdFailed `isPrefixOf` msg)
                           then f (drop (length cmdFailed) msg)
                           else f msg)

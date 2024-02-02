-----------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Compile.BuildContext ( BuildContext
                            , buildcEmpty

                            , buildcFullBuild
                            , buildcValidate
                            , buildcTypeCheck, buildcBuild

                            , buildcRunExpr, buildcRunEntry
                            , buildcCompileExpr, buildcCompileEntry

                            , buildcAddRootSources
                            , buildcAddRootModules
                            , buildcRoots
                            , buildcClearRoots, buildcRemoveRootModule, buildcRemoveRootSource

                            , buildcFindModuleName
                            , buildcGetDefinitions
                            , buildcGetMatchNames
                            , buildcLookupTypeOf
                            , buildcLookupInfo
                            , buildcOutputDir
                            , buildcSearchSourceFile

                            , runBuildIO, runBuildMaybe

                            , Definitions(..)
                            ) where


import Debug.Trace
import Data.List
import Control.Monad( when )
import qualified Data.Map.Strict as M
import Platform.Config
import Lib.PPrint
import Common.Name
import Common.NamePrim (nameSystemCore, nameTpNamed, nameTpAsync, isSystemCoreName)
import Common.Range
import Common.File
import Common.Error
import Common.Failure
import Common.ColorScheme
import Type.Type
import qualified Type.Pretty as TP
import Type.Kind       (extractHandledEffect, getHandledEffectX )
import Type.Assumption
import Compile.Options
import Compile.Module
import Compile.Build
import Text.Parsec.Error (addErrorMessage)
import Compiler.Compile (searchSource)


data BuildContext = BuildContext {
                      buildcRoots   :: ![ModuleName],
                      buildcModules :: ![Module],
                      buildcFileSys :: !(M.Map FilePath (BString,FileTime)),
                      buildcHash    :: !String
                    }

buildcEmpty :: Flags -> BuildContext
buildcEmpty flags
  = BuildContext [] [] M.empty $! flagsHash flags

buildcVFS :: BuildContext -> VFS
buildcVFS buildc = let fs = buildcFileSys buildc
                   in seq fs $ VFS (\fpath -> M.lookup fpath fs)

buildcAddRootSources :: [FilePath] -> BuildContext -> Build (BuildContext,[ModuleName])
buildcAddRootSources fpaths buildc
  = do mods <- mapM (moduleFromSource (buildcVFS buildc)) fpaths
       let rootNames = map modName mods
           roots   = nub (map modName mods ++ buildcRoots buildc)
           modules = mergeModules mods (buildcModules buildc)
       seqList roots $ seqList modules $
        return (buildc{ buildcRoots = roots, buildcModules = modules }, rootNames)

buildcAddRootModules :: [ModuleName] -> BuildContext -> Build BuildContext
buildcAddRootModules moduleNames buildc
  = do mods <- mapM (moduleFromModuleName (buildcVFS buildc) "" {-relative dir-}) moduleNames
       let roots   = nub (map modName mods ++ buildcRoots buildc)
           modules = mergeModules mods (buildcModules buildc)
       seqList roots $ seqList modules $
        return buildc{ buildcRoots = roots, buildcModules = modules }

buildcClearRoots :: BuildContext -> BuildContext
buildcClearRoots buildc
  = buildc{ buildcRoots = [] }


buildcRemoveRootModule :: ModuleName -> BuildContext -> BuildContext
buildcRemoveRootModule mname buildc
  = buildc{ buildcRoots = filter (/=mname) (buildcRoots buildc) }

buildcFindModuleName :: FilePath -> BuildContext -> Maybe ModuleName
buildcFindModuleName fpath0 buildc
  = let fpath = normalize fpath0
    in modName <$> find (\m -> modSourcePath m == fpath || modSourceRelativePath m == fpath) (buildcModules buildc)

buildcRemoveRootSource :: FilePath -> BuildContext -> BuildContext
buildcRemoveRootSource fpath buildc
  = case buildcFindModuleName fpath buildc of
      Just mname -> buildcRemoveRootModule mname buildc
      _          -> buildc

buildcGetDefinitions :: [ModuleName] -> BuildContext -> Definitions
buildcGetDefinitions modules0 buildc
  = let modules = if null modules0 then buildcRoots buildc else modules
    in defsFromModules (filter (\mod -> modName mod `elem` modules) (buildcModules buildc))

buildcGetMatchNames :: [ModuleName] -> BuildContext -> [String]
buildcGetMatchNames modules buildc
  = let defs = buildcGetDefinitions modules buildc
    in map (showPlain . unqualify) $ gammaPublicNames (defsGamma defs)


buildcFreshFromRoots :: BuildContext -> Build BuildContext
buildcFreshFromRoots buildc
  = do let (roots,imports) = buildcSplitRoots buildc
           rootSources = map modSourcePath roots
       flags <- getFlags
       (buildc1,_) <- buildcAddRootSources rootSources (buildc{ buildcRoots = [], buildcModules=[], buildcHash = flagsHash flags })
       let (roots1,_) = buildcSplitRoots buildc1
       mods  <- modulesReValidate (buildcVFS buildc) False [] [] roots1
       return buildc1{ buildcModules = mods }

buildcValidateFlags :: BuildContext -> Build BuildContext
buildcValidateFlags buildc
  = do flags <- getFlags
       let hash = flagsHash flags
       if (hash == buildcHash buildc)
        then return buildc
        else buildcFreshFromRoots buildc

buildcValidate :: Bool -> [ModuleName] -> BuildContext -> Build BuildContext
buildcValidate rebuild forced buildc
  = do flags <- getFlags
       let hash = flagsHash flags
       if (hash /= buildcHash buildc)
         then buildcFreshFromRoots buildc
         else do let (roots,imports) = buildcSplitRoots buildc
                 mods <- modulesReValidate (buildcVFS buildc) rebuild forced imports roots
                 return buildc{ buildcModules = mods }

buildcSplitRoots :: BuildContext -> ([Module],[Module])
buildcSplitRoots buildc
  = partition (\m -> modName m `elem` buildcRoots buildc) (buildcModules buildc)




buildcTypeCheck :: BuildContext -> Build BuildContext
buildcTypeCheck buildc0
  = do buildc <- buildcValidateFlags buildc0
       mods   <- modulesTypeCheck (buildcModules buildc)
       return buildc{ buildcModules = mods }

buildcBuild :: [Name] -> BuildContext -> Build BuildContext
buildcBuild mainEntries buildc0
  = do buildc <- buildcValidateFlags buildc0
       mods   <- modulesBuild mainEntries (buildcModules buildc)
       return (buildc{ buildcModules = mods})

buildcFullBuild :: Bool -> [ModuleName] -> [Name] -> BuildContext -> Build BuildContext
buildcFullBuild rebuild forced mainEntries buildc0
  = phaseTimed 2 "building" (\penv -> empty) $
    do buildc <- buildcValidate rebuild forced buildc0
       mods <- modulesBuild mainEntries (buildcModules buildc)
       return (buildc{ buildcModules = mods})


buildcRunEntry :: Name -> BuildContext -> Build BuildContext
buildcRunEntry name buildc
  = buildcRunExpr [qualifier name] (show name ++ "()") buildc

buildcRunExpr :: [ModuleName] -> String -> BuildContext -> Build BuildContext
buildcRunExpr importNames expr buildc
  = do (buildc1,mbTpEntry) <- buildcCompileExpr True False importNames expr buildc
       case mbTpEntry of
          Just(_,Just (_,run))
            -> do phase "" (\penv -> empty)
                  liftIO $ run
          _ -> return ()
       return buildc1

buildcCompileEntry :: Bool -> Name -> BuildContext -> Build (BuildContext,Maybe (Type, Maybe (FilePath,IO ())))
buildcCompileEntry typeCheckOnly name buildc
  = buildcCompileExpr False typeCheckOnly [qualifier name] (show name ++ "()") buildc

buildcCompileExpr :: Bool -> Bool -> [ModuleName] -> String -> BuildContext -> Build (BuildContext, Maybe (Type, Maybe (FilePath,IO ())))
buildcCompileExpr addShow typeCheckOnly importNames0 expr buildc
  = phaseTimed 2 "compile" (\penv -> empty) $
    do let importNames = if null importNames0 then buildcRoots buildc else importNames
           sourcePath = joinPaths [
                          virtualMount,
                          case [modSourceRelativePath mod | mname <- importNames,
                                                   mod <- case find (\mod -> modName mod == mname) (buildcModules buildc) of
                                                            Just m  -> [m]
                                                            Nothing -> []] of
                              (fpath:_) -> noexts fpath
                              _         -> "",
                          "@main" ++ sourceExtension]
           importDecls = map (\mname -> "@open import " ++ show mname) importNames
           content     = bunlines $ importDecls ++ [
                           "pub fun @expr()",
                           "#line 1",
                           "  " ++ expr
                         ]

       withVirtualModule sourcePath content buildc $ \mainModName buildc1 ->
         do -- type check first
            let exprName = qualify mainModName (newName "@expr")
            buildc2 <- buildcValidate False [] buildc1
            buildc3 <- buildcTypeCheck buildc2
            mbRng   <- hasBuildError
            case mbRng of
              Just rng -> do when (addShow || typeCheckOnly) $ showMarker rng
                             return (buildc3,Nothing)
              Nothing  -> case buildcLookupTypeOf exprName buildc3 of
                            Nothing -> do addErrorMessageKind ErrBuild (\penv -> text "unable to resolve the type of the expression" <+> parens (TP.ppName penv exprName))
                                          return (buildc3, Nothing)
                            Just tp -> if typeCheckOnly
                                        then return (buildc3,Just (tp,Nothing))
                                        else buildcCompileMainBody addShow expr importDecls sourcePath mainModName exprName tp buildc3

buildcCompileMainBody :: Bool -> String -> [String] -> FilePath -> Name -> Name -> Type -> BuildContext -> Build (BuildContext,Maybe (Type, Maybe (FilePath,IO ())))
buildcCompileMainBody addShow expr importDecls sourcePath mainModName exprName tp buildc1
  = do  -- then compile with a main function
        (tp,showIt,mainBody) <- completeMain True exprName tp buildc1
        let mainName = qualify mainModName (newName "@main")
            mainDef  = bunlines $ importDecls ++ [
                        "pub fun @expr() : _ ()",
                        "#line 1",
                        "  " ++ showIt expr,
                        "",
                        "pub fun @main() : io ()",
                        "  " ++ mainBody,
                        ""
                        ]
        buildc2 <- buildcSetVirtualFile sourcePath mainDef buildc1
        buildc3 <- buildcValidate False [] buildc2
        buildc4 <- buildcBuild [mainName] buildc3
        mbRng <- hasBuildError
        case mbRng of
          Just rng -> do when addShow $ showMarker rng
                         return (buildc4,Nothing)
          _        -> do -- and return the entry point
                         let mainMod = buildcFindModule mainModName buildc4
                             entry   = modEntry mainMod
                         return $ seq entry (buildc4,Just(tp,entry))

showMarker :: Range -> Build ()
showMarker rng
  = do let c1 = posColumn (rangeStart rng)
           c2 = if (posLine (rangeStart rng) == posLine (rangeStart rng))
                 then posColumn (rangeEnd rng)
                else c1
       cscheme <- getColorScheme
       term    <- getTerminal
       let doc = color (colorMarker cscheme) (text (replicate (c1 - 1) ' ' ++ replicate 1 {- (c2 - c1 + 1) -} '^'))
       liftIO $ termInfo term doc


bunlines :: [String] -> BString
bunlines xs = stringToBString $ unlines xs

completeMain :: Bool -> Name -> Type -> BuildContext -> Build (Type,String -> String,String)
completeMain addShow exprName tp buildc
  = case splitFunScheme tp of
      Just (_,_,_,eff,resTp)
        -> let (ls,_) = extractHandledEffect eff
           in do print    <- printExpr resTp
                 mainBody <- addDefaultHandlers rangeNull eff ls callExpr
                 return (resTp,print,mainBody)
      _ -> return (tp, id, callExpr) -- todo: given an error?
  where
    callExpr
      = show exprName ++ "()"

    printExpr resTp
      = if isTypeUnit resTp || not addShow
          then return id
          else case expandSyn resTp of
                 TFun _ _ _ -> return (\expr -> "println(\"<function>\")")
                 _          -> return (\expr -> "(" ++ expr ++ ").println")

    exclude = [nameTpNamed] -- nameTpCps,nameTpAsync

    addDefaultHandlers :: Range -> Effect -> [Effect] -> String -> Build String
    addDefaultHandlers range eff [] body     = return body
    addDefaultHandlers range eff (l:ls) body
      = case getHandledEffectX exclude l of
          Nothing -> addDefaultHandlers range eff ls body
          Just (_,effName)
            -> let defaultHandlerName
                      = makeHiddenName "default" (if isSystemCoreName effName
                                                    then qualify nameSystemCore (unqualify effName) -- std/core/* defaults must be in std/core
                                                    else effName) -- and all others in the same module as the effect
              in case buildcLookupInfo defaultHandlerName buildc of
                    [fun@InfoFun{}]
                      -> do phaseVerbose 2 "main" $ \penv -> text "add default effect for" <+> TP.ppName penv effName
                            let handle b = show defaultHandlerName ++ "(fn() " ++ b ++ ")"
                            if (effName == nameTpAsync)  -- always put async as the most outer effect
                              then do body' <- addDefaultHandlers range eff ls body
                                      return (handle body')
                              else addDefaultHandlers range eff ls (handle body)
                    infos
                      -> do throwError (\penv -> errorMessageKind ErrBuild range
                                           (text "there are unhandled effects for the main expression" <-->
                                            text " inferred effect :" <+> TP.ppType penv eff <-->
                                            text " unhandled effect:" <+> TP.ppType penv l <-->
                                            text " hint            : wrap the main function in a handler"))
                            addDefaultHandlers range eff ls body


withVirtualModule :: FilePath -> BString -> BuildContext -> (ModuleName -> BuildContext -> Build (BuildContext,a)) -> Build (BuildContext,a)
withVirtualModule fpath0 content buildc action
  = do let fpath = normalize fpath0
       buildc1 <- buildcSetVirtualFile fpath content buildc
       (buildc2,[modName]) <- buildcAddRootSources [fpath] buildc1
       (buildc3,x) <- action modName buildc2
       buildc4 <- buildcDeleteVirtualFile fpath buildc3
       return (buildcRemoveRootSource fpath buildc4, x)

buildcSetVirtualFile :: FilePath -> BString -> BuildContext -> Build BuildContext
buildcSetVirtualFile fpath0 content buildc
  = do let fpath = normalize fpath0
       ftime <- liftIO $ getCurrentTime
       phaseVerbose 2 "trace" (\penv -> text "add virtual file" <+> text fpath <+> text ", content:" <-> text (bstringToString content))
       return buildc{ buildcFileSys = M.insert fpath (content,ftime) (buildcFileSys buildc) }

buildcDeleteVirtualFile :: FilePath -> BuildContext -> Build BuildContext
buildcDeleteVirtualFile fpath0 buildc
  = do let fpath = normalize fpath0
       return buildc{ buildcFileSys = M.delete fpath (buildcFileSys buildc) }

buildcFindModule :: HasCallStack => ModuleName -> BuildContext -> Module
buildcFindModule modname buildc
  = case find (\mod -> modName mod == modname) (buildcModules buildc) of
      Just mod -> mod
      _        -> failure ("Compile.BuildIde.btxFindModule: cannot find " ++ show modname ++ " in " ++ show (map modName (buildcModules buildc)))


buildcLookupInfo :: Name -> BuildContext -> [NameInfo]
buildcLookupInfo name buildc
  = case find (\mod -> modName mod == qualifier name) (buildcModules buildc) of
      Just mod -> -- trace ("lookup " ++ show name ++ " in " ++ show (modName mod) ++ "\n" ++ showHidden (defsGamma (defsFromModules [mod]))) $
                  map snd (gammaLookup name (defsGamma (defsFromModules [mod])))
      _        -> []

buildcLookupTypeOf :: Name -> BuildContext -> Maybe Type
buildcLookupTypeOf name buildc
  = case buildcLookupInfo name buildc of
      [info] | isInfoValFunExt info -> Just (infoType info)
      _      -> Nothing

buildcOutputDir :: Build FilePath
buildcOutputDir
  = do flags <- getFlags
       return (outName flags "")

buildcSearchSourceFile :: FilePath -> BuildContext -> Build (Maybe (FilePath,FilePath))
buildcSearchSourceFile fpath buildc
  = searchSourceFile (buildcVFS buildc) "" fpath

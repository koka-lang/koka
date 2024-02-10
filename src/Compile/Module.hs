-----------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Modules
    Should be abstract and hidden and only used by `Build` and `BuildContext`.
    other libraries should use BuildContext if possible.
-}
-----------------------------------------------------------------------------
module Compile.Module( Module(..), ModulePhase(..)
                     , moduleNull, moduleCreateInitial
                     , modCoreImports, modImportNames

                     , Definitions(..), defsNull
                     , defsCompose, defsFromCore, defsFromModules

                     , Modules
                     , inlinesFromModules -- , mergeModules
                     , mergeModulesLeftBias
                     , phaseProgress, isErrorPhase
                     ) where

import Lib.Trace
import Lib.PPrint
import Data.List              ( foldl' )
import Data.Char              ( isAlphaNum )
import Common.Range           ( Range, rangeNull, makeSourceRange, Source, sourceNull )
import Common.Name            ( Name, ModuleName, newName, unqualify, isHiddenName, showPlain)
import Common.Error
import Common.File

import Syntax.Syntax
import Syntax.Lexeme
import Syntax.RangeMap
import Type.Assumption        ( Gamma )
import qualified Core.Core as Core


import Static.FixityResolve   ( Fixities, fixitiesEmpty, fixitiesNew, fixitiesCompose )
import Kind.ImportMap
import Kind.Synonym           ( Synonyms, synonymsEmpty, synonymsCompose, extractSynonyms )
import Kind.Newtypes          ( Newtypes, newtypesEmpty, newtypesCompose, extractNewtypes )
import Kind.Constructors      ( Constructors, constructorsEmpty, constructorsCompose, extractConstructors )
import Kind.Assumption        ( KGamma, kgammaInit, extractKGamma, kgammaUnion, kgammaUnionLeftBias )

import Type.Assumption        ( Gamma, gammaInit, gammaUnion, extractGamma, gammaNames, gammaPublicNames)
import Type.Type              ( DataInfo )
import Core.Inlines           ( Inlines, inlinesNew, inlinesEmpty, inlinesExtends )
import Core.Borrowed          ( Borrowed, borrowedEmpty, extractBorrowed, borrowedCompose )
import Common.Failure (HasCallStack)


{--------------------------------------------------------------------------
  Compilation
--------------------------------------------------------------------------}
type Modules = [Module]

data ModulePhase
  = PhaseInit
  | PhaseLexed          -- modLexemes, modDeps (always succeeds with possible error lexemes)
  | PhaseParsedError
  | PhaseParsed         -- modProgram
  | PhaseTypedError
  | PhaseTyped          -- modCore, modRangeMap, modDefines
  | PhaseIfaceLoaded    -- interface is loaded, but the inline definitions are not yet parsed
  | PhaseOptimized      -- compiled and optimized core, modCore is updated, modInlines
  | PhaseCodeGen        -- compiled to backend code (.c,.js files)
  | PhaseLibIfaceLoaded -- a (library) interface is loaded but it's kki and libs are not yet copied to the output directory
  | PhaseLinkedError
  | PhaseLinked         -- kki and object files are generated (and exe linked for a main module)
  deriving (Eq,Ord,Show,Enum)

data Module  = Module{ -- initial
                       modPhase       :: !ModulePhase
                     , modName        :: !Name
                     , modRange       :: !Range             -- (1,1) in the source (or pre-compiled iface)
                     , modErrors      :: !Errors            -- collected errors; set for Phase<xxx>Error phases

                     , modIfacePath   :: !FilePath          -- output interface (.kki)
                     , modIfaceTime   :: !FileTime
                     , modLibIfacePath:: !FilePath          -- precompiled interface (for example for the std libs in <prefix>/lib)
                     , modLibIfaceTime:: !FileTime
                     , modSourcePath  :: !FilePath          -- can be empty for pre-compiled sources
                     , modSourceRelativePath :: !FilePath   -- for messages display a shorter path if possible
                     , modSourceTime  :: !FileTime

                       -- lexing
                     , modSource      :: !Source
                     , modLexemes     :: ![Lexeme]
                     , modDeps        :: ![LexImport]       -- initial dependencies from import statements in the program

                       -- parsing
                     , modProgram     :: !(Maybe (Program UserType UserKind))

                       -- type check; modCore is initial core that is not yet core-compiled
                     , modRangeMap    :: !(Maybe RangeMap)
                     , modCore        :: !(Maybe Core.Core)
                     , modDefinitions :: !(Maybe Definitions)

                     -- core optimized; updates `modCore` to final core
                     -- from parsing, `modInlines` contains Right inline definitions,
                     -- from an interface file, `modInlines` is a Left function that given a gamma parses the inline definitions
                     , modInlines     :: !(Either (Gamma -> Error () [Core.InlineDef]) [Core.InlineDef])

                     -- codegen
                     -- entry is set if compiled with a main entry; contains the executable path and
                     -- an IO function that runs it using a correct command line
                     , modEntry       :: !(Maybe (FilePath,IO()))

                     -- temporary values
                     , modShouldOpen  :: !Bool
                     }


moduleNull :: Name -> Module
moduleNull modName
  = Module  PhaseInit modName rangeNull errorsNil
            "" fileTime0 "" fileTime0 "" "" fileTime0
            -- lex
            sourceNull [] []
            -- parse
            Nothing
            -- type check
            Nothing Nothing Nothing
            -- core compiled
            (Right [])
            -- codegen
            Nothing
            -- temporary
            False

moduleCreateInitial :: Name -> FilePath -> FilePath -> FilePath -> Module
moduleCreateInitial modName sourcePath ifacePath libIfacePath
  = (moduleNull modName){ modSourcePath = sourcePath,
                          modSourceRelativePath = sourcePath,
                          modIfacePath = ifacePath,
                          modLibIfacePath = libIfacePath,
                          modRange = makeSourceRange (if null sourcePath then ifacePath else sourcePath) 1 1 1 1 }

{-
mergeModules :: [Module] -> [Module] -> [Module]
mergeModules mods1 mods2
  = seqqList $ mergeModulesWith (\m1 m2 -> if modPhase m1 >= modPhase m2 then m1 else m2) mods1 mods2
-}

mergeModulesLeftBias :: [Module] -> [Module] -> [Module]
mergeModulesLeftBias mods1 mods2
  = seqqList $ mergeModulesWith (\m1 m2 -> m1) mods1 mods2


mergeModulesWith :: (Module -> Module -> Module) -> [Module] -> [Module] -> [Module]
mergeModulesWith combine mods1 mods2
  = seqqList $ foldl' (mergeModuleWith combine) mods1 mods2

mergeModuleWith :: (Module -> Module -> Module) -> [Module] -> Module -> [Module]
mergeModuleWith combine [] mod  = [mod]
mergeModuleWith combine (m:ms) mod
  = if modName m /= modName mod
     then m : mergeModuleWith combine ms mod
     else combine m mod : ms


modCoreImports :: Module -> [Core.Import]
modCoreImports mod
  = case modCore mod of
      Nothing   -> []
      Just core -> seqqList $! Core.coreProgImports core


modImportNames :: Module -> [ModuleName]
modImportNames mod
  = seqqList $! map Core.importName (modCoreImports mod)



data Definitions  = Definitions {
                        defsGamma       :: !Gamma
                      , defsKGamma      :: !KGamma
                      , defsSynonyms    :: !Synonyms
                      , defsNewtypes    :: !Newtypes
                      , defsConstructors:: !Constructors
                      , defsFixities    :: !Fixities
                      , defsBorrowed    :: !Borrowed
                    }

defsNull :: Definitions
defsNull = Definitions gammaInit
                      kgammaInit
                      synonymsEmpty
                      newtypesEmpty
                      constructorsEmpty
                      fixitiesEmpty
                      borrowedEmpty


defsNames :: Definitions -> [Name]
defsNames defs
  = gammaNames (defsGamma defs)

defsMatchNames :: Definitions -> [String]
defsMatchNames defs
  = seqqList $ map (showPlain . unqualify) $ gammaPublicNames (defsGamma defs)


defsFromCore :: Bool -> Core.Core -> Definitions
defsFromCore privateAsPublic core
  = Definitions (extractGamma Core.dataInfoIsValue privateAsPublic core)
                (extractKGamma core)
                (extractSynonyms core)
                (extractNewtypes core)
                (extractConstructors core)
                (extractFixities core)
                (extractBorrowed core)
  where
    extractFixities :: Core.Core -> Fixities
    extractFixities core
      = fixitiesNew [(name,fix) | Core.FixDef name fix <- Core.coreProgFixDefs core]


defsFromModules :: HasCallStack => [Module] -> Definitions
defsFromModules mods
  = let defs = defsMerge $ map (\mod -> {- case modDefinitions mod of
                               Just defs | not (modShouldOpen mod) -> defs  -- cached
                               _ -> -} case modCore mod of
                                      Just core -> defsFromCore (modShouldOpen mod) core
                                      Nothing   -> defsNull) mods
    in seq defs defs

defsMerge :: HasCallStack => [Definitions] -> Definitions
defsMerge defss
  = let defs = foldl' defsCompose defsNull defss
    in seq defs defs

defsCompose :: HasCallStack => Definitions -> Definitions -> Definitions
defsCompose defs1 defs2
  = Definitions (gammaUnion (defsGamma defs1) (defsGamma defs2))
                (kgammaUnionLeftBias(defsKGamma defs1) (defsKGamma defs2))
                (synonymsCompose (defsSynonyms defs1) (defsSynonyms defs2))
                (newtypesCompose (defsNewtypes defs1) (defsNewtypes defs2))
                (constructorsCompose (defsConstructors defs1) (defsConstructors defs2))
                (fixitiesCompose (defsFixities defs1) (defsFixities defs2))
                (borrowedCompose (defsBorrowed defs1) (defsBorrowed defs2))


inlinesFromModules :: [Module] -> Inlines
inlinesFromModules modules
  = inlinesExtends (concatMap inlineDefsFromModule modules) inlinesEmpty
  where
    inlineDefsFromModule mod
      = case modInlines mod of
          Right idefs -> idefs
          _           -> []      -- todo: interface files should go from typed to compiled after we resolve these





phaseProgress :: ModulePhase -> String
phaseProgress latest =
  case latest of
    PhaseInit           -> "determining dependencies..."
    PhaseLexed          -> "scanning..."
    PhaseParsedError    -> "encountered a parse error"
    PhaseParsed         -> "parsing..."
    PhaseTypedError     -> "encountered a type error"
    PhaseTyped          -> "type checking..."
    PhaseIfaceLoaded    -> "loading interface..."
    PhaseOptimized      -> "optimizing..."
    PhaseCodeGen        -> "generating code..."
    PhaseLibIfaceLoaded -> "copying libraries..."
    PhaseLinkedError    -> "encountered a link error"
    PhaseLinked         -> "linking..."

isErrorPhase :: ModulePhase -> Bool
isErrorPhase phase =
  case phase of
    PhaseParsedError -> True
    PhaseTypedError -> True
    PhaseLinkedError -> True
    _ -> False

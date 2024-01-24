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
module Compile.Module( Module(..), Modules, ModulePhase(..), moduleNull, moduleCreateInitial
                      ) where

import Lib.Trace
import Lib.PPrint
import Data.Char              ( isAlphaNum )
import Common.Range           ( Range, rangeNull, makeSourceRange )
import Common.Name            ( Name, ModuleName, newName, unqualify, isHiddenName, showPlain)
import Common.Error
import Common.File            ( FileTime, fileTime0, maxFileTimes, splitPath )

import Syntax.Syntax
import Syntax.Lexeme
import Syntax.RangeMap
import Type.Assumption        ( Gamma )
import qualified Core.Core as Core


{--------------------------------------------------------------------------
  Compilation
--------------------------------------------------------------------------}
type Modules = [Module]

data ModulePhase
  = ModInit
  | ModLoaded         -- imports are known
  | ModParsed         -- lexemes, program is known
  | ModTyped          -- rangemap, gamma, inlines
  | ModOptimized      -- optimized core
  | ModCompiled       -- compiled
  deriving (Eq,Ord,Show)

data Module  = Module{ -- initial
                       modPhase       :: !ModulePhase
                     , modName        :: !Name
                     , modRange       :: !Range             -- (1,1) in the source (or pre-compiled iface)
                     , modErrors      :: !Errors            -- collected errors

                     , modIfacePath   :: !FilePath          -- output interface (.kki)
                     , modIfaceTime   :: !FileTime
                     , modLibIfacePath:: !FilePath          -- precompiled interface (for example for the std libs in <prefix>/lib)
                     , modLibIfaceTime:: !FileTime
                     , modSourcePath  :: !FilePath          -- can be empty for pre-compiled sources
                     , modSourceTime  :: !FileTime

                       -- lexing
                     , modLexemes     :: ![Lexeme]
                     , modImports     :: ![ModuleName]      -- initial dependencies from import statements

                       -- parsing
                     , modProgram     :: !(Maybe (Program UserType UserKind))

                       -- type check
                     , modRangeMap    :: !(Maybe RangeMap)
                     -- , modInitialCore :: !Core.Core

                       -- optimize & interface
                     , modCore        :: !(Maybe Core.Core)
                     , modInlines     :: -- from a core file, we return a function that given the gamma parses the inlines
                                         !(Either (Gamma -> Error () [Core.InlineDef]) ([Core.InlineDef]))

                       -- codegen
                     , modExePath     :: !FilePath
                     , modExeTime     :: !FileTime

                       -- unused
                    --  , modCompiled    :: !Bool
                    --  , modTime        :: !FileTime
                     --, modPackageQName:: FilePath          -- A/B/C
                     --, modPackageLocal:: FilePath          -- lib
                     }


moduleNull :: Name -> Module
moduleNull modName
  = Module  ModInit modName rangeNull errorsNil
            "" fileTime0 "" fileTime0 "" fileTime0
            -- lex
            [] []
            -- parse
            Nothing
            -- type check
            Nothing
            -- optimize
            Nothing (Right [])
            -- codegen
            "" fileTime0

moduleCreateInitial :: Name -> FilePath -> FilePath -> FilePath -> Module
moduleCreateInitial modName sourcePath ifacePath libIfacePath
  = (moduleNull modName){ modSourcePath = sourcePath,
                          modIfacePath = ifacePath,
                          modLibIfacePath = libIfacePath,
                          modRange = makeSourceRange (if null sourcePath then ifacePath else sourcePath) 1 1 1 1 }

{-
modPackageName :: Module -> PackageName
modPackageName mod
  = last (splitPath (modPackageQName mod))

modPackagePath :: Module -> PackageName
modPackagePath mod
  = joinPkg (modPackageName mod) (modPackageLocal mod)

modPackageQPath :: Module -> PackageName
modPackageQPath mod
  = joinPkg (modPackageQName mod) (modPackageLocal mod)
-}
--modLexemes :: Module -> [Lexeme]
--modLexemes mod = case modProgram mod of
--                   Just program -> programLexemes program
--                   _            -> []



addOrReplaceModule :: Module -> Modules -> Modules
addOrReplaceModule mod []
  = [mod]
addOrReplaceModule mod (m:ms)
  = if modIfacePath mod == modIfacePath m
     then mod:ms
     else m : addOrReplaceModule mod ms

removeModule :: Name -> Modules -> Modules
removeModule name modules
  = filter (\m -> modName m /= name) modules

-- extractFixities :: Core.Core -> Fixities
-- extractFixities core
--   = fixitiesNew [(name,fix) | Core.FixDef name fix <- Core.coreProgFixDefs core]

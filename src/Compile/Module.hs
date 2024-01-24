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
module Compile.Module( Module(..), Modules, ModulePhase(..), moduleNull, moduleCreateEmpty
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
  = ModEmpty
  -- | ModLexed  -- todo: we can increase parallelism by having the lexer as a phase (so we know the imports/dependencies more early)
  | ModParsed
  | ModTyped
  | ModOptimized
  | ModCompiled
  deriving (Eq,Ord,Show)

data Module  = Module{ -- initial
                       modPhase       :: !ModulePhase
                     , modName        :: !Name
                     , modIfacePath   :: !FilePath
                     , modSourcePath  :: !FilePath          -- can be empty
                     , modRange       :: !Range
                     , modErrors      :: !Errors      --
                       -- lexing
                     , modSourceTime  :: !FileTime
                     , modLexemes     :: ![Lexeme]
                     , modImports     :: ![ModuleName]
                       -- parsing
                     , modProgram     :: !(Maybe (Program UserType UserKind))
                       -- type check
                     , modRangeMap    :: !(Maybe RangeMap)
                     , modInitialCore :: !Core.Core
                       -- optimize & interface
                     , modIfaceTime   :: !FileTime
                     , modCore        :: !Core.Core
                     , modInlines     :: !(Either (Gamma -> Error () [Core.InlineDef]) ([Core.InlineDef]))
                       -- codegen
                     , modExeTime     :: !FileTime

                       -- unused
                     , modCompiled    :: !Bool
                     , modTime        :: !FileTime
                     --, modPackageQName:: FilePath          -- A/B/C
                     --, modPackageLocal:: FilePath          -- lib
                     }


moduleNull :: Name -> Module
moduleNull modName
  = Module  ModEmpty modName "" "" rangeNull errorsNil
            fileTime0 [] []
            Nothing
            Nothing    (Core.coreNull modName)
            fileTime0  (Core.coreNull modName)  (Left (\g -> return []))
            fileTime0
            False fileTime0

moduleCreateEmpty :: Name -> FilePath -> FilePath -> Module
moduleCreateEmpty modName sourcePath ifacePath
  = (moduleNull modName){ modSourcePath = sourcePath, modIfacePath = ifacePath,
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

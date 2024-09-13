-----------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Compile.TypeCheck( typeCheck, importMapFromCoreImports ) where

import Debug.Trace
import Data.Char
import Data.Maybe
import Data.List
import Data.Either
import Control.Monad

import Lib.PPrint
import Common.Error
import Common.Range
import Common.Unique
import Common.Name
import Common.NamePrim( isPrimitiveModule, isPrimitiveName, nameCoreHnd )
import Common.Syntax
import qualified Common.NameSet as S
import Syntax.RangeMap
import Syntax.Syntax
import Static.FixityResolve( fixitiesCompose, fixitiesNew, fixityResolve )
import Static.BindingGroups( bindingGroups )
import Core.Pretty( prettyDef, prettyCore )
import Core.CoreVar( extractDepsFromSignatures )

import Core.Check( checkCore )
import Core.CheckFBIP( checkFBIP )
import Core.MatchMerge( matchMergeDefs )
import Core.Simplify( simplifyDefs )
import Core.FunLift( liftFunctions )
import Core.UnReturn( unreturn )
import Core.Borrowed
import Core.Uniquefy( uniquefy )

import Kind.Assumption( extractKGamma )
import Kind.Newtypes( Newtypes )
import Kind.ImportMap
import Kind.Infer( inferKinds )
import Type.Pretty
import Type.Assumption( Gamma, extractGamma, extractGammaImports, gammaUnions, showHidden )
import Type.Infer( inferTypes )
import qualified Core.Core as Core
import Compile.Options
import Compile.Module( Definitions(..), Module (modName) )


{---------------------------------------------------------------
  Type check
---------------------------------------------------------------}

typeCheck :: Flags -> Definitions -> [Core.Import] -> UserProgram -> Error () (Core.Core,Maybe RangeMap)
typeCheck flags defs coreImports program0
  = Core.runCorePhase 0 {-unique-} $
     do -- import map
        let importMap   = importMapFromCoreImports (programImports program0) coreImports
            progName    = getName program0
            penv        = (prettyEnvFromFlags flags){ context = progName }

        -- binding groups and fixities
        let fixities     = fixitiesNew [(name,fix) | FixDef name fix rng vis <- programFixDefs program0]
            coreFixities = [Core.FixDef name fix   | FixDef name fix rng vis <- programFixDefs program0, vis == Public]
            program1 = bindingGroups program0
        (program2,_) <- Core.liftError $ fixityResolve (colorSchemeFromFlags flags)
                                                        (fixitiesCompose (defsFixities defs) fixities) program1

        -- kind inference
        (progDefs, kgamma, synonyms, newtypes, constructors, coreProgram, mbRangeMap0)
          <- inferKinds
              (Core.dataInfoIsValue)
              (colorSchemeFromFlags flags)
              (platform flags)
              (if (outHtml flags > 0 || genRangeMap flags) then Just rangeMapNew else Nothing)
              importMap
              (defsKGamma defs)
              (defsSynonyms defs)
              (defsNewtypes defs)
              program2

        -- initial gamma
        let  gamma0  = gammaUnions [defsGamma defs
                                  ,extractGamma Core.dataInfoIsValue True coreProgram
                                  ,extractGammaImports (importsList importMap) progName
                                  ]

        -- Type inference
        (gamma,coreDefs,mbRangeMap1)
          <- inferTypes
              penv
              mbRangeMap0
              synonyms
              newtypes
              constructors
              importMap
              gamma0
              progName
              progDefs

        Core.setCoreDefs coreDefs

        -- when (show progName == "std/text/parse") $
        --   trace ("type check " ++ show progName ++ ", gamma: " ++ showHidden gamma) $ return ()

        -- check generated core
        let checkCoreDefs title = when (coreCheck flags) $ Core.Check.checkCore False False penv gamma
        -- traceDefGroups "initial"

        -- remove return statements
        unreturn penv

        -- checkCoreDefs "unreturn"
        let borrowed = borrowedExtendICore (coreProgram{ Core.coreProgDefs = coreDefs }) (defsBorrowed defs)
        checkFBIP penv (platform flags) newtypes borrowed gamma
        
        matchMergeDefs
        -- trace "Finished match merging" $ return ()
        -- coreDefs <- Core.getCoreDefs
        -- let coreDoc2 = Core.Pretty.prettyCore (prettyEnvFromFlags flags){ coreIface = False, coreShowDef = True } (C CDefault) [] 
        --                  (coreProgram{ Core.coreProgDefs = coreDefs })
        -- trace (show coreDoc2) $ return ()

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

        coreDefsFinal <- Core.getCoreDefs
        uniqueFinal   <- unique
        -- traceM ("final: " ++ show uniqueFinal)

        let mbRangeMap       = fmap rangeMapSort mbRangeMap1
            coreUnique       = uniquefy $ coreProgram {
                                 Core.coreProgImports = coreImports,
                                 Core.coreProgDefs    = coreDefsFinal,
                                 Core.coreProgFixDefs = coreFixities
                               }

            -- add extra imports needed to resolve types in this module
            typeDeps         = extractDepsFromSignatures coreUnique
            currentImports   = S.fromList (map Core.importName coreImports)
            typeImports      = [Core.Import name "" Core.ImportTypes Private "" | name <- typeDeps, not (S.member name currentImports) && not (name == progName)]
            coreFinal        = coreUnique{ Core.coreProgImports = Core.coreProgImports coreUnique ++ typeImports }

        return (coreFinal,mbRangeMap)




-- import map needs the program imports for aliases.
importMapFromCoreImports :: [Import] -> [Core.Import] -> ImportMap
importMapFromCoreImports progImports cimports
  = foldl' extend importsEmpty (map Core.importName cimports)
  where
    extend importMap modname
      = case importsExtend (getAlias modname) modname importMap of
          Nothing         -> importMap
          Just importMap' -> importMap'

    getAlias modname
      = case find (\imp -> importFullName imp == modname) progImports of
          Just imp -> importName imp
          Nothing  -> modname



traceDefGroups :: Flags -> String -> Core.CorePhase () ()
traceDefGroups flags title
  = do dgs <- Core.getCoreDefs
       trace (unlines (["","/* -----------------", title, "--------------- */"] ++
                map showDef (Core.flattenDefGroups dgs))) $ return ()
  where
    showDef def = show (Core.Pretty.prettyDef (penv{coreShowDef=True}) def)
    penv = prettyEnvFromFlags flags


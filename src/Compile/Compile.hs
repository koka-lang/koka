-----------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Compile.Compile( typeCheck, compileCore, importMapFromCoreImports ) where

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
import Core.Pretty( prettyDef )
import Core.CoreVar( extractDepsFromInlineDefs, extractDepsFromSignatures )

import Core.Check( checkCore )
import Core.CheckFBIP( checkFBIP )
import Core.Simplify( simplifyDefs )
import Core.FunLift( liftFunctions )
import Core.UnReturn( unreturn )
import Core.Borrowed
import Core.Uniquefy( uniquefy )

import Core.Inline( inlineDefs  )
import Core.Inlines( Inlines, inlinesFilter, inlinesExtends, extractInlineDefs)
import Core.Monadic( monTransform )
import Core.MonadicLift( monadicLift )
import Core.Specialize( specialize, extractSpecializeDefs )
import Core.CTail( ctailOptimize )
import Core.OpenResolve( openResolve )
import Core.Unroll( unrollDefs )

import Kind.Assumption( extractKGamma )
import Kind.Newtypes( Newtypes )
import Kind.ImportMap
import Kind.Infer( inferKinds )
import Type.Pretty
import Type.Assumption( Gamma, extractGamma, extractGammaImports, gammaUnions, showHidden )
import Type.Infer( inferTypes )
import qualified Core.Core as Core
import Compiler.Options
import Compile.Module( Definitions(..) )


{---------------------------------------------------------------
  Type check
---------------------------------------------------------------}

typeCheck :: Flags -> Definitions -> [Core.Import] -> UserProgram -> Error () (Core.Core,Maybe RangeMap)
typeCheck flags defs coreImports program0
  = Core.runCorePhase 0 {-unique-} $
     do -- import map
        let importMap   = importMapFromCoreImports (programImports program0) coreImports
            programName = getName program0
            penv        = prettyEnvFromFlags flags

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
                                  ,extractGammaImports (importsList importMap) programName
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
              programName
              progDefs

        Core.setCoreDefs coreDefs

        -- when (show programName == "std/text/parse") $
        --   trace ("type check " ++ show programName ++ ", gamma: " ++ showHidden gamma) $ return ()

        -- check generated core
        let checkCoreDefs title = when (coreCheck flags) $ Core.Check.checkCore False False penv gamma
        -- traceDefGroups "initial"

        -- remove return statements
        unreturn penv

        -- checkCoreDefs "unreturn"
        let borrowed = borrowedExtendICore (coreProgram{ Core.coreProgDefs = coreDefs }) (defsBorrowed defs)
        checkFBIP penv (platform flags) newtypes borrowed gamma

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
            typeImports      = [Core.Import name "" Core.ImportTypes Private "" | name <- typeDeps, not (S.member name currentImports)]
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


{---------------------------------------------------------------
  compile core:
  these may need a richer gamma with
  hidden imports to check the inlined definitions.
---------------------------------------------------------------}

compileCore :: Flags -> Newtypes -> Gamma -> Inlines -> Core.Core -> Error () (Core.Core,[Core.InlineDef])
compileCore flags newtypes gamma inlines coreProgram
  = Core.runCorePhase 10000 {-unique-} $
     do Core.setCoreDefs (Core.coreProgDefs coreProgram)
        let progName = Core.coreProgName coreProgram
            penv     = prettyEnvFromFlags flags
            checkCoreDefs title = when (coreCheck flags) $ Core.Check.checkCore False False penv gamma

        -- when (show progName == "std/text/parse") $
        --  trace ("compile " ++ show progName ++ ", gamma: " ++ showHidden gamma) $ return ()

        -- define simplify
        let ndebug           = optimize flags > 0
            simplifyX dupMax = simplifyDefs penv False {-unsafe-} ndebug (simplify flags) dupMax
            simplifyDupN     = when (simplify flags >= 0) $ simplifyX (simplifyMaxDup flags)
            simplifyNoDup    = simplifyX 0

        -- unroll recursive definitions (before inline so generated wrappers can be inlined)
        when (optUnroll flags > 0) $
          do unrollDefs penv (optUnroll flags)
        -- traceDefGroups flags "unrolled"

        -- inline: inline local definitions more aggressively (2x)
        when (optInlineMax flags > 0) $
          do let inlinesX = if isPrimitiveModule progName then inlines
                            else inlinesFilter (\name -> nameModule nameCoreHnd /= nameModule name) inlines
             inlineDefs penv (2*(optInlineMax flags)) inlinesX
              -- checkCoreDefs "inlined"

        simplifyDupN
        -- traceDefGroups flags "inlined"

        -- specialize
        specializeDefs <- if isPrimitiveModule progName then return []
                            else Core.withCoreDefs (\defs -> extractSpecializeDefs inlines defs)
        -- traceM ("Spec defs:\n" ++ unlines (map show specializeDefs))

        when (optSpecialize flags && not (isPrimitiveModule progName)) $
          do specialize (inlinesExtends specializeDefs inlines) penv
             -- traceDefGroups flags "specialized"
             simplifyDupN
             -- lifting remaining recursive functions to top level (must be after specialize as that can generate local recursive definitions)
             liftFunctions penv

        -- simplify once more
        simplifyDupN
        coreDefsInlined <- Core.getCoreDefs
        -- traceDefGroups flags "simplified"

        ------------------------------
        -- backend optimizations

        -- tail-call-modulo-cons optimization
        when (optctail flags) $
          ctailOptimize penv newtypes gamma (optctailCtxPath flags)

        -- transform effects to explicit monadic binding (and resolve .open calls)
        when (enableMon flags && not (isPrimitiveModule progName)) $
          -- trace (show progName ++ ": monadic transform") $
          do Core.Monadic.monTransform penv
             openResolve penv gamma           -- must be after monTransform
        checkCoreDefs "monadic transform"

        -- simplify open applications (needed before inlining open defs)
        simplifyNoDup
        -- traceDefGroups flags "open resolved"

        -- monadic lifting to create fast inlined paths
        monadicLift penv
        checkCoreDefs "monadic lifting"
        -- traceDefGroups flags "monadic lift"

        -- now inline primitive definitions (like yield-bind)
        let inlinesX = inlinesFilter isPrimitiveName inlines
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

        let localInlineDefs  = extractInlineDefs (optInlineMax flags) coreDefsInlined
            -- give priority to specializeDefs, since inlining can prevent specialize opportunities
            allInlineDefs    = specializeDefs ++ localInlineDefs

            -- add extra required imports for inlined definitions
            inlineDeps       = extractDepsFromInlineDefs allInlineDefs
            currentImports   = map Core.importName (Core.coreProgImports coreProgram)
            inlineImports    = [Core.Import name "" Core.ImportCompiler Private "" | name <- inlineDeps, not (name `elem` currentImports)]

            coreFinal        = (if (null inlineImports) then id else trace (show progName ++ ": extra inline imports: " ++ show (map Core.importName inlineImports))) $
                               uniquefy $ coreProgram {
                                 Core.coreProgDefs = coreDefsFinal,
                                 Core.coreProgImports = Core.coreProgImports coreProgram ++ inlineImports
                               }

        return (coreFinal, allInlineDefs)



traceDefGroups :: Flags -> String -> Core.CorePhase () ()
traceDefGroups flags title
  = do dgs <- Core.getCoreDefs
       trace (unlines (["","/* -----------------", title, "--------------- */"] ++
                map showDef (Core.flattenDefGroups dgs))) $ return ()
  where
    showDef def = show (Core.Pretty.prettyDef (penv{coreShowDef=True}) def)
    penv = prettyEnvFromFlags flags


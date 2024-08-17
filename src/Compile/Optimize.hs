-----------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Compile.Optimize( coreOptimize ) where

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
import Common.NamePrim( isPrimitiveModule, isPrimitiveName, nameCoreHnd, nameLocalVar )
import Common.Syntax
import qualified Common.NameSet as S
import Core.Pretty( prettyDef )
import Core.Check( checkCore )

import Core.CoreVar( extractDepsFromInlineDefs )
import Core.Simplify( simplifyDefs )
import Core.Uniquefy( uniquefy )
import Core.FunLift( liftFunctions )
import Core.Inline( inlineDefs  )
import Core.Inlines( Inlines, inlinesFilter, inlinesExtends, extractInlineDefs)
import Core.Monadic( monTransform )
import Core.MonadicLift( monadicLift )
import Core.Specialize( specialize, extractSpecializeDefs )
import Core.CTail( ctailOptimize )
import Core.OpenResolve( openResolve )
import Core.Unroll( unrollDefs )

import qualified Type.Pretty as TP
import Kind.Newtypes( Newtypes )
import Type.Assumption( Gamma )
import qualified Core.Core as Core
import Compile.Options
import Compile.Module( Definitions(..) )

{---------------------------------------------------------------
  compile core:
  these may need a richer gamma with
  hidden imports to check the inlined definitions.
---------------------------------------------------------------}

coreOptimize :: Flags -> Newtypes -> Gamma -> Inlines -> Core.Core -> Error () (Core.Core,[Core.InlineDef])
coreOptimize flags newtypes gamma inlines coreProgram
  = Core.runCorePhase 10000 {-unique-} $
     do Core.setCoreDefs (Core.coreProgDefs coreProgram)
        let progName = Core.coreProgName coreProgram
            penv     = prettyEnvFromFlags flags
            checkCoreDefs title = when (coreCheck flags) $ Core.Check.checkCore False False penv gamma
            vmInlineFilter = if (target flags == VM) then (\n -> n `notElem` [nameLocalVar]) else const True

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
             inlineDefs penv (2*(optInlineMax flags)) (inlinesFilter vmInlineFilter inlinesX)
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

        when (target flags == VM) $ openResolve penv gamma

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
        inlineDefs penv (2*optInlineMax flags) (inlinesFilter vmInlineFilter inlinesX) -- (loadedInlines loaded)

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
            inlineImports    = [Core.Import name "" Core.ImportCompiler Private "" | name <- inlineDeps, not (name `elem` currentImports) && not (name == progName)]

            coreFinal        = (if (null inlineImports || verbose flags <= 2) then id else trace (show progName ++ ": extra inline imports: " ++ show (map Core.importName inlineImports))) $
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
    showDef def = show (Core.Pretty.prettyDef (penv{TP.coreShowDef=True}) def)
    penv = prettyEnvFromFlags flags


module Compile.Compile( typeCheck ) where

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
import Common.Syntax
import Syntax.RangeMap
import Syntax.Syntax
import Static.FixityResolve( fixitiesCompose, fixitiesNew, fixityResolve )
import Static.BindingGroups( bindingGroups )
import Core.Pretty( prettyDef )
import Core.Check( checkCore )
import Core.CheckFBIP( checkFBIP )
import Core.Simplify( simplifyDefs )
import Core.FunLift( liftFunctions )
import Core.UnReturn( unreturn )
import Core.Borrowed
import Core.Uniquefy( uniquefy )
import Kind.Assumption( extractKGamma )
import Kind.ImportMap
import Kind.Infer( inferKinds )
import Type.Pretty
import Type.Assumption( extractGamma, extractGammaImports, gammaUnions )
import Type.Infer( inferTypes )
import qualified Core.Core as Core
import Compiler.Options
import Compile.Module( Definitions(..) )


typeCheck :: Flags -> Definitions -> UserProgram -> Error () (Core.Core,Maybe RangeMap)
typeCheck flags defs program0
  = Core.runCorePhase 0 {-unique-} $
     do -- import map
        let importMap   = importMapFromProgram program0
            coreImports = coreImportsFromProgram program0
            programName = getName program0

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
        let coreFinal
              = uniquefy $
                coreProgram { Core.coreProgImports = coreImports
                            , Core.coreProgDefs    = coreDefsFinal
                            , Core.coreProgFixDefs = coreFixities
                            }

            mbRangeMap = fmap rangeMapSort mbRangeMap1
        return (coreFinal,mbRangeMap)
  where
    penv = prettyEnvFromFlags flags

    traceDefGroups title
              = do dgs <- Core.getCoreDefs
                   trace (unlines (["","/* -----------------", title, "--------------- */"] ++
                           map showDef (Core.flattenDefGroups dgs))) $ return ()
              where
                showDef def = show (Core.Pretty.prettyDef (penv{coreShowDef=True}) def)

coreImportsFromProgram :: UserProgram -> [Core.Import]
coreImportsFromProgram program
  = [Core.Import (importFullName imp) "" (importVis imp) "" {- todo: doc -}  | imp <- programImports program]

importMapFromProgram :: UserProgram -> ImportMap
importMapFromProgram program
  = foldl' extend importsEmpty (programImports program)
  where
    extend importMap imp
      = case importsExtend (importName imp) (importFullName imp) importMap of
          Nothing -> importMap
          Just importMap' -> importMap'
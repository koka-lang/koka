------------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Main entry for an executable.
    Parses the command line arguments and drives the compiler.
-}
-----------------------------------------------------------------------------
module Main.Run( runPlain, runWithLS, runWith ) where

import Debug.Trace
import System.Exit            ( exitFailure )
import System.IO              ( hPutStrLn, stdout, stderr)
import Control.Monad          ( when, foldM )
import Data.List              ( intersperse)
import Data.Maybe

import Platform.Config
import Lib.PPrint
import Lib.Printer

import Common.ColorScheme
import Common.Failure         ( catchIO )
import Common.Error
import Common.Name
import Common.File            ( joinPath, getCwd )

import Core.Core              ( coreProgDefs, flattenDefGroups, defType, Def(..) )
import Interpreter.Interpret  ( interpret  )
import Kind.ImportMap         ( importsEmpty )
import Kind.Synonym           ( synonymsIsEmpty, ppSynonyms, synonymsFilter )
import Kind.Assumption        ( kgammaFilter, ppKGamma )
import Type.Assumption        ( ppGamma, ppGammaHidden, gammaFilter, createNameInfoX, gammaNew )
import Type.Pretty            ( ppScheme, Env(context,importsMap,colors), ppName)

import Compile.Options
import Compile.BuildContext
import qualified Platform.GetOptions

-- Main entry point for the command line compiler
runPlain :: IO ()
runPlain
  = runWith ""

runWith :: String -> IO ()
runWith args
  = runWithLSArgs plainLS ""

-- Main entry point for the language server enabled compiler
runWithLS :: (ColorPrinter -> Flags -> [FilePath] -> IO ()) -> IO ()
runWithLS ls
  = runWithLSArgs ls ""

plainLS :: ColorPrinter -> Flags -> [FilePath] -> IO ()
plainLS p flags files
  = do hPutStrLn stderr "The plain build of Koka cannot run as a language server"
       exitFailure


-- Parse the arguments given a potential language server
runWithLSArgs :: (ColorPrinter -> Flags -> [FilePath] -> IO ()) -> String -> IO ()
runWithLSArgs runLanguageServer args
  = do (flags,flags0,mode) <- getOptions args
       let with = if (not (null (redirectOutput flags)))
                   then withFileNoColorPrinter (redirectOutput flags)
                   else if (console flags == "html")
                    then withHtmlColorPrinter
                   else if (console flags == "ansi")
                    then withColorPrinter
                    else withNoColorPrinter (if (languageServerStdio flags) then stderr else stdout)
       with (mainMode runLanguageServer flags flags0 mode)
    `catchIO` \err ->
    do if ("ExitFailure" `isPrefix` err)
        then return ()
        else hPutStrLn stderr err
       exitFailure
  where
    isPrefix s t  = (s == take (length s) t)

-- The main mode determines what the compiler should be doing
mainMode :: (ColorPrinter -> Flags -> [FilePath] -> IO ()) -> Flags -> Flags -> Mode -> ColorPrinter -> IO ()
mainMode runLanguageServer flags flags0 mode p
  = case mode of
     ModeHelp
      -> showHelp flags p
     ModeVersion
      -> withNoColorPrinter stdout (\monop -> showVersion flags monop)
     ModeCompiler files
      -> do ok <- compileAll p flags files
            when (not ok) $
              do hPutStrLn stderr ("Failed to compile " ++ concat (intersperse "," files))
                 exitFailure
     ModeInteractive files
      -> interpret p flags flags0 files
     ModeLanguageServer files
      -> runLanguageServer p flags files


-- Compile (and/or link and/or evaluate) argument files
compileAll :: ColorPrinter -> Flags -> [FilePath] -> IO Bool
compileAll p flags fpaths
  = do cwd <- getCwd
       (mbRuns,_) <- -- run the build monad with a terminal and flags
                     runBuildIO (term cwd) flags False $
                       do -- build
                          (buildc0,roots) <- buildcAddRootSources fpaths (buildcEmpty flags)
                          buildc          <- buildcBuildEx (rebuild flags) roots {-force roots always-} [] buildc0
                          buildcThrowOnError buildc
                          -- compile & run entry points
                          let mainEntries = if library flags then [] else map (\rootName -> qualify rootName (newName "main")) roots
                          runs <- mapM (compileEntry buildc) mainEntries
                          -- when (evaluate flags) $ mapM_ buildLiftIO runs
                          -- show info
                          mapM_ (compileShowInfo buildc) roots
                          buildcFlushErrors buildc -- for warnings
                          return runs
       case mbRuns of
         Just runs -> do when (evaluate flags) $ sequence_ runs
                         return True
         Nothing   -> return False
  where
    -- all output should go via the terminal
    term cwd
      = Terminal (putErrorMessage p cwd (showSpan flags) cscheme)
                 (if (verbose flags > 1) then (\msg -> withColor p (colorSource cscheme) (writeLn p msg))
                                         else (\_ -> return ()))
                 (if (verbose flags > 0) then writePrettyLn p else (\_ -> return ()))
                 (writePrettyLn p)

    cscheme
      = colorSchemeFromFlags flags

    putErrorMessage p cwd endToo cscheme err
      = do writePrettyLn p (ppErrorMessage cwd endToo cscheme err)
           writeLn p ""


-- Compile and link and entry point and return an IO action to run it.
compileEntry :: BuildContext -> Name -> Build (IO ())
compileEntry buildc entry
  = do (buildc',mbTpEntry) <- buildcCompileEntry False entry buildc
       buildcThrowOnError buildc'
       case mbTpEntry of
         Just(_,Just(_,run)) -> return run
         _                   -> do addErrorMessageKind ErrBuild (\penv -> text "unable to find main entry point" <+> ppName penv entry)
                                   return (return ())


-- Show type and kind information for a given module
compileShowInfo :: BuildContext -> ModuleName -> Build ()
compileShowInfo buildc modname
  = do  flags <- buildcFlags
        -- show (kind) gamma ?
        let defs = buildcGetDefinitions [modname] buildc
        when (showKindSigs flags) $
          do buildcTermInfo $ \penv -> space <-> ppKGamma (colors penv) modname (importsMap penv) (defsKGamma defs)
             let syns = defsSynonyms defs
             when (not (synonymsIsEmpty syns)) $
               buildcTermInfo $ \penv -> space <-> ppSynonyms penv{context=modname} syns
        when (showTypeSigs flags || showHiddenTypeSigs flags) $
          buildcTermInfo $ \penv -> space <-> (if showHiddenTypeSigs flags then ppGammaHidden else ppGamma) penv{context=modname} (defsGamma defs)



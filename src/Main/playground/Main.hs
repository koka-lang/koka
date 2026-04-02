{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Main entry point for the WASM playground compiler.
    Compiles from stdin, writes JSON result to stdout.
-}
-----------------------------------------------------------------------------
module Main where

#ifdef KOKA_WASM

import Control.Monad          ( when )
import Data.IORef             ( IORef, newIORef, readIORef, modifyIORef )
import System.IO              ( hPutStr, hPutStrLn, stderr, hFlush, stdout )

import Lib.PPrint
import Lib.Printer
import Platform.Var           ( newVar, takeVar, putVar )

import Common.Name
import Common.Error
import Common.ColorScheme
import Common.Range           ( BString, stringToBString )

import Compile.Options        ( getOptions, Flags(..), Mode(..), Terminal(..) )
import Compile.BuildContext
import Compile.Build          ( virtualMount )

main :: IO ()
main = do
  (flags0, mode) <- getOptions ""
  let moduleName = case mode of
        ModeCompiler (f:_) -> f
        _                  -> "main"
  sourceText <- getContents
  result <- compileToJS flags0 moduleName sourceText
  putStrLn result
  hFlush stdout

compileToJS :: Flags -> String -> String -> IO String
compileToJS flags0 moduleName sourceText = do
  errRef <- newIORef []
  let flags = flags0{ verbose = if verbose flags0 > 0 then verbose flags0 else 1 }
      docToAnsi doc = do
        con <- newVar ansiDefault
        st  <- newVar ""
        let p = AnsiString con st
        writePrettyLn p doc
        takeVar st
      term  = Terminal (\err -> modifyIORef errRef (show err :))
                       (\msg -> hPutStrLn stderr msg >> hFlush stderr)
                       (\_ -> return ())
                       (if verbose flags > 0 then (\doc -> do s <- docToAnsi doc; hPutStr stderr s >> hFlush stderr)
                                             else (\_ -> return ()))
                       (\doc -> do s <- docToAnsi doc; hPutStr stderr s >> hFlush stderr)
      sourcePath = virtualMount ++ "/" ++ moduleName ++ ".kk"
      content    = stringToBString sourceText
  (mbResult, _) <- runBuildIO term flags False $ do
    let buildc0 = buildcEmpty flags
    withVirtualModule sourcePath content buildc0 $ \mainModName buildc1 ->
      do let [root] = buildcRoots buildc1
         let entryStr = if null (mainEntryName flags) then "main" else mainEntryName flags
         (buildc2, entryInfo) <- buildcCompileEntry False (qualify root (newName entryStr)) buildc1
         buildcThrowOnError buildc2
         return (buildc2, ())
  errs <- readIORef errRef
  case mbResult of
    Just _  -> return "{\"success\": true}"
    Nothing -> return ("{\"success\": false, \"errors\": " ++ show (reverse errs) ++ "}")

#else

import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  hPutStrLn stderr "koka-playground is only available when built with the GHC WASM backend."
  hPutStrLn stderr "Build with: wasm32-wasi-cabal build koka:exe:koka-playground"

#endif

------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Main module.
-}
-----------------------------------------------------------------------------
module Main where

import System.Exit            ( exitFailure )
import Control.Monad          ( when )

import Platform.Config
import Lib.PPrint             ( Pretty(pretty), writePrettyLn )
import Lib.Printer
import Common.ColorScheme
import Common.Failure         ( catchIO )
import Common.Error           
import Common.Name
import Common.File            ( joinPath )
import Compiler.Options       
import Compiler.Compile       ( compileFile, CompileTarget(..), Module(..), Loaded(..), Terminal(..) )
import Interpreter.Interpret  ( interpret  )
import Kind.ImportMap         ( importsEmpty )
import Kind.Synonym           ( synonymsIsEmpty, ppSynonyms, synonymsFilter )
import Kind.Assumption        ( kgammaFilter )
import Type.Assumption        ( ppGamma, gammaFilter )
import Type.Pretty            ( ppScheme, Env(context,importsMap) )



-- compiled entry
main      = mainArgs ""

-- ghci entry
maing     = maingg "" 
maindoc   = maingg "--html"
mainjs    = maingg "--target=js"  
maincs    = maingg "--target=cs"  

maingg extraOptions
  = mainArgs ("-ilib -itest --verbose " ++ extraOptions)

-- hugs entry
mainh     = mainArgs "-ilib -itest --console=raw"


mainArgs args
  = do (flags,mode) <- getOptions args
       let with = if (not (null (redirectOutput flags)))
                   then withFileNoColorPrinter (redirectOutput flags)
                   else if (console flags == "html")
                    then withHtmlColorPrinter
                   else if (console flags == "ansi") 
                    then withColorPrinter 
                    else withNoColorPrinter  
       with (mainMode flags mode)
    `catchIO` \err ->
    do if ("ExitFailure" `isPrefix` err)
        then return ()
        else putStr err
       exitFailure
  where
    isPrefix s t  = (s == take (length s) t)

mainMode :: Flags -> Mode -> ColorPrinter -> IO ()
mainMode flags mode p
  = case mode of
     ModeHelp       
      -> showHelp flags p 
     ModeVersion
      -> showVersion p
     ModeCompiler files 
      -> mapM_ (compile p flags) files
     ModeInteractive files 
      -> interpret p flags files 


compile :: ColorPrinter -> Flags -> FilePath -> IO ()
compile p flags fname
  = do let exec = Executable (newName "main") ()
       err <- compileFile term flags [] 
                (if (not (evaluate flags)) then (if library flags then Library else exec) else exec) fname
       case checkError err of
         Left msg 
           -> do putPrettyLn p (ppErrorMessage (showSpan flags) cscheme msg)
                 exitFailure

         Right (Loaded gamma kgamma synonyms newtypes constructors _ imports _
                (Module modName _ _ _ _ _warnings rawProgram core _ modTime) _ 
               , warnings)
           -> do when (not (null warnings))
                   (let msg = ErrorWarning warnings ErrorZero
                    in putPrettyLn p (ppErrorMessage (showSpan flags) cscheme msg))
                 when (showKindSigs flags)
                   (do putPrettyLn p (pretty (kgammaFilter modName kgamma))
                       let localSyns = synonymsFilter modName synonyms
                       when (not (synonymsIsEmpty localSyns)) 
                        (putPrettyLn p (ppSynonyms (prettyEnv flags modName imports) localSyns))
                       )
                 when (showTypeSigs flags)
                   (do putPrettyLn p (ppGamma (prettyEnv flags modName imports) (gammaFilter modName gamma)))
                 {-
                 when (showCore flags)
                   (do putPrettyLn p (prettyCore (prettyEnv flags modName imports) core)) 
                 -}
                  
  where
    term
      = Terminal (putErrorMessage p (showSpan flags) cscheme) 
                 (if (verbose flags > 1) then putStrLn else (\_ -> return ()))  
                 (if (verbose flags > 0) then writePrettyLn p else (\_ -> return ()))
                 (putScheme p (prettyEnv flags nameNil importsEmpty)) 
                 (writePrettyLn p)

    cscheme
      = colorSchemeFromFlags flags

    prettyEnv flags ctx imports
      = (prettyEnvFromFlags flags){ context = ctx, importsMap = imports }


putScheme p env tp
  = putPrettyLn p (ppScheme env tp)

putErrorMessage p endToo cscheme err
  = putPrettyLn p (ppErrorMessage endToo cscheme err)
       
putPhase p cscheme msg
  = withColor p (colorInterpreter cscheme) (writeLn p msg)

putPrettyLn p doc
  = do writePrettyLn p doc
       writeLn p ""


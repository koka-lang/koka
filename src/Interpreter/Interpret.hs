------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Interpreter
-}
-----------------------------------------------------------------------------
module Interpreter.Interpret( interpret ) where

import Lib.Trace

import Platform.Filetime
import System.Directory            ( getCurrentDirectory, setCurrentDirectory )
import Data.List                   ( isPrefixOf )
import Data.Char                   ( isSpace, toLower )
import Control.Monad
import Control.Applicative

import Platform.Config hiding (programName)
import qualified Platform.Config as Config
import Platform.ReadLine      ( withReadLine, readLineEx, addHistory )
import Lib.PPrint
import Lib.Printer
import Common.Failure         ( raiseIO, catchIO )
import Common.ColorScheme
import Common.File            ( notext, joinPath, searchPaths, runSystem, isPathSep, startsWith, getCwd )
import Common.Name            ( Name, ModuleName, unqualify, qualify, newName, newQualified, nameNil )
import Common.NamePrim        ( nameExpr, nameType, nameInteractiveModule, nameSystemCore )
import Common.Range
import Common.Error

import Common.Syntax
import Syntax.Syntax

import Syntax.Highlight       ( highlightPrint )
import Kind.ImportMap
import Kind.Synonym           ( synonymsIsEmpty,synonymsDiff, ppSynonyms )
import Kind.Assumption        ( kgammaFind, kgammaIsEmpty, ppKGamma )
import Kind.Pretty            ( prettyKind )
import Type.Type              ( Scheme, Type, typeVoid )
import Type.Pretty            ( ppScheme, ppSchemeEffect, Env(context,importsMap))
import Type.Assumption        ( gammaIsEmpty, ppGamma, infoType, gammaFilter )

import Compile.Options
import Interpreter.Command
import qualified Compile.BuildContext   as B


{---------------------------------------------------------------
  interpreter state
---------------------------------------------------------------}
data State = State{  printer    :: !ColorPrinter
                   -- system variables
                   , flags      :: !Flags                -- processed flags
                   , flags0     :: !Flags                -- unprocessed flags
                   , evalDisable   :: !Bool
                   -- program state
                   , defines       :: ![(Name,[String])] -- interactive definitions
                   , errorRange    :: !(Maybe Range)       -- last error location
                   , lastLoad      :: ![FilePath]        -- last load command
                   , buildContext  :: !B.BuildContext
                   }


{---------------------------------------------------------------
  Main
---------------------------------------------------------------}
interpret ::  ColorPrinter -> Flags -> Flags -> [FilePath] -> IO ()
interpret printer flags0 flagspre files
  = withReadLine (buildDir flags0) $
    do{ let st0 = (State printer flags0 flagspre False [] Nothing [] (B.buildcEmpty flags0))
      ; messageHeader st0
      ; let st2 = st0

      ; (mbSt,erng) <- loadModulesEx (terminal st2) st2{ flags = flags0{ showCore = False }} [show (nameSystemCore)] False False
      ; case mbSt of
          Nothing     -> do messageInfoLn st2 ("unable to load the " ++ show nameSystemCore ++ " module; standard functions are not available")
                            messageEvaluation st2
                            interpreterEx st2{ flags      = (flags st2){ evaluate = False }
                                             , errorRange = erng }
          Just coreSt -> if (null files)
                             then interpreterEx coreSt{ lastLoad = [show nameSystemCore] }
                             else command coreSt (Load files False)

      }

messageEvaluation st
  = messageInfoLnLn st "evaluation is disabled"

{---------------------------------------------------------------
  Interpreter loop
---------------------------------------------------------------}
interpreter ::  State -> IO ()
interpreter st
  = interpreterEx st{ errorRange = Nothing }

interpreterEx ::  State -> IO ()
interpreterEx st
  = do flush (printer st)
       cmd <- getCommand st
       -- ; messageLn ""
       command st cmd


{---------------------------------------------------------------
  Interprete a command
---------------------------------------------------------------}
command ::  State -> Command -> IO ()
command st cmd
  = let term = terminal st
    in do{ case cmd of
  Eval line   -> do st' <- buildRunExpr term st line
                    interpreterEx st'

  Load fnames forceAll
              -> do let st' = st{ lastLoad = fnames }
                    st'' <- loadModules term st' fnames forceAll True
                    interpreterEx st''

  Reload      -> do st' <- loadModules term st (lastLoad st) False True
                    interpreterEx st'


  TypeOf line -> do (mbType,st') <- buildTypeExpr term st line
                    case mbType of
                      Just tp -> messageSchemeEffect st' tp
                      _       -> return ()
                    interpreterEx st'

{-
  Define line -> do err <- compileValueDef term (flags st) (loaded st) (program st) (lineNo st) line
                    checkInfer2Snd st True err $ \(defName,ld) ->
                       do{ let tp    = infoType $ gammaFind defName (loadedGamma ld)
                               tpdoc = prettyScheme st tp
                               sig   = show defName ++ " :: " ++ show tpdoc
                         ; messagePrettyLnLn st (text (show (unqualify defName)) <+> text ":" <+> tpdoc)
                         ; interpreter st{ program  = maybe (program st) id (modProgram (loadedModule ld))
                                         , loaded   = ld
                                         , defines  = filter (\(name,_) -> defName /= name) (defines st)
                                                      ++ [(defName,[dropLet line,""])]
                                         }
                         }

  TypeOf line -> do err <- compileExpression (const Nothing) term (flags st) (loaded st) Object (program st) bigLine line
                    checkInfer2Fst st True err $ \(ld, _) ->
                       do{ let tp = infoType $ gammaFind (qualify nameInteractiveModule nameExpr) (loadedGamma ld)
                         ; messageSchemeEffect st tp
                         ; interpreter st{ loaded = ld } -- (loaded st){ loadedModules  = loadedModules ld }}
                         }

  KindOf line -> do err <- compileType term (flags st) (loaded st) (program st) bigLine line
                    checkInfer st True err $ \ld ->
                       do{ let (_,kind,_) = kgammaFind (getName (program st)) nameType (loadedKGamma ld)
                         ; messagePrettyLnLn st (prettyKind (colorSchemeFromFlags (flags st)) kind)
                         ; interpreter st{ loaded = ld }
                         }

  TypeDef line-> -- trace ("modules: " ++ show (map (show . modName . loadedModule) (loadedModules st))) $
                 do err <- compileTypeDef term (flags st) (loaded st) (program st) (lineNo st) line
                    checkInfer2Snd st True err $ \(defName, ld) ->
                     do{ let (qname,kind,doc) = kgammaFind (getName (program st)) defName (loadedKGamma ld)
                       ; messagePrettyLnLn st (text (show defName) <+> text "::" <+> pretty kind)
                       ; interpreter st{ program  = maybe (program st) id $ modProgram (loadedModule ld)
                                       , loaded   = ld
                                       , defines  = filter (\(name,_) -> defName /= name) (defines st)
                                                    ++ [(defName,[line,""])]
                                       }
                       }
-}

  Edit []     -> do{ let fpath = lastFilePath st
                   ; if (null fpath)
                      then do remark st "nothing to edit"
                              interpreterEx st
                      else do runEditor st fpath
                              -- command st Reload
                              interpreter st
                   }
  Edit fname  -> do{ mbpath <- B.runBuildMaybe (terminal st) (flags st) $ B.buildcSearchSourceFile fname (buildContext st)
                   ; case mbpath of
                      Nothing
                        -> do messageErrorMsgLnLn st [errorFileNotFound (flags st) fname]
                              interpreter st
                      Just (root,fname)
                        -> do runEditor st (joinPath root fname)
                              -- command st Reload
                              interpreter st
                   }

  Shell cmd   -> do{ runSystem cmd
                   ; messageLn st ""
                   ; interpreterEx st
                   }

  ChangeDir d -> do{ if (null d)
                      then do{ fpath <- getCurrentDirectory
                             ; messageInfoLnLn st fpath
                             }
                      else setCurrentDirectory d
                   ; interpreterEx st
                   }

  Options opts-> do{ (newFlags,newFlags0,mode) <- processOptions (flags0 st) (words opts)
                   ; let setFlags files
                          = do if (null files)
                                 then messageLn st ""
                                 else messageError st "(ignoring file arguments)"
                               (mbBuildc,_) <- B.runBuildIO (terminal st) newFlags (B.buildcValidate False [] (buildContext st))
                               interpreter (st{ flags = newFlags, flags0 = newFlags0, buildContext = maybe (buildContext st) id mbBuildc })
                   ; case mode of
                       ModeHelp     -> do doc <- commandLineHelp (flags st)
                                          messagePrettyLn st doc
                                          interpreterEx st
                       ModeVersion  -> do showVersion (flags st) (printer st)
                                          messageLn st ""
                                          interpreter st
                       ModeCompiler files     -> setFlags files
                       ModeInteractive files  -> setFlags files
                       -- ModeDoc files          -> setFlags files
                   }

  Error err   -> do{ messageInfoLn st err
                   ; messageInfoLn st "invalid command."
                   ; messageInfoLnLn st "(type \":?\" for help on commands)"
                   ; interpreterEx st
                   }

  Show showcmd-> do{ showCommand st showcmd
                   ; interpreterEx st
                   }

  Quit        -> do{ putQuote st
                   }

  None        -> do{ interpreterEx st }
      }
   `catchIO` \msg ->
      do{ messageError st msg
        ; interpreter st
        }


-- todo: set error range
loadModules :: Terminal -> State -> [FilePath] -> Bool -> Bool -> IO State
loadModules term st files forceAll forceRoots
  = do (mbSt,erng) <- loadModulesEx term st files forceAll forceRoots
       let st' = case mbSt of
                  Just st' -> st'
                  Nothing  -> st
       return (st'{ errorRange = erng <|> errorRange st'})

loadModulesEx :: Terminal -> State -> [FilePath] -> Bool -> Bool -> IO (Maybe State,Maybe Range)
loadModulesEx term st files forceAll forceRoots
  = do (mbBuildc,erng) <- B.runBuildIO term (flags st) $ B.buildcLiftErrors id $
                          do (buildc1,rootNames) <- B.buildcAddRootSources files (B.buildcClearRoots (buildContext st))
                             B.buildcBuildEx (forceAll || rebuild (flags st))
                                             (if forceRoots then rootNames else [])
                                                [] -- [newQualified "samples/basic/caesar" "main"]
                                                buildc1

       case mbBuildc of
         Nothing     -> return (Nothing,erng)
         Just buildc -> return (Just st{ buildContext = buildc }, erng)

buildRunExpr :: Terminal -> State -> String -> IO State
buildRunExpr term st expr
  = do (mbBuildc,erng) <- B.runBuildIO term (flags st) $ B.buildcLiftErrors id $
                          do B.buildcRunExpr [] expr (buildContext st)
       case mbBuildc of
         Nothing     -> return st{ errorRange = erng <|> errorRange st }
         Just buildc -> return st{ buildContext = buildc, errorRange = erng <|> errorRange st  }


buildTypeExpr :: Terminal -> State -> String -> IO (Maybe Type,State)
buildTypeExpr term st expr
  = do (mbBuildc,erng) <- B.runBuildIO term (flags st) $ B.buildcLiftErrors fst $
                          do B.buildcCompileExpr False True [] expr (buildContext st)
       case mbBuildc of
         Nothing     -> return (Nothing, st{ errorRange = erng <|> errorRange st })
         Just (buildc,mbEntry) -> let st' = st{ buildContext = buildc, errorRange = erng <|> errorRange st }
                                  in case mbEntry of
                                      Just (tp,_) -> return (Just tp,st')
                                      Nothing     -> return (Nothing,st')



findPath :: ColorScheme -> [FilePath] -> String -> String -> IO FilePath
findPath cscheme path ext name
  = do mbfpath <- searchPaths path [ext] name
       case mbfpath of
         Just fpath -> return fpath
         Nothing    -> raiseIO (show (docNotFound cscheme path name))


errorFileNotFound :: Flags -> FilePath -> ErrorMessage
errorFileNotFound flags name
  = errorMessageKind ErrBuild rangeNull (docNotFound (colorSchemeFromFlags flags) (includePath flags) name)

docNotFound cscheme path name
  = text "could not find:" <+> ppPath name <->
    if (null path)
     then text ("search path empty. (use the \"-i\" flag at command line?)")
     else text "search path   :" <+> align (cat (punctuate comma (map ppPath path)))
  where
    ppPath p
      = color (colorSource cscheme) (text p)

{--------------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------------}

maybeMessageMarker ::  State -> Range -> IO ()
maybeMessageMarker st rng
  = if (lineNo st == posLine (rangeStart rng) || posLine (rangeStart rng) == bigLine)
     then messageMarker st rng
     else return ()

lineNo st
  = bigLine + (length (defines st) + 1)

dropLet s
  = if isPrefixOf "let" s
     then dropEndWhile (\c -> isSpace c || c == '}') (dropWhile (\c -> isSpace c || c == '{') (drop 3 s))
     else s
  where
    dropEndWhile p
      = reverse . dropWhile p . reverse

messageScheme ::  State -> Scheme -> IO ()
messageScheme st tp
  = do messagePrettyLnLn st (prettyScheme st tp)

prettyScheme st tp
  = ppScheme (prettyEnv st) tp

messageSchemeEffect ::  State -> Scheme -> IO ()
messageSchemeEffect st tp
  = do messagePrettyLnLn st (prettySchemeEffect st tp)

prettySchemeEffect st tp
  = ppSchemeEffect (prettyEnv st) tp

lastFilePath st
   = case reverse (lastLoad st) of
       (fname:_) -> fname
       _         -> ""

lastSourceFull st
  = let fpath = lastFilePath st
    in if (null fpath)
        then return sourceNull
        else do text <- readInput fpath
                          `catchIO` (\msg -> do{ messageError st msg; return bstringEmpty })
                return (Source fpath text)


isSourceNull source
  = null (sourceName source)

{---------------------------------------------------------------
  Interprete a show command
---------------------------------------------------------------}

prettyEnv st
  = (prettyEnvFromFlags (flags st)) -- { context = loadedName (loaded st), importsMap = loadedImportMap (loaded st) }


mainModuleName :: State -> ModuleName
mainModuleName st
  = case B.buildcRoots (buildContext st) of
      (mname:_) -> mname
      _         -> nameNil

getImportMap :: State -> ImportMap
getImportMap st
  = []

showCommand ::  State -> ShowCommand -> IO ()
showCommand st cmd
  = case cmd of
      ShowHelp         -> do messagePrettyLn st (commandHelp (colorSchemeFromFlags (flags st)))
                             showEnv (flags st) (printer st)

      ShowVersion      -> do showVersion (flags st) (printer st)
                             messageLn st ""

      ShowKindSigs     -> let kgamma = B.defsKGamma (B.buildcGetDefinitions [] (buildContext st))
                          in if (kgammaIsEmpty kgamma)
                           then remark st "no kinds to show"
                           else messagePrettyLnLn st (ppKGamma colors (mainModuleName st) (getImportMap st) kgamma)

      ShowTypeSigs     -> let gamma = B.defsGamma (B.buildcGetDefinitions [] (buildContext st))
                          in if (gammaIsEmpty gamma)
                           then remark st "no types to show"
                           else messagePrettyLnLn st (ppGamma (prettyEnv st) gamma)

      ShowSynonyms     -> let syns = B.defsSynonyms (B.buildcGetDefinitions [] (buildContext st))
                          in if (synonymsIsEmpty syns)
                           then remark st "no synonyms to show"
                           else messagePrettyLnLn st
                                  (ppSynonyms (prettyEnv st) syns)

      ShowSource       -> do source <- lastSourceFull st
                             if (isSourceNull source)
                              then remark st "no source code to show"
                              else do syntaxColor source
                                      messageLnLn st ""
                                      -- messageLnLn st (sourceText (programSource (program st)))

      ShowDefines      -> if (null (defines st))
                           then remark st "no definitions to show"
                           else syntaxColor (interactiveSource (stringToBString (unlines (concatMap snd (defines st)))))
                                -- messagePrettyLn st (color (colorSource colors)
                                --                     (vcat (concatMap (map string . snd) (defines st))))


  where
    colors
      = colorSchemeFromFlags (flags st)

    tab doc
      = fill 12 doc

    mod doc
      = color (colorInterpreter colors) doc

    syntaxColor source
      = do highlightPrint colors (sourceName source) 1 (sourceBString source) (printer st)



interactiveSource str
  = Source (show (nameInteractiveModule)) str

{--------------------------------------------------------------------------
  Run an editor
--------------------------------------------------------------------------}
runEditor ::  State -> FilePath -> IO ()
runEditor st fpath
  = let (line,col) = case errorRange st of
                       Just rng | sourceName (rangeSource rng) == fpath
                         -> let pos = rangeStart rng in (posLine pos, posColumn pos)
                       _ -> (1,1)
    in runEditorAt st fpath line col

runEditorAt ::  State -> FilePath -> Int -> Int -> IO ()
runEditorAt st fpath line col
  = let command  = replace line col (editor (flags st)) fpath
    in if null (editor (flags st))
        then raiseIO ("no editor specified. (use the \"koka_editor\" environment variable?)")
        else do -- messageInfoLn st ("command: " ++ command)
                runSystem command

replace :: Int -> Int -> FilePath -> String -> String
replace line col s fpath
  = walk True s
  where
    qfpath  = fpath
    walk add s
      = let (pre,post) = span (/='%') s
        in case post of
             ('%':'c':cs) -> pre ++ show col ++ walk add cs
             ('%':'l':cs) -> pre ++ show line ++ walk add cs
             ('%':'f':cs) -> pre ++ qfpath ++ walk False cs
             (c:cs)       -> pre ++ [c] ++ walk add cs
             []           -> pre ++ (if add then " " ++ qfpath else "")


{--------------------------------------------------------------------------
  Messages
--------------------------------------------------------------------------}
getCommand :: State -> IO Command
getCommand st
  = do messageLn st ""
       let cscheme = colorSchemeFromFlags (flags st)
           ansiPrompt = if isConsolePrinter (printer st) -- || osName == "macos"
                          then ""
                          else if isAnsiPrinter (printer st)
                            then let c = ansiColor (colorInterpreter cscheme)
                                 in ("\x1B[" ++ show c ++ "m\x02> \x1B[0m\x02")  -- readline needs "STX" ("\x02") ending of escape sequence
                                    -- ("\x1B[" ++ show c ++ "m> \x1B[0m")  -- readline needs "STX" ("\x02") ending of escape sequence
                                    -- ansiWithColor (colorInterpreter (colorSchemeFromFlags (flags st))) "> "
                                    -- "> "
                            else "> "

       mbInput <- readLineEx cscheme (includePath (flags st)) (B.buildcGetMatchNames [] (buildContext st))
                                     (optionCompletions) ansiPrompt (prompt st)
       let input = maybe ":quit" id mbInput
       -- messageInfoLn st ("cmd: " ++ show input)
       let cmd   = readCommand input
       case cmd of
         -- Quit     -> return ()
         Error _  -> return ()
         _        | null input -> return ()
                  | otherwise  -> addHistory input
       return cmd


prompt ::  State -> IO ()
prompt st
  = do messageInfo st "> "
       flush (printer st)

remark ::  State -> String -> IO ()
remark st s
  = messageInfoLnLn st ("<" ++ s ++ ">")

terminal :: State -> Terminal
terminal st
  = Terminal (\err -> messageErrorMsgLn st [err])
             (if (verbose (flags st) > 2)
               then (\s -> withColor (printer st) (colorSource (colorSchemeFromFlags (flags st))) (message st (s ++ "\n"))) else (\_ -> return ()))
             (messagePrettyLn st)  -- (\_ -> return ()) --
             (messagePrettyLn st)

messageErrorMsgLn :: State -> [ErrorMessage] -> IO ()
messageErrorMsgLn st errs
  = do cwd <- getCwd
       messagePrettyLn st (vcat (map (ppErrorMessage cwd (showSpan (flags st)) (colorSchemeFromFlags (flags st))) errs))

messageErrorMsgLnLn :: State -> [ErrorMessage] -> IO ()
messageErrorMsgLnLn st errs
  = do cwd <- getCwd
       messagePrettyLnLn st (vcat (map (ppErrorMessage cwd (showSpan (flags st)) (colorSchemeFromFlags (flags st))) errs))

messageError ::  State -> String -> IO ()
messageError st msg
  = messageInfoLnLn st msg

messageInfoLnLn ::  State -> String -> IO ()
messageInfoLnLn st s
  = do messageInfoLn st s
       messageLn st ""

messageInfoLn ::  State -> String -> IO ()
messageInfoLn st s
  = do messageInfo st s
       messageLn st ""

messageInfo ::  State -> String -> IO ()
messageInfo st s
  = withColor (printer st) (colorInterpreter colors) (message st s)
  where
    colors
      = colorSchemeFromFlags (flags st)

messagePrettyLnLn ::  State -> Doc -> IO ()
messagePrettyLnLn st d
  = do messagePrettyLn st d
       messageLn st ""

messagePrettyLn ::  State -> Doc -> IO ()
messagePrettyLn st d
  = do messagePretty st d
       messageLn st ""

messagePretty ::  State -> Doc -> IO ()
messagePretty st d
  = writePretty (printer st) d

messageLnLn ::  State -> String -> IO ()
messageLnLn st s
  = messageLn st (s ++ "\n")

messageLn ::  State -> String -> IO ()
messageLn st s
  = writeLn (printer st) s

message ::  State -> String -> IO ()
message st s
  = write (printer st) s

messageMarker ::  State -> Range -> IO ()
messageMarker st rng
  = messagePrettyLn st (makeMarker rng)
  where
    colors
      = colorSchemeFromFlags (flags st)

    makeMarker rng
      = let c1 = posColumn (rangeStart rng)
            c2 = if (posLine (rangeStart rng) == posLine (rangeStart rng))
                  then posColumn (rangeEnd rng)
                  else c1
        in color (colorMarker colors) (text (replicate (c1 + 1) ' ' ++ replicate 1 {- (c2 - c1 + 1) -} '^'))

{--------------------------------------------------------------------------
  Header
--------------------------------------------------------------------------}
messageHeader st
  = messagePrettyLnLn st header
  where
    colors = colorSchemeFromFlags (flags st)
    header = color(colorInterpreter colors) $ vcat [
        text " _         _ "
       ,text "| |       | |"
       ,text "| | _ ___ | | _ __ _   "  <.> welcome
       ,text "| |/ / _ \\| |/ / _' |  " <.> headerVersion
       ,text "|   ( (_) |   ( (_| |  "  <.> text "output dir:" <+> color (colorSource colors) (text (fullBuildDir (flags st)))
       ,text "|_|\\_\\___/|_|\\_\\__,_|  "  <.> color (colorSource colors) (text "type :? for help, and :q to quit")
       {-
       ,text " _         _ "
       ,text "| |       | |"
       ,text "| | _ ___ | | _ __ _"
       ,text "| |/ / _ \\| |/ / _' |  " <.> welcome
       ,text "|   < (_) |   < (_| |  "  <.> headerVersion
       ,text "|_|\\_\\___/|_|\\_\\__,_|  "  <.> color (colorSource colors) (text "type :? for help, and :q to quit")
       ,text " _          _ "
       ,text "| |        | |"
       ,text "| | __ ___ | | __ __ _"     <.> welcome
       ,text "| |/ // _ \\| |/ // _` |  " <.> headerVersion
       ,text "|   <| (_) |   <| (_| |  "  <.> (color (colorSource colors) (text "output dir:")) <+> text (fullBuildDir (flags st))
       ,text "|_|\\_\\\\___/|_|\\_\\\\__,_|  "  <.> color (colorSource colors) (text "type :? for help, and :q to quit")
       -}
       ]
    headerVersion = text $ "version " ++ version ++
                           (if compilerBuildVariant /= "release" then (" (" ++ compilerBuildVariant ++ ")") else "") ++ ", "
                           ++ buildDate ++ targetMsg
    welcome       = text ("welcome to the " ++ Config.programName ++ " interactive compiler")
    tgt = target (flags st)
    targetMsg
      = case tgt of
          C _ | isTargetWasm tgt
                -> ", " ++ show tgt
                    ++ show (8*sizePtr (platform (flags st)))
                    ++ " (" ++ (ccName (ccomp (flags st))) ++ ")"
          C _   -> ", " ++ show tgt
                    ++ " " ++ cpuArch -- show (8*sizePtr (platform (flags st))) ++ "-bit"
                    ++ " (" ++ (ccName (ccomp (flags st))) ++ ")"
          JS _  -> ", " ++ show tgt
          CS    -> ", .net"
          _     -> ""

semiRandom min max
  = do t <- getCurrentTime
       let i = fileTimeToPicoseconds t `div` 100000000000
       return (fromInteger (min + (i `mod` (max - min))))

putQuote ::  State -> IO ()
putQuote st
  = do idx <- semiRandom 0 (toInteger (length quotes-1))
       let (quote,author) = quotes!!idx
       messageInfoLnLn st ("\n" ++ quote ++ "\n -- " ++ author)

quotes :: [(String,String)]
quotes
  = [("The cause is hidden. The effect is visible to all.","Ovid (43 BC - 17 AD)")
    ,("I think of my body as a side effect of my mind.", "Carrie Fisher (1956)")
    ,("Laughter is a tranquilizer with no side effects.", "Arnold H. Glasgow")
    ,("To the extent this helps us in any sort of competition, that's great,\nbut that's actually a side effect. It's a happy side effect, nonetheless.", "Gary Flake")
    ,("There are no side effects -- only effects.\nThose we thought of in advance, the ones we like, we call\nthe main, or intended, effects, and take credit for them.\nThe ones we didn't anticipate, the ones that came around\nand bit us in the rear -- those are the `side effects'", "John D. Sterman (2002)")
    ,("Many people recognize that technology often comes\nwith unintended and undesirable side effects.","Leon Kass")
    ,("Before the effect one believes in different causes than one does after the effect.","Friedrich Nietzsche")
    ,("The cause ceasing, the effect ceases also.","Edward Coke")
    ,("Logic can often be reversed, but the effect does not precede the cause.","Gregory Bateson")
    -- ,("Most exciting ideas are not important.\nMost important ideas are not exciting.\nNot every problem has a solution.\nEvery solution has side effects.", "Daniel E. Geer Jr.")
    ,("Every cause produces more than one effect.","Herbert Spencer")
    ,("No action is without its side effects.","Barry Commoner")
    ]

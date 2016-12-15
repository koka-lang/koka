{-# OPTIONS -fspec-constr-count=0 #-}
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
module Interpreter.Command
              ( Command(..), ShowCommand(..)
              , readCommand
              , commandHelp
              ) where

import Data.Char         ( isSpace, toLower )
import Data.List         ( isPrefixOf )
import Lib.PPrint   ( Doc, text,vcat,(<->),hang,empty,fill,(<>),color)
import Common.ColorScheme
import Common.Name  ( Name, newName )
-- import Syntax.Lexer ( idchars )
-- import Compiler.Options( Flags, colorSchemeFromFlags, showIncludeInfo)
import Text.Parsec hiding (Error)

type Parser a = Parsec String () a

{--------------------------------------------------------------------------
  Interpreter Command parsing
--------------------------------------------------------------------------}
-- | A commmand line command.
data Command  = Quit
              | Error    String
              | Load     [FilePath] Bool
              | Reload 
              | Eval     String
              | TypeOf   String
              | KindOf   String
              | Define   String
              | TypeDef  String
              | Options  String
              | Edit     FilePath
              | Shell    String
              | ChangeDir FilePath
              | Show     ShowCommand
              | None
              
-- | A /show/ command -- @?@.
data ShowCommand
              = ShowSource
              | ShowTypeSigs
              | ShowKindSigs
              | ShowSynonyms
              | ShowDefines
              | ShowHelp
              | ShowVersion

{--------------------------------------------------------------------------
  Read a command
--------------------------------------------------------------------------}
readCommand :: String -> Command
readCommand line
  = parseCommand (edit [] line)


{---------------------------------------------------------------
  Edit the command line: remove backspaces
---------------------------------------------------------------}
edit xs []
  = reverse xs

edit xs (c:cs)
  = case c of
      '\b'   | not (null xs) -> edit (tail xs) cs
             | otherwise     -> edit xs cs
      other  -> edit (c:xs) cs

{--------------------------------------------------------------------------
  Parse a command line into a command, raises an exception on failure.
--------------------------------------------------------------------------}
-- | Parse a command line into a command, raises an exception on failure.
parseCommand :: String -> Command
parseCommand input
  = case parse (wrap command) "" input of
      Left err  -> Error ("error in command: " ++ show err)
      Right cmd -> cmd

command :: Parser Command
command 
  =   do{ special ":"; cmdeval }
  <|> do{ expression }
  <|> return None
  <?> "command"


cmdeval
  =   do{ symbol "l" <|> symbol "load"; fpaths <- filenames; return (Load fpaths False) }
  <|> do{ symbol "f" <|> symbol "fload"; fpaths <- filenames; return (Load fpaths True) }
  <|> do{ symbol "r" <|> symbol "reload"; return Reload }
  <|> do{ symbol "q" <|> symbol "quit"; return Quit }
  -- external
  <|> do{ symbol "e" <|> symbol "edit"; fpath <- option "" filename; return (Edit fpath) }
  <|> do{ symbol "cd"; fpath <- option "" filename; return (ChangeDir fpath) }
  <|> do{ special "!"; cmd <- shellCommand; return (Shell cmd) }
  -- help
  <|> do{ special "?" <|> symbol "h" <|> symbol "help"; return (Show ShowHelp) }
  -- complex
  <|> do{ symbol "t" <|> symbol "type" <|> symbol "b" <|> symbol "browse"; 
        (do p <- getPosition; x <- expr; return (TypeOf (replicate (sourceColumn p-1) ' ' ++ x))
         <|>
         return (Show ShowTypeSigs))}
  <|> do{ symbol "set"; opts <- commandLine; return (Options opts) }
  <|> do{ symbol "s" <|> symbol "source"
        ; return (Show (ShowSource))}
  <|> do{ symbol "k" <|> symbol "kind";   
          (do p <- getPosition; x <- expr; return (KindOf (replicate (sourceColumn p-1) ' ' ++ x))
           <|>
           return (Show ShowKindSigs))}
  <|> do{ symbol "d" <|> symbol "defines"
        ; return (Show ShowDefines) }
  <|> do{ symbol "alias"; return (Show ShowSynonyms) }
  <|> do{ symbol "w" <|> symbol "warranty" <|> symbol "version"; return (Show ShowVersion) }
  <?> "command"

commandHelp :: ColorScheme -> Doc
commandHelp colors
  = hang 2 (infotext "commands:" <-> vcat 
    [cmd "<expression>" ""          "evaluate the given expression"
    ,cmd "val"      "<definition>"  "add a value definition"
    ,cmd "fun"      "<definition>"  "add a function definition"
    ,cmd "type"     "<definition>"  "add a new type definition"
    ,cmd "alias"    "<definition>"  "add a type synonym definition"
    ,empty
    ,cmd ":l[oad]"  "{modulename}"  "load module(s)"
    ,cmd ":r[eload]" ""             "reload the current module(s)"
--    ,cmd ":f[ind]" "<identifier>"   "edit file containing the identifier"
    ,cmd ":e[dit]" "[filename]"     "edit file (and jump to error location)"   
    ,cmd ":set"    "<options>"      "set (command line) options"
    ,empty
    ,cmd ":t[ype]" "[expression]"   "show type signature(s) (of a given expression)"
    ,cmd ":k[ind]" "[type]"         "show kind signature(s) (of a given type)"
    ,cmd ":s[ource]" ""             "show the source code of the current module"
    ,cmd ":d[efines]" "[identifier]" "show interactively defined values"
    ,cmd ":alias"   ""            "show type alias signatures"
    ,cmd ":version"  ""             "show version and warranty information"
    ,cmd ":cd"     ""               "show the current directory"
    ,cmd ":cd"     "<directory>"    "change the current directory"
    ,cmd ":!"      "<command>"      "run a shell command"
    ,cmd ":?"      ""               "show this information"
    ,cmd ":q[uit]"  ""              "quit the interpreter"
    ,empty
    ]) <->
    hang 2 (infotext "remarks:" <-> vcat
    [text "The type command can also be cotype, rectype, or struct."
    ,text "Use :set -? to see help on command line flags."
    ]) 
  where
    cmd c arg explain
      = fill 12 (text c) <> fill 14 (text arg) <> infotext explain
    
    infotext s
      = color (colorInterpreter colors) (text s)


expression :: Parser Command
expression
  = do src <- expr
       if (isPrefixOf "fun" src)
         then return (Define src)
        else if (isPrefixOf "val" src)
         then return (Define src)
        else if (isPrefixOf "type" src || isPrefixOf "open type" src || isPrefixOf "extend type" src)
         then return (TypeDef src)
        else if (isPrefixOf "cotype" src)
         then return (TypeDef src)
        else if (isPrefixOf "rectype" src)
         then return (TypeDef src)
        else if (isPrefixOf "alias" src)
         then return (TypeDef src)
        else if (isPrefixOf "struct" src)
         then return (TypeDef src)
        else if (isPrefixOf "enum" src)
         then return (TypeDef src)      
         else return (Eval src)

expr :: Parser String
expr
  = anything <?> "expression"

shellCommand :: Parser String
shellCommand
  = anything <?> "shell command"

commandLine :: Parser String
commandLine
  = many (noneOf "\n") <?> "options"

anything :: Parser String
anything
  = lexeme (many1 (noneOf "\^Z"))

identifier :: Parser Name
identifier
  = lexeme (
    do c  <- lower <|> upper
       cs <- idchars
       return (newName (c:cs))
    <?> "identifier")

{-    
nat :: Parser Int
nat
  = lexeme (
    do{ ds <- many1 digit
      ; return (foldl (\n d -> 10*n + digitToInt d) 0 ds)
      })
  <?> "number"
-}

filenames :: Parser [String]
filenames
  = many filename

filename :: Parser String
filename
  = lexeme (
    do{ char '"'
      ; s <- many1 (noneOf "\"\n")
      ; char '"'
      ; return s
      }
    <|>
    do{ many1 (noneOf "\"\n \t")  }
    <?> "file name")


{--------------------------------------------------------------------------
  Whitespace and lexemes
--------------------------------------------------------------------------}    
special :: String -> Parser ()
special name
  = lexeme (do{ string name; return () })

symbol :: String -> Parser ()
symbol name
  = lexeme (try (do{ istring name; notFollowedBy alphaNum }))

istring :: String -> Parser ()
istring s
  = (mapM_ (\c -> satisfy (\d -> toLower d == toLower c)) s)
    <?> s


lexeme p       
    = do{ x <- p; whiteSpace; return x  }

wrap p
  = do{ whiteSpace
      ; x <- p
      ; eof
      ; return x
      }
  
--whiteSpace    
whiteSpace 
  = skipMany white
  
white
  = simpleSpace <|> {- oneLineComment <|> -} multiLineComment <?> ""
      
simpleSpace 
  = skipMany1 (satisfy isSpace)    
    
oneLineComment :: Parser ()
oneLineComment 
  = do{ try (string "--")
      ; skipMany (satisfy (/= '\n'))
      ; return ()
      }

multiLineComment 
  = do { try (string "{-")
       ; inComment
       }

inComment
    =   do{ try (string "-}") ; return () }
    <|> do{ multiLineComment             ; inComment}
    <|> do{ skipMany1 (noneOf startEnd)  ; inComment}
    <|> do{ oneOf startEnd               ; inComment}
    <?> "end of comment"  
    where
      startEnd   = "{-}"

p << q
  = do{ x <- p; q; return x }

-- Parsers from `Syntax.Lexer`

idchars :: Parser String
idchars
  = do cs <- many idchar
       return (concat cs)

idchar :: Parser String
idchar
  = do c <- alphanum <|> oneOf "_"
       return [c]
  <|> 
    try (do char '-'
            c <- letter
            return ['-',c])

alphanum = letter <|> digit

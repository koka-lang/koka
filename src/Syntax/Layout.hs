------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Add braces and semicolons based on layout.
-}
-----------------------------------------------------------------------------
module Syntax.Layout( layout, testFile, combineLineComments ) where

import Common.Range hiding (after)

-----------------------------------------------------------
-- testing
-----------------------------------------------------------
-- import Lib.Trace
import Syntax.Lexeme
import Syntax.Lexer
import Common.Name  ( Name )

testFile fname
  = do input <- readInput ("test/" ++ fname)
       testEx fname input

test xs
  = testEx "" xs

testEx fname input
  = let source = Source fname input 
        xs = lexing source 1 input 
    in putStrLn (unlines (map show (layout True xs)))

-- | @layout idmods lexs@ does layout processing on a list of lexemes @lexs@.
layout :: Bool -> [Lexeme] -> [Lexeme]
layout semiInsert lexemes
  = let semi f = if semiInsert then f else id
        ls =  -- semi indentLayout $ 
              -- semi lineLayout $
              removeWhite $ 
              associateComments $
              removeWhiteSpace $ 
              combineLineComments $ 
              -- semi checkComments $ 
              lexemes
    in -- trace (unlines (map show (take 100 ls))) $
       seq (length ls) ls          

removeWhite :: [Lexeme] -> [Lexeme]
removeWhite lexemes
  = filter (not . lexemeIsWhite) lexemes

removeWhiteSpace :: [Lexeme] -> [Lexeme]
removeWhiteSpace lexemes
  = filter (not . lexemeIsWhiteSpace) lexemes
  where
    lexemeIsWhiteSpace (Lexeme _ (LexWhite _)) = True
    lexemeIsWhiteSpace _ = False



-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

endLine
  = posLine . rangeEnd

startLine
  = posLine . rangeStart

startCol
  = posColumn . rangeStart

endCol
  = posColumn . rangeEnd

-- Just before or after a token.
before range
  = makeRange (rangeStart range) (rangeStart range)

after range
  = makeRange (rangeEnd range) (rangeEnd range)

-----------------------------------------------------------
-- Associate comments that precede a declaration
-- to the corresponding keyword
-----------------------------------------------------------
associateComments :: [Lexeme] -> [Lexeme]
associateComments lexs
  = scan lexs
  where
    scan lexs
      = case lexs of
          (Lexeme r1 (LexComment comment) : Lexeme r2 (LexKeyword k _) : ls)
            | k `elem` docKeyword && adjacent comment r1 r2 
             -> Lexeme r1 (LexComment comment) : Lexeme r2 (LexKeyword k comment) : scan ls
          (Lexeme r1 (LexComment comment) : Lexeme rv (LexKeyword kv docv) : Lexeme r2 (LexKeyword k _) : ls)
            | kv `elem` ["public","private","abstract"] && 
              k `elem` docKeyword && adjacent comment r1 r2
             -> Lexeme r1 (LexComment comment) : Lexeme rv (LexKeyword kv docv) : Lexeme r2 (LexKeyword k comment) : scan ls
          (l:ls)
             -> l : scan ls
          [] -> []
      where
        docKeyword = ["fun","function","val","type","cotype","rectype","effect","struct","con","alias","extern","external","module"]

        adjacent comment r1 r2
          = case (reverse comment) of
              '\n':_ -> posLine (rangeEnd r1) == posLine (rangeStart r2)
              _      -> posLine (rangeEnd r1) == posLine (rangeStart r2) - 1


-----------------------------------------------------------
-- Combine adjacent line comments into one  block comment (for html output)
-----------------------------------------------------------
combineLineComments :: [Lexeme] -> [Lexeme]
combineLineComments lexs
  = scan lexs
  where
    scan lexs
      = case lexs of
          -- combine newline comments into one big comment. This is for html output.
          (Lexeme r1 (LexComment ('/':'/':c1)) : Lexeme r2 (LexComment ('/':'/':c2)) : ls)
              -> scan (Lexeme (combineRange r1 r2) (LexComment ("//" ++ c1 ++ "//" ++ c2)) : ls)
          (l:ls)
              -> l : scan ls
          []  -> []

{----------------------------------------------------------
  Deprecated
----------------------------------------------------------}

-----------------------------------------------------------
-- Check for comments in indentation
-----------------------------------------------------------
checkComments :: [Lexeme] -> [Lexeme]
checkComments lexemes
  = check 0 rangeNull lexemes
  where
    check prevLine commentRng []
      = []
    check prevLine commentRng (lexeme@(Lexeme rng lex) : ls)
      = lexeme : 
        case lex of
          LexComment _ -> check prevLine rng ls 
          LexWhite _   -> check prevLine commentRng ls
          _            -> checkIndent ++
                          check (endLine rng) commentRng ls
      where
        checkIndent 
          = if (startLine rng > prevLine && startLine rng == endLine commentRng && endCol commentRng > 1 {- for wrap-around line columns -}) 
             then [Lexeme commentRng (LexError "layout: comments cannot be placed in the indentation of a line")]
             else []


{----------------------------------------------------------
  Semicolon insertion: assumes no whitespace
----------------------------------------------------------}
indentLayout :: [Lexeme] -> [Lexeme]
indentLayout []     = [Lexeme rangeNull LexInsSemi]
indentLayout (l:ls) = tail $
                      brace 0 -- (posColumn (rangeStart (getRange l)))
                            [] 
                            (before (getRange l)) 
                            (Lexeme (before (getRange l)) (LexSpecial "{"):l:ls)

brace :: Int ->   [Int] -> Range -> [Lexeme] -> [Lexeme]
brace layout layouts prevRng []
  = [Lexeme (after prevRng) LexInsSemi] -- end of file
brace layout layouts prevRng lexemes@(lexeme@(Lexeme rng lex):ls)
  = case lex of
      LexSpecial "{"
        -> case ls of
             [] -> check layout layouts prevRng lexemes
             (Lexeme rng2 lex2 : _)
                -> let layoutNew = startCol rng2
                   in  checkNewLayout layoutNew rng2 lex2 ++
                       check layoutNew (layout:layouts) rng lexemes
      LexSpecial "}"
        -> [Lexeme (after prevRng) LexInsSemi] ++
           case layouts of
             []     -> check 0 [] rng lexemes -- unbalanced braces
             (i:is) -> check i is rng lexemes -- pop the layout stack
      LexError _
        -> lexeme : check layout layouts prevRng lexemes   -- ignore lexical errors 
      _ -> check layout layouts prevRng lexemes
  where
    checkNewLayout layoutNew rng2 lex2
      = if (layoutNew <= layout) 
         then case lex2 of
                LexSpecial "}" 
                  -> []
                _ -> [Lexeme rng2 (LexError ("layout start: line must be indented more than the enclosing layout context (column " ++ show layout ++ ")"))] 
         else []


check :: Int -> [Int] -> Range -> [Lexeme] -> [Lexeme]
check layout layouts prevRng []
  = []

check layout layouts prevRng (lexeme@(Lexeme rng lex):ls)
  = checkIndent ++
    insertSemi ++
    (lexeme : brace layout layouts rng ls)
  where
    newline = endLine prevRng < startLine rng 
    indent  = startCol rng
       
    checkIndent 
      = if (newline && indent < layout)
         then [Lexeme rng (LexError ("layout: line must be indented at least as much as the enclosing layout context (column " ++ show layout ++ ")"))]
         else []

    insertSemi
      = if (newline && indent == layout)
         then case lex of
                 LexSpecial s    |  s `elem` ["{","]",")"] -> []
                 LexKeyword k _  |  k `elem` ["then","else","elif"] -> []
                 _ -> [Lexeme (after prevRng) LexInsSemi]
         else []




-----------------------------------------------------------
-- Line Layout: insert semi colon based on line ending token
-----------------------------------------------------------
lineLayout :: [Lexeme] -> [Lexeme]
lineLayout lexemes
  = case lexemes of
      [] -> []
      (l:ls) -> semiInsert (Lexeme (before (getRange l)) (LexWhite "")) (l:ls)

semiInsert :: Lexeme -> [Lexeme] -> [Lexeme]
semiInsert (Lexeme prevRng prevLex) lexemes
  = case lexemes of
      [] -> [semi] -- eof
      (lexeme@(Lexeme rng lex) : ls) ->
        case lex of
          LexSpecial "}" -> semi : lexeme : semiInsert lexeme ls -- always before '}'
          _ -> if (endLine prevRng < startLine rng && endingToken prevLex && not (continueToken lex))
                then semi : lexeme : semiInsert lexeme ls
                else lexeme : semiInsert lexeme ls  
  where 
    semi = Lexeme (after prevRng) LexInsSemi

    endingToken lex
      = case lex of
          LexId _     -> True
          LexIdOp _   -> True
          LexCons _   -> True
          LexInt _ _  -> True
          LexFloat _ _-> True
          LexChar _   -> True
          LexString _ -> True
          LexSpecial s -> s `elem` ["}",")","]"]
          LexOp s      -> show s == ">"
          _            -> False

    continueToken lex
      = case lex of
          LexKeyword k _ -> k `elem` ["then","else","elif","=",":","->"]
          -- LexSpecial "{" -> True
          LexSpecial s   -> s `elem` ["{",")","]"]
          LexOp s        -> show s == ">"
          _              -> False
-----------------------------------------------------------
-- Identify module identifiers: assumes no whitespace
-----------------------------------------------------------
identifyModules :: [Lexeme] -> [Lexeme]
identifyModules lexemes
  = let (names,headerCount) = scanImports 0 [] (map unLexeme lexemes) 
    in replaceModules (const True) (take headerCount lexemes)
        ++ 
       replaceModules (`elem` names) (drop headerCount lexemes)
       -- debug: map (\n -> Lexeme rangeNull (LexId n)) names
  where
    unLexeme (Lexeme rng lex) = lex

replaceModules :: (Name -> Bool) -> [Lexeme] -> [Lexeme]
replaceModules replaceName lexemes
  = map replace lexemes
  where
    replace lex
      = case lex of
          (Lexeme rng (LexId id)) | replaceName id
             -> Lexeme rng (LexModule id id)
          _  -> lex

scanImports :: Int -> [Name] -> [Lex] -> ([Name],Int)
scanImports count modules lexemes
  = case lexemes of
      (lex@(LexKeyword "import" _) : lexs)  -> scanImport (count+1) modules lexs
      (lex:lexs) | inImportSection lex      -> scanImports (count+1) modules lexs
      _ -> (modules,count)
  where
    inImportSection lex
      = case lex of
          LexKeyword kw _ -> kw `elem` ["module","import","as","public","private","."]
          LexSpecial s    -> s `elem` [";","(",")","{","}"]
          LexId id        -> True
          _               -> False

scanImport count modules lexemes
  = case lexemes of
      (LexId id : lex@(LexKeyword "." _) : lexs)
        -> scanImport (count+2) modules lexs
      (LexId mid : lex1@(LexKeyword "as" _) : LexId id : lexs)
        -> scanImports (count+3) (id : modules) lexs
      (LexId id : lexs)
        -> scanImports (count+1) (id : modules) lexs
      _ -> scanImports count modules lexemes -- just ignore



------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Add braces and semicolons based on layout.
-}
-----------------------------------------------------------------------------
module Syntax.Layout( layout, testFile, combineLineComments ) where

import Data.Char(isSpace)
import Data.List(partition)
import Common.Range hiding (after)

-----------------------------------------------------------
-- testing
-----------------------------------------------------------
-- import Lib.Trace
import Syntax.Lexeme
import Syntax.Lexer
import Common.Name  ( Name, nameId )

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
        ls =  semi indentLayout $ 
              -- semi lineLayout $
              removeWhite $ 
              associateComments $
              removeWhiteSpace $ 
              combineLineComments $ 
              semi checkComments $ 
              lexemes
    in -- trace (unlines (map show ls)) $
       seq (last ls) ls

isLexError (Lexeme _ (LexError {})) = True
isLexError _ = False

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
          -- special comments
          (Lexeme r1 (LexComment (comment@('/':'/':'.':cs))) : ls) | not (any isSpace (trimRight cs))
             -> Lexeme r1 (LexSpecial (trimRight comment)) : scan ls
          -- comment association
          (Lexeme r1 (LexComment comment) : Lexeme r2 (LexKeyword k _) : ls)  
            | k `elem` docKeyword && adjacent comment r1 r2 
             -> Lexeme r1 (LexComment comment) : Lexeme r2 (LexKeyword k comment) : scan ls
          (Lexeme r1 (LexComment comment) : l : Lexeme r2 (LexKeyword k _) : ls)  -- public type, inline fun
            | k `elem` docKeyword && adjacent comment r1 r2 && isAttr l
            -> Lexeme r1 (LexComment comment) : l : Lexeme r2 (LexKeyword k comment) : scan ls
          (Lexeme r1 (LexComment comment) : l1 : l2 : Lexeme r2 (LexKeyword k _) : ls) -- private inline fun, public value type
            | k `elem` docKeyword && adjacent comment r1 r2 && isAttr l1 && isAttr l2
            -> Lexeme r1 (LexComment comment) : l1 : l2 : Lexeme r2 (LexKeyword k comment) : scan ls             
          -- other
          (l:ls)
             -> l : scan ls
          [] -> []
      where
        docKeyword = ["fun","function","val","control","rcontrol","except"
                     ,"type","cotype","rectype","effect","struct","con","alias"
                     ,"extern","external","module"
                     ]                     

        isAttr l   = case l of  -- just approximate is ok
                      Lexeme _ (LexKeyword{}) -> True
                      Lexeme _ (LexId{})      -> True
                      _ -> False

        adjacent comment r1 r2
          = case (reverse comment) of
              '\n':_ -> posLine (rangeEnd r1) == posLine (rangeStart r2)
              _      -> posLine (rangeEnd r1) == posLine (rangeStart r2) - 1

trimRight s
  = reverse (dropWhile isSpace (reverse s))
  
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
indentLayout (l:ls) = let prevLex = Lexeme (before (getRange l)) (LexInsLCurly)
                      in  tail $
                          brace 0 -- (posColumn (rangeStart (getRange l)))
                                [] 
                                prevLex
                                (prevLex:l:ls)

brace :: Int ->   [Int] -> Lexeme -> [Lexeme] -> [Lexeme]
brace layout layouts prev []
  = [Lexeme (after (getRange prev)) LexInsSemi] -- end of file
    ++ (case layouts of 
          [] -> []
          (_:lays) -> map (\_ -> Lexeme (after (getRange prev)) LexInsRCurly) lays)  -- closing braces
brace layout layouts prev lexemes@(lexeme@(Lexeme _ (LexError{})):ls)  -- ignore errors
  = lexeme : brace layout layouts prev ls
brace layout layouts prev@(Lexeme prevRng prevLex) lexemes@(lexeme@(Lexeme rng lex):ls)
  = case lex of
      LexSpecial "{"  -> openBrace
      LexSpecial "}"  -> closeBrace
      LexInsLCurly    -> openBrace
      LexInsRCurly    -> closeBrace
      LexError _
        -> lexeme : check layout layouts prev lexemes   -- ignore lexical errors 
      _ -> check layout layouts prev lexemes
  where
    openBrace
      = case ls of
          [] -> check layout layouts prev lexemes
          (err@(Lexeme _ (LexError{})) : Lexeme rng2 lex2 : _)
            -> let layoutNew = startCol rng2
                in  [err] ++ 
                    checkNewLayout layoutNew rng2 lex2 ++
                    (lexeme : brace layoutNew (layout:layouts) lexeme ls)
          (Lexeme rng2 lex2 : _)
            -> let layoutNew = startCol rng2
                in  checkNewLayout layoutNew rng2 lex2 ++
                    (lexeme : brace layoutNew (layout:layouts) lexeme ls)

    closeBrace 
      = (case prevLex of
          LexSpecial ";" -> []
          LexInsSemi     -> []
          _ -> [Lexeme (after prevRng) LexInsSemi]) ++ 
        [lexeme] ++
        case layouts of
          []     -> brace 0 [] lexeme ls -- unbalanced braces
          (i:is) -> brace i is lexeme ls -- pop the layout stack                    
                    
    checkNewLayout layoutNew rng2 lex2
      = if (layoutNew <= layout) 
         then case lex2 of
                LexSpecial "}" -> []
                LexInsRCurly   -> []
                _ -> [Lexeme rng2 (LexError ("layout start: line must be indented more than the enclosing layout context (column " ++ show layout ++ ")"))] 
         else []


check :: Int -> [Int] -> Lexeme -> [Lexeme] -> [Lexeme]
check layout layouts prev []
  = []
check layout layouts prev@(Lexeme prevRng prevLex) lexemes@(lexeme@(Lexeme rng lex):ls)
  = -- checkIndent ++
    case insertOpenCloseBrace of
      []        -> insertSemi ++ (lexeme : brace layout layouts lexeme ls)
      openClose -> brace layout layouts prev (openClose ++ lexemes)
  where
    newline = endLine prevRng < startLine rng 
    indent  = startCol rng
    

    {-   
    checkIndent 
      = if (newline && indent < layout)
         then [Lexeme rng (LexError ("layout: line must be indented at least as much as the enclosing layout context (column " ++ show layout ++ ")"))]
         else []
    -}

    insertSemi
      = if (newline && indent == layout && not (continuationToken lex))
         then [Lexeme (after prevRng) LexInsSemi]
         else []

    insertOpenCloseBrace 
      = if (newline && indent > layout &&
            not (endingToken prevLex) && not (continuationToken lex))
         then [Lexeme (after prevRng) LexInsLCurly]
         else if (newline && indent < layout) 
          then case lex of 
                LexSpecial "}" -> []
                LexInsRCurly   -> []
                _ -> [Lexeme (after prevRng) LexInsRCurly]                
          else []

continuationToken :: Lex -> Bool 
continuationToken lex
      = case lex of
          LexSpecial s    -> s `elem` [")",">","]",",","{","}"]
          LexKeyword k _  -> k `elem` ["then","else","elif","->","=","|",":",".",":="] 
          LexOp op        -> not (nameId op `elem` ["<"])
          LexInsLCurly    -> True
          LexInsRCurly    -> True
          _ -> False

endingToken :: Lex -> Bool 
endingToken lex
      = case lex of
          LexSpecial s    -> s `elem` ["(","<","[",",","{"]
          LexKeyword k _  -> k `elem` ["."]
          LexInsLCurly    -> True
          LexOp op        -> not (nameId op `elem` [">"])
          _ -> False


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
          LexInsRCurly    -> True
          LexInsLCurly    -> True
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

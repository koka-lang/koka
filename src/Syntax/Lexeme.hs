------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Definition of lexical tokens ('Lexeme').
-}
-----------------------------------------------------------------------------
module Syntax.Lexeme
             ( Lexeme(..) -- Eq, Show
             , Lex(..)    -- Eq, Show, Enum
             , sameLexeme, sameLex
             , lexemeIsWhite, lexIsWhite
             , isTypeVar
             ) where

import Data.Char(isLower,isDigit,toUpper)
import Common.Name  ( Name, nameId )
import Common.Range

isTypeVar :: Name -> Bool
isTypeVar name
  = case nameId name of
      (c:cs) -> (isLower c && all isDigit cs)
      _      -> False


-----------------------------------------------------------
-- Lexer tokens
-----------------------------------------------------------
-- | A lexical token with an associated range.
data Lexeme = Lexeme !Range !Lex
            deriving Eq

-- | A lexical token.
data Lex    = LexInt     !Integer !String {- original number, used for documentation -}
            | LexFloat   !Double !String {- original number, used for documentation -}
            | LexChar    !Char
            | LexString   !String
            | LexId       !Name
            | LexCons     !Name       
            | LexOp       !Name
            | LexPrefix   !Name
            | LexIdOp     !Name
            | LexWildCard !Name
            | LexKeyword  !String !String
            | LexSpecial  !String
            | LexComment  !String
            | LexWhite    !String

            -- special for highlighting
            | LexModule   !Name !Name   -- ^ alias full-import
            | LexTypedId  !Name !String
            
            | LexInsLCurly  -- ^ inserted '{'
            | LexInsRCurly  -- ^ inserted '}'
            | LexInsSemi    -- ^ inserted ';'
            | LexError String  -- ^ layout errors


            deriving Eq

-- | 'True' when the lexeme is whitespace
lexemeIsWhite :: Lexeme -> Bool
lexemeIsWhite (Lexeme range lex)
  = lexIsWhite lex

-- | 'True' when the lexical token is whitespace
lexIsWhite :: Lex -> Bool
lexIsWhite lex
  = case lex of
      LexWhite txt   -> True
      LexComment txt -> True
      other          -> False

-- | Returns 'True' if the lexical tokens of the lexeme are of the same kind.
sameLexeme :: Lexeme -> Lexeme -> Bool
sameLexeme (Lexeme range1 lex1) (Lexeme range2 lex2)
  = sameLex lex1 lex2

-- | Returns 'True' if the lexical tokens are of the same kind (i.e. same constructor)
sameLex :: Lex -> Lex -> Bool
sameLex lex1 lex2
  = case (lex1,lex2) of
      (LexKeyword name1 _, LexKeyword name2 _)  -> name1 == name2
      (LexSpecial name1, LexSpecial name2)      -> name1 == name2
      other                                     -> fromEnum lex1 == fromEnum lex2

instance Enum Lex where
  fromEnum lex
    = case lex of
        LexInt _ _      -> 0
        LexFloat _ _    -> 1
        LexChar _       -> 2
        LexString _     -> 3
        LexId  _        -> 4
        LexOp    _      -> 5
        LexPrefix _     -> 19
        LexIdOp _       -> 20
        LexWildCard _   -> 6
        LexModule _ _   -> 7
        LexKeyword _ _  -> 8
        LexSpecial _    -> 9
        LexComment _    -> 10
        LexWhite   _    -> 11
        LexInsLCurly    -> 13
        LexInsRCurly    -> 14
        LexInsSemi      -> 15
        LexError _      -> 16
        LexCons _       -> 17
        LexTypedId _ _  -> 18

  toEnum lex
    = error "Lexer.Lex.toEnum: can not convert a number to lex element"

instance Show Lexeme where
  show = showLexeme

instance Show Lex where
  show = showLex

instance Ranged Lexeme where
  getRange (Lexeme rng _) = rng

showLexeme :: Lexeme -> String
showLexeme (Lexeme r lex)
  = showFullRange r ++ ": " ++ showLex lex
    -- showLex lex

showLex :: Lex -> String
showLex lex
  = -- showSpaces $
    case lex of
      LexInt i s    -> s -- if isHex then ("0x" ++ map toUpper (showHex i "")) else show i
      LexFloat f s  -> s -- show f
      LexChar c     -> show c
      LexString s   -> show s
      LexId  id     -> "identifier \"" ++ show id ++ "\""
      LexOp id      -> "operator \"" ++ show id ++ "\""
      LexPrefix id  -> "prefix operator \"" ++ show id ++ "\""
      LexIdOp id    -> "identifier \"(" ++ show id ++ ")\""
      LexWildCard id-> "wildcard \"" ++ show id ++ "\""
      LexModule id _  -> "module \"" ++ show id ++ "\""
      LexKeyword k d-> "keyword " ++ k ++ (if null d then "" else " (" ++ d ++ ")")
      LexSpecial s  -> "\"" ++ s ++ "\""
      LexComment s  -> "comment \"" ++ s ++ "\""
      LexWhite w    -> "white"
      LexInsLCurly  -> "start of statements"
      LexInsRCurly  -> "end of statements"
      LexInsSemi    -> "end of statement"
      LexError msg  -> msg
      LexCons id    -> "constructor \"" ++ show id ++ "\""      
      LexTypedId id tp -> "typedid " ++ show id ++ ":" ++ tp
  where
    showSpaces = map (\c -> if (c==' ') then '_' else c)


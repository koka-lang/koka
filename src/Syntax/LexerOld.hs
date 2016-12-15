------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-  Break up the input in lexical tokens.
-}
-----------------------------------------------------------------------------
module Syntax.Lexer
            ( module Syntax.Lexeme
            , lexing, lexer
            , readInput, extractLiterate
            -- * character classes
            , symbols
            , specialNames
            , reservedNames
            ) where

-- import Lib.Trace
import Data.Char                                 hiding    ( isLetter, isAlphaNum ) 
import Lib.Set as Set                               ( Set, fromList, member )
import qualified Lib.PPrint as PP

import Text.Parsec                   as Parsec hiding ( space,tab,lower,upper,alphaNum,letter )
import Text.Parsec.Pos   (newPos)
import Text.Parsec.String

import Common.Failure
import Common.Error
import Common.Name    hiding (qualify)
import Common.Range as Range
import Syntax.Lexeme

import System.IO hiding (utf8)

-----------------------------------------------------------
-- Reserved
-----------------------------------------------------------

specialNames :: [String]
specialNames
  = [ "{", "}"
    , "(", ")"
    , "<", ">"
    , "[", "]"
    , ";", ","
    , "`"
    ]

reservedNames :: Set String
reservedNames
  = Set.fromList $
    [ "infix", "infixr", "infixl", "prefix", "postfix"
    , "type", "cotype", "rectype", "alias"
    , "struct", "enum", "con"
    , "fun", "function", "val", "var"
    , "external"
    , "if", "then", "else", "elif", "return", "match"
    , "forall", "exists", "some", "with"
    , "private", "public", "abstract"
    , "module", "import", "as"
    , "="
    , "."
    , ":"
    , "->"
    , ":="
    -- for core interfaces
    , "rec"
    -- future reserved
    , "try", "yield"
    , "interface", "instance"
    ]

symbols :: [Char]
symbols
  = "$%&*+@!/\\^~=.:-?<>|"    

isReserved :: String -> Bool
isReserved name
  = Set.member name reservedNames

specials :: Parser Lex
specials
  = choice (map special specialNames)
  <?> "special"

special :: String -> Parser Lex
special name
  = do try (string name)
       return (LexSpecial name)
  <?> name


-----------------------------------------------------------
-- test
-----------------------------------------------------------


test xs
  = testEx "" xs

testEx source xs
  = case parse (lexemes (Source "" xs) 1) source (bstringToString xs) of
      Left err  -> print err
      Right xs  -> putStrLn (unlines [show x | Lexeme _ x <- xs, x /= LexWhite ""])

{--------------------------------------------------------------------------
  TODO: lexer really should not fail.
--------------------------------------------------------------------------}

lexer :: FilePath -> Int -> BString -> [Lexeme]
lexer sourceName lineNo input
  = let source = Source sourceName input
    in uneither $ parse (lexemes source lineNo) sourceName (bstringToString input)

uneither (Left err) = failure $ "lexer error: " ++ show err
uneither (Right x)  = x

-----------------------------------------------------------
-- Lexemes
-----------------------------------------------------------

-- note: input can differ from sourceText source for literate files.
lexing  :: Source -> Int -> BString -> [Lexeme] -- Either ErrorMessage [Lexeme]
lexing source line input 
  = uneither $ parse (lexemes source line) (Range.sourceName source) (bstringToString input) -- of
    --  Left err  -> Left  $ ErrorParse (rangeNull) $ PP.text "invalid syntax" PP.<+> PP.string (show err)
    --  Right xs  -> Right $ xs

lexemes :: Source -> Int -> Parser [Lexeme]
lexemes source line
  = do pos <- getPosition
       setPosition (newPos (Parsec.sourceName pos) line 1)
       xs <- many $ choice $ map ranged tokens
       eof
       return xs
  where
    tokens  -- order matters! for example, first comment or otherwise /* may be scanned as an operator
      = [whitespace,comment,idOrOp,specials,literal,wildcard]  

    ranged :: Parser Lex -> Parser Lexeme
    ranged p
      = do start <- getPosition
           x <- p
           end   <- getPosition
           let range = makeRange (makePos source (-1) (sourceLine start) (sourceColumn start))
                                 (makePos source (-1) (sourceLine end) (sourceColumn end - 1))
           seq range $ return (Lexeme range x)

{--------------------------------------------------------------------------
  Literal
--------------------------------------------------------------------------}
literal :: Parser Lex
literal
  = numberLit <|> stringLit <|> charLit

-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
numberLit :: Parser Lex
numberLit
  = do iorf <- intOrFloat
       return (either LexInt LexFloat iorf)
  <?> "number"

integer :: Parser Lex
integer         = do i <- int
                     return (LexInt i)
                <?> "integer"

-- floats
floating        = do{ n <- decimal
                    ; fractExponent n
                    }


intOrFloat      = do{ char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat

zeroNumFloat    =  do{ n <- hexadecimal {- <|> octal -}
                     ; return (Left n)
                     }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)

decimalFloat    = do{ n <- decimal
                    ; option (Left n)
                             (fractFloat n)
                    }

fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }

fractExponent n = do{ fract <- try fraction -- "try" due to ".." as in "[1..6]"
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }

fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                  <?> "fraction"
                where
                  op d f    = (f + fromIntegral (digitToInt d))/10.0

exponent'       = do{ oneOf "eE"
                    ; f <- sign
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                  <?> "exponent"
                where
                   power e  | e < 0      = 1.0/power(-e)
                            | otherwise  = fromInteger (10^e)

sign            =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id


-- integers
int             = zeroNumber <|> decimal

zeroNumber      = do{ char '0'
                    ; hexadecimal <|> {- octal <|> -} decimal <|> return 0
                    }
                  <?> ""

decimal         = number 10 digit
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
-- octal           = do{ oneOf "oO"; number 8 octDigit  }

number :: Integer -> Parser Char -> Parser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = digitsToNum base digits
        ; seq n (return n)
        }

digitsToNum :: Integer -> [Char] -> Integer
digitsToNum base digits
  = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits

-----------------------------------------------------------
-- Qualification
-----------------------------------------------------------
idOrOp
  = qualify (plainid <|> plainop <|> plaincon) 

qualify :: Parser Lex -> Parser Lex
qualify p
  = do cid <- conid   -- constructor?
       (do char '.'   -- it was actually a module qualifier..
           tok <- p  
           return $ case tok of -- and qualify
             LexId name      -> LexId (addQualifier cid name)
             LexCons name    -> LexCons (addQualifier cid name) 
             LexKeyword name _ -> LexError ("cannot qualify keywords (" ++ show name ++ ")")
             _               -> failure ("Syntax.Lexer.qualify: unexpected token: " ++ show tok)
        <|>
           -- no qualifier: it was just a constructor
           return (LexCons (newName cid))  
        )
   <|>
    -- otherwise, just return p  
    do p
  where
    addQualifier modid name
      = newQualified (if null (nameModule name) then modid else (modid ++ "." ++ nameModule name)) (nameId name)


-----------------------------------------------------------
-- Operators
-----------------------------------------------------------
plainop :: Parser Lex
plainop = do name <- plainopx
             if (isReserved name)
               then return (LexKeyword name "")
               else return (LexOp (newName name))

plainopx
  = do c <- angle
       (try (do cs <- many anglebar
                ds <- many1 symbol
                return (c:(cs ++ ds)))
        <|> return [c])
  <|>
    do c <- oneOf "|?"
       ( do notFollowedBy angle
            cs <- many symbol
            return (c:cs)
        <|> return [c])
  <|>
    do c <- char ':'
       ( do notFollowedBy (char '?')
            cs <- many symbol
            return (c:cs)
        <|> return [c])
  <|>
    do many1 symbol

symbol    = oneOf symbols
anglebar  = angle <|> char '|'
angle     = oneOf "<>"


-----------------------------------------------------------
-- Identifiers
-----------------------------------------------------------
plaincon :: Parser Lex
plaincon
  = do cid <- conid
       return (LexCons (newName cid))

conid :: Parser String
conid
  = do c <- upper
       cs <- idchars
       return (c:cs)

plainid :: Parser Lex
plainid 
  = do c <- lower
       cs <- idchars
       let name = c:cs
       if (isReserved name)
        then return (LexKeyword name "")
        else return (LexId (newName name))

wildcard :: Parser Lex
wildcard
  = do c  <- char '_'
       cs <- idchars
       return (LexWildCard (newName (c:cs)))


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
   



-----------------------------------------------------------
-- Strings
-----------------------------------------------------------
charLit :: Parser Lex
charLit   
  = do char '\''
       c <- charGraphic '\'' <|> space <|> escape <|> utf8
       char '\''
       return (LexChar c)
    <?> "character"

charGraphic x  
  = satisfy (\c -> (isGraphic c) && (c /= '\\') && (c /= x))


stringLit :: Parser Lex
stringLit   
  = do string "@\"" -- literal string
       s <- many (satisfy (/='"') <|> do{ try (string "\"\""); return '"' })
       char '"' <?> "end of a string literal"
       return (LexString s)
  <|>
    do char '"'
       s <- many ((charGraphic '"' <|> space <|> escape <|> utf8) <?> "")
       char '"' <?> "end of a string literal"
       return (LexString s)                    
    <?> "string"


utf8 :: Parser Char
utf8 
  = do c1 <- extended
       let between lo hi  = (c1 >= lo && c1 <= hi)
       if (between '\xC2' '\xDF')
         then do c2 <- cont
                 return (decode2 c1 c2)
        else if (c1 == '\xE0')
         then do c2 <- range '\xA0' '\xBF'
                 c3 <- cont
                 return (decode3 c1 c2 c3)
        else if (between '\xE1' '\xEC')
         then do c2 <- cont
                 c3 <- cont
                 return (decode3 c1 c2 c3)
        else if (c1 == '\xED')
         then do c2 <- range '\x80' '\x9F'
                 c3 <- cont
                 return (decode3 c1 c2 c3)
        else if (between '\xEE' '\xEF')
         then do c2 <- cont
                 c3 <- cont
                 return (decode3 c1 c2 c3)
        else if (c1 == '\xF0')
         then do c2 <- range '\x90' '\xBF'
                 c3 <- cont
                 c4 <- cont
                 return (decode4 c1 c2 c3 c4)
        else if (between '\xF1' '\xF3')
         then do c2 <- cont
                 c3 <- cont
                 c4 <- cont
                 return (decode4 c1 c2 c3 c4)
        else if (c1 == '\xF4') 
         then do c2 <- range '\x80' '\x8F'
                 c3 <- cont
                 c4 <- cont
                 return (decode4 c1 c2 c3 c4)
         else fail "invalid utf-8 character"
  where
    range c1 c2   = satisfy (\c -> (c >= c1 && c <= c2))
    cont          = range '\x80' '\xBF'

    decode2 :: Char -> Char -> Char
    decode2 c1 c2 = toEnum $ ((fromEnum c1 - 0xC0) * 0x40) + (fromEnum c2 - 0x80)

    decode3 :: Char -> Char -> Char -> Char
    decode3 c1 c2 c3 = toEnum $ ((fromEnum c1 - 0xE0) * 0x40 * 0x40) + ((fromEnum c2 - 0x80) * 0x40) + (fromEnum c3 - 0x80)

    decode4 :: Char -> Char -> Char -> Char -> Char
    decode4 c1 c2 c3 c4 = toEnum $ ((fromEnum c1 - 0xF0) * 0x40 * 0x40 * 0x40) + ((fromEnum c2 - 0x80) * 0x40 * 0x40) + ((fromEnum c3 - 0x80) * 0x40) + (fromEnum c4 - 0x80)



escape          
  = do char '\\'
       (charesc <|> hexesc <?> "escape code")

hexesc :: Parser Char
hexesc
  = do digits <- do{ char 'x'; count 2 hexDigit }
                 <|>
                 do{ char 'u'; count 4 hexDigit }
                 <|>
                 do{ char 'U'; count 6 hexDigit }
       return (toEnum (fromInteger (digitsToNum 16 digits)))  -- TODO: check overflow

charesc         
  = choice (map parseEsc escMap)
 where
    parseEsc (c,code) = do{ char c; return code }
    escMap            = zip ("abfnrtv0\\\"\'")
                            ("\a\b\f\n\r\t\v\x00\\\"\'")



-----------------------------------------------------------
-- Whitespace
-----------------------------------------------------------
whitespace :: Parser Lex
whitespace
  = do xs <- many1 whitechar
       return (LexWhite xs)
  <|>
    do char '\t'
       return (LexError "tab character: use spaces instead")
  <?> ""

comment :: Parser Lex
comment
  = do content <- linecomment <|> blockcomment
       return (LexComment content)
  <?> ""

linecomment
  = do{ start <- try (string "//")
      ; com   <- many linechar
      ; return (start ++ com)
      }

linechar
  = graphic <|> extended <|> space <|> tab

blockcomment
  = do{ start <- try (string "/*")
      ; incomment [start]
      }

incomment acc
    =   do{ xs <- try (string "*/");  return (concat (reverse (xs:acc))) }
    <|> do{ xs <- blockcomment;       incomment (xs:acc)  }
    <|> do{ xs <- many1 contentchar;  incomment (xs:acc)  }
    <|> do{ x  <- oneOf commentchar;  incomment ([x]:acc) }
    <?> "end of comment"
    where
      commentchar     = "*/"
      contentchar     =   whitechar 
                      <|> satisfy (\c -> (isGraphic c || isExtended c || c=='\t') && not (c `elem` commentchar))


-----------------------------------------------------------
-- Character classes
-----------------------------------------------------------

space    :: Parser Char
space     = char ' '
tab      :: Parser Char
tab       = char '\t'
whitechar :: Parser Char
whitechar = satisfy isWhiteChar
alphanum :: Parser Char
alphanum  = letter <|> digit
isAlphaNum :: Char -> Bool
isAlphaNum c = isLetter c || isDigit c
letter   :: Parser Char
letter    = satisfy isLetter
isLetter  :: Char -> Bool
isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') 
lower    :: Parser Char
lower     = satisfy (\c -> (c >= 'a' && c <= 'z'))
upper    :: Parser Char
upper     = satisfy (\c -> (c >= 'A' && c <= 'Z'))
hexdigit :: Parser Char
hexdigit  = hexDigit

graphic  = satisfy isGraphic
extended = satisfy isExtended

isWhiteChar  :: Char -> Bool
isWhiteChar c = c == ' ' || c == '\n' || c == '\r'
isGraphic    :: Char -> Bool
isGraphic c   = c >= '\x21' && c <= '\x7E'
isExtended   :: Char -> Bool
isExtended c  = c >= '\x7F' && c <= '\xFF' 

-- bom      = do{ char '\xEF'; char '\xBB'; char '\xBF' }

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------
skip :: Parser a -> Parser ()
skip p
  = do{ p; return () }

-----------------------------------------------------------
-- Read in put
-----------------------------------------------------------
{-
utfDecode :: String -> String -> String
utfDecode acc s
  = case s of
      [] -> (reverse acc)
      (c:cs)            | (c <= '\x7F') -> utfDecode (c:acc) cs
      (c1:c2:cs)        | (c1 >= '\xC2' && c1 <= '\xDF' && isCont c2) -> utfDecode (decode2 c1 c2:acc) cs
      (c1:c2:c3:cs)     | (c1 >= '\xE0' && c1 <= '\xEF' && isCont c2 && isCont c3) -> utfDecode (decode3 c1 c2 c3:acc) cs
      (c1:c2:c3:c4:cs)  | (c1 >= '\xF0' && c1 <= '\xF4' && isCont c2 && isCont c3 && isCont c4) -> utfDecode (decode4 c1 c2 c3 c4:acc) cs

      -- modified utf8 encoding of 0
      (c1:c2:cs)        | (c1 == '\xC0' && c2 == '\x80') -> utfDecode ('\x00':acc) cs
      
      -- invalid sequence of 4: use replacement
      (c1:c2:c3:c4:cs)        | (c1 >= '\xF5' && c1 <= '\xF7' && isCont c2 && isCont c3 && isCont c4) -> utfDecode (replacement:acc) cs
      -- invalid sequence of 5: use replacement
      (c1:c2:c3:c4:c5:cs)     | (c1 >= '\xF8' && c1 <= '\xFB' && isCont c2 && isCont c3 && isCont c4 && isCont c5) -> utfDecode (replacement:acc) cs
      -- invalid sequence of 6: use replacement
      (c1:c2:c3:c4:c5:c6:cs)  | (c1 >= '\xFC' && c1 <= '\xFD' && isCont c2 && isCont c3 && isCont c4 && isCont c5 && isCont c6) -> utfDecode (replacement:acc) cs
      
      -- continuation bytes: skip
      (c:cs)            | isCont c -> utfDecode acc cs
      -- (c1:c2:cs)        | (c1 >= '\x80' && c1 <= '\xBF' && isCont c2) -> utfDecode (decode2 c1 c2) cs
      
      -- otherwise: use replacement character
      (c:cs)            -> utfDecode (replacement:acc) cs
  where
    replacement = '\xFFFD'
    isCont c  = (c >= '\x80' && c <= '\xBF')
    decode2 :: Char -> Char -> Char
    decode2 c1 c2 = toEnum $ ((fromEnum c1 - 0xC0) * 0x40) + (fromEnum c2 - 0x80)

    decode3 :: Char -> Char -> Char -> Char
    decode3 c1 c2 c3 = toEnum $ ((fromEnum c1 - 0xE0) * 0x40 * 0x40) + ((fromEnum c2 - 0x80) * 0x40) + (fromEnum c3 - 0x80)

    decode4 :: Char -> Char -> Char -> Char -> Char
    decode4 c1 c2 c3 c4 = toEnum $ ((fromEnum c1 - 0xF0) * 0x40 * 0x40 * 0x40) + ((fromEnum c2 - 0x80) * 0x40 * 0x40) + ((fromEnum c3 - 0x80) * 0x40) + (fromEnum c4 - 0x80)
-}

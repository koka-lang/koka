------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
------------------------------------------------------------------------------
{
{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Syntax.Lexer(  lexing, lexer
                    , module Syntax.Lexeme
                    , readInput, extractLiterate
                    ) where

import Lib.Trace
import Data.Char
import Common.Failure
import Common.Range as Range
import Common.Name
import qualified Data.Set as Set
import Syntax.Lexeme

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import Data.Word( Word8 )
}

-----------------------------------------------------------
-- Character sets
-----------------------------------------------------------
$digit    = [0-9]
$hexdigit = [0-9a-fA-F]
$lower    = [a-z]
$upper    = [A-Z]
$letter   = [$lower$upper]
$space    = [\ ]
$tab      = [\t]
$return   = \r
$linefeed = \n
$graphic  = [\x21-\x7E]
$cont     = [\x80-\xBF]
$symbol   = [\$\%\&\*\+\~\!\\\^\#\=\.\:\-\?\|\<\>]
$special  = [\(\)\[\]\{\}\;\,]
$anglebar = [\<\>\|]
$angle    = [\<\>]
$finalid  = [\'\?]
$charesc  = [nrt\\\'\"]    -- "

-----------------------------------------------------------
-- Regular expressions
-----------------------------------------------------------
@newline      = $return?$linefeed

@utf8         = \xC0\x80
              | [\xC2-\xDF] $cont
              | \xE0 [\xA0-\xBF] $cont
              | [\xE1-\xEC] $cont $cont
              | \xED [\x80-\x9F] $cont
              | [\xEE-\xEF] $cont $cont
              | \xF0 [\x90-\xBF] $cont
              | [\xF1-\xF3] $cont $cont $cont
              | \xF4 [\x80-\x8F] $cont $cont

@linechar     = [$graphic$space$tab]|@utf8
@commentchar  = ([$graphic$space$tab] # [\/\*])|@newline|@utf8

@hexdigit2    = $hexdigit $hexdigit
@hexdigit4    = @hexdigit2 @hexdigit2
@hexesc       = x@hexdigit2|u@hexdigit4|U@hexdigit4@hexdigit2
@escape       = \\($charesc|@hexesc)
@stringchar   = ([$graphic$space] # [\\\"])|@utf8             -- " fix highlight
@charchar     = ([$graphic$space] # [\\\'])|@utf8
@stringraw    = ([$graphic$space$tab] # [\"])|@newline|@utf8  -- "

@idchar       = $letter|$digit|_|\-
@lowerid      = $lower @idchar* $finalid*
@upperid      = $upper @idchar* $finalid*
@conid        = @upperid
@modulepath   = (@lowerid\/)+
@qvarid       = @modulepath @lowerid
@qconid       = @modulepath @conid
@symbols      = $symbol+|\/
@qidop        = @modulepath \(@symbols\)
@idop         = \(@symbols\)

@decimal      = $digit+
@hexadecimal  = 0[xX]$hexdigit+
@natural      = @decimal|@hexadecimal

@sign         = [\-]?
@exp          = (\-|\+)? @decimal
@exp10        = [eE]@exp
@exp2         = [pP]@exp
@decfloat     = @sign @decimal (\. @decimal @exp10? | @exp10)
@hexfloat     = @sign @hexadecimal (\. $hexdigit+)? @exp2

-----------------------------------------------------------
-- Main tokenizer
-----------------------------------------------------------
program :-
-- white space
<0> $space+               { string $ LexWhite }
<0> $tab+               		{ string $ LexWhite }
<0> @newline              { constant $ LexWhite "\n" }
<0> "/*" $symbol*         { next comment $ more id }
<0> "//" $symbol*         { next linecom $ more id }
<0> ^\# $symbol*          { next linedir $ more id }


-- fun/function followed by '(' or '<'
<0> "fun" [\(\<]          { less 3 $ constant $ LexKeyword "fun.anon" "" }
<0> "function" [\(\<]     { less 7 $ constant $ LexKeyword "function.anon" "" }

-- qualified identifiers
<0> @qconid               { string $ LexCons . newQName }
<0> @qvarid               { string $ LexId . newQName }
<0> @qidop                { string $ LexIdOp . newQName . stripParens }

-- identifiers
<0> @lowerid              { string $ \s -> if isReserved s
                                               then LexKeyword s ""
                                           else if isMalformed s
                                               then LexError messageMalformed
                                               else LexId (newName s) }
<0> @conid                { string $ LexCons . newName }
<0> _@idchar*             { string $ LexWildCard . newName }

-- specials
<0> $special              { string $ LexSpecial }

-- literals
<0> @decfloat             { string $ \s -> LexFloat (read s) s }
<0> @hexfloat             { string $ \s -> LexFloat (parseHexFloat s) s }
<0> @sign @hexadecimal    { string $ \s -> LexInt (parseNum  s) s }
<0> @sign @decimal        { string $ \s -> LexInt (parseNum s) s }


-- type operators
<0> "||"                  { string $ LexOp . newName }
<0> $anglebar $anglebar+  { less 1 $ string $ LexOp . newName }

-- operators
<0> @idop                 { string $ LexIdOp . newName . stripParens }
<0> @symbols              { string $ \s -> if isReserved s
                                             then LexKeyword s ""
                                           else if isPrefixOp s
                                             then LexPrefix (newName s)
                                             else LexOp (newName s) }


-- characters
<0> \"                    { next stringlit $ more (const B.empty) }  -- "
<0> \@\"                  { next stringraw $ more (const B.empty) }  -- "

<0> \'\\$charesc\'        { string $ LexChar . fromCharEsc . head . drop 2 }
<0> \'\\@hexesc\'         { string $ LexChar . fromHexEsc . init . drop 3 }
<0> \'@charchar\'         { string $ LexChar . head . tail }
<0> \'.\'                 { string $ \s -> LexError ("illegal character literal: " ++ show (head (tail s))) }

-- catch errors
<0> .                     { string $ \s -> LexError ("illegal character: " ++ show s ++ (if (s=="\t") then " (replace tabs with spaces)" else "")) }

--------------------------
-- string literals

<stringlit> @stringchar+  { more id }
<stringlit> \\$charesc    { more fromCharEscB }
<stringlit> \\@hexesc     { more fromHexEscB }
<stringlit> \"            { pop $ \_ -> withmore (string LexString . B.init) } -- "
<stringlit> @newline      { pop $ \_ -> constant (LexError "string literal ended by a new line") }
<stringlit> .             { string $ \s -> LexError ("illegal character in string: " ++ show s) }

<stringraw> @stringraw+   { more id }
<stringraw> \"\"          { more B.tail } -- "
<stringraw> \"            { pop $ \_ -> withmore (string LexString . B.init) } -- "
<stringraw> .             { string $ \s -> LexError ("illegal character in raw string: " ++ show s) }

--------------------------
-- block comments

<comment> "*/"            { pop $ \state -> if state==comment then more id
                                             else withmore (string $ LexComment . filter (/='\r')) }
<comment> "/*"            { push $ more id }
<comment> @commentchar+   { more id }
<comment> [\/\*]          { more id }
<comment> .               { string $ \s -> LexError ("illegal character in comment: " ++ show s) }

--------------------------
-- line comments

<linecom> @linechar+      { more id }
<linecom> @newline        { pop $ \_ -> withmore (string $ LexComment . filter (/='\r')) }
<linecom> .               { string $ \s -> LexError ("illegal character in line comment: " ++ show s) }

--------------------------
-- line directives (ignored for now)

<linedir> @linechar+      { more id }
<linedir> @newline        { pop $ \_ -> withmore (string $ LexComment . filter (/='\r')) }
<linedir> .               { string $ \s -> LexError ("illegal character in line directive: " ++ show s) }

{
-----------------------------------------------------------
-- helpers
-----------------------------------------------------------
stripParens s
  = case reverse s of
      (')':cs) -> case span (/='(') cs of
                    (op,'(':qualifier) -> reverse (op ++ qualifier)
                    _ -> s
      _ -> s

newQName s
  = let (rname,rsmod) = span (/='/') (reverse s)
    in case rsmod of
         ('/':rmod)  -> newQualified (reverse rmod) (reverse rname)
         _           -> newName s

fromCharEscB, fromHexEscB :: BString -> BString
fromCharEscB bstr
  = BC.singleton $
    fromCharEsc $ BC.head (B.tail bstr)

fromHexEscB bstr
  = T.encodeUtf8 $ T.singleton $ fromHexEsc (BC.unpack (B.drop 2 bstr))

fromCharEsc :: Char -> Char
fromCharEsc c
  = case c of
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      c   -> c

fromHexEsc :: String -> Char
fromHexEsc s
  = toEnum $ digitsToNum 16 s

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
    ]

reservedNames :: Set.Set String
reservedNames
  = Set.fromList $
    [ "infix", "infixr", "infixl", "prefix", "postfix"
    , "type", "cotype", "rectype", "alias"
    , "struct", "enum", "con"
    , "fun", "val", "var"
    , "extern"
    , "effect", "handle", "handler"
    , "if", "then", "else", "elif", "return", "match"
    , "forall", "exists", "some", "with"
    , "private", "public", "abstract"
    , "module", "import", "as"
    , "="
    , "."
    , ":"
    , "->"
    , ":="
    -- backward compatibility
    , "function","external"
    -- for core interfaces
    , "rec"
    -- future reserved
    -- , "try", "yield"
    , "interface", "instance"
    ]

symbols :: [Char]
symbols
  = "$%&*+~!/\\^~=.:-?<>|"

isReserved :: String -> Bool
isReserved name
  = Set.member name reservedNames

isPrefixOp :: String -> Bool
isPrefixOp name
  = (name == "!" || name == "~")


parseHexFloat :: String -> Double
parseHexFloat s
  = parseSign parsePosHexFloat s

parsePosHexFloat :: String -> Double
parsePosHexFloat s
  = case s of
      '0':'x':ds -> parsePosHexFloatX ds
      '0':'X':ds -> parsePosHexFloatX ds
      _ -> parsePosHexFloatX s

parsePosHexFloatX :: String -> Double
parsePosHexFloatX ds
  = let (man,rest1) = span isHexDigit ds
        (frac,rest2) = case rest1 of
                         '.':rest -> span isHexDigit rest
                         _ -> ("",rest1)
        exp = case rest2 of
                'p':rest -> rest
                'P':rest -> rest
                _        -> "0"
        m = digitsToNum 16 (man ++ frac)
        e = (parseNum exp) - 4*(length frac)
    in encodeFloat m e

parseNum :: Num a => String -> a
parseNum s
  = parseSign parsePosNum s

parseSign :: Num a => (String -> a) -> String -> a
parseSign f s
  = case s of
     '-':xs -> negate (f xs)
     '+':xs -> f xs
     _      -> f s

parsePosNum :: Num a => String -> a
parsePosNum s
  = case s of
       '0':'x':digits -> digitsToNum 16 digits
       '0':'X':digits -> digitsToNum 16 digits
       _ -> digitsToNum 10 s

digitsToNum base digits
  = let n = foldl (\x d -> base*x + fromIntegral (xdigitToInt d)) 0 digits
    in seq n n

xdigitToInt :: Char -> Int
xdigitToInt c
  = if (isHexDigit c) then fromIntegral (digitToInt c) else trace ("lexer hex digit: " ++ [c]) (0)

isMalformed :: String -> Bool
isMalformed s
  = case s of
      '-':c:cs   | not (isLetter c) -> True
      c:'-':cs   | not (isLetter c || isDigit c) -> True
      c:cs       -> isMalformed cs
      []         -> False

messageMalformed = "malformed identifier: a dash must be preceded by a letter or digit, and followed by a letter"
------------------------------------------------------------------------------
-- Lexer state and actions
------------------------------------------------------------------------------
data State = State { pos      :: !Pos    -- current position
                   , startPos :: !Pos    -- token start position (for 'more')
                   , states   :: ![Int]
                   , retained :: ![BString]
                   , previous :: !Char
                   , current  :: !BString
                   , previousLex :: Lex
                   }

type Action = BString -> State -> State -> (Maybe Lex, State)

------------------------------------------------------------------------------
-- Action helpers
------------------------------------------------------------------------------
token :: (BString -> Lex) -> Action
token lex = \bs st0 st1 -> (Just (lex bs), st1)

string :: (String -> Lex) -> Action
string lex = token (lex . bstringToString)

keyword :: Action
keyword = string (\s -> LexKeyword s "")

next :: Int -> Action -> Action
next state action
  = \bs st0 st1 -> let (x,st2) = action bs st0 st1
                   in (x,st2{ states = state:states st2})

push :: Action -> Action
push action
  = \bs st0 st1 -> next (head (states st1)) action bs st0 st1

pop :: (Int -> Action) -> Action
pop action
  = \bs st0 st1 -> let sts     = tail (states st1)
                       sts'    = if null sts then [0] else sts
                       (x,st2) = action (head sts') bs st0 st1
                   in (x,st2{ states = sts' })

more :: (BString -> BString) -> Action
more f = \bs st0 st1 -> (Nothing, st1{ retained = f bs : retained st1 })

less :: Int -> Action -> Action
less n action
  = \bs st0 st1 ->  let bs2 = B.take n (current st0)
                        pos2 = posMoves8 (pos st0) bs2
                        st2 = st1{ pos = pos2, current = B.drop n (current st0) }
                    in action bs2 st0 st2

withmore :: Action -> Action
withmore action
  = \bs st0 st1 -> action (B.concat (reverse (bs : retained st1))) st0 st1{ retained = [] }



constant x
  = token (\_ -> x)

------------------------------------------------------------------------------
-- Set up the Alex lexer framework
------------------------------------------------------------------------------
type AlexInput = State
type Byte   = Word8

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar st = previous st

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte st@State{ current = cs }
  = if B.null cs then Nothing
    else Just (B.head cs, st{ current = B.tail cs })

-- compatibility
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar st@State{ current = cs }
  = if BC.null cs then Nothing
    else Just (BC.head cs, st{ current = BC.tail cs })


-- alexScanTokens :: ByteString -> [token]
lexer :: FilePath -> Int -> BString -> [Lexeme]
lexer sourceName lineNo input
  = lexing (Source sourceName input) lineNo input

lexing :: Source -> Int -> BString -> [Lexeme]
lexing source lineNo input
  = let initPos = makePos source 0 lineNo 1
        initSt  = State initPos initPos [0] [] '\n' input (LexWhite "")
    in go initSt
  where go st =
          -- trace ("scan: " ++ show (pos st) ++ ": <" ++ show (head (states st)) ++ ">: " ++ show (BC.take 5 (current st))) $
          let idx0 = B.length (current st) in
          case alexScan st (head (states st)) of
            AlexEOF -> []
            AlexSkip  st1 len
              -> failure "Syntax.Lexer: rule without action"
            AlexError st1
              -> let range = makeRange (pos st) (pos st1) in
                 if B.null (current st)
                  then [Lexeme range $ LexError "unexpected end of input"]
                  else Lexeme range (LexError ("unexpected character " ++ show (BC.head (current st))))
                        : go (st1{ current = B.tail (current st1) })
            AlexToken st1 len act  -- len is wrong with utf8!
              -> let idx1 = B.length (current st1)
                     bs = B.take (idx0 - idx1) (current st)
                     p  = posMoves8 (pos st) bs
                     (mbtoken,st2) = seq p $ act bs st st1{ pos = p }
                 in case mbtoken of
                      Nothing    -> go st2  -- more
                      Just token -> let range = makeRange (startPos st) (before (pos st2))
                                        ltoken = lparen token (previousLex st1)
                                    in -- trace ("result: " ++ showFullRange range ++ ": " ++ show ltoken) $
                                       seq range $ Lexeme range ltoken : go st2{ startPos = pos st2, previousLex = ltoken }

        lparen token prev
          = case token of
              LexSpecial "("  | isApplyToken prev -> LexSpecial "(.("  -- application
              LexSpecial "["  | isApplyToken prev -> LexSpecial "[.["  -- indexing
              _ -> token

        isApplyToken prev
          = case prev of
              LexSpecial ")" -> True
              LexSpecial "]" -> True
              LexId _        -> True
              LexCons _      -> True
              LexIdOp _      -> True
              _              -> False


before p
  = p{ posColumn = max 1 (posColumn p - 1 ) }

}

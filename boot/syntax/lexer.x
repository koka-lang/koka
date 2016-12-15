------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{
module syntax.lexer 
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
$return   = \r
$linefeed = \n
$graphic  = [\x21-\x7E]
$cont     = [\x80-\xBF]
$symbol   = [\$\%\&\*\+\!\/\\\^\#\=\.\:\-\?\|\<\>]
$special  = [\(\)\[\]\{\}\;\,\`]
$anglebar = [\<\>\|]
$angle    = [\<\>]
$charesc  = [abfnrtv0\\\'\"]    -- "

-----------------------------------------------------------
-- Regular expressions
-----------------------------------------------------------
@newline      = $return?$linefeed
@whitechar    = @newline|$space

@utf8         = \xC0\x80 | [\xC2-\xDF] $cont 
              | \xE0 [\xA0-\xBF] $cont
              | [\xE1-\xEC] $cont $cont | \xED [\x80-\x9F] $cont
              | [\xEE-\xEF] $cont $cont | \xF0 [\x90-\xBF] $cont
              | [\xF1-\xF3] $cont $cont $cont | \xF4 [\x80-\x8F] $cont $cont 
  
@linechar     = [$graphic$space]|@utf8  
@commentchar  = ($graphic # [\/\*])|@whitechar|@utf8

@hexdigit2    = $hexdigit $hexdigit
@hexdigit4    = @hexdigit2 @hexdigit2
@hexesc       = x@hexdigit2|u@hexdigit4|U@hexdigit4@hexdigit2
@escape       = \\($charesc|@hexesc)
@stringchar   = ($graphic # [\\\"])|$space|@utf8|@escape  -- " fix highlight
@charchar     = ($graphic # [\\\'])|$space|@utf8|@escape

@idchar       = $letter|$digit|_|\-$letter
@lowerid      = $lower @idchar*
@upperid      = $upper @idchar*
@conid        = @upperid
@moduleid     = @upperid
@qvarid       = @moduleid\.@lowerid
@qconid       = @moduleid\.@conid
@symbols      = $symbol+
@qop          = @moduleid\.@symbols

@decimal      = $digit+
@hexadecimal  = 0[xX]$hexdigit+
@natural      = @decimal|@hexadecimal

@exponent     = [eE](\-|\+)? @decimal
@float        = @decimal \. @decimal @exponent?

-----------------------------------------------------------
-- Main tokenizer
-----------------------------------------------------------
program :-
-- white space
<0> $space+               { string $ LexWhite }
<0> @newline              { constant $ LexWhite "\n" }
<0> "/*" $symbol*         { next comment $ more }
<0> "//" $symbol*         { next linecom $ more }
<0> ^\# $symbol*          { next linedir $ more }

-- identifiers
<0> @lowerid              { text $ \t -> let s = T.unpack t
                                         in if isReserved s
                                             then LexKeyword s "" 
                                             else LexId (newNameT t) }
<0> @conid                { text $ LexCons . newNameT }
<0> @qconid               { text $ LexCons . newQNameT }
<0> @qvarid               { text $ LexId . newQNameT }
<0> _@idchar*             { text $ LexWildCard . newNameT }             

-- specials
<0> $special              { string $ LexSpecial }

-- type operators
<0> "<" $anglebar+        { less 1 $ text $ LexOp . newNameT }
<0> ">" $anglebar+        { less 1 $ text $ LexOp . newNameT }
<0> "|" $angle $symbol*   { less 1 $ text $ LexOp . newNameT }
<0> "-><" $symbol*        { less 2 $ constant $ LexKeyword "->" "" }
<0> ":?" $symbol*         { less 1 $ constant $ LexKeyword ":" "" }

-- operators 
<0> @symbols              { text $ \t -> let s = T.unpack t
                                         in if isReserved s
                                             then LexKeyword s "" 
                                             else LexOp (newNameT t) }
<0> @qop                  { text $ LexOp . newQNameT }

-- literals
<0> @decimal              { string $ LexInt . digitsToNum 10 }
<0> @hexadecimal          { string $ LexInt . digitsToNum 16 . drop 2 }
<0> @float                { string $ LexFloat . read }

<0> \"                    { next stringlit $ more }  -- " 
<0> \'@charchar\'         { string $ LexChar . head . tail }
<0> \'.\'                 { string $ \s -> LexError ("illegal character literal: " ++ show (head (tail s))) } 

-- catch errors
<0> .                     { string $ \s -> LexError ("illegal character: " ++ show s) } 

--------------------------
-- string literals

<stringlit> \"            { pop $ \_ -> withmore (string $ LexString . tail . init) } -- "
<stringlit> @stringchar+  { more }
<stringlit> @newline      { pop $ \_ -> constant (LexError "string ended by a new line") }
<stringlit> .             { string $ \s -> LexError ("illegal character in string: " ++ show s) } 

--------------------------
-- block comments

<comment> "*/"            { pop $ \state -> if state==comment then more 
                                             else withmore (string $ LexComment . filter (/='\r')) }
<comment> "/*"            { push $ more }
<comment> @commentchar+   { more }
<comment> [\/\*]          { more }
<comment> .               { string $ \s -> LexError ("illegal character in comment: " ++ show s) } 

--------------------------
-- line comments

<linecom> @linechar+      { more }
<linecom> @newline        { pop $ \_ -> withmore (string $ LexComment . filter (/='\r')) }
<linecom> .               { string $ \s -> LexError ("illegal character in line comment: " ++ show s) } 

--------------------------
-- line directives (ignored for now)

<linedir> @linechar+      { more }
<linedir> @newline        { pop $ \_ -> withmore (string $ LexComment . filter (/='\r')) }
<linedir> .               { string $ \s -> LexError ("illegal character in line directive: " ++ show s) } 

{
  
}

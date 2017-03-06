------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Syntax coloring
-}
-----------------------------------------------------------------------------
module Syntax.Highlight( Context(..), Nesting(..), Token(..), TokenComment(..)
                       , highlight
                       , highlightPrint
                       -- * Low level primitives
                       , highlightLexemes, highlightLexeme
                       , commentFlatten
                       , lexComment
                       , showLexeme
                       , isKeywordOp
                       ) where

import Lib.Trace
import Data.Char( isAlpha, isSpace, isAlphaNum, toLower )
import Lib.Printer
import Common.ColorScheme
import Common.Range
import Common.Name
import Syntax.Lexeme   ( Lexeme(..), Lex(..), lexemeIsWhite, isTypeVar)
import Syntax.Lexer    ( lexer )
import Syntax.Layout   ( combineLineComments )

-----------------------------------------------------------
-- Easy syntax highlighting
-----------------------------------------------------------
-- | Print source in color, given a color scheme, source name, initial line number, the input string, and
-- a 'Printer'.
highlightPrint :: Printer p => ColorScheme -> FilePath -> Int -> BString -> p -> IO ()
highlightPrint cscheme sourceName lineNo input p
  = sequence_ $ highlight (fmtPrint cscheme p) id CtxNormal sourceName lineNo input

fmtPrint :: Printer p => ColorScheme -> p -> Token (Lexeme) -> String -> IO ()
fmtPrint cscheme p token
  = case token of
      TokId _ _    -> write p
      TokOp _ _    -> write p
      TokTypeVar  -> withColor p (colorTypeVar cscheme) . write p
      TokTypeId _  -> withColor p (colorTypeCon cscheme) . write p
      TokTypeOp _  -> withColor p (colorTypeCon cscheme) . write p
      TokTypeSpecial -> withColor p (colorTypeSpecial cscheme) . write p
      TokTypeParam -> withColor p (colorTypeParam cscheme) . write p
      TokModule mid  -> withColor p (colorModule cscheme) . write p
      TokCons _    -> withColor p (colorCons cscheme) . write p
      TokNumber   -> withColor p (colorNumber cscheme) . write p
      TokString   -> withColor p (colorString cscheme) . write p
      TokSpecial  -> withColor p (colorSpecial cscheme) . write p
      TokTypeKeyword -> \s -> withColor p (if (not (isKeywordOp s)) then colorTypeKeyword cscheme else colorTypeKeywordOp cscheme) (write p s)
      TokKeyword  -> withColor p (colorKeyword cscheme) . write p 
      TokComment  -> withColor p (colorComment cscheme) . write p 
      TokRichComment cs -> -- \s -> mapM_ (fmtComment (withColor p (colorComment cscheme) . write p)) (cs) -- sequence_ (commentFlatten (withColor p (colorComment cscheme) . write p) cs) -- withColor p (colorComment cscheme) . write p
                           withColor p (colorComment cscheme) . write p
      TokWhite    -> write p
      TokError    -> const (return ())
  where
    fmtComment def com
      = case com of
          ComText s     -> withColor p (colorComment cscheme) $ write p s
          ComEmph s     -> withColor p (colorComment cscheme) $ write p s
          ComPre s      -> withColor p (colorSource cscheme)  $ write p s
          ComPreBlock s -> withColor p (colorSource cscheme)  $ write p (block s)
          ComUrl url    -> withColor p (colorSource cscheme)  $ write p url
          ComLine s     -> withColor p (colorComment cscheme) $ write p s
          ComCode lexs s -> fmtLexs CtxNormal lexs
          ComCodeBlock lexs s -> do{ write p "\n"; fmtLexs CtxNormal lexs; write p "\n" }
          ComCodeLit lexs s   -> do{ write p "\n"; fmtLexs CtxNormal lexs; write p "\n" }
          ComPar        -> return ()
          ComIndent n   -> write p (replicate n ' ')
      where
        block s = "\n" ++ s ++ "\n"

        fmtLexs ctx lexs = sequence_ $ highlightLexemes id (fmtPrint cscheme p) ctx [] lexs
          


showLexeme :: Lexeme -> String
showLexeme (Lexeme _ lex)
  = case lex of
      LexInt _ _    -> show lex
      LexFloat _ _  -> show lex
      LexString s   -> show s
      LexChar c     -> show c
      LexId id      -> show id
      LexIdOp id    -> (if (isQualified id) then show (qualifier id) ++ "/" else "") ++ "(" ++ show (unqualify id) ++ ")"
      LexOp id      -> show id
      LexPrefix id  -> show id
      LexWildCard id-> show id
      LexModule id _ -> show id
      LexCons id    -> show id
      LexTypedId id tp -> show id
      LexKeyword k _ -> normalize k
      LexSpecial s  -> normalize s
      LexComment s  -> s
      LexWhite w    -> w
      LexInsLCurly  -> ""
      LexInsRCurly  -> ""
      LexInsSemi    -> ""
      LexError msg  -> ""

normalize :: String -> String
normalize s 
  = take 1 s ++ takeWhile (/='.') (drop 1 s)

isKeywordOp :: String -> Bool
isKeywordOp s
  = case s of
      "="    -> False
      (c:cs) -> not (isAlpha c)
      ""     -> False

data Context
  = CtxType [Nesting] String
  | CtxNormal
  deriving (Eq,Ord)

data Nesting
  = NestParen
  | NestBracket
  | NestAngle
  deriving (Eq,Ord)

data Token a
  = TokId Name String
  | TokOp Name String
  | TokSpecial
  | TokKeyword

  | TokTypeVar 
  | TokTypeId Name
  | TokTypeOp Name
  | TokTypeSpecial
  | TokTypeKeyword
  | TokTypeParam

  | TokModule Name
  | TokCons Name
  | TokNumber
  | TokString
  | TokComment 
  | TokRichComment [TokenComment a]
  | TokWhite
  | TokError
  deriving (Eq,Ord)

data TokenComment a
  = ComText String
  | ComEmph String
  | ComPre String
  | ComPreBlock String
  | ComUrl String 
  | ComLine String
  | ComCode [a] String
  | ComCodeBlock [a] String
  | ComCodeLit   [a] String
  | ComPar
  | ComIndent !Int
  deriving (Eq,Ord)

commentFlatten :: (String -> a) -> [TokenComment a] -> [a]
commentFlatten f coms
  = concatMap flatten coms
  where
    flatten com
      = case com of
          ComText x   -> [f x]
          ComEmph x   -> [f x]
          ComPre x    -> [f x]
          ComPreBlock x -> [f x]
          ComUrl url  -> [f url]
          ComLine s   -> [f s]
          ComCode xs s -> xs
          ComCodeBlock xs s -> xs
          ComCodeLit xs s -> xs
          ComPar -> []
          ComIndent n -> [f (replicate n ' ')]


isCtxType (CtxType _ _) = True
isCtxType _             = False

ctxNesting (CtxType nest _) = length nest
ctxNesting _                = 0

highlight :: (Token Lexeme -> String -> a) -> ([Lexeme] -> [Lexeme]) -> Context -> FilePath -> Int -> BString -> [a]
highlight fmt transform ctx sourceName lineNo input
  = let xs = lexer sourceName lineNo input 
    in highlightLexemes transform fmt ctx [] (transform (combineLineComments xs))


highlightLexemes :: ([Lexeme] -> [Lexeme]) -> (Token Lexeme -> String -> a) -> Context -> [a] -> [Lexeme] -> [a]
highlightLexemes transform fmt ctx acc []
  = reverse acc
highlightLexemes transform fmt ctx acc (l:ls)
  = let (ctx', content)  = highlightLexeme transform fmt ctx l ls
    in highlightLexemes transform fmt ctx' (content : acc) ls

highlightLexeme :: ([Lexeme] -> [Lexeme]) -> (Token Lexeme -> String -> a) -> Context -> Lexeme -> [Lexeme] -> (Context,a)
highlightLexeme transform fmt ctx0 (Lexeme rng lex) lexs
  = (ctx,con)
  where
    ctx = adjustContext ctx0 lex lexs
    con = case lex of
            LexId id      -> let tok = if (isCtxType ctx)
                                        then (case dropWhile lexemeIsWhite lexs of
                                                (Lexeme _ (LexKeyword ":" _) : _) | ctxNesting ctx > 0 -> TokTypeParam
                                                _  -> if isTypeVar id then TokTypeVar else TokTypeId id)
                                        else TokId id ""
                             in fmt tok (showId (unqualify id))
            LexWildCard id-> fmt (if (isCtxType ctx) then TokTypeVar else TokId id "") (show id)
            LexOp id      -> fmt (if (isCtxType ctx) then (if (show id) `elem` ["<",">","|","::"] then TokTypeSpecial else TokTypeOp id) 
                                                     else TokOp id "") 
                                 (showOp (unqualify id))
            LexPrefix id  -> fmt (TokOp id "") (showId (unqualify id))
            LexIdOp id    -> fmt (TokOp id "") (showId (unqualify id))
            LexInt _ _    -> fmt TokNumber (show lex)
            LexFloat _ _  -> fmt TokNumber (show lex)
            LexString s   -> fmt TokString (show s)
            LexChar c     -> fmt TokString (show c)
            
            LexModule id mid  -> fmt (TokModule mid) (show id)
            LexCons id        -> fmt (if (isCtxType ctx) then TokTypeId id else TokCons id) (showId (unqualify id))
            LexTypedId id tp  -> -- trace ("**fmt type id: " ++ show id ++ ": " ++ show tp) $
                                 fmt (TokId id tp) (showId (unqualify id))

            LexKeyword ":" _ -> fmt TokTypeKeyword ":"
            LexKeyword k _-> fmt (if (isCtxType ctx) then TokTypeKeyword else TokKeyword) (normalize k)
            LexSpecial s  -> fmt (if (isCtxType ctx) then TokTypeSpecial else TokSpecial) (normalize s)
            LexComment s  -> fmt (TokRichComment ({- map highlightComment -} (lexComment (sourceName (rangeSource rng)) (posLine (rangeStart rng)) s))) s
            LexWhite w    -> fmt TokWhite w
            LexInsLCurly  -> fmt TokWhite ""
            LexInsRCurly  -> fmt TokWhite ""
            LexInsSemi    -> fmt TokWhite ""
            LexError msg  -> fmt TokError msg

    showId :: Name -> String
    showId name
      = if (nameId name == "!" || nameId name == "~") then show name
        else case nameId name of
              (c:cs)  | not (isAlphaNum c || c == '_' || c == '(') -> "(" ++ show name ++ ")"
              _       -> show name

    showOp :: Name -> String
    showOp name
      = case nameId name of
          (c:cs)  | isAlphaNum c  -> "`" ++ show name ++ "`"
          _       -> show name

    -- highlightComment :: TokenComment Lexeme -> TokenComment a
    highlightComment com
      = case com of
          ComText s         -> ComText s
          ComEmph s         -> ComEmph s
          ComUrl s          -> ComUrl s
          ComPre  s         -> ComPre s
          ComPreBlock s     -> ComPreBlock s
          ComLine s         -> ComLine s
          ComCode lexs s      -> ComCode (highlightLexemes transform fmt CtxNormal [] (transform lexs)) s
          ComCodeBlock lexs s -> ComCodeBlock (highlightLexemes transform fmt CtxNormal [] (transform lexs)) s
          ComCodeLit lexs s   -> ComCodeLit (highlightLexemes transform fmt CtxNormal [] (transform lexs)) s
          ComPar            -> ComPar
          ComIndent n       -> ComIndent n

adjustContext ctx lex lexs
  = case ctx of
      CtxNormal 
        -> case lex of
             LexOp op             | show op == "::" -> CtxType [] "::"
             LexKeyword ":" _       -> CtxType [] ":"
             LexKeyword "type" _    -> CtxType [] "type"
             LexKeyword "cotype" _  -> CtxType [] "cotype"
             LexKeyword "rectype" _ -> CtxType [] "rectype"
             LexKeyword "alias" _   -> CtxType [] "alias"
             LexKeyword "effect" _  -> CtxType [] "effect"
             LexKeyword "struct" _  -> case dropWhile lexemeIsWhite lexs of
                                         (Lexeme _ (LexSpecial "("):_) -> CtxType [] "struct-tuple"
                                         _ -> CtxType [] "struct"
             _                      -> CtxNormal

      CtxType nest decl
        -> case lex of
             LexId _            -> ctx
             LexCons id         -> ctx
             LexOp op           | show op == "<" -> push NestAngle nest
                                | show op == ">" -> pop NestAngle nest
                                | otherwise      -> ctx

             LexWildCard _      -> ctx
             LexWhite _         -> ctx
             LexComment _       -> ctx

             LexKeyword "." _      -> ctx
             LexKeyword ":" _      -> ctx
             LexKeyword "->" _     -> ctx
             LexKeyword "with" _   -> ctx
             LexKeyword "forall" _ -> ctx
             LexKeyword "some" _   -> ctx
             LexKeyword "exists" _ -> ctx

             LexKeyword "=" _   | decl == "alias" -> ctx
             LexSpecial ","     | not (null nest) || decl == "struct-tuple" -> ctx

             LexSpecial "("     | decl == "struct-tuple" -> CtxType (NestParen:nest) "struct"
                                | decl /= "struct"       -> push NestParen nest
             LexSpecial "["     -> push NestBracket nest

             LexSpecial ")"     -> pop NestParen nest
             LexSpecial "]"     -> pop NestBracket nest

             _                  -> adjustContext CtxNormal lex lexs
        where
          push n nest
            = CtxType (n:nest) decl

          pop n nest
            = case nest of
                []      -> CtxNormal
                (m:ms)  | n == m    -> CtxType ms decl
                        | otherwise -> pop n ms




-----------------------------------------------------------
-- Parse comment formatters
-----------------------------------------------------------


lexComment :: FilePath -> Int -> String -> [TokenComment Lexeme]
lexComment sourceName lineNo content
  = -- trace ("lex comment:\n" ++ content ++ "\n\n")  $
    scan lineNo [] [] (filter (/= '\r') content)  
  where
    -- Top level
    scan :: Int -> [TokenComment Lexeme] -> [Char] -> [Char] -> [TokenComment Lexeme]

    -- skip inside tags (so html renders correctly)
    scan n lacc acc ('<':c:rest)  | not (isSpace c)  = scanTag n (ComText (reverse acc) : lacc) ('<':c:rest)

    -- skip inside $ (so math renders correctly)
    scan n lacc acc ('$':c:rest)  = scanMath n (ComText (reverse acc) : lacc) (c:rest)

    -- code
    scan n lacc acc ('`':c:rest)    
      | c /= '`' = scanCode n ComCode (ComText (reverse acc) : lacc) "" (c:rest)                                   
      | c == ':' = scanCode n ComCode (ComText (reverse (acc)) : lacc) ":" rest      
    scan n lacc acc ('`':'`':'`':c:rest) | whiteLine acc && c /= '`'
      = let (pre,post) = span (/='\n') (c:rest)
            comCode = if (pre == "unchecked") then ComCodeBlock else ComCodeLit
        in if (pre=="unchecked" || pre=="koka" || pre=="")
            then scanCodeBlock (n+1) comCode (n+1) (ComText (reverse (dropLine acc)) : lacc) [] (dropLine post)
            else scanPreBlock 3 n (ComText (reverse ("```" ++ pre ++ acc)) : lacc) [] post
    scan n lacc acc ('`':rest)       
      = let (pre,post) = span (=='`') rest 
            lacc' = ComText (reverse ('`':pre ++ acc)) : lacc
        in if (whiteLine acc && length pre >= 2) 
            then scanPreBlock (length pre + 1) n lacc' [] post
            else scanPre (length pre + 1) n lacc' [] post
    
    -- regular
    scan n lacc acc ('\n':rest)     = scan (n+1) lacc ('\n':acc) rest
    scan n lacc acc (c:rest)        = scan n lacc (c:acc) rest
    scan n lacc acc []              = reverse (ComText (reverse acc) : lacc)
    
    -- scanTag: ignore things inside "<tag...>" and "<script .. </script>" or "<style .. </style>" tags
    scanTag n lacc ('<':rest)       | (tag == "script" || tag == "style") = skipToEndTag 
                                    where
                                      (tagName,rest2) = span isAlphaNum rest
                                      tag = map toLower tagName

                                      skipToEndTag = let (elem,after) = spanToEndTag [] ('<':rest)
                                                         m            = length (filter (=='\n') elem)
                                                     in scan (n+m) (ComText elem : lacc) [] after

                                      spanToEndTag acc ('<':'/':cs) = let (name,ds)   = span isAlphaNum cs
                                                                          (tagend,es) = span (\c -> c `elem` " \t>") ds
                                                                      in if (map toLower name == tag)
                                                                          then (concat (reverse (('<':'/':name ++ tagend):acc)),es)
                                                                          else spanToEndTag ("</":acc) cs
                                      spanToEndTag acc ('<':cs)     = spanToEndTag ("<":acc) cs
                                      spanToEndTag acc []           = (concat (reverse acc), [])
                                      spanToEndTag acc cs           = let (pre,post) = span (/='<') cs
                                                                      in spanToEndTag (pre:acc) post                                                                          

    scanTag n lacc content          = let (tag,close) = span (\c -> not (c `elem` ">\n")) content
                                          (end,rest)  = if null close then ([],[]) else ([head close], tail close)
                                      in scan (if ('\n' `elem` end) then (n+1) else n) (ComText (tag ++ end) : lacc) [] rest


    scanMath n lacc content         = let (math,close) = span (\c -> not (c `elem` "$\n")) content
                                          (end,rest)   = if null close then ([],[]) else ([head close], tail close)
                                      in scan (if ('\n' `elem` end) then (n+1) else n) (ComText ("$" ++ math ++ end) : lacc) [] rest
    
    -- scanPre formatted ``pre``
    scanPre m n lacc acc ('`':rest)  
      = let (pre,post) = span (=='`') rest
        in if (length pre+1 == m)
            then scan n lacc ('`':pre ++ acc) post
            else scanPre m n lacc ('`':acc) rest
    scanPre m n lacc acc ('\n':rest)     = scan n lacc acc ('\n':rest) -- don't go through newlines
    scanPre m n lacc acc (c:rest)        = scanPre m n lacc (c:acc) rest
    scanPre m n lacc acc []              = scan n lacc acc []

    -- scanCode "f(x)"
    scanCode n com lacc acc ('`':rest)      = endCode n com lacc acc "" rest
    scanCode n com lacc acc ('\n':rest)     = endCode n com lacc acc "\n" rest
    scanCode n com lacc acc (c:rest)        = scanCode n com lacc (c:acc) rest
    scanCode n com lacc acc []              = endCode n com lacc acc "" []

    endCode n com lacc acc post rest = let lexemes = lexer sourceName n (stringToBString $ reverse (dropWhile (==' ') acc)) 
                                       in scan n (com (lexemes) (reverse acc) : lacc) (reverse post) rest

    -- pre block ```
    scanPreBlock m n lacc acc ('`':rest) 
      = let (pre,post) = span (=='`') rest
        in if (m == length pre + 1) 
            then scan n lacc ('`':pre ++ acc) post
            else scanPreBlock m n lacc ('`':acc) rest
    scanPreBlock m n lacc acc (c:rest)        = scanPreBlock m (if (c=='\n') then n+1 else n) lacc (c:acc) rest
    scanPreBlock m n lacc acc []              = scan n lacc acc []


    -- code block
    scanCodeBlock n com m lacc acc ('/':'/':'/':'/':rest) | (onLine acc rest)
                                                = scanCodeBlock2 (n+1) ComCodeLit (n+1) lacc (reverse (acc)) "" (dropLine rest)
    scanCodeBlock n com m lacc acc ('`':'`':'`':rest)   | onLine acc rest
                                                = endCodeBlock (n+1) com m lacc "" acc (dropLine rest)
    scanCodeBlock n com m lacc acc (c:rest)     = scanCodeBlock (if (c=='\n') then n+1 else n) com m lacc (c:acc) rest
    scanCodeBlock n com m lacc acc []           = endCodeBlock n com m lacc (reverse acc) "" []

    scanCodeBlock2 n com m lacc pre acc ('`':'`':'`':rest)  | (onLine acc rest)
                                                = endCodeBlock (n+1) com m lacc pre acc (dropLine rest)
    scanCodeBlock2 n com m lacc pre acc (c:rest)= scanCodeBlock2 (if (c=='\n') then n+1 else n) com m lacc pre (c:acc) rest
    scanCodeBlock2 n com m lacc pre acc []      = endCodeBlock n com m lacc pre acc []

    endCodeBlock n com m lacc pre acc rest      = let src     = dropLine (reverse acc)
                                                      lexemes = lexer sourceName m (stringToBString src) 
                                                  in -- trace("code block:\n" ++ src ++ "\n\n") $
                                                     scan n (com (lexemes) (if null pre then src else (pre ++ "\n" ++ src)) : lacc) [] rest


    onLine pre post
      = (whiteLine pre && whiteLine post)

    whiteLine s 
      = case (dropWhile (\c -> c `elem` " \t\r") s) of
          (c:_) -> (c == '\n')
          []    -> True
        
    dropLine s
      = case (dropWhile (\c -> c `elem` " \t\r") s) of
          ('\n':cs) -> cs
          cs        -> cs


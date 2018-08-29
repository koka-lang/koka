------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

{-
  Syntax coloring for source files and code inside comments (to HTML)
-}
-----------------------------------------------------------------------------
module Syntax.Colorize( colorize
                       -- * helpers for Core.GenDoc
                      , tag
                      , span, cspan
                      , escapes, escape
                      , prefix, htmlHeader, htmlFooter
                      , signature, linkModule
                      , popup
                      , fmtHtml
                      , fmtName, fmtTypeName
                      , fmtNameString
                      , linkFromModName
                      , linkEncode
                      , fmtLiterate, showDoc
                      , capitalize, endWithDot
                      , removeComment
                      , kindSignature
                      ) where

import Lib.Trace
import Prelude hiding (span)
import qualified Prelude
import Data.Char( isAlphaNum, isSpace, toUpper )
import Lib.Printer
import Common.File  
import Common.Name
import Common.NamePrim  ( nameSystemCore )
import Common.Range
import Common.QNameMap 
import Syntax.Lexeme   ( Lexeme(..), Lex(..), lexemeIsWhite )
import Syntax.Lexer    ( lexer )
import Syntax.Layout   ( combineLineComments )
import Syntax.Highlight

import Kind.Kind
import Kind.Pretty
import Type.Pretty
import Syntax.RangeMap

import Platform.Config( programName, sourceExtension )

import Kind.Assumption
import Type.Assumption
import Core.Core (canonicalSplit)

-----------------------------------------------------------
-- Advanced syntax highlighting to HTML
-----------------------------------------------------------
-- | Print source in color, given a color scheme, source name, initial line number, the input string, and
-- a 'Printer'.
colorize :: Printer p => Maybe RangeMap -> Env -> KGamma -> Gamma -> Bool -> FilePath -> Int -> BString -> p -> IO ()
colorize mbRangeMap env kgamma gamma fullHtml sourceName lineNo input p  
  | extname sourceName == ".md" && extname (notext sourceName) == sourceExtension -- ".kk.md"
  = let coms = lexComment sourceName lineNo (bstringToString input)
    in mapM_ (write p . fmtComment (fmap rangeMapSort mbRangeMap) env kgamma gamma) coms

colorize mbRangeMap env kgamma gamma fullHtml sourceName lineNo input p  | otherwise    
  = htmlBody $ htmlPre $
    do let xs = lexer sourceName lineNo input   
           lexs = combineLineComments xs
       case mbRangeMap of
         Nothing -> mapM_ (write p) (highlightLexemes id fmtHtml CtxNormal [] lexs)
         Just rm -> mapM_ (write p) $ colorizeLexemes False fmtHtml (rangeMapSort rm) env [] CtxNormal lexs       
 
  where
    htmlBody pre
      = if not fullHtml then pre
        else do mapM_ (writeLn p) (htmlHeader env (concatMap escape (notdir sourceName)))
                pre
                mapM_ (writeLn p) htmlFooter
        
    htmlPre body
      = do write p ("<pre class=\"" ++ prefix ++ "source\">")
           body
           writeLn p ("</pre>\n")  -- add empty line for correct markdown

htmlHeader env title
  = ["<!DOCTYPE html>"
    ,"<html>"
    ,"<!-- NO_CLICK_TRACKING -->" -- for MS website
    ,""
    ,"<head>"
    ,"<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />"
    ,""
    ,"<style type=\"text/css\">.koka .plaincode, .koka a.pp .pc { display: none; } .koka a.pp { color: inherit; text-decoration: none; }</style>"
    , unlines (map linkCss (undelimPaths (htmlCss env)))
    ,"<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fonts.googleapis.com/css?family=Noto+Serif:400,400italic,700,700italic\" />"
    ,"<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fonts.googleapis.com/css?family=Roboto+Mono:400,500,700,400italic\" />"
    ,if (null (htmlJs env)) then "" 
      else if (extname (htmlJs env) == "require") 
       then "<script type=\"text/javascript\" data-main=\"" ++ basename (htmlJs env) ++ "\" src=\"" ++ dirname (htmlJs env) ++ "require.js\"></script>"
       else "<script type=\"text/javascript\" data-main=\"" ++ basename (htmlJs env) ++ "\" src=\"" ++ htmlJs env ++ "\"></script>"
    ,"<title>" ++ title ++ " documentation</title>"
    ,"</head>"
    ,""
    ,"<body class=\"" ++ prefix ++ "doc body\"><div class=\"madoko\">"
    ]
  where
    linkCss cssPath = "<link rel=\"stylesheet\" type=\"text/css\" href=\"" ++ cssPath ++ "\" />"  
  
htmlFooter
  = ["</div></body>"
    ,"</html>"
    ]


---------------------------------------------------------------------------------------------
--
---------------------------------------------------------------------------------------------

colorizeLexemes isLiterate fmtFun rangeMap env rctx ctx lexes
  = scan rangeMap rctx ctx lexes
  where
    scan rangeMap rctx ctx lexes
      = case (rctx,lexes) of
          ([],[])        -> []
          (range:rs, []) -> endTag "span" : scan rangeMap rs ctx lexes
          (range:rs, lex@(Lexeme rng _):ls)  | after range rng
                         -> endTag "span" : scan rangeMap rs ctx lexes
          (_,lex:ls)     -> let (content,ctx',ranges,rangeMap') = colorizeLexeme isLiterate fmtFun rangeMap env ctx lex ls
                            in content : scan rangeMap' (ranges ++ rctx) ctx' ls
        

colorizeLexeme isLiterate fmtFun rangeMap env ctx l@(Lexeme rng lex) ls
  = let (ctx',content) = highlightLexeme id fmtFun ctx l ls
        (ranges,rangeMap',content') = transform isLiterate rng rangeMap env l content 
    in (content',ctx',ranges,rangeMap')


transform :: Bool -> Range -> RangeMap -> Env -> Lexeme -> String -> ([Range], RangeMap, String)
transform isLiterate rng rangeMap env lexeme content
  = let (rinfos,rangeMap') = rangeMapLookup rng rangeMap 
        (ranges,content') = foldl transform1 ([],content) (reverse rinfos)
    in (ranges,rangeMap',content')
  where
    transform1 (ranges,content) (range,rinfo)
      = case rinfo of         
          (Id qname info isdef)
            -> let toLit  = isLiterate 
                            || (isQualified qname && isdef) -- link from definitions sites in source back to documentation (literate code)
                   pcontent = escapes (showLexeme lexeme)
               in
               (ranges,
                case info of
                 NIValue tp     -> signature env toLit isLiterate "type" qname (mangle qname tp) (showType env tp) $
                                     (case lexeme of (Lexeme _ (LexKeyword _ _)) -> cspan "keyword" pcontent  -- for 'return'
                                                     _  -> pcontent)
                 NICon tp       -> signature env toLit isLiterate "type" qname (mangleConName qname) (showType env tp) $ cspan "constructor" pcontent
                 NITypeVar kind -> signature env toLit isLiterate "kind" qname qname (showKind env kind) $ cspan "type typevar" $ spanEffect kind pcontent
                 NITypeCon kind -> signature env toLit isLiterate "kind" qname (mangleTypeName qname) (showKind env kind) $ cspan "type" $ spanEffect kind pcontent
                 NIModule       -> signature env toLit isLiterate "module" qname (qualify qname nameNil) (showModule qname) (cspan "namespace" pcontent)
                 NIKind         -> span "kind" content     
                )
          (Decl s name mname)
             -> (range:ranges, (startTag "span" ("decl-" ++ s ++ "\" id=\"" ++ linkEncode (nameId mname)) ++ content))
          (Block s)
             -> ((range:ranges), (startTag "span" s ++ content))
          (Error doc)
             -> ([range,range] ++ ranges, (startTag "span" ("error") ++ startTag "span" "popup" ++ tag "span" "popup-content" (cspan "keyword" "error: " ++ show doc) ++ content))
          (Warning doc)
             -> ([range,range] ++ ranges, (startTag "span" ("warning") ++ startTag "span" "popup" ++ tag "span" "popup-content" (cspan "keyword" "warning: " ++ show doc) ++ content))
   
    spanEffect kind
      = if (kind == kindLabel || kind == kindEffect)
         then span "effect" 
         else id


    plainText acc ""       = reverse acc
    plainText acc ('<':cs) = plainText acc (drop 1 (dropWhile (/='>') cs))
    plainText acc (c:cs)   = plainText (c:acc) cs

showType env tp
  = concat $ highlight fmtHtml id (CtxType [] ":") "" 1 (compress [] (show (ppType env tp)))
  
showKind env k
  = concat $ highlight fmtHtml id (CtxType [] "::") "" 1 (compress [] (show (prettyKind (colors env) k)))

showModule qname
  = span "module" (fmtName qname)

compress acc []  = stringToBString $ reverse acc
compress acc (c:cs)
  = if (isSpace c)
     then compress (' ':acc) (dropWhile isSpace cs)
     else compress (c:acc) cs
  

fmtHtml :: Token Lexeme -> String -> String
fmtHtml token 
  = case token of
      TokId _ _    -> fmtNameString
      TokOp _ _    -> cspan "operator" . fmtOpString
      TokTypeVar   -> cspan "type typevar" . fmtNameString
      TokTypeId _   -> cspan "type" . fmtNameString
      TokTypeOp _   -> cspan "type operator" . fmtNameString
      TokTypeKeyword   -> \s -> if (not (isKeywordOp s)) then cspan "type keyword" s else cspan "type keyword operator" s
      TokTypeSpecial   -> cspan "type special" . escapes
      TokTypeParam     -> cspan "type typeparam" . fmtNameString
      TokModule mid    -> cspan "namespace" . fmtNameString
      TokCons _         -> cspan "constructor" . fmtNameString
      TokNumber   -> cspan "number"
      TokString   -> cspan "string" . escapes
      TokSpecial  -> escapes
      TokKeyword  -> \s -> if (not (isKeywordOp s)) then cspan "keyword" s else cspan "keyword operator" s
      TokComment  -> cspan "comment" . escapes
      TokWhite    -> id
      TokError    -> escapes
      TokRichComment coms -> \s -> cspan "comment" (escapes s) -- (concatMap fmtComment coms)
{-
fmtComment :: TokenComment String -> String
fmtComment com
  = case com of
      ComText fmt     -> fmt
      ComUrl url      -> "<a href=\"http://" ++ url ++ "\">" ++ escapes url ++ "</a>"
      ComLine s       -> span "comment-line" ""  
      ComEmph fmt     -> span "comment-emph" fmt
      ComPre fmt      -> span "comment-pre" (escapes fmt)      
      ComPreBlock fmt -> span "comment-preblock" (escapes fmt)      
      ComType fmts    -> span "comment-code" (concat fmts)
      ComCode fmts    -> span "comment-code" (concat fmts)
      ComCodeBlock fmts  -> span "comment-codeblock" (concat fmts)
-}


---------------------------------------------------------------------------------------------
--
---------------------------------------------------------------------------------------------

showDoc :: Env -> KGamma -> Gamma -> String -> String
showDoc env kgamma gamma [] = ""
showDoc env kgamma gamma doc
  = -- concat $ showLexemes env kgamma gamma [Lexeme rangeNull (LexComment (removeComment doc))]
    -- trace("showDoc:\n" ++ doc ++ "\n\n" ++ removeComment doc ++ "\n\n") $
    doctag "div" (prefix ++ "comment") $
    doctag "xmp" "" $
    capitalize $ 
    endWithDot $
    concatMap (fmtComment Nothing env kgamma gamma) $
    (lexComment "" 1 (removeComment doc))

showLexemes :: Env -> KGamma -> Gamma -> [Lexeme] -> [String]
showLexemes env kgamma gamma lexs
  = highlightLexemes fmtQualify (fmtLiterate Nothing env kgamma gamma) CtxNormal [] (fmtQualify lexs)
  where
    -- type identifier
    fmtQualify [Lexeme r0 (LexKeyword ":" doc), Lexeme r1 (LexId id)]
      = [Lexeme r0 (LexKeyword ":" doc), Lexeme r1 (tryQualifyType LexId id)]
    fmtQualify [Lexeme r0 (LexKeyword ":" doc), Lexeme r1 (LexOp id)]
      = [Lexeme r0 (LexKeyword ":" doc), Lexeme r1 (tryQualifyType LexOp id)]    
    fmtQualify [Lexeme r0 (LexKeyword ":" doc), Lexeme r1 (LexSpecial "("), Lexeme r2 (LexSpecial ")")]
      = [Lexeme r0 (LexKeyword ":" doc), Lexeme r1 (LexId (qualify nameSystemCore (newName "()")))]    

    -- module identifier
    fmtQualify [l0@(Lexeme r0 (LexKeyword "module" doc)), l1@(Lexeme r1 (LexWhite s)), Lexeme r2 (LexId id)]
      = [l0,l1,Lexeme r2 (LexModule id id)]

    -- single typed identifier
    fmtQualify (Lexeme r1 (LexId id) : Lexeme r2 (LexKeyword ":" doc) : lexs)
      = [Lexeme r1 (LexTypedId id (concatMap showLexeme lexs))] -- : Lexeme r2 (LexKeyword ":" doc) : lexs

    fmtQualify lexs
      = map fmtQualifyId lexs

    -- single identifier
    fmtQualifyId (Lexeme r1 (LexId id))
      = Lexeme r1 (tryQualify LexId id)
    fmtQualifyId (Lexeme r1 (LexOp id))
      = Lexeme r1 (tryQualify LexOp id)
    fmtQualifyId lex
      = lex
      
   

    tryQualify lex name
      = case gammaLookup name gamma of
          [(qname,InfoCon{})]    | nameCaseEqual (unqualify name) (unqualify qname) -> LexCons qname
          [(qname,_)]            | nameCaseEqual (unqualify name) (unqualify qname) -> lex qname
          _  -> if (isQualified name)
                 then case gammaLookup (unsplitModuleName [name]) gamma of
                        [(_,InfoImport{infoFullName=qname})] -> LexModule name qname
                        _ -> lex name
                 else lex name

    tryQualifyType lex name
      = case kgammaLookup ctx name kgamma of
          Found qname kind -> lex qname
          _                -> lex name

    ctx = context env


fmtLiterate :: Maybe RangeMap -> Env -> KGamma -> Gamma -> Token Lexeme -> String -> String
fmtLiterate mbRangeMap env kgamma gamma token s
  = let fmt = fmtHtml token s        
    in case token of
         TokId qid tp | isQualified  qid || not (null tp)
                   -> linkFromId env qid tp gamma                      
         TokOp qid tp | isQualified qid  || not (null tp)
                   -> linkFromId env qid tp gamma                      
         TokTypeId id
                   -> linkFromTypeId env id kgamma fmt
         TokTypeOp id 
                   -> linkFromTypeId env id kgamma fmt
         TokModule mid
                   -> atag (linkFromModName env mid "") fmt
         TokCons qid
                   -> -- atag (linkFromConName env qid) fmt
                      linkFromId env qid "" gamma   
         _ -> fmt

linkFromId :: Env -> Name -> String -> Gamma -> String
linkFromId env name tp gamma
  = -- trace ("linkFromId: " ++ show name) $
    case gammaLookup name gamma of
      [(qname,InfoImport{})] -> signature env True True "module" qname (qualify qname nameNil) (showModule qname) $ cspan "namespace" $ fmtName (unqualify qname) -- atag (linkFromModName env qname) $ span "module" $ span "id" $ fmtName (unqualify qname)
      [(qname,info@InfoCon{})]-> signature env True True "type" qname (mangleConName qname) (showType env (infoType info)) $ cspan "constructor" $ fmtName (unqualify qname) -- atag (linkFromConName env qname) $ span "con" $ span "id" $ fmtName (unqualify qname)
      [(qname,info)]         -> signature env True True "type" qname (mangle qname (infoType info)) (showType env (infoType info)) $ fmtName (unqualify qname)
      results -> let filtered = if null tp then results
                                           else -- trace ("\n***linkFromId: " ++ show (name,tp) ++ ": " ++ show (map (show . ppType defaultEnv . infoType . snd) results)) $
                                                filter (\(qname,info) -> show (ppType defaultEnv (infoType info)) == tp) results
                 in case filtered of
                      [(qname,info)] -> -- atag (linkFromName env qname (infoType info)) $ span "id" $ fmtName (unqualify qname)
                                        signature env True True "type" qname (mangle qname (infoType info)) (showType env (infoType info)) $ fmtName (unqualify qname) 
                      _ -> if (isQualified name) 
                            then atag (linkFromIdName env name)
                                  ((if (isConstructorName name) then cspan "constructor" else id) (fmtName name))
                            else (if (isConstructorName name) then cspan "constructor" else id) (fmtName (unqualify name))

fmtTypeName :: Name -> String
fmtTypeName name
  = fmtNameString (show name)

fmtName :: Name -> String
fmtName name
  = let (pname,postfix) = canonicalSplit name
        pre = case nameId pname of
              (c:cs)  | not (isAlphaNum c || c == '_' || c == '[' || c=='(') -> "(" ++ fmtNameString (show pname) ++ ")"
              _       -> fmtNameString (show pname)
        post = if null postfix then "" else cspan "postfix" postfix
    in pre++post

fmtNameString :: String -> String
fmtNameString s
  = concatMap showChar (escapes s)
  where
    showChar '-'  = cspan "dash" "-"  -- non-breaking hyphen
    showChar '_'  = cspan "underscore" "_"
    showChar '/'  = cspan "fslash" "/"  
    showChar c    = [c]

fmtOpString :: String -> String
fmtOpString "/"
  = cspan "divide" "/"
fmtOpString "-"
  = cspan "minus" "-"
fmtOpString s
  = escapes s

linkFromTypeId :: Env -> Name -> KGamma -> String -> String
linkFromTypeId env name kgamma content
  = case kgammaLookup (context env) name kgamma of
      Found qname kind
        -> -- atag (linkFromTypeName env qname) content
           signature env True True "kind" qname (mangleTypeName qname) (showKind env kind) $ content
      _ -> if isQualified name
            then atag (linkFromTypeName env name) content
            else content


linkFromIdName env qname
  = linkFromTypeNameX env (if (isConstructorName qname) then mangleConName qname else qname)

linkFromConName env qname
  = linkFromTypeNameX env (mangleConName qname)

linkFromModName env qname postfix
  = linkBaseX env (show qname) postfix

linkFromTypeName env qname
  = linkFromTypeNameX env (mangleTypeName qname)

linkFromTypeNameX env qname
  = linkBase env (nameModule qname) ++ "#" ++ linkEncode (nameId qname)


-- | Encode a link string with browser safe codes (ie. ' ' to '%20')
linkEncode s
  = asciiEncode False s 


fmtComment :: Maybe RangeMap -> Env -> KGamma -> Gamma -> TokenComment Lexeme -> String
fmtComment mbRangeMap env kgamma gamma com
  = case com of
      ComText fmt     -> fmt
      ComUrl url      -> "<a href=\"" ++ url ++ "\">" ++ escapes url ++ "</a>"
      ComLine s       -> prefixspan "line" ""  
      ComEmph fmt     -> prefixspan "emph" fmt
      ComPre fmt      -> prefixspan "pre" (escapes fmt)      
      ComPreBlock fmt -> prefixBlockTag "pre" "preblock" (escapes fmt)      
      ComCode lexs s   -> ptag "code" prefix (fmtLexs lexs)
      ComCodeBlock lexs s -> prefixBlockTag "pre" ("source unchecked") (span "plaincode" (escapes s) ++ span "nicecode" (fmtLexs lexs))
      ComCodeLit lexs s   -> prefixBlockTag "pre" ("source") (span "plaincode" (escapes s) ++ span "nicecode" (fmtLitLexs lexs))
      ComIndent n     -> concat (replicate n "&nbsp;")
      ComPar          -> "<br>"
  where
    fmtLexs lexs = concat $ dropColon lexs $ showLexemes env kgamma gamma lexs
    fmtLitLexs lexs = case mbRangeMap of
                        Nothing -> fmtLexs lexs
                        Just rm -> concat $ colorizeLexemes True (fmtLiterate mbRangeMap env kgamma gamma) rm env [] CtxNormal lexs

    dropColon (l:ls) (fmt:fmts) | ignore = drop (length (takeWhile lexemeIsWhite ls)) fmts    
                                where ignore = case l of
                                                 Lexeme _ (LexKeyword ":" _)      -> True
                                                 Lexeme _ (LexKeyword "module" _) -> True
                                                 _ -> False
    dropColon _      fmts       = fmts


removeCommentOpenClose lex
  = case lex of
      Lexeme r (LexComment s) -> Lexeme r (LexComment (removeComment s))
      _ -> lex

removeComment s
  = case dropWhile isSpace s of
      ('/':'/':_)  -> align $ removeLineComments s
      ('/':'*':cs) -> align $ removeBlockComment cs
      _            -> s
  where
    removeLineComments s 
      = unlines (map removeLineComment (lines s))
    removeLineComment line
      = let (pre,post) = Prelude.span isSpace line
        in case post of
          ('/':'/':cs)  -> pre ++ "  " ++ cs
          _ -> line
         
    removeBlockComment s
      = case reverse s of
          ('/':'*':cs) -> reverse cs  -- (dropWhile isSpace cs) 
          _ -> s

    align s
      = unlines (alignLines (lines s))
    alignLines ls
      = let n = minimum (0:(map (length . takeWhile isSpace) (filter (not . null . dropWhile isSpace) ls)))
        in map (drop n) ls


atag link content
  = doctag "a" ("link\" href=\"" ++ link) content

doctag t cls
  = ptag t (if null cls then "" else ("doc " ++ cls))


      
---------------------------------------------------------------------------------------------
--
---------------------------------------------------------------------------------------------
     
linkModule env mod content
  = tag "a" ("module-link\" href=\"" ++ linkBase env mod ++ "\"") content 


kindSignature :: Env -> Name -> Kind -> String -> String
kindSignature env qname kind content
  = signature env True True "kind" qname qname (showKind env kind) $ content

signature env toLit isLiterate knd qname mname scontent content
  = -- trace ("signature: " ++ show qname ++ " in context " ++ show (context env, isLiterate)) $
    popup linkTo (desc ++ {- span knd -} (scontent)) content
  where
    desc 
      = if (knd == "module") --  qname == newName "return")
         then ""
         else showName ++ ": " 

    showName 
      = if isQualified qname
         then cspan "namespace" (fmtNameString (nameModule qname) ++ cspan "fslash last" "/") ++ (fmtName (unqualify qname))
         else fmtName qname

    linkTo 
      = if isQualified mname
         then -- linkEncode 
              (if (context env == qualifier qname && toLit == isLiterate) 
                    then ""
                    else linkBaseX env (nameModule mname) (if toLit then "" else "-source"))
              ++ (if (nameIsNil mname) then "" else "#" ++ linkEncode (nameId mname))
         else ""

linkBase env modname
  = if (modname == show (context env))
     then ""
     else linkBaseX env modname ""

linkBaseX env modname postfix
  = let prefix = case filter (\(pre,base) -> isPrefix pre modname) (htmlBases env) of
                   ((_,base):_) -> base
                   _            -> ""
    in prefix ++ asciiEncode True modname ++ postfix ++ ".html"
  where
    isPrefix pre s
      = (pre == take (length pre) s)


popup link pop content
  = tag "a" ("pp" ++ if null link then "" else ("\" href=\"" ++ link)) (content ++ tag "span" "pc" pop)

cspan cls content  -- use cspan just for syntax highlighting; to save space we don't output a prefix
  = tag "span" cls content

span cls content
  = tag "span" (cls) content

prefixBlockTag t cls content
  = "\n" ++ prefixtag t cls content ++ "\n" -- add empty line (usually for correct markdown)

prefixtag :: String -> String -> String  -> String
prefixtag t cls content
  = ptag t (if null cls then "" else (prefix ++ cls)) content

prefixspan cls content
  = span (if null cls then "" else (prefix ++ cls)) content

ptag t cls content
  = tag t (if null cls then "" else (cls)) content

tag name cls content
  = startTag name cls ++ content ++ endTag name

startTag name cls  
  = "<" ++ name ++ (if null cls then "" else (" class=\"" ++ shorten cls ++ "\"")) ++ ">" 
endTag name
  = "</" ++ name ++ ">"

writeSpan :: Printer p => p -> String -> String -> IO ()
writeSpan p span content
  = write p ("<span class=\"" ++ prefix ++ span ++ "\">" ++ concatMap escape content ++ "</span>")

escapes s = concatMap escape s
escape '<'  = "&lt;"
escape '>'  = "&gt;"
escape '&'  = "&amp;"
-- for markdown in <code>
escape '*'  = "&#42;"
escape '_'  = "&#95;"
escape '`'  = "&#96;"
escape c    = [c]

prefix :: String
prefix
  = programName ++ " "

-- We shorten class names as it save significantly on the size of 
-- generated HTML. For example. system.core source went from 1.4mb to about 1mb
-- just by shortening (includeing the classes popup and popup-content)
shorten classnames
  = unwords (map shortenWord (words classnames))
  where
    shortenWord s   
      = case (Prelude.lookup s shorthands) of
                Just short -> short
                Nothing    -> s

shorthands :: [(String,String)]
shorthands = [
  ("keyword", "kw"),
  ("operator", "op"),
  ("type", "tp"),
  ("string", "st"),
  ("module", "mo"),
  ("identifier","id"),
  ("namespace", "mo"),
  ("constructor","co"),
  ("typeparam","tpp"),
  ("typevar","tv"),
  ("special","sp"),
  ("delimiter","dl")
 ]                  



capitalize s
  = case s of
      c:cs -> toUpper c : cs
      _    -> s

endWithDot s
  = case dropWhile isSpace (reverse s) of
      (c:cs) | not (c `elem` ".?!") -> reverse ('.':c:cs)
      _      -> s
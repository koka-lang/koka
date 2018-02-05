-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Generate documentation index from core.
-----------------------------------------------------------------------------

module Core.GenDoc( genDoc ) where

import Lib.Trace

import Prelude hiding (span)
import Data.List( sortBy, intersperse, partition )
import Data.Char( isSpace, toUpper)
import Common.File( dirname, extname, basename )
import Lib.Printer
import Common.Name
import Common.Range
import Common.Failure
import Common.Syntax
import Kind.Kind
import Kind.Pretty
import Type.Type
import Type.TypeVar( tvsList, ftv )
import Type.Pretty 

import Kind.Assumption
import Type.Assumption

import Syntax.Lexeme
import Syntax.Highlight
import Syntax.Colorize
import Core.Core
import Syntax.RangeMap( mangle, mangleTypeName, mangleConName )

genDoc :: Printer p => Env -> KGamma -> Gamma -> Core -> p -> IO ()
genDoc env kgamma gamma core p
  | noDeclarations && all (\imp -> importVis imp == Private) (coreProgImports core) && not (null (coreProgImports core))
  = -- trace ("genIndex: " ++ show (coreProgName core, noDeclarations, map importVis (coreProgImports core))) $
    htmlBody $
    do writeLn p $ ptag "h1" "module" $ fmtName (coreProgName core)
       writeLn p $ showDoc env kgamma gamma (coreProgDoc core)
       writeLn p $ fmtImports env kgamma gamma (coreProgImports core)
       
  | otherwise
  = -- trace ("genDoc: " ++ show (coreProgName core, noDeclarations, map importVis (coreProgImports core))) $
    htmlBody $
    do writeLn p $ ptag "h1" "" ( (atag (linkFromModName env (coreProgName core) "-source") $ span "module" $ fmtName (coreProgName core))
                                 ++ (atag "toc.html" (span "toc-link" "&#x25b2;toc")))
       writeLn p $ doctag "div" "toc code" $ concatMap (doctag "ul" "toc") $ filter (not . null) $ map concat tocFmts
       -- writeLn p $ ptag "h2" "" "Description"
       writeLn p $ showDoc env kgamma gamma (coreProgDoc core)
       -- writeLn p $ ptag "h2" "" "Exports"
       mapM_ (writeLn p) (map (fmtPublicImport env kgamma gamma) publicImports)
       mapM_ (writeLn p) (map (fmtTypeDef env kgamma gamma) typeDefsDefs)
       mapM_ (writeLn p) (map (fmtDef env kgamma gamma) otherDefs)
       writeLn p $ fmtPrivateImports env kgamma gamma privateImports
  where
    table []   = ""
    table [x]  = x
    table xs   = doctag "table" "toc" (doctag "tr" "" (concatMap (doctag "td" "") xs) ++ "\n")
      
    tocFmts
      = let fmts = concatMap fmtTypeDefTOC typeDefsDefs 
                   ++ (if (null otherDefs || null typeDefsDefs) then [] else [doctag "li" "" "&nbsp;"]) 
                   ++ map (fmtDefTOC False) otherDefs
            n    = length fmts
            niceRows  = 20  
            niceCol   = 4
            maxRows   = 31
        in if (n <= niceCol*niceRows)
            then if (2*n <= niceRows)
                  then [fmts]
                 else if (n <= niceRows)
                  then split ((n `div` 2) + 1) fmts
                 else if (n <= 2*niceRows)
                  then split ((n `div` 3) + 1) fmts
                  else split niceRows fmts
            else -- split ((n `div` maxCol) + 1) fmts
                 split maxRows fmts
           
      where
        split :: Int -> [a] -> [[a]]
        split n []
          = []
        split n fmts
          = let (pre,post) = splitAt n fmts
            in (pre : split n post)
    
    sizeOf (tdef,ds)
      = length ds +
        case tdef of
          Data info _ _ _ -> length (dataInfoConstrs info)
          _               -> 1


    (publicImports,privateImports) 
      = partition (\imp -> importVis imp == Public) (coreProgImports core)

    (otherDefs,typeDefsDefs)
      = (sortDefs odefs, map (\(td,tds) -> (td,sortDefs tds)) tdefs) 
      where
        (odefs,tdefs) 
          = fold selfResult $ 
            fold selfArg $
            fold selfName $ 
            (defs,map (\tdef -> (tdef,[])) typeDefs)
        
        fold :: (TypeCon -> Def -> Bool) -> ([Def],[(TypeDef,[Def])])  -> ([Def],[(TypeDef,[Def])]) 
        fold f (ds,tdds)
          = foldr (assoc f) (ds,[]) tdds

        assoc :: (TypeCon -> Def -> Bool) -> (TypeDef,[Def]) -> ([Def],[(TypeDef,[Def])]) -> ([Def],[(TypeDef,[Def])])
        assoc f (tdef,ds0) (ds, tdds) 
          = let tcon = typeDefTCon tdef
                (ds1,ds2) = partition (f tcon) ds
            in (ds2, (tdef,ds0++ds1) : tdds)
          
        selfName, selfArg, selfResult :: TypeCon -> Def -> Bool
        selfResult tcon def
          = let (_,_,rho) = splitPredType (defType def)
            in case splitFunType rho of
                 Just (_,_,TCon tc) -> (tc == tcon && TCon tc /= typeUnit)
                 Just (_,_,TApp (TCon tc) _) -> (tc == tcon)
                 _  -> False
        
        selfArg tcon def
          = let (_,_,rho) = splitPredType (defType def)
            in case splitFunType rho of
                 Just ((_,TCon tc):_,_,_)          -> (tc == tcon)
                 Just ((_,TApp (TCon tc) _):_,_,_) -> (tc == tcon)
                 _ -> case rho of -- for constant values
                        TCon tc -> (tc == tcon)
                        TApp (TCon tc) _ -> (tc == tcon)
                        _ -> False

        selfName tcon def
          = ((typeConName tcon) == nonCanonicalName (defName def))

        
        typeDefTCon tdef
          = case tdef of
              (Data info _ _ _) -> TypeCon (dataInfoName info) (dataInfoKind info) -- todo: handle exten
              (Synonym info _)  -> TypeCon (synInfoName info) (synInfoKind info)

    sortDefs ds
      = sortOn (show . defName) ds
                 
    sortOn f xs
      = sortBy (\x y -> compare (f x) (f y)) xs

    noDeclarations
      = null defs && null typeDefs && null externals

    defs 
      = filter (\def -> defVis def == Public && not (isHiddenName (defName def))) $
        concatMap getDef (coreProgDefs core) ++ externals
      where
        getDef (DefRec ds) = ds
        getDef (DefNonRec d) = [d]   

    typeDefs
      = sortOn (show . typeDefName) $
        map filterCon $
        filter (\tdef -> typeDefVis tdef == Public && not (isHiddenName (typeDefName tdef))) $
        concatMap getTDef (coreProgTypeDefs core) 
      where
        getTDef (TypeDefGroup ts) = ts

        filterCon (Data info vis conViss isExtend)
          = Data (info{ dataInfoConstrs = sortOn conInfoName [cons | (v,cons) <- zip conViss (dataInfoConstrs info), v == Public]}) vis conViss isExtend
        filterCon other
          = other


    externals
      = map toDef (coreProgExternals core)
      where
        toDef ext = Def (externalName ext) (externalType ext) (failure "Core.GenDoc.genDoc: access to expression")
                        (externalVis ext) (makeDefFun (externalType ext)) (externalRange ext) (externalDoc ext)

    htmlBody pre
      = do mapM_ (writeLn p) (htmlHeader env (show (coreProgName core)))
           pre
           mapM_ (writeLn p) htmlFooter
        

--------------------------------------------------------------------------
--  Index
--------------------------------------------------------------------------


fmtImports :: Env -> KGamma -> Gamma -> [Import] -> String
fmtImports env kgamma gamma imports 
  = doctag "table" "index" $
    unlines $ fmtImportEntries env kgamma gamma [] $ sorted
  where
    sorted = sortBy (\i1 i2 -> compare (importName i1) (importName i2)) imports
    
fmtImportEntries env kgamma gamma root []
  = []
fmtImportEntries env kgamma gamma root (imp:imps)
  = let (root',entry) = fmtImportEntry env kgamma gamma root imp 
    in entry : fmtImportEntries env kgamma gamma root' imps

fmtImportEntry :: Env -> KGamma -> Gamma -> [String] -> Import -> ([String],String)
fmtImportEntry env kgamma gamma root imp
  = case common [] root (splitImport (importName imp)) of
      Right (rootCommon,name)
        -> (rootCommon ++ [name],fmtImport env kgamma gamma rootCommon name imp) -- span "module" $ atag (linkFromModName env (importName imp)) $ span "id" (show name)
      Left (rootCommon,pre) 
        -> let (root',entry) = fmtImportEntry env kgamma gamma (rootCommon ++ [pre]) imp
           in (root',doctag "tr" "" (doctag "td" "code" (indent (length rootCommon) pre)) ++ "\n" ++ entry)
           
  where
    splitImport name
      = split "" (show name)
      where
        split acc ""          = [reverse acc]
        split acc ('/':cs)    = reverse acc : split "" cs
        split acc (c:cs)      = split (c:acc) cs  

    
    common rootc root parts
      = case (root,parts) of
          (_,[name])      -> Right (rootc,name)
          ([],(pre:post)) -> Left (rootc,pre)
          ((rpre:rpost),(pre:post))
            -> if rpre == pre
                then common (rootc ++ [rpre]) rpost post
                else Left (rootc,pre)
          (_,_) -> matchFailure "Core.GenDoc.fmtImportEntry.common"

fmtImport :: Env -> KGamma -> Gamma -> [String] -> String -> (Import) -> String
fmtImport env kgamma gamma root name (imp)
  = doctag "tr" "" $
    (doctag "td" "code" (indent (length root) $ fmtModuleName env name (importName imp)) ++ 
     doctag "td" "" (synopsis env kgamma gamma (importModDoc imp)))
  where
    fmtModuleName env name qname
      = atag (linkFromModName env qname "") $ span "module" $ limit 15 name
      
synopsis :: Env -> KGamma -> Gamma -> String -> String
synopsis env kgamma gamma doc
  = showDoc env kgamma gamma $
    endWithDot $ capitalize $ 
    fst $ extract "" $ 
    dropWhile isSpace $ 
    removeComment doc
  where
    extract acc s
      = case s of
          ('\n':cs) -> case dropWhile (\c -> c==' '||c=='\t') cs of
                         ('\n':rest) -> (reverse acc,rest)
                         _           -> extract ('\n':acc) cs
          (c:cs)    -> extract (c:acc) cs
          []        -> (reverse acc,"")


indent n s
  = span ("nested" ++ show n) s

      
--------------------------------------------------------------------------
--  TOC
--------------------------------------------------------------------------

fmtTypeDefTOC :: (TypeDef,[Def]) -> [String]
fmtTypeDefTOC  (Synonym info _, defs)
  = [doctag "li" "" $
     (doctag "a" ("link\" href=\"#" ++ linkEncode (nameId (mangleTypeName (synInfoName info)))) $
      cspan "keyword" "alias" ++ "&nbsp;" ++ cspan "type" (niceTypeName (synInfoName info))) ++ "\n"]
    ++
    map (fmtDefTOC True) defs


fmtTypeDefTOC (Data info@DataInfo{ dataInfoSort = Inductive, dataInfoConstrs = [conInfo] } _ conViss isExtend, defs)  | conInfoName conInfo == dataInfoName info
  -- struct
  = [doctag "li" "" $
     (doctag "a" ("link\" href=\"#" ++ linkEncode (nameId (mangleTypeName (dataInfoName info)))) $
      cspan "keyword" "struct" ++ "&nbsp;" ++ cspan "type" (niceTypeName (dataInfoName info))) ++ "\n"]
     ++
    map (fmtDefTOC True) defs

fmtTypeDefTOC (Data info _ conViss isExtend, defs)  -- todo: handle extend
  = [doctag "li" "" $
     (doctag "a" ("link\" href=\"#" ++ linkEncode (nameId (mangleTypeName (dataInfoName info)))) $
      cspan "keyword" (show (dataInfoSort info)) ++ "&nbsp;" ++ span "type" (niceTypeName (dataInfoName info))) ++ "\n"]
    ++ map fmtConTOC constructors 
    ++ map (fmtDefTOC True) defs
  where
    constructors = filter (not . isHiddenName . conInfoName) (dataInfoConstrs info)


subTOC [] = ""
subTOC fmts
  = doctag "ul" "toc" $
    concat fmts


fmtConTOC :: ConInfo -> String
fmtConTOC info
  = doctag "li" "nested" $
    doctag "a" ("link\" href=\"#" ++ linkEncode(nameId (mangleConName (conInfoName info)))) $
    cspan "keyword" "con" ++ "&nbsp;" ++ cspan "constructor" (niceNameId (conInfoName info))

fmtDefTOC :: Bool -> Def -> String
fmtDefTOC nested def
  = doctag "li" (if nested then "nested" else "") $
    doctag "a" ("link\" href=\"#" ++ linkEncode (nameId mname)) $
    cspan "keyword" (showDefSort (defSort def)) ++ "&nbsp;" ++ niceNameId (defName def)
  where
    mname = mangle (defName def) (defType def)
  

--------------------------------------------------------------------------
--  
--------------------------------------------------------------------------

fmtPublicImport :: Env -> KGamma -> Gamma -> Import -> String
fmtPublicImport env kgamma gamma imp 
  = doctag "div" ("decl\" id=\"" ++ linkEncode (nameId (importName imp))) (
    concat 
      [doctag "div" "header code"$ 
        doctag "span" "def" $
          cspan "keyword" "module" ++ "&nbsp;"
          ++ fmtModuleName env qname -- (atag (linkFromModName env qname "") $ span "module" $ show qname)
      , synopsis env kgamma gamma (importModDoc imp)
      ]
    )
  where
    qname = importName imp


fmtPrivateImports :: Env -> KGamma -> Gamma -> [Import] -> String
fmtPrivateImports env kgamma gamma [] = ""
fmtPrivateImports env kgamma gamma imps
  = doctag "div" ("decl\" id=\"-private-imports") (
    concat 
      [doctag "div" "header code"$ 
        doctag "span" "def" $
          cspan "keyword" "private import" ++ "&nbsp;"
          ++ concat (intersperse ", " (map (fmtPrivateImport env kgamma gamma) imps))
      ]
    )
  
fmtPrivateImport :: Env -> KGamma -> Gamma -> Import -> String
fmtPrivateImport env kgamma gamma imp
  = doctag "span" "code" $ fmtModuleName env (importName imp)

fmtModuleName env qname
  = signature env True True "module" qname (qualify qname nameNil) (span "module" (fmtName qname)) $ cspan "namespace" $ fmtName (unqualify qname)
  -- atag (linkFromModName env qname "") $ span "module" qname    

fmtTypeDef :: Env -> KGamma -> Gamma -> (TypeDef,[Def]) -> String
fmtTypeDef env kgamma gamma (Synonym info _, defs)
  = nestedDecl defs $
    doctag "div" ("decl\" id=\"" ++ linkEncode (nameId (mangleTypeName (synInfoName info)))) (
    concat 
      [doctag "div" "header code"$ 
        concat
        [ doctag "span" "def" $
            cspan "keyword" "alias" ++ "&nbsp;"
            ++ cspan "type" (signature env False True "kind" (synInfoName info) (mangleTypeName (synInfoName info)) (showKind env (synInfoKind info)) -- atag (linkToSource (synInfoName info)) 
                              (cspan "type" (niceTypeName (synInfoName info))))
        , cspan "type" (angled fmtTVars)
        , "&nbsp;", cspan "keyword" "=", "&nbsp;"
        , fmtTp
        ]
      ,showDoc env kgamma gamma (synInfoDoc info)
      ]
    ++ fmtDefs env kgamma gamma defs)
   where
      (fmtTp:fmtTVars) = showTypes env kgamma gamma (synInfoType info : map TVar (synInfoParams info))

fmtTypeDef env kgamma gamma (Data info@DataInfo{ dataInfoSort = Inductive, dataInfoConstrs = [conInfo] } _ conViss isExtend, defs)  | conInfoName conInfo == dataInfoName info
  -- struct
  = nestedDecl defs $
    doctag "div" ("decl\" id=\"" ++ linkEncode (nameId (mangleTypeName (dataInfoName info)))) $
    concat 
      [ doctag "div" "header code"$ 
        concat
        [ doctag "span" "def" $
            cspan "keyword" "struct" ++ "&nbsp;"
            ++ cspan "type" (signature env False True "kind" (dataInfoName info) (mangleTypeName (dataInfoName info)) (showKind env (dataInfoKind info)) -- atag (linkToSource (dataInfoName info)) 
                              (cspan "type" (niceTypeName (dataInfoName info))))
        , cspan "type" (angled (fmtTVars))
        , parenthesized (map (showParam env' kgamma gamma) (conInfoParams conInfo))
        ]
      ,showDoc env kgamma gamma (dataInfoDoc info)
      ]
    ++ fmtDefs env kgamma gamma defs
  where
    env' = niceEnv env (dataInfoParams info)
    fmtTVars = map (showType env' kgamma gamma . TVar) (dataInfoParams info)



fmtTypeDef env kgamma gamma (Data info _ conViss isExtend, defs) -- TODO: show extend correctly
  = nestedDecl defs $
    doctag "div" ("decl\" id=\"" ++ linkEncode (nameId (mangleTypeName (dataInfoName info)))) $
    concat 
      [doctag "div" "header code"$ 
        concat
        [ doctag "span" "def" $
            cspan "keyword" dataInfo ++ "&nbsp;" 
            ++ cspan "type" (signature env isExtend True "kind" (dataInfoName info) (mangleTypeName (dataInfoName info)) (showKind env (dataInfoKind info)) -- atag (linkToSource (dataInfoName info)) 
                             (cspan "type" (niceTypeName (dataInfoName info))))
        , cspan "type" $ angled (fmtTVars)
        --, "&nbsp;", span "keyword" ":", "&nbsp;", showKind env' (dataInfoKind info)
        ]
      ,showDoc env kgamma gamma (dataInfoDoc info)
      ,concat (map (fmtConstructor env' kgamma gamma) constructors)
      ]
    ++ fmtDefs env kgamma gamma defs
  where
    env' = niceEnv env (dataInfoParams info)
    fmtTVars = map (showType env' kgamma gamma . TVar) (dataInfoParams info)

    dataInfo = show (dataInfoSort info) ++
               (if (isExtend) then "&nbsp;extend" 
                else if (dataInfoIsOpen info) then "&nbsp;open"
                else "")

    constructors
      = filter (\con -> not (isHiddenName (conInfoName con))) $
        dataInfoConstrs info


fmtDefs env kgamma gamma defs
  = if null defs then ""
    else (doctag "div" "nested" (concat (map (fmtDef env kgamma gamma) defs)))

nestedDecl xs
  = id -- if null xs then id else doctag "div" "nested"

fmtConstructor env kgamma gamma info
  = doctag "div" ("con-decl\" id=\"" ++ linkEncode (nameId (mangleConName (conInfoName info)))) $
    concat 
      [doctag "div" "header code"$ 
        concat
        [ doctag "span" "def" $
           cspan "keyword" "con" ++ "&nbsp;" ++
           doctag "a" ("link\" href=\"" ++ linkToSource (mangleConName (conInfoName info))) (cspan "constructor" (niceNameId (conInfoName info)))
        , angled fmtTVars
        , parenthesized (map (showParam env' kgamma gamma) (conInfoParams info))
        ]
        , showDoc env kgamma gamma (conInfoDoc info)        
      ]
  where
    env' = niceEnv env (conInfoExists info)
    fmtTVars = map (showType env' kgamma gamma . TVar) (conInfoExists info)

showParam env kgamma gamma (name,tp)
  = (if (isFieldName name) then "" else cspan "param" (fmtName name) ++ " " ++ cspan "type special" ":" ++ "&nbsp;")
    ++ showType env kgamma gamma tp

fmtDef :: Env -> KGamma -> Gamma -> Def -> String
fmtDef env kgamma gamma def
  = doctag "div" ("decl\" id=\"" ++ linkEncode (nameId mname)) $
    concat 
      [doctag "div" "header code" $
        concat 
         [ doctag "span" "def" $
            cspan "keyword" (showDefSort (defSort def)) ++ "&nbsp;"
            ++ doctag "a" ("link\" href=\"" ++ linkToSource mname) (niceNameId (defName def))
         -- , "&nbsp;"
         -- , span "keyword" ":"
         -- , "&nbsp;"
         , showDeclType env kgamma gamma (defType def)
         ]
     , showDoc env kgamma gamma (defDoc def)
     ]
    
  where
    mname = mangle (defName def) (defType def)


    showModule qname
      = cspan "namespace" (escapes (show qname))

    spanEffect kind
      = if (kind == kindLabel || kind == kindEffect)
         then cspan "type effect" 
         else id

showDefSort (DefFun kind) = "fun"
showDefSort dsort         = show dsort

niceTypeName name
  = fmtTypeName (unqualify name)

niceNameId name
  = fmtName (nonCanonicalName (unqualify name))

linkToSource mname
  = if isQualified mname
     then map (\c -> if c == '/' then '_' else c) (nameModule mname) ++ "-source.html#" ++ linkEncode (nameId mname)
     else ""

showKind env k
  = concat $ highlight fmtHtml id (CtxType [] ":") "" 1 (compress [] (show (prettyKind (colors env) k)))


showDeclType env kgamma gamma tp
  = let (mbParams,res) = ppDeclType (niceEnv (env{fullNames=True}) (tvsList (ftv tp))) tp
    in case mbParams of
         Nothing -> colon ++ hlType res
         Just params | null params
          -> "()" ++ colon ++ hlType res
         Just params
          ->  "( " ++ concat (intersperse ", " [hlParam name ++ hlType tpdoc  | (name,tpdoc) <- params]) ++ " )" ++ " " ++ colon ++ hlType res
  where
    colon     = cspan "type special" ":" ++ "&nbsp;"
    hlType doc = highlightType env kgamma gamma (show doc)
    hlParam name = if (not (nameIsNil name || isFieldName name)) then cspan "type typeparam" (fmtName name) ++ " " ++ colon  else "" -- (cspan "type special" ":")

showType env kgamma gamma tp
  = head (showTypes env kgamma gamma [tp])

showTypes env kgamma gamma tps
  = map (highlightType env kgamma gamma . show . ppType env') tps 
  where    
    env' = niceEnv (env{ fullNames = True }) (tvsList (ftv tps))


highlightType :: Env -> KGamma -> Gamma -> String -> String
highlightType env kgamma gamma typeStr
  = concat $ 
    highlight (fmtLiterate Nothing env kgamma gamma) lexQualifiers (CtxType [] ":") "" 1 $
    compress [] typeStr
  where    
    lexQualifiers :: [Lexeme] -> [Lexeme]
    lexQualifiers lexs
      = case lexs of
          (Lexeme r1 (LexId id1) : Lexeme _ (LexKeyword "." _) : Lexeme r2 (LexId id2) : ls)
            -> lexQualifiers (Lexeme (combineRange r1 r2) (LexId (newQualified (show id1) (show id2))) : ls)
          (Lexeme r1 (LexId id1) : Lexeme _ (LexOp op) : Lexeme _ (LexSpecial "(") : Lexeme r2 (LexSpecial ")") : ls)
            | show op == "/" -> lexQualifiers (Lexeme (combineRange r1 r2) (LexId (newQualified (show id1) "()")) : ls)
          (Lexeme r1 (LexId id1) : Lexeme _ (LexKeyword "." _) : Lexeme r2 (LexOp id2) : ls)
            -> lexQualifiers (Lexeme (combineRange r1 r2) (LexId (newQualified (show id1) (show id2))) : ls)
          (l:ls)
            -> l : lexQualifiers ls
          []
            -> []

compress acc []  = stringToBString $ reverse acc
compress acc (c:cs)
  = if (isSpace c)
     then case dropWhile isSpace cs of
            (',':ds) -> compress acc (',':ds)
            ds -> compress (' ':acc) ds
     else compress (c:acc) cs  


atag link content
  = doctag "a" ("link\" href=\"" ++ link) content

doctag t cls
  = ptag t (if null cls then "" else (cls))

ptag t cls
  = tag t (if null cls then "" else (cls))

angled []        = ""
angled xs        = "&lt;" ++ concat (intersperse (",") xs) ++ "&gt;"


parenthesized [] = ""
parenthesized xs = "(" ++ concat (intersperse (",&nbsp;") xs) ++ ")"

limit n s
  = if (length s <= n) then fmtNameString s 
     else (fmtNameString (take n s) ++  "&hellip;")
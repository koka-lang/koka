-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Backend.JavaScript.FromCore 
      ( javascriptFromCore )
 where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.List ( intersperse )
import Data.Char
-- import Data.Maybe
-- import Data.Monoid ( mappend )
import qualified Data.Set as S

-- import Kind.Kind
import Type.Type 
-- import Type.TypeVar
-- import Type.Kind( getKind )
-- import Type.Assumption( getArity )
import qualified Type.Pretty as Pretty

import Lib.PPrint                
-- import qualified Lib.PPrint
import Common.Name 
-- import Common.Range
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core  
import Core.Pretty ()

type CommentDoc   = Doc
type ConditionDoc = Doc


debug :: Bool
debug  = False

externalNames :: [(TName, Doc)]
externalNames
  = [ (conName exprTrue,  text "true")
    , (conName exprFalse, text "false")
    , (TName nameOptionalNone typeOptional, text "undefined")  -- ugly but has real performance benefit
    ]

--------------------------------------------------------------------------
-- Generate JavaScript code from System-F core language 
--------------------------------------------------------------------------

javascriptFromCore :: Maybe (Name) -> Core -> Doc
javascriptFromCore mbMain core
  = runAsm (Env moduleName penv externalNames False) (genModule mbMain core)
  where
    moduleName = coreProgName core
    penv       = Pretty.defaultEnv{ Pretty.context = moduleName, Pretty.fullNames = False }

genModule :: Maybe (Name) -> Core -> Asm Doc 
genModule mbMain core
  =  do let externs = vcat (concatMap includeExternal (coreProgExternals core)) 
        decls1 <- genTypeDefs (coreProgTypeDefs core)
        decls2 <- genGroups   (coreProgDefs core) -- (removeTypeLamApp $ coreProgDefs core)
        let imports = map importName (coreProgImports core)
            mainEntry = case mbMain of
                          Nothing -> empty
                          Just (name) -> text " " <-> text "// koka main entry:" <-> 
                                           ppName (unqualify name) <> text "();"
        return $  text "// koka generated module: " <> string (showName (coreProgName core)) 
              <-> text "if (typeof define !== 'function') { var define = require('amdefine')(module) }"
              <-> text "define(" <> ( -- (squotes $ ppModFileName $ coreProgName core) <> comma <-> 
                   list ( {- (squotes $ text "_external"): -} (map squotes (map fst externalImports) ++ map moduleImport (coreProgImports core))) <> comma <+>
                   text "function" <> tupled ( {- (text "_external"): -} (map snd externalImports ++ map ppModName imports)) <+> text "{" <->
                    vcat (
                    [ text "\"use strict\";"
                    , text " "
                    , text "// koka declarations:"
                    , externs 
                    , decls1  
                    , decls2  
                    , mainEntry
                    , text " "
                    , text "// koka exports:"
                    , text "return" <+> encloseSep (text "{ ")
                                                   (text " }")
                                                   (text ", ")
                                                   (map 
                                                     (\n-> squotes (ppName n) <> text ":" <+> ppName n) 
                                                     ( exportedConstrs ++ exportedValues )
                                                   )
                                     <> semi 
                    ])
                 ) 
              <-> text "});"
  where
    exportedValues  = let f (DefRec xs)   = map defName xs
                          f (DefNonRec x) = [defName x]
                      in map unqualify $ concatMap f (coreProgDefs core) 
    exportedConstrs = let f (Synonym _ _)    = []
                          f (Data info _ vs) = let xs = zip vs $ map conInfoName (dataInfoConstrs info)
                                               in  map snd $ filter (\(v,_)-> v == Public) xs
                          u (TypeDefGroup xs) = xs
                      in map unqualify $ concatMap f $ concatMap u (coreProgTypeDefs core)

    externalImports :: [(Doc,Doc)]
    externalImports
      = concatMap importExternal (coreProgExternals core)                      

moduleImport :: Import -> Doc
moduleImport imp
  = squotes (text (if null (importPackage imp) then "." else importPackage imp) <> text "/" <> text (moduleNameToPath  (importName imp)))

includeExternal :: External -> [Doc]
includeExternal (ExternalInclude includes range)
  = let content = case lookup JS includes of
                    Just s -> s
                    Nothing -> case lookup Default includes of
                                 Just s -> s
                                 Nothing -> failure ("javascript backend does not support external inline at " ++ show range)
    in [align $ vcat $! map text (lines content)]
includeExternal _  = []  


importExternal :: External -> [(Doc,Doc)]
importExternal (ExternalImport imports range)
  = let (nm,s) = case lookup JS imports of
                    Just s -> s
                    Nothing -> case lookup Default imports of
                                 Just s -> s
                                 Nothing -> failure ("javascript backend does not support external import at " ++ show range)
    in [(text s,pretty nm)]
importExternal _
  = []                                 

---------------------------------------------------------------------------------
-- Generate javascript statements for value definitions
--------------------------------------------------------------------------------- 

genGroups :: [DefGroup] -> Asm Doc
genGroups groups
  = do docs <- mapM genGroup groups
       return (vcat docs)

genGroup :: DefGroup -> Asm Doc
genGroup group
  = localUnique $
    case group of
      DefRec defs   -> do docs <- mapM genDef defs
                          return (vcat docs)
      DefNonRec def -> genDef def

genDef :: Def -> Asm Doc
genDef def@(Def name tp expr vis sort rng comm)
  = do penv <- getPrettyEnv
       let resDoc = typeComment (Pretty.ppType penv tp)
       defDoc <- do mdoc <- tryFunDef name resDoc expr
                    case mdoc of
                      Just doc -> return doc
                      Nothing  -> genStat (ResultAssign name Nothing) expr
       return $ vcat [ if null comm 
                         then empty 
                         else align (vcat (space : map text (lines (trim comm)))) {- already a valid javascript comment -}
                     , defDoc
                     ]
  where
    -- remove final newlines and whitespace
    trim s = reverse (dropWhile (`elem` " \n\r\t") (reverse s))

tryFunDef :: Name -> CommentDoc -> Expr -> Asm (Maybe Doc)
tryFunDef name comment expr 
  = case expr of
      TypeApp e _   ->             tryFunDef  name comment e 
      TypeLam _ e   ->             tryFunDef  name comment e
      Lam args body -> do inStat <- getInStatement
                          if (inStat)
                           then return Nothing
                           else do fun <- genFunDef' name args comment body 
                                   return (Just fun)
      _             -> return Nothing
  where
    genFunDef' :: Name -> [TName] -> CommentDoc -> Expr -> Asm Doc
    genFunDef' name params comm body
      = do let args = map ( ppName . getName ) params
               isTailCall = body `isTailCalling` name
           bodyDoc <- (if isTailCall then withStatement else id) 
                      (genStat (ResultReturn (Just name) params) body)
           return   $ text "function" <+> ppName (unqualify name) 
                                       <> tupled args 
                                      <+> comm
                                      <+> ( if isTailCall
                                              then tcoBlock bodyDoc
                                              else debugComment ("genFunDef: no tail calls to " ++ showName name ++ " found") 
                                                <> block bodyDoc
                                          )

---------------------------------------------------------------------------------
-- Generate value constructors for each defined type
--------------------------------------------------------------------------------- 

genTypeDefs :: TypeDefGroups -> Asm Doc
genTypeDefs groups
  = do docs <- mapM (genTypeDefGroup) groups
       return (vcat docs)

genTypeDefGroup :: TypeDefGroup -> Asm Doc
genTypeDefGroup  (TypeDefGroup tds)
  = do docs <- mapM (genTypeDef ) tds
       return (vcat docs)

genTypeDef :: TypeDef -> Asm Doc
genTypeDef (Synonym {})
  = return empty
genTypeDef (Data info _ _)
  = do let (dataRepr, conReprs) = getDataRepr (-1) {- maxStructFields -} info
       docs <- mapM ( \(c,repr)  -> do let args = map ppName (map fst (conInfoParams c))
                                       name <- genName (conInfoName c)
                                       penv <- getPrettyEnv
                                       if (conInfoName c == nameTrue)
                                        then return (text "var" <+> name <+> text "=" <+> text "true" <> semi)
                                        else if (conInfoName c == nameFalse)
                                        then return (text "var" <+> name <+> text "=" <+> text "false" <> semi)
                                        else return $ case repr of
                                          ConEnum{}   
                                             -> text "var" <+> name <+> text "=" <+> int (conTag repr) <> semi <+> comment (Pretty.ppType penv (conInfoType c))
                                          ConSingleton{}                                             
                                             -> text "var" <+> name <+> text "=" <+> 
                                                  text (if conInfoName c == nameOptionalNone then "undefined" else "null")
                                                   <> semi <+> comment (Pretty.ppType penv (conInfoType c))
                                          -- tagless
                                          ConSingle{}  -> genConstr penv c repr name args [] 
                                          ConAsCons{}  -> genConstr penv c repr name args []
                                          _            -> genConstr penv c repr name args [(tagField, int (conTag repr))]

                    ) $ zip (dataInfoConstrs $ info) conReprs
       return $ debugComment ( "Value constructors for type '" ++ (show $ dataInfoName info) ++ "' (" ++ (show dataRepr) ++ ")" )
            <-> vcat docs
  where
    genConstr penv c repr name args tagFields
      = if null args
         then debugWrap "genConstr: null fields"
            $ text "var" <+> name <+> text "=" <+> object tagFields <> semi <+> comment (Pretty.ppType penv (conInfoType c)) 
         else debugWrap "genConstr: with fields"
            $ text "function" <+> name <> tupled args <+> comment (Pretty.ppType penv (conInfoType c)) 
          <+> block ( text "return" <+> 
                      (if conInfoName c == nameOptional then head args 
                        else object (tagFields ++ map (\arg -> (arg, arg))  args)) <> semi )

---------------------------------------------------------------------------------
-- Statements 
---------------------------------------------------------------------------------

-- | Applies a return context
getResult :: Result -> Doc -> Doc
getResult result doc
  = if isEmptyDoc doc
      then text ""
      else case result of
             ResultReturn _ _  -> text "return" <+> doc <> semi
             ResultAssign n ml -> ( if isWildcard n
                                      then doc <> semi
                                      else text "var" <+> ppName (unqualify n) <+> text "=" <+> doc <> semi
                                  ) <-> case ml of
                                          Nothing -> empty 
                                          Just l  -> text "break" <+> ppName l <> semi 

tryTailCall :: Result -> Expr -> Asm (Maybe Doc)
tryTailCall result expr
  = case expr of
     -- Tailcall case 1
     App (Var n _) args | ( case result of
                              ResultReturn (Just m) _ -> m == getName n 
                              _                       -> False
                          )
       -> do let (ResultReturn _ params) = result
             stmts <- genOverride params args
             return $ Just $ block $ stmts <-> tailcall

     -- Tailcall case 2
     App (TypeApp (Var n _) _) args | ( case result of
                                        ResultReturn (Just m) _ -> m == getName n 
                                        _                       -> False
                                      )
       -> do let (ResultReturn _ params) = result
             stmts <- genOverride params args
             return $ Just $ block $ stmts <-> tailcall

     _ -> return Nothing
  where
    -- overriding function arguments carefully
    genOverride :: [TName] -> [Expr] -> Asm Doc
    genOverride params args
      = fmap (debugWrap "genOverride") $
        do (stmts, varNames) <- fmap unzip $ mapM genVarBinding args
           docs1             <- mapM genTName params
           docs2             <- mapM genTName varNames
           let assigns    = map (\(p,a)-> if p == a
                                            then debugComment ("genOverride: skipped overriding `" ++ (show p) ++ "` with itself")
                                            else debugComment ("genOverride: preparing tailcall") <> p <+> text "=" <+> a <> semi
                                ) (zip docs1 docs2)
           return $ vcat stmts <-> vcat assigns

-- | Generates a statement from an expression by applying a return context (deeply) inside
genStat :: Result -> Expr -> Asm Doc
genStat result expr
  = fmap (debugWrap "genStat") $
    case extractExternal expr of
      Just (tn,fs,es)
        -> do (statDoc, exprDoc) <- genExternalExpr tn fs es 
              return (statDoc <-> getResult result exprDoc)
      Nothing
        -> do mdoc <- tryTailCall result expr 
              case mdoc of
                Just doc
                  -> return doc
                Nothing 
                  -> case expr of
                        -- If expression is inlineable, inline it
                        _  | isInlineableExpr expr
                          -> do exprDoc <- genInline expr
                                return (getResult result exprDoc)

                        Case exprs branches
                           -> do (docs, scrutinees) <- fmap unzip $ mapM (\e-> if isInlineableExpr e && isTypeBool (typeOf e)
                                                                                 then do d       <- genInline e
                                                                                         return (text "", d)
                                                                                 else do (sd,vn) <- genVarBinding e 
                                                                                         vd      <- genTName vn
                                                                                         return (sd, vd)
                                                                         ) exprs
                                 doc                <- genMatch result scrutinees branches
                                 return (vcat docs <-> doc)

                        Let groups body
                          -> do doc1 <- genGroups groups
                                doc2 <- genStat result body
                                return (doc1 <-> doc2)

                        -- Handling all other cases
                        _ -> do (statDoc,exprDoc) <- genExpr expr
                                return (statDoc <-> getResult result exprDoc)

-- | Generates a statement for a match expression regarding a given return context
genMatch :: Result -> [Doc] -> [Branch] -> Asm Doc
genMatch result scrutinees branches
  = fmap (debugWrap "genMatch") $ do
    case branches of
        []  -> fail ("Backend.JavaScript.FromCore.genMatch: no branch in match statement: " ++ show(scrutinees))
        [b] -> fmap snd $ genBranch True result scrutinees b

       {-- Special handling of return related cases - would be nice to get rid of it
        [ Branch [p1] [Guard t1 (App (Var tn _) [r1])], Branch [p2] [Guard t2 e2] ]
            | getName tn == nameReturn && isPat True p1 && isPat False p2 && isExprTrue t1 && isExprTrue t2
           -> case e2 of
                 App (Var tn _) [r2]
                    | getName tn == nameReturn
                   -> do (stmts1, expr1) <- genExpr r1
                         (stmts2, expr2) <- genExpr r2
                         tnameDocs       <- mapM genTName tnames
                         return $ text "if" <> parens (head tnameDocs ) <+> block (stmts1 <-> text "return" <+> expr1 <> semi)
                                                        <-> text "else" <+> block (stmts2 <-> text "return" <+> expr2 <> semi)
                 Con tn _
                    | getName tn == nameTuple 0
                   -> do (stmts, expr) <- genExpr r1
                         tnameDocs     <- mapM genTName tnames
                         return $ text "if" <> parens (head tnameDocs ) <+> block (stmts <-> text "return" <+> expr <> semi)
                 _ -> fail "Backend.JavaScript.genMatch: found something different than () or return in explicit return"
-}
        [Branch [p1] [Guard t1 e1], Branch [p2] [Guard t2 e2]]
           | isExprTrue t1
          && isExprTrue t2
          && isInlineableExpr e1
          && isInlineableExpr e2
          -> do let nameDoc = head scrutinees
                let test    = genTest  (nameDoc, p1) 
                if (isExprTrue e1 && isExprFalse e2)
                  then return $ getResult result $ parens (conjunction test)
                  else do doc1 <- withNameSubstitutions (getSubstitutions nameDoc p1) (genInline e1)
                          doc2 <- withNameSubstitutions (getSubstitutions nameDoc p2) (genInline e2)
                          return $ debugWrap "genMatch: conditional expression"
                                 $ getResult result
                                 $ parens (conjunction test) <+> text "?" <+> doc1 <+> text ":" <+> doc2 

        bs
           | all (\b-> length (branchGuards   b) == 1) bs 
          && all (\b->isExprTrue $ guardTest $ head $ branchGuards b) bs
          -> do xs <- mapM (withStatement . genBranch True result scrutinees) bs
                return $  debugWrap "genMatch: guard-free case"
                       $  hcat  ( map (\(conds,d)-> text "if" <+> parens (conjunction conds)
                                                             <+> block d <-> text "else "
                                      ) (init xs)
                                )
                      <> block (snd (last xs))

        _ -> do (labelF, result') <- case result of
                      ResultReturn _ _        -> return (id, result)
                      ResultAssign n (Just _) -> return (id, result) -- wohoo, we can jump out from deep in!
                      ResultAssign n Nothing  -> return ( \d-> text "match: " <> block d
                                                        , ResultAssign n (Just $ newName "match")
                                                        )
                bs <- mapM (withStatement . genBranch False result' scrutinees) (init branches)
                b  <-      (withStatement . genBranch True  result' scrutinees) (last branches) 
                let ds = map (\(cds,stmts)-> if null cds
                                                  then stmts
                                                  else text "if" <+> parens (conjunction cds)
                                                                <+> block stmts
                             ) bs
                let d  = snd b
                return $ debugWrap "genMatch: regular case" 
                       $ labelF (vcat ds <-> d)
  where
    -- | Generates a statement for a branch with given return context
    genBranch :: Bool -> Result -> [Doc] -> Branch -> Asm ([ConditionDoc], Doc)
    -- Regular catch-all branch generation
    genBranch lastBranch result tnDocs branch@(Branch patterns guards)
      = do let substs     = concatMap (uncurry getSubstitutions) (zip tnDocs patterns)
           let conditions = concatMap genTest (zip tnDocs patterns)
           let se         = withNameSubstitutions substs

           gs <- mapM (se . genGuard False      result) (init guards)
           g  <-      (se . genGuard lastBranch result) (last guards)
           return (conditions, debugWrap "genBranch" $ vcat gs <-> g)
    
    getSubstitutions :: Doc -> Pattern -> [(TName, Doc)]
    getSubstitutions nameDoc pat
          = case pat of
              PatCon tn args _ _ info 
                -> concatMap (\(pat',fn)-> getSubstitutions 
                                             (nameDoc <> (if (getName tn == nameOptional) then empty else (text "."  <> fn)))
                                             pat'
                            ) (zip args (map (ppName . fst) (conInfoParams info)) )
              PatVar tn pat'      -> (tn, nameDoc):(getSubstitutions nameDoc pat')
              PatWild             -> [] 

    genGuard  :: Bool -> Result -> Guard -> Asm Doc
    genGuard lastBranchLastGuard result (Guard t expr)
      = do (testSt, testE) <- genExpr t
           let result'      = case result of
                               ResultAssign n _ | lastBranchLastGuard -> ResultAssign n Nothing
                               _                                      -> result
           exprSt          <- genStat result' expr
           return $ if isExprTrue t
                      then exprSt
                      else testSt <-> text "if" <+> parens testE <> block exprSt

    -- | Generates a list of boolish expression for matching the pattern
    genTest :: (Doc, Pattern) -> [Doc]
    genTest (scrutinee,pattern)
      = case pattern of
              PatWild ->  []
              PatVar _ pat 
                -> genTest (scrutinee,pat)
              PatCon tn fields repr _ info
                | getName tn == nameTrue
                -> [scrutinee]
                | getName tn == nameFalse
                -> [text "!" <> scrutinee]
                | otherwise
                -> case repr of
                     ConEnum _ tag
                       -> [debugWrap "genTest: enum"      $ scrutinee <+> text "===" <+> int tag]
                     ConSingleton{} -- the only constructor without fields (=== null)
                       -> [debugWrap "genTest: singleton" $ scrutinee <+> text "== null"]  -- use == instead of === since undefined == null (for optional arguments)
                     ConSingle{} -- always succeeds
                       -> []
                     ConStruct{}
                       -> fail "Backend.JavaScript.FromCore.genTest: encountered ConStruct, which is not supposed to happen"
                     ConAsCons{}
                       | getName tn == nameOptional
                       -> [scrutinee <+> text "!== undefined"] ++ concatMap (\field -> genTest (scrutinee,field) ) fields
                       | otherwise
                       -> let conTest    = debugWrap "genTest: asCons" $ scrutinee <+> text "!= null" -- use === instead of == since undefined == null (for optional arguments)
                              fieldTests = concatMap
                                             (\(field,fieldName) -> genTest (scrutinee <> dot <> fieldName, field) ) 
                                             (zip fields (map (ppName . fst) (conInfoParams info)) )
                          in (conTest:fieldTests)
                     ConNormal{}
                       -> let conTest    = debugWrap "genTest: normal" $ scrutinee <> dot <> tagField <+> text "===" <+> int (conTag repr)
                              fieldTests  =  concatMap
                                             (\(field,fieldName) -> genTest (scrutinee <> dot <> fieldName, field) ) 
                                             ( zip fields (map (ppName . fst) (conInfoParams info)) )
                          in (conTest:fieldTests)

{-  -- | Generates assignments for the variables in the pattern
    genAssign :: (TName,Pattern) -> Asm [Doc]
    genAssign (TName n t,pattern)
      = do docs <- f (ppName n) pattern
           return $ [debugComment "<genAssign>"] ++ docs ++ [debugComment "</genAssign>"]
      where
        f s pattern
          = case pattern of
              PatWild 
                -> do return []
              PatVar tname pat
                -> do let doc = text "var" <+> ppName (getName tname) <+> text "=" <+> s <> semi
                      docs <- f (ppName (getName tname)) pat -- avoid mutiple a.b.c.d call
                      return (doc:docs)
              PatCon _ fields _ _ info 
                -> do fmap concat $ mapM (\(field,fn) -> f (s <> text "." <> text (show fn)) field) (zip fields (map fst (conInfoParams info))) -- using ppName here writes __null0_ for _field1. WTF?
-}
    -- | Takes a list of docs and concatenates them with logical and
    conjunction :: [Doc] -> Doc
    conjunction docs
      = hcat (intersperse (text " && ") docs)

---------------------------------------------------------------------------------
-- Expressions that produce statements on their way
---------------------------------------------------------------------------------       

-- | Generates javascript statements and a javascript expression from core expression
genExpr :: Expr -> Asm (Doc,Doc)       
genExpr expr
  = case extractExternal expr of
      Just (tn,fs,es)
        -> genExternalExpr tn fs es 
      Nothing
        -> case expr of
             -- check whether the expression is pure an can be inlined
             _  | isInlineableExpr expr
               -> do doc <- genInline expr
                     return (empty,doc)
             
             TypeApp e _ -> genExpr e
             TypeLam _ e -> genExpr e
             
             -- handle not inlineable cases
             App (TypeApp (Con name info) _) [arg]  | getName name == nameOptional
               -> genExpr arg
             App f args 
                -- | isFunExpr f
               -- -> 
               --  | otherwise
               -> case extractList expr of
                    Just (xs,tl) -> genList xs tl
                    Nothing  
                      -> do (decls,fdoc:docs) <- genExprs (f:args) 
                            return (vcat decls, fdoc <> tupled docs <> debugComment "genExpr: App")

             Let groups body 
               -> do decls1       <- genGroups groups
                     (decls2,doc) <- genExpr body
                     return (decls1 <-> decls2, doc)

             Case _ _
               -> do (doc, tname) <- genVarBinding expr
                     nameDoc <- genTName tname
                     return (doc, nameDoc)

             _ -> failure ("JavaScript.FromCore.genExpr: invalid expression:\n" ++ show expr)

extractList :: Expr -> Maybe ([Expr],Expr)
extractList e
  = let (elems,tl) = extract [] e
    in if (length elems > 10) -- only use inlined array for larger lists
        then Just (elems,tl) 
        else Nothing
  where
    extract acc expr
      = case expr of
          App (TypeApp (Con name info) _) [hd,tl]  | getName name == nameCons
            -> extract (hd:acc) tl
          _ -> (reverse acc, expr)

genList :: [Expr] -> Expr -> Asm (Doc,Doc)
genList elems tl
  = do (decls,docs) <- genExprs elems
       (tdecl,tdoc) <- genExpr tl
       return (vcat (decls ++ [tdecl]), text "$std_core.conslist" <> tupled [list docs, tdoc])


genExternalExpr :: TName -> String -> [Expr] -> Asm (Doc,Doc)
genExternalExpr tname format args 
  | getName tname == nameReturn
  = do (statDoc,exprDoc) <- genExpr (head args)
       return (statDoc <-> text "return" <+> exprDoc <> semi <> debugComment "premature return statement (2)"
              , text "") -- emptyness of doc is important! no other way to tell to not generate assignment/return/whatever!
  | otherwise
  = do (statDocs,argDocs) <- genExprs args
       doc <- genExternal tname format argDocs
       return ( debugComment "<genExternalExpr.stmt>" <> vcat statDocs <> debugComment "</genExternalExpr.stmt>"
              , debugComment "<genExternalExpr.expr>" <> doc           <> debugComment "</genExternalExpr.expr>"
              )

genExprs :: [Expr] -> Asm ([Doc],[Doc])
genExprs exprs
  = do xs <- mapM genExpr exprs
       return (unzip xs)

-- | Introduces an additional let binding in core if necessary
--   The expression in the result is guaranteed to be a Var afterwards
genVarBinding :: Expr -> Asm (Doc, TName)
genVarBinding expr
  = case expr of
      Var tn _ -> return $ (empty, tn)
      _        -> do name <- newVarName "x"
                     doc  <- genStat (ResultAssign name Nothing) expr
                     return ( doc, TName name (typeOf expr) )

---------------------------------------------------------------------------------
-- Pure expressions
--------------------------------------------------------------------------------- 

genPure   :: Expr -> Asm Doc
genPure expr
  = case extractExternal expr of
      Just (tn,fs,es)
        -> do vs  <- genVarNames (countExternalArguments tn fs)
              doc <- genExternal tn fs vs
              return $ text "function" <> tupled vs <+> block ( text "return" <+> doc <> semi )
      Nothing
        -> case expr of
             TypeApp e _ -> genPure e
             TypeLam _ e -> genPure e
             Var name info
               -> do doc <- genTName name
                     return $ debugComment (show name)
                           <> doc
             Con name repr
               -> genTName name
             Lit l
               -> return $ ppLit l
             Lam params body
               -> do args    <- mapM genCommentTName params
                     bodyDoc <- genStat (ResultReturn Nothing params) body
                     return (text "function" <> tupled args <+> block bodyDoc)
             _ -> failure ("JavaScript.FromCore.genPure: invalid expression:\n" ++ show expr)

isPat :: Bool -> Pattern -> Bool
isPat b q
  = case q of
      PatWild     -> False
      PatVar _ q' -> isPat b q'
      PatCon {}   -> getName (patConName q) == if b then nameTrue else nameFalse
  
-- | Generates an effect-free javasript expression
--   NOTE: Throws an error if expression is not guaranteed to be effectfree
genInline :: Expr -> Asm Doc
genInline expr
  = case extractExternal expr of
      Just (tn,fs,es)
        -> genExternalInline tn fs es
      Nothing
        -> case expr of
            _  | isPureExpr expr
              -> genPure expr
            TypeLam _ e -> genInline e 
            TypeApp e _ -> genInline e
            App (TypeApp (Con name info) _) [arg]  | getName name == nameOptional
              -> genInline arg
            App f args     
              -> do fdoc    <- genInline f
                    argDocs <- mapM genInline args
                    return (fdoc <> tupled argDocs <> debugComment "genInline: App")
            _ -> failure ("JavaScript.FromCore.genInline: invalid expression:\n" ++ show expr)
  where
    genExternalInline :: TName -> String -> [Expr] -> Asm Doc
    genExternalInline tname format args 
      = do argDocs <- mapM genInline args
           genExternal tname format argDocs
                       
countExternalArguments :: TName -> String -> Int
countExternalArguments tname format
  = let name = getName tname
    in  length $ filter (=='#') format

genExternal :: TName -> String -> [Doc] -> Asm Doc
genExternal tname format argDocs
  = do let name = getName tname
       return (debugComment ("<genExternal format='" ++ format ++ "'>") <> ppExternalF name format argDocs <> debugComment "</genExternal>")
  where
    ppExternalF :: Name -> String -> [Doc] -> Doc
    ppExternalF name []  args
     = empty
    ppExternalF name k@('\\':'#':xs) args
     = char '#' <> ppExternalF name xs args
    ppExternalF name k@('#':'#':xs) args
     = failure ("Backend.JavaScript.FromCore: type arguments in javascript external in: " ++ show tname)
    ppExternalF name k@('#':y:xs)  args
     = if  y `elem` ['1'..'9']
        then (let n = length args
                  i = fromEnum y - fromEnum '1'
              in assertion ("illegal index in external: " ++ show tname ++ "("++k++"): index: " ++ show i) (i < n) $
                 args!!i <> ppExternalF name xs args)
        else char y <> ppExternalF name xs args
    ppExternalF name (x:xs)  args
     = char x <> ppExternalF name xs args

genDefName :: TName -> Asm Doc
genDefName tname
  = return (ppName (unqualify (getName tname)))

genTName :: TName -> Asm Doc
genTName tname
  = do env <- getEnv
       case lookup tname (substEnv env) of
          Nothing -> genName (getName tname)
          Just d  -> return d

genName :: Name -> Asm Doc
genName name
  = if (isQualified name)
      then do modname <- getModule
              if (qualifier name == modname)
               then return (ppName (unqualify name))
               else return (ppName name)
      else return (ppName name)

genVarName :: String -> Asm Doc
genVarName s = do n <- newVarName s
                  return $ ppName n

-- | Generates `i` fresh variables and delivers them as `Doc` right away
genVarNames :: Int -> Asm [Doc]
genVarNames i = do ns <- newVarNames i
                   return $ map ppName ns

-- | Generate a name with its type in comments
genCommentTName :: TName -> Asm Doc
genCommentTName (TName n t)
  = do env <- getPrettyEnv
       return $ ppName n <+> comment (Pretty.ppType env t ) 

---------------------------------------------------------------------------------
-- Classification
---------------------------------------------------------------------------------

extractExternal  :: Expr -> Maybe (TName, String, [Expr])
extractExternal expr
  = case expr of
      App (TypeApp (Var tname (InfoExternal formats)) targs) args
        -> Just (tname, format tname formats, args)
      App var@(Var tname (InfoExternal formats)) args
        -> Just (tname, format tname formats, args)
      _ -> Nothing
  where 
    format tn fs 
      = case lookup JS fs of
          Nothing -> case lookup Default fs of
                       Nothing -> failure ("backend does not support external in " ++ show tn ++ show fs)
                       Just s  -> s
          Just s -> s

isFunExpr :: Expr -> Bool
isFunExpr expr
  = case expr of
      TypeApp e _   -> isFunExpr e
      TypeLam _ e   -> isFunExpr e
      Lam args body -> True 
      _             -> False

isInlineableExpr :: Expr -> Bool
isInlineableExpr expr
  = case expr of
      TypeApp expr _   -> isInlineableExpr expr
      TypeLam _ expr   -> isInlineableExpr expr
      App f args       -> isPureExpr f && all isPureExpr args && not (isFunExpr f) -- avoid `fun() {}(a,b,c)` !
      _                -> isPureExpr expr

isPureExpr :: Expr -> Bool
isPureExpr expr
  = case expr of  
      TypeApp expr _  -> isPureExpr expr
      TypeLam _ expr  -> isPureExpr expr
      Var n _  | getName n == nameReturn -> False -- make sure return will never be inlined
               | otherwise               -> True
      Con _ _ -> True
      Lit _   -> True      
      Lam _ _ -> True
      _       -> False             


isTailCalling :: Expr -> Name -> Bool
isTailCalling expr n
  = case expr of
      TypeApp expr _    -> expr `isTailCalling` n     -- trivial
      TypeLam _ expr    -> expr `isTailCalling` n     -- trivial
      Lam _ _           -> False                      -- lambda body is a new context, can't tailcall
      Var _ _           -> False                      -- a variable is not a call
      Con _ _           -> False                      -- a constructor is not a call
      Lit _             -> False                      -- a literal is not a call
      App (Var tn _) _   | getName tn == n            -- direct application can be a tail call
                        -> True
      App (TypeApp (Var tn _) _) _ | getName tn == n  -- tailcalled function might be polymorphic and is applied to types before
                        -> True
      App (Var tn _) [e] | getName tn == nameReturn   -- a return statement is transparent in terms of tail calling
                        -> e `isTailCalling` n
      App _ _           -> False                      -- other applications don't apply
      Let _ e           -> e `isTailCalling` n        -- tail calls can only happen in the actual body
      Case _ bs         -> any f1 bs                  -- match statement get analyzed in depth
  where                  
    f1 (Branch _ gs) = any f2 gs                      -- does any of the guards tailcall?
    f2 (Guard _ e)   = e `isTailCalling` n            -- does the guarded expression tailcall?

---------------------------------------------------------------------------------
-- The assembly monad
---------------------------------------------------------------------------------

newtype Asm a = Asm { unAsm :: Env -> St -> (a, St)}

instance Functor Asm where
  fmap f (Asm a) = Asm (\env st -> case a env st of
                                     (x,st') -> (f x, st'))

instance Applicative Asm where
  pure  = return
  (<*>) = ap

instance Monad Asm where
  return x      = Asm (\env st -> (x,st))
  (Asm a) >>= f = Asm (\env st -> case a env st of
                                    (x,st1) -> case f x of
                                                 Asm b -> b env st1)

runAsm :: Env -> Asm Doc -> Doc
runAsm initEnv (Asm asm) 
  = case asm initEnv initSt of
      (doc,st) -> doc

data St  = St  { uniq     :: Int             
               }

data Env = Env { moduleName        :: Name                    -- | current module
               , prettyEnv         :: Pretty.Env              -- | for printing nice types
               , substEnv          :: [(TName, Doc)]          -- | substituting names
               , inStatement       :: Bool                    -- | for generating correct function declarations in strict mode
               }

data Result = ResultReturn (Maybe Name) [TName] -- first field carries function name if not anonymous and second the arguments which are always known
            | ResultAssign Name (Maybe Name)    -- variable name and optional label to break

initSt = St 0 

instance HasUnique Asm where
  updateUnique f
    = Asm (\env st -> (uniq st, st{ uniq = f (uniq st)}))

updateSt f
  = Asm (\env st -> (st,f st))

getSt 
  = updateSt id

setSt st
  = updateSt (const st)

getEnv 
  = Asm (\env st -> (env, st))

withEnv f (Asm asm)
  = Asm (\env st -> asm (f env) st)

localUnique asm
  = do u <- updateUnique id
       x <- asm
       setUnique u
       return x
  
newVarName :: String -> Asm Name
newVarName s
  = do u <- unique
       return (newName ("." ++ s ++ show u))

newVarNames :: Int -> Asm [Name]
newVarNames 0 = return []
newVarNames i
  = do n  <- newVarName "x"
       ns <- newVarNames (i - 1)
       return (n:ns)

getModule :: Asm Name
getModule
  = do env <- getEnv
       return (moduleName env)

getPrettyEnv :: Asm Pretty.Env
getPrettyEnv
  = do env <- getEnv
       return (prettyEnv env)

withTypeVars :: [TypeVar] -> Asm a -> Asm a
withTypeVars vars asm
  = withEnv (\env -> env{ prettyEnv = Pretty.niceEnv (prettyEnv env) vars }) asm

withNameSubstitutions :: [(TName, Doc)] -> Asm a -> Asm a
withNameSubstitutions subs asm
  = withEnv (\env -> env{ substEnv = subs ++ substEnv env }) asm

withStatement :: Asm a -> Asm a
withStatement asm
  = withEnv (\env -> env{ inStatement = True }) asm

getInStatement :: Asm Bool
getInStatement 
  = do env <- getEnv
       return (inStatement env)

---------------------------------------------------------------------------------
-- Pretty printing
---------------------------------------------------------------------------------

-- | Approved for use in JavaScript according to ECMA definition
ppLit :: Lit -> Doc
ppLit lit
    = case lit of
      LitInt i    -> (pretty i)
      LitChar c   -> squotes (escape c)
      LitFloat d  -> (pretty d)
      LitString s -> dquotes (hcat (map escape s))
    where
      escape c
        = if (c < ' ') 
           then (if (c=='\n') then text "\\n"
                 else if (c == '\r') then text "\\r"
                 else if (c == '\t') then text "\\t"
                 else text "\\u" <> text (showHex 4 (fromEnum c)))
          else if (c <= '~')
           then (if (c == '\"') then text "\\\""
                 else if (c=='\'') then text "\\'" 
                 else if (c=='\\') then text "\\\\"
                 else char c)
          else if (fromEnum c <= 0xFFFF)
           then text "\\u" <> text (showHex 4 (fromEnum c))
           else text "\\U" <> text (showHex 8 (fromEnum c))

ppName :: Name -> Doc
ppName name
  = if isQualified name
     then ppModName (qualifier name) <> dot <> encode False (unqualify name)
     else encode False name

ppQName :: Name -> Name -> Doc
ppQName modName name
  = if (modName == qualifier name)   -- We need to qualify always since otherwise we may clash with local variables. i.e. fun f( x : int ) { Main.x( x ) }
     then ppName (unqualify name)
     else ppName name

ppModName :: Name -> Doc
ppModName name
  = text "$" <> encode True (name)

encode :: Bool -> Name -> Doc
encode isModule name
  = let s = show name       
    in if (isReserved s) 
         then text ('$' : s)
         else text ( (asciiEncode isModule s))

isReserved :: String -> Bool
isReserved s
  = if (not $ null s) && (head s == 'T') && all isDigit (tail s)
      then True
      else s `S.member` reserved

reserved :: S.Set String
reserved
  = S.fromList $ -- JavaScript pseudo-keywords
    [ "prototype"
    , "toString"
    , "arguments"
    , "eval"
    ]
    ++ -- word literals
    [ "null"
    , "Infinity"
    , "NaN" 
    ]
    ++ -- JavaScript keywords
    [ "break"
    , "case"
    , "catch"
    , "continue"
    , "debugger"
    , "default"
    , "delete"
    , "do"
    , "else"
    , "finally"
    , "for"
    , "function"
    , "if"
    , "in"
    , "instanceof"
    , "new"
    , "return"
    , "switch"
    , "this"
    , "throw"
    , "try"
    , "typeof"
    , "var"
    , "void"
    , "while"
    , "with"
    ]
    ++ -- reserved for future use
    [ "class"
    , "enum"
    , "export"
    , "extends"
    , "import"
    , "super"
    , "const"
    ]
    ++ -- special globals
    [ "window"
    , "document"
    , "process"
    , "exports"
    , "module"
    ]

block :: Doc -> Doc
block doc
  = text "{" <--> tab doc <--> text "}"        


tcoBlock :: Doc -> Doc
tcoBlock doc
  = text "{ tailcall: while(1)" <-> 
    text "{" <--> tab ( doc ) <--> text "}}" 

tailcall :: Doc
tailcall  = text "continue tailcall;" 

object :: [(Doc, Doc)] -> Doc
object xs
  = text "{" <+> hcat ( punctuate (comma <> space) (map f xs) ) <+> text "}"
  where
    f (d1, d2) = d1 <> colon <+> d2

tab :: Doc -> Doc
tab doc
  = indent 2 doc

typeComment = comment

comment :: Doc -> Doc
comment d
  = text " /*" <+> d <+> text "*/ "

debugComment :: String -> Doc
debugComment s
  = if debug
      then comment (text s)
      else empty

debugWrap     :: String -> Doc -> Doc
debugWrap s d
  = if debug 
      then debugComment ("<" ++ s ++ ">") <-> tab d <-> debugComment ("</" ++ s ++ ">")
      else d

tagField :: Doc
tagField  = text "_tag"


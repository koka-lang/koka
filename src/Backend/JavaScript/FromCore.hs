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

import Platform.Config(version)
import Lib.Trace
import Control.Applicative hiding (empty)
import Control.Monad
import Data.List ( intersperse, partition )
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
import Core.CoreVar

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

javascriptFromCore :: Int -> Maybe (Name,Bool) -> Core -> Doc
javascriptFromCore maxStructFields mbMain core
  = runAsm (Env moduleName penv externalNames False) (genModule maxStructFields mbMain core)
  where
    moduleName = coreProgName core
    penv       = Pretty.defaultEnv{ Pretty.context = moduleName, Pretty.fullNames = False }

genModule :: Int -> Maybe (Name,Bool) -> Core -> Asm Doc
genModule maxStructFields mbMain core
  =  do let externs = vcat (concatMap includeExternal (coreProgExternals core))
            (tagDefs,defs) = partition isTagDef (coreProgDefs core)
        decls0 <- genGroups tagDefs
        decls1 <- genTypeDefs maxStructFields (coreProgTypeDefs core)
        decls2 <- genGroups defs
        let imports = map importName (coreProgImports core)
            (mainEntry,mainImports) = case mbMain of
                          Nothing -> (empty,[])
                          Just (name,isAsync)
                            -> (if isAsync
                                 then (text " " <-> text "// main entry:" <->
                                       text "$std_async_.async_handle" <> parens (ppName (unqualify name)) <> semi
                                      ,[(text "./std_async", text "$std_async_")])
                                 else (text " " <-> text "// main entry:" <->
                                       ppName (unqualify name) <> text "($std_core.id);" -- pass id for possible cps translated main
                                      ,[]))
        return $  text "// Koka generated module:" <+> string (showName (coreProgName core)) <> text ", koka version:" <+> string version
              <-> text "if (typeof define !== 'function') { var define = require('amdefine')(module) }"
              <-> text "define(" <> ( -- (squotes $ ppModFileName $ coreProgName core) <> comma <->
                   list ( {- (squotes $ text "_external"): -} (map squotes (map fst (externalImports++mainImports)) ++ map moduleImport (coreProgImports core))) <> comma <+>
                   text "function" <> tupled ( {- (text "_external"): -} (map snd (externalImports ++ mainImports) ++ map ppModName imports)) <+> text "{" <->
                    vcat (
                    [ text "\"use strict\";"
                    , text "var" <+> modName <+> text " = {};"
                    , text " "
                    , text "// externals"
                    , externs
                    , text " "
                    , text "// type declarations"
                    , decls0
                    , decls1
                    , text " "
                    , text "// declarations"
                    , decls2
                    , mainEntry
                    , text " "
                    , text "// exports"
                    , hang 2 (modName <+> text "=" <+> ppModName nameSystemCore <> dot <> text "_export(" <>
                                modName <> text ", {" <-->
                        (vcat $ punctuate comma $
                           map (\n-> fill 12 (ppName n) <> text ":" <+> ppName n)
                              ( exportedConstrs ++ exportedValues ))
                      ) <--> text "});"
                    , text "return" <+> modName <> semi
                    ])
                 )
              <-> text "});"
  where
    modName         = ppModName (coreProgName core)
    exportedValues  = let f (DefRec xs)   = map defName xs
                          f (DefNonRec x) = [defName x]
                      in map unqualify $ concatMap f (coreProgDefs core)
    exportedConstrs = let f (Synonym _ _)    = []
                          f (Data info _ vs _)
                                             = let xs = zip vs $ map conInfoName (dataInfoConstrs info)
                                               in  map snd $ filter (\(v,_)-> v == Public) xs
                          u (TypeDefGroup xs) = xs
                      in map unqualify $ concatMap f $ concatMap u (coreProgTypeDefs core)

    isTagDef (DefNonRec def) = isOpenTagName (defName def)
    isTagDef _               = False

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
                                 Nothing -> ""
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
  = localUnique $
    do docs <- mapM genGroup groups
       return (vcat docs)

genGroup :: DefGroup -> Asm Doc
genGroup group
  = case group of
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
      TypeApp e _   -> tryFunDef  name comment e
      TypeLam _ e   -> tryFunDef  name comment e
      Lam args eff body  -> do inStat <- getInStatement
                               if (inStat)
                                then return Nothing
                                else do fun <- genFunDef' name args comment body
                                        return (Just fun)
      _ -> return Nothing
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

genTypeDefs :: Int -> TypeDefGroups -> Asm Doc
genTypeDefs maxStructFields groups
  = do docs <- mapM (genTypeDefGroup maxStructFields) groups
       return (vcat docs)

genTypeDefGroup :: Int -> TypeDefGroup -> Asm Doc
genTypeDefGroup maxStructFields (TypeDefGroup tds)
  = do docs <- mapM (genTypeDef maxStructFields) tds
       return (vcat docs)

genTypeDef :: Int -> TypeDef -> Asm Doc
genTypeDef maxStructFields (Synonym {})
  = return empty
genTypeDef maxStructFields (Data info _ _ isExtend)
  = do modName <- getModule
       let (dataRepr, conReprs) = getDataRepr maxStructFields info
       docs <- mapM ( \(c,repr)  ->
          do let args = map ppName (map fst (conInfoParams c))
             name <- genName (conInfoName c)
             penv <- getPrettyEnv
             if (conInfoName c == nameTrue)
              then return (constdecl <+> name <+> text "=" <+> text "true" <> semi)
              else if (conInfoName c == nameFalse)
              then return (constdecl <+> name <+> text "=" <+> text "false" <> semi)
              else return $ case repr of
                ConEnum{}
                   -> constdecl <+> name <+> text "=" <+> int (conTag repr) <> semi <+> linecomment (Pretty.ppType penv (conInfoType c))
                ConSingleton{}
                   -> constdecl <+> name <+> text "=" <+>
                        text (if conInfoName c == nameOptionalNone then "undefined" else "null")
                         <> semi <+> linecomment (Pretty.ppType penv (conInfoType c))
                -- tagless
                ConIso{}     -> genConstr penv c repr name args []
                ConSingle{}  -> genConstr penv c repr name args []
                ConAsCons{}  -> genConstr penv c repr name args []
                _            -> genConstr penv c repr name args [(tagField, getConTag modName c repr)]
          ) $ zip (dataInfoConstrs $ info) conReprs
       return $ linecomment (text "type" <+> pretty (unqualify (dataInfoName info)))
            <-> vcat docs
            <-> text ""
  where
    genConstr penv c repr name args tagFields
      = if null args
         then debugWrap "genConstr: null fields"
            $ constdecl <+> name <+> text "=" <+> object tagFields <> semi <+> linecomment (Pretty.ppType penv (conInfoType c))
         else debugWrap "genConstr: with fields"
            $ text "function" <+> name <> tupled args <+> comment (Pretty.ppType penv (conInfoType c))
          <+> block ( text "return" <+>
                      (if (conInfoName c == nameOptional || isConIso repr) then head args
                        else object (tagFields ++ map (\arg -> (arg, arg))  args)) <> semi )

getConTag modName coninfo repr
  = case repr of
      ConOpen{} -> -- ppLit (LitString (show (openConTag (conInfoName coninfo))))
                   let name = toOpenTagName (conInfoName coninfo)
                   in ppName (if (qualifier name == modName) then unqualify name else name)
      _ -> int (conTag repr)

openConTag name
  = name

---------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------

-- | Applies a return context
getResult :: Result -> Doc -> Doc
getResult result doc
  = if isEmptyDoc doc
      then text ""
      else getResultX result (doc,doc)

getResultX result (puredoc,retdoc)
  = case result of
     ResultReturn _ _  -> text "return" <+> retdoc <> semi
     ResultAssign n ml -> ( if isWildcard n
                              then (if (isEmptyDoc puredoc) then puredoc else puredoc <> semi)
                              else text "var" <+> ppName (unqualify n) <+> text "=" <+> retdoc <> semi
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
        do (stmts, varNames) <- do args' <- mapM tailCallArg args
                                   bs    <- mapM genVarBinding args'
                                   return (unzip bs)
           docs1             <- mapM genTName params
           docs2             <- mapM genTName varNames
           let assigns    = map (\(p,a)-> if p == a
                                            then debugComment ("genOverride: skipped overriding `" ++ (show p) ++ "` with itself")
                                            else debugComment ("genOverride: preparing tailcall") <> p <+> text "=" <+> a <> semi
                                ) (zip docs1 docs2)
           return $
             linecomment (text "tail call") <-> vcat stmts <-> vcat assigns

    -- if local variables are captured inside a tailcalling function argument,
    -- we need to capture it by value (instead of reference since we will overwrite the local variables on a tailcall)
    -- we do this by wrapping the argument inside another function application.
    tailCallArg :: Expr -> Asm Expr
    tailCallArg expr
      = let captured = filter (not . isQualified . getName) $ tnamesList $ capturedVar expr
        in if (null captured)
            then return expr
            else -- trace ("Backend.JavaScript.FromCore.tailCall: capture: " ++ show captured ++ ":\n" ++ show expr) $
                 do ns <- mapM (newVarName . show) captured
                    let cnames = [TName cn tp | (cn,TName _ tp) <- zip ns captured]
                        sub    = [(n,Var cn InfoNone) | (n,cn) <- zip captured cnames]
                    return $ App (Lam cnames typeTotal (sub |~> expr)) [Var arg InfoNone | arg <- captured]

    capturedVar :: Expr -> TNames
    capturedVar expr
      = case expr of
          Lam _ _  _  -> fv expr  -- we only care about captures inside a lambda
          Let bgs body -> S.unions (capturedVar body : map capturedDefGroup bgs)
          Case es bs   -> S.unions (map capturedVar es ++ map capturedBranch bs)
          App f args   -> S.unions (capturedVar f : map capturedVar args)
          TypeLam _ e  -> capturedVar e
          TypeApp e _  -> capturedVar e
          _            -> S.empty

    capturedDefGroup bg
      = case bg of
          DefRec defs  -> S.difference (S.unions (map capturedDef defs)) (bv defs)
          DefNonRec def-> capturedDef def

    capturedDef def
      = capturedVar (defExpr def)

    capturedBranch (Branch pat grds)
      = S.difference (S.unions (map capturedGuard grds)) (bv pat)

    capturedGuard (Guard test expr)
      = S.union (capturedVar test) (capturedVar expr)

-- | Generates a statement from an expression by applying a return context (deeply) inside
genStat :: Result -> Expr -> Asm Doc
genStat result expr
  = fmap (debugWrap "genStat") $
    {-
    case extractExternal expr of
      Just (tn,fs,es)
        -> do (statDoc, exprDoc) <- genExternalExpr tn fs es
              return (statDoc <-> getResult result exprDoc)
      Nothing
        -> -}
           do mdoc <- tryTailCall result expr
              case mdoc of
                Just doc
                  -> return doc
                Nothing
                  -> genExprStat result expr


genExprStat result expr
  = case expr of
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

       -- Special handling of return related cases - would be nice to get rid of it
        [ Branch [p1] [Guard t1 (App (Var tn _) [r1])], Branch [p2] [Guard t2 e2] ]
            | getName tn == nameReturn &&
              isPat True p1 && isPat False p2 &&
              isExprTrue t1 && isExprTrue t2
           -> case e2 of
                 App (Var tn _) [r2]
                    | getName tn == nameReturn
                   -> do (stmts1, expr1) <- genExpr r1
                         (stmts2, expr2) <- genExpr r2
                         return $ text "if" <> parens (head scrutinees) <+> block (stmts1 <-> text "return" <+> expr1 <> semi)
                                                        <-> text "else" <+> block (stmts2 <-> text "return" <+> expr2 <> semi)
                 _ -> do (stmts1,expr1) <- genExpr r1
                         (stmts2,expr2) <- genExpr e2
                         return $
                           (text "if" <> parens (head scrutinees) <+> block (stmts1 <-> text "return" <+> expr1 <> semi))
                            <-->
                           (stmts2 <-> getResultX result (if (isExprUnit e2) then text "" else expr2,expr2))

        [Branch [p1] [Guard t1 e1], Branch [p2] [Guard t2 e2]]
           | isExprTrue t1
          && isExprTrue t2
          && isInlineableExpr e1
          && isInlineableExpr e2
          -> do modName <- getModule
                let nameDoc = head scrutinees
                let test    = genTest modName (nameDoc, p1)
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
      = do modName <- getModule
           let substs     = concatMap (uncurry getSubstitutions) (zip tnDocs patterns)
           let conditions = concatMap (genTest modName) (zip tnDocs patterns)
           let se         = withNameSubstitutions substs

           gs <- mapM (se . genGuard False      result) (init guards)
           g  <-      (se . genGuard lastBranch result) (last guards)
           return (conditions, debugWrap "genBranch" $ vcat gs <-> g)

    getSubstitutions :: Doc -> Pattern -> [(TName, Doc)]
    getSubstitutions nameDoc pat
          = case pat of
              PatCon tn args repr _ _ _ info
                -> concatMap (\(pat',fn)-> getSubstitutions
                                             (nameDoc <> (if (getName tn == nameOptional || isConIso repr) then empty else (text "."  <> fn)))
                                             pat'
                            ) (zip args (map (ppName . fst) (conInfoParams info)) )
              PatVar tn pat'      -> (tn, nameDoc):(getSubstitutions nameDoc pat')
              PatWild             -> []
              PatLit lit          -> []

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
    genTest :: Name -> (Doc, Pattern) -> [Doc]
    genTest modName (scrutinee,pattern)
      = case pattern of
              PatWild ->  []
              PatVar _ pat
                -> genTest modName (scrutinee,pat)
              PatLit lit
                -> [scrutinee <+> text "===" <+> ppLit lit]
              PatCon tn fields repr _ _ _ info
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
                     ConIso{} -- alwasy success
                       -> []
                     ConStruct{}
                       -> fail "Backend.JavaScript.FromCore.genTest: encountered ConStruct, which is not supposed to happen"
                     ConAsCons{}
                       | getName tn == nameOptional
                       -> [scrutinee <+> text "!== undefined"] ++ concatMap (\field -> genTest modName (scrutinee,field) ) fields
                       | otherwise
                       -> let conTest    = debugWrap "genTest: asCons" $ scrutinee <+> text "!= null" -- use === instead of == since undefined == null (for optional arguments)
                              fieldTests = concatMap
                                             (\(field,fieldName) -> genTest modName (scrutinee <> dot <> fieldName, field) )
                                             (zip fields (map (ppName . fst) (conInfoParams info)) )
                          in (conTest:fieldTests)
                     _ -> let conTest    = debugWrap "genTest: normal" $ scrutinee <> dot <> tagField <+> text "===" <+> getConTag modName info repr
                              fieldTests  =  concatMap
                                             (\(field,fieldName) -> genTest modName (scrutinee <> dot <> fieldName, field) )
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
    conjunction []
      = text "true"
    conjunction docs
      = hcat (intersperse (text " && ") docs)

---------------------------------------------------------------------------------
-- Expressions that produce statements on their way
---------------------------------------------------------------------------------

-- | Generates javascript statements and a javascript expression from core expression
genExpr :: Expr -> Asm (Doc,Doc)
genExpr expr
  = case expr of
     -- check whether the expression is pure an can be inlined
     _  | isInlineableExpr expr
       -> do doc <- genInline expr
             return (empty,doc)

     TypeApp e _ -> genExpr e
     TypeLam _ e -> genExpr e

     -- handle not inlineable cases
     App (TypeApp (Con name repr) _) [arg]  | getName name == nameOptional || isConIso repr
       -> genExpr arg
     App (Con _ repr) [arg]  | isConIso repr
       -> genExpr arg
     App f args
        -- | isFunExpr f
       -- ->
       --  | otherwise
       -> case extractList expr of
            Just (xs,tl) -> genList xs tl
            Nothing -> case extractExtern f of
             Just (tname,formats)
              -> do (decls,argDocs) <- genExprs args
                    (edecls,doc) <- genExprExternal tname formats argDocs
                    if (getName tname == nameReturn)
                     then return (vcat (decls ++ edecls ++ [doc <> semi]), text "")
                     else return (vcat (decls ++ edecls), doc)
             Nothing
              -> do (decls,fdoc:docs) <- genExprs (f:trimOptionalArgs args)
                    return (vcat decls, fdoc <> tupled docs)

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
       return (vcat (decls ++ [tdecl]), text "$std_core.vlist" <> tupled [list docs, tdoc])

{-
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
-}

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
  = case expr of
     TypeApp e _ -> genPure e
     TypeLam _ e -> genPure e
     Var name (InfoExternal formats)
       -> genWrapExternal name formats  -- unapplied inlined external: wrap as function
     Var name info
       -> genTName name
     Con name repr
       -> genTName name
     Lit l
       -> return $ ppLit l
     Lam params eff body
       -> do args    <- mapM genCommentTName params
             bodyDoc <- genStat (ResultReturn Nothing params) body
             return (text "function" <> tupled args <+> block bodyDoc)
     _ -> failure ("JavaScript.FromCore.genPure: invalid expression:\n" ++ show expr)

isPat :: Bool -> Pattern -> Bool
isPat b q
  = case q of
      PatWild     -> False
      PatLit _    -> False
      PatVar _ q' -> isPat b q'
      PatCon {}   -> getName (patConName q) == if b then nameTrue else nameFalse

-- | Generates an effect-free javasript expression
--   NOTE: Throws an error if expression is not guaranteed to be effectfree
genInline :: Expr -> Asm Doc
genInline expr
  = case expr of
      _  | isPureExpr expr -> genPure expr
      TypeLam _ e -> genInline e
      TypeApp e _ -> genInline e
      App (TypeApp (Con name repr) _) [arg]  | getName name == nameOptional || isConIso repr
        -> genInline arg
      App (Con _ repr) [arg]  | isConIso repr
        -> genInline arg
      App f args
        -> do argDocs <- mapM genInline (trimOptionalArgs args)
              case extractExtern f of
                Just (tname,formats)
                  -> case args of
                       [Lit (LitInt i)] | getName tname == nameInt32 && isSmallInt i
                         -> return (pretty i)
                       _ -> genInlineExternal tname formats argDocs
                Nothing
                  -> do fdoc <- genInline f
                        return (fdoc <> tupled argDocs)
      _ -> failure ("JavaScript.FromCore.genInline: invalid expression:\n" ++ show expr)

extractExtern :: Expr -> Maybe (TName,[(Target,String)])
extractExtern expr
  = case expr of
      TypeApp (Var tname (InfoExternal formats)) targs -> Just (tname,formats)
      Var tname (InfoExternal formats) -> Just (tname,formats)
      _ -> Nothing

-- not fully applied external gets wrapped in a function
genWrapExternal :: TName -> [(Target,String)] -> Asm Doc
genWrapExternal tname formats
  = do let n = snd (getTypeArities (typeOf tname))
       vs  <- genVarNames n
       (decls,doc) <- genExprExternal tname formats vs
       return $ parens (text "function" <> tupled vs <+> block (vcat (decls ++ [text "return" <+> doc <> semi])))

-- inlined external sometimes  needs wrapping in a applied function block
genInlineExternal :: TName -> [(Target,String)] -> [Doc] -> Asm Doc
genInlineExternal tname formats argDocs
  = do (decls,doc) <- genExprExternal tname formats argDocs
       if (null decls)
        then return doc
        else return $ parens $ parens (text "function()" <+> block (vcat (decls ++ [text "return" <+> doc <> semi]))) <> text "()"

-- generate external
genExprExternal :: TName -> [(Target,String)] -> [Doc] -> Asm ([Doc],Doc)
genExprExternal tname formats argDocs0
  = let name = getName tname
        format = getFormat tname formats
        argDocs = map (\argDoc -> if (all (\c -> isAlphaNum c || c == '_') (asString argDoc)) then argDoc else parens argDoc) argDocs0
    in return $ case map (\fmt -> ppExternalF name fmt argDocs) $ lines format of
         [] -> ([],empty)
         ds -> (init ds, last ds)
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
                 (args!!i) <> ppExternalF name xs args)
        else char y <> ppExternalF name xs args
    ppExternalF name (x:xs)  args
     = char x <> ppExternalF name xs args

getFormat :: TName -> [(Target,String)] -> String
getFormat tname formats
  = case lookup JS formats of
      Nothing -> case lookup Default formats of
         Just s  -> s
         Nothing -> -- failure ("backend does not support external in " ++ show tname ++ ": " ++ show formats)
                    trace( "warning: backend does not support external in " ++ show tname ) $
                      ("__std_core._unsupported_external(\"" ++ (show tname) ++ "\")")
      Just s -> s

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

trimOptionalArgs args
  = reverse (dropWhile isOptionalNone (reverse args))
  where
    isOptionalNone arg
      = case arg of
          TypeApp (Con tname _) _ -> getName tname == nameOptionalNone
          _ -> False

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
      Lam args eff body -> True
      _                 -> False

isInlineableExpr :: Expr -> Bool
isInlineableExpr expr
  = case expr of
      TypeApp expr _   -> isInlineableExpr expr
      TypeLam _ expr   -> isInlineableExpr expr
      App f args       -> isPureExpr f && all isPureExpr args
                          -- all isInlineableExpr (f:args)
                          && not (isFunExpr f) -- avoid `fun() {}(a,b,c)` !
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
      Lam _ _ _ -> True
      _       -> False


isTailCalling :: Expr -> Name -> Bool
isTailCalling expr n
  = case expr of
      TypeApp expr _    -> expr `isTailCalling` n     -- trivial
      TypeLam _ expr    -> expr `isTailCalling` n     -- trivial
      Lam _ _ _           -> False                      -- lambda body is a new context, can't tailcall
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
      LitInt i    -> if (isSmallInt(i))
                      then (pretty i)
                      else ppName nameIntConst <> parens (dquotes (pretty i))
      LitChar c   -> text ("0x" ++ showHex 4 (fromEnum c))
      LitFloat d  -> text (showsPrec 20 d "")
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
          else if (fromEnum c > 0x10FFFF)
           then text "\\uFFFD"  -- error instead?
           else let code = fromEnum c - 0x10000
                    hi = (code `div` 0x0400) + 0xD800
                    lo = (code `mod` 0x0400) + 0xDC00
                in text ("\\u" ++ showHex 4 hi ++ "\\u" ++ showHex 4 lo)

isSmallLitInt expr
  = case expr of
      Lit (LitInt i)  -> isSmallInt i
      _ -> False

isSmallInt i = (i > minSmallInt && i < maxSmallInt)

maxSmallInt, minSmallInt :: Integer
maxSmallInt = 9007199254740991  -- 2^53 - 1
minSmallInt = -maxSmallInt

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
    [ "async"
    , "await"
    , "break"
    , "case"
    , "catch"
    , "continue"
    , "const"
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
    , "yield"
    ]
    ++ -- reserved for future use
    [ "class"
    , "enum"
    , "export"
    , "extends"
    , "import"
    , "super"
    ]
    ++ -- special globals
    [ "window"
    , "document"
    , "process"
    , "exports"
    , "module"
    , "Date"
    , "Error"
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
  = text "/*" <+> d <+> text "*/ "

linecomment :: Doc -> Doc
linecomment d
  = text "//" <+> d

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

constdecl :: Doc
constdecl = text "const"

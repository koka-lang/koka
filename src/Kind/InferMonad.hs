------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{--}
-----------------------------------------------------------------------------
module Kind.InferMonad( KInfer
                      , runKindInfer
                      , addError
                      , freshKind,freshTypeVar,subst
                      , getKGamma
                      , getSynonyms
                      , extendInfGamma, extendKGamma, extendKSub
                      , findInfKind
                      , getColorScheme
                      , lookupSynInfo
                      , qualifyDef
                      , addRangeInfo
                      , infQualifiedName
                      )  where


import Control.Applicative
import Control.Monad

import Lib.Trace
import Lib.PPrint
import Common.Failure( failure )
import Common.Range
import Common.ColorScheme
import Common.Unique
import Common.Name
import Common.NamePrim( toShortModuleName )
import Common.QNameMap( Lookup(..) )
import qualified Common.NameMap as M

import Kind.Kind
import Kind.InferKind
import Kind.Assumption
import Kind.Synonym
import Kind.ImportMap

import Syntax.Syntax
import Type.Type

import Syntax.RangeMap

import qualified Core.Core as Core

{---------------------------------------------------------------
  Inference monad
---------------------------------------------------------------}

data KInfer a = KInfer (KEnv -> KSt -> KResult a)

data KSt        = KSt{ kunique :: !Int, ksub :: !KSub, mbRangeMap :: Maybe RangeMap  }
data KEnv       = KEnv{ cscheme :: !ColorScheme, currentModule :: !Name, imports :: ImportMap
                      , kgamma :: !KGamma, infgamma :: !InfKGamma, synonyms :: !Synonyms }
data KResult a  = KResult{ result:: !a, errors:: ![(Range,Doc)], warnings :: ![(Range,Doc)], st :: !KSt }

runKindInfer :: ColorScheme -> Maybe RangeMap -> Name -> ImportMap -> KGamma -> Synonyms -> Int -> KInfer a -> ([(Range,Doc)],[(Range,Doc)],Maybe RangeMap,Int,a)
runKindInfer cscheme mbRangeMap moduleName imports kgamma syns unique (KInfer ki)
  = let imports' = case importsExtend (toShortModuleName moduleName) moduleName imports of
                     Just imp -> imp
                     Nothing  -> imports -- ignore
    in case ki (KEnv cscheme moduleName imports' kgamma M.empty syns) (KSt unique ksubEmpty mbRangeMap) of
         KResult x errs warns (KSt unique1 ksub rm) -> (errs,warns,rm,unique1,x)


instance Functor KInfer where
  fmap f (KInfer ki)  
    = KInfer (\env -> \st -> let r = ki env st in r{ result = f (result r) })

instance Applicative KInfer where
  pure  = return
  (<*>) = ap    

instance Monad KInfer where
  return x  = KInfer (\env -> \st -> KResult x [] [] st)
  (KInfer ki) >>= f
    = KInfer (\env -> \st ->
        case ki env st of
          KResult x errs1 warns1 st1 
            -> case f x of
                 KInfer kif 
                  -> case kif env st1 of
                       KResult y errs2 warns2 st2 -> KResult y (errs1++errs2) (warns1 ++ warns2) st2)

instance HasUnique KInfer where
  updateUnique f
    = KInfer (\env -> \st -> KResult (kunique st) [] [] (st{ kunique = f (kunique st) }))

getKindEnv :: KInfer KEnv
getKindEnv
  = KInfer (\env -> \st -> KResult env [] [] st)

addError :: Range -> Doc -> KInfer ()
addError range doc
  = do addRangeInfo range (Error doc)
       KInfer (\env -> \st -> KResult () [(range,doc)] [] st)

addWarning :: Range -> Doc -> KInfer ()
addWarning range doc
  = do addRangeInfo range (Warning doc)
       KInfer (\env -> \st -> KResult () [] [(range,doc)] st)

getKSub :: KInfer KSub
getKSub
  = KInfer (\env -> \st -> KResult (ksub st) [] [] st)

extendKSub :: KSub -> KInfer ()
extendKSub sub
  = KInfer (\env -> \st -> KResult () [] [] st{ ksub = sub @@ ksub st })

addRangeInfo :: Range -> RangeInfo -> KInfer ()
addRangeInfo range info
  = KInfer (\env -> \st -> KResult () [] [] st{ mbRangeMap = case (mbRangeMap st) of 
                                                              Just rm -> Just (rangeMapInsert range info rm)
                                                              other   -> other 
                                             })

{---------------------------------------------------------------
  Operations
---------------------------------------------------------------}
freshKind :: KInfer InfKind
freshKind
  = do id <- uniqueId "k"
       return (KIVar id)


freshTypeVar :: TypeBinder Kind -> Flavour -> KInfer TypeVar
freshTypeVar (TypeBinder name kind _ _) flavour
  = do id <- uniqueId (show name)
       return (TypeVar id kind flavour)


subst :: HasKindVar k => k -> KInfer k
subst x
  = do sub <- getKSub 
       return (sub |=> x)

getKGamma :: KInfer KGamma
getKGamma
  = do env <- getKindEnv
       return (kgamma env)

getSynonyms :: KInfer Synonyms
getSynonyms
  = do env <- getKindEnv
       return (synonyms env)

getColorScheme :: KInfer ColorScheme
getColorScheme
  = do env <- getKindEnv
       return (cscheme env)

-- | Extend the inference kind assumption; checks for 'shadow' definitions
extendInfGamma :: [TypeBinder InfKind] -> KInfer a -> KInfer a
extendInfGamma tbinders ki
  = do env <- getKindEnv
       foldM check (infgamma env) tbinders
       extendInfGammaUnsafe tbinders ki
  where
    check :: InfKGamma -> TypeBinder InfKind -> KInfer InfKGamma
    check infgamma (TypeBinder name infkind nameRange range)
      = case M.lookup name infgamma of
          Nothing -> return (M.insert name infkind infgamma)
          Just _  -> do env <- getKindEnv
                        let cs = cscheme env
                        addError nameRange $ text "Type" <+> ppType cs name <+> text "is already defined"
                        return (M.insert name infkind infgamma) -- replace
         

extendInfGammaUnsafe :: [TypeBinder InfKind] -> KInfer a -> KInfer a
extendInfGammaUnsafe tbinders (KInfer ki)
  -- ASSUME: assumes left-biased union
  = KInfer (\env -> \st -> ki (env{ infgamma = M.union infGamma (infgamma env) }) st)
  where
    infGamma = M.fromList (map (\(TypeBinder name infkind _ _) -> (name,infkind)) tbinders)


-- | Extend the kind assumption; checks for duplicate definitions
extendKGamma :: [Range] -> Core.TypeDefGroup -> KInfer a -> KInfer a
extendKGamma ranges (Core.TypeDefGroup (tdefs)) ki
  -- = extendKGammaUnsafe tdefs ki
  -- NOTE: duplication check already happens in extendInfGamma but
  -- there can still be a clash with a definition in another inference group
  = do env     <- getKindEnv
       (_,tdefs')  <- foldM check (kgamma env,[])  (zip ranges tdefs)
       extendKGammaUnsafe (reverse tdefs') ki
  where
    check :: (KGamma,[Core.TypeDef]) -> (Range,Core.TypeDef) -> KInfer (KGamma,[Core.TypeDef])
    check (kgamma,tdefs) (range,tdef)
      = if (Core.typeDefIsExtension tdef) then return (kgamma,tdefs)
         else do let (name,kind) = nameKind tdef
                 -- trace("extend kgamma: " ++ show (name)) $
                 case kgammaLookupQ name kgamma of
                   Nothing -> return (kgammaExtend name kind kgamma,tdef:tdefs)
                   Just _  -> do env <- getKindEnv
                                 addError range $ text "Type" <+> ppType (cscheme env) name <+> 
                                                  text "is already defined"
                                 return (kgamma,tdefs)
      where
        nameKind (Core.Synonym synInfo vis) = (synInfoName synInfo, synInfoKind synInfo)
        nameKind (Core.Data dataInfo vis conviss isExtend)   = (dataInfoName dataInfo, dataInfoKind dataInfo)
  

-- | This extend KGamma does not check for duplicates
extendKGammaUnsafe :: [Core.TypeDef] -> KInfer a -> KInfer a
extendKGammaUnsafe (tdefs) (KInfer ki)  
  -- ASSUME: kgamma and synonyms have a right-biased union
  = KInfer (\env -> \st -> ki (env{ kgamma = -- trace ("extend kgamma:\n" ++ show (kgamma env) ++ "\n with\n " ++ show (kGamma)) $ 
                                             kgammaUnion (kgamma env) kGamma
                                  , synonyms = synonymsCompose (synonyms env) kSyns }) st)
  where
    kGamma = kgammaNewNub (map nameKind tdefs) -- duplicates are removed here
    nameKind (Core.Synonym synInfo vis) = (synInfoName synInfo, synInfoKind synInfo)
    nameKind (Core.Data dataInfo vis conviss isExtend)   = (dataInfoName dataInfo, dataInfoKind dataInfo)

    kSyns  = synonymsNew (concatMap nameSyn tdefs)
    nameSyn (Core.Synonym synInfo vis) = [synInfo]
    nameSyn _                          = []

infQualifiedName :: Name -> Range -> KInfer Name
infQualifiedName name range  | not (isQualified name)
  = return name
infQualifiedName name range
  = do env <- getKindEnv
       case importsExpand name (imports env) of
         Right (name',alias) 
          -> if (not (nameCaseEqual (qualifier name) alias))
              then do let cs = cscheme env
                      addError range (text "module" <+> ppModule cs name <+> text "should be cased as" <+> color (colorModule cs) (pretty alias))
                      return name'
              else return name'
         Left [] 
          -> do let cs = cscheme env
                addError range (text "module" <+> color (colorModule cs) (pretty name) <+> text "is undefined")
                return name
         Left aliases 
          -> do let cs = cscheme env
                addError range (text "module" <+> color (colorModule cs) (pretty name) <+> ambiguous cs aliases)
                return name
       
ppModule cs name
  = color (colorModule cs) (text (nameModule name))

findInfKind :: Name -> Range -> KInfer (Name,InfKind)
findInfKind name0 range 
  = do env <- getKindEnv
       let (name,mbAlias) = case importsExpand name0 (imports env) of
                              Right (name',alias) -> (name',Just alias)
                              _                   -> (name0,Nothing) 
           qname          = if isQualified name then name else qualify (currentModule env) name
       -- lookup locally
       -- note: also lookup qualified since it might be recursive definition 
       -- todo: check for the locally inferred names for casing too.
       -- trace("find: " ++ show (name,qname) ++ ": " ++ show (M.elems (infgamma env))) $ return ()
       case M.lookup name (infgamma env)  of
         Just infkind -> return (name,infkind)
         Nothing ->
           case M.lookup qname (infgamma env) of
             Just infkind -> return (qname,infkind)
             Nothing 
                 -> case kgammaLookup (currentModule env) name (kgamma env) of
                      Found qname kind -> do let name' = if isQualified name then qname else (unqualify qname)
                                             if (-- trace ("compare: " ++ show (qname,name,name0)) $ 
                                                 not (nameCaseEqual name' name)) 
                                              then do let cs = cscheme env
                                                      addError range (text "type" <+> (ppType cs (unqualify name0)) <+> text "should be cased as" <+> ppType cs (unqualify name'))
                                              else return ()
                                             case mbAlias of
                                              Just alias | nameModule name0 /= show alias
                                                -> do let cs = cscheme env
                                                      addError range (text "module" <+> color (colorModule cs) (text (nameModule name0)) <+> text "should be cased as" <+> color (colorModule cs) (pretty alias)
                                                         -- <+> text (show (name,qname,mbAlias,name0))
                                                         )
                                              _ -> return ()
                                             return (qname,KICon kind)
                      NotFound         -> do let cs = cscheme env
                                             addError range (text "Type" <+> (ppType cs name) <+> text "is not defined" <->
                                                             text " hint: bind the variable using" <+> color (colorType cs) (text "forall<" <> ppType cs name <> text ">") <+> text "?")
                                             k <- freshKind
                                             return (name,k)
                      Ambiguous names  -> do let cs = cscheme env
                                             addError range (text "Type" <+> ppType cs name <+> ambiguous cs names)
                                             k <- freshKind
                                             return (name,k)

ambiguous :: ColorScheme -> [Name] -> Doc
ambiguous cs [name1,name2]
  = text "is ambiguous." <-> text " hint: It can refer to either" <+> ppType cs name1 <> text ", or" <+> ppType cs name2
ambiguous cs [name1,name2,name3]
  = text "is ambiguous." <-> text " hint: It can refer to either" <+> ppType cs name1 <> text "," <+> ppType cs name2 <> text ", or" <+> ppType cs name3
ambiguous cs names
  = text "is ambiguous and can refer to multiple imports:" <-> indent 1 (list (map (ppType cs) names))

ppType cs name
  = color (colorType cs) (pretty name)

qualifyDef :: Name -> KInfer Name
qualifyDef name
  = do env <- getKindEnv
       return (qualify (currentModule env) name)
         
findKind :: Name -> KInfer (Name,Kind)
findKind name
  = do env <- getKindEnv
       case kgammaLookup (currentModule env) name (kgamma env) of
        Found qname kind -> return (qname,kind)
        _  -> failure ("Kind.Infer.findKind: unknown type constructor: " ++ show name)

lookupSynInfo :: Name -> KInfer (Maybe SynInfo)
lookupSynInfo name
  = do env <- getKindEnv
       return (synonymsLookup name (synonyms env))
         

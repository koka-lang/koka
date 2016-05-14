-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Type.Assumption (
                    -- * Typothesis
                      Gamma, NameInfo(..)
                    , gammaInit
                    , gammaNew, gammaSingle
                    , gammaEmpty
                    , gammaExtend, gammaExtends
                    , gammaLookup, gammaLookupQ
                    , gammaMap
                    , gammaList
                    , gammaIsEmpty
                    , ppGamma, gammaRemove, gammaUnion, gammaUnions
                    , gammaFilter
                    , isInfoCon
                    , isInfoImport
                    , isInfoFun
                    , isInfoValFunExt
                    , infoElement
                    , infoCanonicalName
                    -- * From Core
                    , extractGammaImports
                    , extractGamma   
                    , coreDefInfo
                    , createNameInfo
                    , createNameInfoX
                    , getArity
                    ) where
import Lib.Trace
import Common.Range
import Common.Failure
import Common.Syntax( DefSort(..) )
import qualified Data.List as L
import Lib.PPrint
import qualified Common.NameMap as M
import Common.Name
import Common.ColorScheme
import Common.Syntax( Visibility(Public), Target )
import Type.Type
import Type.TypeVar
import Type.Pretty
import qualified Core.Core as Core

-- import Lib.Trace

data NameInfo
  = InfoVal{ infoCName :: Name, infoType :: Scheme, infoRange :: Range, infoIsVar :: Bool }
  | InfoFun{ infoCName :: Name, infoType :: Scheme, infoArity :: (Int,Int), infoRange :: Range }
  | InfoCon{ infoType :: Scheme, infoRepr  :: Core.ConRepr, infoCon :: ConInfo, infoRange :: Range }
  | InfoExternal{ infoCName :: Name, infoType :: Scheme, infoFormat :: [(Target,String)], infoRange :: Range }
  | InfoImport{ infoType :: Scheme, infoAlias :: Name, infoFullName :: Name, infoRange :: Range }
  deriving (Show)

infoCanonicalName :: Name -> NameInfo -> Name
infoCanonicalName name info
  = case info of
      InfoVal{}      -> infoCName info
      InfoFun{}      -> infoCName info
      InfoExternal{} -> infoCName info
      _              -> name

isInfoImport :: NameInfo -> Bool
isInfoImport (InfoImport{}) = True
isInfoImport _                    = False

isInfoCon :: NameInfo -> Bool
isInfoCon (InfoCon{}) = True
isInfoCon _                 = False

isInfoValFunExt :: NameInfo -> Bool
isInfoValFunExt (InfoVal{}) = True
isInfoValFunExt (InfoFun{}) = True
isInfoValFunExt (InfoExternal{}) = True
isInfoValFunExt _ = False

isInfoFun :: NameInfo -> Bool
isInfoFun (InfoFun{}) = True
isInfoFun _           = False

infoElement :: NameInfo -> String
infoElement info
  = case info of
      InfoCon{}     -> "constructor"
      InfoImport{}  -> "module"
      _             -> "identifier"


{--------------------------------------------------------------------------
  Initial kind gamma
--------------------------------------------------------------------------}
-- | The intial gamma contains the 'builtinTerms'
gammaInit :: Gamma
gammaInit
  = gammaNew [] --  (builtinCon ++ builtinTerms)

gammaIsEmpty :: Gamma -> Bool
gammaIsEmpty (Gamma g)
  = M.null g

{--------------------------------------------------------------------------
  Gamma
--------------------------------------------------------------------------}
-- | Environment mapping names to type schemes. Due to overloading
-- there may be multiple entries for the same qualified name
newtype Gamma   = Gamma (M.NameMap [(Name,NameInfo)])

gammaSchemes :: Gamma -> [Scheme]
gammaSchemes gamma
  = map (infoType . snd) (gammaList gamma)

gammaEmpty :: Gamma
gammaEmpty
  = Gamma M.empty

gammaSingle :: Name -> NameInfo -> Gamma
gammaSingle name tp
  = gammaNew [(name,tp)]

gammaNew :: [(Name,NameInfo)] -> Gamma
gammaNew xs
  = gammaExtends xs gammaEmpty

gammaExtends :: [(Name,NameInfo)] -> Gamma -> Gamma
gammaExtends xs gamma
  = foldl (\m (name,tp) -> gammaExtend name tp m) gamma xs

gammaExtend :: Name -> NameInfo -> Gamma -> Gamma
gammaExtend name tp (Gamma gamma)
  = Gamma (M.insertWith combine (unqualify name) [(name,tp)] gamma)

combine :: [(Name,NameInfo)] -> [(Name,NameInfo)] -> [(Name,NameInfo)] 
combine xs ys
  = -- TODO: check for overlapping type schemes?
    xs ++ ys


gammaLookupQ :: Name -> Gamma -> [NameInfo]
gammaLookupQ name (Gamma gamma)
  = case M.lookup (unqualify name) gamma of
      Nothing -> []
      Just xs -> map snd (filter (\(n,tp) -> n == name) xs)

-- | @gammaLookup context name gamma@ looks up a potentially qualified name in a module named @context@.
gammaLookup :: Name -> Gamma -> [(Name,NameInfo)]
gammaLookup name (Gamma gamma)
  = case M.lookup (unqualify name) gamma of
      Nothing -> []
      Just xs -> -- let qname = if isQualified name then name else qualify context name 
                 -- in filter (\(n,_) -> n == qname) xs 
                 -- trace (" in gamma found: " ++ show (map fst xs)) $ 
                 if (isQualified name)
                  then filter (\(n,_) -> n == name || nameCaseEqual name n) xs
                  else xs
                 
gammaMap :: (NameInfo -> NameInfo) -> Gamma -> Gamma
gammaMap f (Gamma gamma)
  = Gamma (M.map (\xs -> [(name,f tp) | (name,tp) <- xs]) gamma)


gammaList :: Gamma -> [(Name,NameInfo)]
gammaList (Gamma gamma)
  = L.sortBy (\(n1,_) (n2,_) -> compare (show n1) (show n2)) $ concatMap snd (M.toList gamma)

gammaRemove :: Name -> Gamma -> Gamma
gammaRemove name (Gamma gamma)
  = Gamma (M.delete (unqualify name) gamma)

-- | union
gammaUnion :: Gamma -> Gamma -> Gamma
gammaUnion (Gamma g1) (Gamma g2)
  = Gamma (M.unionWith combine g2 g1)

gammaUnions :: [Gamma] -> Gamma
gammaUnions gs
  = foldr gammaUnion gammaEmpty gs

-- | filter out signatures belonging to just one module
gammaFilter :: Name -> Gamma -> Gamma
gammaFilter mod (Gamma g)
  = Gamma (M.map belongs g)
  where
    belongs xs  = [(name,tp) | (name,tp) <- xs, qualifier name == mod]
  
{---------------------------------------------------------------
  Extract from core
---------------------------------------------------------------}

extractGammaImports :: [(Name,Name)] -> Name -> Gamma
extractGammaImports imports modName
  = -- trace ("extend gamma: " ++ show imports) $
    gammaExtend modAlias (InfoImport typeVoid modAlias modName rangeNull) $
    gammaUnions (L.map extractImport imports)
  where
    modAlias = newName (reverse (takeWhile (/='.') (reverse (nameId modName))))

extractImport (name,qname)
  = gammaSingle name (InfoImport typeVoid name qname rangeNull)

-- | Extract a Gamma from a Core module
extractGamma :: Bool -> Int -> Core.Core -> Gamma
extractGamma publicOnly msf (Core.Core name imports fixDefs tdefgroups defgroups externals doc)
  = gammaUnions [gammaUnions (L.map (extractDefGroup isVisible) defgroups)
                ,gammaUnions (L.map (extractExternal isVisible) externals)
                ,gammaUnions (L.map (extractTypeDefGroup isVisible msf) tdefgroups)
                ]
  where
    isVisible Public  = True
    isVisible _       = not publicOnly

extractTypeDefGroup isVisible msf (Core.TypeDefGroup tdefs)
  = gammaUnions (L.map (extractTypeDef isVisible msf) tdefs)

extractTypeDef isVisible msf tdef
  = case tdef of
     Core.Data dataInfo vis conViss isExtend  | isVisible vis
       -> gammaUnions (L.map extractConInfo 
            [(conInfo, conRepr) | (conInfo,(vis,conRepr)) <- zip (dataInfoConstrs dataInfo) 
               (zip conViss (snd (Core.getDataRepr msf {- struct fields do not matter for extraction -} dataInfo))), isVisible vis])
     _ -> gammaEmpty
  where
    extractConInfo (conInfo,conRepr)
      = gammaSingle (conInfoName conInfo) (InfoCon (conInfoType conInfo) conRepr conInfo (conInfoRange conInfo))


extractDefGroup isVisible (Core.DefRec defs)
  = gammaUnions (L.map (extractDef isVisible) defs)
extractDefGroup isVisible (Core.DefNonRec def)
  = extractDef isVisible def




extractDef isVisible def@(Core.Def name tp expr vis sort nameRng doc) | isVisible vis
  = let info = createNameInfoX name sort nameRng tp -- specials since we cannot call isTopLevel as in coreDefInfo
    in gammaSingle (Core.nonCanonicalName name) info
extractDef isVisible _
  = gammaEmpty


coreDefInfo :: Core.Def -> (Name,NameInfo)
coreDefInfo def@(Core.Def name tp expr vis sort nameRng doc)
  = (Core.nonCanonicalName name,
      createNameInfoX name (if (sort==DefFun && not (Core.isTopLevel def)) then DefVal else sort) nameRng tp)
    -- since we use coreDefInfo also for local definitions, we need to be careful to to use DefFun for
    -- things that do not get lifted to toplevel due to free type/variables. test: codegen/rec5

createNameInfoX :: Name -> DefSort -> Range -> Type -> NameInfo
createNameInfoX name sort rng tp
  = -- trace ("createNameInfoX: " ++ show name ++ ", " ++ show sort ++ ": " ++ show (pretty tp)) $
     if (sort /= DefFun) then InfoVal name tp rng (sort == DefVar) else InfoFun name tp (getArity  tp) rng

createNameInfo name isVal rng tp
  = createNameInfoX name (if isVal then DefVal else DefFun) rng tp
    -- if (isVal) then InfoVal name tp rng False else InfoFun name tp (getArity tp) rng

getArity :: Type -> (Int,Int)
getArity tp
  = case expandSyn tp of
      TForall tvars preds t
        -> case expandSyn t of
             TFun pars eff res -> (length tvars, length pars)
             _                 -> (length tvars, -1 )
      TFun pars eff res        -> (0,length pars)
      _                        -> failure ("Type.Assumption.createNameInfo.getArity: illegal type?" ++ show tp)


extractExternal isVisible (Core.External name tp body vis nameRng doc) | isVisible vis
  = gammaSingle (Core.nonCanonicalName name) (InfoExternal name tp body nameRng)
extractExternal isVisible _
  = gammaEmpty

{--------------------------------------------------------------------------
  Instances
--------------------------------------------------------------------------}
instance Show Gamma where
  show = show . pretty

instance Pretty Gamma where
  pretty g
    = ppGamma Type.Pretty.defaultEnv g
    
    
ppGamma :: Env -> Gamma -> Doc
ppGamma env gamma
    = vcat [fill maxwidth (ppName env name) <> color (colorSep (colors env)) (typeColon (colors env)) <+> align (nice scheme)
        | (name,scheme) <- nameSchemes]
    where
      nameSchemes   = [(name,infoType info) | (name,info) <- gammaList gamma]
      maxwidth      = 12 `min` foldl max 0 [length (show name) | (name,scheme) <- nameSchemes]
      nice scheme   = align (head (niceTypes env [scheme]))


instance HasTypeVar Gamma where
  sub `substitute` (Gamma gamma)
    = Gamma (M.map (\xs -> [(name,sub `substitute` info) | (name,info) <- xs]) gamma)

  ftv gamma
    = ftv (gammaSchemes gamma)

  btv gamma
    = btv (gammaSchemes gamma)

instance HasTypeVar NameInfo where
  sub `substitute` info
    = info{ infoType = sub `substitute` (infoType info) }
  
  ftv info
    = ftv (infoType info)

  btv info
    = btv (infoType info)

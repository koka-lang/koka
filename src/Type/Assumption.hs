-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

module Type.Assumption (
                    -- * Typothesis
                      Gamma, NameInfo(..)
                    , gammaInit
                    , gammaNew, gammaSingle
                    , gammaEmpty
                    , gammaExtend, gammaExtends
                    , gammaLookup, gammaLookupQ, gammaLookupPrefix
                    , gammaLookupCanonical, gammaLookupExactCon -- for core
                    , gammaMap
                    , gammaList
                    , gammaIsEmpty
                    , gammaNames, gammaPublicNames
                    , ppGamma, ppGammaHidden, gammaRemove, gammaUnion, gammaUnions
                    , gammaFilter
                    , isInfoCon
                    , isInfoImport
                    , isInfoFun
                    , isInfoVal
                    , isInfoValFunExt
                    , isInfoFunOrExternal
                    , infoElement
                    , infoDocString
                    , infoCanonicalName
                    , infoSort
                    , fipFromNameInfo
                    -- * From Core
                    , extractGammaImports
                    , extractGamma
                    , coreDefInfo
                    -- , createNameInfo
                    , createNameInfoX
                    , getArity
                    , coreVarInfoFromNameInfo, coreExprFromNameInfo
                    , matchQualifiers
                    , showHidden
                    ) where
import Lib.Trace
import Data.List(isPrefixOf)
import Common.Range
import Common.Failure
import Common.Syntax( DefSort(..), isDefFun, defFun, Fip, noFip )
import qualified Data.List as L
import Lib.PPrint
import qualified Common.NameMap as M
import Common.Name
import Common.ColorScheme
import Common.Syntax( Visibility(..), Target )
import Type.Type
import Type.TypeVar
import Type.Pretty
import qualified Core.Core as Core
import qualified Core.CoreVar as CoreVar

import Lib.Trace
import Syntax.Syntax (TypeDef(typeDefDoc))

data NameInfo
  = InfoVal{ infoVis :: !Visibility, infoCName :: !Name, infoType :: !Scheme, infoRange :: !Range, infoIsVar :: !Bool, infoDoc :: !String }
  | InfoFun{ infoVis :: !Visibility, infoCName :: !Name, infoType :: !Scheme, infoArity :: !(Int,Int), infoFip :: !Fip, infoRange :: !Range, infoDoc :: !String }
  | InfoCon{ infoVis :: !Visibility, infoType :: !Scheme, infoRepr  :: !Core.ConRepr, infoCon :: !ConInfo, infoRange :: !Range, infoDoc :: !String }
  | InfoExternal{ infoVis :: !Visibility, infoCName :: !Name, infoType :: !Scheme, infoFormat :: ![(Target,String)], infoFip :: !Fip, infoRange :: !Range, infoDoc :: !String}
  | InfoImport{ infoVis :: !Visibility, infoType :: !Scheme, infoAlias :: !Name, infoFullName :: !Name, infoRange :: !Range}
  deriving (Show)

infoSort :: NameInfo -> String
infoSort info
  = case info of
      InfoVal{}      -> "val"
      InfoFun{}      -> "fun"
      InfoExternal{} -> "extern"
      InfoCon{}      -> "con"
      InfoImport{}   -> "module"


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

isInfoVal :: NameInfo -> Bool
isInfoVal (InfoVal{}) = True
isInfoVal _           = False

isInfoValFunExt :: NameInfo -> Bool
isInfoValFunExt (InfoVal{}) = True
isInfoValFunExt (InfoFun{}) = True
isInfoValFunExt (InfoExternal{}) = True
isInfoValFunExt _ = False

isInfoFun :: NameInfo -> Bool
isInfoFun (InfoFun{}) = True
isInfoFun _           = False

isInfoFunOrExternal :: NameInfo -> Bool
isInfoFunOrExternal (InfoFun{})      = True
isInfoFunOrExternal (InfoExternal{}) = True
isInfoFunOrExternal _                = False

fipFromNameInfo :: NameInfo -> Fip
fipFromNameInfo (InfoFun{infoFip=fip})      = fip
fipFromNameInfo (InfoExternal{infoFip=fip}) = fip
fipFromNameInfo _ = noFip

infoDocString :: NameInfo -> String
infoDocString (InfoVal{infoDoc=doc}) = doc
infoDocString (InfoFun{infoDoc=doc}) = doc
infoDocString (InfoCon{infoDoc=doc}) = doc
infoDocString (InfoExternal{infoDoc=doc}) = doc
infoDocString _ = ""

infoElement :: NameInfo -> String
infoElement info
  = case info of
      InfoCon{}     -> "constructor"
      InfoImport{}  -> "module"
      _             -> "identifier"

infoIsVisible :: NameInfo -> Bool
infoIsVisible info = case infoVis info of
                     Public -> True
                     _      -> False


coreVarInfoFromNameInfo :: NameInfo -> Core.VarInfo
coreVarInfoFromNameInfo info
  = case info of
      InfoVal _ _ tp _ _ _             -> Core.InfoNone
      InfoFun _ _ tp (m,n) _ _ _       -> Core.InfoArity m n
      InfoExternal _ _ tp format _ _ _ -> Core.InfoExternal format
      _                                -> matchFailure "Type.Infer.coreVarInfoFromNameInfo"

coreExprFromNameInfo qname info
  = -- trace ("create name: " ++ show qname) $
    case info of
      InfoVal vis cname tp _ _ _             -> Core.Var (Core.TName cname tp) (Core.InfoNone)
      InfoFun vis cname tp ((m,n)) _ _ _     -> Core.Var (Core.TName cname tp) (Core.InfoArity m n)
      InfoCon vis  tp repr _ _ _             -> Core.Con (Core.TName qname tp) repr
      InfoExternal vis cname tp format _ _ _ -> Core.Var (Core.TName cname tp) (Core.InfoExternal format)
      InfoImport _ _ _ _ _                   -> matchFailure "Type.Infer.coreExprFromNameInfo"


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
  = Gamma (M.insertWith combine (unqualifyFull name) [(name,tp)] gamma)

combine :: [(Name,NameInfo)] -> [(Name,NameInfo)] -> [(Name,NameInfo)]
combine xs ys
  = -- TODO: check for overlapping type schemes?
    xs ++ ys

gammaLookupCanonical:: Name -> Gamma -> [NameInfo]
gammaLookupCanonical name gamma
  = let xs = (gammaLookupQ name {-(nonCanonicalName name)-} gamma)
    in -- trace ("gamma lookup canonical: " ++ show name ++ " in " ++ show xs) $
       filter (\ni -> infoCanonicalName nameNil ni == name) xs

gammaLookupExactCon :: Name -> Gamma -> [NameInfo]
gammaLookupExactCon name gamma
 = let xs = (gammaLookupQ name gamma)
   in -- trace ("gamma lookup canonical: " ++ show name ++ " in " ++ show xs) $
      filter isInfoCon xs


-- Return exactly matching qualified names
gammaLookupQ :: Name -> Gamma -> [NameInfo]
gammaLookupQ name (Gamma gamma)
  = case M.lookup (unqualifyFull name) gamma of
      Nothing -> []
      Just xs -> -- trace ("gamma lookupQ: " ++ show name ++ " in " ++ show xs) $
                 map snd (filter (\(n,tp) -> n == name) xs)

-- | @gammaLookup name gamma@ looks up a potentially (partially) qualified name and returns all matches.
gammaLookup :: Name -> Gamma -> [(Name,NameInfo)]
gammaLookup name (Gamma gamma)
  = let stemName = unqualifyFull name
    in case M.lookup stemName gamma of
      Nothing -> []
      Just candidates0
         -> -- trace ("gamma lookup: " ++ show name ++ ": " ++ show (map fst candidates)) $
            let candidates1 = filter (\(_,info) -> infoIsVisible info) candidates0
            in  if stemName == name then candidates1  -- fast path for unqualified names
                 else let qpaths      = splitRevQualifiers name
                          candidates2 = filter (\(n,_) -> matchRevQualifierPaths qpaths (splitRevQualifiers n)) candidates1
                      in -- trace ("gamma lookup matched: " ++ show name ++ ": " ++ show (map fst candidates2)) $
                          candidates2

-- Given a user qualified name, see if the qualifiers match a resolved name.
-- The user qualified name has already been de-aliased in kind inference (see `Kind/ImportMap/importsExpand`)
-- Note that the user qualified name might not distinguish local qualification from module qualification,
-- e.g. `std/core/int/show` vs  `std/core/#int/show`.
-- ambiguities may occur, where `std/num/float32/foo` should match both `std/num/#float32/foo` and `std/num/float32/#foo`
matchQualifiers :: Name -> Name -> Bool
matchQualifiers uname name
  = matchRevQualifierPaths (splitRevQualifiers uname) (splitRevQualifiers name)

matchRevQualifierPaths :: ([String],[String]) -> ([String],[String]) -> Bool
matchRevQualifierPaths upaths paths
  = matchPaths upaths paths
  where
    -- not qualified
    matchPaths ([],[]) (mpath,lpath)
      = True

    -- no user specified local path
    matchPaths (umpath,[]) (mpath,[])
      = umpath `isPrefixOf` mpath

    matchPaths (umpath,[]) (mpath,lpath)  -- not (null lpath)
      = (umpath `isPrefixOf` lpath) ||    -- user module is a postfix the local qualifier
        (lpath `isPrefixOf` umpath && (drop (length lpath) umpath) `isPrefixOf` mpath) ||  -- stradle both
        (umpath `isPrefixOf` mpath)       -- user module is postfix of the module qualifier
                                          -- (we can not mention local qualifiers, so `std/core/show` matches `std/core/#int/show` for example)

    -- user specified local path: umpath/#ulpath must be a valid postfix of  mpath/#lpath
    matchPaths (umpath,ulpath) (mpath,lpath)  -- not (null ulpath)
      = case umpath of
          [] -> ulpath `isPrefixOf` lpath
          _  -> ulpath == lpath && umpath `isPrefixOf` mpath


-- Split out the module and local qualifier as a _reverse_ list of components
-- e.g. `std/core/#int/show` -> (["core","std"],["int"])
splitRevQualifiers :: Name -> ([String],[String])
splitRevQualifiers name
  = let mpath = reverse (splitModuleName name)
        lpath = reverse (splitLocalQualName name)
    in (mpath,lpath)



gammaLookupPrefix :: Name -> Gamma -> [(Name,NameInfo)]
gammaLookupPrefix name (Gamma gamma)
  = assertion "Assumption.gammaLookupPrefix"  (not (isQualified name)) $
    filter (\(_,info) -> infoIsVisible info) $ concat $ M.elems $ M.filterWithKey isPrefix gamma
  where
    pre            = showPlain (unqualify name) ++ "_"
    isPrefix nm _  = (nm == name) || nameStartsWith nm pre


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

gammaNames :: Gamma -> [Name]
gammaNames (Gamma g)
  = M.keys g

gammaPublicNames :: Gamma -> [Name]
gammaPublicNames (Gamma g)
  = [name | (name,ninfos) <- M.toList g, all (infoIsVisible . snd) ninfos && not (isHiddenName name)]

{---------------------------------------------------------------
  Extract from core
---------------------------------------------------------------}

extractGammaImports :: [(Name,Name)] -> Name -> Gamma
extractGammaImports imports modName
  = -- trace ("extend gamma: " ++ show imports) $
    gammaExtend modAlias (InfoImport Private typeVoid modAlias modName rangeNull) $
    gammaUnions (L.map extractImport imports)
  where
    modAlias = modName -- newName (reverse (takeWhile (/='.') (reverse (nameId modName))))

extractImport (name,qname)
  = gammaSingle name (InfoImport Private typeVoid name qname rangeNull)

-- | Extract a Gamma from a Core module
extractGamma :: (DataInfo -> Bool) -> Bool -> Core.Core -> Gamma
extractGamma isValue privateAsPublic (Core.Core name imports fixDefs tdefgroups defgroups externals doc)
  = gammaUnions [gammaUnions (L.map (extractDefGroup updateVis) defgroups)
                ,gammaUnions (L.map (extractExternal updateVis) externals)
                ,gammaUnions (L.map (extractTypeDefGroup isValue updateVis) tdefgroups)
                ]
  where
    updateVis Public  = Public
    updateVis Private = if (privateAsPublic) then Public else Private


extractTypeDefGroup isValue updateVis (Core.TypeDefGroup tdefs)
  = gammaUnions (L.map (extractTypeDef isValue updateVis) tdefs)

extractTypeDef :: (DataInfo -> Bool) -> (Visibility -> Visibility) -> Core.TypeDef -> Gamma
extractTypeDef isValue updateVis tdef
  = case tdef of
     Core.Data dataInfo isExtend
       -> gammaUnions (L.map extractConInfo
            [(conInfo, conRepr) | (conInfo,conRepr) <- zip (dataInfoConstrs dataInfo)
                 (snd (Core.getDataReprEx isValue dataInfo))] )
     _ -> gammaEmpty
  where
    extractConInfo (conInfo,conRepr)
      = gammaSingle (conInfoName conInfo) (InfoCon (updateVis (conInfoVis conInfo)) (conInfoType conInfo) conRepr conInfo (conInfoRange conInfo) (Core.typeDefDoc tdef))


extractDefGroup updateVis (Core.DefRec defs)
  = gammaUnions (L.map (extractDef updateVis) defs)
extractDefGroup updateVis (Core.DefNonRec def)
  = extractDef updateVis def




extractDef updateVis def@(Core.Def name tp expr vis sort inl nameRng doc)
  = let info = createNameInfoX (updateVis vis) name sort nameRng tp doc -- specials since we cannot call isTopLevel as in coreDefInfo
    in gammaSingle name {- (nonCanonicalName name) -} info


coreDefInfo :: Core.Def -> (Name,NameInfo)
coreDefInfo def@(Core.Def name tp expr vis sort inl nameRng doc)
  = (name {- nonCanonicalName name -},
      createNameInfoX vis name (if (isDefFun sort && not (CoreVar.isTopLevel def)) then DefVal else sort) nameRng tp doc)
    -- since we use coreDefInfo also for local definitions, we need to be careful to to use DefFun for
    -- things that do not get lifted to toplevel due to free type/variables. test: codegen/rec5

createNameInfoX :: Visibility -> Name -> DefSort -> Range -> Type -> String -> NameInfo
createNameInfoX vis name sort rng tp doc
  = -- trace ("createNameInfoX: " ++ show name ++ ", " ++ show sort ++ ": " ++ show (pretty tp)) $
    case sort of
      DefFun _ fip -> InfoFun vis name tp (getArity tp) fip rng doc
      DefVar       -> InfoVal vis name tp rng True doc
      _            -> InfoVal vis name tp rng False doc

createNameInfo name isVal rng tp
  = createNameInfoX Public name (if isVal then DefVal else defFun []) rng tp
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


extractExternal updateVis (Core.External name tp pinfos body vis fip nameRng doc)
  = gammaSingle name {- (nonCanonicalName name) -} (InfoExternal (updateVis vis) name tp body fip nameRng doc)
extractExternal updateVis _
  = gammaEmpty

{--------------------------------------------------------------------------
  Instances
--------------------------------------------------------------------------}
instance Show Gamma where
  show = show . pretty

instance Pretty Gamma where
  pretty g
    = ppGamma Type.Pretty.defaultEnv g

ppGammaInternal :: Bool -> Env -> Gamma -> Doc
ppGammaInternal showHidden env gamma
    = vcat [fill maxwidth (prettyName (colors env) name) {-(ppName env name)-} <.>
             color (colorSep (colors env)) (typeColon (colors env)) <+> align (nice scheme)
        | (name,scheme) <- nameSchemes,
          showHidden || not (isHiddenName name)
        ]
    where
      nameSchemes   = [(name,infoType info) | (name,info) <- gammaList gamma, not (isInfoImport info)]
      maxwidth      = 12 `min` foldl max 0 [length (show name) | (name,scheme) <- nameSchemes]
      nice scheme   = align (head (niceTypes env [scheme]))

ppGamma :: Env -> Gamma -> Doc
ppGamma = ppGammaInternal False

ppGammaHidden :: Env -> Gamma -> Doc
ppGammaHidden = ppGammaInternal True

showHidden :: Gamma -> String
showHidden g = show (ppGammaHidden Type.Pretty.defaultEnv g)

instance HasTypeVar Gamma where
  sub `substitute` (Gamma gamma)
    = Gamma (M.map (\xs -> [(name,sub `substitute` info) | (name,info) <- xs]) gamma)

  ftv gamma
    = ftv (gammaSchemes gamma)

  btv gamma
    = btv (gammaSchemes gamma)

  ftc gamma
    = ftc (gammaSchemes gamma)

instance HasTypeVar NameInfo where
  sub `substitute` info
    = info{ infoType = sub `substitute` (infoType info) }

  ftv info
    = ftv (infoType info)

  btv info
    = btv (infoType info)

  ftc info
    = ftc (infoType info)

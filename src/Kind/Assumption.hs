------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Kind assumptions
-}
-----------------------------------------------------------------------------

module Kind.Assumption (
                    -- * Kindothesis
                      KGamma
                    , kgammaInit 
                    , kgammaNew, kgammaNewNub
                    , kgammaEmpty, kgammaIsEmpty
                    , kgammaExtend
                    , kgammaLookup, kgammaLookupQ, kgammaFind
                    , kgammaList
                    , kgammaUnion
                    , ppKGamma
                    , kgammaFilter
                    -- * Extraction from Core
                    , extractKGamma 
                    ) where

import qualified Data.List as L
import Prelude        hiding (filter,lookup,map)
import Lib.PPrint     hiding (empty)
import Common.ColorScheme
import Common.QNameMap
import Common.Name
import Kind.Kind
import Kind.Pretty( prettyKind, kindColon )  
import Kind.ImportMap

import Type.Type
import qualified Core.Core as Core

{--------------------------------------------------------------------------
  Initial kind gamma
--------------------------------------------------------------------------}
-- | The initial kind gamma contains the 'builtinTypes'.
kgammaInit :: KGamma
kgammaInit
  = kgammaEmpty


kgammaIsEmpty :: KGamma -> Bool
kgammaIsEmpty (KGamma qm)
  = isEmpty qm

{--------------------------------------------------------------------------
  KGamma
--------------------------------------------------------------------------}
-- | Environment mapping types to kind schemes
newtype KGamma   = KGamma {unKGamma :: QNameMap Kind}

kgammaEmpty :: KGamma
kgammaEmpty
  = KGamma empty

kgammaSingle :: Name -> Kind -> KGamma
kgammaSingle name kind
  = KGamma (single name kind)

kgammaNew :: [(Name,Kind)] -> KGamma
kgammaNew xs
  = KGamma (fromList xs)

kgammaNewNub :: [(Name,Kind)] -> KGamma
kgammaNewNub xs
  = KGamma (fromList (L.nubBy (\(n1,x) (n2,y) -> n1 == n2) xs))



kgammaExtend :: Name -> Kind -> KGamma -> KGamma
kgammaExtend name scheme (KGamma kgamma)
  = KGamma (insert name scheme kgamma)

kgammaLookup :: Name -> Name -> KGamma -> Lookup Kind
kgammaLookup context name (KGamma kgamma) 
  = lookup context name kgamma
      
-- | Lookup a fully qualified name
kgammaLookupQ :: Name -> KGamma -> Maybe Kind
kgammaLookupQ name (KGamma kgamma) 
  = lookupQ name kgamma

kgammaFind :: Name -> Name -> KGamma -> (Name,Kind)
kgammaFind context name kg@(KGamma kgamma)
  = case kgammaLookup context name kg of
      Found qname scheme -> (qname,scheme)
      _ -> error ("Kind.Assumption.kgammaFind: unbound type '" ++ show name ++ "' in " ++ show kgamma)

kgammaList :: KGamma -> [(Name,Kind)]
kgammaList (KGamma kgamma)
  = L.sortBy (\(n1,_) (n2,_) -> compare (show n1) (show n2)) (toAscList kgamma)

kgammaFilter :: Name -> KGamma -> KGamma
kgammaFilter modName (KGamma kgamma) 
  = KGamma (filterNames (\name -> qualifier name == modName) kgamma)

-- | kind gamma union; error on duplicates
kgammaUnion :: KGamma -> KGamma -> KGamma
kgammaUnion (KGamma g1) (KGamma g2)
  = KGamma (union g2 g1)

-- | union of disjoint kind gammas
kgammaUnions :: [KGamma] -> KGamma
kgammaUnions ks
  = KGamma (unions (L.map unKGamma ks))

instance Show KGamma where
  show = show . pretty

instance Pretty KGamma where
  pretty kgamma
    = ppKGamma defaultColorScheme nameNil importsEmpty kgamma
  
  
ppKGamma :: ColorScheme -> Name -> ImportMap -> KGamma -> Doc
ppKGamma cscheme context imports kgamma
  = vcat [fill maxwidth (ppName name) <.> kindColon cscheme <+> prettyKind cscheme scheme 
          | (name,scheme) <- nameSchemes]
  where
    nameSchemes       = kgammaList kgamma 
    maxwidth          = 12 `min` foldl max 0 [length (show name) | (name,_) <- nameSchemes]
    (KGamma builtins) = kgammaInit

    ppName name = if (qualifier name == context) 
                    then pretty (unqualify name) 
                    else pretty (importsAlias name imports)

-- | Extract a KGamma from a Core module
extractKGamma :: Core.Core -> KGamma
extractKGamma (core)
  = kgammaUnions (L.map extractTypeDefGroup (Core.coreProgTypeDefs core))

extractTypeDefGroup (Core.TypeDefGroup tdefs)
  = kgammaUnions (L.map extractTypeDef tdefs)

extractTypeDef :: Core.TypeDef -> KGamma
extractTypeDef tdef
  = case tdef of
      Core.Synonym synInfo Core.Public 
        -> kgammaSingle (synInfoName synInfo) (synInfoKind synInfo)
      Core.Data dataInfo Core.Public conviss False
        -> kgammaSingle (dataInfoName dataInfo) (dataInfoKind dataInfo)
      _ -> kgammaEmpty

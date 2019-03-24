-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Type.InfGamma (
                    -- * Typothesis
                      InfGamma
                    , infgammaNew
                    , infgammaSingle
                    , infgammaEmpty
                    , infgammaIsEmpty
                    , infgammaExtend
                    , infgammaExtends
                    , infgammaLookup
                    , infgammaMap
                    , infgammaList
                    , ppInfGamma
                    , infgammaUnion
                    , infgammaLookupX
                    , infgammaExtendX
                    ) where

import Lib.PPrint
import qualified Common.NameMap as M

import Common.Range
import Common.Name
import Common.ColorScheme

import Type.Type
import Type.TypeVar
import Type.Pretty
import Type.Assumption( NameInfo(..) )

{--------------------------------------------------------------------------
  InfGamma
--------------------------------------------------------------------------}
-- | Environment mapping names to type schemes. Due to overloading
-- there may be multiple entries for the same name
newtype InfGamma   = InfGamma (M.NameMap NameInfo)

-- data InfInfo = InfInfo { infName :: Name, infType :: Type, infRange :: Range, infSort :: DefSort }

infgammaEmpty :: InfGamma
infgammaEmpty
  = InfGamma M.empty

infgammaIsEmpty :: InfGamma -> Bool
infgammaIsEmpty (InfGamma infGamma)
  = M.null infGamma

infgammaSingle :: Name -> Scheme -> InfGamma
infgammaSingle name tp
  = infgammaNew [(name,tp)]

infgammaNew :: [(Name,Scheme)] -> InfGamma
infgammaNew xs
  = infgammaExtends xs infgammaEmpty 

infgammaExtend :: Name -> NameInfo -> InfGamma -> InfGamma
infgammaExtend name info (InfGamma infgamma)
  = InfGamma (M.insert (unqualify name) info infgamma)  -- overwrite previous names

infgammaExtendTp :: Name -> Name -> Scheme -> InfGamma -> InfGamma
infgammaExtendTp name cname tp infgamma
  = infgammaExtendX name cname tp rangeNull False infgamma

infgammaExtendX :: Name -> Name -> Scheme -> Range -> Bool -> InfGamma -> InfGamma
infgammaExtendX name cname tp rng isVar infgamma
  = infgammaExtend name (InfoVal cname tp rng isVar) infgamma 

infgammaExtends :: [(Name,Scheme)] -> InfGamma -> InfGamma
infgammaExtends tnames ig
  = foldl (\m (name,tp) -> infgammaExtendTp name name tp m) ig tnames

infgammaLookup :: Name -> InfGamma -> Maybe (Name,Type)
infgammaLookup name infgamma
  = fmap (\info -> (infoCName info, infoType info)) (infgammaLookupX name infgamma)

 
infgammaLookupX :: Name -> InfGamma -> Maybe NameInfo
infgammaLookupX name (InfGamma infgamma)
  = M.lookup name infgamma 

infgammaMap :: (Scheme -> Scheme) -> InfGamma -> InfGamma
infgammaMap f (InfGamma infgamma)
  = InfGamma (M.map (\info -> info{ infoType = f (infoType info)}) infgamma)


infgammaList :: InfGamma -> [(Name,Scheme)]
infgammaList (InfGamma infgamma)
  = [(name,infoType info) | (name,info) <- M.toAscList infgamma]


-- | right-biased union
infgammaUnion :: InfGamma -> InfGamma -> InfGamma
infgammaUnion (InfGamma g1) (InfGamma g2)
  = InfGamma (M.union g2 g1)

infgammaUnions :: [InfGamma] -> InfGamma
infgammaUnions gs
  = foldr infgammaUnion infgammaEmpty gs


instance Show InfGamma where
  show = show . pretty

instance Pretty InfGamma where
  pretty g
    = ppInfGamma Type.Pretty.defaultEnv g
    
    
ppInfGamma :: Env -> InfGamma -> Doc
ppInfGamma env infgamma
    = vcat [fill maxwidth (ppName env name) <.> color (colorSep (colors env)) (typeColon (colors env)) <+> align (nice scheme)
        | (name,scheme) <- nameSchemes]
    where
      nameSchemes   = infgammaList infgamma
      maxwidth      = 12 `min` foldl max 0 [length (show name) | (name,scheme) <- nameSchemes]
      nice scheme   = align (head (niceTypes env [scheme]))


instance HasTypeVar InfGamma where
  sub `substitute` (InfGamma infgamma)
    = InfGamma (M.map (\info -> sub `substitute` info) infgamma)

  ftv infgamma
    = ftv (map snd (infgammaList infgamma))

  btv infgamma
    = btv (map snd (infgammaList infgamma))

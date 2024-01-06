-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
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
                    , infgammaLookupEx
                    , infgammaExtendX
                    ) where

import Lib.PPrint
import qualified Common.NameMap as M
import Debug.Trace
import Common.Range
import Common.Name
import Common.ColorScheme
import Common.Syntax( Visibility(..) )
import Type.Type
import Type.TypeVar
import Type.Pretty
import Type.Assumption( NameInfo(..), matchQualifiers )

{--------------------------------------------------------------------------
  InfGamma
--------------------------------------------------------------------------}
-- | Environment mapping names to type schemes.
newtype InfGamma   = InfGamma (M.NameMap [NameInfo])

-- data InfInfo = InfInfo { infName :: Name, infType :: Type, infRange :: Range, infSort :: DefSort }

infgammaEmpty :: InfGamma
infgammaEmpty
  = InfGamma M.empty

infgammaIsEmpty :: InfGamma -> Bool
infgammaIsEmpty (InfGamma infGamma)
  = M.null infGamma

infgammaSingle :: Name -> Scheme -> String -> InfGamma
infgammaSingle name tp doc
  = infgammaNew [(name,tp,doc)]

infgammaNew :: [(Name,Scheme,String)] -> InfGamma
infgammaNew xs
  = infgammaExtends xs infgammaEmpty

infgammaExtend :: Name -> NameInfo -> InfGamma -> InfGamma
infgammaExtend name info (InfGamma infgamma)
  = InfGamma (M.insertWith combine (unqualifyFull name) [info] infgamma)  -- overwrite previous names

combine :: [NameInfo] -> [NameInfo] -> [NameInfo]
combine [] ys = ys
combine (x:xx) ys
  = combine xx (x : filter (\y -> infoCName y /= infoCName x) ys)

infgammaExtendTp :: Name -> Name -> Scheme -> String -> InfGamma -> InfGamma
infgammaExtendTp name cname tp doc infgamma
  = infgammaExtendX name cname tp rangeNull False doc infgamma

infgammaExtendX :: Name -> Name -> Scheme -> Range -> Bool -> String -> InfGamma -> InfGamma
infgammaExtendX name cname tp rng isVar doc infgamma
  = infgammaExtend name (InfoVal Public cname tp rng isVar doc) infgamma

infgammaExtends :: [(Name,Scheme,String)] -> InfGamma -> InfGamma
infgammaExtends tnames ig
  = foldl (\m (name,tp,doc) -> infgammaExtendTp name name tp doc m) ig tnames

infgammaLookup :: Name -> InfGamma -> Maybe (Name,Type)
infgammaLookup name infgamma
  = case infgammaLookupX name infgamma of
      Just info -> Just (infoCName info, infoType info)
      Nothing   -> Nothing

infgammaLookupX :: Name -> InfGamma -> Maybe NameInfo
infgammaLookupX name infgamma
  = case infgammaLookupEx name infgamma of
      [info] | unqualify (infoCName info) == unqualify name -> Just info
      _      -> Nothing

isMatch name info
  = -- note: the only qualified names in infgamma are from local recursive definitions (so we can safely unqualify)
    matchQualifiers name (infoCName info)

infgammaLookupEx :: Name -> InfGamma -> [NameInfo]
infgammaLookupEx name (InfGamma infgamma)
  = let mbcandidates = M.lookup (unqualifyFull name) infgamma
    in case mbcandidates of
         Nothing -> []
         Just candidates -> filter (isMatch name) candidates


infgammaMap :: (Scheme -> Scheme) -> InfGamma -> InfGamma
infgammaMap f (InfGamma infgamma)
  = InfGamma (M.map (\infos -> map (\info -> info{ infoType = f (infoType info)}) infos) infgamma)


infgammaList :: InfGamma -> [(Name,Scheme)]
infgammaList (InfGamma infgamma)
  = [(infoCName info, infoType info) | (_,infos) <- M.toAscList infgamma, info <- infos]


-- | right-biased union
infgammaUnion :: InfGamma -> InfGamma -> InfGamma
infgammaUnion (InfGamma g1) (InfGamma g2)
  = InfGamma (M.unionWith combine g2 g1)

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

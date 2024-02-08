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
                    , infgammaLookupEx
                    , infgammaLookup
                    , infgammaMap
                    , infgammaList
                    , ppInfGamma
                    , infgammaUnion
                    , infgammaExtendX
                    ) where

import Debug.Trace
import Lib.PPrint
import qualified Common.NameMap as M
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
  = -- note: qualified names can get inserted into gamma due to recursive definitions, or
    -- during inference by inserting fully qualified expressions (in resolveAppName)
    -- (if isQualified name then trace ("infGammaExtend: insert qualified: " ++ show name) else id) $
    InfGamma (M.insertWith combine (unqualifyFull name) [info] infgamma)  -- overwrite previous names

combine :: [NameInfo] -> [NameInfo] -> [NameInfo]
combine [] ys = ys
combine (x:xx) ys
  = combine xx (x : filter (\y -> infoCName y /= infoCName x) ys)

infgammaExtends :: [(Name,Scheme,String)] -> InfGamma -> InfGamma
infgammaExtends tnames ig
  = foldl (\m (name,tp,doc) -> infgammaExtendTp name name tp doc m) ig tnames

infgammaExtendTp :: Name -> Name -> Scheme -> String -> InfGamma -> InfGamma
infgammaExtendTp name cname tp doc infgamma
  = infgammaExtendX name cname tp rangeNull False doc infgamma

infgammaExtendX :: Name -> Name -> Scheme -> Range -> Bool -> String -> InfGamma -> InfGamma
infgammaExtendX name cname tp rng isVar doc infgamma
  = infgammaExtend name (InfoVal Public cname tp rng isVar doc) infgamma


-- lookup any exact match in the local scope
-- note that recursive definitions add the fully qualified name to the infgamma,
-- in that case we also consider it an exact match with just the stem.
infgammaLookup :: Name -> InfGamma -> Either [(Name,NameInfo)] (Name,NameInfo)
infgammaLookup name infgamma
  = case infgammaLookupEx (const True) name infgamma of
      Left [(name,info)] -> Right (name,info)
      other              -> other


-- lookup a local name: return either a list of possibly matching (locally qualified) names, or an exact match
infgammaLookupEx :: (NameInfo -> Bool) -> Name -> InfGamma -> Either [(Name,NameInfo)] (Name,NameInfo)
infgammaLookupEx guard name (InfGamma infgamma)
  = let mbinfos = M.lookup (unqualifyFull name) infgamma
    in case mbinfos of
         Nothing    -> Left []
         Just infos -> let lqname = requalifyLocally name in  -- maybe it is a locally qualified name?
                       -- trace ("infgammaLookupEx: " ++ show (name,lqname) ++ ": " ++ show (map infoCName infos)) $
                       case filter (\info -> guard info && infoCName info == lqname) infos of
                        (info:_) -> -- first exact match in the local scope
                                    Right (infoCName info, info)
                        _        -> -- match qualifiers
                                    case filter (\info -> guard info && matchQualifiers name (infoCName info)) infos of
                                      -- recursive definitions add the fully qualified name to the infgamma;
                                      -- we cannot consider it an exact match though as another global definition may match better.
                                      -- see also issue #433
                                      {-
                                      [info]    -- recursive definitions add the fully qualified name to the infgamma; in that case we also consider it an exact match
                                                | unqualify (infoCName info) == lqname -> Right (infoCName info, info)
                                      -}
                                      matches  -> Left [(infoCName info, info) | info <- matches]


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

  ftc infgamma
    = ftc (map snd (infgammaList infgamma))

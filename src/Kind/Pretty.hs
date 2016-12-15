------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Show kinds
-}
-----------------------------------------------------------------------------
module Kind.Pretty( -- * Pretty
                    prettyKind, ppKind, niceKinds
                    -- * Util
                  , colorKind, kindColon
                  ) where

import Lib.PPrint
import Common.ColorScheme
import Common.Name
import Kind.Kind
import Common.Failure

kindColon colors
  = color (colorSep colors) (text ":")

keyword colors s
  = color (colorKeyword colors) (text s)

{--------------------------------------------------------------------------
  Generate nice quantifier names
--------------------------------------------------------------------------}
niceKinds :: ColorScheme -> [Kind] -> [Doc]
niceKinds colors kinds
  = map (ppKind colors precTop) kinds

prettyKind :: ColorScheme -> Kind -> Doc
prettyKind cscheme k
  = ppKind cscheme precTop k

{--------------------------------------------------------------------------
  Show
--------------------------------------------------------------------------}

instance Pretty Kind where
  pretty kind
    = ppKind defaultColorScheme precTop kind


-- | Precedence
type Prec = Int

precTop, precQuant,precArrow,precApp,precAtom :: Int
precTop   = 0
precQuant = 1
precArrow = 2
precApp   = 3
precAtom  = 4

pparens :: Prec -> Prec -> Doc -> Doc
pparens context prec doc
  | context >= prec = parens doc
  | otherwise       = doc


ppKind :: ColorScheme -> Prec -> Kind -> Doc
ppKind cscheme prec kind
  = color (colorKind cscheme) $
    case kind of
      KCon name      -> pretty name
      KApp (KApp (KCon name) k1) k2 | name == newName "->"
                     -> pparens prec precArrow $
                        case collectFunArgs k2 of
                          [res] -> ppKind cscheme precArrow k1 <+> text "->" <+> ppKind cscheme (precArrow-1) res
                          (args) -> commaParens (ppKind cscheme precTop) (k1:init args) <+> text "->" <+> ppKind cscheme (precArrow-1) (last args)
                          
      KApp k1 k2     -> pparens prec precApp $
                        case collectArgs kind of
                          (k:ks) -> ppKind cscheme (precApp-1) k <> commaParens (ppKind cscheme precTop) ks
                          _     -> matchFailure "Kind.Pretty.ppKind.KApp"

  where
    commaParens f xs
      = tupled (map f xs)

    collectFunArgs kind
      = case kind of
          KApp (KApp (KCon name) k1) k2 | name == newName "->"
            -> k1 : collectFunArgs k2
          _ -> [kind]
          
    collectArgs kind
      = case kind of
          KApp k1 k2  -> collectArgs k1 ++ [k2]
          _           -> [kind]

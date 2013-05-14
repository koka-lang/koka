-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
module Type.Kind ( HasKind( getKind ) ) where

import Common.Failure( failure )
import Kind.Kind
import Type.Type

{--------------------------------------------------------------------------
  Get the kind of a type.
--------------------------------------------------------------------------}
class HasKind a where
  -- | Return the kind of type
  getKind :: a -> Kind


instance HasKind Kind where
  getKind k = k

instance HasKind TypeVar where
  getKind (TypeVar id k _) = k

instance HasKind TypeCon where
  getKind (TypeCon id k) = k


instance HasKind TypeSyn where
  getKind (TypeSyn id k rank _) = k

instance HasKind Type where
  getKind tau
    = case tau of
        TForall _ _ tp -> getKind tp
        TFun _ _ _     -> kindStar
        TVar v         -> getKind v
        TCon c         -> getKind c
        TSyn syn xs tp -> -- getKind tp {- this is wrong for partially applied type synonym arguments, see "kind/alias3" test -}
                          kindApply xs (getKind syn)  
        TApp tp args   -> case collect [] (getKind tp) of
                            (kres:_) -> kres
                            _  -> failure ("Type.Kind: illegal kind in type application? " ++ show (getKind tp) )
    where
      collect :: [Kind] -> Kind -> [Kind]
      collect acc (KApp (KApp arr k1) k2) | arr == kindArrow  = collect (k1:acc) k2
      collect acc k  = k:acc

      kindApply [] k   = k
      kindApply (_:rest) (KApp (KApp arr k1) k2)  = kindApply rest k2
      kindApply _  k   = failure ("Type.Kind.kindApply: illegal kind in application? " ++ show k)




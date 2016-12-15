------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{--}
-----------------------------------------------------------------------------

module Kind.InferKind ( InfKind(..)
                      , infKindStar, infKindHandled
                      , infKindFun
                      , infKindFunN
                      , ppInfKind, niceInfKinds

                      , InfKGamma
  
                      , Kvs
                      , kvsList, kvsMember

                      , KSub
                      , ksubEmpty, ksubSingle, (@@)
                      , HasKindVar( (|=>), fkv )
                      ) where


import Lib.PPrint
import Common.Failure
import Common.Id
import Common.IdNice
import Common.NamePrim( nameKindFun )
import Common.Name
import Common.ColorScheme
import qualified Common.NameMap  as M
import Kind.Kind
import Kind.Pretty

import qualified Common.IdMap as IM
import qualified Common.IdSet as IS

{---------------------------------------------------------------
  Inference Kind: These kinds contain meta kind variables
---------------------------------------------------------------}
data InfKind  = KIVar Id
              | KICon Kind
              | KIApp InfKind InfKind


type InfKGamma = M.Map Name InfKind


infKindStar 
  = KICon kindStar

infKindHandled
  = KICon kindHandled

infKindFun k1 k2
  = KIApp (KIApp (KICon kindArrow) k1) k2

infKindFunN kinds k
  = foldr infKindFun k kinds


-- | Lift KApp nodes to KIApp
liftKApp :: InfKind -> InfKind
liftKApp infkind
  = case infkind of
      KIVar _             -> infkind
      KIApp k1 k2         -> KIApp (liftKApp k1) (liftKApp k2)
      KICon (KCon _)      -> infkind
      KICon (KApp k1 k2)  -> KIApp (liftKApp (KICon k1)) (liftKApp (KICon k2))


{---------------------------------------------------------------
  Substitution 
---------------------------------------------------------------}
newtype KSub = KSub (IM.IdMap InfKind)
newtype Kvs  = Kvs (IS.IdSet)

unKSub (KSub sub) = sub
ksubEmpty = KSub IM.empty
ksubSingle id kind
  = KSub (IM.singleton id kind)

unKvs (Kvs s)     = s
kvsMember id (Kvs s) = IS.member id s
kvsList (Kvs s) = IS.toList s


(@@) :: KSub -> KSub -> KSub
sub1 @@ sub2
  = KSub (IM.union (unKSub sub1) (unKSub (sub1 |=> sub2)))    --ASSUME: left biased union

class HasKindVar k where
  (|=>) :: KSub -> k -> k
  fkv   :: k -> Kvs

instance HasKindVar InfKind where
  sub |=> kind
    = case kind of
        KIVar id -> case IM.lookup id (unKSub sub) of
                      Just k  -> k
                      Nothing -> kind
        KICon _      -> kind
        KIApp k1 k2  -> KIApp (sub |=> k1) (sub |=> k2)

  fkv kind
    = case kind of
        KIVar id     -> Kvs (IS.singleton id)
        KICon _      -> Kvs (IS.empty)
        KIApp k1 k2  -> Kvs (IS.union (unKvs (fkv k1)) (unKvs (fkv k2)))

instance HasKindVar k => HasKindVar [k] where
  sub |=> xs
    = map (sub |=>) xs

  fkv xs
    = Kvs (IS.unions (map (unKvs . fkv) xs))


instance HasKindVar KSub where
  sub1 |=> (KSub sub2)
    = KSub (IM.map (sub1 |=>) sub2)

  fkv sub
    = fkv (IM.elems (unKSub sub))

{---------------------------------------------------------------
  Pretty printing
---------------------------------------------------------------}
instance Show InfKind where
  show kind
    = show (pretty (ppInfKind defaultColorScheme niceEmpty precTop kind))

niceInfKinds :: ColorScheme -> [InfKind] -> [Doc]
niceInfKinds colors kinds
  = let niceInitial = niceExtend (kvsList (fkv kinds)) niceKindVars niceEmpty
    in map (ppInfKind colors niceInitial precTop . liftKApp) kinds

niceKindVars :: [String]
niceKindVars
  = [ [x]              | x <- letters ] ++
    [ ([x] ++ show i)  | i <- [1..], x <- letters]
  where
    letters = "k"

ppInfKind :: ColorScheme -> Nice -> Prec -> InfKind -> Doc
ppInfKind colors nice prec kind
  = color (colorKind colors) $
    case kind of
      KIVar id       -> nicePretty nice id
      KICon kind     -> ppKind colors prec kind
      KIApp (KIApp (KICon (KCon name)) k1) k2 | name == nameKindFun
                     -> pparens prec precArrow $
                        case collectFunArgs k2 of
                          [res] -> ppInfKind colors nice precArrow k1 <+> text "->" <+> ppInfKind colors nice (precArrow-1) res
                          (res:args)  -> commaParens (ppInfKind colors nice precTop) (k1:args) <+> text "->" <+> ppInfKind colors nice (precArrow-1) res
                          _ -> matchFailure "Kind.InferKind.KIApp function"
      KIApp k1 k2    -> pparens prec precApp $
                        case collectArgs kind of
                          (k:ks) -> ppInfKind colors nice (precApp-1) k <+> commaParens (ppInfKind colors nice precTop) ks
                          _ -> matchFailure "Kind.InferKind.KIApp"
      
  where
    commaParens f xs
      = tupled (map f xs)

    collectFunArgs kind
      = case kind of
          KIApp (KIApp (KICon (KCon name)) k1) k2 | name == newName "->"
            -> k1 : collectFunArgs k2         
          _ -> [kind]
          
    collectArgs kind
      = case kind of
          KIApp k1 k2  -> collectArgs k1 ++ [k2]
          _            -> [kind]

-- | Precedence
type Prec = Int

precTop, precArrow,precApp,precAtom :: Int
precTop   = 0
precArrow = 1
precApp   = 2
precAtom  = 3

pparens :: Prec -> Prec -> Doc -> Doc
pparens context prec doc
  | context >= prec = parens doc
  | otherwise       = doc


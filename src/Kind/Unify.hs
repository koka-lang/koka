------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{--}
-----------------------------------------------------------------------------

module Kind.Unify( Context(..), unify, mgu, match ) where

import Lib.PPrint
import Common.Range
import Common.Message( docFromRange, table) 

import Kind.Kind
import Kind.InferKind
import Kind.InferMonad


{---------------------------------------------------------------
  Unify
---------------------------------------------------------------}
data Context = Check String Range
             | Infer Range

unify :: Context -> Range -> InfKind -> InfKind -> KInfer ()
unify context range kind1 kind2
  = do skind1 <- subst kind1
       skind2 <- subst kind2
       -- trace ("unify: " ++ show skind1 ++ " and " ++ show skind2) $
       case mgu skind1 skind2 of
         Ok sub' -> extendKSub sub'
         err     -> do cscheme <- getColorScheme
                       kindError cscheme context range err skind1 skind2

kindError colors context range err kind1 kind2
  = addError range $
    text message <->
    table ([(text "type context", docFromRange colors rangeContext)
           ,(text "type", docFromRange colors range)
           ,(text "inferred kind", niceKind2)
           ,(text expected, niceKind1)
           ]
           ++ extra)
  where
    (rangeContext,expected,extra)
      = case context of
          Check msg range -> (range,"expected kind",[(text "because", text msg)])
          Infer range     -> (range,"expected kind", [])

    [niceKind1,niceKind2]
      = niceInfKinds colors [kind1,kind2]

    message  
      = case err of
          InfiniteKind -> "Invalid type (due to an infinite kind)"
          _            -> "Invalid type"
                 



{---------------------------------------------------------------
  MGU
---------------------------------------------------------------}
data Unify = Ok KSub
           | InfiniteKind
           | NoMatch

mgu :: InfKind -> InfKind -> Unify
-- constants
mgu (KIVar id1) (KIVar id2)  | id1 == id2
  = Ok ksubEmpty
mgu (KICon kind1) (KICon kind2)  
  = if (match kind1 kind2)
     then Ok ksubEmpty
     else NoMatch
mgu (KIApp k1 k2) (KIApp l1 l2)
  = case mgu k1 l1 of
      Ok sub1 -> case mgu (sub1 |=> k2) (sub1 |=> l2) of
                     Ok sub2 -> Ok (sub2 @@ sub1)
                     err -> err
      err -> err

-- pull up KApp's
mgu (KICon (KApp k1 k2)) (KIApp l1 l2)
  = mgu (KIApp (KICon k1) (KICon k2)) (KIApp l1 l2)

mgu (KIApp k1 k2) (KICon (KApp l1 l2))
  = mgu (KIApp k1 k2) (KIApp (KICon l1) (KICon l2))

-- unify variables
mgu (KIVar id) kind
  = unifyVar id kind
mgu kind (KIVar id)
  = unifyVar id kind

-- no match
mgu _ _
  = NoMatch

unifyVar id kind
  = if kvsMember id (fkv kind) 
     then InfiniteKind
     else Ok (ksubSingle id kind)

match :: Kind -> Kind -> Bool
match (KCon c1) (KCon c2)         = (c1 == c2)
match (KApp k1 k2) (KApp l1 l2)   = (match k1 l1) && (match k2 l2)
match _ _ = False






------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Map constructor names to constructor info.
-}
-----------------------------------------------------------------------------
module Kind.Constructors( -- * Constructors
                            Constructors, ConInfo(..)
                          , constructorsEmpty
                          , constructorsExtend, constructorsLookup, constructorsFind
                          , constructorsIsEmpty
                          , constructorsFindScheme
                          , constructorsSet
                          , constructorsCompose, constructorsFromList
                          , extractConstructors
                            -- * Pretty
                          , ppConstructors
                          ) where

import qualified Data.List as L
import qualified Common.NameMap as M
import qualified Common.NameSet as S
import Lib.PPrint
import Common.Failure( failure )
import Common.Name
import Common.Syntax  ( Visibility(..))
import Kind.Pretty    ( kindColon )
import Type.Type
import Type.Pretty
import qualified Core.Core as Core
  
{--------------------------------------------------------------------------
   Newtype map
--------------------------------------------------------------------------}
-- | Constructors: a map from newtype names to newtype information
newtype Constructors  = Constructors (M.NameMap ConInfo)

constructorsEmpty :: Constructors
constructorsEmpty
  = Constructors M.empty

constructorsIsEmpty :: Constructors -> Bool
constructorsIsEmpty (Constructors m)
  = M.null m

constructorsExtend :: Name -> ConInfo -> Constructors -> Constructors
constructorsExtend name conInfo (Constructors m)
  = Constructors (M.insert name conInfo m)

constructorsFromList :: [ConInfo] -> Constructors
constructorsFromList conInfos
  = Constructors (M.fromList [(conInfoName info, info) | info <- conInfos])

constructorsCompose :: Constructors -> Constructors -> Constructors
constructorsCompose (Constructors cons1) (Constructors cons2)
  = Constructors (M.union cons2 cons1) -- ASSUME: left-biased union

constructorsLookup :: Name -> Constructors -> Maybe ConInfo
constructorsLookup name (Constructors m)
  = M.lookup name m

constructorsFind :: Name -> Constructors -> ConInfo
constructorsFind name syn
  = case constructorsLookup name syn of
      Nothing -> failure ("Kind.Constructors.constructorsFind: unknown constructor: " ++ show name)
      Just x  -> x

constructorsFindScheme :: Name -> Constructors -> Scheme
constructorsFindScheme conname cons
  = conInfoType (constructorsFind conname cons)

constructorsSet :: Constructors -> S.NameSet
constructorsSet (Constructors m)
  = S.fromList (M.keys m)

{--------------------------------------------------------------------------
  Pretty printing
--------------------------------------------------------------------------}
instance Show Constructors where
  show = show . pretty

instance Pretty Constructors where
  pretty syns
    = ppConstructors Type.Pretty.defaultEnv syns
    
ppConstructors showOptions (Constructors m)
    = vcat [fill 8 (pretty name) <.> kindColon (colors showOptions) <+>
            ppType showOptions (conInfoType conInfo)
           | (name,conInfo) <- L.sortBy (\(n1,_) (n2,_) -> compare (show n1) (show n2)) $ M.toList m]



-- | Extract constructor environment from core
extractConstructors :: Bool -> Core.Core -> Constructors
extractConstructors publicOnly core
  = Constructors (M.unions (L.map (extractTypeDefGroup isVisible) (Core.coreProgTypeDefs core)))
  where
    isVisible Public = True
    isVisible _      = not publicOnly

extractTypeDefGroup isVisible (Core.TypeDefGroup tdefs)
  = M.unions (L.map (extractTypeDef isVisible) tdefs)

extractTypeDef :: (Visibility -> Bool) -> Core.TypeDef -> M.NameMap ConInfo
extractTypeDef isVisible tdef
  = case tdef of
      Core.Data dataInfo vis conViss isExtend | isVisible vis
        -> let conInfos = dataInfoConstrs dataInfo
           in M.fromList [(conInfoName conInfo,conInfo) | (conInfo,vis) <- zip conInfos conViss, isVisible vis]
      _ -> M.empty

------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Map type names to type definition schemes.
-}
-----------------------------------------------------------------------------
module Kind.Newtypes( -- * Type newtypes
                      Newtypes, DataInfo(..)
                    , newtypesEmpty, newtypesExtend, newtypesLookupPublic, newtypesFind
                    , newtypesLookupAny
                    , newtypesNew, newtypesCompose
                    , newtypesIsEmpty
                    , newtypesTypeDefs
                    , extractNewtypes
                      -- * Data info of types
                    , getDataInfo', getDataDef', extractDataInfo, getConInfo, getFixedDataAllocSize
                      -- * Pretty
                    -- , ppNewtypes
                    ) where

import qualified Common.NameMap as M
import Common.Failure( failure )
import Common.Name
import Common.Syntax  ( Visibility(..), isPublic, Platform, DataDef(..), dataDefIsValue, valueReprSizeScan )
import Common.NamePrim( nameTpBox )
import Type.Type

import Type.Pretty
import Lib.PPrint
import qualified Data.List as L
import Data.Maybe( isJust )

import qualified Core.Core as Core

{--------------------------------------------------------------------------
   Newtype map
--------------------------------------------------------------------------}
-- | Newtypes: a map from newtype names to newtype information
newtype Newtypes  = Newtypes (M.NameMap DataInfo)

newtypesEmpty :: Newtypes
newtypesEmpty
  = Newtypes M.empty

newtypesIsEmpty :: Newtypes -> Bool
newtypesIsEmpty (Newtypes m)
  = M.null m

newtypesNew :: [DataInfo] -> Newtypes
newtypesNew infos
  = Newtypes (M.fromList [(dataInfoName info, info) | info <- infos])

newtypesCompose :: Newtypes -> Newtypes -> Newtypes
newtypesCompose (Newtypes m1) (Newtypes m2)
  = Newtypes (M.union m2 m1) -- ASSUME: left-biased union

newtypesTypeDefs :: Newtypes -> M.NameMap DataInfo
newtypesTypeDefs (Newtypes m)
  = m

newtypesExtend :: Name -> DataInfo -> Newtypes -> Newtypes
newtypesExtend name info (Newtypes m)
  = Newtypes (M.insert name info m)

newtypesLookupPublic :: Name -> Newtypes -> Maybe DataInfo
newtypesLookupPublic name nt
  = case newtypesLookupAny name nt of
      Just info | isPublic (dataInfoVis info)  -> Just info
      _ -> Nothing

newtypesLookupAny :: Name -> Newtypes -> Maybe DataInfo
newtypesLookupAny name (Newtypes m)
  = M.lookup name m

newtypesFind :: Name -> Newtypes -> DataInfo
newtypesFind name syn
  = case newtypesLookupAny name syn of
      Nothing -> failure ("Kind.Newtypes.newtypesFind: unknown newtype: " ++ show name)
      Just x  -> x


{--------------------------------------------------------------------------
   Data info of types
--------------------------------------------------------------------------}

-- | The data type of a type; `Nothing` for type variables, functions, and `box`.
getDataInfo' :: Newtypes -> Type -> Maybe DataInfo
getDataInfo' newtypes tp
  = case extractDataDefType (expandSyn tp) of
      Nothing   -> Nothing
      Just name | name == nameTpBox -> Nothing
      Just name -> case newtypesLookupAny name newtypes of
                      Nothing -> failure $ "Kind.Newtypes.getDataInfo': cannot find type: " ++ show name -- ++ "\n" ++ show newtypes
                      Just di -> -- trace ("datainfo of " ++ show (pretty tp) ++ " = " ++ show di) $
                                 Just di

getDataDef' :: Newtypes -> Type -> Maybe DataDef
getDataDef' newtypes tp
  = case getDataInfo' newtypes tp of
      Just di -> Just (dataInfoDef di)
      _       -> Nothing -- DataDefNormal

-- | The data type of a type (or of the result of a function type), with its name.
extractDataInfo :: Newtypes -> Type -> Maybe (Name,DataInfo)
extractDataInfo newtypes dataType
  = case  extractDataName dataType of
      Nothing   -> Nothing
      Just name -> case newtypesLookupAny name newtypes of
                     Just dataInfo -> Just (name,dataInfo)
                     Nothing       -> Nothing
  where
    extractDataName :: Type -> Maybe Name
    extractDataName tp
      = case expandSyn tp of
          TForall _ t -> extractDataName t
          TFun _ _ t -> extractDataName t
          TApp t _   -> extractDataName t
          TCon tc    -> Just (typeConName tc)
          _          -> Nothing

-- | The constructor info of a constructor, given its type and name.
getConInfo :: Newtypes -> Type -> Name -> Maybe ConInfo
getConInfo newtypes conType conName
  = do (_,dataInfo) <- extractDataInfo newtypes conType
       L.find (\ci -> conInfoName ci == conName) (dataInfoConstrs dataInfo)

-- | The allocation size of a value of this type, when every constructor has the same one.
-- A function value has none: its size depends on what the closure captures, and
-- `extractDataInfo` would otherwise answer with the size of its result type.
getFixedDataAllocSize :: Platform -> Newtypes -> Type -> Maybe (Int, Int)
getFixedDataAllocSize platform newtypes dataType
  | isJust (splitFunScheme dataType) = Nothing
  | otherwise
  = case extractDataInfo newtypes dataType of
      Nothing -> Nothing
      Just (dataName,dataInfo)
        -> if ("_noreuse" `L.isSuffixOf` nameLocal dataName)
            then Nothing
            else let ddef = dataInfoDef dataInfo
                 in if dataDefIsValue ddef
                      then Nothing
                      else let cis = dataInfoConstrs dataInfo
                               sizeScanCounts = map (valueReprSizeScan platform . conInfoValueRepr) cis
                           in case sizeScanCounts of
                                (ss:sss) | all (==ss) sss -> Just ss
                                _        -> Nothing


-- | Extract data infos from core
extractNewtypes :: Core.Core -> Newtypes
extractNewtypes core
  = newtypesNew (concatMap extractTypeDefGroup (Core.coreProgTypeDefs core))

extractTypeDefGroup (Core.TypeDefGroup tdefs)
  = concatMap extractTypeDef tdefs

extractTypeDef :: Core.TypeDef -> [DataInfo]
extractTypeDef tdef
  = case tdef of
      Core.Data dataInfo
        -> [dataInfo]
      _ -> []

{--------------------------------------------------------------------------
  Pretty printing

  TODO: redo
--------------------------------------------------------------------------}

instance Show Newtypes where
  show = show . pretty

instance Pretty Newtypes where
  pretty syns
    = ppNewtypes Type.Pretty.defaultEnv syns

ppNewtypes showOptions (Newtypes m)
    = vcat [fill 8 (pretty name) <.> colon <+>
            -- text "rank" <+> pretty rank <.> colon <+>
            ppDataInfo defaultEnv True dataInfo
           | (name,dataInfo) <- L.sortBy (\(n1,_) (n2,_) -> compare (show n1) (show n2)) $ M.toList m]

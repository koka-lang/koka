------------------------------------------------------------------------------
-- Copyright 2012-2023, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    
-}
-----------------------------------------------------------------------------
module Kind.Repr( orderConFields, createDataDef ) where

import Control.Monad( when )
import Lib.PPrint
import Common.Name
import Common.NamePrim
import Common.Syntax
import Common.Failure
import Type.Type

---------------------------------------------------------
-- Create a datadef and elaborate conInfo's with a ValueRepr
-- and correctly ordered fields depending on alignment
-- constraints and platform sizes.
---------------------------------------------------------

-- value types
createDataDef :: Monad m => (Doc-> m ()) -> (Doc-> m ()) -> (Name -> m (Maybe DataInfo))
                               -> Platform -> Name -> Bool -> Bool -> DataKind 
                                 -> Int -> DataDef -> [ConInfo] -> m (DataDef,[ConInfo])
createDataDef emitError emitWarning lookupDataInfo 
               platform name resultHasKindStar isRec sort 
                extraFields defaultDef conInfos0
  = do --calculate the value repr of each constructor
       conInfos <- mapM createConInfoRepr conInfos0

       -- datadef 
       let maxMembers = maximum ([0] ++ map (length . conInfoParams) conInfos)
           conCount   = length conInfos
           canbeValue = resultHasKindStar && sort /= Retractive
           isEnum     = canbeValue && maxMembers == 0 && conCount >= 1
           isIso      = canbeValue && maxMembers == 1 && conCount == 1
                                    
       ddef  <- case defaultDef of
                  DataDefOpen
                    -> return DataDefOpen
                  DataDefRec
                    -> return DataDefRec
                  
                  DataDefNormal | isRec
                    -> return DataDefRec
                  DataDefNormal 
                    -> do dd <- createMaxDataDef conInfos
                          case dd of
                            {- DataDefValue vr | isEnum  -- allow allocated enum types
                              -> return dd -}
                            DataDefValue vr | isIso   -- iso types are always value types
                              -> return dd
                            _ -> return DataDefNormal
                  
                  DataDefAuto | isRec
                    -> return DataDefRec
                  DataDefAuto 
                    -> do dd <- createMaxDataDef conInfos
                          case dd of
                            DataDefValue vr | isEnum
                              -> return dd
                            DataDefValue vr | isIso   -- iso types are always value types
                              -> return dd
                            DataDefValue vr
                              -> do let wouldGetTagField = (conCount > 1 && not isEnum)
                                        size = valueReprSize platform vr + (if wouldGetTagField then sizeField platform else 0)
                                    when ((size <= 2*sizePtr platform) && (maxMembers <= 3) && canbeValue) $
                                      emitWarning $ text "may be better declared as a value type for efficiency (e.g. 'value type/struct')" <->
                                                    text "or declare as a reference type (e.g. 'ref type/struct') to suppress this warning"
                                    return DataDefNormal
                            _ -> return DataDefNormal
                  
                  DataDefValue{} | isRec
                    -> do emitError $ text "cannot be declared as a value type since it is recursive."
                          return DataDefNormal
                  DataDefValue{} | not resultHasKindStar
                    -> do emitError $ text "is declared as a value type but does not have a value kind ('V')."  -- should never happen?
                          return DataDefNormal
                  DataDefValue{} | sort == Retractive
                    -> do emitError $ text "is declared as a value type but is not (co)inductive."
                          return DataDefNormal
                  DataDefValue{}
                    -> do dd <- createMaxDataDef conInfos
                          case dd of
                            DataDefValue vr
                              -> do let size = valueReprSize platform vr 
                                    when (size > 4*sizePtr platform) $
                                      emitWarning (text "requires" <+> pretty size <+> text "bytes which is rather large for a value type")
                                    when isEnum $
                                      emitWarning (text "is an enumeration -- there is no need to declare it as a value type")
                                    -- when isIso $
                                    --   emitWarning (text "is a isomorphic type -- there is no need to declare it as a value type")
                                    return dd
                            _ -> do emitError $ text "cannot be used as a value type."  -- should never happen?
                                    return DataDefNormal
       return (ddef,conInfos)
  where
    isVal :: Bool
    isVal = dataDefIsValue defaultDef

    -- createConInfoRepr :: ConInfo -> m ConInfo
    createConInfoRepr conInfo
      = do (orderedFields,vrepr) <- orderConFields emitError (text "constructor" <+> pretty (conInfoName conInfo)) 
                                                   lookupDataInfo platform extraFields (conInfoParams conInfo)
           return (conInfo{ conInfoOrderedParams = orderedFields, conInfoValueRepr = vrepr } )

    -- createMaxDataDef :: [ConInfo] -> m DataDef
    createMaxDataDef conInfos
      =  do let vreprs = map conInfoValueRepr conInfos
            ddef <- maxDataDefs vreprs
            case ddef of
              DataDefValue (ValueRepr 0 0 0) -- enumeration
                -> let n = length conInfos
                  in if (n < 256)         then return $ DataDefValue (valueReprRaw 1) -- uint8_t
                      else if (n < 65536) then return $ DataDefValue (valueReprRaw 2) -- uint16_t
                                          else return $ DataDefValue (valueReprRaw 4) -- uint32_t
              _ -> return ddef


    -- note: (m = raw, n = scan)
    -- maxDataDefs :: Monad m => [ValueRepr] -> m DataDef
    maxDataDefs [] 
      = if not isVal 
          then return DataDefNormal  -- reference type, no constructors
          else do let size  = if (name == nameTpChar || name == nameTpInt32 || name == nameTpFloat32)
                               then 4
                              else if (name == nameTpFloat || name == nameTpInt64)
                               then 8
                              else if (name == nameTpInt8)
                               then 1
                              else if (name == nameTpInt16 || name == nameTpFloat16)
                               then 2
                              else if (name == nameTpAny || name == nameTpCField || name == nameTpIntPtrT)
                               then (sizePtr platform)
                              else if (name==nameTpSSizeT)
                               then (sizeSize platform)
                              else 0
                  m <- if (size <= 0)
                        then do emitWarning $ text "is declared as a primitive value type but has no known compilation size, assuming size" <+> pretty (sizePtr platform)
                                return (sizePtr platform)
                        else return size
                  return (DataDefValue (valueReprNew m 0 m))
    maxDataDefs [vr] -- singleton value
      = return (DataDefValue vr)
    maxDataDefs (vr:vrs)
      = do dd <- maxDataDefs vrs
           case (vr,dd) of
              (ValueRepr 0 0 _,    DataDefValue v)                  -> return (DataDefValue v)
              (v,                  DataDefValue (ValueRepr 0 0 _))  -> return (DataDefValue v)
              (ValueRepr m1 0 a1,  DataDefValue (ValueRepr m2 0 a2)) 
                -> return (DataDefValue (valueReprNew (max m1 m2) 0 (max a1 a2)))
              (ValueRepr 0 n1 a1,  DataDefValue (ValueRepr 0 n2 a2)) 
                -> return (DataDefValue (valueReprNew 0 (max n1 n2) (max a1 a2)))
              (ValueRepr m1 n1 a1, DataDefValue (ValueRepr m2 n2 a2))
                -- equal scan fields
                | n1 == n2  -> return (DataDefValue (valueReprNew (max m1 m2) n1 (max a1 a2)))
                -- non-equal scan fields
                | otherwise ->
                  do when isVal $
                       emitError (text "is declared as a value type but has" <+> text "multiple constructors with a different number of regular types overlapping with value types." <->
                                  text "hint: value types with multiple constructors must all use the same number of regular types (use 'box' to use a value type as a regular type).")
                      -- else emitWarning (text "cannot be defaulted to a value type as it has" <+> text "multiple constructors with a different number of regular types overlapping with value types.")
                     -- trace ("warning: cannot default to a value type due to mixed raw/regular fields: " ++ show nameDoc) $
                     return DataDefNormal -- (DataDefValue (max m1 m2) (max n1 n2))
              _ -> return DataDefNormal


---------------------------------------------------------
-- Determine the size of a constructor
---------------------------------------------------------

-- order constructor fields of constructors with raw field so the regular fields come first to be scanned.
-- return the ordered fields, and a ValueRepr (raw size part, the scan count (including tags), align, and full size)
-- The size is used for reuse and should include all needed fields including the tag field for "open" datatypes 
orderConFields :: Monad m => (Doc -> m ()) -> Doc -> (Name -> m (Maybe DataInfo)) -> Platform
                               -> Int -> [(Name,Type)] -> m ([(Name,Type)],ValueRepr)
orderConFields emitError nameDoc getDataInfo platform extraPreScan fields
  = do visit ([], [], [], extraPreScan, 0) fields
  where
    -- visit :: ([((Name,Type),ValueRepr)],[((Name,Type),ValueRepr)],[(Name,Type)],Int,Int) -> [(Name,Type)] -> m ([(Name,Type)],ValueRepr)
    visit (rraw, rmixed, rscan, scanCount0, alignment0) []  
      = do when (length rmixed > 1) $
             do emitError (nameDoc <+> text "has multiple value type fields that each contain both raw types and regular types." <->
                             text ("hint: use 'box' on either field to make it a non-value type."))
           let  -- scancount and size before any mixed and raw fields
                preSize    = (sizeHeader platform) + (scanCount0 * sizeField platform)

                -- if there is a mixed value member (with scan fields) we may need to add padding scan fields (!)
                -- (or otherwise the C compiler may insert uninitialized padding)
                (padding,mixedScan)   
                          = case rmixed of
                              ((_,ValueRepr _ scan ralign):_) 
                                 -> let padSize    = preSize `mod` ralign
                                        padCount   = padSize `div` sizeField platform
                                    in assertion ("Kind.Infer.orderConFields: illegal alignment: " ++ show ralign) (padSize `mod` sizeField platform == 0) $
                                       ([((newPaddingName (scanCount0 + i),typeAny),valueReprScan 1) | i <- [1..padCount]]
                                       ,scan + padCount)
                              [] -> ([],0)

                -- calculate the rest now
                scanCount = scanCount0 + mixedScan  
                alignment = if scanCount > 0 then max alignment0 (sizeField platform) else alignment0
                rest      = padding ++ rmixed ++ reverse rraw
                restSizes = [valueReprSize platform vr | (_field,vr) <- rest]
                restFields= [field | (field,_vr) <- rest]
                size      = alignedSum preSize restSizes                            
                rawSize   = size - (sizeHeader platform) - (scanCount * sizeField platform)
                vrepr     = valueReprNew rawSize scanCount alignment
           -- (if null padding then id else trace ("constructor: " ++ show cname ++ ": " ++ show vrepr) $
           return (reverse rscan ++ restFields, vrepr)

    visit (rraw,rmixed,rscan,scanCount,alignment0) (field@(name,tp) : fs)
      = do mDataDef <- getDataDef getDataInfo tp
           case mDataDef of
             Just (DataDefValue vr@(ValueRepr raw scan align))
               -> -- let extra = if (hasTagField dataRepr) then 1 else 0 in -- adjust scan count for added "tag_t" members in structs with multiple constructors
                  let alignment = max align alignment0 in
                  if (raw > 0 && scan > 0)
                   then -- mixed raw/scan: put it at the head of the raw fields (there should be only one of these as checked in Kind/Infer)
                        -- but we count them to be sure (and for function data)
                        visit (rraw, (field,vr):rmixed, rscan, scanCount, alignment) fs
                   else if (raw > 0)
                         then visit (insertRaw field vr rraw, rmixed, rscan, scanCount, alignment) fs
                         else visit (rraw, rmixed, field:rscan, scanCount + scan, alignment) fs
             _ -> visit (rraw, rmixed, field:rscan, scanCount + 1, alignment0) fs

    -- insert raw fields in (reversed) order of alignment so they align to the smallest total size in a datatype
    insertRaw :: (Name,Type) -> ValueRepr -> [((Name,Type),ValueRepr)] -> [((Name,Type),ValueRepr)]
    insertRaw field vr ((f,vrf):rs)
      | valueReprAlignment vr <= valueReprAlignment vrf  = (field,vr):(f,vrf):rs
      | otherwise                                        = (f,vrf):insertRaw field vr rs
    insertRaw field vr []
      = [(field,vr)]
    
    

-- | Return the DataDef for a type.
-- This may be 'Nothing' for abstract types.
getDataDef :: Monad m => (Name -> m (Maybe DataInfo)) -> Type -> m (Maybe DataDef)
getDataDef lookupDI tp
   = case extractDataDefType tp of
       Nothing -> return $ Just DataDefNormal
       Just name | name == nameTpBox -> return $ Just DataDefNormal
       Just name -> do mdi <- lookupDI name 
                       case mdi of
                         Nothing -> return Nothing
                         Just di -> return $ Just (dataInfoDef di)
    where 
      extractDataDefType :: Type -> Maybe Name
      extractDataDefType tp
        = case expandSyn tp of
            TApp t _      -> extractDataDefType t
            TForall _ _ t -> extractDataDefType t
            TCon tc       -> Just (typeConName tc)
            _             -> Nothing


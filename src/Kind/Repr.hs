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
module Kind.Repr( orderConFields ) where

import Control.Monad( when )
import Lib.PPrint
import Common.Name
import Common.NamePrim
import Common.Syntax
import Common.Failure
import Type.Type

---------------------------------------------------------
-- Determine the size of a constructor
---------------------------------------------------------

-- order constructor fields of constructors with raw field so the regular fields come first to be scanned.
-- return the ordered fields, and a ValueRepr (raw size part, the scan count (including tags), align, and full size)
-- The size is used for reuse and should include all needed fields including the tag field for "open" datatypes 
orderConFields :: Monad m => ((Doc -> Doc) -> m ()) -> (Name -> m (Maybe DataInfo)) -> Platform
                               -> Bool -> [(Name,Type)] -> m ([(Name,Type)],ValueRepr)
orderConFields emitError getDataInfo platform isOpen fields
  = do visit ([], [], [], if isOpen then 1 else 0, 0) fields
  where
    -- visit :: ([((Name,Type),Int,Int,Int)],[((Name,Type),Int,Int,Int)],[(Name,Type)],Int,Int) -> [(Name,Type)] -> m ([(Name,Type)],ValueRepr)
    visit (rraw, rmixed, rscan, scanCount0, alignment0) []  
      = do when (length rmixed > 1) $
             do emitError (\nameDoc -> (text "Constructor:" <+> nameDoc <+> text "has multiple value type fields that each contain both raw types and regular types." <->
                                        text ("hint: use 'box' on either field to make it a non-value type.")))
                {-
                cs <- getColorScheme
                let nameDoc = color (colorCons cs) (pretty cname)
                addError range (text "Constructor:" <+> nameDoc <+> text "has multiple value type fields that each contain both raw types and regular types." <->
                                text ("hint: use 'box' on either field to make it a non-value type."))
                -}
           let  -- scancount and size before any mixed and raw fields
                preSize    = (sizeHeader platform) + (scanCount0 * sizeField platform)

                -- if there is a mixed value member (with scan fields) we may need to add padding scan fields (!)
                -- (or otherwise the C compiler may insert uninitialized padding)
                (padding,mixedScan)   
                          = case rmixed of
                              ((_,_,scan,ralign):_) 
                                 -> let padSize    = preSize `mod` ralign
                                        padCount   = padSize `div` sizeField platform
                                    in assertion ("Kind.Infer.orderConFields: illegal alignment: " ++ show ralign) (padSize `mod` sizeField platform == 0) $
                                       ([((newHiddenName ("padding" ++ show i),typeInt),sizeField platform,1,sizeField platform) | i <- [1..padCount]]
                                       ,scan + padCount)
                              [] -> ([],0)

                -- calculate the rest now
                scanCount = scanCount0 + mixedScan  
                alignment = if scanCount > 0 then max alignment0 (sizeField platform) else alignment0
                rest      = padding ++ rmixed ++ reverse rraw
                restSizes = [size  | (_field,size,_scan,_align) <- rest]
                restFields= [field | (field,_size,_scan,_align) <- rest]
                size      = alignedSum preSize restSizes
                rawSize   = size - (sizeHeader platform) - (scanCount * sizeField platform)
                vrepr     = valueReprNew platform rawSize scanCount alignment
           -- trace ("constructor: " ++ show cname ++ ": " ++ show vrepr) $
           return (reverse rscan ++ restFields, vrepr)

    visit (rraw,rmixed,rscan,scanCount,alignment0) (field@(name,tp) : fs)
      = do mDataDef <- getDataDef getDataInfo tp
           case mDataDef of
             Just (DataDefValue (ValueRepr raw scan align _))
               -> -- let extra = if (hasTagField dataRepr) then 1 else 0 in -- adjust scan count for added "tag_t" members in structs with multiple constructors
                  let alignment = max align alignment0 in
                  if (raw > 0 && scan > 0)
                   then -- mixed raw/scan: put it at the head of the raw fields (there should be only one of these as checked in Kind/Infer)
                        -- but we count them to be sure (and for function data)
                        visit (rraw, (field,raw,scan,align):rmixed, rscan, scanCount, alignment) fs
                   else if (raw > 0)
                         then visit (insertRaw field raw scan align rraw, rmixed, rscan, scanCount, alignment) fs
                         else visit (rraw, rmixed, field:rscan, scanCount + scan, alignment) fs
             _ -> visit (rraw, rmixed, field:rscan, scanCount + 1, alignment0) fs

    -- insert raw fields in (reversed) order of alignment so they align to the smallest total size in a datatype
    insertRaw :: (Name,Type) -> Int -> Int -> Int -> [((Name,Type),Int,Int,Int)] -> [((Name,Type),Int,Int,Int)]
    insertRaw field raw scan align ((f,r,s,a):rs)
      | align <= a  = (field,raw,scan,align):(f,r,s,a):rs
      | otherwise   = (f,r,s,a):insertRaw field raw scan align rs
    insertRaw field raw scan align []
      = [(field,raw,scan,align)]
    
    

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


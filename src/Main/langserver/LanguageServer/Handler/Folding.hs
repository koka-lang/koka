------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards, FlexibleInstances #-}
module LanguageServer.Handler.Folding(foldingHandler) where

import Control.Lens ((^.))
import qualified Data.Text as T
import Data.Maybe (maybeToList)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Types (FoldingRangeKind(FoldingRangeKind_Region))
import Language.LSP.Server (Handlers, requestHandler)

import qualified Common.Range            as R
import Common.Name (nameNil, Name, nameLocal )
import Common.Range (Pos(..), Ranged (getRange))
import Common.Syntax
import Syntax.Syntax
import Type.Pretty (ppName)
import LanguageServer.Conversions (fromLspPos, toLspRange)
import LanguageServer.Monad

-- Handles hover requests
foldingHandler :: Handlers LSM
foldingHandler
  = requestHandler J.SMethod_TextDocumentFoldingRange $ \req responder ->
    do let J.FoldingRangeParams _ _ doc = req ^. J.params
           uri = J.toNormalizedUri (doc ^. J.uri)
           done = responder $ Right $ J.InR J.Null
           liftMaybe :: LSM (Maybe a) -> (a -> LSM ()) -> LSM ()
           liftMaybe action next = do res <- action
                                      case res of
                                         Nothing -> done
                                         Just x  -> next x
       liftMaybe (lookupModuleName uri) $ \(fpath,modname) ->
         liftMaybe (lookupProgram modname) $ \program ->
            do let fs = foldings program
               responder $ Right $ J.InL fs


class HasFoldingRanges a where
  foldings :: a -> [J.FoldingRange]

instance HasFoldingRanges a => HasFoldingRanges (Maybe a) where
  foldings = maybe [] foldings

instance HasFoldingRanges a => HasFoldingRanges [a] where
  foldings = (foldings =<<)

instance HasFoldingRanges () where
  foldings = const []

instance HasFoldingRanges UserProgram where
  foldings prog = foldings (programTypeDefs prog) ++ foldings (programDefs prog)

-- Type definition instances

instance HasFoldingRanges UserTypeDefGroup where
  foldings tdg = case tdg of
    TypeDefRec tds   -> foldings tds
    TypeDefNonRec td -> foldings td

instance HasFoldingRanges UserTypeDef where
  foldings td = makeFolding r n
    where
      b = typeDefBinder td
      n = tbinderName b
      r = typeDefRange td

instance HasFoldingRanges UserUserCon where
  foldings c = makeFolding r n ++ cs
    where
      n = userconName c
      r = userconRange c
      cs = foldings (userconParams c)

type UserConParam = (Visibility,ValueBinder UserType (Maybe (Expr UserType)))
instance HasFoldingRanges UserConParam where
  foldings (v, b) = makeFolding r n ++ cs
    where
      n = binderName b
      r = binderRange b
      cs = foldings (binderExpr b)

-- Value definition instances
instance HasFoldingRanges UserDefGroup where
  foldings dg = case dg of
    DefRec    ds -> foldings ds
    DefNonRec d  -> foldings d

instance HasFoldingRanges UserDef where
  foldings d = makeFolding r n ++ cs
    where
      b = defBinder d
      n = binderName b
      r = defRange d
      cs = foldings $ binderExpr b

instance HasFoldingRanges e => HasFoldingRanges (ValueBinder t e) where
  foldings b = makeFolding r n ++ cs
    where
      n = binderName b
      r = binderRange b
      cs = foldings $ binderExpr b

instance HasFoldingRanges UserExpr where
  foldings ex = case ex of
    Lam bs e r                            -> foldings bs ++ foldings e ++ makeFoldingNoName r
    Let (DefRec dfs) e r                  -> concatMap foldings dfs ++ foldings e
    Let (DefNonRec df) e r                -> foldings df ++ foldings e
    Bind d e r                            -> foldings d ++ foldings e ++ makeFolding r (getName d)
    App e nes _                           -> foldings e ++ foldings (map snd nes)
    Ann e _ r                             -> foldings e ++ makeFoldingNoName r
    Case e bs r                           -> foldings e ++ foldings bs ++ makeFoldingNoName r
    Parens e _ _ _                        -> foldings e
    Handler _ _ _ _ _ bs e1 e2 e3 hbs _ r -> foldings bs ++ foldings e1
                                                        ++ foldings e2
                                                        ++ foldings e3
                                                        ++ foldings hbs ++ makeFoldingNoName r
    Inject _ e _ _                        -> foldings e
    _ -> []

instance HasFoldingRanges UserHandlerBranch where
  foldings hb = makeFolding r n ++ cs
    where
      n = hbranchName hb
      e = hbranchExpr hb
      r = R.getRange (hbranchNameRange hb) `R.combineRange` R.getRange e
      ps = hbranchPars hb
      cs = foldings ps ++ foldings e

instance HasFoldingRanges UserBranch where
  foldings b = foldings gs
    where
      p = branchPattern b
      gs = branchGuards b

instance HasFoldingRanges UserGuard where
  foldings g = foldings t ++ foldings e
    where
      t = guardTest g
      e = guardExpr g

makeFoldingNoName :: R.Range -> [J.FoldingRange]
makeFoldingNoName r = [J.FoldingRange lineStart (Just charStart) lineEnd (Just charEnd) (Just J.FoldingRangeKind_Region) Nothing |  rangeSpansLine r]
  where J.Range {_start=J.Position{_line=lineStart, _character=charStart}, _end=J.Position{_line=lineEnd, _character=charEnd}} = toLspRange r

makeFolding :: R.Range -> Name -> [J.FoldingRange]
makeFolding r n =
  [J.FoldingRange lineStart (Just charStart) lineEnd (Just charEnd) (Just J.FoldingRangeKind_Region) (Just (T.pack $ nameLocal n)) |  rangeSpansLine r]
  where J.Range {_start=J.Position{_line=lineStart, _character=charStart}, _end=J.Position{_line=lineEnd, _character=charEnd}} = toLspRange r


rangeSpansLine :: R.Range -> Bool
rangeSpansLine r = lineStart /= lineEnd
  where lineStart = posLine $ R.rangeStart r
        lineEnd = posLine $ R.rangeEnd r

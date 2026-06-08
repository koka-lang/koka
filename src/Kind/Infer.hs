------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Responsibilities of the kind checker:

    - Kindcheck all types in the program

    - Translate user types to internal types

    - Collect lists of data types, synonyms and constructors

    - Expand all synonyms (i.e., replace @id(int)@ by @id(int) == int@)

    - Transate type definition groups and externals to Core.
-}
-----------------------------------------------------------------------------

module Kind.Infer (inferKinds ) where

import Debug.Trace
-- import Type.Pretty

import Data.Char(isAlphaNum)
import Data.List(groupBy,intersperse,nubBy,sortOn,partition,find)
import Data.Maybe(catMaybes,isJust)
import Control.Monad(when)

import Lib.PPrint
import Common.Failure
import Common.Unique( uniqueId, setUnique, unique )
import Common.Error
import Common.ColorScheme( ColorScheme, colorType, colorSource, colorCons )
import Common.Range
import Common.Syntax( Platform(..), dataDefIsLazy )
import Common.Name
import Common.NamePrim
import Common.Syntax
import Common.File( startsWith )
import qualified Common.NameMap as M
import Syntax.Syntax
import Syntax.Promote( promoteType )
import Syntax.RangeMap
import qualified Core.Core as Core

import Static.BindingGroups( groupTypeDefBindings, groupBindings )

import Kind.ImportMap
import Kind.Kind
import Kind.Assumption
import Kind.Constructors
import Kind.Newtypes
import Kind.Synonym
import Kind.Repr( createDataDef )
import Type.Type
import Type.Assumption
import Type.TypeVar( tvsIsEmpty, ftv, subNew, (|->), tvsMember, tvsList )
import Type.Pretty
import Type.Kind( getKind )

import Kind.InferKind
import Kind.InferMonad
import Kind.Unify

{--------------------------------------------------------------------------
  Kindcheck a program and calculate types
  Also groups recursive type and expressing bindings in order.
--------------------------------------------------------------------------}
-- | Kindcheck a program and calculate types
inferKinds
  :: (DataInfo -> Bool) -- ^ is this a value type?
  -> ColorScheme      -- ^ Color scheme used for error messages
  -> TargetPlatform   -- ^ Target platform 
  -> Maybe RangeMap   -- ^ possible range map for tool integration
  -> ImportMap        -- ^ Import aliases
  -> KGamma           -- ^ Initial kind kgamma
  -> Synonyms         -- ^ Initial list of synonyms
  -> Newtypes         -- ^ Initial list of data types
  -> Program UserType UserKind  -- ^ Original program
  -> Core.CorePhase b
           ( DefGroups Type       --  Translated program (containing translated types)
           -- , Gamma                --  Gamma containing generated functions, i.e type scheme for every constructor
           , KGamma               --  updated kind gamma
           , Synonyms             --  Updated synonyms
           , Newtypes             --  Update data types
           , Constructors         --  Constructor information
           -- , Core.TypeDefGroups   --  Core type definition groups
           -- , Core.Externals       --  Core externals
           , Core.Core            --  Initial core program with type definition groups, externals, and some generated definitions for data types (like folds).
           , Maybe RangeMap
           )
inferKinds isValue colors tplatform mbRangeMap imports kgamma0 syns0 data0
            (Program source modName nameRange tdefs defs importdefs externals fixdefs doc)
  =do unique0 <- unique
      let -- order and group recursive let-bindings
          tdgroups0 = groupTypeDefBindings modName tdefs
          (errs1,warns1,rm1,unique1,(tdgroups,kgamma1,syns1,data1,lazyExprs)) = runKindInfer colors tplatform mbRangeMap modName imports kgamma0 syns0 data0 unique0 (infTypeDefGroups tdgroups0)
          (errs2,warns2,rm2,unique2,externals1)              = runKindInfer colors tplatform rm1 modName imports kgamma1 syns1 data1 unique1 (infExternals externals)
          (errs3,warns3,rm3,unique3,defs1)                   = runKindInfer colors tplatform rm2 modName imports kgamma1 syns1 data1 unique2 (infDefGroups defs)
          (errs4,warns4,rm4,unique4,synDefs)                 = runKindInfer colors tplatform rm3 modName imports kgamma1 syns1 data1 unique3 (synTypeDefGroups modName lazyExprs tdgroups)
          (_,_,rm5,_,_) = runKindInfer colors tplatform rm3 modName imports kgamma1 syns1 data1 unique3 (infImports modName nameRange importdefs)

          (synInfos,dataInfos) = unzipEither (extractInfos tdgroups)
          conInfos  = concatMap dataInfoConstrs dataInfos
          cons1     = constructorsFromList conInfos
          gamma1    = constructorGamma isValue dataInfos
          errs5     = constructorCheckDuplicates colors conInfos
          warns     = [warningMessageKind ErrKind rng doc | (rng,doc) <- warns1 ++ warns2 ++ warns3 ++ warns4]
          errs      = [errorMessageKind ErrKind rng doc  | (rng,doc) <- errs1 ++ errs2 ++ errs3 ++ errs4 ++ errs5]

          -- now order and group definitions including newly synthesized definitions for type definitions
          dgroups   = groupBindings modName (synDefs ++ defs1)
      setUnique unique4
      Core.liftError  (addWarnings warns $
                        if (null errs)
                          then return (dgroups
                                      -- ,gamma1
                                      ,kgamma1
                                      ,syns1
                                      ,data1 -- newtypesNew dataInfos
                                      ,cons1
                                      -- ,cgroups
                                      -- ,externals1
                                      ,Core.Core modName [] [] tdgroups [] externals1 doc
                                      ,rm5
                                      )
                          else errorMsgs errs)

unzipEither :: [Either a b] -> ([a],[b])
unzipEither xs
  = unz [] [] xs
  where
    unz left right (Left x:xs)  = unz (x:left) right xs
    unz left right (Right x:xs) = unz left (x:right) xs
    unz left right []           = (reverse left, reverse right)


extractInfos :: [Core.TypeDefGroup] -> [Either SynInfo DataInfo]
extractInfos groups
  = concatMap extractGroupInfos groups
  where
    extractGroupInfos (Core.TypeDefGroup ctdefs)
      = map extractInfo ctdefs

    extractInfo (Core.Synonym synInfo )  = Left synInfo
    extractInfo (Core.Data dataInfo)     = Right dataInfo

{---------------------------------------------------------------

---------------------------------------------------------------}
type LazyExpr = (Name,Expr Type)

synTypeDefGroups :: Name -> [LazyExpr] -> [Core.TypeDefGroup] -> KInfer (DefGroups Type)
synTypeDefGroups modName lazyExprs tdgroups
  = do xss <- mapM (synTypeDefGroup modName lazyExprs) tdgroups
       return (concat xss)

synTypeDefGroup :: Name -> [LazyExpr] -> Core.TypeDefGroup -> KInfer (DefGroups Type)
synTypeDefGroup modName lazyExprs (Core.TypeDefGroup ctdefs)
  = do xss <- mapM (synTypeDef modName lazyExprs) ctdefs
       return (concat xss)

synTypeDef :: Name -> [LazyExpr] -> Core.TypeDef -> KInfer (DefGroups Type)
synTypeDef modName lazyExprs (Core.Synonym synInfo) = return []
synTypeDef modName lazyExprs (Core.Data dataInfo) | isHiddenName (dataInfoName dataInfo) = return []
synTypeDef modName lazyExprs (Core.Data dataInfo)
  = do synLazy <-
          if (dataInfoIsLazy dataInfo)
           then do evalLocked <- synLazyEvalLocked lazyExprs dataInfo
                   return ([synLazyTag dataInfo] ++ evalLocked ++ [synLazyEval dataInfo] ++
                           [synLazyWhnf dataInfo, synLazyForce dataInfo, synLazyStep dataInfo])
           else return []
       return $
        (if not (dataInfoIsOpen dataInfo || dataInfoIsLazy dataInfo) then synAccessors modName dataInfo else [])
        ++
        (if (length (dataInfoConstrs dataInfo) == 1
            && not (dataInfoIsOpen dataInfo || dataInfoIsLazy dataInfo)
            && not (isHiddenName (conInfoName (head (dataInfoConstrs dataInfo))))
            && hasKindStarResult (dataInfoKind dataInfo) )
          then [synCopyCon modName dataInfo (head (dataInfoConstrs dataInfo))]
          else [])
        ++
        (if (length (dataInfoConstrs dataInfo) > 1 || (dataInfoIsOpen dataInfo)) && not (dataInfoIsLazy dataInfo)
          then concatMap (synTester dataInfo) (dataInfoConstrs dataInfo)
          else [])
        ++
        (if (dataInfoIsOpen dataInfo)
          then map synConstrTag (dataInfoConstrs dataInfo)
          else [])
        ++
        synLazy


synCopyCon :: Name -> DataInfo -> ConInfo -> DefGroup Type
synCopyCon modName info con
  = let rc = rangeHide (conInfoRange con)
        defName = unqualify $ copyNameOf (dataInfoName info)

        fullTp = let (vars,rho) = splitTypeScheme (conInfoType con)
                 in case splitFunType rho of
                      Just (args,eff,res)
                        -> TForall vars (TFun ([(argName,res)] ++ [(name,if not (hasAccessor name t con)
                                                                                 then t else makeOptionalType t) | (name,t) <- args]) eff res)
                      Nothing
                        -> TForall vars (TFun [(argName,rho)] typeTotal rho)    -- for unary constructors, like unit

        var n = Var n False rc
        app x []  = x
        app x xs  = App x [(Nothing,y) | y <- xs] rc

        argName  = newName "@this"

        params = [ValueBinder fldName Nothing
                  (if not (hasAccessor fldName t con)
                     then Nothing
                     else (Just (app (var (typeQualifiedNameOf (dataInfoName info) fldName)) [var argName]))) rc rc
                 | (fldName,t) <- conInfoParams con]
        expr = Lam ([ValueBinder argName Nothing Nothing rc rc] ++ params) body True rc
        body = app (var (conInfoName con)) [var name | (name,tp) <- conInfoParams con]
        def  = DefNonRec (Def (ValueBinder defName () (Ann expr fullTp rc) rc rc) rc (dataInfoVis info) (defFun []) InlineAuto "")
    in def

hasAccessor :: Name -> Type -> ConInfo -> Bool
hasAccessor name tp cinfo
  = not (isFieldName name || name==nameNil) &&  -- named?
    (let tvs = ftv tp          -- and no existentials?
     in all (\tv -> not (tvsMember tv tvs)) (conInfoExists cinfo))

synAccessors :: Name -> DataInfo -> [DefGroup Type]
synAccessors modName info
  = let paramss = map (\conInfo -> zipWith (\(name,tp) (pvis,rng) -> (name,(tp,rng,pvis,conInfo)))
                                   (conInfoParams conInfo) (zip (conInfoParamVis conInfo) (conInfoParamRanges conInfo)))
                      (dataInfoConstrs info)

        fields :: [(Name,(Type,Range,Visibility,ConInfo))]
        fields  = filter occursOnAllConstrs $
                  nubBy (\x y -> fst x == fst y) $
                  filter (\(name,(tp,_,_,cinfo)) -> hasAccessor name tp cinfo) $
                  concat paramss

        occursOnAllConstrs (name,(tp,rng,pvis,_))
          = not (nameIsNil name) &&
            all (\ps -> any (\(n,(t,_,_,_)) -> n == name && eqType t tp) ps) paramss

        synAccessor :: (Name,(Type,Range,Visibility,ConInfo)) -> DefGroup Type
        synAccessor (name,(tp,xrng,visibility,cinfo))
          = let rng      = rangeHide xrng

                -- dataName = unqualify $ dataInfoName info
                -- defName  = qualifyLocally (nameAsModuleName dataName) name -- TODO: only for type names that are valid module names!
                defName = unqualify $ typeQualifiedNameOf (dataInfoName info) name
                -- arg = newHiddenName "self"
                arg = let dataName = unqualify (dataInfoName info)
                      in if (all isAlphaNum (show dataName)) then dataName else newName "@this"
                fld = newHiddenName "x"

                dataTp = typeApp (TCon (TypeCon (dataInfoName info) (dataInfoKind info))) (map TVar (dataInfoParams info))
                fullTp = let (foralls,rho) = splitTypeScheme tp
                         in tForall (dataInfoParams info ++ foralls) $
                            typeFun [(arg,dataTp)] (if isPartial then typePartial else typeTotal) rho

                expr       = Ann (Lam [ValueBinder arg Nothing Nothing rng rng] caseExpr True rng) fullTp xrng
                caseExpr   = Case (Var arg False rng) (map snd branches ++ defaultBranch) False rng
                -- visibility = if (all (==Public) (map fst branches)) then Public else Private

                isPartial = (length branches < length (dataInfoConstrs info)) || dataInfoIsOpen info

                branches :: [(Visibility,Branch Type)]
                branches = concatMap makeBranch (dataInfoConstrs info)
                makeBranch (con)
                    = let r = conInfoRange con
                      in case lookup name (zip (map fst (conInfoParams con)) [0..]) of
                        Just i
                          -> let patterns = [(Nothing,PatWild r) | _ <- [0..i-1]] ++ [(Nothing,PatVar (ValueBinder fld Nothing (PatWild r) r r))] ++ [(Nothing,PatWild r) | _ <- [i+1..length (conInfoParams con)-1]]
                             in [(conInfoVis con,Branch (PatCon (conInfoName con) patterns r r)
                                                        [Guard guardTrue (Var fld False r)])]
                        Nothing -> []
                defaultBranch
                  = if isPartial
                     then [Branch (PatWild rng)
                             [Guard guardTrue (App (Var namePatternMatchError False rng) [(Nothing,msg) | msg <- messages] rng)]]
                     else []
                messages
                  = [Lit (LitString (sourceName (posSource (rangeStart rng)) ++ show rng) rng), Lit (LitString (show name) rng)]
                doc = "// Automatically generated. Retrieves the `" ++ show name ++ "` constructor field of the `:" ++ nameLocal (dataInfoName info) ++ "` type.\n"
            in DefNonRec (Def (ValueBinder defName () expr xrng xrng) rng visibility (defFunEx [Borrow] noFip) InlineAlways doc)

    in map synAccessor fields

synTester :: DataInfo -> ConInfo -> [DefGroup Type]
synTester info con | isHiddenName (conInfoName con) || conInfoIsLazy con
  = []
synTester info con
  = let name = (prependRaw "is-" (toVarName (unqualify (conInfoName con))))
        arg = unqualify $ dataInfoName info -- newHiddenName "self"
        rc  = rangeHide (conInfoRange con)

        expr      = Lam [ValueBinder arg Nothing Nothing rc rc] caseExpr True rc
        caseExpr  = Case (Var arg False rc) [branch1,branch2] (conInfoIsLazy con) rc
        branch1   = Branch (PatCon (conInfoName con) patterns rc rc) [Guard guardTrue (Var nameTrue False rc)]
        branch2   = Branch (PatWild rc) [Guard guardTrue (Var nameFalse False rc)]
        patterns  = [(Nothing,PatWild rc) | _ <- conInfoParams con]
        doc = "// Automatically generated. Tests for the `" ++ nameLocal (conInfoName con) ++ "` constructor of the `:" ++ nameLocal (dataInfoName info) ++ "` type.\n"
    in [DefNonRec (Def (ValueBinder name () expr rc rc) rc (conInfoVis con) (defFunEx [Borrow] (Fip (AllocAtMost 0))) InlineAlways doc)]

synConstrTag :: (ConInfo) -> DefGroup Type
synConstrTag (con)
  = let name = toOpenTagName (unqualify (conInfoName con))
        rc   = rangeHide (conInfoRange con)
        expr = Lit (LitString (show (conInfoName con)) rc)
        -- Tags must be public, since they may extend a type outside of the current module
    in DefNonRec (Def (ValueBinder name () expr rc rc) rc Public DefVal InlineNever "")

{- ------------------------------------------------------------------------------------------------------------
   Lazy types and constructors
------------------------------------------------------------------------------------------------------------ -}
{- add a lazy tag
   ```
   lazy type stream<a>
    SNil
    SCons( head : a, tail : stream<a> )
    lazy SAppRev( pre : stream<a>, post : list<a> ) ->
      match pre
        SNil        -> sreverse(post)
        SCons(x,xx) -> SCons(x,SAppRev(xx,post))

   stream/lazy-tag = 3
   ```
-}
synLazyTag :: DataInfo -> DefGroup Type
synLazyTag info
  = let xrng     = dataInfoRange info
        rng      = rangeHide xrng
        defName  = lazyName info "tag"
        lazyTag  = case find (isLazyIndirectConName . conInfoName) (dataInfoConstrs info) of
                      Nothing  -> failure $ "Kind.Infer.synIsWhnf: no indirection constructor found: " ++ show (dataInfoName info)
                      Just ind -> conInfoTag ind
        expr      = App (Var nameInternalInt32 False rng) [(Nothing,Lit (LitInt (toInteger lazyTag) rng))] rng
    in DefNonRec (Def (ValueBinder defName () expr rng rng) rng (dataInfoVis info) DefVal InlineAlways "// Automatically generated tag value of lazy indirection")

{- force is a one step unfolding of whnf and is inlined at call sites.
  inline fun stream/lazy-force( s : stream<a> ) : div stream<a>
    if lazy/datatype-is-whnf(s,stream/lazy-tag)
      then s
      else stream/lazy-whnf(s)
-}
synLazyForce :: DataInfo -> DefGroup Type
synLazyForce info
  = let xrng     = dataInfoRange info
        rng      = rangeHide xrng
        defName  = lazyName info "force"
        dataTp   = typeApp (TCon (TypeCon (dataInfoName info) (dataInfoKind info))) (map TVar (dataInfoParams info))
        fullTp   = tForall (dataInfoParams info) (typeFun [(nameNil,dataTp)] typeDivergent dataTp )
        nameIsWhnf = if (all (\conInfo -> not (null (conInfoParams conInfo))) (dataInfoConstrs info))
                       then nameLazyPtrIsWhnf else nameLazyIsWhnf

        argName   = newHiddenName "lazy"
        arg       = Var argName False rng
        expr      = (\e -> Ann e fullTp rng) $
                    Lam [ValueBinder argName Nothing Nothing rng rng] body True rng
        body      = Case tst [Branch (PatCon nameTrue [] rng rng) [Guard guardTrue arg]
                             ,Branch (PatCon nameFalse [] rng rng) [Guard guardTrue whnf]] False rng
        tst       = App (Var nameIsWhnf False rng) [(Nothing,arg),(Nothing,Var (lazyName info "tag") False rng)] rng
        whnf      = App (Var (lazyName info "whnf") False rng) [(Nothing,arg)] rng
    in DefNonRec (Def (ValueBinder defName () expr rng rng) rng (dataInfoVis info) (DefFun [] (lazyFip info)) InlineAlways "")


{- recursive force:
  noinline fun stream/lazy-whnf(s : stream<a> ) : div stream<a>
    val x = stream/lazy-eval(s,True)
    if lazy/datatype-is-whnf(x,steam/lazy-tag) then x else stream/lazy-whnf(x)
-}
synLazyWhnf :: DataInfo -> DefGroup Type
synLazyWhnf info
  = let xrng     = dataInfoRange info
        rng      = rangeHide xrng
        defName  = lazyName info "whnf"
        dataTp   = typeApp (TCon (TypeCon (dataInfoName info) (dataInfoKind info))) (map TVar (dataInfoParams info))
        -- fullTp   = tForall (dataInfoParams info) [] (typeFun [(nameNil,dataTp)] typePure dataTp )
        nameIsWhnf = if (all (\conInfo -> not (null (conInfoParams conInfo))) (dataInfoConstrs info))
                       then nameLazyPtrIsWhnf else nameLazyIsWhnf

        valName   = newHiddenName "val"
        val       = Var valName False rng

        argName   = newHiddenName "lazy"
        arg       = Var argName False rng

        nameEval  = lazyName info "eval"
        evalExpr  = App (Var nameEval False rng) [(Nothing,arg),(Nothing,Var nameTrue False rng)] rng
        valDef    = Def (ValueBinder valName () evalExpr rng rng) rng Private DefVal InlineNever ""

        expr      = Lam [ValueBinder argName Nothing Nothing rng rng] body True xrng
        body      = Bind valDef
                    (Case tst [Branch (PatCon nameTrue [] rng rng) [Guard guardTrue val]
                              ,Branch (PatCon nameFalse [] rng rng) [Guard guardTrue whnf]] False rng) rng
        tst       = App (Var nameIsWhnf False rng) [(Nothing,val),(Nothing,Var (lazyName info "tag") False rng)] rng
        whnf      = App (Var (lazyName info "whnf") False rng) [(Nothing,val)] rng
    in DefNonRec (Def (ValueBinder defName () expr rng rng) rng (dataInfoVis info) (DefFun [] (lazyFip info)) InlineNever "")


{-
Lazy step function
inline fun stream/lazy-step( @lazy : stream<a> ) : _ stream<a>
  if lazy/datatype-is-whnf(@lazy,stream/lazy-tag)
      then s
      else stream/lazy-eval(@lazy,False)
-}
synLazyStep :: DataInfo -> DefGroup Type
synLazyStep info
  = let xrng     = dataInfoRange info
        rng      = rangeHide xrng
        defName  = lazyName info "step"
        dataTp   = typeApp (TCon (TypeCon (dataInfoName info) (dataInfoKind info))) (map TVar (dataInfoParams info))
        -- fullTp   = tForall (dataInfoParams info) [] (typeFun [(nameNil,dataTp)] typeDivergent dataTp )
        nameIsWhnf = if (all (\conInfo -> not (null (conInfoParams conInfo))) (dataInfoConstrs info))
                       then nameLazyPtrIsWhnf else nameLazyIsWhnf

        argName   = newHiddenName "lazy"
        arg       = Var argName False rng
        expr      = -- (\e -> Ann e fullTp rng) $
                    Lam [ValueBinder argName Nothing Nothing rng rng] body True xrng
        body      = Case tst [Branch (PatCon nameTrue [] rng rng) [Guard guardTrue arg]
                             ,Branch (PatCon nameFalse [] rng rng) [Guard guardTrue eval]] False rng
        tst       = App (Var nameIsWhnf False rng) [(Nothing,arg),(Nothing,Var (lazyName info "tag") False rng)] rng
        eval      = App (Var (lazyName info "eval") False rng) [(Nothing,arg),(Nothing,Var nameFalse False rng)] rng
    in DefNonRec (Def (ValueBinder defName () expr rng rng) rng (dataInfoVis info) (DefFun [] (lazyFip info)) InlineAlways "")


{-
Lazy eval function

noinline fun stream/lazy-eval(s : stream<a>, recurse : bool ) : _ stream<a>
  if lazy/datatype-ptr-is-unique || !lazy/atomic-enter(s,stream/lazy-tag) then
    stream/lazy-eval-locked(s,recurse)
  else
    val v = stream/lazy-eval-locked(s,recurse)
    lazy/atomic-leave(s)
    v
-}
synLazyEval :: DataInfo -> DefGroup Type
synLazyEval info
  = let xrng     = dataInfoRange info
        rng      = rangeHide xrng
        defName  = lazyName info "eval"
        dataTp   = typeApp (TCon (TypeCon (dataInfoName info) (dataInfoKind info))) (map TVar (dataInfoParams info))
        fullTp   = tForall (dataInfoParams info) (typeFun [(nameNil,typeBool),(nameNil,dataTp)] typePure dataTp )

        recName   = newHiddenName "recurse"
        argName   = newHiddenName "lazy"
        recurse   = Var recName False rng
        arg       = Var argName False rng
        expr      = Lam [ValueBinder argName Nothing Nothing rng rng,ValueBinder recName (Just typeBool) Nothing rng rng] body True xrng
        body      = Case tst [Branch (PatCon nameTrue [] rng rng) [Guard guardTrue eval]
                             ,Branch (PatCon nameFalse [] rng rng) [Guard guardTrue atomic]] False rng
        tst       = App (Var nameOr False rng)
                     [(Nothing,App (Var nameDataTypePtrIsUnique False rng) [(Nothing,arg)] rng),
                      (Nothing,App (Var (newQualified "std/core/types" "not") False rng)
                                   [(Nothing, App (Var nameLazyEnter False rng)
                                                  [(Nothing,arg),(Nothing,Var (lazyName info "tag") False rng)] rng)] rng)] rng
        eval      = App (Var (lazyName info "eval-locked") False rng) [(Nothing,arg),(Nothing,recurse)] rng
        atomic    = let v = newName "v"
                        vdef = Def (ValueBinder v () eval rng rng) rng Private DefVal InlineNever ""
                        bdef = Def (ValueBinder nameNil () (App (Var nameLazyLeave False rng) [(Nothing,arg)] rng) rng rng) rng Private DefVal InlineNever ""
                    in Bind vdef (Bind bdef (Var v False rng) rng) rng
    in DefNonRec (Def (ValueBinder defName () expr rng rng) rng (dataInfoVis info) (DefFun [] (lazyFip info)) InlineNever "")



{-
  Add lazy evaluation:
  ```
  noinline fbip fun stream/lazy-eval-locked( s : stream<a>,recurse : bool ) : _ stream<a>
    lazy-whnf-target(s)
    match s
      SAppRev(pre, post) -> stream/lazy-SAppRev( s, recurse, pre, post )
      SIndirect(ind)     -> ind
      _                  -> s
  ```
-}
synLazyEvalLocked :: [LazyExpr] -> DataInfo -> KInfer [DefGroup Type]
synLazyEvalLocked lazyExprs info
  = do let  xrng     = dataInfoRange info
            rng      = rangeHide xrng

            defName  = lazyName info "eval-locked"
            recName  = newHiddenName "recurse"
            argName  = newHiddenName "lazy"
            recurse  = Var recName False rng
            arg      = Var argName False rng

            rotate (x:xs) = xs ++ [x]
            rotate []     = []
            lazyConstrs  = rotate (filter conInfoIsLazy (dataInfoConstrs info))  -- indirect match comes last


            -- dataTp    = typeApp (TCon (TypeCon (dataInfoName info) (dataInfoKind info))) (map TVar (dataInfoParams info))
            -- fullTp    = tForall (dataInfoParams info) [] (typeFun [(nameNil,dataTp)] typePure dataTp )

            -- generate branch
            branch :: ConInfo -> KInfer ([Def Type],Branch Type)
            branch conInfo | isLazyIndirectConName (conInfoName conInfo)
              = let [(par,tp)] = conInfoParams conInfo
                    compress   = App (Var nameLazyIndirectCompress False rng)
                                     [(Nothing, Var par False rng),(Nothing, Var (lazyName info "tag") False rng)] rng
                in return $ ([],Branch (PatCon (conInfoName conInfo) [(Nothing,PatVar (ValueBinder par Nothing (PatWild rng) rng rng))] rng rng)
                                 [Guard guardTrue compress])
            branch conInfo
              = do let parNames = [if isWildcard par then postpendRaw (show i) (unWildcard par) else par | (i,(par,tp)) <- zip [1..] (conInfoParams conInfo)]
                   -- return $ ([], Branch (PatCon (conInfoName conInfo) [(Nothing,makePat par rng) | par <- parNames] rng rng)
                   --                  [Guard guardTrue arg])
                   (def,body) <- branchExpr conInfo parNames
                   return $ ([def], Branch (PatCon (conInfoName conInfo) [(Nothing,makePat par rng) | par <- parNames] rng rng)
                                     [Guard guardTrue body])
              where
                makePat par rng
                  = if (isWildcard par) then PatWild rng else PatVar (ValueBinder par Nothing (PatWild rng) rng rng)

            branchExpr :: ConInfo -> [Name] -> KInfer (Def Type,Expr Type)
            branchExpr conInfo parNames
              = case lookup (conInfoName conInfo) lazyExprs of
                  Just lazyExpr -> lazyConDefCall info conInfo parNames recurse arg lazyExpr
                  Nothing -> failure $ "Kind.Infer.synLazyEval.branchExpr: cannot find expression for lazy constructor " ++ show (conInfoName conInfo)

       (defss,branches) <- unzip <$> mapM branch lazyConstrs

       let  body      = Case arg (branches ++ [Branch (PatWild rng) [Guard guardTrue arg]]) True rng
            lam       = Lam [ValueBinder argName Nothing Nothing rng rng,ValueBinder recName (Just typeBool) Nothing rng rng] body True xrng
            def       = Def (ValueBinder defName () lam rng rng) rng (dataInfoVis info) (DefFun [] (lazyFip info)) InlineNever ""
       return $ (map DefNonRec (concat defss)) ++ [DefNonRec def]


{- Lazy constructor
  ```
  SAppRev( pre, post )
      -> match pre
           SNil        -> sreverse(post)
           SCons(x,xx) -> SCons(x,SAppRev(xx,post))
  ```
  ~>
  ```
  fun stream/lazy-SAppRev(  @lazy : stream<a>, recurse : bool, pre : stream<a>, post : stream<a> ) : stream<a>
    lazy/memoize-target(@lazy)
    match pre
      SNil        -> lazy/memoize(@lazy,sreverse(post))
                  or: if recurse then lazy-SAppRev(s,recurse,...) else lazy/memoize(@lazy,SAppRev(...))
      SCons(x,xx) -> lazy/memoize(@lazy,SCons(x,SAppRev(xx,post)))
  ```
-}


type ErrDoc = ColorScheme -> Doc


lazyConDefCall :: DataInfo -> ConInfo -> [Name] -> Expr Type -> Expr Type -> Expr Type -> KInfer (Def Type,Expr Type)
lazyConDefCall info conInfo parNames recurseArg memoTarget topExpr
  = do platform <- getPlatform
       let rng         = conInfoRange conInfo
           -- lazy-SAppRev(@recurse,@lazy,pre,post)
           callExpr    = App (Var nameLazyCon False rng)
                             ([(Nothing,memoTarget),(Nothing,recurseArg)] ++ [(Nothing,Var par False rng) | par <- parNames]) rng

       branchExpr <- memoizeExpr topExpr

       let userTpCon  = TpCon (dataInfoName info) rng
           userTpVarNames = [newName ("_" ++ show tv) | tv <- dataInfoParams info]
           userDataTp = if null userTpVarNames then userTpCon
                          else TpApp userTpCon [TpVar tv rng | tv <- userTpVarNames] rng
           userConTps = [(par, TpVar (newName ("_" ++ show i)) rng) | (i,(par,_)) <- zip [1..] (conInfoParams conInfo)] -- todo: improve types?
           userFullTp = TpFun ([(nameNil, userDataTp),(nameNil, TpCon nameTpBool rng)]
                               ++ userConTps)
                               (TpVar (newName "_eff") rng)
                               userDataTp
                               rng
       fullTp <- infResolveType (promoteType userFullTp) (Infer rng) -- todo: use Check?

       let -- fun lazy-SAppRev(@memo,recurse,pre,post)
           --   lazy/memoize-target(@memo)
           --   <memoize topExpr>
           def     = Def (ValueBinder nameLazyCon () (Ann lam fullTp rng) rng rng) rng Private (DefFun [] (conInfoLazyFip conInfo)) InlineAuto ""
           lam     = Lam ([ValueBinder nameLazyMemo Nothing Nothing rng rng,ValueBinder nameRecurse (Just typeBool) Nothing rng rng]
                           ++ [ValueBinder par Nothing Nothing rng rng | par <- parNames])
                         (Bind target branchExpr rng) True rng
           (targetSize,targetScan) = Core.conReprAllocSizeScan platform (Core.getConRepr info conInfo)
           target  = Def (ValueBinder nameNil ()
                         (App (Var nameLazyMemoizeTarget False rng)
                          [(Nothing,Var nameLazyMemo False rng),
                           (Nothing,Lit (LitInt (toInteger targetSize) rng)),
                           (Nothing,Lit (LitInt (toInteger targetScan) rng))] rng) rng rng) rng Private DefVal InlineNever ""
           -- conExpr = makeApp (Var (conInfoName conInfo) False rng)
           --                  [(Nothing,Var par False rng) | par <- parNames] rng

       return (def, callExpr)
  where
    nameLazyCon  = lazyName info (show (unqualify (conInfoName conInfo)))  -- lazy-SAppRev
    nameLazyMemo = newHiddenName "lazy"
    nameRecurse  = newHiddenName "recurse"

    (lazyConstrs,whnfConstrs) = partition conInfoIsLazy (dataInfoConstrs info)
    modName      = qualifier (dataInfoName info)

    lookupCon cname constrs
      = find (\cinfo -> (if isQualified cname then cname else qualify modName cname) == conInfoName cinfo) constrs

    isWhnfCon cname
      = isJust (lookupCon cname whnfConstrs)

    -- memoizeExpr :: Expr t -> KInfer (Expr t)
    memoizeExpr expr
      = case expr of
          Lam    binds expr tl rng  -> (\expr' -> Lam binds expr' tl rng) <$> memoizeExpr expr
          Let    defs expr range -> (\expr' -> Let defs expr' range) <$> memoizeExpr expr
          Bind   def expr range  -> (\expr' -> Bind def expr' range) <$> memoizeExpr expr
          Ann    expr tp range   -> (\expr' -> Ann expr' tp range) <$> memoizeExpr expr
          Case   expr brs lazyMatch range
            -> do brs' <- mapM memoizeBranch brs
                  -- specialize force insertion here so the dependency is known
                  -- for all other matches, this is done during type inference
                  let isWhnfConPat p
                        = case p of
                            PatParens pat _      -> isWhnfConPat pat
                            PatAnn pat _ _       -> isWhnfConPat pat
                            PatVar v             -> isWhnfConPat (binderExpr v)
                            PatCon cname _ _ _   -> isWhnfCon cname
                            _                    -> False
                      isWhnfConBranch (Branch pat guards) = isWhnfConPat pat
                      forceName = lazyName info "force"
                      exprForce = if not lazyMatch && any isWhnfConBranch brs
                                    then let r = rangeHide (getRange expr)
                                         in App (Var forceName False r) [(Nothing,expr)] r
                                    else expr
                  return (Case exprForce brs' lazyMatch range)
          Parens expr name pre range -> do expr' <- memoizeExpr expr
                                           return $ Parens expr' name pre range
          App con@(Var cname _ _) nargs range
            -> memoizeCon cname con nargs range
          Var cname _ range
            -> memoizeCon cname expr [] range
          _ -> memoizeUnknown expr

    memoizeBranch (Branch pattern guards)
      = do guards' <- mapM memoizeGuard guards
           return $ Branch pattern guards'

    memoizeGuard (Guard test body)
      = do body' <- memoizeExpr body
           return $ Guard test body'


    memoizeWarning rng fdoc
      = do cs <- getColorScheme
           let showConName name = color (colorCons cs) (pretty (unqualify name))
           addWarning rng $ text "Cannot update the lazy constructor" <+> showConName (conInfoName conInfo) <+> fdoc showConName


    memoizeCon cname con nargs range
      = case lookupCon cname whnfConstrs of
          -- strict constructor result
          Just whnfCon -> do memoizeCheckCanFit range whnfCon
                             memoize (makeApp con nargs range)
          -- lazy constructor
          Nothing -> case lookupCon cname lazyConstrs of
                       -- unknown constructor
                       Nothing     -> memoizeUnknown (makeApp con nargs range)
                       -- known lazy constructor
                       Just cinfo  -> do platform <- getPlatform
                                         let (targetSize,targetScan) = Core.conReprAllocSizeScan platform (Core.getConRepr info conInfo)
                                             (resultSize,resultScan) = Core.conReprAllocSizeScan platform (Core.getConRepr info cinfo)
                                         if targetSize >= resultSize && targetScan >= resultScan
                                           then do recCall  <- -- recursively call a lazy constructor function directly
                                                                do let conFunName = lazyName info (show (unqualify (conInfoName cinfo)))
                                                                   return (App (Var conFunName False range)
                                                                              ([(Nothing,Var nameLazyMemo False range),(Nothing,Var nameRecurse False range)] ++ nargs) range)
                                                   stepCall <-  -- step function; return the lazy constructor
                                                                do memoize (makeApp con nargs range)
                                                   let ifexpr = Case (Var nameRecurse False range)
                                                                 [Branch (PatCon nameTrue [] range range) [Guard guardTrue recCall]
                                                                 ,Branch (PatCon nameFalse [] range range) [Guard guardTrue stepCall]] False range
                                                   return ifexpr
                                           else -- memoize and return; whnf does the recursion
                                                do memoizeWarning range $ \_ -> text "in-place as the result constructor does not have the same size as the lazy target (" <.> pretty targetSize <+> text "vs" <+> pretty resultSize <+> text "bytes) -- using an indirection instead"
                                                   memoize (makeApp con nargs range)

    makeApp con [] range    = con
    makeApp con nargs range = App con nargs range

    memoizeUnknown expr
      = do let rng = rangeHide $ getRange expr
           memoizeWarning rng $ \_ -> text "in-place as the result constructor is not statically known -- using an indirection instead"
           memoize expr -- (App (Var evalName False rng) [(Nothing,expr)] rng)

    -- a lazy constructor of our datatype
    recursiveMemoize expr cinfo
      = do let rng = rangeHide $ getRange expr
           -- no need to warn anymore as we don't force recursively here
           -- memoizeWarning rng $ \showCon -> text "in-place as the lazy result constructor" <+> showCon (conInfoName cinfo) <+> text "needs to be recursively forced -- using an indirection instead"
           memoizeCheckCanFit (getRange expr) cinfo
           memoize expr -- (App (Var evalName False rng) [(Nothing,expr)] rng)

    memoizeCheckCanFit range resultCon
      = do platform <- getPlatform
           let (targetSize,targetScan) = Core.conReprAllocSizeScan platform (Core.getConRepr info conInfo)
               (resultSize,resultScan) = Core.conReprAllocSizeScan platform (Core.getConRepr info resultCon)
           when (targetSize < resultSize) $
             memoizeWarning range $ \_ -> text "in-place as the result constructor is larger (" <.> pretty targetSize <+> text "vs" <+> pretty resultSize <+> text "bytes) -- using an indirection instead"


    -- memoize :: Expr t -> KInfer (Expr t)
    memoize expr
      = let rng = rangeHide $ getRange expr
        in return $ App (Var nameLazyMemoize False rng) [(Nothing,Var nameLazyMemo False rng),(Nothing,expr)] rng


lazyAddUpdate :: DataInfo -> ConInfo -> Name -> Expr t -> Expr t -> KInfer (Expr t)
lazyAddUpdate info conInfo evalName arg topExpr
  = add topExpr
  where
    (lazyConstrs,whnfConstrs) = partition conInfoIsLazy (dataInfoConstrs info)
    modName     = qualifier (dataInfoName info)

    lookupCon cname constrs
      = find (\cinfo -> (if isQualified cname then cname else qualify modName cname) == conInfoName cinfo) constrs

    isWhnfCon cname
      = isJust (lookupCon cname whnfConstrs)

    -- add :: Expr t -> KInfer (Expr t)
    add expr
      = case expr of
          Lam    binds expr tl rng  -> (\expr' -> Lam binds expr' tl rng) <$> add expr
          Let    defs expr range -> (\expr' -> Let defs expr' range) <$> add expr
          Bind   def expr range  -> (\expr' -> Bind def expr' range) <$> add expr
          Ann    expr tp range   -> (\expr' -> Ann expr' tp range) <$> add expr
          Case   expr brs lazyMatch range
            -> do brs' <- mapM addBranch brs
                  -- specialize force insertion here so the dependency is known
                  -- for all other matches, this is done during type inference
                  let isWhnfConPat p
                        = case p of
                            PatParens pat _      -> isWhnfConPat pat
                            PatAnn pat _ _       -> isWhnfConPat pat
                            PatVar v             -> isWhnfConPat (binderExpr v)
                            PatCon cname _ _ _   -> isWhnfCon cname
                            _                    -> False
                      isWhnfConBranch (Branch pat guards) = isWhnfConPat pat
                      forceName = lazyName info "force"
                      exprForce = if not lazyMatch && any isWhnfConBranch brs
                                    then let r = rangeHide (getRange expr)
                                         in App (Var forceName False r) [(Nothing,expr)] r
                                    else expr
                  return (Case exprForce brs' lazyMatch range)
          Parens expr name pre range -> do expr' <- add expr
                                           return $ Parens expr' name pre range
          App (Var cname _ _) nargs range
            -> lazyUpdateCon cname expr
          Var cname _ _
            -> lazyUpdateCon cname expr
          _ -> unknownUpdate expr

    addBranch (Branch pattern guards)
      = do guards' <- mapM addGuard guards
           return $ Branch pattern guards'

    addGuard (Guard test body)
      = do body' <- add body
           return $ Guard test body'


    updateWarning rng fdoc
      = do cs <- getColorScheme
           let showConName name = color (colorCons cs) (pretty (unqualify name))
           addWarning rng $ text "Cannot update the lazy constructor" <+> showConName (conInfoName conInfo) <+> fdoc showConName


    lazyUpdateCon cname expr
      = case lookupCon cname whnfConstrs of
          Nothing -> case lookupCon cname lazyConstrs of
                       Nothing     -> unknownUpdate expr
                       Just cinfo  -> recursiveUpdate expr cinfo
          Just whnfCon -> do warnUpdate (getRange expr) whnfCon
                             lazyUpdate expr

    unknownUpdate expr
      = do let rng = rangeHide $ getRange expr
           updateWarning rng $ \_ -> text "in-place as the result constructor is not statically known -- using an indirection instead"
           lazyUpdate expr -- (App (Var evalName False rng) [(Nothing,expr)] rng)

    recursiveUpdate expr cinfo
      = do let rng = rangeHide $ getRange expr
           -- no need to warn anymore as we don't force recursively here
           -- updateWarning rng $ \showCon -> text "in-place as the lazy result constructor" <+> showCon (conInfoName cinfo) <+> text "needs to be recursively forced -- using an indirection instead"
           warnUpdate (getRange expr) cinfo
           lazyUpdate expr -- (App (Var evalName False rng) [(Nothing,expr)] rng)

    warnUpdate range resultCon
      = do platform <- getPlatform
           let lazySize = Core.conReprAllocSize platform (Core.getConRepr info conInfo)
               whnfSize = Core.conReprAllocSize platform (Core.getConRepr info resultCon)
           when (lazySize < whnfSize) $
             updateWarning range $ \_ -> text "in-place as the result constructor is larger (" <.> pretty lazySize <+> text "vs" <+> pretty whnfSize <+> text "bytes) -- using an indirection instead"


    -- lazyUpdate :: Expr t -> KInfer (Expr t)
    lazyUpdate expr
      = let rng = rangeHide $ getRange expr
        in return $ App (Var nameLazyMemoize False rng) [(Nothing,arg),(Nothing,expr)] rng


lazyFip :: HasCallStack => DataInfo -> Fip
lazyFip info
  = case dataInfoDef info of
      DataDefLazy fip -> fip
      _               -> failure ("Kind.Infer.lazyFip: not a lazy data type? " ++ show (dataInfoName info))


{---------------------------------------------------------------
  Types for constructors
---------------------------------------------------------------}
constructorGamma :: (DataInfo -> Bool) -> [DataInfo] -> Gamma
constructorGamma isValue dataInfos
  = conInfoGamma (concatMap (\info -> zip (dataInfoConstrs info) (snd (Core.getDataReprEx isValue info))) dataInfos)
  where
    conInfoGamma conInfos
      = gammaNew [(conInfoName conInfo,InfoCon (conInfoVis conInfo) (conInfoType conInfo) conRepr conInfo (conInfoRange conInfo) (conInfoDoc conInfo)) | (conInfo,conRepr) <- conInfos]

constructorCheckDuplicates :: ColorScheme -> [ConInfo] -> [(Range,Doc)]
constructorCheckDuplicates cscheme conInfos
  = concatMap duplicate $ groupBy sameName conInfos
  where
    sameName ci1 ci2
      = conInfoName ci1 == conInfoName ci2

    duplicate (ci1:ci2:_)
      = [(conInfoRange ci2
         ,text "Constructor" <+> color (colorSource cscheme) (pretty (conInfoName ci2)) <+>
          text "is already defined at" <+> text (show (conInfoRange ci1)))]
    duplicate _
      = []


infImports :: Name -> Range -> [Import] -> KInfer ()
infImports modName modRange imports
  = do addRangeInfo modRange  (Id modName NIModule [] True)
       mapM_ infImport imports

infImport :: Import -> KInfer ()
infImport (Import alias qname aliasRange nameRange range vis isOpen)
  = do addRangeInfo nameRange  (Id qname NIModule [] True)
       addRangeInfo aliasRange (Id qname NIModule [] True)

{---------------------------------------------------------------
  Infer kinds for type definition groups
---------------------------------------------------------------}
infTypeDefGroups :: [TypeDefGroup UserType UserKind] -> KInfer ([Core.TypeDefGroup],KGamma,Synonyms,Newtypes,[LazyExpr])
infTypeDefGroups (tdgroup:tdgroups)
  = do (ctdgroup,lazyExprs)  <- infTypeDefGroup tdgroup
       (ctdgroups,kgamma,syns,datas,lazyExprss) <- extendKGamma (getRanges tdgroup) ctdgroup $ infTypeDefGroups tdgroups
       return (ctdgroup:ctdgroups,kgamma,syns,datas,lazyExprs ++ lazyExprss)
  where
    getRanges (TypeDefRec tdefs)   = map getRange tdefs
    getRanges (TypeDefNonRec tdef) = [getRange tdef]

infTypeDefGroups []
  = do kgamma <- getKGamma
       syns <- getSynonyms
       datas <- getAllNewtypes
       return ([],kgamma,syns,datas,[])

infTypeDefGroup :: TypeDefGroup UserType UserKind -> KInfer (Core.TypeDefGroup,[LazyExpr])
infTypeDefGroup (TypeDefRec tdefs)
  = infTypeDefs True tdefs

infTypeDefGroup (TypeDefNonRec tdef)
  = infTypeDefs False [tdef]

infTypeDefs :: Bool -> [TypeDef UserType UserType UserKind] -> KInfer (Core.TypeDefGroup,[LazyExpr])
infTypeDefs isRec tdefs0
  = do tdefs <- mapM addLazyIndirect tdefs0
       -- trace ("infTypeDefs: " ++ show (length tdefs)) $ return ()
       xinfgamma <- mapM bindTypeDef tdefs -- set up recursion
       let infgamma = map fst (filter snd xinfgamma)
       ctdefs' <- withDataEffects (concatMap getDataEffect (zip infgamma tdefs)) $ -- make effect types visible for lifting to handled
                   extendInfGamma infgamma $ -- extend inference gamma, also checks for duplicates
                   do let names = map tbinderName infgamma
                      tdefs1 <- -- trace ("recursive group: " ++ show infgamma) $
                                mapM infTypeDef (zip (map fst xinfgamma) tdefs)
                      mapM (resolveTypeDef isRec names) tdefs1
       checkRecursion tdefs -- check for recursive type synonym definitions rather late so we spot duplicate definitions first
       let (ctdefs,lazyExprss) = unzip ctdefs'
       return (Core.TypeDefGroup ctdefs, concat lazyExprss)
    where
      getDataEffect (tbind, DataType{ typeDefEffect = DataNoEffect })
          | isInfKindHandled (tbinderKind tbind) -- todo: add (n)handled(1) kind constructors?
          = [(tbinderName tbind, DataEffect False False)] -- for user declared `type eff :: HX`  `test/algeff/wrong/eff-rec5`
      getDataEffect (tbind, DataType{ typeDefEffect = deff })
          = [(tbinderName tbind, deff)]
      getDataEffect _ = []

checkRecursion :: [TypeDef UserType UserType UserKind] -> KInfer ()
checkRecursion tdefs
  = if (length tdefs <= 1 || any isDataType tdefs)
     then return ()
     else do addError (getRange tdefs) (text "Type synonyms cannot be recursive")
             return ()
  where
    isDataType (DataType{}) = True
    isDataType _            = False


{---------------------------------------------------------------
  Setup type environment for recursive definitions
---------------------------------------------------------------}
bindTypeDef :: TypeDef UserType UserType UserKind -> KInfer (TypeBinder InfKind,Bool {-not isExtend-})
bindTypeDef tdef -- extension
  = do (TypeBinder name kind rngName rng) <- bindTypeBinder (typeDefBinder tdef)
       qname <- if isExtend then return name else qualifyDef name
       return (TypeBinder qname kind rngName rng, not isExtend)
  where
    isExtend =
      case tdef of
        (DataType newtp args constructors range vis sort ddef dataEff doc) -> dataDefIsExtend ddef
        _ -> False

bindTypeBinder :: TypeBinder UserKind -> KInfer (TypeBinder InfKind)
bindTypeBinder (TypeBinder name userKind rngName rng)
  = do kind <- userKindToKind userKind
       return (TypeBinder name kind rngName rng)

userKindToKind :: UserKind -> KInfer InfKind
userKindToKind userKind
  = convert userKind
  where
    convert userKind
      = case userKind of
          KindCon name rng -> return $ KICon (KCon name)
          KindArrow k1 k2  -> do k1' <- convert k1
                                 k2' <- convert k2
                                 case (k1',k2') of
                                   (KICon kk1,KICon kk2) -> return (KICon (kindFun kk1 kk2))
                                   _ -> return $ KIApp k1' k2'
          KindParens k rng -> convert k
          KindNone         -> freshKind -- failure ("Kind.Infer.userKindToKind.convert: unexpected kindNone")


{---------------------------------------------------------------
  Infer kinds of external definitions
---------------------------------------------------------------}
infExternals :: [External] -> KInfer Core.Externals
infExternals externals
  = walk [] externals
  where
    walk names [] = return []
    walk names (external:externals)
      = do (exts1,names1)  <- infExternal names external
           exts2           <- walk names1 externals
           return (exts1 ++ exts2)

infExternal :: [Name] -> External -> KInfer ([Core.External],[Name])
infExternal names (External name tp pinfos nameRng rng ecall vis fip doc)
  = do tp' <- infResolveType tp (Check "Externals must be values" rng)
       qname <- qualifyDef name
       let cname = qname {- let n = length (filter (==qname) names) in
                   canonicalName n qname -}
       checkExternal cname nameRng
       if (isHiddenName name)
        then return ()
        else do addRangeInfo nameRng (Id qname (NIValue "extern" tp' doc True) [] True)
                addRangeInfo rng (Decl "extern" qname (mangle cname tp') (Just tp'))
       -- trace ("infExternal: " ++ show cname ++ ": " ++ show (pretty tp')) $
       tplatform <- getTargetPlatform
       return ([Core.External cname tp' pinfos (formatCall tplatform tp' ecall) vis fip nameRng doc]
               ,qname:names)
infExternal names (ExternalImport imports range)
  = do return ([Core.ExternalImport imports range], names)       

formatCall :: TargetPlatform -> Type -> (ExternalCall) -> String
formatCall tplatform tp (ExternalInline inline) = (inline)
formatCall tplatform tp (ExternalCall fname)
  = case tplTarget tplatform of
      CS      -> formatCS
      JS _    -> formatJS
      C _     -> formatC
      _       -> formatJS
  where
    (foralls,rho) = splitTypeScheme tp

    argumentCount
      = case splitFunType rho of
          Just (args,eff,res) -> length args
          Nothing             -> 0

    arguments
      = "(" ++ concat (intersperse "," ["#" ++ show i | i <- [1..argumentCount]]) ++ ")"

    typeArguments
      = if null foralls then ""
        else ("<" ++ concat (intersperse "," ["##" ++ show i | i <- [1..length foralls]]) ++ ">")

    formatJS
      = fname ++ arguments

    formatCS
      = fname ++ typeArguments ++ arguments

    formatC
      = fname ++ argumentsC
    argumentsC
      = "(" ++ concat (intersperse "," (["#" ++ show i | i <- [1..argumentCount]] ++ ctx)) ++ ")"
    ctx
      = if (fname `startsWith` "kk_") then ["kk_context()"] else []




infResolveType :: UserType -> Context -> KInfer Type
infResolveType tp ctx
  = do infTp <- infUserType infKindStar ctx tp
       resolveType M.empty False infTp

infResolveHX :: UserType -> Context -> KInfer Type
infResolveHX tp ctx
  = do infTp <- infUserType infKindHandled ctx tp
       resolveType M.empty False infTp

infResolveX :: UserType -> Context -> Range -> KInfer Type
infResolveX tp ctx rng
  = do ikind <- freshKind
       infTp <- infUserType ikind ctx tp
       skind <- subst ikind
       -- allow also effect label constructors without giving type parameters
       let (kargs,kres) = infExtractKindFun skind
       if (length kargs > 2 || (not (null kargs) && isInfKindLabel kres))  -- either user defined, or a primitive like `:local`
         then let vars     = [(newName ("_" ++ show i)) | i <- [1..(length kargs - (if isInfKindLabel kres then 0 else 2))]]
                  quals    = map (\(name) -> TypeBinder name KindNone rng rng) vars
                  tpvars   = map (\(name) -> TpVar name rng) vars
                  newtp    = foldr (\q t -> TpQuan QSome q t rng) (TpApp tp tpvars rng) quals
              in infResolveX newtp ctx rng -- recurse..
         else -- auto upgrade bare labels of HX or HX1 to X kind labels.
              do  effect <- case skind of
                              KICon kind | kind == kindLabel   -> return infTp
                              KICon kind | isKindHandled kind
                                -> do wrap <- -- trace ("lift: " ++ show infTp) $
                                              getEffectLift infTp
                                      return $ wrap infTp rng

                              -- KICon kind | isKindHandled kind  -> return (makeHandled infTp rng)
                              ---KICon kind | isKindHandled1 kind -> return (makeHandled1 infTp rng)
                              _ -> do trace ("infResolveX: " ++ show tp ++ ": " ++ show skind) $
                                       unify ctx rng infKindLabel skind
                                      return infTp
                  resolveType M.empty False effect



{---------------------------------------------------------------
  Infer kinds of definitions
---------------------------------------------------------------}
infDefGroups :: [DefGroup UserType] -> KInfer [DefGroup Type]
infDefGroups defgroups
  = mapM infDefGroup defgroups

infDefGroup :: DefGroup UserType -> KInfer (DefGroup Type)
infDefGroup (DefRec defs)
  = do defs' <- mapM infDef defs
       return (DefRec defs')
infDefGroup (DefNonRec def)
  = do def' <- infDef def
       return (DefNonRec def')

infDef :: (Def UserType) -> KInfer (Def Type)
infDef (Def binder rng vis isVal inl doc)
  = do binder' <- infValueBinder binder
       return (Def binder' rng vis isVal inl doc)

infValueBinder (ValueBinder name () expr nameRng rng)
  = do expr'   <- infExpr expr
       return (ValueBinder name () expr' nameRng rng)


infLamValueBinder (ValueBinder name mbTp mbExpr nameRng rng)
  = do mbTp' <- case mbTp of
                  Nothing -> return Nothing
                  Just tp -> do tp' <- infResolveType tp (Check "Function parameters must be values" rng)
                                return (Just tp')
       mbExpr' <- case mbExpr of
                  Nothing   -> return Nothing
                  Just (Parens (Var iname _ nrng) nm pre prng)  | isImplicitParamName name  -- ?? unpack
                            -> do (qname,ikind,_) <- findInfKind iname rng
                                  -- kind          <- resolveKind ikind
                                  -- addRangeInfo r (Id qname (NITypeCon kind) [] False)
                                  return (Just (Parens (Var qname False nrng) nm "implicit" prng))
                  Just expr -> do expr' <- infExpr expr
                                  return (Just expr')
       return (ValueBinder name mbTp' mbExpr' nameRng rng)

infPatValueBinder (ValueBinder name mbTp pat nameRng rng)
  = do mbTp' <- case mbTp of
                  Nothing -> return Nothing
                  Just tp -> do tp' <- infResolveType tp (Check "Matched expressions must be values" rng)
                                return (Just tp')
       pat'  <- infPat pat
       return (ValueBinder name mbTp' pat' nameRng rng)

infHandlerValueBinder (ValueBinder name mbTp () nameRng rng)
  = do mbTp' <- case mbTp of
                  Nothing -> return Nothing
                  Just tp -> do tp' <- infResolveType tp (Check "Handler parameters must be values" rng)
                                return (Just tp')
       return (ValueBinder name mbTp' () nameRng rng)


infExpr :: Expr UserType -> KInfer (Expr Type)
infExpr expr
  = case expr of
      Lam    binds expr tl rng  -> do binds' <- mapM infLamValueBinder binds
                                      expr' <- infExpr expr
                                      return (Lam binds' expr' tl rng)
      Let    defs expr range -> do defs' <- infDefGroup defs
                                   expr' <- infExpr expr
                                   return (Let defs' expr' range)
      Bind   def expr range  -> do def' <- infDef def
                                   expr' <- infExpr expr
                                   return (Bind def' expr' range)
      App    fun nargs range -> do fun'   <- infExpr fun
                                   let (names,exprs) = unzip nargs
                                   exprs' <- mapM infExpr exprs
                                   return (App fun' (zip names exprs') range)
      Var    name isOp range -> do -- resolve qualified term identifiers during type inference
                                   return (Var name isOp range)
      Lit    lit             -> return (Lit lit)
      Ann    expr tp range   -> do expr' <- infExpr expr
                                   tp'   <- infResolveType tp (Check "Expressions must be values" range)
                                   -- trace ("resolve ann: " ++ show (pretty tp')) $
                                   return (Ann expr' tp' range)
      Case   expr brs lazy range  -> do expr' <- infExpr expr
                                        brs'   <- mapM infBranch brs
                                        return (Case expr' brs' lazy range)
      Parens expr name pre range -> do expr' <- infExpr expr
                                       return (Parens expr' name pre range)
      Handler hsort scoped override allowMask meff pars reinit ret final ops hrng rng
                             -> do pars' <- mapM infHandlerValueBinder pars
                                   meff' <- case meff of
                                              Nothing  -> return Nothing
                                              Just eff -> do eff' <- infResolveX eff (Check "Handler types must be effect constants (of kind X)" hrng) hrng
                                                             return (Just eff')
                                   {-
                                   hsort' <- case hsort of
                                               HandlerResource (Just rexpr)
                                                -> do rexpr' <- infExpr rexpr
                                                      return (HandlerResource (Just rexpr'))
                                               HandlerInstance -> return HandlerInstance
                                               HandlerNormal -> return HandlerNormal
                                               -}
                                   ret' <- infExprMaybe ret
                                   reinit' <- infExprMaybe reinit
                                   final'  <- infExprMaybe final
                                   ops' <- mapM infHandlerBranch ops
                                   return (Handler hsort scoped override allowMask meff' pars' reinit' ret' final' ops' hrng rng)
      Inject tp expr b range-> do expr' <- infExpr expr
                                  tp'   <- infResolveX tp (Check "Can only inject effect constants (of kind X)" range) (getRange tp)
                                  -- trace ("resolved inject: " ++ show (pretty tp')) $
                                  return (Inject tp' expr' b range)

infExprMaybe mbExpr
  = case mbExpr of
      Nothing -> return Nothing
      Just expr -> do expr' <- infExpr expr
                      return (Just expr')

infPat pat
  = case pat of
      PatWild range           -> return (PatWild range)
      PatLit lit              -> return (PatLit lit)
      PatVar  binder          -> do binder' <- infPatValueBinder binder
                                    return (PatVar binder')
      PatAnn  pat tp range    -> do pat' <- infPat pat
                                    tp'  <- infResolveType tp (Check "Patterns must be values" range)
                                    return (PatAnn pat' tp' range)
      PatCon  name args r1 r2 -> do args' <- mapM (\(mbName,pat) -> do pat' <- infPat pat; return (mbName,pat')) args
                                    return (PatCon name args' r1 r2)
      PatParens pat range     -> do pat' <- infPat pat
                                    return (PatParens pat' range)


infHandlerBranch (HandlerBranch name pars expr opSort nameRng rng)
  = do pars' <- mapM infHandlerValueBinder pars
       expr' <- infExpr expr
       return (HandlerBranch name pars' expr' opSort nameRng rng)

infBranch (Branch pattern guards)
  = do pattern'<- infPat pattern
       guards' <- mapM infGuard guards
       return (Branch pattern' guards')

infGuard (Guard test body)
  = do test' <- infExpr test
       body' <- infExpr body
       return (Guard test' body')


{---------------------------------------------------------------
  Infer the kinds for a type definition
---------------------------------------------------------------}
infTypeDef :: (TypeBinder InfKind, TypeDef UserType UserType UserKind) -> KInfer (TypeDef (KUserType InfKind) UserType InfKind)
infTypeDef (tbinder, Synonym syn args tp range vis doc)
  = do infgamma <- mapM bindTypeBinder args
       kind <- freshKind
       tp' <- extendInfGamma infgamma (infUserType kind (Infer range) tp)
       tbinder' <- unifyBinder tbinder syn range infgamma kind
       return (Synonym tbinder' infgamma tp' range vis doc)

infTypeDef (tbinder, td@(DataType newtp args constructors range vis sort ddef dataEff doc))
  = do infgamma <- mapM bindTypeBinder args
       constructors' <- extendInfGamma infgamma (mapM infConstructor constructors)
       -- todo: unify extended datatype kind with original
       reskind <- if dataDefIsOpen ddef then return infKindStar else freshKind
       tbinder' <- unifyBinder tbinder newtp range infgamma reskind
       if not (dataDefIsExtend ddef) then return ()
        else do (qname,kind,_) <- findInfKind (tbinderName newtp) (tbinderRange newtp)
                unify (Check "extended type must have the same kind as the open type" (tbinderRange newtp) ) (tbinderRange newtp) (typeBinderKind tbinder') kind
       return (DataType tbinder' infgamma constructors' range vis sort ddef dataEff doc)

unifyBinder tbinder defbinder range infgamma reskind
 = do let kind = infKindFunN (map typeBinderKind infgamma) reskind
      unify (Infer range) range (typeBinderKind tbinder) kind
      return tbinder

typeBinderKind (TypeBinder name kind _ _) = kind

infConstructor :: UserCon UserType UserType UserKind -> KInfer (UserCon (KUserType InfKind) UserType InfKind)
infConstructor (UserCon name exist params mbresult mblazy rngName rng vis doc)
  = do infgamma <- mapM bindTypeBinder exist
       params'  <- extendInfGamma infgamma (mapM infConValueBinder params)
       result'  <- case mbresult of
                     Nothing -> return Nothing
                     Just tp -> do tp' <- extendInfGamma infgamma $ infUserType infKindStar (Check "Constructor results must be values" rng) tp
                                   return (Just tp')
       return (UserCon name infgamma params' result' mblazy rngName rng vis doc)

infConValueBinder :: (Visibility,ValueBinder UserType (Maybe (Expr UserType))) -> KInfer (Visibility,ValueBinder (KUserType InfKind) (Maybe (Expr UserType)))
infConValueBinder (vis,ValueBinder name tp mbExpr nameRng rng)
  = do tp' <- infUserType infKindStar (Check "Constructor parameters must be values" rng) tp
       return (vis,ValueBinder name tp' mbExpr nameRng rng)


infUserType :: InfKind -> Context -> UserType -> KInfer (KUserType InfKind)
infUserType expected  context userType
  = let range = getRange userType in
    case userType of
      TpQuan quant tname tp rng
        -> do ikind  <- case quant of
                          QSome -> return expected
                          _     -> do unify context range expected infKindStar
                                      return expected
              tname' <- bindTypeBinder tname
              tp'    <- extendInfGamma [tname'] $
                        infUserType ikind (checkQuant range) tp
              return (TpQuan quant tname' tp' rng)
      TpFun args effect tp rng
        -> do unify context range expected infKindStar
              args'   <- mapM (infParam infKindStar (checkArg range)) args
              -- somewhat involved since we auto-wrap labels L into effects E here
              ekind   <- freshKind
              etp     <- infUserType ekind (checkEff range) effect
              skind   <- subst ekind
              effect' <- case skind of
                          KICon kind | kind == kindLabel -> return (makeEffectExtend etp makeEffectEmpty)
                          KICon kind | isKindHandled kind
                            -> do wrap <- getEffectLift etp
                                  return $ makeEffectExtend (wrap etp rng) makeEffectEmpty
                          {-
                          KICon kind | isKindHandled kind ->  -- TODO: check if there is an effect declaration
                                          return (makeEffectExtend (makeHandled etp rng) makeEffectEmpty)
                          KICon kind | isKindHandled1 kind ->  -- TODO: check if there is an effect declaration
                                          return (makeEffectExtend (makeHandled1 etp rng) makeEffectEmpty)
                          -}
                          _  -> do unify (checkEff range) range (KICon kindEffect) skind
                                   return etp
              tp'     <- infUserType infKindStar (checkRes range) tp
              return (TpFun args' effect' tp' rng)

      TpApp tp@(TpCon name _) [lab,tl] rng  | name == nameEffectExtend
        -> do tp'   <- infUserType (infKindFunN [KICon kindLabel,KICon kindEffect] expected) (checkApp range) tp
              tl'   <- infUserType (KICon kindEffect) (checkExtendTail range) tl
              -- somewhat involved since we allow fixed effects to be used as labels
              -- the append nodes are just used temporarily (see resolveApp in this file)
              lkind <- freshKind
              ltp   <- infUserType lkind (Infer range) lab
              skind <- subst lkind
              case skind of
                KICon kind | kind == kindEffect
                  -> return (makeEffectAppend ltp tl')
                KICon kind | isKindHandled kind -- TODO: check effects environment if really effect?
                  -> do unify (checkExtendLabel range) range (KICon kindHandled) skind
                        wrap <- getEffectLift ltp
                        return (TpApp tp' [wrap ltp rng, tl'] rng)
                        {-
                KICon kind | isKindHandled kind -- TODO: check effects environment if really effect?
                  -> do unify (checkExtendLabel range) range (KICon kindHandled) skind
                        return (TpApp tp' [makeHandled ltp rng, tl'] rng)
                KICon kind | isKindHandled1 kind -- TODO: check effects environment if really effect?
                  -> do unify (checkExtendLabel range) range (KICon kindHandled1) skind
                        return (TpApp tp' [makeHandled1 ltp rng, tl'] rng)
                        -}
                _ -> do unify (checkExtendLabel range) range (KICon kindLabel) skind
                        return (TpApp tp' [ltp,tl'] rng)

      TpApp tp args rng
        -> do kinds <- mapM (\arg -> freshKind) args
              tp'   <- infUserType (infKindFunN kinds expected) (checkApp range) tp
              args' <- mapM (\(kind,arg) -> infUserType kind (Infer range) arg) (zip kinds args)
              return (TpApp tp' args' rng)
      TpVar name rng
        -> do (qname,kind,_) <- findInfKind name rng
              unify context range expected kind
              return (TpVar qname rng)
      TpCon name rng
        -> do (qname,kind,_) <- findInfKind name rng
              unify context range expected kind
              return (TpCon qname rng)
      TpParens tp rng
        -> do tp' <- infUserType expected context tp
              return (TpParens tp' rng)
      TpAnn tp userKind
        -> do kind <- userKindToKind userKind
              unify context range expected kind
              tp' <- infUserType kind (checkAnnot range) tp
              return (TpAnn tp' kind)

getEffectLift :: KUserType InfKind -> KInfer (KUserType k -> Range -> KUserType k)
getEffectLift utp
  = case utp of
      TpQuan quant tname tp rng
                      -> getEffectLift tp
      TpParens tp _   -> getEffectLift tp
      TpApp tp _ _    -> getEffectLift tp
      TpCon name rng  -> do dataEff <- lookupDataEffect name
                            case dataEff of
                              DataEffect named linear
                                -> -- trace ("getEffectLift: wrap with handled: " ++ show utp) $
                                   return (\u r -> TpApp (TpCon (makeTpHandled named linear) rangeNull) [u] rangeNull)
                              DataNoEffect
                                -> -- trace ("getEffectLift: no effect: " ++ show utp) $
                                   return (\tp _ -> tp)
      _               -> trace ("getEffectLift: unexpected type " ++ show utp) $
                         return (\tp _ -> tp)

infParam expected context (name,tp)
  = do tp' <- infUserType expected context tp
       return (name,tp')

-- Add indirection constructor to a lazy datatype
addLazyIndirect (DataType newtp targs constructors range vis sort ddef dataEff doc)
  | any userConIsLazy constructors || dataDefIsLazy ddef
  = do let (lazyCons,strictCons) = partition userConIsLazy constructors
           rng                 = rangeHide $ tbinderNameRange newtp
           makeTpApp t targs   = if null targs then t else TpApp t targs rng
           indirectName        = newHiddenName "indirect"
           indirectPar         = ValueBinder (indirectName)
                                             (makeTpApp (TpCon (tbinderName newtp) rng)
                                                        [TpVar (tbinderName targ) (tbinderNameRange targ) | targ <- targs ])
                                             Nothing rng rng
           indirectCon         = UserCon (toLazyIndirectConName (tbinderName newtp)) []
                                          [(Private,indirectPar)]
                                          Nothing
                                          (Just (fipNoAlloc, Var indirectName False rng))
                                          rng rng Public
                                          "// automatically generated lazy indirection constructor"

           -- order with lazy last (so we can check quickly at runtime)
           newConstructors     = strictCons ++ [indirectCon] ++ lazyCons
           validDdef           = case ddef of
                                   DataDefAuto{} -> True
                                   DataDefNormal -> True
                                   DataDefLazy _ -> True
                                   _             -> False

       when (not validDdef) $
         addError rng $ text "Cannot add lazy constructors to a" <+> text (show ddef) <+> text "type"

       -- get fip annotation of the data defintion
       let userConFip con = case userConLazy con of
                              Just (fip,_) -> fip
                              Nothing      -> noFip
           defaultFip = foldr fipMax fipBot (map userConFip lazyCons)
       fip <- case ddef of
                DataDefLazy (NoFip False) -> return defaultFip
                DataDefLazy fip
                  -> -- trace ("fip check: " ++ show (fip,defaultFip) ++ ", gt? " ++ show (fip > defaultFip)) $
                     if (not (fip `fipSubsumes` defaultFip))  -- annotated
                       then do addError rng $ text "The datatype" <+> text (show fip) <+> text "annotation cannot be more restrictive than the fip annotations of the lazy constructors"
                               return defaultFip
                       else return fip
                _ -> return defaultFip


       return (DataType newtp targs newConstructors range vis sort (DataDefLazy fip) dataEff doc)

addLazyIndirect td
  = return td

userConIsLazy :: UserCon t u k -> Bool
userConIsLazy userCon
  = case userConLazy userCon of
      Just _  -> True
      Nothing -> isLazyIndirectConName (userconName userCon)


checkQuant range  = Check "Can only quantify over types" range
checkPred range   = Check "The left-hand side of a \"=>\" can only contain predicates" range
checkArg range    = Check "The parameters of a function type must be types" range
checkEff range    = Check "The effect of a function type must be an effect type" range
checkRes range    = Check "The result of a function type must be a type" range
checkAnnot range  = Check "The inferred kind doesn't match the annotated kind" range
checkApp range    = Check "The type cannot be applied to those arguments" range
checkExtendTail r = Check "The extension of an effect must be an effect type" r
checkExtendLabel r= Check "The elements of an effect must be effect constants" r

{---------------------------------------------------------------
  Resolve kinds: from InfKind to Kind, and UserType to Type
---------------------------------------------------------------}

resolveTypeDef :: Bool -> [Name] -> TypeDef (KUserType InfKind) UserType InfKind -> KInfer (Core.TypeDef,[LazyExpr])
resolveTypeDef isRec recNames (Synonym syn params tp range vis doc)
  = do syn' <- resolveTypeBinderDef doc syn
       params' <- mapM (resolveTypeBinder "") params
       typeVars  <- mapM (\param -> freshTypeVar param Bound) params'
       let tvarMap = M.fromList (zip (map getName params') typeVars)
       tp' <- resolveType tvarMap True tp
       -- eta-expand type synonyms
       let kind = typeBinderKind syn'
           arity = kindArity kind
           etaKinds = drop (length typeVars) arity

       (etaTp,etaParams)
          <- if (null etaKinds)
              then return (tp',typeVars)
              else do etaVars <- mapM (\kind -> do id <- uniqueId "eta"; return (TypeVar id kind Bound)) etaKinds
                      return (typeApp tp' (map TVar etaVars), typeVars ++ etaVars)

       -- trace (showTypeBinder syn') $
       addRangeInfo range (Decl "alias" (getName syn') (mangleTypeName (getName syn')) (Just tp'))
       let synInfo = SynInfo (getName syn') (typeBinderKind syn') etaParams etaTp (maxSynonymRank etaTp + 1) range vis doc
       addSynonym synInfo
       return (Core.Synonym synInfo,[])
  where
    kindArity (KApp (KApp kcon k1) k2)  | kcon == kindArrow = k1 : kindArity k2
    kindArity _ = []

resolveTypeDef isRec recNames (DataType newtp params constructors range vis sort ddef dataEff doc)
  = do -- trace ("datatype: " ++ show(tbinderName newtp) ++ " " ++ show isExtend ++ ", doc: " ++ doc) $ return ()
       newtp' <- if dataDefIsExtend ddef
                  then do (qname,ikind,_) <- findInfKind (tbinderName newtp) (tbinderRange newtp)
                          kind  <- resolveKind ikind
                          -- addRangeInfo range (Id qname (NITypeCon kind doc) [] False)
                          return (TypeBinder qname kind (tbinderNameRange newtp) (tbinderRange newtp))
                  else resolveTypeBinderDef doc newtp
       params' <- mapM (resolveTypeBinder "") params
       let typeResult = TCon (TypeCon (getName newtp') (typeBinderKind newtp'))
       typeVars  <- let (kargs,kres) = extractKindFun (typeBinderKind newtp')
                    in if (null params' && not (null kargs))
                     then mapM (\karg -> do{ id <- uniqueId "k"; return (TypeVar id karg Bound) }) kargs  -- invent parameters if they are not given (and it has an arrow kind)
                     else mapM (\param -> freshTypeVar param Bound) params'
       let tvarMap = M.fromList (zip (map getName params') typeVars)

       cs <- getColorScheme
       let qname  = getName newtp'
           fname  = unqualify qname
           name   = fname -- if (isHandlerName fname) then fromHandlerName fname else fname
           nameDoc = color (colorType cs) (pretty name)

       consinfos <- mapM (resolveConstructor (getName newtp') sort
                            (not (dataDefIsOpen ddef) && length constructors == 1)
                            typeResult typeVars tvarMap) (zip constructors [1..])  -- constructor tags start at 1
       let (lazyExprss,conInfos0) = unzip consinfos

       --check recursion
       if (sort == Retractive)
        then return ()
        else let effNames = concatMap fromOpsName recNames
                 fromOpsName nm = if (isOperationsName nm) then [fromOperationsName nm] else []
             in if (any (occursNegativeCon (recNames ++ effNames)) (conInfos0))
              then do addError range (text "Type" <+> nameDoc <+> text "is declared as being" <-> text " (co)inductive but it occurs recursively in a negative position." <->
                                     text " hint: declare it as a divergent (or retractive) type using 'div type' (or 'div effect') to allow negative occurrences")
              else return ()

       -- create datadef and conInfos with correct ValueRepr and ordered fields
       let emitError d    = addError range (text "Type" <+> nameDoc <+> d)
           emitWarning d  = addWarning range (text "Type" <+> nameDoc <+> d)
           resultHasKindStar = hasKindStarResult (getKind typeResult)
           maxMembers     = maximum ([0] ++ map (length . conInfoParams) conInfos0)
           conCount       = length conInfos0
           willNeedStructTag   = dataDefIsValue ddef && conCount > 1 && maxMembers >= 1
           extraFields = if (dataDefIsOpen ddef) then 1 {- open datatype tag -}
                         else if willNeedStructTag then 1 {- explicit struct tag -}
                         else 0
       platform <- getPlatform
       (ddef1,conInfos1)
          <- createDataDef emitError emitWarning lookupDataInfo
                platform qname resultHasKindStar isRec sort extraFields ddef conInfos0

       let docx = doc -- don't extend the doc with constructors as it is shown when hovering with `ctrl`
                  {- memberDoc doc "constructors"
                      [ (maybe "" (\fip -> "lazy ") (conInfoLazy con)) ++
                        "con " ++ show (unqualify (conInfoName con)) ++
                        let params = filter (not . isHiddenName . fst) (conInfoParams con)
                            tps = niceTypes defaultEnv{alwaysUnqualify=True} (map snd params)
                            names = map fst params
                        in if null params then "" else
                            "(" ++ concat (intersperse ", " [show name ++ " : " ++ show tp  | (name,tp) <- zip names tps]) ++ ")"
                      | con <- conInfos1, not (isHiddenName (conInfoName con))] -}
           dataInfo = DataInfo sort (getName newtp') (typeBinderKind newtp') typeVars conInfos1 range ddef1 dataEff isRec vis docx

       assertion ("Kind.Infer.resolveTypeDef: assuming value struct tag but not inferred as such " ++ show (ddef,ddef1))
                 ((willNeedStructTag && Core.needsTagField (fst (Core.getDataRepr dataInfo))) || not willNeedStructTag) $ return ()


       {-
       -- adjust datainfo in case an extra value tag was needed
       dataInfo  <- case ddef1 of
                      DataDefValue (ValueRepr m n a)  | Core.needsTagField (fst (Core.getDataRepr dataInfo0))
                        ->  -- recalculate with extra required tag field to the size
                            do (ddef2,conInfos2) <- createDataDef emitError emitWarning lookupDataInfo
                                                      platform qname resultHasKindStar isRec sort
                                                        1 {- extra field for tag -} ddef1 {- guarantees value type again -} conInfos1
                               let dataInfo1 = dataInfo0{ dataInfoDef = ddef2, dataInfoConstrs = conInfos2 }
                               return dataInfo1
                      _ -> return dataInfo0
       -}
       -- trace (showTypeBinder newtp') $
       let declaration = (if dataInfoIsLazy dataInfo then "lazy " else "") ++
                         (if sort /= Inductive then "" else (if dataDefIsValue ddef1 then "value " else "reference "))
                          ++ show sort
       addRangeInfo range (Decl declaration (getName newtp') (mangleTypeName (getName newtp')) Nothing)
       return (Core.Data dataInfo, concat lazyExprss)
  where
    conVis (UserCon name exist params result mblazy rngName rng vis _) = vis


occursNegativeCon :: [Name] -> ConInfo -> Bool
occursNegativeCon names conInfo
  = let (_,rho) = splitTypeScheme (conInfoType conInfo)
    in case splitFunType rho of
         Just (pars,eff,res) -> any (occursNegative names) (map snd pars) || occursNegative names res
         Nothing -> False

occursNegative :: [Name] -> Type -> Bool
occursNegative names tp
  = occurs names False tp

occurs :: [Name] -> Bool -> Type -> Bool
occurs names isNeg tp
  = case tp of
      TForall vars tp        -> occurs names isNeg tp
      TFun args effect result -> any (occurs names (not isNeg)) (map snd args) || occurs names (not isNeg) effect
                                       || occurs names isNeg result
      TCon tcon               -> -- trace ("con name: " ++ show (typeConName tcon)) $
                                 if (typeConName tcon `elem` names) then isNeg
                                  -- else if (toOperationsName (typeConName tcon) `elem` names) then isNeg
                                    else False
      TVar tvar               -> False
      TApp tp args            -> any (occurs names isNeg) (tp:args)
      TSyn syn xs tp          -> occurs names isNeg tp


showTypeBinder :: TypeBinder Kind -> String
showTypeBinder (TypeBinder name kind _ _)
  = show name ++ ": " ++ show kind


resolveTypeBinderDef :: String -> TypeBinder InfKind -> KInfer (TypeBinder Kind)
resolveTypeBinderDef doc (TypeBinder name infkind rngName rng)
  = do infkind' <- resolveKind infkind
       qname    <- qualifyDef name
       addRangeInfo rngName (Id qname (NITypeCon infkind' doc) [] True)
       return (TypeBinder qname infkind' rngName rng)

resolveTypeBinder :: String -> TypeBinder InfKind -> KInfer (TypeBinder Kind)
resolveTypeBinder doc (TypeBinder name infkind rngName rng)
  = do infkind' <- resolveKind infkind
       addRangeInfo rngName (Id name (NITypeCon infkind' doc) [] True)
       return (TypeBinder name infkind' rngName rng)

resolveKind :: InfKind -> KInfer Kind
resolveKind infkind
  = do skind <- subst infkind
       return (resolve skind)
  where
    resolve (KIVar id)   = kindStar  -- default unconstrained parameter to kind star
    resolve (KICon kind) = kind
    resolve (KIApp k1 k2) = KApp (resolve k1) (resolve k2)

resolveConstructor :: Name -> DataKind -> Bool -> Type -> [TypeVar] -> M.NameMap TypeVar -> (UserCon (KUserType InfKind) UserType InfKind,Int) -> KInfer ([(Name,Expr Type)] {-UserCon Type Type Kind-}, ConInfo)
resolveConstructor typeName typeSort isSingleton typeResult typeParams idmap (UserCon name exist params mbResult mblazy rngName rng vis doc,tag)
  = do qname  <- qualifyDef name
       exist' <- mapM (resolveTypeBinder doc) exist
       existVars <- mapM (\ename -> freshTypeVar ename Bound) exist'
       let idmap' = M.union (M.fromList (zip (map getName exist) existVars)) idmap  -- ASSUME: left-biased union
       params' <- mapM (resolveConParam idmap') params -- mapM (resolveType idmap' False) params
       result' <- case mbResult of
                    Nothing -> return $ typeApp typeResult (map TVar typeParams)
                    Just tp -> resolveType idmap' False tp
       mblazy' <- case mblazy of
                    Nothing -> return Nothing
                    Just (fip,e)  -> do e' <- infExpr e
                                        return (Just (fip,e'))
       let lazyExpr  = case mblazy' of
                         Nothing     -> []
                         Just (_,e)  -> [(qname,e)]
           mbLazyFip = case mblazy' of
                         Nothing      -> Nothing
                         Just (fip,_) -> Just fip

       let scheme = quantifyType (typeParams ++ existVars) $
                    if (null params') then result' else typeFun [(binderName p, binderType p) | (_,p) <- params'] typeTotal result'
       addRangeInfo rng (Decl "con" qname (mangleConName qname) (Just scheme))
       addRangeInfo rngName (Id qname (NICon scheme doc) [] True)
       let fields = map (\(i,b) -> (if (nameIsNil (binderName b)) then newFieldName i else binderName b, binderType b)) (zip [1..] (map snd params'))
       --    emitError makeMsg = do cs <- getColorScheme
       --                           let nameDoc = color (colorCons cs) (pretty name)
       --                           addError rng (makeMsg nameDoc)
       -- platform <- getPlatform
       -- (orderedFields,vrepr) <- orderConFields emitError lookupDataInfo platform (if isOpen then 1 else 0) fields
       return (lazyExpr -- UserCon qname exist' params' (Just result') mblazy' rngName rng vis doc
              ,ConInfo qname typeName typeParams existVars
                  fields
                  scheme
                  typeSort rngName
                  (map (binderNameRange . snd) params')
                  (map fst params')
                  isSingleton
                  -- orderedFields vrepr
                  [] valueReprZero   -- initialized later at the datadef
                  mbLazyFip tag vis doc)


---------------------------------------------------------
--
---------------------------------------------------------

resolveConParam :: M.NameMap TypeVar -> (Visibility,ValueBinder (KUserType InfKind) (Maybe (Expr UserType))) -> KInfer (Visibility,ValueBinder Type (Maybe (Expr Type)))
resolveConParam idmap (vis,vb)
  = do tp <- resolveType idmap False (binderType vb)
       expr <- case (binderExpr vb) of
                 Nothing -> return Nothing
                 Just e  -> {- do e' <- infExpr e
                                  return (Just e') -}
                            return (Just (failure "Kind.Infer.resolveConParam: optional parameter expression in constructor"))
       -- addRangeInfo (binderNameRange vb) (Id (binderName vb) (NIValue "val" tp "" True) [] True)
       return (vis,vb{ binderType = tp, binderExpr = expr })

-- | @resolveType@ takes: a map from locally quantified type name variables to types,
-- a boolean that is 'True' if partially applied type synonyms are allowed (i.e. when
-- these are arguments to type synonyms themselves), a user type with inference kinds,
-- and it returns a fully resolved type.
resolveType :: M.NameMap TypeVar -> Bool -> KUserType InfKind -> KInfer Type
resolveType idmap partialSyn userType
  = case userType of
      TpQuan QForall tname tp rng
        -> do tname' <- resolveTypeBinder "" tname
              tvar   <- freshTypeVar tname' Bound
              tp'    <- resolveType (M.insert (getName tname) tvar idmap) False tp
              return (quantifyType [tvar] tp')
      TpQuan QSome tname tp rng
        -> do tname' <- resolveTypeBinder "" tname
              tvar   <- freshTypeVar tname' Meta
              tp'    <- resolveType (M.insert (getName tname) tvar idmap) False tp
              -- trace ("Kind.Infer.Some") $
              return tp'
      TpQuan QExists tname tp rng
        -> todo "Kind.Infer.resolveType: existentials are not supported yet"
      TpFun args effect tp rng
        -> do args' <- mapM resolveParam args
              effect' <- resolveType idmap False effect
              tp'     <- resolveType idmap False tp
              return (TFun args' effect' tp')
      TpApp tp args rng
        -> resolveApp idmap partialSyn (collectArgs tp args) rng
      TpVar name rng
        -> resolveApp idmap partialSyn (userType,[]) rng
      TpCon name rng
        -> resolveApp idmap partialSyn (userType,[]) rng
      TpParens tp rng
        -> resolveType idmap partialSyn tp
      TpAnn tp userKind
        -> resolveType idmap partialSyn tp

  where
    resolveParam (name,tp)
      = do tp' <- resolveType idmap False tp
           return (name,tp')

    collectArgs tp args
      = case tp of
          TpApp tp' args' _ -> collectArgs tp' (args' ++ args)
          TpParens tp' _    -> collectArgs tp' args
          TpAnn tp' _       -> collectArgs tp' args
          _                 -> (tp, args)


resolveApp idmap partialSyn (TpVar name r,args) rng
  = do (tp',kind) <- case M.lookup name idmap of
                      Nothing   -> do cs <- getColorScheme
                                      -- failure ("Kind.Infer.ResolveApp: cannot find: " ++ show name ++ " at " ++ show rng)
                                      addError rng (text "Type variable" <+> color (colorType cs) (pretty name) <+> text "is undefined" <->
                                                    text " hint: bind the variable using" <+> color (colorType cs) (text "forall<" <.> pretty name <.> text ">"))
                                      id <- uniqueId (show name)
                                      return (TVar (TypeVar id kindStar Bound), kindStar)

                      Just tvar -> return (TVar tvar, typevarKind tvar)
       if (not (isImplicitTypeVarName name))
        then addRangeInfo r (Id name (NITypeVar kind) [] False)
        else return ()
       args' <- mapM (resolveType idmap False) args
       return (typeApp tp' args')

resolveApp idmap partialSyn (TpCon name r,[fixed,ext]) rng  | name == nameEffectAppend
  = do fixed' <- resolveType idmap False fixed
       ext'   <- resolveType idmap False ext
       let (ls,tl) = extractOrderedEffect fixed'
       if isEffectEmpty tl
        then return ()
        else addError rng (text "Effects can only have one extension point (use a `|` instead of a comma in the effect type ?)")
       return (shallowEffectExtend fixed' ext')

resolveApp idmap partialSyn (TpCon name r,args) rng
  =  do (qname,ikind,doc) <- findInfKind name rng
        kind  <- resolveKind ikind
        addRangeInfo r (Id qname (NITypeCon kind doc) [] False)

        mbSyn <- lookupSynInfo name
        case mbSyn of
          Just syn@(SynInfo name kind params tp rank range vis doc)
            -> do -- check over/under application
                  if (not partialSyn && length args < length params)
                   then do cs <- getColorScheme
                           addError rng (text "Type alias" <+> color (colorType cs) (pretty name) <+> text "has too few arguments")
                   else if (length args > length params)
                    then do cs <- getColorScheme
                            addError rng (text "Type alias" <+> color (colorType cs) (pretty name) <+> text "has too many arguments")
                    else return ()
                  args' <- mapM (resolveType idmap True) args    -- partially applied synonyms are allowed in synonym applications
                  let tsyn = (TSyn (TypeSyn name kind rank (Just syn)) args' (subNew (zip params args') |-> tp))
                  -- trace ("resolved type syn: " ++ show (pretty syn)) $
                  return tsyn
                  -- NOTE: on partially applied type synonyms, we get a funky body type with free parameters but this
                  -- is only inside synonyms arguments so we are ok.
          Nothing
            -> do args' <- mapM (resolveType idmap False) args
                  return (typeApp (TCon (TypeCon name kind)) args')

resolveApp idmap partialSyn _ rng
  = failure "Kind.Infer.resolveApp: this case should never occur after kind checking"


makeEffectAppend fixed ext
  = TpApp (TpCon nameEffectAppend rangeNull) [fixed,ext] rangeNull

makeEffectExtend (label) ext
  = TpApp (TpCon nameEffectExtend rangeNull) [label,ext] rangeNull

makeEffectEmpty
  = TpCon nameEffectEmpty rangeNull


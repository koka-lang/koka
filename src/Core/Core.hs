-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-    System F-like core language.
-}
-----------------------------------------------------------------------------

module Core.Core ( -- Data structures
                     Core(..)
                   , Imports, Import(..)
                   , Externals, External(..), externalVis
                   , FixDefs, FixDef(..)
                   , TypeDefGroups, TypeDefGroup(..), TypeDefs, TypeDef(..)
                   , DefGroups, DefGroup(..), Defs, Def(..), InlineDef(..)
                   , Expr(..), Lit(..)
                   , Branch(..), Guard(..), Pattern(..)
                   , TName(..), typeDefName
                   , showTName
                   , flattenTypeDefGroups
                   , flattenDefGroups
                   , flattenAllDefGroups
                   , mapDefGroup
                   , mapMDefGroup
                   , extractSignatures
                   , typeDefIsExtension
                   , typeDefVis
                   , externalImportLookup, eimportLookup

                     -- Core term builders
                   , defIsVal
                   , defTName , defGroupTNames , defGroupsTNames
                   , addTypeLambdas, addTypeApps, addLambdas, addLambdasTName, addApps
                   , makeLet, makeTypeApp
                   , addNonRec, addCoreDef, coreNull
                   , freshName
                   , typeOf
                   , isExprUnit, exprUnit
                   , isExprTrue,  exprTrue, patTrue
                   , isExprFalse, exprFalse, patFalse
                   , isValueExpr
                   , openEffectExpr
                   , makeIfExpr
                   , makeInt32, makeSizeT
                   , makeEvIndex
                   , makeList, makeVector
                   , makeDef, makeTDef, makeStats
                   , unzipM
                   , Visibility(..), Fixity(..), Assoc(..), isPublic
                   , coreName
                   , tnamesList, tnamesEmpty, tnamesDiff, tnamesInsertAll
                   , tnamesUnion, tnamesUnions, tnamesRemove, tnamesFromList
                   , tnamesMember
                   -- , getTypeArityExpr -- ,getParamArityExpr
                   , getEffExpr
                   , TNames
                   , splitFun
                   , splitTForall
                   , isTotal
                   -- * Data representation
                   , DataRepr(..), ConRepr(..)
                   , isConSingleton
                   , isConNormal
                   , isConIso
                   , isDataStruct
                   , getDataRepr, getDataReprEx, dataInfoIsValue
                   , getConRepr
                   , dataReprIsValue, conReprIsValue
                   , VarInfo(..), isInfoArity
                   , infoIsRefCounted, infoIsLocal

                   , isMonType, isMonEffect

                   -- Inlining
                   , costDef, costExpr, costInf
                   , isInlineable
                   , inlineDefIsSpecialize

                   -- * Canonical names
                   -- , canonicalName, nonCanonicalName, canonicalSplit
                   , infoArity, infoTypeArity

                   , Deps, dependencies

                   , foldMapExpr
                   , anySubExpr
                   , foldExpr
                   , rewriteBottomUp
                   , rewriteBottomUpM

                   , CorePhase
                   , getCoreDefs, setCoreDefs, withCoreDefs
                   , runCorePhase
                   , liftCorePhase, liftCorePhaseUniq
                   , liftError
                   ) where

import Control.Applicative (liftA2)
import Control.Monad (forM)
import Control.Monad.Identity

import Data.Char( isDigit )
import qualified Data.Set as S
import Data.Maybe
import Data.Monoid (Endo(..), Any(..))
import Lib.PPrint
import Common.Name
import Common.Range
import Common.Failure
import Common.Unique
import Common.Id
import Common.Error
import Common.NamePrim( nameTrue, nameFalse, nameTuple, nameTpBool, nameEffectOpen, nameReturn, nameTrace, nameLog,
                        nameEvvIndex, nameOpenAt, nameOpenNone, nameInt32, nameSSizeT, nameBox, nameUnbox,
                        nameVector, nameCons, nameNull, nameTpList, nameUnit, nameTpUnit, nameTpCField)
import Common.Syntax
import Kind.Kind
import Type.Type
import Type.Pretty ()
import Type.TypeVar
import Type.Kind    ( getKind, getHandledEffect, HandledSort(ResumeMany), isHandledEffect, extractHandledEffect )

import Lib.Trace

isExprUnit (Con tname _)  = getName tname == nameTuple 0
isExprUnit _              = False

isExprTrue (Con tname _)  = (getName tname == nameTrue)
isExprTrue _              = False

isExprFalse (Con tname _)  = (getName tname == nameFalse)
isExprFalse _              = False

exprUnit :: Expr
exprUnit = Con (TName nameUnit typeUnit) (ConEnum nameTpUnit DataEnum 0)
           -- (ConInfo nameUnit typeUnit [] [] [] (TFun [] typeTotal typeUnit) Inductive rangeNull [] [] False Public "")

(patFalse,exprFalse) = patExprBool nameFalse 0
(patTrue,exprTrue)   = patExprBool nameTrue 1

patExprBool name tag
  = let tname   = TName name typeBool
        conEnum = ConEnum nameTpBool DataEnum tag
        conInfo = ConInfo name nameTpBool [] [] [] (TFun [] typeTotal typeBool) Inductive rangeNull [] [] False Public ""
        pat = PatCon tname [] conEnum [] [] typeBool conInfo False
        expr = Con tname conEnum
    in (pat,expr)

makeIfExpr :: Expr -> Expr -> Expr -> Expr
makeIfExpr pexpr texpr eexpr
  = Case [pexpr] [Branch [patTrue] [Guard exprTrue texpr],
                  Branch [PatWild] [Guard exprTrue eexpr]]

makeVector :: Type -> [Expr] -> Expr
makeVector tp exprs
  = App (TypeApp vectorFromList [tp]) [makeList tp exprs]
  where
    vectorFromList
      = Var (TName nameVector (TForall [a] [] (typeFun [(nameNil,TApp typeList [TVar a])] typeTotal (TApp typeVector [TVar a]))))
            (InfoArity 1 1)
    a = TypeVar (0) kindStar Bound

makeList :: Type -> [Expr] -> Expr
makeList tp exprs
  = foldr cons nil exprs
  where
    nilTp    = TForall [a] [] (TApp typeList [TVar a])
    nilCon   = Con (TName nameNull nilTp) (ConSingleton nameTpList DataAsList 0)
    nil      = TypeApp nilCon [tp]
    consTp   = TForall [a] [] (typeFun [(nameNil,TVar a),(nameNil,TApp typeList [TVar a])] typeTotal (TApp typeList [TVar a]))
    consCon  = Con (TName nameCons consTp) (ConAsCons nameTpList DataAsList nameNull 1)
    cons expr xs = App (TypeApp consCon [tp]) [expr,xs]
    a = TypeVar (0) kindStar Bound

makeDef :: Name -> Expr -> Def
makeDef name expr
  = Def name (typeOf expr) expr Private DefVal InlineNever rangeNull ""

makeTDef :: TName -> Expr -> Def
makeTDef (TName name tp) expr
  = Def name tp expr Private DefVal InlineNever rangeNull ""


makeStats :: [Expr] -> Expr
makeStats []
  = failure "Core.Parc.makeStats: no expressions"
makeStats [expr]
  = expr
makeStats exprs
  = Let [DefNonRec (makeDef nameNil expr) | expr <- init exprs]
        (last exprs)

unzipM :: Monad m => m [(a,b)] -> m ([a],[b])
unzipM m = fmap unzip m

{--------------------------------------------------------------------------
  Top-level structure
--------------------------------------------------------------------------}

data Core = Core{ coreProgName :: Name
                , coreProgImports :: Imports
                , coreProgFixDefs :: FixDefs
                , coreProgTypeDefs :: TypeDefGroups
                , coreProgDefs :: DefGroups
                , coreProgExternals :: Externals
                , coreProgDoc :: String
                }


type FixDefs
  = [FixDef]

data FixDef
  = FixDef Name Fixity


coreName :: Core -> Name
coreName (Core name _ _ _ _ _ _) = name

{---------------------------------------------------------------
  Imports
---------------------------------------------------------------}

-- | Core imports
type Imports = [Import]

data Import  = Import{ importName :: Name
                     , importPackage :: String
                     , importVis  :: Visibility
                     , importModDoc :: String
                     }

{--------------------------------------------------------------------------
  Externals
--------------------------------------------------------------------------}

type Externals = [External]

data External = External{ externalName :: Name
                        , externalType :: Scheme
                        , externalFormat :: [(Target,String)]
                        , externalVis' :: Visibility
                        , externalRange :: Range
                        , externalDoc :: String
                        }
              | ExternalImport { externalImport :: [(Target,[(String,String)])]
                               , externalRange :: Range }

externalVis :: External -> Visibility
externalVis (External{ externalVis' = vis }) = vis
externalVis _ = Private

externalImportLookup :: Target -> BuildType -> String -> External -> Maybe String
externalImportLookup target buildType key (ExternalImport imports range)
  = let keyvals = case lookup target imports of
                    Just keyvals -> keyvals
                    Nothing -> case lookup Default imports of
                                 Just keyvals -> keyvals
                                 Nothing -> []
    in eimportLookup buildType key keyvals

externalImportLookup target buildType key ext
  = Nothing

eimportLookup :: BuildType -> String -> [(String,String)] -> Maybe String
eimportLookup buildType key keyvals
  = case lookup (key ++ "-" ++ show buildType) keyvals of
      Just val -> Just val
      Nothing  -> lookup key keyvals


{--------------------------------------------------------------------------
  Type definitions
--------------------------------------------------------------------------}

type TypeDefGroups = [TypeDefGroup]

data TypeDefGroup = TypeDefGroup TypeDefs

type TypeDefs = [TypeDef]

-- | A type definition
data TypeDef =
    Synonym{ typeDefSynInfo :: SynInfo }             -- ^ name, synonym info, and the visibility
  | Data{ typeDefDataInfo :: DataInfo, typeDefIsExtend :: Bool }  -- ^ name, info, visibility, and the visibilities of the constructors, the isExtend is true if this is an extension of the datatype.

typeDefName (Synonym info) = synInfoName info
typeDefName (Data info _)  = dataInfoName info

typeDefIsExtension (Data _  True) = True
typeDefIsExtension _              = False

typeDefVis (Synonym info) = synInfoVis info
typeDefVis (Data info _)  = dataInfoVis info

flattenTypeDefGroups :: TypeDefGroups -> [TypeDef]
flattenTypeDefGroups tdgs = concatMap (\(TypeDefGroup tdg) -> tdg) tdgs

{--------------------------------------------------------------------------
  Data representation
--------------------------------------------------------------------------}
data DataRepr = -- value types
                DataEnum            -- only singletons (as an enumeration)
              | DataIso             -- only one constructor with one field  (isomorpic)
              | DataSingleStruct    -- only one constructor (no tag needed)
              | DataAsMaybe         -- one constructor with fields, and one singleton
              | DataStruct          -- compatible constructors (all raw or regular types) and possibly singletons (need tag)
              -- non-value types
              | DataSingle{ hasSingletons :: Bool } -- only one constructor (no tag needed), hasSingletons true if it is a singleton as well
              | DataAsList          -- one constructor with fields, and one singleton (don't need a tag, for example can distinguish pointer vs enum)
              | DataSingleNormal    -- one constructor with fields, and multiple singletons (distinguish one pointer vs enums)
              | DataNormal{ hasSingletons :: Bool }
              | DataOpen
              deriving (Eq,Ord,Show)

data ConRepr  = ConEnum{ conTypeName :: Name, conDataRepr :: DataRepr, conTag :: Int }                     -- part of enumeration (none has fields)
              | ConIso{ conTypeName:: Name, conDataRepr :: DataRepr, conTag :: Int }                       -- one constructor with one field
              | ConSingleton{ conTypeName :: Name, conDataRepr :: DataRepr, conTag :: Int }                -- constructor without fields (and not part of an enum)
              | ConSingle{ conTypeName :: Name, conDataRepr :: DataRepr, conTag :: Int }                   -- there is only one constructor and it is not iso or singleton (and this is it)
              | ConAsJust{ conTypeName :: Name, conDataRepr :: DataRepr, conAsNothing :: Name, conTag :: Int } -- constructor is the cons node of a maybe-like value datatype  (may have one or more fields)
              | ConStruct{ conTypeName :: Name, conDataRepr :: DataRepr, conTag :: Int }                   -- constructor as value type
              | ConAsCons{ conTypeName :: Name, conDataRepr :: DataRepr, conAsNil :: Name, conTag :: Int } -- constructor is the cons node of a list-like datatype  (may have one or more fields)
              | ConOpen  { conTypeName :: Name, conDataRepr :: DataRepr }                                  -- constructor of open data type
              | ConNormal{ conTypeName :: Name, conDataRepr :: DataRepr, conTag :: Int }                   -- a regular constructor
              deriving (Eq,Ord,Show)

isConSingleton (ConSingleton _ _ _) = True
isConSingleton _ = False

isConNormal (ConNormal _ _ _) = True
isConNormal _  = False

isConIso (ConIso{}) = True
isConIso _ = False

isDataStruct (DataStruct) = True
isDataStruct _ = False

-- Value data is not heap allocated and needs no header
dataReprIsValue :: DataRepr -> Bool
dataReprIsValue DataEnum         = True
dataReprIsValue DataIso          = True
dataReprIsValue DataSingleStruct = True
dataReprIsValue DataAsMaybe      = True
dataReprIsValue DataStruct       = True   -- structs have a tag field though
dataReprIsValue _                = False

conReprIsValue :: ConRepr -> Bool
conReprIsValue crepr = dataReprIsValue (conDataRepr crepr)

dataInfoIsValue :: DataInfo -> Bool
dataInfoIsValue info = dataDefIsValue (dataInfoDef info)

getDataRepr :: DataInfo -> (DataRepr,[ConRepr])
getDataRepr info
  = getDataReprEx dataInfoIsValue info

getConRepr :: DataInfo -> ConInfo -> ConRepr
getConRepr dataInfo conInfo
  = let (_,creprs) = getDataRepr dataInfo
    in case [crepr | (ci,crepr) <- zip (dataInfoConstrs dataInfo) creprs, conInfoName ci == conInfoName conInfo] of
         [crepr] -> crepr
         _ -> failure ("Core.Core: getConRepr: constructor not in the datatype: " ++ show (dataInfoName dataInfo, conInfoName conInfo))

getDataReprEx :: (DataInfo -> Bool) -> DataInfo -> (DataRepr,[ConRepr])
getDataReprEx getIsValue info
  = let typeName  = dataInfoName info
        conInfos = dataInfoConstrs info
        conTags  = [0..length conInfos - 1]
        singletons =  filter (\con -> null (conInfoParams con)) conInfos
        hasExistentials = any (\con -> not (null (conInfoExists con))) conInfos
        isValue = getIsValue info && not (dataInfoIsRec info)
        (dataRepr,conReprFuns) =
         if (dataInfoIsOpen(info))
          then (DataOpen, map (\conInfo conTag -> ConOpen typeName DataOpen) conInfos)
         -- TODO: only for C#? check this during kind inference?
         -- else if (hasExistentials)
         --  then (DataNormal, map (\con -> ConNormal typeName) conInfos)
         else if (isValue
                    && (null (dataInfoParams info) || typeName == nameTpCField)
                    && all (\con -> null (conInfoParams con)) conInfos)
          then (DataEnum,map (const (ConEnum typeName DataEnum)) conInfos)
         else if (length conInfos == 1)
          then let conInfo = head conInfos
                   dataRepr = if (isValue && length (conInfoParams conInfo) == 1)
                                then DataIso
                               else if (isValue && null singletons && not (dataInfoIsRec info))
                                then DataSingleStruct
                                else DataSingle (not (null singletons))
               in (dataRepr
                  ,[if (isValue && length (conInfoParams conInfo) == 1) then ConIso typeName dataRepr
                    else if length singletons == 1 then ConSingleton typeName dataRepr
                    else ConSingle typeName dataRepr])
         else if (isValue && not (dataInfoIsRec info)) then (
           if (length conInfos == 2 && length singletons == 1)
             then (DataAsMaybe
                  ,map (\con -> if (null (conInfoParams con)) then ConSingleton typeName DataAsMaybe
                                 else ConAsJust typeName DataAsMaybe (conInfoName (head singletons))) conInfos)
             else (DataStruct, map (\con -> if null (conInfoParams con)
                                          then ConSingleton typeName DataStruct
                                          else ConStruct typeName DataStruct) conInfos )
         )
         else (
          if (length conInfos == 2 && length singletons == 1)
            then (DataAsList
                 ,map (\con -> if (null (conInfoParams con)) then ConSingleton typeName DataAsList
                                else ConAsCons typeName DataAsList (conInfoName (head singletons))) conInfos)
           else let dataRepr = if (length singletons == length conInfos -1 || null conInfos)
                                then DataSingleNormal else (DataNormal (not (null singletons)))
                in (dataRepr
                   ,map (\con -> if null (conInfoParams con)
                                  then ConSingleton typeName dataRepr
                                  else ConNormal typeName dataRepr) conInfos
                   )
         )
      in (dataRepr, [conReprFun tag | (conReprFun,tag) <- zip conReprFuns [1..]])


{--------------------------------------------------------------------------
  Definition groups
--------------------------------------------------------------------------}

type DefGroups = [DefGroup]

data DefGroup =
    DefRec Defs
  | DefNonRec Def

type Defs = [Def]

flattenDefGroups :: [DefGroup] -> [Def]
flattenDefGroups defGroups
  = concatMap (\defg -> case defg of { DefRec defs -> defs; DefNonRec def -> [def]}) defGroups

flattenAllDefGroups :: [DefGroups] -> [Def]
flattenAllDefGroups defGroups
  = concatMap flattenDefGroups defGroups

mapDefGroup :: (Def -> Def) -> DefGroup -> DefGroup
mapDefGroup f (DefNonRec def) = DefNonRec $ f def
mapDefGroup f (DefRec defs) = DefRec $ map f defs

mapMDefGroup :: (Monad m) => (Def -> m Def) -> DefGroup -> m DefGroup
mapMDefGroup f (DefNonRec def) = DefNonRec <$> f def
mapMDefGroup f (DefRec defs) = DefRec <$> mapM f defs

-- | A value definition
data Def = Def{ defName  :: Name
              , defType  :: Scheme
              , defExpr  :: Expr
              , defVis   :: Visibility     -- Private, Public
              , defSort  :: DefSort        -- DefFun, DefVal, DefVar
              , defInline:: DefInline      -- InlineAuto, InlineAlways, InlineNever
              , defNameRange :: Range
              , defDoc :: String
              }

data InlineDef = InlineDef{ inlineName :: Name, inlineExpr :: Expr, inlineRec :: Bool, inlineCost :: Int, specializeArgs :: [Bool], isTwoStepSpecialize :: Bool }

defIsVal :: Def -> Bool
defIsVal def
  = case defSort def of
      DefFun   -> False
      _        -> True


inlineDefIsSpecialize :: InlineDef -> Bool
inlineDefIsSpecialize inlDef = not (null (specializeArgs inlDef))

instance Show InlineDef where
  show (InlineDef name expr isRec cost specArgs isTwoStepSpecialize)
    = "InlineDef " ++ show name ++ " " ++ (if isRec then "rec " else "") ++ show cost ++ " " ++ show specArgs ++ " " ++ (if isTwoStepSpecialize then "twoStepSpecialize" else "")


newtype CorePhase a = CP (Int -> DefGroups -> Error (CPState a))

data CPState a = CPState !a !Int !DefGroups

instance Functor CorePhase where
  fmap f (CP cp)
    = CP (\uniq defs -> do (CPState x uniq' defs') <- cp uniq defs
                           return (CPState (f x) uniq' defs'))

instance Applicative CorePhase where
  pure  = return
  (<*>) = ap

instance Monad CorePhase where
  return x      = CP (\uniq defs -> return (CPState x uniq defs))
  (CP cp) >>= f = CP (\uniq defs -> do (CPState x uniq' defs') <- cp uniq defs
                                       case f x of
                                         CP cp' -> cp' uniq' defs')

instance HasUnique CorePhase where
  updateUnique f = CP (\uniq defs -> return (CPState uniq (f uniq) defs))
  setUnique uniq = CP (\_ defs -> return (CPState () uniq defs))
  unique         = CP (\uniq defs -> return (CPState uniq uniq defs))

getCoreDefs :: CorePhase DefGroups
getCoreDefs = CP (\uniq defs -> return (CPState defs uniq defs))

setCoreDefs :: DefGroups -> CorePhase ()
setCoreDefs defs = CP (\uniq _ -> return (CPState () uniq defs))

withCoreDefs :: (DefGroups -> a) -> CorePhase a
withCoreDefs f
  = do defs <- getCoreDefs
       return (f defs)

runCorePhase :: Int -> CorePhase a -> Error a
runCorePhase uniq (CP cp)
  = do (CPState x _ _) <- cp uniq []
       return x

liftCorePhaseUniq :: (Int -> DefGroups -> (DefGroups,Int)) -> CorePhase ()
liftCorePhaseUniq f
  = CP (\uniq defs -> let (defs',uniq') = f uniq defs in return (CPState () uniq' defs'))

liftCorePhase :: (DefGroups -> DefGroups) -> CorePhase ()
liftCorePhase f
  = liftCorePhaseUniq (\u defs -> (f defs, u))

liftError :: Error a -> CorePhase a
liftError err
  = CP (\uniq defs -> do x <- err
                         return (CPState x uniq defs))


{--------------------------------------------------------------------------
  Expressions

  Since this is System-F, all binding sites are annotated with their type.
--------------------------------------------------------------------------}

data Expr =
  -- Core lambda calculus
    Lam [TName] Effect Expr
  | Var{ varName :: TName, varInfo :: VarInfo }  -- ^ typed name and possible typeArity/parameter arity tuple for top-level functions
  | App Expr [Expr]                              -- ^ always fully applied!
  | TypeLam [TypeVar] Expr                       -- ^ Type (universal) abstraction/application
  | TypeApp Expr [Type]
  -- Literals, constants and labels
  | Con{ conName :: TName, conRepr ::  ConRepr  }          -- ^ typed name and its representation
  | Lit Lit
  -- Let
  | Let DefGroups Expr
  -- Case expressions
  | Case{ caseExprs :: [Expr], caseBranches :: [Branch] }

foldMapExpr :: Monoid a => (Expr -> a) -> Expr -> a
foldMapExpr acc e = case e of
  Lam _ _ body -> acc e <> foldMapExpr acc body
  Var _ _ -> acc e
  App f xs -> acc e <> acc f <> mconcat (foldMapExpr acc <$> xs)
  TypeLam _ body -> acc e <> foldMapExpr acc body
  TypeApp expr _ -> acc e <> foldMapExpr acc expr
  Con _ _ -> acc e
  Lit _ -> acc e
  Let binders body -> acc e <> mconcat [foldMapExpr acc (defExpr def) | def <- flattenDefGroups binders] <> foldMapExpr acc body
  Case cases branches -> acc e <> mconcat (foldMapExpr acc <$> cases) <>
    mconcat [foldMapExpr acc e | branch <- branches, guard <- branchGuards branch, e <- [guardTest guard, guardExpr guard]]

anySubExpr :: (Expr -> Bool) -> Expr -> Bool
anySubExpr f = getAny . foldMapExpr (Any . f)

foldExpr :: (Expr -> a -> a) -> a -> Expr -> a
foldExpr f z e = appEndo (foldMapExpr (Endo . f) e) z

rewriteBottomUp :: (Expr -> Expr) -> Expr -> Expr
rewriteBottomUp f = runIdentity . rewriteBottomUpM (Identity . f)

rewriteBottomUpM :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
rewriteBottomUpM f e = f =<< case e of
  Lam params eff body -> Lam params eff <$> rec body
  Var _ _ -> pure e
  App fun xs -> liftA2 App (rec fun) (mapM rec xs)
  TypeLam types body -> TypeLam types <$> rec body
  TypeApp expr types -> (\fexpr -> TypeApp fexpr types) <$> rec expr
  Con _ _ -> pure e
  Lit _ -> pure e
  Let binders body -> do
    newBinders <- forM binders $ \binder ->
      case binder of
        DefNonRec def@Def{defExpr = defExpr} -> do
          fexpr <- rec defExpr
          pure $ DefNonRec def { defExpr = fexpr, defType = typeOf fexpr }
        DefRec defs -> fmap DefRec $ forM defs $ \def@Def{defExpr = defExpr} -> do
          fexpr <- rec defExpr
          pure def{ defExpr = fexpr, defType = typeOf fexpr }

    Let newBinders <$> rec body

  Case cases branches -> liftA2 Case mcases mbranches
    where
      mcases = mapM rec cases
      mbranches = forM branches $ \(Branch patterns guards) ->
        Branch patterns <$> forM guards (\(Guard e1 e2) -> liftA2 Guard (rec e1) (rec e2))
  where
    rec = f <=< rewriteBottomUpM f


data TName = TName
  { getName :: Name
  , tnameType :: Type
  }

showTName (TName name tp)
    = show name -- ++ ": " ++ minCanonical tp


defTName :: Def -> TName
defTName def
  = TName (defName def) (defType def)

defGroupTNames :: DefGroup -> TNames
defGroupTNames (DefNonRec def) = S.singleton (defTName def)
defGroupTNames (DefRec defs) = S.fromList $ map defTName defs

defGroupsTNames :: DefGroups -> TNames
defGroupsTNames group = foldr S.union S.empty (map defGroupTNames group)

data VarInfo
  = InfoNone
  | InfoArity Int Int               -- #Type parameters, #parameters
  | InfoExternal [(Target,String)]  -- inline body
  | InfoReuse Pattern
  | InfoConField TName Name         -- constructor name, field name

instance Show VarInfo where
  show info = case info of
                InfoNone
                  -> ""
                InfoReuse pat
                  -> "reuse:<pat>"
                InfoConField conName fieldName
                  -> "field:" ++ show conName ++ "." ++ show fieldName
                InfoArity m n
                  -> "arity:" ++ show (m,n)
                InfoExternal formats
                  -> "external:" ++ show formats


infoArity (InfoArity m n) = n
infoArity (_)             = 0

infoTypeArity (InfoArity m n) = m
infoTypeArity (_)             = 0

isInfoArity (InfoArity _ _) = True
isInfoArity _ = False

infoIsLocal info
  = case info of
      InfoNone       -> True
      InfoReuse{}    -> True
      InfoArity{}    -> False
      InfoExternal{} -> False
      InfoConField{} -> False

infoIsRefCounted info
  = infoIsLocal info

data Branch = Branch { branchPatterns :: [Pattern]  -- length = length exprs in the match
                     , branchGuards   :: [Guard]    -- any number (>= 1) of guarded expressions
                     }

data Guard  = Guard { guardTest :: Expr  -- boolean
                    , guardExpr :: Expr  -- body of the branch
                    }

data Pattern
  = PatCon{ patConName :: TName,        -- ^ names the constructor with full signature.
            patConPatterns:: [Pattern], -- ^ sub-patterns. fully materialized to match arity.
            patConRepr :: ConRepr,      -- ^ representation of ctor in backend.
            patTypeArgs :: [Type],      -- ^ zipped with patConPatterns
            patExists :: [TypeVar],     -- ^ closed under existentials here
            patTypeRes :: Type,         -- ^ result type
            patConInfo :: ConInfo,      -- ^ other constructor info
            patConSkip :: Bool         -- ^ skip testing for this constructor (as it should match already)
          }
  | PatVar{ patName :: TName,           -- ^ name/type of variable
            patPattern :: Pattern       -- ^ named sub-pattern
          }
  | PatLit{ patLit :: Lit }
  | PatWild

data Lit =
    LitInt    Integer
  | LitFloat  Double
  | LitChar   Char
  | LitString String
  deriving (Eq)



-- | a core expression is total if it cannot cause non-total evaluation
{-
isTotal:: Expr -> Bool
isTotal expr
  = case expr of
      Lam _ _ _ -> True
      Var _ _ -> True
      TypeLam _ _ -> True
      TypeApp e _ -> isTotal e
      Con _ _ -> True
      Lit _   -> True
      App (Var v _) _ | getName v == nameReturn || getName v == nameTrace || getName v ==nameLog -> False
      App f args -> case typeOf f of
                      TFun pars eff res -> (length args == length pars && eff == typeTotal && all isTotal args)
                      _                 -> False
      _       -> False  -- todo: a let or case could be total
-}

-- | a core expression that cannot cause any evaluation _for sure_
isTotal :: Expr -> Bool
isTotal expr
 = case expr of
     Lam _ _ _   -> True
     Var _ _     -> True
     TypeLam _ e -> isTotal e
     TypeApp e _ -> isTotal e
     Con _ _     -> True
     Lit _      -> True
     Let dgs e  -> all isTotalDef (flattenDefGroups dgs) && isTotal e
     Case exps branches -> all isTotal exps && all isTotalBranch branches
     -- inline box/unbox
     App (Var v _) [arg] | getName v `elem` [nameBox,nameUnbox] -> isTotal arg
     _          -> False


isTotalDef def = isTotal (defExpr def)

isTotalBranch (Branch pat guards) = all isTotalGuard guards
isTotalGuard (Guard test expr)    = isTotal test && isTotal expr


isMonType :: Type -> Bool
isMonType tp
  | isKindEffect (getKind tp) = isMonEffect tp
  | otherwise =
    case expandSyn tp of
      TForall vars preds t -> isMonType t
      TFun pars eff res    -> isMonEffect eff
      _ -> False

isMonEffect :: Effect -> Bool
isMonEffect eff
  = let (ls,tl) = extractEffectExtend eff
    in not (isEffectEmpty tl) ||
       any (\l -> case getHandledEffect l of
                    Just (ResumeMany,_) -> True
                    _                   -> False) ls


isInlineable :: Int -> Def -> Bool
isInlineable inlineMax def
  = case defInline def of
      InlineAlways -> True
      InlineNever  -> False
      _            -> costDef def <= inlineMax


costInf :: Int
costInf = 1000

costDef :: Def -> Int
costDef def
  = let n = costLocalDef def
    in if (defIsVal def)
        then (if (n<=1) then 0 else costInf) -- don't duplicate (too much) work
        else n

costLocalDef :: Def -> Int
costLocalDef def
  = costExpr (defExpr def)

costDefGroup dg
  = case dg of
      DefRec defs   -> sum (map costLocalDef defs)
      DefNonRec def -> costLocalDef def

costExpr :: Expr -> Int
costExpr expr
  = case expr of
      Var tname info     | isHiddenExternalName (getName tname)
                         -> -- trace ("hidden external: " ++ show (getName tname) ) $
                            costInf
      Lam tname eff body -> 0 + costExpr body
      Var tname info     -> 0
      App e args         -> 1 + costExpr e + sum (map costExpr args)
      TypeLam tvs e      -> costExpr e
      TypeApp e tps      -> costExpr e
      Con tname repr     -> 0
      Lit lit            -> 0
      Let defGroups body -> sum (map costDefGroup defGroups) + (costExpr body)
      Case exprs branches -> (length branches - 1) + sum (map costExpr exprs) + sum (map costBranch branches)

costBranch (Branch patterns guards)
  = sum (map costGuard guards)

costGuard (Guard test expr)
  = costExpr test + costExpr expr

getTypeArityExpr :: Expr -> Int
getTypeArityExpr expr
  = fst (getTypeArities (typeOf expr))

getParamArityExpr :: Expr -> Int
getParamArityExpr expr
  = snd (getTypeArities (typeOf expr))

{-
getTypeArityExpr :: Expr -> Int
getTypeArityExpr expr
  = case expr of
      Var _ (InfoArity m n) -> m
      Var tname _           -> fst (getTypeArities (tnameType tname))
      Con tname _           -> fst (getTypeArities (tnameType tname))
      TypeApp e targs       -> getTypeArityExpr e - length targs
      TypeLam pars _        -> length pars
      Case _ (Branch _ (Guard _ e:_):_) -> getTypeArityExpr e
      _ -> 0

-- fun foo(x:int){ fun bar(y){ x + y }; [1,2].map(bar) }

getParamArityExpr :: Expr -> Int
getParamArityExpr expr
  = case expr of
    Var _ (InfoArity m n) -> n
    Var tname _           -> snd (getTypeArities (tnameType tname))
    Con tname _           -> snd (getTypeArities (tnameType tname))
    Lam pars _ _          -> length pars
    App f args            -> getParamArityExpr f - length args
    TypeLam _ e           -> getParamArityExpr e
    TypeApp e _           -> getParamArityExpr e
    Case _ (Branch _ (Guard _ e:_):_) -> getParamArityExpr e
    _ -> 0
-}

getEffExpr :: Expr -> Effect
getEffExpr (Lam _ eff _) = eff
getEffExpr (TypeLam _ (Lam _ eff _)) = eff
getEffExpr _ = effectEmpty


{--------------------------------------------------------------------------
  Type variables inside core expressions
--------------------------------------------------------------------------}


---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

type TNames = S.Set TName

tnamesEmpty :: TNames
tnamesEmpty = S.empty

tnamesList :: TNames -> [TName]
tnamesList tns
  = S.elems tns

tnamesFromList :: [TName] -> TNames
tnamesFromList tns
  = S.fromList tns

tnamesInsertAll :: TNames -> [TName] -> TNames
tnamesInsertAll  = foldr S.insert

tnamesUnion :: TNames -> TNames -> TNames
tnamesUnion = S.union

tnamesUnions :: [TNames] -> TNames
tnamesUnions xs = foldr tnamesUnion tnamesEmpty xs

tnamesDiff :: TNames -> TNames -> TNames
tnamesDiff = S.difference

tnamesRemove :: [TName] -> TNames -> TNames
tnamesRemove names set
  = foldr S.delete set names

tnamesMember :: TName -> TNames -> Bool
tnamesMember tname tnames = S.member tname tnames


instance Eq TName where
  (TName name1 tp1) == (TName name2 tp2)  = (name1 == name2) --  && matchType tp1 tp2)

instance Ord TName where
  compare (TName name1 tp1) (TName name2 tp2)
    = compare name1 name2
       {- EQ  -> compare (minCanonical tp1) (minCanonical tp2)
        lgt -> lgt -}




instance Show TName where
  show tname
    = show (getName tname)


{--------------------------------------------------------------------------
  Auxiliary functions to build Core terms
--------------------------------------------------------------------------}

-- | Create a let expression
makeLet :: [DefGroup] -> Expr -> Expr
makeLet [] expr = expr
makeLet defs (Let defs' body) = Let (defs ++ defs') body
makeLet defs expr = Let defs expr

makeTypeApp expr []     = expr
makeTypeApp (TypeApp expr targs0) targs1 = makeTypeApp expr (targs0 ++ targs1)
makeTypeApp expr targs  = TypeApp expr targs

-- | Add a value application
addApps :: [Expr] -> (Expr -> Expr)
addApps [] e             = e
addApps es (App e args)  = App e (args ++ es)
addApps es e             = App e es

-- | Add kind and type application
addTypeApps :: [TypeVar] -> (Expr -> Expr)
addTypeApps [] e                = e
addTypeApps ts (TypeApp e args) = TypeApp e (args ++ [TVar t | t <- ts])
addTypeApps ts e                = TypeApp e [TVar t | t <- ts]

-- | Add kind and type lambdas
addTypeLambdas :: [TypeVar] -> (Expr -> Expr)
addTypeLambdas []   e              = e
addTypeLambdas pars (TypeLam ps e) = TypeLam (pars ++ ps) e
addTypeLambdas pars e              = TypeLam pars e

-- | Add term lambdas
addLambdas :: [(Name, Type)] -> (Type -> Expr -> Expr)
addLambdas [] eff e              = e
addLambdas pars eff (Lam ps _ e) = Lam ([TName x tp | (x,tp) <- pars] ++ ps) eff e
addLambdas pars eff e            = Lam [TName x tp | (x,tp) <- pars] eff e

-- | Add term lambdas
addLambdasTName :: [TName] -> (Type -> Expr -> Expr)
addLambdasTName [] eff e              = e
addLambdasTName pars eff (Lam ps _ e) = Lam (pars ++ ps) eff e
addLambdasTName pars eff e            = Lam pars eff e

-- | Bind a variable inside a term
addNonRec :: Name -> Type -> Expr -> (Expr -> Expr)
addNonRec x tp e e' = Let [DefNonRec (Def x tp e Private (if isValueExpr e then DefVal else DefFun ) InlineAuto rangeNull "")] e'

-- | Is an expression a value or a function
isValueExpr :: Expr -> Bool
isValueExpr (TypeLam tpars (Lam pars eff e))   = False
isValueExpr (Lam pars eff e)                   = False
isValueExpr _                                  = True

-- | Add a definition
addCoreDef :: Core -> Def -> Core
addCoreDef (Core name imports fixdefs typeDefGroups (defGroups) externals doc) def
  = Core name imports fixdefs typeDefGroups (defGroups ++ [DefNonRec def]) externals doc

-- | Empty Core program
coreNull :: Name -> Core
coreNull name = Core name [] [] [] [] [] ""

-- | Create a fresh variable name with a particular prefix
freshName :: HasUnique m => String -> m Name
freshName prefix
  = do id <- unique
       return (newName $ prefix ++ "." ++ show id)

openEffectExpr :: Effect -> Effect -> Type -> Type -> Expr -> Expr
openEffectExpr effFrom effTo tpFrom tpTo expr
  = if (hasNoEffectExpr expr)
     then expr
     else --trace ("open effect: " ++ show (map pretty [effFrom,effTo,tpFrom,tpTo])) $
          App (TypeApp varOpen [effFrom,effTo,tpFrom,tpTo]) [expr]
  where
    varOpen = Var (TName nameEffectOpen tpOpen) (InfoExternal [(Default,"#1")])    -- NOTE: quite fragile as it relies on the exact definition in core.kk
    tpOpen  = TForall [e1,e2,a,b] [] (TFun [(newName "x", tpFrom)] typeTotal tpTo)
    a       = TypeVar (-1) kindStar Bound
    b       = TypeVar (-2) kindStar Bound
    e1      = TypeVar (-3) kindEffect Bound
    e2      = TypeVar (-4) kindEffect Bound

    hasNoEffectExpr expr
      = case expr of
          TypeApp e targs -> hasNoEffectExpr e
          Lit{} -> True
          Con{} -> True
          -- Var _ InfoExternal{} -> True  -- TODO: maybe too liberal?
          _     -> False

makeInt32 :: Integer -> Expr
makeInt32 i
  = let int32 = Var (TName nameInt32 (typeFun [(nameNil,typeInt)] typeTotal typeInt32)) (InfoArity 1 0 )
    in App int32 [Lit (LitInt i)]

makeEvIndex :: Integer -> Expr
makeEvIndex i | i < 0 = failure $ ("Core.Core.makeEvIndex: index < 0: " ++ show i)
makeEvIndex i
  = let sizet = Var (TName nameSSizeT (typeFun [(nameNil,typeInt)] typeTotal typeEvIndex)) (InfoArity 1 0 )
    in App sizet [Lit (LitInt i)]

makeSizeT :: Integer -> Expr
makeSizeT i | i < 0 = failure $ ("Core.Core.makeSizeT: size_t < 0: " ++ show i)
makeSizeT i
  = let sizet = Var (TName nameSSizeT (typeFun [(nameNil,typeInt)] typeTotal typeSSizeT)) (InfoArity 1 0 )
    in App sizet [Lit (LitInt i)]

---------------------------------------------------------------------------
-- type of a core term
---------------------------------------------------------------------------
class HasType a where
  typeOf :: a -> Type

instance HasType Def where
  typeOf def  = defType def

instance HasType TName where
  typeOf (TName _ tp)   = tp

instance HasType Expr where
  -- Lambda abstraction
  typeOf (Lam pars eff expr)
    = typeFun [(name,tp) | TName name tp <- pars] eff (typeOf expr)

  -- Variables
  typeOf (Var tname info)
    = typeOf tname

  -- Constants
  typeOf (Con tname repr)
    = typeOf tname

  -- Application
  typeOf expr@(App fun args)
    = -- snd (splitFun (typeOf fun))
      case splitFunScheme (typeOf fun) of
        Just (_,_,targs,eff,tres)          -- ignore forall as we can call this after box/unbox
           | length args == length targs || length targs == 0 -> tres
           | length args > length targs  -> typeOf (App (Var (TName (newName "tmp") tres) InfoNone) (drop (length targs) args))
           | otherwise -> TFun (drop (length args) targs) eff tres
        _ -> error ("Core.Core.typeOf.App: Expected function: " ++ show (pretty (typeOf fun)) ++ show (map (pretty . typeOf) args))  -- ++ " in the application " ++ show expr

  -- Type lambdas
  typeOf (TypeLam xs expr)
    = TForall xs [] (typeOf expr)

  -- Type application
  typeOf (TypeApp expr [])
    = typeOf expr

  typeOf tapp@(TypeApp expr tps)
    = let (tvs,tp1) = splitTForall (typeOf expr)
      in -- assertion "Core.Core.typeOf.TypeApp" (getKind a == getKind tp) $
         -- trace ("typeOf:TypeApp: , tvs: " ++ show (map pretty tvs) ++ ", tp1: " ++ show (pretty tp1)) $
         subNew (zip tvs tps) |-> tp1

  -- Literals
  typeOf (Lit l)
    = typeOf l

  -- Let
  typeOf (Let defGroups expr)
    = typeOf expr

  -- Case
  typeOf (Case exprs branches)
    = typeOf (head branches)


instance HasType Lit where
  typeOf lit
    = case lit of
        LitInt _    -> typeInt
        LitFloat _  -> typeFloat
        LitChar _   -> typeChar
        LitString _ -> typeString




{--------------------------------------------------------------------------
  Type of a branch
--------------------------------------------------------------------------}
instance HasType Branch where
  typeOf (Branch _ guards)
    = case guards of
        (guard:_) -> typeOf guard
        _         -> failure "Core.Core.HasType Branch: branch without any guards"

instance HasType Guard where
  typeOf (Guard _ expr)
    = typeOf expr


{--------------------------------------------------------------------------
  Extract types
--------------------------------------------------------------------------}
extractSignatures :: Core -> [Type]
extractSignatures core
  = let tps = concat [
                extractExternals (coreProgExternals core),
                extractDefs (coreProgDefs core)
              ]
    in -- trace ("extract signatures: " ++ show (map pretty tps)) $
       tps
  where
    extractExternals = concatMap extractExternal
    extractExternal ext@(External{ externalType = tp }) | externalVis ext == Public = [tp]
    extractExternal _ = []

    extractDefs = map defType . -- filter (\d -> defVis d == Public) .
                                flattenDefGroups

{--------------------------------------------------------------------------
  Decompose types
--------------------------------------------------------------------------}

splitFun :: Type -> ([(Name,Type)], Type)
-- splitFun (TApp (TApp con arg) res) | con == typeArrow = (arg, res)
splitFun tp
  = case expandSyn tp of
      TFun args eff res -> (args,res)
      _ -> failure ("Core.Core.splitFun: Expected function: " ++ show (pretty tp))

splitTForall :: Type -> ([TypeVar], Type)
splitTForall tp
  = case expandSyn tp of
      (TForall tvs _ tp) -> (tvs, tp) -- TODO what about the rest of the variables and preds?
      _ ->  failure ("Core.Core.splitTForall: Expected forall: " ++ show (pretty tp))


type Deps = S.Set Name

depsUnions xs = foldr S.union S.empty xs

depTName :: TName -> Deps
depTName tname = depName (getName tname)

depName :: Name -> Deps
depName name
  = if (isQualified name) then S.singleton (qualifier name) else S.empty

dependencies :: [InlineDef] -> Core -> Deps
dependencies inlineDefs core
  = S.union (inlineDependencies inlineDefs) (coreDependencies core)

inlineDependencies :: [InlineDef] -> Deps
inlineDependencies inlineDefs
  = depsUnions (map (depExpr . inlineExpr) inlineDefs)

coreDependencies :: Core -> Deps
coreDependencies (Core{coreProgName = mname, coreProgImports = imports, coreProgTypeDefs = tdefs, coreProgDefs = defs})
  = let deps = S.filter (mname /=) $
               depsUnions [S.fromList (map importName (filter (isPublic . importVis) imports)),
                           depsUnions (map depTDef (flattenTypeDefGroups tdefs)),
                           depsUnions (map depDef (flattenDefGroups defs))]
    in -- trace ("dependencies for " ++ show mname ++ ": " ++ show (S.elems deps)) $
       deps

depTDef :: TypeDef -> Deps
depTDef (Synonym info) = depType (synInfoType info)
depTDef (Data info _)  = depsUnions (map (depType . conInfoType) (dataInfoConstrs info))

depType :: Type -> Deps
depType tp
  = case tp of
      TForall vars preds rho  -> depType rho
      TFun args eff tp        -> depsUnions (map depType (tp:eff:map snd args))
      TCon tc                 -> depName (typeConName tc)
      TVar _                  -> S.empty
      TApp tp tps             -> depsUnions (map depType (tp:tps))
      TSyn syn args tp        -> depsUnions (map depType (tp:args))

depDef :: Def -> Deps
depDef def  = depsUnions [depType (defType def), depExpr (defExpr def)]

depExpr :: Expr -> Deps
depExpr expr
  = case expr of
      Var tname info     -> depTName tname
      Lam tname eff body -> S.union (depType eff) (depExpr body)
      App e args         -> depsUnions (map depExpr (e:args))
      TypeLam tvs e      -> depExpr e
      TypeApp e tps      -> depsUnions (depExpr e : map depType tps)
      Con tname repr     -> depTName tname
      Lit lit            -> S.empty
      Let defGroups body -> depsUnions (depExpr body : map depDef (flattenDefGroups defGroups))
      Case exprs branches -> depsUnions (map depExpr exprs ++ map depBranch branches)

depBranch (Branch patterns guards)
  = depsUnions (map depPat patterns ++ map depGuard guards)

depGuard (Guard test expr)
  = S.union (depExpr test) (depExpr expr)

depPat pat
  = case pat of
      PatCon{patConName=tname,patConPatterns=pats}
        -> depsUnions (depTName tname : map depPat pats)
      PatVar tname pat
        -> depPat pat
      _ -> S.empty

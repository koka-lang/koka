  -----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
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
                   , TName(..), getName, typeDefName
                   , showTName
                   , flattenTypeDefGroups
                   , flattenDefGroups
                   , flattenAllDefGroups
                   , extractSignatures
                   , typeDefIsExtension
                   , typeDefVis

                     -- Core term builders
                   , defIsVal
                   , defTName , defGroupTNames , defGroupsTNames
                   , addTypeLambdas, addTypeApps, addLambdas, addLambdasTName, addApps
                   , makeLet, makeTypeApp
                   , addNonRec, addCoreDef, coreNull
                   , freshName
                   , typeOf
                   , isExprUnit
                   , isExprTrue,  exprTrue, patTrue
                   , isExprFalse, exprFalse, patFalse
                   , isValueExpr
                   , openEffectExpr
                   , makeIfExpr
                   , makeInt32
                   , Visibility(..), Fixity(..), Assoc(..), isPublic
                   , coreName
                   , tnamesList, tnamesEmpty, tnamesDiff, tnamesInsertAll
                   , tnamesUnion, tnamesUnions, tnamesRemove
                   , getTypeArityExpr, getParamArityExpr, getEffExpr
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
                   , getDataRepr
                   , VarInfo(..)

                   , isMonType, isMonEffect

                   -- Inlining
                   , costDef, costExpr, costInf
                   , isInlineable

                   -- * Canonical names
                   , canonicalName, nonCanonicalName, canonicalSplit
                   , infoArity, infoTypeArity
                   ) where

import Data.Char( isDigit )
import qualified Data.Set as S
import Data.Maybe
import Lib.PPrint
import Common.Name
import Common.Range
import Common.Failure
import Common.Unique
import Common.NamePrim( nameTrue, nameFalse, nameTuple, nameTpBool, nameEffectOpen, nameReturn, nameTrace, nameLog,
                        nameEvvIndex, nameOpenAt, nameOpenNone, nameInt32 )
import Common.Syntax
import Kind.Kind
import Type.Type
import Type.Pretty ()
import Type.TypeVar
import Type.Kind    ( getKind, getHandledEffect, HandledSort(ResumeMany), isHandledEffect )

import Lib.Trace

isExprUnit (Con tname _)  = getName tname == nameTuple 0
isExprUnit _              = False

isExprTrue (Con tname _)  = (getName tname == nameTrue)
isExprTrue _              = False

isExprFalse (Con tname _)  = (getName tname == nameFalse)
isExprFalse _              = False

(patFalse,exprFalse) = patExprBool nameFalse 1
(patTrue,exprTrue)   = patExprBool nameTrue 2

patExprBool name tag
  = let tname   = TName name typeBool
        conEnum = ConEnum nameTpBool tag
        conInfo = ConInfo name nameTpBool [] [] [] (TFun [] typeTotal typeBool) Inductive rangeNull [] [] False Public ""
        pat = PatCon tname [] conEnum [] [] typeBool conInfo
        expr = Con tname conEnum
    in (pat,expr)

makeIfExpr :: Expr -> Expr -> Expr -> Expr
makeIfExpr pexpr texpr eexpr
  = Case [pexpr] [Branch [patTrue] [Guard exprTrue texpr],
                  Branch [PatWild] [Guard exprTrue eexpr]]


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
              | ExternalInclude{ externalInclude :: [(Target,String)]
                               , externalRange :: Range }
              | ExternalImport { externalImport :: [(Target,(Name,String))]
                               , externalRange :: Range }

externalVis :: External -> Visibility
externalVis (External{ externalVis' = vis }) = vis
externalVis _ = Private

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
data DataRepr = DataEnum            -- only singletons
              | DataIso             -- only one constructor with one field
              | DataSingleStruct    -- only one constructor; it has  less than max-struct fields
              | DataSingle          -- only one constructor
              | DataAsList          -- one constructor with fields, and one singleton
              | DataSingleNormal    -- one constructor with fields, and possibly singletons
              | DataStruct          -- one constructor with non-recursive fields <= max-struct fields, and possibly singletons
              | DataNormal
              | DataOpen
              deriving (Eq,Ord,Show)

data ConRepr  = ConEnum{ conTypeName :: Name, conTag :: Int }                     -- part of enumeration (none has fields)
              | ConIso{ conTypeName:: Name, conTag :: Int }                       -- one constructor with one field
              | ConSingleton{ conTypeName :: Name, conTag :: Int }                -- the only constructor without fields
              | ConSingle{ conTypeName :: Name, conTag :: Int }                   -- there is only one constructor (and this is it)
              | ConStruct{ conTypeName :: Name, conTag :: Int }                   -- constructor as value type
              | ConAsCons{ conTypeName :: Name, conAsNil :: Name, conTag :: Int } -- constructor is the cons node of a list-like datatype  (may have one or more fields)
              | ConOpen  { conTypeName :: Name }                                  -- constructor of open data type
              | ConNormal{ conTypeName :: Name, conTag :: Int }                   -- a regular constructor
              deriving (Eq,Ord,Show)

isConSingleton (ConSingleton _ _) = True
isConSingleton _ = False

isConNormal (ConNormal _ _) = True
isConNormal _  = False

isConIso (ConIso{}) = True
isConIso _ = False

isDataStruct (DataStruct) = True
isDataStruct _ = False

getDataRepr :: Int -> DataInfo -> (DataRepr,[ConRepr])
getDataRepr maxStructFields info
  = let typeName  = dataInfoName info
        conInfos = dataInfoConstrs info
        conTags  = [0..length conInfos - 1]
        singletons =  filter (\con -> null (conInfoParams con)) conInfos
        hasExistentials = any (\con -> not (null (conInfoExists con))) conInfos
        (dataRepr,conReprFuns) =
         if (dataInfoIsOpen(info))
          then (DataOpen, map (\conInfo conTag -> ConOpen typeName) conInfos)
         else if (hasExistentials)
          then (DataNormal, map (\con -> ConNormal typeName) conInfos)
         else if (null (dataInfoParams info) && all (\con -> null (conInfoParams con)) conInfos)
          then (DataEnum,map (const (ConEnum typeName)) conInfos)
         else if (length conInfos == 1)
          then let conInfo = head conInfos
               in (if (length (conInfoParams conInfo) == 1)
                    then DataIso
                   else if (length (conInfoParams conInfo) <= maxStructFields && null singletons && not (dataInfoIsRec info))
                    then DataSingleStruct
                    else DataSingle
                  ,[if (length (conInfoParams conInfo) == 1) then ConIso typeName
                    else if length singletons == 1 then ConSingleton typeName
                    else ConSingle typeName])
         else if (length singletons == length conInfos-1 && length (concatMap conInfoParams conInfos) <= maxStructFields && not (dataInfoIsRec info))
          then (DataStruct, map (\_ -> ConStruct typeName) conInfos )
         else if (length conInfos == 2 && length singletons == 1)
          then (DataAsList
               ,map (\con -> if (null (conInfoParams con)) then ConSingleton typeName
                              else ConAsCons typeName (conInfoName (head singletons))) conInfos)
         else (if (length singletons == length conInfos -1 || null conInfos) then DataSingleNormal else DataNormal
               ,map (\con -> {- if null (conInfoParams con) then ConSingleton typeName else -}
                              ConNormal typeName) conInfos
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

-- | A value definition
data Def = Def{ defName  :: Name
              , defType  :: Scheme
              , defExpr  :: Expr
              , defVis   :: Visibility     -- Private, Public
              , defSort  :: DefSort        -- DefFun, DefVal, DefVar
              , defNameRange :: Range
              , defDoc :: String
              }

data InlineDef = InlineDef{ inlineName :: Name, inlineExpr :: Expr, inlineRec :: Bool, inlineCost :: Int }

defIsVal :: Def -> Bool
defIsVal def
  = case defSort def of
      DefFun   -> False
      _        -> True

canonicalName :: Int -> Name -> Name
canonicalName n name
  = if (n/=0) then postpend ([canonicalSep] ++ show n) name else name

nonCanonicalName :: Name -> Name
nonCanonicalName name
  = fst (canonicalSplit name)

canonicalSplit :: Name -> (Name,String)
canonicalSplit name
  = case (span isDigit (reverse (nameId name))) of
      (postfix, c:rest) | c == canonicalSep && not (null postfix) -> (newQualified (nameModule name) (reverse rest), c:reverse postfix)
      _        -> (name,"")


{--------------------------------------------------------------------------
  Expressions

  Since this is System-F, all binding sites are annotated with their type.
--------------------------------------------------------------------------}

data Expr =
  -- Core lambda calculus
    Lam [TName] Effect Expr
  | Var{ varName :: TName, varInfo :: VarInfo }  -- ^ typed name and possible typeArity/parameter arity tuple for top-level functions
  | App Expr [Expr]                              -- ^ always fully applied!
  -- Type (universal) abstraction/application
  | TypeLam [TypeVar] Expr
  | TypeApp Expr [Type]
  -- Literals, constants and labels
  | Con{ conName :: TName, conRepr ::  ConRepr  }          -- ^ typed name and its representation
  | Lit Lit
  -- Let
  | Let DefGroups Expr
  -- Case expressions
  | Case{ caseExprs :: [Expr], caseBranches :: [Branch] }

data TName = TName Name Type

getName (TName name _) = name

tnameType (TName name tp) = tp

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
  deriving Show

infoArity (InfoArity m n) = n
infoArity (_)             = 0

infoTypeArity (InfoArity m n) = m
infoTypeArity (_)             = 0


data Branch = Branch { branchPatterns :: [Pattern]
                     , branchGuards   :: [Guard]
                     }

data Guard  = Guard { guardTest :: Expr
                    , guardExpr :: Expr
                    }

data Pattern
  = PatCon{ patConName :: TName, patConPatterns:: [Pattern], patConRepr :: ConRepr, patTypeArgs :: [Type], patExists :: [TypeVar], patTypeRes :: Type, patConInfo :: ConInfo }
  | PatVar{ patName :: TName, patPattern :: Pattern }
  | PatLit{ patLit :: Lit }
  | PatWild

data Lit =
    LitInt    Integer
  | LitFloat  Double
  | LitChar   Char
  | LitString String
  deriving (Eq)



-- | a core expression is total if it cannot cause non-total evaluation
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
  = costDef def <= inlineMax


costInf :: Int
costInf = 1000

costDef :: Def -> Int
costDef def
  = let n = costLocalDef def
    in if (defIsVal def)
        then (if (n==0) then 0 else costInf) -- don't duplicate work
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
                            1000
      Lam tname eff body -> 1 + costExpr body
      Var tname info     -> 0
      App e args         -> 1 + costExpr e + sum (map costExpr args)
      TypeLam tvs e      -> costExpr e
      TypeApp e tps      -> costExpr e
      Con tname repr     -> 0
      Lit lit            -> 0
      Let defGroups body -> sum (map costDefGroup defGroups) + (costExpr body)
      Case exprs branches -> 1 + sum (map costExpr exprs) + sum (map costBranch branches)

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


instance HasTypeVar DefGroup where
  sub `substitute` defGroup
    = case defGroup of
        DefRec defs   -> DefRec (sub `substitute` defs)
        DefNonRec def -> DefNonRec (sub `substitute` def)

  ftv defGroup
    = case defGroup of
        DefRec defs   -> ftv defs
        DefNonRec def -> ftv def

  btv defGroup
    = case defGroup of
        DefRec defs   -> btv defs
        DefNonRec def -> btv def


instance HasTypeVar Def where
  sub `substitute` (Def name scheme expr vis isVal nameRng doc)
    = Def name (sub `substitute` scheme) (sub `substitute` expr) vis isVal nameRng doc

  ftv (Def name scheme expr vis isVal  nameRng doc)
    = ftv scheme `tvsUnion` ftv expr

  btv (Def name scheme expr vis isVal nameRng doc)
    = btv scheme `tvsUnion` btv expr

instance HasTypeVar Expr where
  sub `substitute` expr
    = case expr of
        Lam tnames eff expr -> Lam (sub `substitute` tnames) (sub `substitute` eff) (sub `substitute` expr)
        Var tname info    -> Var (sub `substitute` tname) info
        App f args        -> App (sub `substitute` f) (sub `substitute` args)
        TypeLam tvs expr  -> let sub' = subRemove tvs sub
                              in TypeLam tvs (sub' |-> expr)
        TypeApp expr tps   -> TypeApp (sub `substitute` expr) (sub `substitute` tps)
        Con tname repr     -> Con (sub `substitute` tname) repr
        Lit lit            -> Lit lit
        Let defGroups expr -> Let (sub `substitute` defGroups) (sub `substitute` expr)
        Case exprs branches -> Case (sub `substitute` exprs) (sub `substitute` branches)

  ftv expr
    = case expr of
        Lam tname eff expr -> tvsUnions [ftv tname, ftv eff, ftv expr]
        Var tname info     -> ftv tname
        App a b            -> ftv a `tvsUnion` ftv b
        TypeLam tvs expr   -> tvsRemove tvs (ftv expr)
        TypeApp expr tp    -> ftv expr `tvsUnion` ftv tp
        Con tname repr     -> ftv tname
        Lit lit            -> tvsEmpty
        Let defGroups expr -> ftv defGroups `tvsUnion` ftv expr
        Case exprs branches -> ftv exprs `tvsUnion` ftv branches

  btv expr
    = case expr of
        Lam tname eff expr -> tvsUnions [btv tname, btv eff, btv expr]
        Var tname info     -> btv tname
        App a b            -> btv a `tvsUnion` btv b
        TypeLam tvs expr   -> tvsInsertAll tvs (btv expr)
        TypeApp expr tp    -> btv expr `tvsUnion` btv tp
        Con tname repr     -> btv tname
        Lit lit            -> tvsEmpty
        Let defGroups expr -> btv defGroups `tvsUnion` btv expr
        Case exprs branches -> btv exprs `tvsUnion` btv branches


instance HasTypeVar Branch where
  sub `substitute` (Branch patterns guards)
    = let sub' = subRemove (tvsList (btv patterns)) sub
      in Branch (map ((sub `substitute`)) patterns) (map (sub' `substitute`) guards)

  ftv (Branch patterns guards)
    = ftv patterns `tvsUnion` (tvsDiff (ftv guards) (btv patterns))

  btv (Branch patterns guards)
    = btv patterns `tvsUnion` btv guards


instance HasTypeVar Guard where
  sub `substitute` (Guard test expr)
    = Guard (sub `substitute` test) (sub `substitute` expr)
  ftv (Guard test expr)
    = ftv test `tvsUnion` ftv expr
  btv (Guard test expr)
    = btv test `tvsUnion` btv expr

instance HasTypeVar Pattern where
  sub `substitute` pat
    = case pat of
        PatVar tname pat   -> PatVar (sub `substitute` tname) (sub `substitute` pat)
        PatCon tname args repr tps exists restp info
          -> let sub' = subRemove exists sub
             in PatCon (sub `substitute` tname) (sub' `substitute` args) repr (sub' `substitute` tps) exists (sub' `substitute` restp) info
        PatWild           -> PatWild
        PatLit lit        -> pat


  ftv pat
    = case pat of
        PatVar tname pat    -> tvsUnion (ftv tname) (ftv pat)
        PatCon tname args _ targs exists tres _ -> tvsRemove exists (tvsUnions [ftv tname,ftv args,ftv targs,ftv tres])
        PatWild             -> tvsEmpty
        PatLit lit          -> tvsEmpty

  btv pat
    = case pat of
        PatVar tname pat           -> tvsUnion (btv tname) (btv pat)
        PatCon tname args _ targs exists tres _  -> tvsUnions [btv tname,btv args,btv targs,btv tres,tvsNew exists]
        PatWild                 -> tvsEmpty
        PatLit lit              -> tvsEmpty


instance HasTypeVar TName where
  sub `substitute` (TName name tp)
    = TName name (sub `substitute` tp)
  ftv (TName name tp)
    = ftv tp
  btv (TName name tp)
    = btv tp


---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

type TNames = S.Set TName

tnamesEmpty :: TNames
tnamesEmpty = S.empty

tnamesList :: TNames -> [TName]
tnamesList tns
  = S.elems tns

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
addNonRec x tp e e' = Let [DefNonRec (Def x tp e Private (if isValueExpr e then DefVal else DefFun ) rangeNull "")] e'

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
  = App (TypeApp varOpen [effFrom,effTo,tpFrom,tpTo]) [expr]
  where
    varOpen = Var (TName nameEffectOpen tpOpen) (InfoExternal [(Default,"#1")])    -- NOTE: quite fragile as it relies on the exact definition in core.kk
    tpOpen  = TForall [e1,e2,a,b] [] (TFun [(newName "x", tpFrom)] typeTotal tpTo)
    a       = TypeVar (-1) kindStar Bound
    b       = TypeVar (-2) kindStar Bound
    e1      = TypeVar (-3) kindEffect Bound
    e2      = TypeVar (-4) kindEffect Bound


makeInt32 :: Integer -> Expr
makeInt32 i
  = let int32 = Var (TName nameInt32 (typeFun [(nameNil,typeInt)] typeTotal typeInt32)) (InfoArity 1 0 )
    in App int32 [Lit (LitInt i)]

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
      case expandSyn (typeOf fun) of
         TFun targs eff tres
           | length args == length targs || length targs == 0 -> tres
           | length args > length targs  -> typeOf (App (Var (TName (newName "tmp") tres) InfoNone) (drop (length targs) args))
           | otherwise -> TFun (drop (length args) targs) eff tres
         _ -> failure ("Core.Core.typeOf.App: Expected function: " ++ show (pretty (typeOf fun))) -- ++ " in the application " ++ show (expr))

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

    extractDefs = map defType . filter (\d -> defVis d == Public) . flattenDefGroups

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
      _ ->  failure ("Core.Core.splitTForall: Expected forall" ++ show (pretty tp))

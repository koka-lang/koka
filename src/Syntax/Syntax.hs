------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Definition of the concrete syntax.
-}
-----------------------------------------------------------------------------
module Syntax.Syntax where

import Common.NamePrim( nameTrue, nameSystemCore )
import Common.Name
import Common.Range
import Common.Failure( failure )
import Common.Syntax
import qualified Common.NameSet as S

-- | A program
data Program t k
  = Program{ programSource :: Source
           , programName   :: Name
           , programNameRange :: Range
           , programTypeDefs :: [TypeDefGroup t k]
           , programDefs      :: (DefGroups t)
           , programImports  :: Imports
           , programExternals :: Externals
           , programFixDefs :: FixDefs
           , programDoc :: String
           }

{--------------------------------------------------------------------------
  Some instantiations
--------------------------------------------------------------------------}

type UserProgram = Program UserType UserKind
type UserExpr    = Expr UserType
type UserDef     = Def UserType
type UserDefGroup = DefGroup UserType
type UserTypeDef = TypeDef UserType UserType UserKind
type UserTypeDefGroup = TypeDefGroup UserType UserKind
type UserUserCon = UserCon UserType UserType UserKind
type UserPattern = Pattern UserType
type UserBranch  = Branch UserType
type UserHandlerBranch = HandlerBranch UserType
type UserValueBinder = ValueBinder () -- (Maybe UserType)
type UserTypeBinder = TypeBinder UserKind

{--------------------------------------------------------------------------
  External definitions
--------------------------------------------------------------------------}
type Externals
  = [External]

data External
  = External{ extName :: Name
            , extType :: UserType
            , extNameRange :: Range
            , extRange :: Range
            , extInline :: [(Target,ExternalCall)]  -- map: target inline
            , extVis  :: Visibility
            , extDoc :: String
            }
  | ExternalInclude{ extInclude :: [(Target,String)]
                   , extRange :: Range
                   }
  | ExternalImport{ extImport :: [(Target,(Name,String))]
                  , extRange :: Range }

data ExternalCall
  = ExternalInline String  -- inline everywhere
  | ExternalCall String    -- create a function call

type FixDefs
  = [FixDef]

data FixDef
  = FixDef Name Fixity Range

{---------------------------------------------------------------
  Import definitions
---------------------------------------------------------------}
type Imports  = [Import]

-- | Import declaration
data Import
  = Import{ importName ::  Name    -- ^ module name
          , importFullName :: Name     -- ^ fully qualified module name
          , importRange :: Range   -- ^ range of the import declaration
          , importVis   :: Visibility  -- ^ visibility of the module
          }


{--------------------------------------------------------------------------
  type definitions
--------------------------------------------------------------------------}
-- | Recursive type definition groups
type TypeDefGroups t k
  = [TypeDefGroup t k]

-- | A recursive type definition group
data TypeDefGroup t k
  = TypeDefRec [TypeDef t t k]
  | TypeDefNonRec (TypeDef t t k)

-- | type definitions.
data TypeDef t u k
  = Synonym { typeDefBinder :: (TypeBinder k)
            , typeDefParams :: [TypeBinder k]
            , typeDefSynonym :: t
            , typeDefRange :: Range
            , typeDefVis :: Visibility
            , typeDefDoc :: String
            }
  | DataType{ typeDefBinder :: (TypeBinder k)
            , typeDefParams :: [TypeBinder k]
            , typeDefConstrs :: [UserCon t u k]
            , typeDefRange :: Range
            , typeDefVis :: Visibility
            , typeDefSort :: DataKind
            , typeDefDef  :: DataDef
            , typeDefIsExtend :: Bool -- ^ True if this is an extension; the binder contains a qualified id (and is not a declaration)
            , typeDefDoc  :: String
            }

data TypeBinder k
  = TypeBinder{ tbinderName :: Name -- ^ name
              , tbinderKind :: k  -- ^ kind
              , tbinderNameRange :: Range -- ^ name range
              , tbinderRange :: Range -- ^ total range
              }

-- | Constructor: name, existentials, type parameters, name range, total range, and visibility
data UserCon t u k
  = UserCon { userconName :: Name
            , userconExists :: [TypeBinder k] -- ^ existentials
            , userconParams :: [(Visibility,ValueBinder t (Maybe (Expr u)))]            -- ^ parameters
            , userconNameRange :: Range       --  ^ name range
            , userconRange :: Range           --  ^ total range
            , userconVis :: Visibility     -- ^  visibility
            , userconDoc :: String
            }

{--------------------------------------------------------------------------
  Definitions
--------------------------------------------------------------------------}
-- | A list of a list of value definitions. Each element is (recursive) binding group
type DefGroups t
  = [DefGroup t]

-- | Value definitions, either recursive or a single non-recursive binding
data DefGroup t
  = DefRec    [Def t]
  | DefNonRec (Def t)

type Defs t
  = [Def t]

data ValueBinder t e
  = ValueBinder{ binderName :: Name    -- ^ name
               , binderType :: t        -- ^ Type. Always present for constructors.
               , binderExpr  :: e      -- ^ Expression: always present for definitions as 'Expr t'
                                       -- Function and constructor parameters use 'Maybe (Expr t)' for default values.
                                       -- Pattern bindings ('PatVar') use unit '()'.
               , binderNameRange :: Range  -- ^ name range
               , binderRange :: Range      -- ^ full range
               }


--  | A value or function definition
data Def t
  = Def{ defBinder :: ValueBinder () (Expr t)  -- a type on the definition goes into an outer Ann node on the expression
       , defRange  :: Range
       , defVis    :: Visibility
       , defSort   :: DefSort
       , defDoc    :: String
       }


defIsVal def
  = not (isDefFun (defSort def))


guardTrue
  = Var nameTrue False rangeNull

{--------------------------------------------------------------------------
  Expression
--------------------------------------------------------------------------}

-- | Expressions
data Expr t
  = Lam    [ValueBinder (Maybe t) (Maybe (Expr t))] (Expr t) Range
  | Let    (DefGroup t) (Expr t)    Range
  | Bind   (Def t) (Expr t)         Range
  | App    (Expr t) [(Maybe (Name,Range),Expr t)] Range
  | Var    Name Bool Range
  | Lit    Lit
  | Ann    (Expr t) t Range
  | Case   (Expr t) [Branch t]   Range
  | Parens (Expr t)              Range
  | Handler (HandlerSort (Expr t)) HandlerScope (Maybe t) [ValueBinder (Maybe t) ()] 
                  (Expr t) (Expr t) (Expr t) [HandlerBranch t] Range Range
  | Inject t (Expr t) Range

data HandlerScope
  = HandlerNoScope | HandlerScoped
  deriving (Eq)

data HandlerBranch t
  = HandlerBranch{ hbranchName :: Name
                 , hbranchPars :: [ValueBinder (Maybe t) ()]
                 , hbranchExpr :: Expr t
                 , hbranchNameRange :: Range
                 , hbranchPatRange  :: Range
                 }

data Branch t
  = Branch{ branchPattern :: (Pattern t)
          , branchGuard :: (Expr t)
          , branchExpr   :: (Expr t)
          }

-- | Patterns
data Pattern t
  = PatWild   Range
  | PatVar    (ValueBinder (Maybe t) (Pattern t)) -- the binderExpr is a potential other pattern (used for "as x" bindings)
  | PatAnn    (Pattern t) t Range
  | PatCon    Name    [(Maybe (Name,Range), Pattern t)] Range Range  -- name range and full range
  | PatParens (Pattern t) Range
  | PatLit    Lit

-- | Literals
data Lit
  = LitInt      Integer   Range
  | LitFloat    Double  Range
  | LitChar     Char     Range
  | LitString   String Range

{--------------------------------------------------------------------------
  types and Kinds
--------------------------------------------------------------------------}
type KUserTypes k
  = [KUserType k]

data UserQuantifier  = QSome | QForall | QExists

-- (Higher ranked) types
data KUserType k
  = TpQuan     UserQuantifier (TypeBinder k) (KUserType k) Range
  | TpQual     [KUserType k] (KUserType k)
  | TpFun      [(Name,KUserType k)] (KUserType k) (KUserType k) Range
  | TpApp      (KUserType k)  [KUserType k] Range
  | TpVar      Name                  Range
  | TpCon      Name                  Range
  | TpParens   (KUserType k)         Range
  | TpAnn      (KUserType k)  k

type UserType
  = KUserType UserKind


-- | A kind
data UserKind
  = KindCon    Name Range
  | KindArrow  UserKind UserKind
  | KindParens UserKind Range
  | KindNone  -- flags that there is no explicit kind annotation



{--------------------------------------------------------------------------
  The range of expressions/types etc. can be retrieved via "getRange"
--------------------------------------------------------------------------}
instance Ranged (TypeDef t u k) where
  getRange typeDef
    = typeDefRange typeDef

instance Ranged t => Ranged (Def t) where
  getRange (Def binder nameTypeRange _ _ _)
    = getRange binder

instance Ranged (ValueBinder t e) where
  getRange vb = binderRange vb


instance Ranged k => Ranged (KUserType k) where
  getRange tp
    = case tp of
       TpQuan     quant tname userType range -> range
       TpQual     preds tp        -> combineRange (getRange preds) (getRange tp)
       TpFun      args effect tp r-> r
       TpApp      tp tps rng      -> rng
       TpVar      name range      -> range
       TpCon      name range      -> range
       TpParens   userTp range    -> range
       TpAnn      userTp kind -> combineRange (getRange userTp) (getRange kind)

instance Ranged (TypeBinder k) where
  getRange (TypeBinder _ _ _ range) = range

instance Ranged (UserCon t u k) where
  getRange (UserCon _ _ _ _ range _ _) = range

instance Ranged a => Ranged [a] where
  getRange rs
    = if null rs then rangeNull else combineRange (getRange (head rs)) (getRange (last rs))

instance Ranged UserKind where
  getRange kind
    = case kind of
        KindCon    name range -> range
        KindArrow  k1 k2      -> combineRange (getRange k1) (getRange k2)
        KindParens knd range  -> range
        KindNone              -> rangeNull


instance Ranged (Expr t) where
  getRange expr
    = case expr of
        Lam    pat expr range  -> range
        Let    defs expr range -> range
        Bind   def expr range  -> range
        App    fun exprs range -> range
        Var    name isop range -> range
        Lit    lit             -> getRange lit
        Ann    expr tp range   -> range
        Case   exprs branches range -> range
        Parens expr range      -> range
        Handler shallow scoped eff pars reinit ret final ops hrng range -> range
        Inject tp expr range -> range

instance Ranged Lit where
  getRange lit
    = case lit of
        LitInt   num range -> range
        LitFloat num range -> range
        LitChar  chr range -> range
        LitString s range  -> range

instance Ranged (Pattern t) where
  getRange pat
    = case pat of
        PatWild range           -> range
        PatVar  binder          -> getRange binder
        PatAnn  pat tp range    -> range
        PatCon  name args nameRng range -> range
        PatParens pat range     -> range
        PatLit lit              -> getRange lit

instance Ranged (Branch t) where
  getRange (Branch patterns guard body)
    = combineRange (getRange patterns) (getRange body)

instance Ranged (HandlerBranch t) where
  getRange (HandlerBranch{ hbranchPatRange=rng, hbranchExpr=expr })
    = combineRange rng (getRange expr)

---------------------------------------------------------------------------
-- Get Name
---------------------------------------------------------------------------

type NameRange = (Name,Range)

class HasName a where
  getName :: a -> Name
  getNameRange :: a -> Range
  getRName :: a -> NameRange

  getRName x       = (getName x, getNameRange x)
  getNameRange x   = snd (getRName x)
  getName x        = fst (getRName x)

instance HasName (Program t k) where
  getName (Program _ name _ _ _ _ _ _ _) = name
  getNameRange (Program _ _ range _ _ _ _ _ _) = range

instance HasName (TypeBinder k) where
  getName (TypeBinder name kind nameRange range) = name
  getNameRange (TypeBinder name kind nameRange range) = nameRange

instance HasName (UserCon t u k) where
  getName (UserCon name exist params nameRange range _ _) = name
  getNameRange (UserCon name exist params nameRange range _ _) = nameRange

instance HasName (TypeDef t u k) where
  getRName typeDef
    = getRName (typeDefBinder typeDef)

instance HasName (ValueBinder t e) where
  getRName vb = (binderName vb,binderNameRange vb)

instance HasName (Def t) where
  getRName (Def vb range _ _ _) = getRName vb




---------------------------------------------------------------------------
-- Free type variables
---------------------------------------------------------------------------
class HasFreeTypeVar a where
  freeTypeVars :: a -> S.NameSet

instance HasFreeTypeVar a => HasFreeTypeVar (Maybe a) where
  freeTypeVars Nothing = S.empty
  freeTypeVars (Just x) = freeTypeVars x

instance HasFreeTypeVar a => HasFreeTypeVar [a] where
  freeTypeVars xs = S.unions (map freeTypeVars xs)

instance HasFreeTypeVar (KUserType k) where
  freeTypeVars tp
    = case tp of
       TpQuan quant (TypeBinder name k _ _) tp _
                                      -> S.delete name (freeTypeVars tp)
       TpQual     preds tp            -> freeTypeVars (tp:preds)
       TpFun      args effect tp rng  -> freeTypeVars (tp:effect:map snd args)
       TpApp      tp args range       -> S.union (freeTypeVars tp) (freeTypeVars args)
       TpVar      name range          -> S.singleton name
       TpCon      name range          -> S.empty
       TpParens   tp range            -> freeTypeVars tp
       TpAnn      tp kind             -> freeTypeVars tp

instance HasFreeTypeVar a => HasFreeTypeVar (Either a b) where
  freeTypeVars (Left x) = freeTypeVars x
  freeTypeVars (Right y) = S.empty

{--------------------------------------------------------------------------
  Access definitions
--------------------------------------------------------------------------}
defBody :: Def t -> Expr t
defBody (Def vb _ _ _ _)  = binderExpr vb

defName :: Def t -> Name
defName (Def vb _ _ _ _)  = binderName vb

defType :: Def t -> Maybe t
defType def
  = case binderExpr (defBinder def) of
      Ann _ tp _ -> Just tp
      _          -> Nothing


typeDefName typeDef
  = tbinderName (typeDefBinder typeDef)

typeDefNameRange typeDef
  = getRange (typeDefBinder typeDef)


programNull :: Name -> Program t k
programNull name = Program sourceNull name rangeNull [] [] [preludeImport] [] [] ""

-- | Import declaration for the standard prelude
preludeImport :: Import
preludeImport
  = Import nameSystemCore nameSystemCore rangeNull Private

makeProgram :: Name -> [TypeDef t t k] -> (Defs t) -> Program t k
makeProgram name typedefs defs
  = Program sourceNull name rangeNull [TypeDefRec typedefs] [DefRec defs] [] [] [] ""

programAddImports :: Program t k -> [Import] -> Program t k
programAddImports program imports
  = program{ programImports = (programImports program) ++ imports }

programAddDefs :: Program t k -> [TypeDef t t k] -> (Defs t) -> Program t k
programAddDefs (Program source modName nameRange tdefs defs imports externals fixDefs doc) ts1 ds1
  = Program source modName nameRange
    (case tdefs of
       []                -> [TypeDefRec (ts1)]
       [TypeDefRec ts]   -> [TypeDefRec (removeTDefs ts ts1 ++ ts1)]
       _                 -> failure "Syntax.Syntax: can not add type definitions to processed tree")
    (case defs of
       []          -> [DefRec (ds1)]
       [DefRec ds] -> [DefRec (removeDefs ds ds1 ++ ds1)]
       _           -> failure "Syntax.Syntax: can not add definitions to processed tree")
    imports
    externals
    fixDefs
    doc
  where
    removeTDefs tdefs ts
      = foldl removeTDef tdefs ts

    removeTDef tdefs tdef
      = filter (\t -> typeDefName tdef /= typeDefName t) tdefs

    removeDefs defs ds
      = foldl removeDef defs ds

    removeDef defs def
      = filter (\d -> defName def /= defName d) defs


programRemoveAllDefs :: Program t k -> Program t k
programRemoveAllDefs program
  = program{ programDefs = [] }

programRemoveDef :: Name -> Program t k -> Program t k
programRemoveDef name (Program source modName nameRange tdefs defs imports ext fixDefs doc)
  = Program source modName nameRange (map filterTDef tdefs) (map filterDef defs) imports ext fixDefs doc
  where
    filterTDef  (TypeDefRec ts)    = TypeDefRec (filter (neqName . typeDefName) ts)
    filterTDef  (TypeDefNonRec t)  = if (neqName (typeDefName t)) then TypeDefNonRec t else TypeDefRec []

    filterDef   (DefRec ds)        = DefRec (filter (neqName . defName) ds)
    filterDef   (DefNonRec d)      = if neqName (defName d)
                                      then DefNonRec d
                                      else DefRec []

    neqName n  = (n /= name)


programFind :: Ranged t => Name -> Program t k -> Maybe Range
programFind name (Program source modName nameRange tdefs defs imports ext fixDefs doc)
  = lookup name (concatMap trange tdefs ++ concatMap drange defs)
  where
    trange (TypeDefRec ts)    = [(typeDefName t, getRange t) | t <- ts]
    trange (TypeDefNonRec td) = [(typeDefName t, getRange t) | t <- [td]]

    drange (DefRec ds)        = [(defName d, getRange d) | d <- ds]
    drange (DefNonRec def)    = [(defName d, getRange d) | d <- [def]]


instance Show UserQuantifier where
  show QSome
    = "some"
  show QForall
    = "forall"
  show QExists
    = "exists"

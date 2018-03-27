------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
module Static.BindingGroups( bindingGroups ) where


import qualified Common.NameMap as M
import qualified Common.NameSet as S

import Data.List(partition,isPrefixOf)
import Lib.Scc( scc )  -- determine strongly connected components
import Common.Name
import Common.NamePrim (toShortModuleName)
import Common.Range
import Common.Syntax
import Syntax.Syntax

import Lib.Trace (trace)

---------------------------------------------------------------------------
-- Program
---------------------------------------------------------------------------

bindingGroups :: UserProgram -> UserProgram
bindingGroups (Program source modName nameRange typeDefs defs imports externals fixDefs doc)
  = Program source modName nameRange (bindingsTypeDefs typeDefs) (bindings (toShortModuleName modName) defs) imports externals fixDefs doc

---------------------------------------------------------------------------
-- Binding groups in type definitions
---------------------------------------------------------------------------
bindingsTypeDefs :: [UserTypeDefGroup] -> [UserTypeDefGroup]
bindingsTypeDefs typeDefGroups
  = let (ds,extends) = partition isDefinition (flatten typeDefGroups)
    in groupTypeDefs ds (M.fromList (map dependencyTypeDef ds)) ++ (map TypeDefNonRec extends)
  where
    flatten groups
      = concatMap (\g -> case g of { TypeDefRec typeDefs -> typeDefs; TypeDefNonRec td -> [td]}) groups

    isDefinition td
      = case td of
          DataType binder args cons range vis sort ddef isExtend doc -> not isExtend
          _ -> True

dependencyTypeDef :: UserTypeDef -> (Name,S.NameSet)
dependencyTypeDef typeDef
  = case typeDef of
      Synonym binder args tp range vis doc    -> (typeDefName typeDef, freeTypes tp)
      DataType binder args cons range vis sort ddef isExtend doc -> (typeDefName typeDef, freeTypes cons)

---------------------------------------------------------------------------
-- Free type constructors
---------------------------------------------------------------------------

class HasFreeTypes a where
  freeTypes :: a -> S.NameSet

instance HasFreeTypes a => HasFreeTypes [a] where
  freeTypes xs = S.unions (map freeTypes xs)

instance HasFreeTypes a => HasFreeTypes (Maybe a) where
  freeTypes Nothing  = S.empty
  freeTypes (Just x) = freeTypes x

instance (HasFreeTypes t) => HasFreeTypes (UserCon t u k) where
  freeTypes (UserCon name exist params nameRng rng vis doc)
    = freeTypes (map snd params)

instance (HasFreeTypes t) => HasFreeTypes (ValueBinder t e) where
  freeTypes vb
    = freeTypes (binderType vb)

instance HasFreeTypes (KUserType k) where
  freeTypes tp
    = case tp of
       TpQuan     quant tname tp rng  -> freeTypes tp
       TpQual     preds tp            -> freeTypes (tp:preds)
       TpFun      args eff tp rng     -> freeTypes (tp:eff:map snd args)
       TpApp      tp args range       -> S.union (freeTypes tp) (freeTypes args)
       TpVar      name range          -> S.empty
       TpCon      name range          -> S.singleton name
       TpParens   tp range            -> freeTypes tp
       TpAnn      tp kind             -> freeTypes tp


---------------------------------------------------------------------------
-- Binding groups in definitions
---------------------------------------------------------------------------
bindings :: Name -> [UserDefGroup] -> [UserDefGroup]
bindings modName defGroups
  = group defs deps
  where
    (defs, deps) = unzipWith (concat, unions) (map (bindingsDefGroup modName) defGroups)

unions ms
  = foldr (M.unionWith S.union) M.empty ms

bindingsDefGroup :: Name -> UserDefGroup -> ([UserDef], Deps)
bindingsDefGroup modName group
  = case group of
      DefNonRec def  -> let (newDef,deps) = dependencyDef modName def in ([newDef],deps)
      DefRec defs    -> dependencies modName defs


dependencies :: Name -> [UserDef] -> ([UserDef], Deps)
dependencies modName defs
  = (depDefs, deps)
  where
    defVars  = M.keys deps
    freeVars = S.unions (M.elems deps)
    (depDefs, deps)  = unzipWith (id,unions) (map (dependencyDef modName) defs)

dependencyDef :: Name -> UserDef -> (UserDef, Deps)
dependencyDef modName (Def binding range vis isVal defDoc)
  = (Def depBinding range vis isVal defDoc, deps)
  where
    (depBinding,deps) = dependencyBinding modName binding

dependencyBinding :: Name -> UserValueBinder UserExpr -> (UserValueBinder UserExpr, Deps)
dependencyBinding modName vb
  = -- trace ("dependency def: " ++ show (binderName vb) ++ ": " ++ show (S.toList freeVar)) $
    (vb{ binderExpr = depBody }, M.singleton ((binderName vb)) freeVar)
  where
    (depBody, freeVar) = dependencyExpr modName (binderExpr vb)


dependencyDefFv :: Name -> UserDef -> (UserDef, FreeVar)
dependencyDefFv modName def
  = let (depDef, deps) = dependencyDef modName def
    in (depDef, S.unions (M.elems deps))

dependencyDefGroupFv :: Name -> UserDefGroup -> ([UserDefGroup],FreeVar,S.NameSet)
dependencyDefGroupFv modName defGroup
  = (group defs deps, freeVar, names)
  where
    freeVar = S.difference (S.unions (M.elems deps)) names
    names   = S.fromList (M.keys deps)
    (defs,deps) = bindingsDefGroup modName defGroup

dependencyExpr :: Name -> UserExpr -> (UserExpr, FreeVar)
dependencyExpr modName expr
  = case expr of
      Lam binders body rng -> let (depBody,fv1) = dependencyExpr modName body
                                  (binders',fv2) = dependencyLamBinders modName fv1 binders
                                                   -- unzip (map dependencyLamBinder binders)
                              in (Lam binders' depBody rng, fv2) -- S.difference (S.unions (fv:fvs)) (S.fromList (map binderName binders')))
      Bind def body rng    -> let (depDef,fv1) = dependencyDefFv modName def
                                  (depBody,fv2) = dependencyExpr modName body
                              in (Bind depDef depBody rng, S.union fv1 (S.delete (defName def) fv2))
      Let group body rng   -> let (depGroups,fv1,names) = dependencyDefGroupFv modName group
                                  (depBody,fv2)   = dependencyExpr modName body
                              in (foldr (\g b -> Let g b rng)  depBody depGroups, S.union fv1 (S.difference fv2 names))
      Var name op rng      -> let uname = name -- if (qualifier name == modName) then unqualify name else name
                              in if isConstructorName name
                                  then (expr,S.fromList [uname,newCreatorName uname])
                                  else (expr,S.singleton uname)
      App fun nargs rng    -> let (fun', funvars) = dependencyExpr modName fun
                                  (argNames,args) = unzip nargs
                                  (args', argvars) = unzipWith (id,S.unions) (map (dependencyExpr modName) args)
                              in (App fun' (zip argNames args') rng, S.union funvars argvars)
      Ann expr t rng       -> let (depExpr,fv) = dependencyExpr modName expr
                              in (Ann depExpr t rng, fv)
      Case expr branches rng -> let (depExpr,fv1) = dependencyExpr modName expr
                                    (depBranches,fv2) = dependencyBranches dependencyBranch modName branches
                                in (Case depExpr depBranches rng, S.union fv1 fv2)
      Parens expr rng      -> let (depExpr, fv) = dependencyExpr modName expr
                              in (Parens depExpr rng, fv)
--      Con    name isop range -> (expr, S.empty)
      Lit    lit           -> (expr, S.empty)
      Handler shallow scoped eff pars reinit ret final ops hrng rng
        -> let (depRet,fv1)     = dependencyExpr modName ret
               (depBranches,fv2)= dependencyBranches dependencyHandlerBranch modName ops
               (depReinit,fv3)  = dependencyExpr modName reinit
               (depFinal,fv4)   = dependencyExpr modName final
               fvs              = S.difference (S.unions [fv1,fv2,fv3,fv4]) (S.fromList (map binderName pars))
           in (Handler shallow scoped eff pars depReinit depRet depFinal depBranches hrng rng,fvs)
      Inject tp body rng   -> let (depBody,fv) = dependencyExpr modName body
                              in (Inject tp depBody rng, fv)

dependencyBranches f modName branches
  = unzipWith (id,S.unions) (map (f modName) branches)


dependencyHandlerBranch :: Name -> UserHandlerBranch -> (UserHandlerBranch, FreeVar)
dependencyHandlerBranch modName hb@(HandlerBranch{ hbranchName=name, hbranchPars=pars, hbranchExpr=expr })
  = (hb{ hbranchExpr = depExpr }, S.insert uname (S.difference fvExpr (S.fromList (map getName pars))))
  where
    uname = if (qualifier name == modName) then unqualify name else name
    (depExpr, fvExpr)   = dependencyExpr modName expr


dependencyBranch :: Name -> UserBranch -> (UserBranch, FreeVar)
dependencyBranch modName (Branch pattern guard expr)
  = (Branch pattern depGuard depExpr, S.difference (S.union fvGuard fvExpr) (freeVar pattern))
  where
    (depGuard, fvGuard) = dependencyExpr modName guard
    (depExpr, fvExpr)   = dependencyExpr modName expr

dependencyLamBinders :: Name -> FreeVar -> [ValueBinder (Maybe UserType) (Maybe UserExpr)] -> ([ValueBinder (Maybe UserType) (Maybe UserExpr)], FreeVar)
dependencyLamBinders modName fv []
  = ([],fv)
dependencyLamBinders modName fv (binder:binders)
  = let (binders0,fv0) = dependencyLamBinders modName fv binders
        fv1            = S.delete (binderName binder) fv0
    in case binderExpr binder of
         Nothing -> (binder:binders0,fv1)
         Just expr -> let (expr',fv2) = dependencyExpr modName expr
                      in (binder{ binderExpr = Just expr' }:binders0, S.union fv1 fv2)

dependencyLamBinder :: Name -> ValueBinder (Maybe UserType) (Maybe UserExpr) -> (ValueBinder (Maybe UserType) (Maybe UserExpr), FreeVar)
dependencyLamBinder modName binder
  = case binderExpr binder of
      Nothing -> (binder,S.empty)
      Just expr -> let (expr',fv) = dependencyExpr modName expr
                   in (binder{ binderExpr = Just expr' }, fv)

---------------------------------------------------------------------------
-- Free variables
---------------------------------------------------------------------------
class HasFreeVar a where
  freeVar :: a -> FreeVar

instance HasFreeVar (Pattern t) where
  freeVar pat
    = case pat of
        PatWild range            -> S.empty
        PatCon  name args _ _    -> S.unions (map (freeVar . snd) args)
        PatVar  binder           -> S.singleton (getName binder)
        PatAnn  pat tp range     -> freeVar pat
        PatParens pat range      -> freeVar pat
        PatLit _                 -> S.empty

unzipWith (f,g) xs
  = let (x,y) = unzip xs in (f x, g y)

---------------------------------------------------------------------------
-- Dependencies
---------------------------------------------------------------------------

type Deps = M.NameMap S.NameSet
type FreeVar = S.NameSet

---------------------------------------------------------------------------
-- Topological sort
---------------------------------------------------------------------------
group :: [UserDef] -> Deps -> [UserDefGroup]
group defs deps
  = let -- get definition id's
        defVars  = S.fromList (M.keys deps)
        -- constrain to the current group of id's
        defDeps0 = M.map (\fvs -> S.intersection defVars fvs) deps
        -- determine strongly connected components
        defDeps   = [(id,S.toList fvs) | (id,fvs) <- M.toList defDeps0]
        defOrder0 = scc defDeps
        defOrder  = let (xs,ys) = partition noDeps defOrder0  -- no dependencies first
                        noDeps ids = case ids of
                                       [id] -> S.null (M.find id defDeps0)
                                       _    -> False
                        (xxs,xys) = partition isHidden xs    -- and hidden names first inside those
                        isHidden ids = case ids of
                                         [id] -> isHiddenName id
                                         _ -> False

                    in (xxs++xys++ys)
        -- create a map from definition id's to definitions.
        defMap   = M.fromListWith (\xs ys -> ys ++ xs) [(defName def,[def]) | def <- defs]
        -- create a definition group from a list of mutual recursive identifiers.
        makeGroup ids  = case ids of
                           [id] -> if S.member id (M.find id defDeps0)
                                    then [DefRec (M.find id defMap)]
                                    else map DefNonRec (M.find id defMap)
                           _    -> [DefRec [def | id <- ids, def <- M.find id defMap]]
    in -- trace ("trace: binding order: " ++ show defVars ++ "\n " ++ show (defDeps) ++ "\n " ++ show defOrder0 ++ "\n " ++ show defOrder) $
       concatMap makeGroup defOrder

groupTypeDefs :: [UserTypeDef] -> Deps -> [UserTypeDefGroup]
groupTypeDefs typeDefs deps
  = let -- get type names
        typeNames = S.fromList (M.keys deps)
        -- constrain to current group of id's
        typeDeps  = M.map (\fts -> S.intersection typeNames fts) deps
        -- determine strongly connected components
        typeOrder = scc [(id,S.toList fts) | (id,fts) <- M.toList typeDeps]
        -- create a map from type id's to type defs
        -- note: due to duplicate definitions (which are checked for during kind checking),
        --       we map to a list of possible defintions
        typeMap   = M.fromListWith (\xs ys -> ys ++ xs) [(typeDefName def,[def]) | def <- typeDefs]
        -- create a (recursive) definition group
        makeGroup ids = case ids of
                          [id] -> if S.member id (M.find id typeDeps)
                                   then [TypeDefRec (M.find id typeMap)]
                                   else map TypeDefNonRec (M.find id typeMap)
                          _    -> [TypeDefRec (concat [M.find id typeMap | id <- ids])]
     in -- trace ("Static.BindingGroups: typedef binding order: " ++ show typeOrder) $
        -- trace ("Static.BindingGropus: typedefs: " ++ show (map (tbinderName . typeDefBinder) typeDefs)) $
        concatMap makeGroup typeOrder


orderedPartition pred xs 
  = part xs ([],[])
  where
    part [] (ys,zs)
      = (reverse ys, reverse zs)
    part (x:xx) (ys,zs)  
      = if (pred x) then part xx (x:ys,zs) else part xx (ys,x:zs)

{-


{--------------------------------------------------------------------
  Group
--------------------------------------------------------------------}
ATTR Program TypeDefs TypeDef Def Defs Expr Pattern Lit
     Exprs Patterns Branch Branches
     UserType UserTypes UserKindScheme UserKind
     Externals External
     FixDefs FixDef
      [ || grouped : SELF ]


ATTR DefGroup  [ || grouped : DefGroups]
ATTR DefGroups [ || grouped USE {++} {[]}: DefGroups]

SEM DefGroup
  | DefNonRec lhs.grouped = [DefNonRec @def.grouped]
  | DefRec    lhs.grouped = group @defs.grouped @defs.deps


ATTR TypeDefGroup  [ || grouped : TypeDefGroups]
ATTR TypeDefGroups [ || grouped USE {++} {[]}: TypeDefGroups]

SEM TypeDefGroup
  | TypeDefGroup lhs.grouped = groupTypeDefs @typeDefs.grouped @typeDefs.deps


{
group :: Defs -> Deps -> DefGroups
group defs deps
  = let -- get definition id's
        defVars  = S.fromList (M.keys deps)
        -- constrain to the current group of id's
        defDeps  = M.map (\fvs -> S.intersection defVars fvs) deps
        -- determine strongly connected components
        defOrder = scc [(id,S.toList fvs) | (id,fvs) <- M.toList defDeps]
        -- create a map from definition id's to definitions.
        defMap   = M.fromList [(defName def,def) | def <- defs]
        -- create a definition group from a list of mutual recursive identifiers.
        makeGroup ids  = case ids of
                           [id] -> if S.member id (M.find id defDeps)
                                    then DefRec [M.find id defMap]
                                    else DefNonRec (M.find id defMap)
                           _    -> DefRec [M.find id defMap | id <- ids]
    in map makeGroup defOrder

groupTypeDefs :: TypeDefs -> Deps -> TypeDefGroups
groupTypeDefs typeDefs deps
  = let -- get type names
        typeNames = S.fromList (M.keys deps)
        -- constrain to current group of id's
        typeDeps  = M.map (\fts -> S.intersection typeNames fts) deps
        -- determine strongly connected components
        typeOrder = scc [(id,S.toList fts) | (id,fts) <- M.toList typeDeps]
        -- create a map from type id's to type defs
        typeMap   = M.fromList [(typeDefName def,def) | def <- typeDefs]
        -- create a (recursive) definition group
        makeGroup ids = TypeDefGroup [M.find id typeMap | id <- ids]
    in map makeGroup typeOrder
}

{--------------------------------------------------------------------
  Dependencies
--------------------------------------------------------------------}
{
type Deps = M.NameMap S.NameSet
}

ATTR TypeDef TypeDefs Def Defs [ || deps USE {`M.union`} {M.empty} : Deps ]

SEM Def
  | Def   lhs.deps  = M.single @name @body.freeVar

SEM TypeDef
  | Synonym lhs.deps = M.single @name @tp.freeTypes
  | Newtype lhs.deps = M.single @name @tp.freeTypes

{--------------------------------------------------------------------
  Free variables (and defined variables)
--------------------------------------------------------------------}
ATTR DefGroups DefGroup Defs Def Expr Exprs Branch Branches    [ || freeVar USE {`S.union`}{S.empty} : {S.NameSet} ]
ATTR DefGroups DefGroup Defs Def Pattern Patterns  [ || defVar USE {`S.union`}{S.empty} : {S.NameSet} ]

SEM DefGroup
  | DefRec    lhs.freeVar = S.difference @defs.freeVar @defs.defVar
  | DefNonRec lhs.freeVar = S.difference @def.freeVar @def.defVar     -- paranoia :-)

SEM Def
  | Def     lhs.defVar  = S.single @name

SEM Expr
  | Lam     lhs.freeVar = S.difference @body.freeVar @pat.defVar
  | Let     lhs.freeVar = S.union @defs.freeVar (S.difference @body.freeVar @defs.defVar)
  | Var     lhs.freeVar = S.single @name

SEM Branch
  | Branch  lhs.freeVar = S.difference (S.union @guard.freeVar @expr.freeVar) @pats.defVar

SEM Pattern
  | PatVar  lhs.defVar  = S.single @name

{--------------------------------------------------------------------------
  Free types
--------------------------------------------------------------------------}
ATTR UserTypes UserType [ || freeTypes USE {`S.union`} {S.empty} : {S.NameSet} ]

SEM UserType
  | TpCon       lhs.freeTypes = S.single @name
-}

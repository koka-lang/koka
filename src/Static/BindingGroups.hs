------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Static.BindingGroups( groupProgramBindings, groupTypeDefBindings, groupBindings, hasFreeVar ) where


import qualified Common.NameMap as M
import qualified Common.NameSet as S

import Data.List(partition,isPrefixOf)
import Lib.Scc( scc )  -- determine strongly connected components
import Common.Name
import Common.Range
import Common.Syntax
import Syntax.Syntax

import Lib.Trace (trace)

---------------------------------------------------------------------------
-- Program
---------------------------------------------------------------------------

groupProgramBindings :: UserProgram -> UserProgram
groupProgramBindings (Program source modName nameRange typeDefs defs imports externals fixDefs doc)
  = Program source modName nameRange
      (groupTypeDefBindings modName typeDefs)
      (groupBindings ({-toShortModuleName-} modName) defs) imports externals fixDefs doc

---------------------------------------------------------------------------
-- Binding groups in type definitions
---------------------------------------------------------------------------
groupTypeDefBindings :: Name -> [UserTypeDefGroup] -> [UserTypeDefGroup]
groupTypeDefBindings modName typeDefGroups
  = let (ds,extends) = partition isDefinition (flatten typeDefGroups)
    in groupTypeDefs ds (M.fromList (map (dependencyTypeDef modName) ds)) ++ (map TypeDefNonRec extends)
  where
    flatten groups
      = concatMap (\g -> case g of { TypeDefRec typeDefs -> typeDefs; TypeDefNonRec td -> [td]}) groups

    isDefinition td
      = case td of
          DataType binder args cons range vis sort ddef deff doc -> not (dataDefIsExtend ddef)
          _ -> True

dependencyTypeDef :: Name -> UserTypeDef -> (Name,S.NameSet)
dependencyTypeDef modName typeDef
  = case typeDef of
      Synonym binder args tp range vis doc    -> (typeDefName typeDef, S.map normalize (freeTypes tp))
      DataType binder args cons range vis sort ddef deff doc -> (typeDefName typeDef, S.map normalize (freeTypes cons))
  where
    normalize name = if qualifier name == modName then unqualify name else name

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
  freeTypes (UserCon name exist params result lazy nameRng rng vis doc)
    = freeTypes (map snd params) `S.union` freeTypes result

instance (HasFreeTypes t) => HasFreeTypes (ValueBinder t e) where
  freeTypes vb
    = freeTypes (binderType vb)

instance HasFreeTypes (KUserType k) where
  freeTypes tp
    = case tp of
       TpQuan     quant tname tp rng  -> freeTypes tp
       TpFun      args eff tp rng     -> freeTypes (tp:eff:map snd args)
       TpApp      tp args range       -> S.union (freeTypes tp) (freeTypes args)
       TpVar      name range          -> S.empty
       TpCon      name range          -> S.singleton name
       TpParens   tp range            -> freeTypes tp
       TpAnn      tp kind             -> freeTypes tp


---------------------------------------------------------------------------
-- Binding groups in definitions
---------------------------------------------------------------------------
groupBindings :: Name -> [DefGroup t] -> [DefGroup t]
groupBindings modName defGroups
  = -- trace ("groupBindings: " ++ show (concatMap defGroupNames defGroups)) $
    group defs deps
  where
    (defs, deps0) = unzipWith (concat, unions) (map (bindingsDefGroup modName) defGroups)
    deps          = M.mapWithKey extend deps0
    extend defName ndeps  = ndeps `S.union` (S.fromList (concatMap (extendName defName) (S.toList ndeps)))
    extendName defName n  = case M.lookup (unqualify n) extraDefs of
                              Just extra -> -- trace (" groupBindings: " ++ show defName ++ ": add extra defs: " ++ show n ++ " -> " ++ show extra) $
                                            extra
                              Nothing    -> []
    extraDefs     = extractDefGroups defGroups

    defGroupNames (DefNonRec def) = [defName def]
    defGroupNames (DefRec defs)   = map defName defs

type ExtraDeps = M.NameMap [Name]  -- maps base names (`eq`) to internally qualified names (`int/eq`,`char/eq`)

extractDefGroups:: [DefGroup t] -> ExtraDeps
extractDefGroups dgs
  = M.unionsWith (++) (map eedDefGroup dgs)

eedDefGroup (DefNonRec def) = eedDef def
eedDefGroup (DefRec defs)   = M.unionsWith (++) (map eedDef defs)

eedDef :: Def t -> ExtraDeps
eedDef def
  = let name = binderName (defBinder def)
    in if isLocallyQualified name
         then M.singleton (unqualifyFull name) [name]
         else M.empty


unions ms
  = foldr (M.unionWith S.union) M.empty ms

bindingsDefGroup :: Name -> DefGroup t -> ([Def t], Deps)
bindingsDefGroup modName group
  = case group of
      DefNonRec def  -> let (newDef,deps) = dependencyDef modName def in ([newDef],deps)
      DefRec defs    -> dependencies modName defs


dependencies :: Name -> [Def t] -> ([Def t], Deps)
dependencies modName defs
  = (depDefs, deps)
  where
    defVars  = M.keys deps
    freeVars = S.unions (M.elems deps)
    (depDefs, deps)  = unzipWith (id,unions) (map (dependencyDef modName) defs)

dependencyDef :: Name -> Def t -> (Def t, Deps)
dependencyDef modName def
  = (def{ defBinder = depBinding}, deps)
  where
    (depBinding,deps) = dependencyBinding modName (defBinder def)

dependencyBinding :: Name -> ValueBinder () (Expr t) -> (ValueBinder () (Expr t), Deps)
dependencyBinding modName vb
  = -- trace ("dependency def: " ++ show (binderName vb) ++ ": " ++ show (S.toList freeVar)) $
    (vb{ binderExpr = depBody }, M.singleton (binderName vb) freeVar)
  where
    (depBody, freeVar) = dependencyExpr modName (binderExpr vb)


dependencyDefFv :: Name -> Def t -> (Def t, FreeVar)
dependencyDefFv modName def
  = let (depDef, deps) = dependencyDef modName def
    in (depDef, S.unions (M.elems deps))

dependencyDefGroupFv :: Name -> DefGroup t -> ([DefGroup t],FreeVar,S.NameSet)
dependencyDefGroupFv modName defGroup
  = (group defs deps, freeVar, names)
  where
    freeVar = S.difference (S.unions (M.elems deps)) names
    names   = S.fromList (M.keys deps)
    (defs,deps) = bindingsDefGroup modName defGroup

dependencyExpr :: Name -> Expr t -> (Expr t, FreeVar)
dependencyExpr modName expr
  = case expr of
      Lam binders body tl rng -> let (depBody,fv1) = dependencyExpr modName body
                                     (binders',fv2) = dependencyLamBinders modName fv1 binders
                                                   -- unzip (map dependencyLamBinder binders)
                                 in -- trace ("binders: " ++ show (map binderName binders') ++ ": " ++ show (S.toList fv2)) $
                                    (Lam binders' depBody tl rng, fv2) -- S.difference fv2 (S.fromList (map binderName binders')))
      Bind def body rng    -> let (depDef,fv1) = dependencyDefFv modName def
                                  (depBody,fv2) = dependencyExpr modName body
                              in (Bind depDef depBody rng, S.union fv1 (S.delete (defName def) fv2))
      Let group body rng   -> let (depGroups,fv1,names) = dependencyDefGroupFv modName group
                                  (depBody,fv2)   = dependencyExpr modName body
                              in (foldr (\g b -> Let g b rng)  depBody depGroups, S.union fv1 (S.difference fv2 names))
      Var name op rng      -> let uname = unqualify name -- if (qualifier name == modName) then unqualify name else name
                              in if isConstructorName name
                                  then (expr,S.fromList [{-uname,-}newCreatorName uname])
                                  else {-let extra = case M.lookup (unqualifyFull name) of
                                                     Just extras -> extras
                                                     Nothing     -> []
                                       in-} (expr,S.fromList ([uname,toValueOperationName uname] {-++ extra-}))
      App fun nargs rng    -> let (fun', funvars) = dependencyExpr modName fun
                                  (argNames,args) = unzip nargs
                                  (args', argvars) = unzipWith (id,S.unions) (map (dependencyExpr modName) args)
                              in (App fun' (zip argNames args') rng, S.union funvars argvars)
      Ann expr t rng       -> let (depExpr,fv) = dependencyExpr modName expr
                              in (Ann depExpr t rng, fv)
      Case expr branches lazy rng -> let (depExpr,fv1) = dependencyExpr modName expr
                                         (depBranches,fv2) = dependencyBranches dependencyBranch modName branches
                                     in (Case depExpr depBranches lazy rng, S.union fv1 fv2)
      Parens expr name pre rng -> let (depExpr, fv) = dependencyExpr modName expr
                                  in (Parens depExpr name pre rng, fv)
--      Con    name isop range -> (expr, S.empty)
      Lit    lit           -> (expr, S.empty)
      Handler shallow scoped override allowMask eff pars reinit ret final ops hrng rng
        -> let (depRet,fv1)     = dependencyExprMaybe modName ret
               (depBranches,fv2)= dependencyBranches (dependencyHandlerBranch) modName ops
               (depReinit,fv3)  = dependencyExprMaybe modName reinit
               (depFinal,fv4)   = dependencyExprMaybe modName final
               fvs              = S.difference (S.unions [fv1,fv2,fv3,fv4]) (S.fromList (map binderName pars))
           in (Handler shallow scoped override allowMask eff pars depReinit depRet depFinal depBranches hrng rng,fvs)
      Inject tp body b rng -> let (depBody,fv) = dependencyExpr modName body
                              in (Inject tp depBody b rng, fv)

dependencyBranches f modName branches
  = unzipWith (id,S.unions) (map (f modName) branches)

dependencyExprMaybe modName mbExpr
  = case mbExpr of
      Nothing -> (Nothing,S.empty)
      Just expr -> let (depExpr,fv) = dependencyExpr modName expr
                   in (Just depExpr,fv)

dependencyHandlerBranch :: Name -> HandlerBranch t -> (HandlerBranch t, FreeVar)
dependencyHandlerBranch modName hb@(HandlerBranch{ hbranchName=name, hbranchPars=pars, hbranchExpr=expr })
  = (hb{ hbranchExpr = depExpr }, S.insert uname (S.difference fvExpr (S.fromList (map getName pars))))
  where
    uname = if (qualifier name == modName) then unqualify name else name
    (depExpr, fvExpr)   = dependencyExpr modName expr


dependencyBranch :: Name -> Branch t -> (Branch t, FreeVar)
dependencyBranch modName (Branch pattern guards)
  = let (depGuards, fvGuards) = unzipWith (id,S.unions) (map (dependencyGuard modName) guards)
    in  (Branch pattern depGuards, S.difference fvGuards (freeVar pattern))

dependencyGuard :: Name -> Guard t -> (Guard t, FreeVar)
dependencyGuard modName (Guard test expr)
  = (Guard depTest depExpr, S.union fvTest fvExpr)
  where
    (depTest, fvTest) = dependencyExpr modName test
    (depExpr, fvExpr) = dependencyExpr modName expr

dependencyLamBinders :: Name -> FreeVar -> [ValueBinder (Maybe t) (Maybe (Expr t))] -> ([ValueBinder (Maybe t) (Maybe (Expr t))], FreeVar)
dependencyLamBinders modName fv []
  = ([],fv)
dependencyLamBinders modName fv (binder:binders)
  = let (binders0,fv0) = dependencyLamBinders modName fv binders
        fv1            = S.delete (binderName binder) fv0
    in case binderExpr binder of
         Nothing -> (binder:binders0,fv1)
         Just expr -> let (expr',fv2) = dependencyExpr modName expr
                      in (binder{ binderExpr = Just expr' }:binders0, S.union fv1 fv2)

dependencyLamBinder :: Name -> ValueBinder (Maybe t) (Maybe (Expr t)) -> (ValueBinder (Maybe t) (Maybe (Expr t)), FreeVar)
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

instance HasFreeVar a => HasFreeVar [a] where
  freeVar xs = S.unions (map freeVar xs)

instance HasFreeVar a => HasFreeVar (Maybe a) where
  freeVar Nothing  = S.empty
  freeVar (Just x) = freeVar x

instance HasFreeVar (Pattern t) where
  freeVar pat
    = case pat of
        PatWild range            -> S.empty
        PatCon  name args _ _    -> S.unions (map (freeVar . snd) args)
        PatVar  binder           -> S.singleton (getName binder)
        PatAnn  pat tp range     -> freeVar pat
        PatParens pat range      -> freeVar pat
        PatLit _                 -> S.empty

instance HasFreeVar (Expr t) where
  freeVar expr = case expr of
      Lam binders body tl rng -> foldr (\b fv -> S.delete (binderName b) fv) (freeVar body) binders
      Bind def body rng    -> S.union (freeVar (defBody def)) (S.delete (defName def) (freeVar body))
      Let group body rng   -> let (fv,bound) = freeBoundVar group
                              in S.union fv (S.difference (freeVar body) bound)
      Var name op rng      -> if isConstructorName name
                                then S.empty
                                else S.singleton name
      App fun nargs rng    -> freeVar (fun:map snd nargs)
      Ann expr t rng       -> freeVar expr
      Case expr bs _ rng     -> S.union (freeVar expr) (freeVar bs)
      Parens expr name pre rng -> freeVar expr
      Lit    lit           -> S.empty
      Inject tp body b rng -> freeVar body
      Handler shallow scoped override allowMask eff pars reinit ret final ops hrng rng
        -> let fvs = S.unions [freeVar ret, freeVar ops, freeVar reinit, freeVar final]
           in S.difference fvs (S.fromList (map binderName pars))


instance HasFreeVar (HandlerBranch t) where
  freeVar (HandlerBranch{ hbranchName=name, hbranchPars=pars, hbranchExpr=expr })
    = S.difference (freeVar expr) (S.fromList (map getName pars))

instance HasFreeVar (Branch t) where
  freeVar (Branch pattern guards)
    = S.difference (freeVar guards) (freeVar pattern)

instance HasFreeVar (Guard t) where
  freeVar (Guard test expr)
    = S.union (freeVar test) (freeVar expr)

freeBoundVar :: DefGroup t -> (FreeVar,FreeVar)
freeBoundVar (DefNonRec def)
  = (S.singleton (defName def), freeVar (defBody def))
freeBoundVar (DefRec defs)
  = let bound = S.fromList (map defName defs)
        free  = freeVar (map defBody defs)
    in (bound, S.difference free bound)


hasFreeVar :: Expr t -> Name -> Bool
hasFreeVar expr name
  = S.member name (freeVar expr)

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
group :: [Def t] -> Deps -> [DefGroup t]
group defs deps
  = let -- get definition id's
        defVars  = S.fromList (M.keys deps)

        -- constrain to the current group of id's
        defDeps  = M.map (\fvs -> S.intersection defVars fvs) deps

        -- determine strongly connected components (`scc`) in order of definitions
        defDepsList = -- [(id,S.toList fvs) | (id,fvs) <- M.toList defDeps]
                      concatMap (\def -> case M.lookup (defName def) defDeps of
                                           Just fvs -> [(def,S.toList fvs)]
                                           Nothing  -> []) defs
        defOrderScc = scc [(defName def,defdeps) | (def,defdeps) <- defDepsList]

        -- create a map from definition id's to definitions.
        defMap      = M.fromListWith (\xs ys -> ys ++ xs) [(defName def,[def]) | def <- defs]

        -- try to maintain original source order as much as possible
        -- reorder the `defOrderScc` according to the (earliest) source line (of a recursive group)
        -- without violating explicit dependencies. Regardless of the particular dependency order
        -- returned by the `scc` algorithm (as dependency order is partial), this should always
        -- result in the same canonical order (essentially making the ordering total using source lines
        -- as a tie breaker). This is important since at this point in the compilation we cannot
        -- yet resolve names fully:
        -- 1. We may overestimate dependencies: a name like `foo` may refer to a locally qualified
        --    name `int/foo` or `bool/foo` and we don't know until type checking and assume
        --    it might refer to either one.
        -- 2. We may not see all dependencies: implicit parameters are resolved at type checking
        --    and may introduce dependencies that we cannot determine yet. As a programmer, we
        --    can ensure the correct order by putting such implicit dependencies first in the source.
        lineOf ids  = let getLine id = map (posLine . rangeStart . defRange) (M.find id defMap)
                      in case concatMap getLine ids of
                           []    -> 0
                           lines -> minimum lines
        defOrder    = reverse $            -- and reverse to go from least to most dependent
                      foldl insert [] $    -- fold from least to most dependent, creating a list from most to least dependent
                      defOrderScc          -- in dependency order (from least to most)
                    where
                      insert :: [[Name]] -> [Name] -> [[Name]]
                      insert rdefs idgrp   -- insert `idgrp` into the reverse definitions (from most to least dependent)
                        = let n           = lineOf idgrp
                              idgrpDeps   = S.unions (map (\id -> M.find id defDeps) idgrp)
                              goesAfter x = (n < lineOf x) &&                           -- don't go beyond source order
                                            not (any (\id -> S.member id idgrpDeps) x)  -- and not beyond an actual dependency
                          in case span goesAfter rdefs of
                               (pre,post) -> -- trace ("insert: " ++ show (idgrp,idgrpDeps,n) ++ " into " ++ show ([(p,lineOf p) | p <- pre],[(p,lineOf p) | p <- post])) $
                                             pre ++ (idgrp : post)

        -- create a definition group from a list of mutual recursive identifiers.
        -- split out `wrong/...` id's always into separate groups (as these usually contain errors)
        makeGroup ids0 = let (wids,gids) = partition (isInWrongNameSpace) ids0
                         in concatMap (\wid -> group [wid]) wids ++ group gids
                       where
                         group ids = case ids of
                                      [id] -> if S.member id (M.find id defDeps)
                                              then [DefRec (M.find id defMap)]
                                              else map DefNonRec (M.find id defMap)
                                      _    ->  [DefRec [def | id <- ids, def <- M.find id defMap]]
        finalGroup     = concatMap makeGroup defOrder
    in --trace ("groups:\n"
       --  ++ unlines [show (defName def) ++ ": line " ++ show (posLine (rangeStart (defRange def))) ++ ": " ++ show defdeps | (def,defdeps) <- defDepsList]
       --  ++ "\ninitial order: " ++ show defOrderScc ++ "\n\nfinal order: " ++ show defOrder) $
       finalGroup


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
     in -- trace("Static.BindingGroups: typedef binding order: " ++ show typeOrder) $
        -- trace("Static.BindingGroups: typedef deps: " ++ show deps) $
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
As a tribute to Doaitse Swierstra, let's leave in this code which
was from a time when we used the Attribute Grammar system from
Doaitse developed at the University of Utrecht.

{--------------------------------------------------------------------
  Group
--------------------------------------------------------------------}
ATTR Program TypeDefs TypeDef Def Defs Expr Pattern Lit
     Exprs Patterns Branch Branches
     t UserTypes UserKindScheme UserKind
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
  | TypeDefGroup lhs.grouped = groupTypeDefBindings @typeDefs.grouped @typeDefs.deps


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

groupTypeDefBindings :: TypeDefs -> Deps -> TypeDefGroups
groupTypeDefBindings typeDefs deps
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
ATTR UserTypes t [ || freeTypes USE {`S.union`} {S.empty} : {S.NameSet} ]

SEM t
  | TpCon       lhs.freeTypes = S.single @name
-}

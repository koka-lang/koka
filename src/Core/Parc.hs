-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
module Core.Parc ( parcCore ) where

import Lib.Trace (trace)
import Control.Applicative hiding (empty)
import Control.Monad
import Data.List ( intersperse, partition )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.Char
-- import Data.Maybe
-- import Data.Monoid ( mappend )
import qualified Data.Set as S

import Kind.Kind
import Kind.Newtypes
import Type.Type
import Type.TypeVar
import Type.Kind( getKind )
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.Name
import Common.Range
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty
import Core.CoreVar

import Platform.Runtime (unsafePerformIO)
import qualified System.Environment as Sys

{-# NOINLINE enabled #-}
enabled :: Bool
enabled = unsafePerformIO $ do
  e <- Sys.lookupEnv "KK_PARC"
  case e of
    Nothing -> return False
    Just val -> return $ map toLower val `elem` ["1", "on", "yes", "true", "y", "t"]

--------------------------------------------------------------------------
-- Local typeclass for convenience
--------------------------------------------------------------------------

class ParcName a where
  parcGetName :: a -> Name

instance ParcName Name where
  parcGetName = id

instance ParcName TName where
  parcGetName = getName

--------------------------------------------------------------------------
-- Reference count transformation
--------------------------------------------------------------------------

parcCore :: Pretty.Env -> Newtypes -> Int -> Core -> (Core,Int)
parcCore penv newtypes u core
  | not enabled = (core, u)
  | otherwise   = let (defs',u1) = runParc penv newtypes u (parcDefGroups True (coreProgDefs core))
                  in (core{ coreProgDefs  = defs' }, u1)

{--------------------------------------------------------------------------
  definition groups
--------------------------------------------------------------------------}

parcDefGroups :: Bool -> DefGroups -> Parc DefGroups
parcDefGroups topLevel defGroups
  = reverseMapM (parcDefGroup topLevel) defGroups

parcDefGroup :: Bool -> DefGroup -> Parc DefGroup
parcDefGroup topLevel dg
  = case dg of
      DefRec defs    -> do defs' <- reverseMapM (parcDef topLevel) defs
                           return (DefRec defs')
      DefNonRec def  -> do def' <- parcDef topLevel def
                           return (DefNonRec def')

parcDef :: Bool -> Def -> Parc Def
parcDef topLevel def
  = (if topLevel then isolated else id) $
    do parcTraceDoc (\penv -> prettyDef (penv{Pretty.coreShowDef=True}) def )
       expr <- parcExpr (defExpr def)
       let result = def{defExpr=expr}
       parcTraceDoc (\penv -> prettyDef (penv{Pretty.coreShowDef=True}) result)
       return result
  where
    isolated action
      = do (x,inuse) <- isolateInUse action
           -- assertion ("Core.Parc.parcDef: inuse not empty: " ++ show (defName def))  (S.null inuse) $
           return x


{- try:
:set --target=c
fun f(x: list<a>): list<a> { x + x }

fun f(x: list<int>): list<int> { return [] }

fun f(x : list<int>) : list<int> { val y = match(x) { Cons(_, _) -> x Nil -> [] } return y }

fun f(x : int) : int { val y = match(x+1) { 2 -> 3 ; _ -> x } return y }

-}
parcExpr :: Expr -> Parc Expr
parcExpr expr
  = do parcTraceDoc (\penv -> prettyExpr penv expr)
       case expr of
         TypeLam tpars body
           -> do body' <- parcExpr body
                 return $ TypeLam tpars body'
         TypeApp body targs
           -> do body' <- parcExpr body
                 return $ TypeApp body targs
         Lam pars eff body
           -> do let freeBody = freeLocals body
                     parsSet  = S.fromList pars
                     free     = tnamesList $ S.difference freeBody parsSet      
                     parsUnused = tnamesList $ S.difference parsSet freeBody                     
                 freeDups <- dupTNames (zip free (repeat InfoNone))
                 parDrops <- mapM genDrop parsUnused
                 body' <- withIsolated $
                          withOwned (tnamesList freeBody) $
                          parcExpr body                                                     
                 return $ maybeStats freeDups (Lam pars eff (maybeStats parDrops body'))
         Var tname info
           -> do mbDup <- dupTName (tname,info)
                 case mbDup of
                   Just dup -> return dup
                   Nothing  -> return expr
         App fn args
           -> do args' <- reverseMapM parcExpr args
                 fn'   <- parcExpr fn
                 return $ App fn' args'
         Lit _
           -> return expr
         Con ctor repr
           -> do return expr
         Let dgs body
           -> do body' <- parcExpr body
                 dgs' <- parcDefGroups False dgs
                 return $ Let dgs' body'
         -- all variable scrutinees?
         Case exprs branches  | all isExprVar exprs
           ->  do xbranches' <- reverseMapM parcBranch branches   
                  let (branches',inUses) = unzip xbranches'
                  setInUse (S.unions inUses)
                  _ <- reverseMapM parcExpr exprs  -- process, but don't use
                  return (Case exprs branches')  -- exprs is all borrowed vars (should not dup)
         -- bind scrutinees first
         Case exprs branches
           -> do (vexprs,dgs) <- caseExpandExprs exprs
                 assertion ("Core.Parc.parcExpr.Case") (not (null dgs)) $
                   parcExpr (makeLet dgs (Case vexprs branches))

-- Generate variable names for scrutinee expressions
caseExpandExprs :: [Expr] -> Parc ([Expr], DefGroups)
caseExpandExprs [] = return ([], [])
caseExpandExprs (x:xs)
  = case x of
      Var _ _ -> do (xs', defs) <- caseExpandExprs xs
                    return (x:xs', defs)
      _ -> do name <- uniqueName "match"
              let def = DefNonRec (makeDef name x)
              let var = Var (TName name (typeOf x)) InfoNone
              (xs', defs) <- caseExpandExprs xs
              return (var:xs', def:defs)

isExprVar (Var{})  = True
isExprVar _        = False

{-

match(xs) {
  Cons(y,yy)         | foo(yy) -> e1
  Cons(y,Cons(yy,_)) | bar(y)  -> e2
  _ -> 
}

-}


parcBranch :: Branch -> Parc (Branch,InUse)
parcBranch b@(Branch patterns guards)
  = do let pvars = (bv patterns)
       (guards',inUse) <- fmap unzip $ reverseMapM (parcGuard pvars) guards
       return (Branch patterns guards', S.unions inUse)
       
parcGuard :: TNames -> Guard -> Parc (Guard,InUse)
parcGuard pvars (Guard test expr) 
  = isolateInUse $
    do -- body
       contInUse <- getInUse  -- everything in use in our continuation after the match           
       let free      = freeLocals expr
           pvarInUse = tnamesList $ S.intersection pvars free
       owned    <- getOwned      
       let isUsed tname = S.member (getName tname) (S.union (S.map getName free) contInUse)
           (stillOwned,dropOwned)  = partition isUsed owned
       drops    <- mapM genDrop dropOwned
       pvarDups <- mapM genDup pvarInUse
       expr'    <- withOwned (stillOwned ++ pvarInUse) $
                   parcExpr expr
  
       -- test: treat all variables as if in-use so no reference counts change
       let freeTest = freeLocals test
       (test',_) <- isolateInUse $ do setInUse (S.map getName freeTest)
                                      parcExpr test                
                      
       return (Guard test' (maybeStats (pvarDups ++ drops) expr'))
       
        


addUniqueNames :: Expr -> Parc Expr
addUniqueNames e@(Case exprs branches)
  = do branches' <- mapM addUniqueNamesToBranch branches
       return $ e { caseBranches = branches' }
addUniqueNames _ = error "addUniqueNames only applies to Case exprs"

addUniqueNamesToBranch :: Branch -> Parc Branch
addUniqueNamesToBranch branch
  = do patterns' <- mapM addUniqueNamesToPattern (branchPatterns branch)
       return (branch { branchPatterns = patterns' })

{-
data Pattern
  = PatCon{ patConName :: TName,        ** names the constructor. full signature. not good to use since it can contain existential types
            patConPatterns:: [Pattern], -- sub-patterns. fully materialized to match arity.
            patConRepr :: ConRepr,      -- representation of ctor in backend. not needed
            patTypeArgs :: [Type],      -- can be zipped with patConPatterns above
            patExists :: [TypeVar],     -- closed under existentials here
            patTypeRes :: Type,         -- result type
            patConInfo :: ConInfo }     -- all other info. not needed
  | PatVar{ patName :: TName,           ** name/type of variable
            patPattern :: Pattern }     -- sub-pattern
  | PatLit{ patLit :: Lit }             ** just a literal (below)
  | PatWild                             ** can be top-level. need types of scrutinees

data Lit =
    LitInt    Integer
  | LitFloat  Double
  | LitChar   Char
  | LitString String

Use typeOf for exprs/literals
-}

addUniqueNamesToPattern :: Pattern -> Parc Pattern
addUniqueNamesToPattern = return -- TODO: need typed names here


dupTNames :: [(TName,VarInfo)] -> Parc [Maybe Expr]
dupTNames tnames
  = mapM dupTName tnames

dupTName :: (TName,VarInfo) -> Parc (Maybe Expr)
dupTName (tname,InfoNone)
  = do needsDup <- isInUse tname
       if needsDup
        then genDup tname
        else do addInUse tname
                return Nothing

dupTName (tname,_)
  = return Nothing

reverseMapM :: Monad m => (a -> m b) -> [a] -> m [b]
reverseMapM action args =
  do args' <- mapM action (reverse args)
     return $ reverse args'


maybeStats :: [Maybe Expr] -> Expr -> Expr
maybeStats xs expr
  = makeStats (catMaybes xs ++ [expr])

-- Generate a "drop match"
genDropMatch :: TName -> [TName] -> [TName] -> Parc Expr
genDropMatch con dups drops
  = do xdrops <- mapM genDrop drops
       xdups  <- mapM genDup dups
       cdrop  <- genDrop con
       return $ makeIfExpr (genIsUnique con)
                  (makeStats (catMaybes xdrops ++ [genFree con]))
                  (makeStats (catMaybes (xdups ++ [cdrop])))

genKeepMatch :: TName -> [TName] -> [TName] -> Parc Expr
genKeepMatch con dups drops
  = do xdups  <- mapM genDup dups
       cdrop  <- genDrop con
       return $ makeStats (catMaybes (xdups ++ [cdrop]))

-- Generate a "reuse match"
genReuseMatch :: TName -> [TName] -> [TName] -> Parc Expr
genReuseMatch con dups drops
 = do xdrops <- mapM genDrop drops
      xdups  <- mapM genDup dups
      cdrop  <- genDrop con
      return $ makeIfExpr (genIsUnique con)
                 (makeStats (catMaybes xdrops ++ [genReuse con]))
                 (makeStats (catMaybes (xdups ++ [cdrop]) ++ [genNoReuse]))

-- Generate a test if a (locally bound) name is unique
genIsUnique :: TName -> Expr
genIsUnique tname
  = App (Var (TName nameIsUnique funTp) (InfoExternal [(C, "constructor_is_unique(#1)")]))
        [Var tname InfoNone]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeBool


-- Generate a free of a constructor
genFree :: TName -> Expr
genFree tname
  = App (Var (TName nameFree funTp) (InfoExternal [(C, "constructor_free(#1)")]))
        [Var tname InfoNone]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeUnit

-- Generate a reuse of a constructor
genReuse :: TName -> Expr
genReuse tname
  = App (Var (TName nameReuse funTp) (InfoExternal [(C, "constructor_reuse(#1)")]))
        [Var tname InfoNone]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeReuse


-- Generate a reuse of a constructor
genNoReuse :: Expr
genNoReuse
  = App (Var (TName nameNoReuse funTp) (InfoArity 0 0)) []
  where
    funTp = TFun [] typeTotal typeReuse

genDup  tname = genDupDrop True tname
genDrop tname = genDupDrop False tname

-- Generate a dup/drop over a given (locally bound) name
-- May return Nothing if the type never needs a dup/drop (like an `int` or `bool`)
genDupDrop :: Bool -> TName -> Parc (Maybe Expr)
genDupDrop isDup tname
  = do let tp = typeOf tname
       mbRepr <- getDataDefRepr tp
       case mbRepr of
         Just (dataDef,dataRepr)
           -> case dataDef of
                 DataDefValue _ 0 -> return Nothing    -- no need to dup/drop a value type with no pointer fields (like int)
                 _ -> return (Just (App (dupDropFun isDup tp) [Var tname InfoNone]))
         _ -> return Nothing


dupFun tp  = dupDropFun True tp
dropFun tp = dupDropFun False tp

dupDropFun isDup tp
  = Var (TName name coerceTp) (InfoExternal [(C, (if isDup then "dup" else "drop") ++ "(#1)")])
  where
    name = if isDup then nameDup else nameDrop
    coerceTp = TFun [(nameNil,tp)] typeTotal (if isDup then tp else typeUnit)


{--------------------------------------------------------------------------
 Parc monad
--------------------------------------------------------------------------}
newtype Parc a = Parc (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env,
                newtypes  :: Newtypes,
                owned     :: Owned
              }

type Owned = [TName]  -- = S.Set TName
type InUse = S.Set Name

data ReuseInfo = ReuseInfo{ reuseRepr :: ConRepr, reuseCon :: ConInfo }

data State = State{ uniq :: Int, inuse :: InUse, reuse :: [(Name,ReuseInfo)] }

data Result a = Ok a State

runParc :: Pretty.Env -> Newtypes -> Int -> Parc a -> (a,Int)
runParc penv newtypes u (Parc c)
 = case c (Env [] penv newtypes []) (State u S.empty []) of
     Ok x st -> (x,uniq st)

instance Functor Parc where
 fmap f (Parc c)  = Parc (\env st -> case c env st of
                                       Ok x st' -> Ok (f x) st')

instance Applicative Parc where
 pure  = return
 (<*>) = ap

instance Monad Parc where
 return x       = Parc (\env st -> Ok x st)
 (Parc c) >>= f = Parc (\env st -> case c env st of
                                     Ok x st' -> case f x of
                                                   Parc d -> case d env st' of
                                                               Ok x' st'' -> Ok x' st'')

instance HasUnique Parc where
 updateUnique f = Parc (\env st -> Ok (uniq st) st{ uniq = f (uniq st) })
 setUnique  i   = Parc (\env st -> Ok () st{ uniq = i})

withEnv :: (Env -> Env) -> Parc a -> Parc a
withEnv f (Parc c)
 = Parc (\env st -> c (f env) st)

getEnv :: Parc Env
getEnv
 = Parc (\env st -> Ok env st)

updateSt :: (State -> State) -> Parc State
updateSt f
 = Parc (\env st -> Ok st (f st))

getSt :: Parc State
getSt
  = Parc (\env st -> Ok st st)


-----------------------
-- owned names

withOwned :: [TName] -> Parc a -> Parc a
withOwned tnames action
  = withEnv (\env -> env{ owned = {- tnamesInsertAll (owned env) -} tnames}) action

getOwned :: Parc [TName]
getOwned
  = do env <- getEnv
       return (owned env)

ownedAndNotUsed :: Parc [TName]
ownedAndNotUsed
  = do owned <- getOwned
       used  <- getInUse
       let isUsed tname = S.member (getName tname) used
       return $ filter (not . isUsed) owned


-----------------------
-- in-use sets

addInUse :: ParcName p => p -> Parc ()
addInUse name'
  = do updateSt (\st -> st{ inuse = S.insert name (inuse st)})
       return ()
    where name = parcGetName name'

isInUse :: ParcName p => p -> Parc Bool
isInUse name'
  = do st <- getSt
       return (S.member name (inuse st))
    where name = parcGetName name'

dropInUse :: ParcName p => p -> Parc ()
dropInUse name'
  = do updateSt (\st -> st{ inuse = S.delete name (inuse st)})
       return ()
    where name = parcGetName name'

getInUse :: Parc InUse
getInUse = inuse <$> getSt

setInUse :: InUse -> Parc ()
setInUse inuse0
 = do updateSt (\st -> st{ inuse = inuse0 })
      return ()

-- TODO: also save/restore the reuseInfo?
isolateInUse :: Parc a -> Parc (a, InUse)
isolateInUse action
  = do inuse0 <- getInUse
       x <- action
       st1 <- updateSt (\st -> st{ inuse = inuse0 })  -- restore
       return (x,inuse st1)

-- branches are isolated from each other
isolateBranches :: [Parc a] -> Parc [a]
isolateBranches branches
  = do xs0 <- reverseMapM isolateInUse branches
       let (xs, inuses) = unzip xs0
           inuse = S.unions inuses
       setInUse inuse
       return xs

-- for a lambda, fully isolated
withIsolated :: Parc a -> Parc a   
withIsolated action
  = fmap fst $
    isolateInUse $
    withEnv (\env -> env{ owned = [] }) $
    do updateSt (\st -> st{ inuse = S.empty, reuse = [] })
       action



-----------------------
-- drop statement
{-
genDropStmt :: TName -> Expr -> Parc Expr
genDropStmt tn e
  = do r <- uniqueName "drop"
       dr <- genDrop tn
       let def = makeTDef tn
       return $ makeLet []

makeExprSeq :: [Expr] -> Parc Expr
makeExprSeq [] = error "empty list provided to makeExprSeq"
makeExprSeq [x] = return x
makeExprSeq (x:xs)
  =
-}
-----------------------
-- reuse

withReuse :: ReuseInfo -> Parc a -> Parc (a, Maybe Name)
withReuse reuseInfo action
  = do r <- uniqueName "reuse"
       updateSt (\st -> st{ reuse = (r,reuseInfo): reuse st })
       x <- action
       st0 <- updateSt (\st -> st{ reuse = filter (\(r',_) -> r /= r') (reuse st) })
       let isReused = not (any (\(r',_) -> r == r') (reuse st0))
       return (x, if isReused then Just r else Nothing)

tryReuse :: TName -> ConRepr -> Parc (Maybe Name)
tryReuse conName conRepr
  = return Nothing -- TODO: lookup if we can reuse, and if so, remove the name from the ReuseInfo


-----------------------
-- tracing

withCurrentDef :: Def -> Parc a -> Parc a
withCurrentDef def action
 = -- trace ("Parcing: " ++ show (defName def)) $
   withEnv (\env -> env{currentDef = def:currentDef env}) $
   action

parcTraceDoc :: (Pretty.Env -> Doc) -> Parc ()
parcTraceDoc f
 = do env <- getEnv
      parcTrace (show (f (prettyEnv env)))

parcTrace :: String -> Parc ()
parcTrace msg
 = do env <- getEnv
      trace ("Core.Parc: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()


----------------
getNewtypes :: Parc Newtypes
getNewtypes = newtypes <$> getEnv

getDataDefRepr :: Type -> Parc (Maybe (DataDef,DataRepr))
getDataDefRepr tp
  = case extractDataDefType tp of
      Nothing -> return (Just (DataDefNormal,DataNormal))
      Just name -> do newtypes <- getNewtypes
                      case newtypesLookupAny name newtypes of
                        Nothing -> failure $ "Core.Parc.getDataInfo: cannot find type: " ++ show name
                        Just di -> return (Just (dataInfoDef di, fst (getDataRepr di)))

extractDataDefType tp
  = case expandSyn tp of
      TApp t _      -> extractDataDefType t
      TForall _ _ t -> extractDataDefType t
      TCon tc       -> Just (typeConName tc)
      _             -> Nothing

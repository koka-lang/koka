-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Make local variables unique
-}
-----------------------------------------------------------------------------

module Core.Uniquefy ( uniquefy
                     , uniquefyDefGroup {- used for divergence analysis -}
                     ) where

import Control.Monad
import Control.Applicative
import Common.Name
import qualified Common.NameSet as S
import qualified Common.NameMap as M
import Core.Core


type Locals = S.NameSet
type Renaming = M.NameMap Name

data Un a  = Un (State -> (a,State))
data State = St{ locals :: Locals, renaming :: Renaming }

instance Functor Un where
  fmap f (Un u)  = Un (\st -> case u st of
                                (x,st1) -> (f x,st1))

instance Applicative Un where
  pure  = return
  (<*>) = ap

instance Monad Un where
  return x  = Un (\st -> (x,st))
  (Un u) >>= f  = Un (\st0 -> case u st0 of (x,st1) -> case f x of Un u1 -> u1 st1)

updateSt f
  = Un (\st -> (st,f st))

getSt = updateSt id
setSt st  = updateSt (const st)

fullLocalized u
  = do locals <- getLocals
       x <- localized u
       setLocals locals
       return x

localized u
  = do ren <- getRenaming
       x <- u
       setRenaming ren
       return x

getLocals = fmap locals getSt
getRenaming = fmap renaming getSt
setLocals l = updateSt (\st -> st{ locals = l })
setRenaming r = updateSt (\st -> st{ renaming = r })

runUn (Un u)
  = fst (u (St S.empty M.empty))



uniquefy :: Core -> Core
uniquefy core
  = -- core{ coreProgDefs = map (uniquefyDefGroup) (coreProgDefs core) }
    runUn $
    do locals <- getLocals
       let toplevelDefs = map defName (flattenDefGroups (coreProgDefs core))
       setLocals (foldr (\name locs -> S.insert (unqualify name) locs) locals toplevelDefs)
       defGroups <- mapM (fullLocalized . uniquefyDG) (coreProgDefs core)
       return (core{coreProgDefs = defGroups})
  where
    uniquefyDG (DefNonRec def)
      = fmap DefNonRec $ 
        do expr <- uniquefyExpr (defExpr def)
           return def{ defExpr = expr }
    uniquefyDG (DefRec defs)
      = fmap DefRec $ 
        do exprs <- mapM (uniquefyExpr . defExpr) defs
           return [def{ defExpr = expr } | (def,expr) <- zip defs exprs]


uniquefyDefGroup :: DefGroup -> DefGroup
uniquefyDefGroup defgroup
  = runUn $
    case defgroup of
      DefNonRec def
        -> fmap DefNonRec $ uniquefyDef def
      DefRec defs
        -> fmap DefRec $ 
           do locals <- getLocals
              setLocals (foldr (\name locs -> S.insert name locs) locals (map defName defs))
              exprs <- localized $ mapM (uniquefyExpr . defExpr) defs
              return [def{ defExpr = expr } | (def,expr) <- zip defs exprs]



uniquefyInnerDefGroup :: DefGroup -> Un DefGroup
uniquefyInnerDefGroup dg
  = case dg of
      DefNonRec def 
        -> fmap DefNonRec $ uniquefyDef def
      DefRec defs   
        -> fmap DefRec $ uniquefyRecDefs defs

uniquefyRecDefs :: [Def] -> Un [Def]
uniquefyRecDefs defs
  = do names <- mapM (uniquefyName . defName) defs
       exprs <- localized $ mapM (uniquefyExpr . defExpr) defs
       return [def{ defExpr = expr, defName = name } | (def,(name,expr)) <- zip defs (zip names exprs)]

uniquefyDef :: Def -> Un Def
uniquefyDef def
  = do expr1 <- localized $ uniquefyExpr (defExpr def)
       name1 <- uniquefyName (defName def)  -- works because we can't have overloaded recursive identifiers in an inner scope
       return (def{ defName = name1, defExpr = expr1 })

uniquefyExpr :: Expr -> Un Expr
uniquefyExpr expr
  = case expr of
      Lam tnames eff expr-> localized $
                            do tnames1 <- mapM uniquefyTName tnames
                               expr1   <- uniquefyExpr expr                               
                               return (Lam tnames1 eff expr1)
      Var tname info    -> do renaming <- getRenaming
                              case M.lookup (getName tname) renaming of
                                Just name -> return $ Var (TName name (typeOf tname)) info
                                Nothing   -> return expr
      App f args        -> do f1 <- uniquefyExpr f
                              args1 <- mapM uniquefyExpr args
                              return (App f1 args1)
      TypeLam tvs expr  -> do expr1 <- uniquefyExpr expr
                              return (TypeLam tvs expr1)
      TypeApp expr tps  -> do expr1 <- uniquefyExpr expr
                              return (TypeApp expr1 tps)
      Con tname repr    -> return expr
      Lit lit           -> return expr
      Let defGroups expr  -> do defGroups1 <- mapM uniquefyInnerDefGroup defGroups
                                expr1 <- uniquefyExpr expr
                                return (Let defGroups1 expr1)
      Case exprs branches 
        -> do exprs1 <- mapM uniquefyExpr exprs 
              branches1 <- localized $ mapM uniquefyBranch branches
              return (Case exprs1 branches1 )


uniquefyBranch (Branch patterns guardExprs)
  = do patterns1   <- mapM uniquefyPattern patterns
       guardExprs1 <- mapM uniquefyGuard guardExprs
       return (Branch patterns1 guardExprs1)

uniquefyGuard (Guard test expr)
  = do t <- uniquefyExpr test
       e <- uniquefyExpr expr
       return (Guard t e)

uniquefyPattern pattern
  = case pattern of
      PatWild -> return pattern
      PatVar tname pat -> do tname' <- uniquefyTName tname
                             pat'   <- uniquefyPattern pat
                             return (PatVar tname' pat')
      PatCon { patConPatterns = patterns }
        -> do patterns1 <- mapM uniquefyPattern patterns
              return pattern{ patConPatterns = patterns1 }

uniquefyTName :: TName -> Un TName
uniquefyTName tname
  = do name1 <- uniquefyName (getName tname)
       return (TName name1 (typeOf tname))

uniquefyName :: Name -> Un Name
uniquefyName name
  = do locals <- getLocals
       if (S.member name locals)
        then do renaming <- getRenaming
                let name1 = findUniqueName 0 name locals
                    locals1 = S.insert name1 locals
                    renaming1 = M.insert name name1 renaming
                setLocals locals1
                setRenaming renaming1
                return name1
        else do setLocals (S.insert name locals)
                return name

findUniqueName i name locals
  = let name1 = qualify (qualifier name) (newName (nameId name ++ show i))
    in if (S.member name1 locals)
        then findUniqueName (i+1) name locals
        else name1

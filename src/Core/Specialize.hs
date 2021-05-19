module Core.Specialize where

import Data.Maybe (mapMaybe)

import Lib.PPrint
import Common.Name
import qualified Common.NameMap  as M
import qualified Common.NameSet as S
import Core.Core
import Core.Pretty()
import Type.Pretty
import qualified Core.Pretty as Pretty
import Lib.Trace

data SpecializeDef = SpecializeDef
  { targetFunc :: Name
  , argsToSpecialize :: [Int]
  } deriving (Show)

extractSpecializeDefs :: DefGroups -> SpecializeEnv
extractSpecializeDefs = 
    specenvNew
  . filter (not . null . argsToSpecialize)
  . map getInline
  . flattenDefGroups
  . filter isRecursiveDefGroup

  where
    isRecursiveDefGroup (DefRec [def]) = True
    isRecursiveDefGroup _ = False

getInline :: Def -> SpecializeDef
getInline def =
    SpecializeDef (defName def)
  $ map snd
  $ filter (\(name, _) -> name `S.member` calledInThisDef def) (passedRecursivelyToThisDef def)

calledInThisDef :: Def -> S.NameSet
calledInThisDef def = foldMapExpr go $ defExpr def
  where 
    go (App (Var (TName name _) _) xs) = S.singleton name

    -- this doesn't seems to make a difference?
    go (App (TypeApp (Var (TName name _) _) _) xs) = S.singleton name
    go _ = S.empty

-- return list of (paramName, paramIndex) that get called recursively to the same function in the same order
passedRecursivelyToThisDef :: Def -> [(Name, Int)]
passedRecursivelyToThisDef def 
  -- TODO: FunDef type to avoid this check?
  | Lam args effect body <- defExpr def = foldMapExpr (go args) $ defExpr def
  | TypeLam _ (Lam params effect body) <- defExpr def = foldMapExpr (go params) $ defExpr def
  | otherwise = mempty
  where
    go :: [TName] -> Expr -> [(Name, Int)]
    go params (App (Var (TName name _) _) args)
      | name == defName def = doWork args params
    go params (App (TypeApp (Var (TName name _) _) _) args)
      | name == defName def = doWork args params
    go params _ = mempty

    doWork :: [Expr] -> [TName] -> [(Name, Int)]
    doWork args params =
      flip mapMaybe (zip3 [0..] args params) $ \(i, arg, param) ->
        case arg of
          Var tname _ | tname == param -> Just (getName tname, i)
          _ -> Nothing


{--------------------------------------------------------------------------
  
--------------------------------------------------------------------------}

-- | Environment mapping names to specialize definitions
newtype SpecializeEnv   = SpecializeEnv (M.NameMap SpecializeDef)

-- | The intial SpecializeEnv
specenvEmpty :: SpecializeEnv
specenvEmpty
  = SpecializeEnv M.empty

specenvNew :: [SpecializeDef] -> SpecializeEnv
specenvNew xs
  = specenvExtends xs specenvEmpty

specenvExtends :: [SpecializeDef] -> SpecializeEnv -> SpecializeEnv
specenvExtends xs specenv
  = foldr specenvExtend specenv xs

specenvExtend :: SpecializeDef -> SpecializeEnv -> SpecializeEnv
specenvExtend idef (SpecializeEnv specenv)
  = SpecializeEnv (M.insert (targetFunc idef) idef specenv)

specenvLookup :: Name -> SpecializeEnv -> Maybe SpecializeDef
specenvLookup name (SpecializeEnv specenv)
  = M.lookup name specenv


instance Show SpecializeEnv where
 show = show . pretty

instance Pretty SpecializeEnv where
 pretty g
   = ppSpecializeEnv defaultEnv g


ppSpecializeEnv :: Env -> SpecializeEnv -> Doc
ppSpecializeEnv env (SpecializeEnv specenv)
   = vcat [fill maxwidth (ppName env name) <+> list (map pretty (argsToSpecialize sdef))
          | (name,sdef) <- M.toList specenv]
   where
     maxwidth      = 12

module Core.Specialize( SpecializeEnv
                      , specenvNew
                      , specenvEmpty
                      , specenvExtend, specenvExtends
                      , specenvLookup
                      , ppSpecializeEnv

                      , extractSpecializeDefs 
                      ) where

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
    go (App (Var (TName name _) _) xs)             = S.singleton name
    go (App (TypeApp (Var (TName name _) _) _) xs) = S.singleton name
    go _ = S.empty

-- return list of (paramName, paramIndex) that get called recursively to the same function in the same order
passedRecursivelyToThisDef :: Def -> [(Name, Int)]
passedRecursivelyToThisDef def 
  -- TODO: FunDef type to avoid this check?
  = case defExpr def of
      Lam params effect body 
        -> foldMapExpr (callsWith params) $ defExpr def
      TypeLam _ (Lam params effect body) 
        -> foldMapExpr (callsWith params) $ defExpr def
      _ -> mempty
  where
    dname = defName def

    callsWith :: [TName] -> Expr -> [(Name, Int)]
    callsWith params (App (Var (TName name _) _) args)
      | name == dname  = check args params
    callsWith params (App (TypeApp (Var (TName name _) _) _) args)
      | name == dname   = check args params
    callsWith params _ = mempty

    check :: [Expr] -> [TName] -> [(Name, Int)]
    check args params =
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

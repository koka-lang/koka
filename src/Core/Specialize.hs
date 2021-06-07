module Core.Specialize( SpecializeEnv
                      , specenvNew
                      , specenvEmpty
                      , specenvExtend, specenvExtends
                      , specenvLookup
                      , ppSpecializeEnv

                      , specialize

                      , extractSpecializeEnv
                      ) where

import Data.Bifunctor
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.Maybe (mapMaybe, fromMaybe, catMaybes, isJust, fromJust)

import Lib.PPrint
import Common.Failure (failure)
import Common.Syntax
import Common.Name
import Common.NameMap (NameMap)
import qualified Common.NameMap  as M
import Common.NameSet (NameSet)
import qualified Common.NameSet as S
import Core.Core
import Core.Pretty ()
import Type.Pretty
import Lib.Trace

{--------------------------------------------------------------------------
  
--------------------------------------------------------------------------}

data SpecializeInfo = SpecializeInfo
  { specName :: Name
  , specArgs :: [Bool]
  , specExpr :: Expr
  } deriving (Show)



{--------------------------------------------------------------------------
  Specialization Monad

  val f = fn(x){ .. }
  map(xs,f) ++ map(ys,f)

--------------------------------------------------------------------------}

data SpecState = SpecState
  { _inScope :: NameMap Expr
  , _newDefs :: DefGroups
  } deriving (Show)

type SpecM = ReaderT SpecializeEnv (State SpecState)

runSpecM :: NameMap Expr -> SpecializeEnv -> SpecM a -> (a, DefGroups)
runSpecM scope specEnv specM = 
  let (result, SpecState _ newDefs) = flip runState (SpecState { _inScope = scope, _newDefs = [] }) $ flip runReaderT specEnv specM
  in (result, newDefs)

inScope :: SpecM (NameMap Expr)
inScope = gets _inScope

queryScope :: Name -> SpecM (Maybe Expr)
queryScope name = gets (M.lookup name . _inScope)

addToScope :: Name -> Expr -> SpecM ()
addToScope name expr = modify (\state@SpecState{ _inScope = inScope } -> state{ _inScope = M.insert name expr inScope })

emitSpecializedDefGroup :: DefGroup -> SpecM ()
emitSpecializedDefGroup defGroup = modify (\state@SpecState { _newDefs = newDefs } -> state{ _newDefs = defGroup:newDefs})



{--------------------------------------------------------------------------
  Specialization
  
DefRec  
  foo_map(xs) {
    val f = fn(x){ x + foo(n-1) }
    ...
  }

  foo(n) {
    val f = fn(x){ x + foo(n-1) }
    foo_map(xs)
  }
--------------------------------------------------------------------------}

specialize :: Env -> Int -> SpecializeEnv -> DefGroups -> (DefGroups, Int)
specialize env uniq specEnv groups =
  let 
    -- TODO: initial scope isn't empty
    (changedDefs, newDefs) = runSpecM M.empty specEnv $ mapM specOneDefGroup groups
  in (changedDefs ++ newDefs, uniq)

speclookupM :: Name -> SpecM (Maybe SpecializeInfo)
speclookupM name = asks (specenvLookup name)

specOneDefGroup :: DefGroup -> SpecM DefGroup
specOneDefGroup = mapMDefGroup specOneDef

specOneDef :: Def -> SpecM Def
specOneDef def = do
  e <- specOneExpr $ defExpr def
  pure def{ defExpr = e }

specOneExpr :: Expr -> SpecM Expr
specOneExpr = rewriteBottomUpM $ \e -> case e of
  App (Var (TName name _) _) args -> go name e
  App (TypeApp (Var (TName name _) _)_) args -> go name e
  e -> pure e
  where
    go name e = do
      specDef <- speclookupM name
      case specDef of
        Nothing -> pure e
        Just def -> specOneCall def e

lookupInScope :: Name -> Def
lookupInScope = undefined

createSpecializedDef :: Name -> [Bool] -> [Expr] -> SpecM Def
createSpecializedDef name paramsToSpecialize args = 
  case defExpr def of
    e@(Lam params eff expr) -> pure $ go e params
    -- TypeLam types e@(Lam params eff expr) -> pure $ go e params
    _ -> failure "Unexpected specialize target"
  where
    def = lookupInScope name
    go (Lam params eff body) _ = 
      Def undefined undefined undefined undefined undefined undefined undefined undefined 
        where
          newParams = filterBools paramsToSpecialize $ zip params args
          specializedBody = Lam params eff $ Let [DefNonRec $ Def (getName paramName) undefined arg undefined undefined undefined undefined undefined | (paramName, arg) <- newParams] body

filterBools :: [Bool] -> [a] -> [a]
filterBools bools as = catMaybes $ zipWith (\bool a -> guard bool >> Just a) bools as

specOneCall :: SpecializeInfo -> Expr -> SpecM Expr
specOneCall (SpecializeInfo{ specArgs=specArgs }) e = case e of
  App (Var (TName name _) _) args -> do
    specializedDef <- createSpecializedDef name specArgs args
    emitSpecializedDefGroup $ DefRec [specializedDef]
    replaceCall specializedDef specArgs args

  -- the result may no longer be a typeapp
  -- App (TypeApp (Var (TName name _) _) _) args -> undefined
  _ -> error "specOneCall"

replaceCall :: Def -> [Bool] -> [Expr] -> SpecM Expr
replaceCall specializedDef bools args =
    pure $ App 
     (Var (TName (defName specializedDef) (defType specializedDef)) $ error "VarInfo")
     newArgs
  where
    newArgs = filterBools bools args


{--------------------------------------------------------------------------
  Extract definitions that should be specialized
--------------------------------------------------------------------------}

extractSpecializeEnv :: DefGroups -> SpecializeEnv
extractSpecializeEnv = 
    specenvNew
  . filter (not . null . specArgs)
  . map getInline
  . flattenDefGroups
  . filter isRecursiveDefGroup

  where
    isRecursiveDefGroup (DefRec [def]) = True
    isRecursiveDefGroup _ = False

getInline :: Def -> SpecializeInfo
getInline def =
  let specArgs = map (maybe False (`S.member` usedInThisDef def))
                 $ passedRecursivelyToThisDef def
  in SpecializeInfo (defName def) specArgs (defExpr def)

usedInThisDef :: Def -> S.NameSet
usedInThisDef def = foldMapExpr go $ defExpr def
  where 
    go (Var (TName name _) _) = S.singleton name
    go _ = mempty
    -- go (App (Var (TName name _) _) xs)             = S.singleton name
    -- go (App (TypeApp (Var (TName name _) _) _) xs) = S.singleton name
    -- go _ = S.empty

-- return list of (paramName, paramIndex) that get called recursively to the same function in the same order
passedRecursivelyToThisDef :: Def -> [Maybe Name]
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

    callsWith params (App (Var (TName name _) _) args)
      | name == dname  = check args params
    callsWith params (App (TypeApp (Var (TName name _) _) _) args)
      | name == dname  = check args params
    callsWith params _ = mempty

    check args params =
      zipWith (\arg param ->
        case arg of
          Var tname _ | tname == param -> Just (getName tname)
          _ -> Nothing) args params


{--------------------------------------------------------------------------
  Specialize Environment
--------------------------------------------------------------------------}

-- | Environment mapping names to specialize definitions
newtype SpecializeEnv   = SpecializeEnv (M.NameMap SpecializeInfo)

-- | The intial SpecializeEnv
specenvEmpty :: SpecializeEnv
specenvEmpty
  = SpecializeEnv M.empty

specenvNew :: [SpecializeInfo] -> SpecializeEnv
specenvNew xs
  = specenvExtends xs specenvEmpty

specenvExtends :: [SpecializeInfo] -> SpecializeEnv -> SpecializeEnv
specenvExtends xs specenv
  = foldr specenvExtend specenv xs

specenvExtend :: SpecializeInfo -> SpecializeEnv -> SpecializeEnv
specenvExtend idef (SpecializeEnv specenv)
  = SpecializeEnv (M.insert (specName idef) idef specenv)

specenvLookup :: Name -> SpecializeEnv -> Maybe SpecializeInfo
specenvLookup name (SpecializeEnv specenv)
  = M.lookup name specenv


instance Show SpecializeEnv where
 show = show . pretty

instance Pretty SpecializeEnv where
 pretty g
   = ppSpecializeEnv defaultEnv g


ppSpecializeEnv :: Env -> SpecializeEnv -> Doc
ppSpecializeEnv env (SpecializeEnv specenv)
   = vcat [fill maxwidth (ppName env name) <+> list (map pretty (specArgs sdef))
          | (name,sdef) <- M.toList specenv]
   where
     maxwidth      = 12

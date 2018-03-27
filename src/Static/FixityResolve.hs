------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Resolve operator expressions according to their fixity.
-}
-----------------------------------------------------------------------------

module Static.FixityResolve( fixityResolve
                           , Fixities, findFixity, fixitiesEmpty, fixitiesCompose
                           , fixitiesNew
                           , resolve
                           )
                           where

-- import Lib.Trace
import Control.Applicative
import Control.Monad
import qualified Common.NameMap as M
import Lib.PPrint
import Common.Failure( failure )
import Common.NamePrim( nameOpExpr )
import Common.Name
import Common.ColorScheme( ColorScheme )
import Common.Error
import Common.Range
import Common.Syntax
import Syntax.Syntax

fixityResolve :: ColorScheme -> Fixities -> UserProgram -> Error (UserProgram,Fixities)
fixityResolve cscheme fixMap (Program source modName nameRange tdgroups defs importdefs externals fixdefs doc)
  = let fixMap1 = fixitiesCompose fixMap (extractFixMap fixdefs)
    in  do defs1 <- runFixM fixMap1 (resolveDefs defs)
           return (Program source modName nameRange tdgroups defs1 importdefs externals fixdefs doc,fixMap1)

extractFixMap :: [FixDef] -> Fixities
extractFixMap fixDefs
  = fixitiesNew (concatMap extractFix fixDefs)
  where
    extractFix (FixDef name fixity range)
      = [(name,fixity)]


{--------------------------------------------------------------------------
  Rewrite syntax tree
--------------------------------------------------------------------------}
resolveDefs :: DefGroups UserType -> FixM (DefGroups UserType)
resolveDefs defs
  = mapM resolveDefGroup defs

resolveDefGroup (DefRec defs)
  = do defs' <- mapM resolveDef defs
       return (DefRec defs')
resolveDefGroup (DefNonRec def)
  = resolveDef def >>= return . DefNonRec

resolveDef (Def binder range vis isVal doc)
  = do binder' <- resolveBinder binder
       return (Def binder' range vis isVal doc)

resolveBinder binder
  = do expr' <- resolveExpr (binderExpr binder)
       return (binder{ binderExpr = expr' })

resolveBinderMaybe binder
  = do mbExpr <- case (binderExpr binder) of
                   Just expr -> resolveExpr expr >>= return . Just
                   Nothing   -> return Nothing
       return (binder{ binderExpr = mbExpr })

resolveExpr :: UserExpr -> FixM UserExpr
resolveExpr expr
  = case expr of
      Lam    binds expr rng  -> do binds' <- mapM resolveBinderMaybe binds
                                   expr' <- resolveExpr expr
                                   return (Lam binds' expr' rng)
      Let    defs expr range -> do defs' <- resolveDefGroup defs
                                   expr' <- resolveExpr expr
                                   return (Let defs' expr' range)
      Bind   def expr range  -> do def' <- resolveDef def
                                   expr' <- resolveExpr expr
                                   return (Bind def' expr' range)
      App    fun nargs range -> do fun' <- resolveExpr fun
                                   let (names,args) = unzip nargs
                                   args' <- mapM resolveExpr args
                                   let nargs' = zip names args'
                                   case fun of
                                     Var name _ _ | name == nameOpExpr
                                       -> resolve args'
                                     _ -> return (App fun' nargs' range)
      Var    name isOp range -> return expr
      Lit    lit             -> return expr
      Ann    expr tp range   -> do expr' <- resolveExpr expr
                                   return (Ann expr' tp range)
      Case   expr brs range  -> do expr' <- resolveExpr expr
                                   brs'   <- mapM resolveBranch brs
                                   return (Case expr' brs' range)
      Parens expr range      -> do expr' <- resolveExpr expr
                                   return (Parens expr' range)
      Handler shallow scoped eff pars reinit ret final ops hrng rng
                             -> do ret' <- resolveExpr ret
                                   reinit' <- resolveExpr reinit
                                   final' <- resolveExpr final
                                   ops' <- mapM resolveHandlerBranch ops
                                   return (Handler shallow scoped eff pars reinit' ret' final' ops' hrng rng)
      Inject tp expr range   -> do expr' <- resolveExpr expr
                                   return (Inject tp expr range)                                   

isJust (Just _) = True
isJust Nothing  = False

resolveBranch (Branch pattern guard body)
  = do guard'  <- resolveExpr guard
       body'   <- resolveExpr body
       return (Branch pattern guard' body')

resolveHandlerBranch hb@(HandlerBranch{ hbranchExpr=expr })
  = do expr'   <- resolveExpr expr
       return hb{ hbranchExpr = expr' }

{--------------------------------------------------------------------------
  Fixity map for all operators
--------------------------------------------------------------------------}
type Fixities = M.NameMap Fixity

fixitiesEmpty :: Fixities
fixitiesEmpty
  = M.empty

findFixity :: Fixities -> Name -> Fixity
findFixity fixities name
  = case M.lookup name fixities of
      Just (fix) -> fix
      Nothing    -> FixInfix 50 AssocNone


fixitiesCompose :: Fixities -> Fixities -> Fixities
fixitiesCompose fix1 fix2
  = M.unionWith compose fix2 fix1
  where
    compose f1 f2
      = case (f1,f2) of
          (_, FixInfix _ _) -> f2
          (FixInfix  _ _,_) -> f1
          _                 -> f2

fixitiesNew :: [(Name,Fixity)] -> Fixities
fixitiesNew fs
  = M.fromList [(name,f) | (name,f@(FixInfix _ _)) <- fs]

-- The fixity monad collects error messages and passes a fixity map
data FixM a = FixM (Fixities -> Res a)
data Res a  = Res !a ![(Range,Doc)]

runFixM :: Fixities -> FixM a -> Error a
runFixM fixities (FixM f)
  = case f fixities of
      Res x errors -> if null errors then return x else errorMsg (ErrorStatic errors)

instance Functor FixM where
  fmap  = liftM

instance Applicative FixM where
  pure  = return
  (<*>) = ap

instance Monad FixM where
  return x          = FixM (\fixmap -> Res x [])
  (FixM fm) >>= f   = FixM (\fixmap -> case fm fixmap of
                                         Res x errs1 -> case f x of
                                                          FixM fm' -> case fm' fixmap of
                                                                        Res y errs2 -> Res y (errs1 ++ errs2))
getFixities :: FixM Fixities
getFixities
  = FixM (\fm -> Res fm [])

emitError :: Range -> Doc -> FixM ()
emitError range doc
  = FixM (\fm -> Res () [(range,doc)])

{--------------------------------------------------------------------------
  Resolve fixities:
  The algorithm is written a bit long to enable easy extension
  to prefix, postfix, and distfix operators.
--------------------------------------------------------------------------}

data Op             = Op UserExpr Fixity

data Term           = Term UserExpr
                    | Oper Op

type ExprStack      = [UserExpr]
type OpStack        = [Op]

resolve :: [UserExpr] -> FixM UserExpr
resolve exprs
  = do fixMap <- getFixities
       let terms = map (toTerm fixMap) exprs
       resolveTerms [] [] terms


-- Find out if this is an operator
toTerm :: Fixities -> UserExpr -> Term
toTerm fixities expr
  = case expr of
      Var name True _ -> Oper (Op expr (findFixity fixities name))
      _               -> Term expr


-- The real resolve algorithm uses Dijkstra's strategy with two stacks
resolveTerms :: ExprStack -> OpStack -> [Term] -> FixM UserExpr

-- final term computed
resolveTerms [x] [] []
  = return x

-- cleanup stage, apply all ops
resolveTerms xs ops@(op:_) []
  = apply xs ops []

-- always push terms
resolveTerms xs ops (Term t:tt)
  = resolveTerms (t:xs) ops tt

{-
-- prefix operator
resolveTerms xs ops (Oper t@(Op (Var name _ _) FixPrefix):tt)
  = push xs ops tt t
-- postfix operator
resolveTerms xs ops ts@(Oper t@(Op op FixPostfix):tt)
  = push xs ops tt t
-}

-- infix operator
resolveTerms xs ops ts@(Oper t@(Op op (FixInfix prec assoc)):tt)
    | prec > precCtx   = push xs ops tt t
    | prec < precCtx   = apply xs ops ts
    | prec == precCtx  = do{ checkAmbigious ops t
                           ; let (Op op fix) = t
                           ; if (assoc == AssocRight)
                              then push xs ops tt t
                              else apply xs ops ts
                           }
    where
      prec      = precedenceOp t
      precCtx   | null ops  = 0
                | otherwise = precedenceOp (head ops)


resolveTerms [] [] []
    = failure "Static.FixityResolve.resolveTerms: no term: fix parser"
resolveTerms xs ops ts
    = failure "Static.FixityResolve.resolveTerms: fixity resolver error"


precedenceOp (Op op fix)            = precedence fix
precedence (FixInfix prec assoc)    = prec
precedence (FixPrefix )             = 102
precedence (FixPostfix )            = 101



checkAmbigious (Op opCtx fixCtx@(FixInfix _ assocCtx):ops) (Op op fix@(FixInfix _ assoc))
    | assocCtx == AssocNone || assocCtx /= assoc
    = ambigious fixCtx fix op
checkAmbigious ops t
    = return ()

ambigious :: Fixity -> Fixity -> UserExpr -> FixM ()
ambigious fixCtx fix op
    = emitError (getRange op)
                (text "Ambigious" <+> ppFixity fix <+> text "operator" <+> opText <> text "in a"
                  <+> ppFixity fixCtx <+> text "context" <->
                 text " hint: add parenthesis around the sub-expression to disambiguate")
    where
      opText  = case op of
                  Var name _ _  -> pretty name <> space
                  _             -> Lib.PPrint.empty

ppFixity (FixInfix prec assoc)
  = case assoc of
      AssocNone  -> text "non-associative"
      AssocRight -> text "right-associative"
      AssocLeft  -> text "left-associative"
ppFixity (FixPrefix)
  = text "prefix"
ppFixity (FixPostfix)
  = text "postfix"

-----------------------------------------------------------
-- Helper operations: push & apply
-----------------------------------------------------------
push xs ops ts t@(Op op fix)
    = resolveTerms xs (t:ops) ts

apply xs (Op op FixPostfix:ops) ts
    = do{ ys <- applyUnaryOp op xs
        ; resolveTerms ys ops ts
        }

apply xs (Op op FixPrefix:ops) ts
    = do{ ys <- applyUnaryOp op xs
        ; resolveTerms ys ops ts
        }

apply xs (Op op (FixInfix _ _):ops) ts
    = do{ ys <- applyInfixOp op xs
        ; resolveTerms ys ops ts
        }
apply xs ops ts
    = failure "Static.FixityResolve.apply: fixity resolver failure"

applyUnaryOp op (t:ts)
  = return ((App op [(Nothing, t)] (combineRanged op t)):ts)

applyUnaryOp op ts
  = do{ emitError (getRange op)
          (text "Unary operator has not enough arguments")
      ; return ts
      }

applyInfixOp op (t1:t2:ts)
    = return ((makeApp op t2 t1 (combineRanged t1 t2)):ts)
applyInfixOp op ts
    = do{ emitError (getRange op)
                    (text "Infix operator has not enough arguments")
        ; return ts
        }

makeApp op e1 e2 r
    = App op [(Nothing,e1),(Nothing,e2)] r

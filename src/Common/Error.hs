-----------------------------------------------------------------------------
-- Copyright 2012-2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Internal error messages and monad.
-}
-----------------------------------------------------------------------------
module Common.Error( -- error messages
                     ErrorMessage(..), ErrorKind(..), ErrorSeverity(..), Errors(..), Warnings
                   , ppErrorMessage, ppErrors
                   , errorMessage, warningMessage, errorMessageKind, warningMessageKind, toWarning
                   , mergeErrors, errorsNil, errorsSingle, errorsAdd
                   -- monad: todo redesign
                   , Error, ok
                   , warningMsg, errorMsg, warningMsgs, errorMsgs
                   , errorMsgPartial, errorMsgsPartial, addErrorMsg
                   , handleError, checkError, checkPartial, setPartial
                   , addWarnings, addPartialResult
                   , ignoreWarnings
                  ) where

import Data.Typeable
import Control.Exception
import Control.Monad
import Control.Monad.Fail
import Control.Applicative
import Lib.PPrint
import Common.Range
import Common.ColorScheme
import Common.Message

{--------------------------------------------------------------------------
  Error messages
--------------------------------------------------------------------------}

-- | Error messages
data Errors       = Errors{ errors :: ![ErrorMessage] }
                  deriving (Typeable)

type Warnings     = Errors

data ErrorMessage = ErrorMsg{ errRange   :: !Range
                            , errMessage :: !Doc
                            , errSeverity:: !ErrorSeverity
                            , errKind    :: !ErrorKind
                            }
                  deriving (Typeable)

data ErrorSeverity= SevInfo | SevWarning | SevError
                  deriving (Eq,Ord,Typeable)

data ErrorKind    = ErrGeneral | ErrParse | ErrStatic | ErrKind | ErrType | ErrBuild | ErrInternal
                  deriving (Eq, Typeable)

instance Exception ErrorMessage
instance Exception Errors

instance Ranged ErrorMessage where
  getRange err = errRange err

instance Ranged Errors where
  getRange (Errors errs)  = combineRanges (map errRange errs)

instance Show ErrorMessage where
  show err = show (pretty err)

instance Show Errors where
  show errs = show (pretty errs)

isWarning emsg
  = errSeverity emsg <= SevWarning


infoMessageKind ekind range doc
  = ErrorMsg range doc SevInfo ekind

warningMessageKind ekind range doc
  = ErrorMsg range doc SevWarning ekind

errorMessageKind ekind range doc
  = ErrorMsg range doc SevError ekind

warningMessage range doc
  = warningMessageKind ErrGeneral range doc

errorMessage range doc
  = errorMessageKind ErrGeneral range doc


errorsNil :: Errors
errorsNil = Errors []

errorsSingle :: ErrorMessage -> Errors
errorsSingle err = Errors [err]

errorsAdd :: [ErrorMessage] -> Errors -> Errors
errorsAdd errs (Errors errs')  = Errors (errs' ++ errs)

mergeErrors :: Errors -> Errors -> Errors
mergeErrors (Errors errs1) (Errors errs2)
  = Errors (errs1 ++ errs2)


{--------------------------------------------------------------------------
  pretty
--------------------------------------------------------------------------}
instance Pretty ErrorMessage where
  pretty msg  = ppErrorMessage "" False defaultColorScheme msg

instance Pretty Errors where
  pretty errs  = ppErrors "" False defaultColorScheme errs

ppErrorSeverity :: ColorScheme -> ErrorKind -> ErrorSeverity -> Doc
ppErrorSeverity cscheme ekind sev
  = case sev of
      SevError   -> header colorError "error"
      SevWarning -> header colorWarning "warning"
      _          -> header colorWarning "info"
  where
    header clr txt = color (clr cscheme) (text (ekindTxt ++ txt ++ ":"))
    ekindTxt    = case ekind of
                    ErrParse   -> "parse "
                    ErrKind    -> "kind "
                    ErrType    -> "type "
                    ErrBuild   -> "build "
                    ErrInternal-> "internal "
                    _          -> ""


ppErrorMessage :: FilePath -> Bool -> ColorScheme -> ErrorMessage -> Doc
ppErrorMessage cwd endToo {-show end of range as well?-} cscheme (ErrorMsg range doc esev ekind)
  = hang 2 $ ppRange cwd endToo cscheme range <.> colon <+> ppErrorSeverity cscheme ekind esev <+> doc

ppErrors :: FilePath -> Bool -> ColorScheme -> Errors -> Doc
ppErrors cwd endToo cscheme (Errors errs)
  = vcat (map (ppErrorMessage cwd endToo cscheme) errs)


toWarning :: ErrorKind -> (Range,Doc) -> ErrorMessage
toWarning ekind (range,doc)
  = warningMessageKind ekind range doc





{--------------------------------------------------------------------------
  Errors
--------------------------------------------------------------------------}

-- | Error monad -- deprecated
data Error b a    = Error !Errors (Maybe b) -- B is a partial result
                  | Ok !a !Warnings (Maybe b)


-- | Check an 'Error'
checkError :: Error b a -> Either Errors (a,Warnings)
checkError err
  = case err of
      Error err m -> Left err
      Ok x w m    -> Right (x,w)

checkPartial :: Error b a -> Either (Errors, Maybe b) (a, Warnings, Maybe b)
checkPartial err
  = case err of
      Error err m -> Left (err,m)
      Ok x w m    -> Right (x,w,m)

setPartial :: Maybe c -> Error b a -> Error c a
setPartial c err
  = case err of
      Error msg m -> Error msg c
      Ok x w m    -> Ok x w c

handleError :: Error b a -> (Errors -> Error b a) -> Error b a
handleError err handle
  = case err of
      Error errs m -> addPartialResult (handle errs) m
      Ok x w m     -> Ok x w m

ok :: a -> Error b a
ok x    = Ok x errorsNil Nothing

errorMsgs :: [ErrorMessage] -> Error b a
errorMsgs errs = Error (Errors errs) Nothing

errorMsg :: ErrorMessage -> Error b a
errorMsg msg = Error (errorsSingle msg) Nothing

errorMsgsPartial :: [ErrorMessage] -> Maybe b -> Error b a
errorMsgsPartial msgs b = Error (Errors msgs) b

errorMsgPartial :: ErrorMessage -> Maybe b -> Error b a
errorMsgPartial msg b = Error (errorsSingle msg) b

addErrorMsg :: ErrorMessage -> Error b ()
addErrorMsg err
  = warningMsg err

warningMsgs :: [ErrorMessage] -> Error b ()
warningMsgs ws = Ok () (Errors ws) Nothing

warningMsg :: ErrorMessage -> Error b ()
warningMsg w  = Ok () (errorsSingle w) Nothing

addWarnings :: [ErrorMessage] -> Error b a -> Error b a
addWarnings [] err  = err
addWarnings ws err
  = case err of
      Error errs m -> Error (errorsAdd ws errs) m
      Ok x ws2 m  -> Ok x (errorsAdd ws ws2) m

addPartialResult :: Error b a -> Maybe b -> Error b a
addPartialResult err m
  = case err of
      Error errs m1 -> Error errs (m1 <|> m)
      Ok x ws m1    -> Ok x ws (m1 <|> m)

overridePartialResult :: Error b a -> Maybe b -> Error b a
overridePartialResult err m
  = case err of
      Error errs m1 -> Error errs (m <|> m1)
      Ok x ws m1    -> Ok x ws (m <|> m1)

ignoreWarnings :: Error b a -> Error b a
ignoreWarnings err
  = case err of
      Error (Errors errs) m -> Error (Errors (filter (not . isWarning) errs)) m
      Ok x (Errors ws) m    -> Ok x errorsNil m



{--------------------------------------------------------------------------
  instances
--------------------------------------------------------------------------}
instance Functor (Error b) where
  fmap f e      = case e of
                    Ok x w m    -> Ok (f x) w m
                    Error msg m -> Error msg m

instance Applicative (Error b) where
  pure x = Ok x errorsNil Nothing
  (<*>) = ap

instance Monad (Error b) where
  -- return = pure
  e >>= f       = case e of
                    Ok x (Errors ws) m    -> addPartialResult (addWarnings ws (f x)) m
                    Error msg m -> Error msg m

instance MonadFail (Error b) where
  fail s        = Error (errorsSingle (errorMessage rangeNull (text s))) Nothing

instance MonadPlus (Error b) where
  mzero         = Error errorsNil Nothing
  mplus e1 e2   = case e1 of
                    Ok{}  -> e1
                    Error m1 m11 -> case e2 of
                                      Ok{}  -> addPartialResult e2 m11
                                      Error m2 m12 -> Error (mergeErrors m1 m2) (m12 `mplus` m11)


instance Alternative (Error b) where
  (<|>) = mplus
  empty = mzero


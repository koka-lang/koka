-----------------------------------------------------------------------------
-- Copyright 2018-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Kinds describing how an operation is using the resumption.
-}
-----------------------------------------------------------------------------
module Common.ResumeKind ( ResumeKind(..) ) where

data ResumeKind
  = ResumeNever
  | ResumeTail
  | ResumeScopedOnce
  | ResumeScoped
  | ResumeOnce
  | ResumeNormal
  | ResumeOnceRaw
  | ResumeNormalRaw
  deriving (Eq,Ord,Enum)

instance Show ResumeKind where
  show rk = case rk of
              ResumeNever -> "never"
              ResumeTail  -> "tail"
              ResumeScopedOnce -> "scoped once"
              ResumeScoped -> "scoped"
              ResumeOnce  -> "once"
              ResumeNormal -> "normal"
              ResumeOnceRaw -> "once (no finalization)"
              ResumeNormalRaw -> "normal (no finalization)"
-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Internal identifiers (just 'Int's).
-}
-----------------------------------------------------------------------------
module Common.Id
          ( Id, Ids     -- instance Eq Ord Show
          , showId      -- show with quotes
          , genId
          , newId
          , newIdFromId
          , idNil
          , idNumber
          ) where

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
-- | A list of identifiers
type Ids    = [Id]

-- | Identifiers are unique compiler generated identities.
type Id     = Int

-- | Show quotes around the id
showId :: Id -> String
showId id
  = "\"" ++ show id ++ "\""

-- | Generate an 'Id' with a certain base name (which is ignored :-)
genId :: String -> Int -> Id
genId baseName i
  = newId i

-- | Create a fresh identifier
newId :: Int -> Id
newId i = i

newIdFromId :: Id -> Id
newIdFromId id
  = id+1

-- | Dummy identifier
idNil :: Id
idNil
  = newId 0

-- | Only used internally to guarantee unique identifiers
idNumber :: Id -> Int
idNumber i
  = i

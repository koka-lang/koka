/////////////////////////////////////////////////////////////////////////////
// Copyright 2012-2021, Microsoft Research, Daan Leijen.
//
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the LICENSE file at the root of this distribution.
/////////////////////////////////////////////////////////////////////////////
import std/data/map
import std/data/imap
import std/data/iset
/// Internal identifiers (just 'Int's).

/// Types

/// A list of identifiers
alias ids = list<id>
// A map from identifiers to values
// alias id-map = imap<a>
// A set of identifiers
// alias id-set = iset

/// Identifiers are unique compiler generated identities
alias id = int;

/// Show quotes around the id
fun showId(id: id): <> string
  "\"" ++ id.show ++ "\""

/// Generate an 'Id' with a certain base name (which is ignored :-)
fun genId(baseName: string, i: int): <> id
  i.newId

/// Create a fresh identifier
fun newId(i: int): <> id
  i

fun newIdFromId(id: id): <> id
  id + 1

/// Dummy identifier
fun idNil(): <> id
  0.newId

/// Only used internally to guarantee unique identifiers
fun idNumber(id: id): <> int
  id

// Map identifiers to /nice/ strings.
// alias nice = id-map<string>

// val niceEmpty:nice = empty

// TODO: // Add fresh identifier substitutions give an (infinite) list of nice names.
// fun extend(n: nice, ids: ids, names: list<string>): nice
//   val names.filter(fn(i) -> n.)

// Show the nice version of an identifier
// fun niceShow(n: nice, id: id): string
//   match n.lookup(id)
//     Just(name) -> name
//     Nothing -> id.show

// Pretty print an identifier nicely.
// fun nicePretty(n: nice, id: id): doc
//   n.niceShow(id).text

/// Tests for identifiers
// fun main()
//   (idNil().newId.newIdFromId.idNumber == 1).show.println
//   (genId("test", 1).showId == "\"1\"").show.println
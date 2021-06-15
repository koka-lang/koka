/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Export module `mod` extended with `exp`. Modifies `exp` in place and assigns to mod
function _export(mod,exp) {
  for(var prop in mod) {
    if (exp[prop] === undefined) {
      exp[prop] = mod[prop];
    }
  }
  return exp;
}

// Add "_export" to the exports of this module
$std_core_types = _export( $std_core_types, {
                    "_export": _export,
                  });

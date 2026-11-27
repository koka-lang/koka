/*---------------------------------------------------------------------------
  Copyright 2026, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Tasks run sequentially in JavaScript so a `get` that is not yet satisfied would block forever.
function _lvar_get( lv, bot, is_gte ) {
  if (is_gte(lv.value, bot) !== 0) return lv.value;
  throw new Error("lvar get would block forever (tasks run sequentially in JavaScript)");
}

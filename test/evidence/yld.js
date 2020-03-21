/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

function evv_insert_ev(ev,evv) {
  const n = evv.length;
  const evv2 = new Array(n+1);
  var i;
  for(i = 0; i < n; i++) {
    const ev2 = evv[i];
    if (ev.tag <= ev2.tag) break;
    evv2[i] = ev2;
  }
  evv2[i] = ev;
  for(; i < n; i++) {
    evv2[i+1] = evv[i];
  }
  return evv2;
}

function evv_lookup(tag,evv) {
  for(var i = 0; i < evv.length; i++) {
    if (tag === evv[i].tag) return evv[i];
  }
  console.error("cannot find " + tag + " in " + evv.toString());
  return null;
}

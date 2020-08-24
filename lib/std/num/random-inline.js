/*---------------------------------------------------------------------------
-- Copyright 2020, Daan Leijen, Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

var crypto = null;
if ($std_core.host() === "node") {
  crypto = require('crypto');
}

var random_out = new Int32Array(32);
var random_used = random_out.length;
var random_strong = false;

function _srandom_round() {
  if (crypto && crypto.randomFillSync) {
    random_strong = true;
    crypto.randomFillSync(random_out);
  }
  else if (window && window.crypto && window.crypto.getRandomValues) {
    random_strong = true;
    window.crypto.getRandomValues(random_out);
  }
  else if (window && window.msCrypto && window.msCrypto.getRandomValues) {
    random_strong = true;
    window.msCrypto.getRandomValues(random_out);
  }
  else {
    random_strong = false;
    for(var i = 0; i < random_out.length; i++) {
      random_out[i] = ((2.0*Math.random() - 1.0)*2147483647.0)|0;
    }
  }
  random_used = 0;  
}

function _srandom_is_strong() {
  return random_strong;
}

function _srandom_int32() {
  if (random_out.length <= random_used) {
    _srandom_round();
  }
  return random_out[random_used++];
}

function _srandom_double() {
  // use 48-bits for a double in the range [0,1)
  var lo = _srandom_int32();
  var hi = _srandom_int32();
  hi = (0x3FF00000 | (hi >>> 12))|0;
  lo = (lo << 4)|0;
  return ($std_num_double.double_from_bits(lo,hi) - 1.0); 
}

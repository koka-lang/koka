// Koka generated module: std/text/unicode, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_exn from './std_core_exn.mjs';
import * as $std_core_bool from './std_core_bool.mjs';
import * as $std_core_order from './std_core_order.mjs';
import * as $std_core_char from './std_core_char.mjs';
import * as $std_core_int from './std_core_int.mjs';
import * as $std_core_vector from './std_core_vector.mjs';
import * as $std_core_string from './std_core_string.mjs';
import * as $std_core_sslice from './std_core_sslice.mjs';
import * as $std_core_list from './std_core_list.mjs';
import * as $std_core_maybe from './std_core_maybe.mjs';
import * as $std_core_maybe2 from './std_core_maybe2.mjs';
import * as $std_core_either from './std_core_either.mjs';
import * as $std_core_result from './std_core_result.mjs';
import * as $std_core_tuple from './std_core_tuple.mjs';
import * as $std_core_lazy from './std_core_lazy.mjs';
import * as $std_core_show from './std_core_show.mjs';
import * as $std_core_debug from './std_core_debug.mjs';
import * as $std_core_delayed from './std_core_delayed.mjs';
import * as $std_core_console from './std_core_console.mjs';
import * as $std_core from './std_core.mjs';
import * as $std_core_undiv from './std_core_undiv.mjs';
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
// normalize a string
function _normalize(s,nf) {
  if (typeof s.normalize === "function") {
    return s.normalize(nf || "NFC");
  }
  else {
    // .. no unicode normalization support on this platform; let's hope for the best
    return s; 
  }
}
 
// type declarations
// type normalization
export const NFC = 1; // normalization
export const NFD = 2; // normalization
export const NFKC = 3; // normalization
export const NFKD = 4; // normalization
// type rtree
export const Tip = null; // rtree
export function Bin(lo, hi, left, right) /* (lo : int, hi : int, left : rtree, right : rtree) -> rtree */  {
  return { lo: lo, hi: hi, left: left, right: right };
}
 
// declarations
 
 
// Is this a combining character?
export function is_combining(c) /* (c : char) -> bool */  {
   
  var i = c;
  if ($std_core_types._int_ge(i,768)) {
    if ($std_core_types._int_le(i,879)) {
      return true;
    }
    else {
      if ($std_core_types._int_ge(i,6832)) {
        if ($std_core_types._int_le(i,6911)) {
          return true;
        }
        else {
          if ($std_core_types._int_ge(i,7616)) {
            if ($std_core_types._int_le(i,7679)) {
              return true;
            }
            else {
              if ($std_core_types._int_ge(i,8400)) {
                if ($std_core_types._int_le(i,8447)) {
                  return true;
                }
                else {
                  return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
                }
              }
              else {
                return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
              }
            }
          }
          else {
            if ($std_core_types._int_ge(i,8400)) {
              if ($std_core_types._int_le(i,8447)) {
                return true;
              }
              else {
                return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
              }
            }
            else {
              return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
            }
          }
        }
      }
      else {
        if ($std_core_types._int_ge(i,7616)) {
          if ($std_core_types._int_le(i,7679)) {
            return true;
          }
          else {
            if ($std_core_types._int_ge(i,8400)) {
              if ($std_core_types._int_le(i,8447)) {
                return true;
              }
              else {
                return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
              }
            }
            else {
              return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
            }
          }
        }
        else {
          if ($std_core_types._int_ge(i,8400)) {
            if ($std_core_types._int_le(i,8447)) {
              return true;
            }
            else {
              return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
            }
          }
          else {
            return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
          }
        }
      }
    }
  }
  else {
    if ($std_core_types._int_ge(i,6832)) {
      if ($std_core_types._int_le(i,6911)) {
        return true;
      }
      else {
        if ($std_core_types._int_ge(i,7616)) {
          if ($std_core_types._int_le(i,7679)) {
            return true;
          }
          else {
            if ($std_core_types._int_ge(i,8400)) {
              if ($std_core_types._int_le(i,8447)) {
                return true;
              }
              else {
                return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
              }
            }
            else {
              return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
            }
          }
        }
        else {
          if ($std_core_types._int_ge(i,8400)) {
            if ($std_core_types._int_le(i,8447)) {
              return true;
            }
            else {
              return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
            }
          }
          else {
            return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
          }
        }
      }
    }
    else {
      if ($std_core_types._int_ge(i,7616)) {
        if ($std_core_types._int_le(i,7679)) {
          return true;
        }
        else {
          if ($std_core_types._int_ge(i,8400)) {
            if ($std_core_types._int_le(i,8447)) {
              return true;
            }
            else {
              return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
            }
          }
          else {
            return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
          }
        }
      }
      else {
        if ($std_core_types._int_ge(i,8400)) {
          if ($std_core_types._int_le(i,8447)) {
            return true;
          }
          else {
            return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
          }
        }
        else {
          return ($std_core_types._int_ge(i,65056)) ? $std_core_types._int_le(i,65071) : false;
        }
      }
    }
  }
}
 
export function consrev(xs, xss) /* (xs : list<char>, xss : list<string>) -> list<string> */  {
  if (xs === null) {
    return xss;
  }
  else {
    return $std_core_types.Cons($std_core_string.listchar_fs_string($std_core_list.reverse_acc($std_core_types.Nil, xs)), xss);
  }
}
 
 
// Join combining characters with their base into a grapheme.
export function join_combining(cs, comb, acc) /* (cs : list<char>, comb : ? (list<char>), acc : ? (list<grapheme>)) -> list<grapheme> */  { tailcall: while(1)
{
  if (cs !== null) {
    var _x0 = is_combining(cs.head);
    if (_x0) {
      {
        // tail call
        var _x2 = (comb !== undefined) ? comb : $std_core_types.Nil;
        var _x1 = $std_core_types.Cons(cs.head, _x2);
        var _x4 = (acc !== undefined) ? acc : $std_core_types.Nil;
        var _x3 = _x4;
        cs = cs.tail;
        comb = _x1;
        acc = _x3;
        continue tailcall;
      }
    }
    else {
      {
        // tail call
        var _x5 = $std_core_types.Cons(cs.head, $std_core_types.Nil);
        var _x8 = (comb !== undefined) ? comb : $std_core_types.Nil;
        if (_x8 === null) {
          var _x7 = (acc !== undefined) ? acc : $std_core_types.Nil;
        }
        else {
          var _x9 = (comb !== undefined) ? comb : $std_core_types.Nil;
          var _x10 = (acc !== undefined) ? acc : $std_core_types.Nil;
          var _x7 = $std_core_types.Cons($std_core_string.listchar_fs_string($std_core_list.reverse_acc($std_core_types.Nil, _x9)), _x10);
        }
        var _x6 = _x7;
        cs = cs.tail;
        comb = _x5;
        acc = _x6;
        continue tailcall;
      }
    }
  }
  else {
     
    var _x11 = (comb !== undefined) ? comb : $std_core_types.Nil;
    if (_x11 === null) {
      var xs_10002 = (acc !== undefined) ? acc : $std_core_types.Nil;
    }
    else {
      var _x12 = (comb !== undefined) ? comb : $std_core_types.Nil;
      var _x13 = (acc !== undefined) ? acc : $std_core_types.Nil;
      var xs_10002 = $std_core_types.Cons($std_core_string.listchar_fs_string($std_core_list.reverse_acc($std_core_types.Nil, _x12)), _x13);
    }
    return $std_core_list.reverse_acc($std_core_types.Nil, xs_10002);
  }
}}
 
export function normalizex(s, norm) /* (s : string, norm : string) -> string */  {
  return _normalize(s,norm);
}
 
 
// Normalize a unicode string. If no normalization form is given, `NFC` is used.
export function normalize(s, norm) /* (s : string, norm : ? normalization) -> string */  {
  if (norm !== undefined) {
    if (norm === 1) {
      var _x11 = "NFC";
    }
    else if (norm === 2) {
      var _x11 = "NFD";
    }
    else if (norm === 3) {
      var _x11 = "NFKC";
    }
    else {
      var _x11 = "NFKD";
    }
  }
  else {
    var _x11 = "NFC";
  }
  return normalizex(s, _x11);
}
 
 
// Convert a string to a list of graphemes.
// Each grapheme will be in `NFC` normalized form.
export function graphemes(s) /* (s : string) -> list<grapheme> */  {
  return join_combining($std_core_string.list(normalizex(s, "NFC")));
}
 
 
// Automatically generated. Tests for the `NFC` constructor of the `:normalization` type.
export function is_nfc(normalization) /* (normalization : normalization) -> bool */  {
  return (normalization === 1);
}
 
 
// Automatically generated. Tests for the `NFD` constructor of the `:normalization` type.
export function is_nfd(normalization) /* (normalization : normalization) -> bool */  {
  return (normalization === 2);
}
 
 
// Automatically generated. Tests for the `NFKC` constructor of the `:normalization` type.
export function is_nfkc(normalization) /* (normalization : normalization) -> bool */  {
  return (normalization === 3);
}
 
 
// Automatically generated. Tests for the `NFKD` constructor of the `:normalization` type.
export function is_nfkd(normalization) /* (normalization : normalization) -> bool */  {
  return (normalization === 4);
}
 
export function bin(x, y) /* (x : rtree, y : rtree) -> rtree */  {
  if (x !== null && y !== null) {
    var _x12 = ($std_core_types._int_le((x.lo),(y.lo))) ? x.lo : y.lo;
    var _x13 = ($std_core_types._int_ge((x.hi),(y.hi))) ? x.hi : y.hi;
    return Bin(_x12, _x13, x, y);
  }
  else if (x === null) {
    return y;
  }
  else {
    return x;
  }
}
 
export function _trmc_combine_pairs(xs, _acc) /* (xs : list<rtree>, ctx<list<rtree>>) -> list<rtree> */  { tailcall: while(1)
{
  if (xs !== null && xs.tail !== null) {
     
    var _trmc_x10334 = bin(xs.head, xs.tail.head);
     
    var _trmc_x10335 = undefined;
     
    var _trmc_x10336 = $std_core_types.Cons(_trmc_x10334, _trmc_x10335);
    {
      // tail call
      var _x14 = $std_core_types._cctx_extend(_acc,_trmc_x10336,({obj: _trmc_x10336, field_name: "tail"}));
      xs = xs.tail.tail;
      _acc = _x14;
      continue tailcall;
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,xs);
  }
}}
 
export function combine_pairs(xs_0) /* (xs : list<rtree>) -> list<rtree> */  {
  return _trmc_combine_pairs(xs_0, $std_core_types._cctx_empty());
}
 
export function combine(xs) /* (xs : list<rtree>) -> rtree */  { tailcall: while(1)
{
  var _x15 = _trmc_combine_pairs(xs, $std_core_types._cctx_empty());
  if (_x15 === null) {
    return Tip;
  }
  else if (_x15 !== null && _x15.tail === null) {
    return _x15.head;
  }
  else {
    {
      // tail call
      xs = _x15;
      continue tailcall;
    }
  }
}}
 
export function build_rtree(xs) /* (xs : list<rtree>) -> rtree */  {
  return combine(xs);
}
 
export function single(lo, hi) /* (lo : int, hi : int) -> rtree */  {
  return Bin(lo, hi, Tip, Tip);
}
 
 
// These characters are considered wide, i.e. 2 columns wide.
export var asian_wide;
var asian_wide = $std_core_delayed.delay(function() {
  return combine($std_core_vector.vlist([Bin(4352, 4447, Tip, Tip), Bin(9001, 9001, Tip, Tip), Bin(9002, 9002, Tip, Tip), Bin(11904, 12350, Tip, Tip), Bin(12352, 42191, Tip, Tip), Bin(44032, 55203, Tip, Tip), Bin(63744, 64255, Tip, Tip), Bin(65040, 65049, Tip, Tip), Bin(65072, 65135, Tip, Tip), Bin(65280, 65376, Tip, Tip), Bin(65504, 65510, Tip, Tip), Bin(131072, 196605, Tip, Tip), Bin(196608, 262141, Tip, Tip)], $std_core_types.Nil));
});
 
 
// These characters have zero width
export var zero_widths;
var zero_widths = $std_core_delayed.delay(function() {
  return combine($std_core_vector.vlist([Bin(0, 31, Tip, Tip), Bin(127, 160, Tip, Tip), Bin(768, 879, Tip, Tip), Bin(1155, 1158, Tip, Tip), Bin(1160, 1161, Tip, Tip), Bin(1425, 1469, Tip, Tip), Bin(1471, 1471, Tip, Tip), Bin(1473, 1474, Tip, Tip), Bin(1476, 1477, Tip, Tip), Bin(1479, 1479, Tip, Tip), Bin(1536, 1539, Tip, Tip), Bin(1552, 1557, Tip, Tip), Bin(1611, 1630, Tip, Tip), Bin(1648, 1648, Tip, Tip), Bin(1750, 1764, Tip, Tip), Bin(1767, 1768, Tip, Tip), Bin(1770, 1773, Tip, Tip), Bin(1807, 1807, Tip, Tip), Bin(1809, 1809, Tip, Tip), Bin(1840, 1866, Tip, Tip), Bin(1958, 1968, Tip, Tip), Bin(2027, 2035, Tip, Tip), Bin(2305, 2306, Tip, Tip), Bin(2364, 2364, Tip, Tip), Bin(2369, 2376, Tip, Tip), Bin(2381, 2381, Tip, Tip), Bin(2385, 2388, Tip, Tip), Bin(2402, 2403, Tip, Tip), Bin(2433, 2433, Tip, Tip), Bin(2492, 2492, Tip, Tip), Bin(2497, 2500, Tip, Tip), Bin(2509, 2509, Tip, Tip), Bin(2530, 2531, Tip, Tip), Bin(2561, 2562, Tip, Tip), Bin(2620, 2620, Tip, Tip), Bin(2625, 2626, Tip, Tip), Bin(2631, 2632, Tip, Tip), Bin(2635, 2637, Tip, Tip), Bin(2672, 2673, Tip, Tip), Bin(2689, 2690, Tip, Tip), Bin(2748, 2748, Tip, Tip), Bin(2753, 2757, Tip, Tip), Bin(2759, 2760, Tip, Tip), Bin(2765, 2765, Tip, Tip), Bin(2786, 2787, Tip, Tip), Bin(2817, 2817, Tip, Tip), Bin(2876, 2876, Tip, Tip), Bin(2879, 2879, Tip, Tip), Bin(2881, 2883, Tip, Tip), Bin(2893, 2893, Tip, Tip), Bin(2902, 2902, Tip, Tip), Bin(2946, 2946, Tip, Tip), Bin(3008, 3008, Tip, Tip), Bin(3021, 3021, Tip, Tip), Bin(3134, 3136, Tip, Tip), Bin(3142, 3144, Tip, Tip), Bin(3146, 3149, Tip, Tip), Bin(3157, 3158, Tip, Tip), Bin(3260, 3260, Tip, Tip), Bin(3263, 3263, Tip, Tip), Bin(3270, 3270, Tip, Tip), Bin(3276, 3277, Tip, Tip), Bin(3298, 3299, Tip, Tip), Bin(3393, 3395, Tip, Tip), Bin(3405, 3405, Tip, Tip), Bin(3530, 3530, Tip, Tip), Bin(3538, 3540, Tip, Tip), Bin(3542, 3542, Tip, Tip), Bin(3633, 3633, Tip, Tip), Bin(3636, 3642, Tip, Tip), Bin(3655, 3662, Tip, Tip), Bin(3761, 3761, Tip, Tip), Bin(3764, 3769, Tip, Tip), Bin(3771, 3772, Tip, Tip), Bin(3784, 3789, Tip, Tip), Bin(3864, 3865, Tip, Tip), Bin(3893, 3893, Tip, Tip), Bin(3895, 3895, Tip, Tip), Bin(3897, 3897, Tip, Tip), Bin(3953, 3966, Tip, Tip), Bin(3968, 3972, Tip, Tip), Bin(3974, 3975, Tip, Tip), Bin(3984, 3991, Tip, Tip), Bin(3993, 4028, Tip, Tip), Bin(4038, 4038, Tip, Tip), Bin(4141, 4144, Tip, Tip), Bin(4146, 4146, Tip, Tip), Bin(4150, 4151, Tip, Tip), Bin(4153, 4153, Tip, Tip), Bin(4184, 4185, Tip, Tip), Bin(4448, 4607, Tip, Tip), Bin(4959, 4959, Tip, Tip), Bin(5906, 5908, Tip, Tip), Bin(5938, 5940, Tip, Tip), Bin(5970, 5971, Tip, Tip), Bin(6002, 6003, Tip, Tip), Bin(6068, 6069, Tip, Tip), Bin(6071, 6077, Tip, Tip), Bin(6086, 6086, Tip, Tip), Bin(6089, 6099, Tip, Tip), Bin(6109, 6109, Tip, Tip), Bin(6155, 6157, Tip, Tip), Bin(6313, 6313, Tip, Tip), Bin(6432, 6434, Tip, Tip), Bin(6439, 6440, Tip, Tip), Bin(6450, 6450, Tip, Tip), Bin(6457, 6459, Tip, Tip), Bin(6679, 6680, Tip, Tip), Bin(6912, 6915, Tip, Tip), Bin(6964, 6964, Tip, Tip), Bin(6966, 6970, Tip, Tip), Bin(6972, 6972, Tip, Tip), Bin(6978, 6978, Tip, Tip), Bin(7019, 7027, Tip, Tip), Bin(7616, 7626, Tip, Tip), Bin(7678, 7679, Tip, Tip), Bin(8203, 8207, Tip, Tip), Bin(8234, 8238, Tip, Tip), Bin(8288, 8291, Tip, Tip), Bin(8298, 8303, Tip, Tip), Bin(8400, 8431, Tip, Tip), Bin(12330, 12335, Tip, Tip), Bin(12441, 12442, Tip, Tip), Bin(43014, 43014, Tip, Tip), Bin(43019, 43019, Tip, Tip), Bin(43045, 43046, Tip, Tip), Bin(64286, 64286, Tip, Tip), Bin(65024, 65039, Tip, Tip), Bin(65056, 65059, Tip, Tip), Bin(65279, 65279, Tip, Tip), Bin(65529, 65531, Tip, Tip), Bin(68097, 68099, Tip, Tip), Bin(68101, 68102, Tip, Tip), Bin(68108, 68111, Tip, Tip), Bin(68152, 68154, Tip, Tip), Bin(68159, 68159, Tip, Tip), Bin(119143, 119145, Tip, Tip), Bin(119155, 119170, Tip, Tip), Bin(119173, 119179, Tip, Tip), Bin(119210, 119213, Tip, Tip), Bin(119362, 119364, Tip, Tip), Bin(917505, 917505, Tip, Tip), Bin(917536, 917631, Tip, Tip), Bin(917760, 917999, Tip, Tip)], $std_core_types.Nil));
});
 
 
// Automatically generated. Tests for the `Tip` constructor of the `:rtree` type.
export function is_tip(rtree) /* (rtree : rtree) -> bool */  {
  return (rtree === null);
}
 
 
// Automatically generated. Tests for the `Bin` constructor of the `:rtree` type.
export function is_bin(rtree) /* (rtree : rtree) -> bool */  {
  return (rtree !== null);
}
 
 
// Is a value contained in a range tree?
export function contains(t, i) /* (t : rtree, i : int) -> bool */  { tailcall: while(1)
{
  if (t === null) {
    return true;
  }
  else {
    if ($std_core_types._int_lt(i,(t.lo))) {
      return false;
    }
    else {
      if ($std_core_types._int_gt(i,(t.hi))) {
        return false;
      }
      else {
        var _x16 = contains(t.left, i);
        if (_x16) {
          return true;
        }
        else {
          {
            // tail call
            t = t.right;
            continue tailcall;
          }
        }
      }
    }
  }
}}
 
 
// Return the column-width of a unicode character.
// Equivalent to ``wcwidth``
export function char_fs_width(c) /* (c : char) -> int */  {
  var _x17 = contains($std_core_delayed.unsafe_no_state_div_cast(function() {
      return $std_core_delayed.force_fs_go(zero_widths);
    })(), c);
  if (_x17) {
    return 0;
  }
  else {
    var _x18 = contains($std_core_delayed.unsafe_no_state_div_cast(function() {
        return $std_core_delayed.force_fs_go(asian_wide);
      })(), c);
    return (_x18) ? 2 : 1;
  }
}
 
 
// Return the total column-width of a string.
export function string_fs_width(s) /* (s : string) -> int */  {
  return function() {
     
    var loc = { value: 0 };
     
    $std_core_sslice.string_fs_foreach(s, function(c /* char */ ) {
         
        var x_10328 = ((loc).value);
         
        var y_10329 = char_fs_width(c);
        return ((loc).value = ($std_core_types._int_add(x_10328,y_10329)));
      });
     
    var res = ((loc).value);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
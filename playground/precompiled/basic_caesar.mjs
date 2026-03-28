// Koka generated module: basic/caesar, koka version: 3.2.4
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
import * as $std_num_float64 from './std_num_float64.mjs';
 
// externals
 
// type declarations
 
// declarations
 
export function encode(s, shift) /* (s : string, shift : ? int) -> string */  {
  return $std_core_list.string_fs_map(s, function(c /* char */ ) {
      if ((c < 0x0061)) {
        return c;
      }
      else {
        if ((c > 0x007A)) {
          return c;
        }
        else {
           
          var x_10002 = c;
           
          var y_10003 = 0x0061;
           
          var base = ((($std_core_types._int_sub(x_10002,y_10003))));
           
          var _x0 = (shift !== undefined) ? shift : 3;
          var rot = $std_core_types._int_mod(($std_core_types._int_add(base,_x0)),26);
           
          var c_1_10004 = (rot);
           
          var x_10000 = c_1_10004;
           
          var y_10001 = 0x0061;
          return (($std_core_types._int_add(x_10000,y_10001)));
        }
      }
    });
}
 
export function caesar(s) /* (s : string) -> string */  {
  return encode(s, 3);
}
 
 
// The letter frequency table for English
export var english;
var english = $std_core_vector.vlist([8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1], $std_core_types.Nil);
 
 
// Small helper functions
export function percent(n, m) /* (n : int, m : int) -> float64 */  {
  return ((100.0) * ((($std_core_types._int_to_double(n)) / ($std_core_types._int_to_double(m)))));
}
 
export function rotate(xs, n) /* forall<a> (xs : list<a>, n : int) -> list<a> */  {
   
  var xs_0_10006 = $std_core_list.drop(xs, n);
   
  var ys_10007 = $std_core_list.take(xs, n);
  return $std_core_list.append(xs_0_10006, ys_10007);
}
 
 
// Calculate a frequency table for a string
export function freqs(s) /* (s : string) -> list<float64> */  {
   
  var lowers = $std_core_list.map($std_core_list.range_fs_list(0x0061, 0x007A), (function(_x0) {
      return (_x0);
    }));
   
  var occurs = $std_core_list.map(lowers, function(c /* char */ ) {
      return ((($std_core_string.char_fs_string(c))) ? ((s).match(new RegExp((($std_core_string.char_fs_string(c))).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'))||[]).length : 0);
    });
   
  var total = $std_core_list.foldl(occurs, 0, $std_core_int._lp__plus__rp_);
  return $std_core_list.map(occurs, function(i /* int */ ) {
      return ((100.0) * ((($std_core_types._int_to_double(i)) / ($std_core_types._int_to_double(total)))));
    });
}
 
 
// Calculate how well two frequency tables match according
// to the _chi-square_ statistic.
export function chisqr(xs, ys) /* (xs : list<float64>, ys : list<float64>) -> float64 */  {
  return $std_num_float64.sum($std_core_list.zipwith(xs, ys, function(x /* float64 */ , y /* float64 */ ) {
      return ((Math.pow(((x - y)),(2.0))) / y);
    }));
}
 
 
// Crack a Caesar encoded string
export function uncaesar(s) /* (s : string) -> string */  {
   
  var table = freqs(s);
   
  var chitab = $std_core_list.map($std_core_list.range_fs_list(0, 25), function(n /* int */ ) {
       
      var xs_1_10017 = $std_core_list.drop(table, n);
       
      var ys_0_10018 = $std_core_list.take(table, n);
       
      var xs_10013 = $std_core_list.append(xs_1_10017, ys_0_10018);
      return $std_num_float64.sum($std_core_list.zipwith(xs_10013, english, function(x /* float64 */ , y /* float64 */ ) {
          return ((Math.pow(((x - y)),(2.0))) / y);
        }));
    });
   
  if (chitab === null) {
    var min = 0.0;
  }
  else {
    var min = $std_core_list.foldl(chitab.tail, chitab.head, $std_num_float64.min);
  }
   
  var shift = $std_core_list.index_of(chitab, function(f /* float64 */ ) {
      return (f === min);
    });
  return encode(s, $std_core_types._int_negate(shift));
}
 
 
// test an example
export function example_uncaesar() /* () -> console/console () */  {
   
  var s_10020 = uncaesar("nrnd lv d ixq odqjxdjh");
  return $std_core_console.printsln(s_10020);
}
 
export function main() /* () -> console/console () */  {
   
  var s_10021 = $std_core_types._lp__plus__plus__rp_("plain  : ", "Koka is a well-typed language");
   
  $std_core_console.printsln(s_10021);
   
  var s_0_10022 = $std_core_types._lp__plus__plus__rp_("encoded: ", encode("Koka is a well-typed language", 3));
   
  $std_core_console.printsln(s_0_10022);
   
  var s_2_10024 = $std_core_types._lp__plus__plus__rp_("cracked: ", uncaesar(encode("Koka is a well-typed language", 3)));
  return $std_core_console.printsln(s_2_10024);
}
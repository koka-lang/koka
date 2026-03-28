// Koka generated module: std/core/maybe, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_exn from './std_core_exn.mjs';
 
// externals
 
// type declarations
 
// declarations
 
 
// Match a `:maybe` value and either return a default value on `Nothing` or apply a function to the value on `Just`
export function maybe(m, onNothing, onJust) /* forall<a,b,e> (m : maybe<a>, onNothing : b, onJust : (a) -> e b) -> e b */  {
  if (m === null) {
    return onNothing;
  }
  else {
    return onJust(m.value);
  }
}
 
 
// Convert a `:maybe<a>` value to `:a`, using the `nothing` parameter for `Nothing`.
export function $default(m, nothing) /* forall<a> (m : maybe<a>, nothing : a) -> a */  {
  return (m === null) ? nothing : m.value;
}
 
 
// Get the value of the `Just` constructor or raise an exception
export function unjust(m, _implicit_fs_kk_file_line) /* forall<a> (m : maybe<a>, ?kk-file-line : string) -> exn a */  {
  if (m !== null) {
    return m.value;
  }
  else {
    return $std_core_exn.$throw($std_core_types._lp__plus__plus__rp_("unexpected Nothing in ", _implicit_fs_kk_file_line));
  }
}
 
 
// Get the value of the `Just` constructor or raise an exception with `error-msg`
export function expect(m, error_msg) /* forall<a> (m : maybe<a>, error-msg : string) -> exn a */  {
  if (m !== null) {
    return m.value;
  }
  else {
    return $std_core_exn.$throw(error_msg);
  }
}
 
 
// monadic lift
export function _mlift_map_10019(_y_x10006) /* forall<a,e> (a) -> e maybe<a> */  {
  return $std_core_types.Just(_y_x10006);
}
 
export function map(m, f) /* forall<a,b,e> (m : maybe<a>, f : (a) -> e b) -> e maybe<b> */  {
  if (m === null) {
    return $std_core_types.Nothing;
  }
  else {
     
    var x_0_10022 = f(m.value);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10006 /* 273 */ ) {
        return $std_core_types.Just(_y_x10006);
      });
    }
    else {
      return $std_core_types.Just(x_0_10022);
    }
  }
}
 
export function _lp__bar__bar__rp_(m1, m2) /* forall<a> (m1 : maybe<a>, m2 : maybe<a>) -> maybe<a> */  {
  return (m1 === null) ? m2 : m1;
}
 
export function flatten(mm) /* forall<a> (mm : maybe<maybe<a>>) -> maybe<a> */  {
  if (mm !== null && mm.value !== null) {
    return $std_core_types.Just(mm.value.value);
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// Equality on `:maybe`
export function _lp__eq__eq__rp_(mb1, mb2, _implicit_fs__lp__eq__eq__rp_) /* forall<a,e> (mb1 : maybe<a>, mb2 : maybe<a>, ?(==) : (a, a) -> e bool) -> e bool */  {
  if (mb1 !== null) {
    if (mb2 !== null) {
      return _implicit_fs__lp__eq__eq__rp_(mb1.value, mb2.value);
    }
    else {
      return false;
    }
  }
  else {
    return (mb2 === null);
  }
}
 
 
// Order on `:maybe` values
export function cmp(mb1, mb2, _implicit_fs_cmp) /* forall<a,e> (mb1 : maybe<a>, mb2 : maybe<a>, ?cmp : (a, a) -> e order) -> e order */  {
  if (mb1 !== null) {
    if (mb2 !== null) {
      return _implicit_fs_cmp(mb1.value, mb2.value);
    }
    else {
      return $std_core_types.Gt;
    }
  }
  else {
    return (mb2 === null) ? $std_core_types.Eq : $std_core_types.Lt;
  }
}
 
 
// monadic lift
export function _mlift_order2_10020(_y_x10014) /* forall<a,e> (order2<a>) -> e order2<maybe<a>> */  {
  if (_y_x10014._tag === 2) {
    return $std_core_types.Eq2($std_core_types.Just(_y_x10014.eq));
  }
  else if (_y_x10014._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Just(_y_x10014.lt), $std_core_types.Just(_y_x10014.gt));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Just(_y_x10014.lt), $std_core_types.Just(_y_x10014.gt));
  }
}
 
 
// Order two `:maybe` values in ascending order
export function order2(mb1, mb2, _implicit_fs_order2) /* forall<a,e> (mb1 : maybe<a>, mb2 : maybe<a>, ?order2 : (a, a) -> e order2<a>) -> e order2<maybe<a>> */  {
  if (mb1 !== null) {
    if (mb2 !== null) {
       
      var x_0_10026 = _implicit_fs_order2(mb1.value, mb2.value);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10014 /* order2<619> */ ) {
          return _mlift_order2_10020(_y_x10014);
        });
      }
      else {
        if (x_0_10026._tag === 2) {
          return $std_core_types.Eq2($std_core_types.Just(x_0_10026.eq));
        }
        else if (x_0_10026._tag === 1) {
          return $std_core_types.Lt2($std_core_types.Just(x_0_10026.lt), $std_core_types.Just(x_0_10026.gt));
        }
        else {
          return $std_core_types.Gt2($std_core_types.Just(x_0_10026.lt), $std_core_types.Just(x_0_10026.gt));
        }
      }
    }
    else {
      return $std_core_types.Gt2($std_core_types.Nothing, $std_core_types.Just(mb1.value));
    }
  }
  else {
    return $std_core_types.Lt2($std_core_types.Nothing, mb2);
  }
}
 
 
// monadic lift
export function _mlift_show_10021(_y_x10017) /* forall<e> (string) -> e string */  {
  return $std_core_types._lp__plus__plus__rp_("Just(", $std_core_types._lp__plus__plus__rp_(_y_x10017, ")"));
}
 
 
// Show a `:maybe` type
export function show(mb, _implicit_fs_show) /* forall<a,e> (mb : maybe<a>, ?show : (a) -> e string) -> e string */  {
  if (mb !== null) {
     
    var x_0_10029 = _implicit_fs_show(mb.value);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10017 /* string */ ) {
        return $std_core_types._lp__plus__plus__rp_("Just(", $std_core_types._lp__plus__plus__rp_(_y_x10017, ")"));
      });
    }
    else {
      return $std_core_types._lp__plus__plus__rp_("Just(", $std_core_types._lp__plus__plus__rp_(x_0_10029, ")"));
    }
  }
  else {
    return "Nothing";
  }
}
 
 
// Convert a maybe type to a boolean, equivalent to `is-just`.
export function bool(mb) /* forall<a> (mb : maybe<a>) -> bool */  {
  return (mb !== null);
}
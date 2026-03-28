// Koka generated module: std/core/maybe2, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_exn from './std_core_exn.mjs';
 
// externals
 
// type declarations
 
// declarations
 
 
// Match a `:maybe2` value and either return a default value on `Nothing2` or apply a function to the value on `Just2`
export function maybe2(m, onNothing, onJust) /* forall<a,b,c,e> (m : maybe2<a,b>, onNothing : c, onJust : (a, b) -> e c) -> e c */  {
  if (m._tag === 1) {
    return onNothing;
  }
  else {
    return onJust(m.fst, m.snd);
  }
}
 
 
// Convert a `:maybe2<a,b>` value to `:(a,b)`, using the `nothing` parameter for `Nothing2`.
export function $default(m, nothing) /* forall<a,b> (m : maybe2<a,b>, nothing : (a, b)) -> (a, b) */  {
  if (m._tag === 1) {
    return nothing;
  }
  else {
    return $std_core_types.Tuple2(m.fst, m.snd);
  }
}
 
 
// Get the value of the `Just` constructor or raise an exception
export function unjust(m, _implicit_fs_kk_file_line) /* forall<a,b> (m : maybe2<a,b>, ?kk-file-line : string) -> exn (a, b) */  {
  if (m._tag === 2) {
    return $std_core_types.Tuple2(m.fst, m.snd);
  }
  else {
    return $std_core_exn.$throw($std_core_types._lp__plus__plus__rp_("unexpected Nothing2 in ", _implicit_fs_kk_file_line));
  }
}
 
 
// Get the value of the `Just` constructor or raise an exception
export function expect(m, error_msg) /* forall<a,b> (m : maybe2<a,b>, error-msg : string) -> exn (a, b) */  {
  if (m._tag === 2) {
    return $std_core_types.Tuple2(m.fst, m.snd);
  }
  else {
    return $std_core_exn.$throw(error_msg);
  }
}
 
 
// monadic lift
export function _mlift_map_10021(_y_x10006) /* forall<a,b,e> ((a, b)) -> e maybe2<a,b> */  {
  return $std_core_types.Just2(_y_x10006.fst, _y_x10006.snd);
}
 
export function map(m, f) /* forall<a,b,c,d,e> (m : maybe2<a,b>, f : (a, b) -> e (c, d)) -> e maybe2<c,d> */  {
  if (m._tag === 1) {
    return $std_core_types.Nothing2;
  }
  else {
     
    var x_0_10026 = f(m.fst, m.snd);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10006 /* (427, 428) */ ) {
        return $std_core_types.Just2(_y_x10006.fst, _y_x10006.snd);
      });
    }
    else {
      return $std_core_types.Just2(x_0_10026.fst, x_0_10026.snd);
    }
  }
}
 
export function _lp__bar__bar__rp_(m1, m2) /* forall<a,b> (m1 : maybe2<a,b>, m2 : maybe2<a,b>) -> maybe2<a,b> */  {
  return (m1._tag === 1) ? m2 : m1;
}
 
 
// monadic lift
export function _lp__at_mlift_x_10022_eq__eq__rp_(_implicit_fs_snd_fs__lp__eq__eq__rp_, b, y, _y_x10008) /* forall<a,e> (?snd/(==) : (a, a) -> e bool, b : a, y : a, bool) -> e bool */  {
  if (_y_x10008) {
    return _implicit_fs_snd_fs__lp__eq__eq__rp_(y, b);
  }
  else {
    return false;
  }
}
 
 
// Equality on `:maybe`
export function _lp__eq__eq__rp_(mb1, mb2, _implicit_fs_fst_fs__lp__eq__eq__rp_, _implicit_fs_snd_fs__lp__eq__eq__rp_) /* forall<a,b,e> (mb1 : maybe2<a,b>, mb2 : maybe2<a,b>, ?fst/(==) : (a, a) -> e bool, ?snd/(==) : (b, b) -> e bool) -> e bool */  {
  if (mb1._tag === 2) {
    if (mb2._tag === 2) {
       
      var x_0_10030 = _implicit_fs_fst_fs__lp__eq__eq__rp_(mb1.fst, mb2.fst);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10008 /* bool */ ) {
          if (_y_x10008) {
            return _implicit_fs_snd_fs__lp__eq__eq__rp_(mb1.snd, mb2.snd);
          }
          else {
            return false;
          }
        });
      }
      else {
        if (x_0_10030) {
          return _implicit_fs_snd_fs__lp__eq__eq__rp_(mb1.snd, mb2.snd);
        }
        else {
          return false;
        }
      }
    }
    else {
      return false;
    }
  }
  else {
    return (mb2._tag === 1);
  }
}
 
 
// monadic lift
export function _mlift_cmp_10023(b, _implicit_fs_snd_fs_cmp, y, _y_x10013) /* forall<a,e> (b : a, ?snd/cmp : (a, a) -> e order, y : a, order) -> e order */  {
  if (_y_x10013 === 2) {
    return _implicit_fs_snd_fs_cmp(y, b);
  }
  else {
    return _y_x10013;
  }
}
 
 
// Order on `:maybe` values
export function cmp(mb1, mb2, _implicit_fs_fst_fs_cmp, _implicit_fs_snd_fs_cmp) /* forall<a,b,e> (mb1 : maybe2<a,b>, mb2 : maybe2<a,b>, ?fst/cmp : (a, a) -> e order, ?snd/cmp : (b, b) -> e order) -> e order */  {
  if (mb1._tag === 2) {
    if (mb2._tag === 2) {
       
      var x_0_10037 = _implicit_fs_fst_fs_cmp(mb1.fst, mb2.fst);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10013 /* order */ ) {
          if (_y_x10013 === 2) {
            return _implicit_fs_snd_fs_cmp(mb1.snd, mb2.snd);
          }
          else {
            return _y_x10013;
          }
        });
      }
      else {
        if (x_0_10037 === 2) {
          return _implicit_fs_snd_fs_cmp(mb1.snd, mb2.snd);
        }
        else {
          return x_0_10037;
        }
      }
    }
    else {
      return $std_core_types.Gt;
    }
  }
  else {
    return (mb2._tag === 1) ? $std_core_types.Eq : $std_core_types.Lt;
  }
}
 
 
// monadic lift
export function _mlift_show_10024(_y_x10018, _y_x10019) /* forall<e> (string, string) -> e string */  {
  return $std_core_types._lp__plus__plus__rp_("Just2(", $std_core_types._lp__plus__plus__rp_(_y_x10018, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10019, ")"))));
}
 
 
// monadic lift
export function _mlift_show_10025(_implicit_fs_snd_fs_show, y, _y_x10018) /* forall<a,e> (?snd/show : (a) -> e string, y : a, string) -> e string */  {
   
  var x_10044 = _implicit_fs_snd_fs_show(y);
   
  function next_10045(_y_x10019) /* (string) -> 754 string */  {
    return $std_core_types._lp__plus__plus__rp_("Just2(", $std_core_types._lp__plus__plus__rp_(_y_x10018, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10019, ")"))));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10045);
  }
  else {
    return next_10045(x_10044);
  }
}
 
 
// Show a `:maybe2` type
export function show(mb, _implicit_fs_fst_fs_show, _implicit_fs_snd_fs_show) /* forall<a,b,e> (mb : maybe2<a,b>, ?fst/show : (a) -> e string, ?snd/show : (b) -> e string) -> e string */  {
  if (mb._tag === 2) {
     
    var x_0_10048 = _implicit_fs_fst_fs_show(mb.fst);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10018 /* string */ ) {
        return _mlift_show_10025(_implicit_fs_snd_fs_show, mb.snd, _y_x10018);
      });
    }
    else {
       
      var x_1_10051 = _implicit_fs_snd_fs_show(mb.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10019 /* string */ ) {
          return $std_core_types._lp__plus__plus__rp_("Just2(", $std_core_types._lp__plus__plus__rp_(x_0_10048, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10019, ")"))));
        });
      }
      else {
        return $std_core_types._lp__plus__plus__rp_("Just2(", $std_core_types._lp__plus__plus__rp_(x_0_10048, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_1_10051, ")"))));
      }
    }
  }
  else {
    return "Nothing2";
  }
}
 
 
// Convert a `:maybe2` type to a boolean, equivalent to `is-just2`.
export function bool(mb) /* forall<a,b> (mb : maybe2<a,b>) -> bool */  {
  return (mb._tag === 2);
}
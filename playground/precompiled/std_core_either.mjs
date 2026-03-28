// Koka generated module: std/core/either, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
 
// externals
 
// type declarations
 
// declarations
 
 
// Convert a `:either` to a `:maybe` type discarding the value of the `Left` constructor
// and using `Just` for the `Right` constructor.
export function maybe(e) /* forall<a,b> (e : either<a,b>) -> maybe<b> */  {
  if (e._tag === 1) {
    return $std_core_types.Nothing;
  }
  else {
    return $std_core_types.Just(e.right);
  }
}
 
 
// monadic lift
export function _mlift_map_10010(_y_x10000) /* forall<a,b,e> (b) -> e either<a,b> */  {
  return $std_core_types.Right(_y_x10000);
}
 
 
// Map over the `Right` component of an `:either` type.
export function map(e, f) /* forall<a,b,c,e> (e : either<a,b>, f : (b) -> e c) -> e either<a,c> */  {
  if (e._tag === 2) {
     
    var x_0_10013 = f(e.right);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10000 /* 166 */ ) {
        return $std_core_types.Right(_y_x10000);
      });
    }
    else {
      return $std_core_types.Right(x_0_10013);
    }
  }
  else {
    return $std_core_types.Left(e.left);
  }
}
 
 
// Equality on `:either` values
export function _lp__eq__eq__rp_(e1, e2, _implicit_fs_left_fs__lp__eq__eq__rp_, _implicit_fs_right_fs__lp__eq__eq__rp_) /* forall<a,b,e> (e1 : either<a,b>, e2 : either<a,b>, ?left/(==) : (a, a) -> e bool, ?right/(==) : (b, b) -> e bool) -> e bool */  {
  if (e1._tag === 1) {
    if (e2._tag === 1) {
      return _implicit_fs_left_fs__lp__eq__eq__rp_(e1.left, e2.left);
    }
    else {
      return false;
    }
  }
  else {
    if (e2._tag === 2) {
      return _implicit_fs_right_fs__lp__eq__eq__rp_(e1.right, e2.right);
    }
    else {
      return false;
    }
  }
}
 
 
// monadic lift
export function _mlift_show_10011(_y_x10007) /* forall<e> (string) -> e string */  {
  return $std_core_types._lp__plus__plus__rp_("Right(", $std_core_types._lp__plus__plus__rp_(_y_x10007, ")"));
}
 
 
// monadic lift
export function _mlift_show_10012(_y_x10008) /* forall<e> (string) -> e string */  {
  return $std_core_types._lp__plus__plus__rp_("Left(", $std_core_types._lp__plus__plus__rp_(_y_x10008, ")"));
}
 
 
// Show an `:either` type
export function show(e, _implicit_fs_left_fs_show, _implicit_fs_right_fs_show) /* forall<a,b,e> (e : either<a,b>, ?left/show : (a) -> e string, ?right/show : (b) -> e string) -> e string */  {
  if (e._tag === 2) {
     
    var x_0_10017 = _implicit_fs_right_fs_show(e.right);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10007 /* string */ ) {
        return $std_core_types._lp__plus__plus__rp_("Right(", $std_core_types._lp__plus__plus__rp_(_y_x10007, ")"));
      });
    }
    else {
      return $std_core_types._lp__plus__plus__rp_("Right(", $std_core_types._lp__plus__plus__rp_(x_0_10017, ")"));
    }
  }
  else {
     
    var x_1_10021 = _implicit_fs_left_fs_show(e.left);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10008 /* string */ ) {
        return $std_core_types._lp__plus__plus__rp_("Left(", $std_core_types._lp__plus__plus__rp_(_y_x10008, ")"));
      });
    }
    else {
      return $std_core_types._lp__plus__plus__rp_("Left(", $std_core_types._lp__plus__plus__rp_(x_1_10021, ")"));
    }
  }
}
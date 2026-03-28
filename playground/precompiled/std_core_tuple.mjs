// Koka generated module: std/core/tuple, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
 
// externals
 
// type declarations
 
// declarations
 
 
// monadic lift
export function tuple2_fs__mlift_map_10153(_y_x10000, _y_x10001) /* forall<a,e> (a, a) -> e (a, a) */  {
  return $std_core_types.Tuple2(_y_x10000, _y_x10001);
}
 
 
// monadic lift
export function tuple2_fs__mlift_map_10154(f, t, _y_x10000) /* forall<a,b,e> (f : (a) -> e b, t : (a, a), b) -> e (b, b) */  {
   
  var _x0 = t.snd;
  var x_10215 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10001 /* 537 */ ) {
      return $std_core_types.Tuple2(_y_x10000, _y_x10001);
    });
  }
  else {
    return $std_core_types.Tuple2(_y_x10000, x_10215);
  }
}
 
 
// Map a function over a tuple of elements of the same type.
export function tuple2_fs_map(t, f) /* forall<a,b,e> (t : (a, a), f : (a) -> e b) -> e (b, b) */  {
   
  var _x0 = t.fst;
  var x_10219 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10000 /* 537 */ ) {
      return tuple2_fs__mlift_map_10154(f, t, _y_x10000);
    });
  }
  else {
     
    var _x0 = t.snd;
    var x_0_10222 = f(_x0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10001 /* 537 */ ) {
        return $std_core_types.Tuple2(x_10219, _y_x10001);
      });
    }
    else {
      return $std_core_types.Tuple2(x_10219, x_0_10222);
    }
  }
}
 
 
// monadic lift
export function tuple3_fs__mlift_map_10155(_y_x10002, _y_x10003, _y_x10004) /* forall<a,e> (a, a, a) -> e (a, a, a) */  {
  return $std_core_types.Tuple3(_y_x10002, _y_x10003, _y_x10004);
}
 
 
// monadic lift
export function tuple3_fs__mlift_map_10156(_y_x10002, f, t, _y_x10003) /* forall<a,b,e> (b, f : (a) -> e b, t : (a, a, a), b) -> e (b, b, b) */  {
   
  var _x0 = t.thd;
  var x_10227 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10004 /* 697 */ ) {
      return $std_core_types.Tuple3(_y_x10002, _y_x10003, _y_x10004);
    });
  }
  else {
    return $std_core_types.Tuple3(_y_x10002, _y_x10003, x_10227);
  }
}
 
 
// monadic lift
export function tuple3_fs__mlift_map_10157(f, t, _y_x10002) /* forall<a,b,e> (f : (a) -> e b, t : (a, a, a), b) -> e (b, b, b) */  {
   
  var _x0 = t.snd;
  var x_10232 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10003 /* 697 */ ) {
      return tuple3_fs__mlift_map_10156(_y_x10002, f, t, _y_x10003);
    });
  }
  else {
    return tuple3_fs__mlift_map_10156(_y_x10002, f, t, x_10232);
  }
}
 
 
// Map a function over a triple of elements of the same type.
export function tuple3_fs_map(t, f) /* forall<a,b,e> (t : (a, a, a), f : (a) -> e b) -> e (b, b, b) */  {
   
  var _x0 = t.fst;
  var x_10234 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10002 /* 697 */ ) {
      return tuple3_fs__mlift_map_10157(f, t, _y_x10002);
    });
  }
  else {
     
    var _x0 = t.snd;
    var x_0_10237 = f(_x0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10003 /* 697 */ ) {
        return tuple3_fs__mlift_map_10156(x_10234, f, t, _y_x10003);
      });
    }
    else {
       
      var _x0 = t.thd;
      var x_1_10240 = f(_x0);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10004 /* 697 */ ) {
          return $std_core_types.Tuple3(x_10234, x_0_10237, _y_x10004);
        });
      }
      else {
        return $std_core_types.Tuple3(x_10234, x_0_10237, x_1_10240);
      }
    }
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_map_10158(_y_x10005, _y_x10006, _y_x10007, _y_x10008) /* forall<a,e> (a, a, a, a) -> e (a, a, a, a) */  {
  return $std_core_types.Tuple4(_y_x10005, _y_x10006, _y_x10007, _y_x10008);
}
 
 
// monadic lift
export function tuple4_fs__mlift_map_10159(_y_x10005, _y_x10006, f, t, _y_x10007) /* forall<a,b,e> (b, b, f : (a) -> e b, t : (a, a, a, a), b) -> e (b, b, b, b) */  {
   
  var _x0 = t.field4;
  var x_10246 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10008 /* 903 */ ) {
      return $std_core_types.Tuple4(_y_x10005, _y_x10006, _y_x10007, _y_x10008);
    });
  }
  else {
    return $std_core_types.Tuple4(_y_x10005, _y_x10006, _y_x10007, x_10246);
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_map_10160(_y_x10005, f, t, _y_x10006) /* forall<a,b,e> (b, f : (a) -> e b, t : (a, a, a, a), b) -> e (b, b, b, b) */  {
   
  var _x0 = t.thd;
  var x_10252 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10007 /* 903 */ ) {
      return tuple4_fs__mlift_map_10159(_y_x10005, _y_x10006, f, t, _y_x10007);
    });
  }
  else {
    return tuple4_fs__mlift_map_10159(_y_x10005, _y_x10006, f, t, x_10252);
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_map_10161(f, t, _y_x10005) /* forall<a,b,e> (f : (a) -> e b, t : (a, a, a, a), b) -> e (b, b, b, b) */  {
   
  var _x0 = t.snd;
  var x_10254 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10006 /* 903 */ ) {
      return tuple4_fs__mlift_map_10160(_y_x10005, f, t, _y_x10006);
    });
  }
  else {
    return tuple4_fs__mlift_map_10160(_y_x10005, f, t, x_10254);
  }
}
 
 
// Map a function over a quadruple of elements of the same type.
export function tuple4_fs_map(t, f) /* forall<a,b,e> (t : (a, a, a, a), f : (a) -> e b) -> e (b, b, b, b) */  {
   
  var _x0 = t.fst;
  var x_10256 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10005 /* 903 */ ) {
      return tuple4_fs__mlift_map_10161(f, t, _y_x10005);
    });
  }
  else {
     
    var _x0 = t.snd;
    var x_0_10259 = f(_x0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10006 /* 903 */ ) {
        return tuple4_fs__mlift_map_10160(x_10256, f, t, _y_x10006);
      });
    }
    else {
       
      var _x0 = t.thd;
      var x_1_10262 = f(_x0);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10007 /* 903 */ ) {
          return tuple4_fs__mlift_map_10159(x_10256, x_0_10259, f, t, _y_x10007);
        });
      }
      else {
         
        var _x0 = t.field4;
        var x_2_10265 = f(_x0);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10008 /* 903 */ ) {
            return $std_core_types.Tuple4(x_10256, x_0_10259, x_1_10262, _y_x10008);
          });
        }
        else {
          return $std_core_types.Tuple4(x_10256, x_0_10259, x_1_10262, x_2_10265);
        }
      }
    }
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_map_10162(_y_x10009, _y_x10010, _y_x10011, _y_x10012, _y_x10013) /* forall<a,e> (a, a, a, a, a) -> e (a, a, a, a, a) */  {
  return $std_core_types.Tuple5(_y_x10009, _y_x10010, _y_x10011, _y_x10012, _y_x10013);
}
 
 
// monadic lift
export function tuple5_fs__mlift_map_10163(_y_x10009, _y_x10010, _y_x10011, f, t, _y_x10012) /* forall<a,b,e> (b, b, b, f : (a) -> e b, t : (a, a, a, a, a), b) -> e (b, b, b, b, b) */  {
   
  var _x0 = t.field5;
  var x_10272 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10013 /* 1140 */ ) {
      return $std_core_types.Tuple5(_y_x10009, _y_x10010, _y_x10011, _y_x10012, _y_x10013);
    });
  }
  else {
    return $std_core_types.Tuple5(_y_x10009, _y_x10010, _y_x10011, _y_x10012, x_10272);
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_map_10164(_y_x10009, _y_x10010, f, t, _y_x10011) /* forall<a,b,e> (b, b, f : (a) -> e b, t : (a, a, a, a, a), b) -> e (b, b, b, b, b) */  {
   
  var _x0 = t.field4;
  var x_10279 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10012 /* 1140 */ ) {
      return tuple5_fs__mlift_map_10163(_y_x10009, _y_x10010, _y_x10011, f, t, _y_x10012);
    });
  }
  else {
    return tuple5_fs__mlift_map_10163(_y_x10009, _y_x10010, _y_x10011, f, t, x_10279);
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_map_10165(_y_x10009, f, t, _y_x10010) /* forall<a,b,e> (b, f : (a) -> e b, t : (a, a, a, a, a), b) -> e (b, b, b, b, b) */  {
   
  var _x0 = t.thd;
  var x_10281 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10011 /* 1140 */ ) {
      return tuple5_fs__mlift_map_10164(_y_x10009, _y_x10010, f, t, _y_x10011);
    });
  }
  else {
    return tuple5_fs__mlift_map_10164(_y_x10009, _y_x10010, f, t, x_10281);
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_map_10166(f, t, _y_x10009) /* forall<a,b,e> (f : (a) -> e b, t : (a, a, a, a, a), b) -> e (b, b, b, b, b) */  {
   
  var _x0 = t.snd;
  var x_10283 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10010 /* 1140 */ ) {
      return tuple5_fs__mlift_map_10165(_y_x10009, f, t, _y_x10010);
    });
  }
  else {
    return tuple5_fs__mlift_map_10165(_y_x10009, f, t, x_10283);
  }
}
 
 
// Map a function over a quintuple of elements of the same type.
export function tuple5_fs_map(t, f) /* forall<a,b,e> (t : (a, a, a, a, a), f : (a) -> e b) -> e (b, b, b, b, b) */  {
   
  var _x0 = t.fst;
  var x_10285 = f(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10009 /* 1140 */ ) {
      return tuple5_fs__mlift_map_10166(f, t, _y_x10009);
    });
  }
  else {
     
    var _x0 = t.snd;
    var x_0_10288 = f(_x0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10010 /* 1140 */ ) {
        return tuple5_fs__mlift_map_10165(x_10285, f, t, _y_x10010);
      });
    }
    else {
       
      var _x0 = t.thd;
      var x_1_10291 = f(_x0);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10011 /* 1140 */ ) {
          return tuple5_fs__mlift_map_10164(x_10285, x_0_10288, f, t, _y_x10011);
        });
      }
      else {
         
        var _x0 = t.field4;
        var x_2_10294 = f(_x0);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10012 /* 1140 */ ) {
            return tuple5_fs__mlift_map_10163(x_10285, x_0_10288, x_1_10291, f, t, _y_x10012);
          });
        }
        else {
           
          var _x0 = t.field5;
          var x_3_10297 = f(_x0);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10013 /* 1140 */ ) {
              return $std_core_types.Tuple5(x_10285, x_0_10288, x_1_10291, x_2_10294, _y_x10013);
            });
          }
          else {
            return $std_core_types.Tuple5(x_10285, x_0_10288, x_1_10291, x_2_10294, x_3_10297);
          }
        }
      }
    }
  }
}
 
 
// Compare unit values. Useful to build composite equality for structures containing a unit (e.g. either<string, ()>)
export function unit_fs__lp__eq__eq__rp_(a, b) /* (a : (), b : ()) -> bool */  {
  return true;
}
 
 
// monadic lift
export function tuple2_fs__lp__at_mlift_x_10167_eq__eq__rp_(_implicit_fs_snd_fs__lp__eq__eq__rp_, y1, y2, _y_x10014) /* forall<a,e> (?snd/(==) : (a, a) -> e bool, y1 : a, y2 : a, bool) -> e bool */  {
  if (_y_x10014) {
    return _implicit_fs_snd_fs__lp__eq__eq__rp_(y1, y2);
  }
  else {
    return false;
  }
}
 
 
// Element-wise tuple equality
export function tuple2_fs__lp__eq__eq__rp_(_pat_x36__22, _pat_x36__39, _implicit_fs_fst_fs__lp__eq__eq__rp_, _implicit_fs_snd_fs__lp__eq__eq__rp_) /* forall<a,b,e> ((a, b), (a, b), ?fst/(==) : (a, a) -> e bool, ?snd/(==) : (b, b) -> e bool) -> e bool */  {
   
  var x_10305 = _implicit_fs_fst_fs__lp__eq__eq__rp_(_pat_x36__22.fst, _pat_x36__39.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10014 /* bool */ ) {
      if (_y_x10014) {
        return _implicit_fs_snd_fs__lp__eq__eq__rp_(_pat_x36__22.snd, _pat_x36__39.snd);
      }
      else {
        return false;
      }
    });
  }
  else {
    if (x_10305) {
      return _implicit_fs_snd_fs__lp__eq__eq__rp_(_pat_x36__22.snd, _pat_x36__39.snd);
    }
    else {
      return false;
    }
  }
}
 
 
// monadic lift
export function tuple3_fs__lp__at_mlift_x_10168_eq__eq__rp_(_implicit_fs_thd_fs__lp__eq__eq__rp_, z1, z2, _y_x10020) /* forall<a,e> (?thd/(==) : (a, a) -> e bool, z1 : a, z2 : a, bool) -> e bool */  {
  if (_y_x10020) {
    return _implicit_fs_thd_fs__lp__eq__eq__rp_(z1, z2);
  }
  else {
    return false;
  }
}
 
 
// monadic lift
export function tuple3_fs__lp__at_mlift_x_10169_eq__eq__rp_(_implicit_fs_snd_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, y1, y2, z1, z2, _y_x10019) /* forall<a,b,e> (?snd/(==) : (a, a) -> e bool, ?thd/(==) : (b, b) -> e bool, y1 : a, y2 : a, z1 : b, z2 : b, bool) -> e bool */  {
  if (_y_x10019) {
     
    var x_10312 = _implicit_fs_snd_fs__lp__eq__eq__rp_(y1, y2);
     
    var next_10313 = function(_y_x10020 /* bool */ ) {
      if (_y_x10020) {
        return _implicit_fs_thd_fs__lp__eq__eq__rp_(z1, z2);
      }
      else {
        return false;
      }
    };
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(next_10313);
    }
    else {
      return next_10313(x_10312);
    }
  }
  else {
    return false;
  }
}
 
 
// Element-wise triple equality
export function tuple3_fs__lp__eq__eq__rp_(_pat_x40__22, _pat_x40__44, _implicit_fs_fst_fs__lp__eq__eq__rp_, _implicit_fs_snd_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_) /* forall<a,b,c,e> ((a, b, c), (a, b, c), ?fst/(==) : (a, a) -> e bool, ?snd/(==) : (b, b) -> e bool, ?thd/(==) : (c, c) -> e bool) -> e bool */  {
   
  var x_10318 = _implicit_fs_fst_fs__lp__eq__eq__rp_(_pat_x40__22.fst, _pat_x40__44.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10019 /* bool */ ) {
      return tuple3_fs__lp__at_mlift_x_10169_eq__eq__rp_(_implicit_fs_snd_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, _pat_x40__22.snd, _pat_x40__44.snd, _pat_x40__22.thd, _pat_x40__44.thd, _y_x10019);
    });
  }
  else {
    if (x_10318) {
       
      var x_0_10321 = _implicit_fs_snd_fs__lp__eq__eq__rp_(_pat_x40__22.snd, _pat_x40__44.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10020 /* bool */ ) {
          if (_y_x10020) {
            return _implicit_fs_thd_fs__lp__eq__eq__rp_(_pat_x40__22.thd, _pat_x40__44.thd);
          }
          else {
            return false;
          }
        });
      }
      else {
        if (x_0_10321) {
          return _implicit_fs_thd_fs__lp__eq__eq__rp_(_pat_x40__22.thd, _pat_x40__44.thd);
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
}
 
 
// monadic lift
export function tuple4_fs__lp__at_mlift_x_10170_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, w1, w2, _y_x10028) /* forall<a,e> (?field4/(==) : (a, a) -> e bool, w1 : a, w2 : a, bool) -> e bool */  {
  if (_y_x10028) {
    return _implicit_fs_field4_fs__lp__eq__eq__rp_(w1, w2);
  }
  else {
    return false;
  }
}
 
 
// monadic lift
export function tuple4_fs__lp__at_mlift_x_10171_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, w1, w2, z1, z2, _y_x10027) /* forall<a,b,e> (?field4/(==) : (b, b) -> e bool, ?thd/(==) : (a, a) -> e bool, w1 : b, w2 : b, z1 : a, z2 : a, bool) -> e bool */  {
  if (_y_x10027) {
     
    var x_10328 = _implicit_fs_thd_fs__lp__eq__eq__rp_(z1, z2);
     
    var next_10329 = function(_y_x10028 /* bool */ ) {
      if (_y_x10028) {
        return _implicit_fs_field4_fs__lp__eq__eq__rp_(w1, w2);
      }
      else {
        return false;
      }
    };
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(next_10329);
    }
    else {
      return next_10329(x_10328);
    }
  }
  else {
    return false;
  }
}
 
 
// monadic lift
export function tuple4_fs__lp__at_mlift_x_10172_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_snd_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, w1, w2, y1, y2, z1, z2, _y_x10026) /* forall<a,b,c,e> (?field4/(==) : (c, c) -> e bool, ?snd/(==) : (a, a) -> e bool, ?thd/(==) : (b, b) -> e bool, w1 : c, w2 : c, y1 : a, y2 : a, z1 : b, z2 : b, bool) -> e bool */  {
  if (_y_x10026) {
     
    var x_10334 = _implicit_fs_snd_fs__lp__eq__eq__rp_(y1, y2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10027 /* bool */ ) {
        return tuple4_fs__lp__at_mlift_x_10171_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, w1, w2, z1, z2, _y_x10027);
      });
    }
    else {
      return tuple4_fs__lp__at_mlift_x_10171_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, w1, w2, z1, z2, x_10334);
    }
  }
  else {
    return false;
  }
}
 
 
// Element-wise quadruple equality
export function tuple4_fs__lp__eq__eq__rp_(_pat_x44__22, _pat_x44__49, _implicit_fs_fst_fs__lp__eq__eq__rp_, _implicit_fs_snd_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, _implicit_fs_field4_fs__lp__eq__eq__rp_) /* forall<a,b,c,d,e> ((a, b, c, d), (a, b, c, d), ?fst/(==) : (a, a) -> e bool, ?snd/(==) : (b, b) -> e bool, ?thd/(==) : (c, c) -> e bool, ?field4/(==) : (d, d) -> e bool) -> e bool */  {
   
  var x_10336 = _implicit_fs_fst_fs__lp__eq__eq__rp_(_pat_x44__22.fst, _pat_x44__49.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10026 /* bool */ ) {
      return tuple4_fs__lp__at_mlift_x_10172_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_snd_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, _pat_x44__22.field4, _pat_x44__49.field4, _pat_x44__22.snd, _pat_x44__49.snd, _pat_x44__22.thd, _pat_x44__49.thd, _y_x10026);
    });
  }
  else {
    if (x_10336) {
       
      var x_0_10339 = _implicit_fs_snd_fs__lp__eq__eq__rp_(_pat_x44__22.snd, _pat_x44__49.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10027 /* bool */ ) {
          return tuple4_fs__lp__at_mlift_x_10171_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, _pat_x44__22.field4, _pat_x44__49.field4, _pat_x44__22.thd, _pat_x44__49.thd, _y_x10027);
        });
      }
      else {
        if (x_0_10339) {
           
          var x_1_10342 = _implicit_fs_thd_fs__lp__eq__eq__rp_(_pat_x44__22.thd, _pat_x44__49.thd);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10028 /* bool */ ) {
              if (_y_x10028) {
                return _implicit_fs_field4_fs__lp__eq__eq__rp_(_pat_x44__22.field4, _pat_x44__49.field4);
              }
              else {
                return false;
              }
            });
          }
          else {
            if (x_1_10342) {
              return _implicit_fs_field4_fs__lp__eq__eq__rp_(_pat_x44__22.field4, _pat_x44__49.field4);
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
    }
    else {
      return false;
    }
  }
}
 
 
// monadic lift
export function tuple5_fs__lp__at_mlift_x_10173_eq__eq__rp_(_implicit_fs_field5_fs__lp__eq__eq__rp_, v1, v2, _y_x10038) /* forall<e,a> (?field5/(==) : (a, a) -> e bool, v1 : a, v2 : a, bool) -> e bool */  {
  if (_y_x10038) {
    return _implicit_fs_field5_fs__lp__eq__eq__rp_(v1, v2);
  }
  else {
    return false;
  }
}
 
 
// monadic lift
export function tuple5_fs__lp__at_mlift_x_10174_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_, v1, v2, w1, w2, _y_x10037) /* forall<a,e,b> (?field4/(==) : (a, a) -> e bool, ?field5/(==) : (b, b) -> e bool, v1 : b, v2 : b, w1 : a, w2 : a, bool) -> e bool */  {
  if (_y_x10037) {
     
    var x_10349 = _implicit_fs_field4_fs__lp__eq__eq__rp_(w1, w2);
     
    var next_10350 = function(_y_x10038 /* bool */ ) {
      if (_y_x10038) {
        return _implicit_fs_field5_fs__lp__eq__eq__rp_(v1, v2);
      }
      else {
        return false;
      }
    };
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(next_10350);
    }
    else {
      return next_10350(x_10349);
    }
  }
  else {
    return false;
  }
}
 
 
// monadic lift
export function tuple5_fs__lp__at_mlift_x_10175_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, v1, v2, w1, w2, z1, z2, _y_x10036) /* forall<a,b,e,c> (?field4/(==) : (b, b) -> e bool, ?field5/(==) : (c, c) -> e bool, ?thd/(==) : (a, a) -> e bool, v1 : c, v2 : c, w1 : b, w2 : b, z1 : a, z2 : a, bool) -> e bool */  {
  if (_y_x10036) {
     
    var x_10355 = _implicit_fs_thd_fs__lp__eq__eq__rp_(z1, z2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10037 /* bool */ ) {
        return tuple5_fs__lp__at_mlift_x_10174_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_, v1, v2, w1, w2, _y_x10037);
      });
    }
    else {
      return tuple5_fs__lp__at_mlift_x_10174_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_, v1, v2, w1, w2, x_10355);
    }
  }
  else {
    return false;
  }
}
 
 
// monadic lift
export function tuple5_fs__lp__at_mlift_x_10176_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_, _implicit_fs_snd_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, v1, v2, w1, w2, y1, y2, z1, z2, _y_x10035) /* forall<a,b,c,e,d> (?field4/(==) : (c, c) -> e bool, ?field5/(==) : (d, d) -> e bool, ?snd/(==) : (a, a) -> e bool, ?thd/(==) : (b, b) -> e bool, v1 : d, v2 : d, w1 : c, w2 : c, y1 : a, y2 : a, z1 : b, z2 : b, bool) -> e bool */  {
  if (_y_x10035) {
     
    var x_10357 = _implicit_fs_snd_fs__lp__eq__eq__rp_(y1, y2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10036 /* bool */ ) {
        return tuple5_fs__lp__at_mlift_x_10175_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, v1, v2, w1, w2, z1, z2, _y_x10036);
      });
    }
    else {
      return tuple5_fs__lp__at_mlift_x_10175_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, v1, v2, w1, w2, z1, z2, x_10357);
    }
  }
  else {
    return false;
  }
}
 
 
// Element-wise quintuple equality
export function tuple5_fs__lp__eq__eq__rp_(_pat_x50__22, _pat_x50__54, _implicit_fs_fst_fs__lp__eq__eq__rp_, _implicit_fs_snd_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, _implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_) /* forall<a,b,c,d,e,a1> ((a, b, c, d, a1), (a, b, c, d, a1), ?fst/(==) : (a, a) -> e bool, ?snd/(==) : (b, b) -> e bool, ?thd/(==) : (c, c) -> e bool, ?field4/(==) : (d, d) -> e bool, ?field5/(==) : (a1, a1) -> e bool) -> e bool */  {
   
  var x_10359 = _implicit_fs_fst_fs__lp__eq__eq__rp_(_pat_x50__22.fst, _pat_x50__54.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10035 /* bool */ ) {
      return tuple5_fs__lp__at_mlift_x_10176_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_, _implicit_fs_snd_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, _pat_x50__22.field5, _pat_x50__54.field5, _pat_x50__22.field4, _pat_x50__54.field4, _pat_x50__22.snd, _pat_x50__54.snd, _pat_x50__22.thd, _pat_x50__54.thd, _y_x10035);
    });
  }
  else {
    if (x_10359) {
       
      var x_0_10362 = _implicit_fs_snd_fs__lp__eq__eq__rp_(_pat_x50__22.snd, _pat_x50__54.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10036 /* bool */ ) {
          return tuple5_fs__lp__at_mlift_x_10175_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_, _implicit_fs_thd_fs__lp__eq__eq__rp_, _pat_x50__22.field5, _pat_x50__54.field5, _pat_x50__22.field4, _pat_x50__54.field4, _pat_x50__22.thd, _pat_x50__54.thd, _y_x10036);
        });
      }
      else {
        if (x_0_10362) {
           
          var x_1_10365 = _implicit_fs_thd_fs__lp__eq__eq__rp_(_pat_x50__22.thd, _pat_x50__54.thd);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10037 /* bool */ ) {
              return tuple5_fs__lp__at_mlift_x_10174_eq__eq__rp_(_implicit_fs_field4_fs__lp__eq__eq__rp_, _implicit_fs_field5_fs__lp__eq__eq__rp_, _pat_x50__22.field5, _pat_x50__54.field5, _pat_x50__22.field4, _pat_x50__54.field4, _y_x10037);
            });
          }
          else {
            if (x_1_10365) {
               
              var x_2_10368 = _implicit_fs_field4_fs__lp__eq__eq__rp_(_pat_x50__22.field4, _pat_x50__54.field4);
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(function(_y_x10038 /* bool */ ) {
                  if (_y_x10038) {
                    return _implicit_fs_field5_fs__lp__eq__eq__rp_(_pat_x50__22.field5, _pat_x50__54.field5);
                  }
                  else {
                    return false;
                  }
                });
              }
              else {
                if (x_2_10368) {
                  return _implicit_fs_field5_fs__lp__eq__eq__rp_(_pat_x50__22.field5, _pat_x50__54.field5);
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
}
 
 
// Order on unit
export function unit_fs_cmp(a, b) /* (a : (), b : ()) -> order */  {
  return $std_core_types.Eq;
}
 
 
// monadic lift
export function tuple2_fs__mlift_cmp_10177(_implicit_fs_snd_fs_cmp, y1, y2, _y_x10046) /* forall<a,e> (?snd/cmp : (a, a) -> e order, y1 : a, y2 : a, order) -> e order */  {
  if (_y_x10046 === 2) {
    return _implicit_fs_snd_fs_cmp(y1, y2);
  }
  else {
    return _y_x10046;
  }
}
 
 
// Order on tuples
export function tuple2_fs_cmp(_pat_x60__21, _pat_x60__38, _implicit_fs_fst_fs_cmp, _implicit_fs_snd_fs_cmp) /* forall<a,b,e> ((a, b), (a, b), ?fst/cmp : (a, a) -> e order, ?snd/cmp : (b, b) -> e order) -> e order */  {
   
  var x_10375 = _implicit_fs_fst_fs_cmp(_pat_x60__21.fst, _pat_x60__38.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10046 /* order */ ) {
      if (_y_x10046 === 2) {
        return _implicit_fs_snd_fs_cmp(_pat_x60__21.snd, _pat_x60__38.snd);
      }
      else {
        return _y_x10046;
      }
    });
  }
  else {
    if (x_10375 === 2) {
      return _implicit_fs_snd_fs_cmp(_pat_x60__21.snd, _pat_x60__38.snd);
    }
    else {
      return x_10375;
    }
  }
}
 
 
// monadic lift
export function tuple3_fs__mlift_cmp_10178(_implicit_fs_thd_fs_cmp, z1, z2, _y_x10052) /* forall<a,e> (?thd/cmp : (a, a) -> e order, z1 : a, z2 : a, order) -> e order */  {
  if (_y_x10052 === 2) {
    return _implicit_fs_thd_fs_cmp(z1, z2);
  }
  else {
    return _y_x10052;
  }
}
 
 
// monadic lift
export function tuple3_fs__mlift_cmp_10179(_implicit_fs_snd_fs_cmp, _implicit_fs_thd_fs_cmp, y1, y2, z1, z2, _y_x10051) /* forall<a,b,e> (?snd/cmp : (a, a) -> e order, ?thd/cmp : (b, b) -> e order, y1 : a, y2 : a, z1 : b, z2 : b, order) -> e order */  {
  if (_y_x10051 === 2) {
     
    var x_10382 = _implicit_fs_snd_fs_cmp(y1, y2);
     
    var next_10383 = function(_y_x10052 /* order */ ) {
      if (_y_x10052 === 2) {
        return _implicit_fs_thd_fs_cmp(z1, z2);
      }
      else {
        return _y_x10052;
      }
    };
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(next_10383);
    }
    else {
      return next_10383(x_10382);
    }
  }
  else {
    return _y_x10051;
  }
}
 
 
// Order on triples
export function tuple3_fs_cmp(_pat_x66__26, _pat_x66__48, _implicit_fs_fst_fs_cmp, _implicit_fs_snd_fs_cmp, _implicit_fs_thd_fs_cmp) /* forall<a,b,c,e> ((a, b, c), (a, b, c), ?fst/cmp : (a, a) -> e order, ?snd/cmp : (b, b) -> e order, ?thd/cmp : (c, c) -> e order) -> e order */  {
   
  var x_10388 = _implicit_fs_fst_fs_cmp(_pat_x66__26.fst, _pat_x66__48.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10051 /* order */ ) {
      return tuple3_fs__mlift_cmp_10179(_implicit_fs_snd_fs_cmp, _implicit_fs_thd_fs_cmp, _pat_x66__26.snd, _pat_x66__48.snd, _pat_x66__26.thd, _pat_x66__48.thd, _y_x10051);
    });
  }
  else {
    if (x_10388 === 2) {
       
      var x_0_10391 = _implicit_fs_snd_fs_cmp(_pat_x66__26.snd, _pat_x66__48.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10052 /* order */ ) {
          if (_y_x10052 === 2) {
            return _implicit_fs_thd_fs_cmp(_pat_x66__26.thd, _pat_x66__48.thd);
          }
          else {
            return _y_x10052;
          }
        });
      }
      else {
        if (x_0_10391 === 2) {
          return _implicit_fs_thd_fs_cmp(_pat_x66__26.thd, _pat_x66__48.thd);
        }
        else {
          return x_0_10391;
        }
      }
    }
    else {
      return x_10388;
    }
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_cmp_10180(_implicit_fs_field4_fs_cmp, w1, w2, _y_x10060) /* forall<a,e> (?field4/cmp : (a, a) -> e order, w1 : a, w2 : a, order) -> e order */  {
  if (_y_x10060 === 2) {
    return _implicit_fs_field4_fs_cmp(w1, w2);
  }
  else {
    return _y_x10060;
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_cmp_10181(_implicit_fs_field4_fs_cmp, _implicit_fs_thd_fs_cmp, w1, w2, z1, z2, _y_x10059) /* forall<a,b,e> (?field4/cmp : (b, b) -> e order, ?thd/cmp : (a, a) -> e order, w1 : b, w2 : b, z1 : a, z2 : a, order) -> e order */  {
  if (_y_x10059 === 2) {
     
    var x_10398 = _implicit_fs_thd_fs_cmp(z1, z2);
     
    var next_10399 = function(_y_x10060 /* order */ ) {
      if (_y_x10060 === 2) {
        return _implicit_fs_field4_fs_cmp(w1, w2);
      }
      else {
        return _y_x10060;
      }
    };
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(next_10399);
    }
    else {
      return next_10399(x_10398);
    }
  }
  else {
    return _y_x10059;
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_cmp_10182(_implicit_fs_field4_fs_cmp, _implicit_fs_snd_fs_cmp, _implicit_fs_thd_fs_cmp, w1, w2, y1, y2, z1, z2, _y_x10058) /* forall<a,b,c,e> (?field4/cmp : (c, c) -> e order, ?snd/cmp : (a, a) -> e order, ?thd/cmp : (b, b) -> e order, w1 : c, w2 : c, y1 : a, y2 : a, z1 : b, z2 : b, order) -> e order */  {
  if (_y_x10058 === 2) {
     
    var x_10404 = _implicit_fs_snd_fs_cmp(y1, y2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10059 /* order */ ) {
        return tuple4_fs__mlift_cmp_10181(_implicit_fs_field4_fs_cmp, _implicit_fs_thd_fs_cmp, w1, w2, z1, z2, _y_x10059);
      });
    }
    else {
      return tuple4_fs__mlift_cmp_10181(_implicit_fs_field4_fs_cmp, _implicit_fs_thd_fs_cmp, w1, w2, z1, z2, x_10404);
    }
  }
  else {
    return _y_x10058;
  }
}
 
 
// Order on quadruples
export function tuple4_fs_cmp(_pat_x74__26, _pat_x74__53, _implicit_fs_fst_fs_cmp, _implicit_fs_snd_fs_cmp, _implicit_fs_thd_fs_cmp, _implicit_fs_field4_fs_cmp) /* forall<a,b,c,d,e> ((a, b, c, d), (a, b, c, d), ?fst/cmp : (a, a) -> e order, ?snd/cmp : (b, b) -> e order, ?thd/cmp : (c, c) -> e order, ?field4/cmp : (d, d) -> e order) -> e order */  {
   
  var x_10406 = _implicit_fs_fst_fs_cmp(_pat_x74__26.fst, _pat_x74__53.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10058 /* order */ ) {
      return tuple4_fs__mlift_cmp_10182(_implicit_fs_field4_fs_cmp, _implicit_fs_snd_fs_cmp, _implicit_fs_thd_fs_cmp, _pat_x74__26.field4, _pat_x74__53.field4, _pat_x74__26.snd, _pat_x74__53.snd, _pat_x74__26.thd, _pat_x74__53.thd, _y_x10058);
    });
  }
  else {
    if (x_10406 === 2) {
       
      var x_0_10409 = _implicit_fs_snd_fs_cmp(_pat_x74__26.snd, _pat_x74__53.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10059 /* order */ ) {
          return tuple4_fs__mlift_cmp_10181(_implicit_fs_field4_fs_cmp, _implicit_fs_thd_fs_cmp, _pat_x74__26.field4, _pat_x74__53.field4, _pat_x74__26.thd, _pat_x74__53.thd, _y_x10059);
        });
      }
      else {
        if (x_0_10409 === 2) {
           
          var x_1_10412 = _implicit_fs_thd_fs_cmp(_pat_x74__26.thd, _pat_x74__53.thd);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10060 /* order */ ) {
              if (_y_x10060 === 2) {
                return _implicit_fs_field4_fs_cmp(_pat_x74__26.field4, _pat_x74__53.field4);
              }
              else {
                return _y_x10060;
              }
            });
          }
          else {
            if (x_1_10412 === 2) {
              return _implicit_fs_field4_fs_cmp(_pat_x74__26.field4, _pat_x74__53.field4);
            }
            else {
              return x_1_10412;
            }
          }
        }
        else {
          return x_0_10409;
        }
      }
    }
    else {
      return x_10406;
    }
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_cmp_10183(_implicit_fs_field5_fs_cmp, v1, v2, _y_x10070) /* forall<e,a> (?field5/cmp : (a, a) -> e order, v1 : a, v2 : a, order) -> e order */  {
  if (_y_x10070 === 2) {
    return _implicit_fs_field5_fs_cmp(v1, v2);
  }
  else {
    return _y_x10070;
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_cmp_10184(_implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp, v1, v2, w1, w2, _y_x10069) /* forall<a,e,b> (?field4/cmp : (a, a) -> e order, ?field5/cmp : (b, b) -> e order, v1 : b, v2 : b, w1 : a, w2 : a, order) -> e order */  {
  if (_y_x10069 === 2) {
     
    var x_10419 = _implicit_fs_field4_fs_cmp(w1, w2);
     
    var next_10420 = function(_y_x10070 /* order */ ) {
      if (_y_x10070 === 2) {
        return _implicit_fs_field5_fs_cmp(v1, v2);
      }
      else {
        return _y_x10070;
      }
    };
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(next_10420);
    }
    else {
      return next_10420(x_10419);
    }
  }
  else {
    return _y_x10069;
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_cmp_10185(_implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp, _implicit_fs_thd_fs_cmp, v1, v2, w1, w2, z1, z2, _y_x10068) /* forall<a,b,e,c> (?field4/cmp : (b, b) -> e order, ?field5/cmp : (c, c) -> e order, ?thd/cmp : (a, a) -> e order, v1 : c, v2 : c, w1 : b, w2 : b, z1 : a, z2 : a, order) -> e order */  {
  if (_y_x10068 === 2) {
     
    var x_10425 = _implicit_fs_thd_fs_cmp(z1, z2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10069 /* order */ ) {
        return tuple5_fs__mlift_cmp_10184(_implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp, v1, v2, w1, w2, _y_x10069);
      });
    }
    else {
      return tuple5_fs__mlift_cmp_10184(_implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp, v1, v2, w1, w2, x_10425);
    }
  }
  else {
    return _y_x10068;
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_cmp_10186(_implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp, _implicit_fs_snd_fs_cmp, _implicit_fs_thd_fs_cmp, v1, v2, w1, w2, y1, y2, z1, z2, _y_x10067) /* forall<a,b,c,e,d> (?field4/cmp : (c, c) -> e order, ?field5/cmp : (d, d) -> e order, ?snd/cmp : (a, a) -> e order, ?thd/cmp : (b, b) -> e order, v1 : d, v2 : d, w1 : c, w2 : c, y1 : a, y2 : a, z1 : b, z2 : b, order) -> e order */  {
  if (_y_x10067 === 2) {
     
    var x_10427 = _implicit_fs_snd_fs_cmp(y1, y2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10068 /* order */ ) {
        return tuple5_fs__mlift_cmp_10185(_implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp, _implicit_fs_thd_fs_cmp, v1, v2, w1, w2, z1, z2, _y_x10068);
      });
    }
    else {
      return tuple5_fs__mlift_cmp_10185(_implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp, _implicit_fs_thd_fs_cmp, v1, v2, w1, w2, z1, z2, x_10427);
    }
  }
  else {
    return _y_x10067;
  }
}
 
 
// Order on quintuples
export function tuple5_fs_cmp(_pat_x86__26, _pat_x86__58, _implicit_fs_fst_fs_cmp, _implicit_fs_snd_fs_cmp, _implicit_fs_thd_fs_cmp, _implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp) /* forall<a,b,c,d,e,a1> ((a, b, c, d, a1), (a, b, c, d, a1), ?fst/cmp : (a, a) -> e order, ?snd/cmp : (b, b) -> e order, ?thd/cmp : (c, c) -> e order, ?field4/cmp : (d, d) -> e order, ?field5/cmp : (a1, a1) -> e order) -> e order */  {
   
  var x_10429 = _implicit_fs_fst_fs_cmp(_pat_x86__26.fst, _pat_x86__58.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10067 /* order */ ) {
      return tuple5_fs__mlift_cmp_10186(_implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp, _implicit_fs_snd_fs_cmp, _implicit_fs_thd_fs_cmp, _pat_x86__26.field5, _pat_x86__58.field5, _pat_x86__26.field4, _pat_x86__58.field4, _pat_x86__26.snd, _pat_x86__58.snd, _pat_x86__26.thd, _pat_x86__58.thd, _y_x10067);
    });
  }
  else {
    if (x_10429 === 2) {
       
      var x_0_10432 = _implicit_fs_snd_fs_cmp(_pat_x86__26.snd, _pat_x86__58.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10068 /* order */ ) {
          return tuple5_fs__mlift_cmp_10185(_implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp, _implicit_fs_thd_fs_cmp, _pat_x86__26.field5, _pat_x86__58.field5, _pat_x86__26.field4, _pat_x86__58.field4, _pat_x86__26.thd, _pat_x86__58.thd, _y_x10068);
        });
      }
      else {
        if (x_0_10432 === 2) {
           
          var x_1_10435 = _implicit_fs_thd_fs_cmp(_pat_x86__26.thd, _pat_x86__58.thd);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10069 /* order */ ) {
              return tuple5_fs__mlift_cmp_10184(_implicit_fs_field4_fs_cmp, _implicit_fs_field5_fs_cmp, _pat_x86__26.field5, _pat_x86__58.field5, _pat_x86__26.field4, _pat_x86__58.field4, _y_x10069);
            });
          }
          else {
            if (x_1_10435 === 2) {
               
              var x_2_10438 = _implicit_fs_field4_fs_cmp(_pat_x86__26.field4, _pat_x86__58.field4);
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(function(_y_x10070 /* order */ ) {
                  if (_y_x10070 === 2) {
                    return _implicit_fs_field5_fs_cmp(_pat_x86__26.field5, _pat_x86__58.field5);
                  }
                  else {
                    return _y_x10070;
                  }
                });
              }
              else {
                if (x_2_10438 === 2) {
                  return _implicit_fs_field5_fs_cmp(_pat_x86__26.field5, _pat_x86__58.field5);
                }
                else {
                  return x_2_10438;
                }
              }
            }
            else {
              return x_1_10435;
            }
          }
        }
        else {
          return x_0_10432;
        }
      }
    }
    else {
      return x_10429;
    }
  }
}
 
 
// fip ordering of unit values
export function unit_fs_order2(a, b) /* (a : (), b : ()) -> order2<()> */  {
  return $std_core_types.Eq2(a);
}
 
 
// monadic lift
export function tuple2_fs__mlift_order2_10187(z1, _y_x10079) /* forall<a,b,e> (z1 : a, order2<b>) -> e order2<(a, b)> */  {
  if (_y_x10079._tag === 2) {
    return $std_core_types.Eq2($std_core_types.Tuple2(z1, _y_x10079.eq));
  }
  else if (_y_x10079._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple2(z1, _y_x10079.lt), $std_core_types.Tuple2(z1, _y_x10079.gt));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple2(z1, _y_x10079.lt), $std_core_types.Tuple2(z1, _y_x10079.gt));
  }
}
 
 
// monadic lift
export function tuple2_fs__mlift_order2_10188(_implicit_fs_b_fs_order2, x2, y2, _y_x10078) /* forall<a,b,e> (?b/order2 : (b, b) -> e order2<b>, x2 : b, y2 : b, order2<a>) -> e order2<(a, b)> */  {
  if (_y_x10078._tag === 2) {
     
    var x_10445 = _implicit_fs_b_fs_order2(x2, y2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10079 /* order2<2700> */ ) {
        return tuple2_fs__mlift_order2_10187(_y_x10078.eq, _y_x10079);
      });
    }
    else {
      return tuple2_fs__mlift_order2_10187(_y_x10078.eq, x_10445);
    }
  }
  else if (_y_x10078._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple2(_y_x10078.lt, x2), $std_core_types.Tuple2(_y_x10078.gt, y2));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple2(_y_x10078.lt, y2), $std_core_types.Tuple2(_y_x10078.gt, x2));
  }
}
 
 
// order2 of tuple2 values
export function tuple2_fs_order2(_pat_x104__24, _pat_x104__42, _implicit_fs_a_fs_order2, _implicit_fs_b_fs_order2) /* forall<a,b,e> ((a, b), (a, b), ?a/order2 : (a, a) -> e order2<a>, ?b/order2 : (b, b) -> e order2<b>) -> e order2<(a, b)> */  {
   
  var x_10447 = _implicit_fs_a_fs_order2(_pat_x104__24.fst, _pat_x104__42.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10078 /* order2<2699> */ ) {
      return tuple2_fs__mlift_order2_10188(_implicit_fs_b_fs_order2, _pat_x104__24.snd, _pat_x104__42.snd, _y_x10078);
    });
  }
  else {
    if (x_10447._tag === 2) {
       
      var x_0_10450 = _implicit_fs_b_fs_order2(_pat_x104__24.snd, _pat_x104__42.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10079 /* order2<2700> */ ) {
          return tuple2_fs__mlift_order2_10187(x_10447.eq, _y_x10079);
        });
      }
      else {
        if (x_0_10450._tag === 2) {
          return $std_core_types.Eq2($std_core_types.Tuple2(x_10447.eq, x_0_10450.eq));
        }
        else if (x_0_10450._tag === 1) {
          return $std_core_types.Lt2($std_core_types.Tuple2(x_10447.eq, x_0_10450.lt), $std_core_types.Tuple2(x_10447.eq, x_0_10450.gt));
        }
        else {
          return $std_core_types.Gt2($std_core_types.Tuple2(x_10447.eq, x_0_10450.lt), $std_core_types.Tuple2(x_10447.eq, x_0_10450.gt));
        }
      }
    }
    else if (x_10447._tag === 1) {
      return $std_core_types.Lt2($std_core_types.Tuple2(x_10447.lt, _pat_x104__24.snd), $std_core_types.Tuple2(x_10447.gt, _pat_x104__42.snd));
    }
    else {
      return $std_core_types.Gt2($std_core_types.Tuple2(x_10447.lt, _pat_x104__42.snd), $std_core_types.Tuple2(x_10447.gt, _pat_x104__24.snd));
    }
  }
}
 
 
// monadic lift
export function tuple3_fs__mlift_order2_10189(z1, z2, _y_x10085) /* forall<a,b,c,e> (z1 : a, z2 : b, order2<c>) -> e order2<(a, b, c)> */  {
  if (_y_x10085._tag === 2) {
    return $std_core_types.Eq2($std_core_types.Tuple3(z1, z2, _y_x10085.eq));
  }
  else if (_y_x10085._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple3(z1, z2, _y_x10085.lt), $std_core_types.Tuple3(z1, z2, _y_x10085.gt));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple3(z1, z2, _y_x10085.lt), $std_core_types.Tuple3(z1, z2, _y_x10085.gt));
  }
}
 
 
// monadic lift
export function tuple3_fs__mlift_order2_10190(_implicit_fs_c_fs_order2, x3, y3, z1, _y_x10084) /* forall<a,b,c,e> (?c/order2 : (c, c) -> e order2<c>, x3 : c, y3 : c, z1 : a, order2<b>) -> e order2<(a, b, c)> */  {
  if (_y_x10084._tag === 2) {
     
    var x_10453 = _implicit_fs_c_fs_order2(x3, y3);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10085 /* order2<3164> */ ) {
        return tuple3_fs__mlift_order2_10189(z1, _y_x10084.eq, _y_x10085);
      });
    }
    else {
      return tuple3_fs__mlift_order2_10189(z1, _y_x10084.eq, x_10453);
    }
  }
  else if (_y_x10084._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple3(z1, _y_x10084.lt, x3), $std_core_types.Tuple3(z1, _y_x10084.gt, y3));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple3(z1, _y_x10084.lt, y3), $std_core_types.Tuple3(z1, _y_x10084.gt, x3));
  }
}
 
 
// monadic lift
export function tuple3_fs__mlift_order2_10191(_implicit_fs_b_fs_order2, _implicit_fs_c_fs_order2, x2, x3, y2, y3, _y_x10083) /* forall<a,b,c,e> (?b/order2 : (b, b) -> e order2<b>, ?c/order2 : (c, c) -> e order2<c>, x2 : b, x3 : c, y2 : b, y3 : c, order2<a>) -> e order2<(a, b, c)> */  {
  if (_y_x10083._tag === 2) {
     
    var x_10455 = _implicit_fs_b_fs_order2(x2, y2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10084 /* order2<3163> */ ) {
        return tuple3_fs__mlift_order2_10190(_implicit_fs_c_fs_order2, x3, y3, _y_x10083.eq, _y_x10084);
      });
    }
    else {
      return tuple3_fs__mlift_order2_10190(_implicit_fs_c_fs_order2, x3, y3, _y_x10083.eq, x_10455);
    }
  }
  else if (_y_x10083._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple3(_y_x10083.lt, x2, x3), $std_core_types.Tuple3(_y_x10083.gt, y2, y3));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple3(_y_x10083.lt, y2, y3), $std_core_types.Tuple3(_y_x10083.gt, x2, x3));
  }
}
 
 
// order2 of tuple3 values
export function tuple3_fs_order2(_pat_x115__24, _pat_x115__49, _implicit_fs_a_fs_order2, _implicit_fs_b_fs_order2, _implicit_fs_c_fs_order2) /* forall<a,b,c,e> ((a, b, c), (a, b, c), ?a/order2 : (a, a) -> e order2<a>, ?b/order2 : (b, b) -> e order2<b>, ?c/order2 : (c, c) -> e order2<c>) -> e order2<(a, b, c)> */  {
   
  var x_10457 = _implicit_fs_a_fs_order2(_pat_x115__24.fst, _pat_x115__49.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10083 /* order2<3162> */ ) {
      return tuple3_fs__mlift_order2_10191(_implicit_fs_b_fs_order2, _implicit_fs_c_fs_order2, _pat_x115__24.snd, _pat_x115__24.thd, _pat_x115__49.snd, _pat_x115__49.thd, _y_x10083);
    });
  }
  else {
    if (x_10457._tag === 2) {
       
      var x_0_10460 = _implicit_fs_b_fs_order2(_pat_x115__24.snd, _pat_x115__49.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10084 /* order2<3163> */ ) {
          return tuple3_fs__mlift_order2_10190(_implicit_fs_c_fs_order2, _pat_x115__24.thd, _pat_x115__49.thd, x_10457.eq, _y_x10084);
        });
      }
      else {
        if (x_0_10460._tag === 2) {
           
          var x_1_10463 = _implicit_fs_c_fs_order2(_pat_x115__24.thd, _pat_x115__49.thd);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10085 /* order2<3164> */ ) {
              return tuple3_fs__mlift_order2_10189(x_10457.eq, x_0_10460.eq, _y_x10085);
            });
          }
          else {
            if (x_1_10463._tag === 2) {
              return $std_core_types.Eq2($std_core_types.Tuple3(x_10457.eq, x_0_10460.eq, x_1_10463.eq));
            }
            else if (x_1_10463._tag === 1) {
              return $std_core_types.Lt2($std_core_types.Tuple3(x_10457.eq, x_0_10460.eq, x_1_10463.lt), $std_core_types.Tuple3(x_10457.eq, x_0_10460.eq, x_1_10463.gt));
            }
            else {
              return $std_core_types.Gt2($std_core_types.Tuple3(x_10457.eq, x_0_10460.eq, x_1_10463.lt), $std_core_types.Tuple3(x_10457.eq, x_0_10460.eq, x_1_10463.gt));
            }
          }
        }
        else if (x_0_10460._tag === 1) {
          return $std_core_types.Lt2($std_core_types.Tuple3(x_10457.eq, x_0_10460.lt, _pat_x115__24.thd), $std_core_types.Tuple3(x_10457.eq, x_0_10460.gt, _pat_x115__49.thd));
        }
        else {
          return $std_core_types.Gt2($std_core_types.Tuple3(x_10457.eq, x_0_10460.lt, _pat_x115__49.thd), $std_core_types.Tuple3(x_10457.eq, x_0_10460.gt, _pat_x115__24.thd));
        }
      }
    }
    else if (x_10457._tag === 1) {
      return $std_core_types.Lt2($std_core_types.Tuple3(x_10457.lt, _pat_x115__24.snd, _pat_x115__24.thd), $std_core_types.Tuple3(x_10457.gt, _pat_x115__49.snd, _pat_x115__49.thd));
    }
    else {
      return $std_core_types.Gt2($std_core_types.Tuple3(x_10457.lt, _pat_x115__49.snd, _pat_x115__49.thd), $std_core_types.Tuple3(x_10457.gt, _pat_x115__24.snd, _pat_x115__24.thd));
    }
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_order2_10192(z1, z2, z3, _y_x10093) /* forall<a,b,c,d,e> (z1 : a, z2 : b, z3 : c, order2<d>) -> e order2<(a, b, c, d)> */  {
  if (_y_x10093._tag === 2) {
    return $std_core_types.Eq2($std_core_types.Tuple4(z1, z2, z3, _y_x10093.eq));
  }
  else if (_y_x10093._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple4(z1, z2, z3, _y_x10093.lt), $std_core_types.Tuple4(z1, z2, z3, _y_x10093.gt));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple4(z1, z2, z3, _y_x10093.lt), $std_core_types.Tuple4(z1, z2, z3, _y_x10093.gt));
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_order2_10193(_implicit_fs_field4_fs_order2, x4, y4, z1, z2, _y_x10092) /* forall<a,b,c,d,e> (?field4/order2 : (d, d) -> e order2<d>, x4 : d, y4 : d, z1 : a, z2 : b, order2<c>) -> e order2<(a, b, c, d)> */  {
  if (_y_x10092._tag === 2) {
     
    var x_10466 = _implicit_fs_field4_fs_order2(x4, y4);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10093 /* order2<3838> */ ) {
        return tuple4_fs__mlift_order2_10192(z1, z2, _y_x10092.eq, _y_x10093);
      });
    }
    else {
      return tuple4_fs__mlift_order2_10192(z1, z2, _y_x10092.eq, x_10466);
    }
  }
  else if (_y_x10092._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple4(z1, z2, _y_x10092.lt, x4), $std_core_types.Tuple4(z1, z2, _y_x10092.gt, y4));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple4(z1, z2, _y_x10092.lt, y4), $std_core_types.Tuple4(z1, z2, _y_x10092.gt, x4));
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_order2_10194(_implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, x3, x4, y3, y4, z1, _y_x10091) /* forall<a,b,c,d,e> (?c/order2 : (c, c) -> e order2<c>, ?field4/order2 : (d, d) -> e order2<d>, x3 : c, x4 : d, y3 : c, y4 : d, z1 : a, order2<b>) -> e order2<(a, b, c, d)> */  {
  if (_y_x10091._tag === 2) {
     
    var x_10468 = _implicit_fs_c_fs_order2(x3, y3);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10092 /* order2<3837> */ ) {
        return tuple4_fs__mlift_order2_10193(_implicit_fs_field4_fs_order2, x4, y4, z1, _y_x10091.eq, _y_x10092);
      });
    }
    else {
      return tuple4_fs__mlift_order2_10193(_implicit_fs_field4_fs_order2, x4, y4, z1, _y_x10091.eq, x_10468);
    }
  }
  else if (_y_x10091._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple4(z1, _y_x10091.lt, x3, x4), $std_core_types.Tuple4(z1, _y_x10091.gt, y3, y4));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple4(z1, _y_x10091.lt, y3, y4), $std_core_types.Tuple4(z1, _y_x10091.gt, x3, x4));
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_order2_10195(_implicit_fs_b_fs_order2, _implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, x2, x3, x4, y2, y3, y4, _y_x10090) /* forall<a,b,c,d,e> (?b/order2 : (b, b) -> e order2<b>, ?c/order2 : (c, c) -> e order2<c>, ?field4/order2 : (d, d) -> e order2<d>, x2 : b, x3 : c, x4 : d, y2 : b, y3 : c, y4 : d, order2<a>) -> e order2<(a, b, c, d)> */  {
  if (_y_x10090._tag === 2) {
     
    var x_10470 = _implicit_fs_b_fs_order2(x2, y2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10091 /* order2<3836> */ ) {
        return tuple4_fs__mlift_order2_10194(_implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, x3, x4, y3, y4, _y_x10090.eq, _y_x10091);
      });
    }
    else {
      return tuple4_fs__mlift_order2_10194(_implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, x3, x4, y3, y4, _y_x10090.eq, x_10470);
    }
  }
  else if (_y_x10090._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple4(_y_x10090.lt, x2, x3, x4), $std_core_types.Tuple4(_y_x10090.gt, y2, y3, y4));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple4(_y_x10090.lt, y2, y3, y4), $std_core_types.Tuple4(_y_x10090.gt, x2, x3, x4));
  }
}
 
 
// order2 of tuple4 values
export function tuple4_fs_order2(_pat_x130__24, _pat_x130__56, _implicit_fs_a_fs_order2, _implicit_fs_b_fs_order2, _implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2) /* forall<a,b,c,d,e> ((a, b, c, d), (a, b, c, d), ?a/order2 : (a, a) -> e order2<a>, ?b/order2 : (b, b) -> e order2<b>, ?c/order2 : (c, c) -> e order2<c>, ?field4/order2 : (d, d) -> e order2<d>) -> e order2<(a, b, c, d)> */  {
   
  var x_10472 = _implicit_fs_a_fs_order2(_pat_x130__24.fst, _pat_x130__56.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10090 /* order2<3835> */ ) {
      return tuple4_fs__mlift_order2_10195(_implicit_fs_b_fs_order2, _implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, _pat_x130__24.snd, _pat_x130__24.thd, _pat_x130__24.field4, _pat_x130__56.snd, _pat_x130__56.thd, _pat_x130__56.field4, _y_x10090);
    });
  }
  else {
    if (x_10472._tag === 2) {
       
      var x_0_10475 = _implicit_fs_b_fs_order2(_pat_x130__24.snd, _pat_x130__56.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10091 /* order2<3836> */ ) {
          return tuple4_fs__mlift_order2_10194(_implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, _pat_x130__24.thd, _pat_x130__24.field4, _pat_x130__56.thd, _pat_x130__56.field4, x_10472.eq, _y_x10091);
        });
      }
      else {
        if (x_0_10475._tag === 2) {
           
          var x_1_10478 = _implicit_fs_c_fs_order2(_pat_x130__24.thd, _pat_x130__56.thd);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10092 /* order2<3837> */ ) {
              return tuple4_fs__mlift_order2_10193(_implicit_fs_field4_fs_order2, _pat_x130__24.field4, _pat_x130__56.field4, x_10472.eq, x_0_10475.eq, _y_x10092);
            });
          }
          else {
            if (x_1_10478._tag === 2) {
               
              var x_2_10481 = _implicit_fs_field4_fs_order2(_pat_x130__24.field4, _pat_x130__56.field4);
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(function(_y_x10093 /* order2<3838> */ ) {
                  return tuple4_fs__mlift_order2_10192(x_10472.eq, x_0_10475.eq, x_1_10478.eq, _y_x10093);
                });
              }
              else {
                if (x_2_10481._tag === 2) {
                  return $std_core_types.Eq2($std_core_types.Tuple4(x_10472.eq, x_0_10475.eq, x_1_10478.eq, x_2_10481.eq));
                }
                else if (x_2_10481._tag === 1) {
                  return $std_core_types.Lt2($std_core_types.Tuple4(x_10472.eq, x_0_10475.eq, x_1_10478.eq, x_2_10481.lt), $std_core_types.Tuple4(x_10472.eq, x_0_10475.eq, x_1_10478.eq, x_2_10481.gt));
                }
                else {
                  return $std_core_types.Gt2($std_core_types.Tuple4(x_10472.eq, x_0_10475.eq, x_1_10478.eq, x_2_10481.lt), $std_core_types.Tuple4(x_10472.eq, x_0_10475.eq, x_1_10478.eq, x_2_10481.gt));
                }
              }
            }
            else if (x_1_10478._tag === 1) {
              return $std_core_types.Lt2($std_core_types.Tuple4(x_10472.eq, x_0_10475.eq, x_1_10478.lt, _pat_x130__24.field4), $std_core_types.Tuple4(x_10472.eq, x_0_10475.eq, x_1_10478.gt, _pat_x130__56.field4));
            }
            else {
              return $std_core_types.Gt2($std_core_types.Tuple4(x_10472.eq, x_0_10475.eq, x_1_10478.lt, _pat_x130__56.field4), $std_core_types.Tuple4(x_10472.eq, x_0_10475.eq, x_1_10478.gt, _pat_x130__24.field4));
            }
          }
        }
        else if (x_0_10475._tag === 1) {
          return $std_core_types.Lt2($std_core_types.Tuple4(x_10472.eq, x_0_10475.lt, _pat_x130__24.thd, _pat_x130__24.field4), $std_core_types.Tuple4(x_10472.eq, x_0_10475.gt, _pat_x130__56.thd, _pat_x130__56.field4));
        }
        else {
          return $std_core_types.Gt2($std_core_types.Tuple4(x_10472.eq, x_0_10475.lt, _pat_x130__56.thd, _pat_x130__56.field4), $std_core_types.Tuple4(x_10472.eq, x_0_10475.gt, _pat_x130__24.thd, _pat_x130__24.field4));
        }
      }
    }
    else if (x_10472._tag === 1) {
      return $std_core_types.Lt2($std_core_types.Tuple4(x_10472.lt, _pat_x130__24.snd, _pat_x130__24.thd, _pat_x130__24.field4), $std_core_types.Tuple4(x_10472.gt, _pat_x130__56.snd, _pat_x130__56.thd, _pat_x130__56.field4));
    }
    else {
      return $std_core_types.Gt2($std_core_types.Tuple4(x_10472.lt, _pat_x130__56.snd, _pat_x130__56.thd, _pat_x130__56.field4), $std_core_types.Tuple4(x_10472.gt, _pat_x130__24.snd, _pat_x130__24.thd, _pat_x130__24.field4));
    }
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_order2_10196(z1, z2, z3, z4, _y_x10103) /* forall<a,b,c,d,e,a1> (z1 : a, z2 : b, z3 : c, z4 : d, order2<a1>) -> e order2<(a, b, c, d, a1)> */  {
  if (_y_x10103._tag === 2) {
    return $std_core_types.Eq2($std_core_types.Tuple5(z1, z2, z3, z4, _y_x10103.eq));
  }
  else if (_y_x10103._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple5(z1, z2, z3, z4, _y_x10103.lt), $std_core_types.Tuple5(z1, z2, z3, z4, _y_x10103.gt));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple5(z1, z2, z3, z4, _y_x10103.lt), $std_core_types.Tuple5(z1, z2, z3, z4, _y_x10103.gt));
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_order2_10197(_implicit_fs_field5_fs_order2, x5, y5, z1, z2, z3, _y_x10102) /* forall<a,b,c,d,e,a1> (?field5/order2 : (a1, a1) -> e order2<a1>, x5 : a1, y5 : a1, z1 : a, z2 : b, z3 : c, order2<d>) -> e order2<(a, b, c, d, a1)> */  {
  if (_y_x10102._tag === 2) {
     
    var x_10484 = _implicit_fs_field5_fs_order2(x5, y5);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10103 /* order2<4755> */ ) {
        return tuple5_fs__mlift_order2_10196(z1, z2, z3, _y_x10102.eq, _y_x10103);
      });
    }
    else {
      return tuple5_fs__mlift_order2_10196(z1, z2, z3, _y_x10102.eq, x_10484);
    }
  }
  else if (_y_x10102._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple5(z1, z2, z3, _y_x10102.lt, x5), $std_core_types.Tuple5(z1, z2, z3, _y_x10102.gt, y5));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple5(z1, z2, z3, _y_x10102.lt, y5), $std_core_types.Tuple5(z1, z2, z3, _y_x10102.gt, x5));
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_order2_10198(_implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2, x4, x5, y4, y5, z1, z2, _y_x10101) /* forall<a,b,c,d,e,a1> (?field4/order2 : (d, d) -> e order2<d>, ?field5/order2 : (a1, a1) -> e order2<a1>, x4 : d, x5 : a1, y4 : d, y5 : a1, z1 : a, z2 : b, order2<c>) -> e order2<(a, b, c, d, a1)> */  {
  if (_y_x10101._tag === 2) {
     
    var x_10486 = _implicit_fs_field4_fs_order2(x4, y4);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10102 /* order2<4753> */ ) {
        return tuple5_fs__mlift_order2_10197(_implicit_fs_field5_fs_order2, x5, y5, z1, z2, _y_x10101.eq, _y_x10102);
      });
    }
    else {
      return tuple5_fs__mlift_order2_10197(_implicit_fs_field5_fs_order2, x5, y5, z1, z2, _y_x10101.eq, x_10486);
    }
  }
  else if (_y_x10101._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple5(z1, z2, _y_x10101.lt, x4, x5), $std_core_types.Tuple5(z1, z2, _y_x10101.gt, y4, y5));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple5(z1, z2, _y_x10101.lt, y4, y5), $std_core_types.Tuple5(z1, z2, _y_x10101.gt, x4, x5));
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_order2_10199(_implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2, x3, x4, x5, y3, y4, y5, z1, _y_x10100) /* forall<a,b,c,d,e,a1> (?c/order2 : (c, c) -> e order2<c>, ?field4/order2 : (d, d) -> e order2<d>, ?field5/order2 : (a1, a1) -> e order2<a1>, x3 : c, x4 : d, x5 : a1, y3 : c, y4 : d, y5 : a1, z1 : a, order2<b>) -> e order2<(a, b, c, d, a1)> */  {
  if (_y_x10100._tag === 2) {
     
    var x_10488 = _implicit_fs_c_fs_order2(x3, y3);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10101 /* order2<4752> */ ) {
        return tuple5_fs__mlift_order2_10198(_implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2, x4, x5, y4, y5, z1, _y_x10100.eq, _y_x10101);
      });
    }
    else {
      return tuple5_fs__mlift_order2_10198(_implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2, x4, x5, y4, y5, z1, _y_x10100.eq, x_10488);
    }
  }
  else if (_y_x10100._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple5(z1, _y_x10100.lt, x3, x4, x5), $std_core_types.Tuple5(z1, _y_x10100.gt, y3, y4, y5));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple5(z1, _y_x10100.lt, y3, y4, y5), $std_core_types.Tuple5(z1, _y_x10100.gt, x3, x4, x5));
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_order2_10200(_implicit_fs_b_fs_order2, _implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2, x2, x3, x4, x5, y2, y3, y4, y5, _y_x10099) /* forall<a,b,c,d,e,a1> (?b/order2 : (b, b) -> e order2<b>, ?c/order2 : (c, c) -> e order2<c>, ?field4/order2 : (d, d) -> e order2<d>, ?field5/order2 : (a1, a1) -> e order2<a1>, x2 : b, x3 : c, x4 : d, x5 : a1, y2 : b, y3 : c, y4 : d, y5 : a1, order2<a>) -> e order2<(a, b, c, d, a1)> */  {
  if (_y_x10099._tag === 2) {
     
    var x_10490 = _implicit_fs_b_fs_order2(x2, y2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10100 /* order2<4751> */ ) {
        return tuple5_fs__mlift_order2_10199(_implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2, x3, x4, x5, y3, y4, y5, _y_x10099.eq, _y_x10100);
      });
    }
    else {
      return tuple5_fs__mlift_order2_10199(_implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2, x3, x4, x5, y3, y4, y5, _y_x10099.eq, x_10490);
    }
  }
  else if (_y_x10099._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Tuple5(_y_x10099.lt, x2, x3, x4, x5), $std_core_types.Tuple5(_y_x10099.gt, y2, y3, y4, y5));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Tuple5(_y_x10099.lt, y2, y3, y4, y5), $std_core_types.Tuple5(_y_x10099.gt, x2, x3, x4, x5));
  }
}
 
 
// order2 of tuple5 values
export function tuple5_fs_order2(_pat_x148__24, _pat_x148__63, _implicit_fs_a_fs_order2, _implicit_fs_b_fs_order2, _implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2) /* forall<a,b,c,d,e,a1> ((a, b, c, d, a1), (a, b, c, d, a1), ?a/order2 : (a, a) -> e order2<a>, ?b/order2 : (b, b) -> e order2<b>, ?c/order2 : (c, c) -> e order2<c>, ?field4/order2 : (d, d) -> e order2<d>, ?field5/order2 : (a1, a1) -> e order2<a1>) -> e order2<(a, b, c, d, a1)> */  {
   
  var x_10492 = _implicit_fs_a_fs_order2(_pat_x148__24.fst, _pat_x148__63.fst);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10099 /* order2<4750> */ ) {
      return tuple5_fs__mlift_order2_10200(_implicit_fs_b_fs_order2, _implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2, _pat_x148__24.snd, _pat_x148__24.thd, _pat_x148__24.field4, _pat_x148__24.field5, _pat_x148__63.snd, _pat_x148__63.thd, _pat_x148__63.field4, _pat_x148__63.field5, _y_x10099);
    });
  }
  else {
    if (x_10492._tag === 2) {
       
      var x_0_10495 = _implicit_fs_b_fs_order2(_pat_x148__24.snd, _pat_x148__63.snd);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10100 /* order2<4751> */ ) {
          return tuple5_fs__mlift_order2_10199(_implicit_fs_c_fs_order2, _implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2, _pat_x148__24.thd, _pat_x148__24.field4, _pat_x148__24.field5, _pat_x148__63.thd, _pat_x148__63.field4, _pat_x148__63.field5, x_10492.eq, _y_x10100);
        });
      }
      else {
        if (x_0_10495._tag === 2) {
           
          var x_1_10498 = _implicit_fs_c_fs_order2(_pat_x148__24.thd, _pat_x148__63.thd);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10101 /* order2<4752> */ ) {
              return tuple5_fs__mlift_order2_10198(_implicit_fs_field4_fs_order2, _implicit_fs_field5_fs_order2, _pat_x148__24.field4, _pat_x148__24.field5, _pat_x148__63.field4, _pat_x148__63.field5, x_10492.eq, x_0_10495.eq, _y_x10101);
            });
          }
          else {
            if (x_1_10498._tag === 2) {
               
              var x_2_10501 = _implicit_fs_field4_fs_order2(_pat_x148__24.field4, _pat_x148__63.field4);
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(function(_y_x10102 /* order2<4753> */ ) {
                  return tuple5_fs__mlift_order2_10197(_implicit_fs_field5_fs_order2, _pat_x148__24.field5, _pat_x148__63.field5, x_10492.eq, x_0_10495.eq, x_1_10498.eq, _y_x10102);
                });
              }
              else {
                if (x_2_10501._tag === 2) {
                   
                  var x_3_10504 = _implicit_fs_field5_fs_order2(_pat_x148__24.field5, _pat_x148__63.field5);
                  if ($std_core_hnd._yielding()) {
                    return $std_core_hnd.yield_extend(function(_y_x10103 /* order2<4755> */ ) {
                      return tuple5_fs__mlift_order2_10196(x_10492.eq, x_0_10495.eq, x_1_10498.eq, x_2_10501.eq, _y_x10103);
                    });
                  }
                  else {
                    if (x_3_10504._tag === 2) {
                      return $std_core_types.Eq2($std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.eq, x_2_10501.eq, x_3_10504.eq));
                    }
                    else if (x_3_10504._tag === 1) {
                      return $std_core_types.Lt2($std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.eq, x_2_10501.eq, x_3_10504.lt), $std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.eq, x_2_10501.eq, x_3_10504.gt));
                    }
                    else {
                      return $std_core_types.Gt2($std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.eq, x_2_10501.eq, x_3_10504.lt), $std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.eq, x_2_10501.eq, x_3_10504.gt));
                    }
                  }
                }
                else if (x_2_10501._tag === 1) {
                  return $std_core_types.Lt2($std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.eq, x_2_10501.lt, _pat_x148__24.field5), $std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.eq, x_2_10501.gt, _pat_x148__63.field5));
                }
                else {
                  return $std_core_types.Gt2($std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.eq, x_2_10501.lt, _pat_x148__63.field5), $std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.eq, x_2_10501.gt, _pat_x148__24.field5));
                }
              }
            }
            else if (x_1_10498._tag === 1) {
              return $std_core_types.Lt2($std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.lt, _pat_x148__24.field4, _pat_x148__24.field5), $std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.gt, _pat_x148__63.field4, _pat_x148__63.field5));
            }
            else {
              return $std_core_types.Gt2($std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.lt, _pat_x148__63.field4, _pat_x148__63.field5), $std_core_types.Tuple5(x_10492.eq, x_0_10495.eq, x_1_10498.gt, _pat_x148__24.field4, _pat_x148__24.field5));
            }
          }
        }
        else if (x_0_10495._tag === 1) {
          return $std_core_types.Lt2($std_core_types.Tuple5(x_10492.eq, x_0_10495.lt, _pat_x148__24.thd, _pat_x148__24.field4, _pat_x148__24.field5), $std_core_types.Tuple5(x_10492.eq, x_0_10495.gt, _pat_x148__63.thd, _pat_x148__63.field4, _pat_x148__63.field5));
        }
        else {
          return $std_core_types.Gt2($std_core_types.Tuple5(x_10492.eq, x_0_10495.lt, _pat_x148__63.thd, _pat_x148__63.field4, _pat_x148__63.field5), $std_core_types.Tuple5(x_10492.eq, x_0_10495.gt, _pat_x148__24.thd, _pat_x148__24.field4, _pat_x148__24.field5));
        }
      }
    }
    else if (x_10492._tag === 1) {
      return $std_core_types.Lt2($std_core_types.Tuple5(x_10492.lt, _pat_x148__24.snd, _pat_x148__24.thd, _pat_x148__24.field4, _pat_x148__24.field5), $std_core_types.Tuple5(x_10492.gt, _pat_x148__63.snd, _pat_x148__63.thd, _pat_x148__63.field4, _pat_x148__63.field5));
    }
    else {
      return $std_core_types.Gt2($std_core_types.Tuple5(x_10492.lt, _pat_x148__63.snd, _pat_x148__63.thd, _pat_x148__63.field4, _pat_x148__63.field5), $std_core_types.Tuple5(x_10492.gt, _pat_x148__24.snd, _pat_x148__24.thd, _pat_x148__24.field4, _pat_x148__24.field5));
    }
  }
}
 
 
// Convert a unit value `()` to a string
export function unit_fs_show(u) /* (u : ()) -> string */  {
  return "()";
}
 
 
// monadic lift
export function tuple2_fs__mlift_show_10201(_y_x10110, _y_x10111) /* forall<e> (string, string) -> e string */  {
  return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(_y_x10110, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10111, ")"))));
}
 
 
// monadic lift
export function tuple2_fs__mlift_show_10202(_implicit_fs_snd_fs_show, x, _y_x10110) /* forall<a,b,e> (?snd/show : (b) -> e string, x : (a, b), string) -> e string */  {
   
  var _x0 = x.snd;
  var x_0_10507 = _implicit_fs_snd_fs_show(_x0);
   
  function next_10508(_y_x10111) /* (string) -> 5017 string */  {
    return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(_y_x10110, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10111, ")"))));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10508);
  }
  else {
    return next_10508(x_0_10507);
  }
}
 
 
// Show a tuple
export function tuple2_fs_show(x, _implicit_fs_fst_fs_show, _implicit_fs_snd_fs_show) /* forall<a,b,e> (x : (a, b), ?fst/show : (a) -> e string, ?snd/show : (b) -> e string) -> e string */  {
   
  var _x0 = x.fst;
  var x_0_10511 = _implicit_fs_fst_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10110 /* string */ ) {
      return tuple2_fs__mlift_show_10202(_implicit_fs_snd_fs_show, x, _y_x10110);
    });
  }
  else {
     
    var _x0 = x.snd;
    var x_1_10514 = _implicit_fs_snd_fs_show(_x0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10111 /* string */ ) {
        return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(x_0_10511, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10111, ")"))));
      });
    }
    else {
      return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(x_0_10511, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_1_10514, ")"))));
    }
  }
}
 
 
// monadic lift
export function tuple3_fs__mlift_show_10203(_y_x10112, _y_x10113, _y_x10114) /* forall<e> (string, string, string) -> e string */  {
  return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(_y_x10112, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10113, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10114, ")"))))));
}
 
 
// monadic lift
export function tuple3_fs__mlift_show_10204(_y_x10112, _implicit_fs_thd_fs_show, x, _y_x10113) /* forall<a,b,c,e> (string, ?thd/show : (c) -> e string, x : (a, b, c), string) -> e string */  {
   
  var _x0 = x.thd;
  var x_0_10519 = _implicit_fs_thd_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10114 /* string */ ) {
      return tuple3_fs__mlift_show_10203(_y_x10112, _y_x10113, _y_x10114);
    });
  }
  else {
    return tuple3_fs__mlift_show_10203(_y_x10112, _y_x10113, x_0_10519);
  }
}
 
 
// monadic lift
export function tuple3_fs__mlift_show_10205(_implicit_fs_snd_fs_show, _implicit_fs_thd_fs_show, x, _y_x10112) /* forall<a,b,c,e> (?snd/show : (b) -> e string, ?thd/show : (c) -> e string, x : (a, b, c), string) -> e string */  {
   
  var _x0 = x.snd;
  var x_0_10521 = _implicit_fs_snd_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10113 /* string */ ) {
      return tuple3_fs__mlift_show_10204(_y_x10112, _implicit_fs_thd_fs_show, x, _y_x10113);
    });
  }
  else {
    return tuple3_fs__mlift_show_10204(_y_x10112, _implicit_fs_thd_fs_show, x, x_0_10521);
  }
}
 
 
// Show a triple
export function tuple3_fs_show(x, _implicit_fs_fst_fs_show, _implicit_fs_snd_fs_show, _implicit_fs_thd_fs_show) /* forall<a,b,c,e> (x : (a, b, c), ?fst/show : (a) -> e string, ?snd/show : (b) -> e string, ?thd/show : (c) -> e string) -> e string */  {
   
  var _x0 = x.fst;
  var x_0_10523 = _implicit_fs_fst_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10112 /* string */ ) {
      return tuple3_fs__mlift_show_10205(_implicit_fs_snd_fs_show, _implicit_fs_thd_fs_show, x, _y_x10112);
    });
  }
  else {
     
    var _x0 = x.snd;
    var x_1_10526 = _implicit_fs_snd_fs_show(_x0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10113 /* string */ ) {
        return tuple3_fs__mlift_show_10204(x_0_10523, _implicit_fs_thd_fs_show, x, _y_x10113);
      });
    }
    else {
       
      var _x0 = x.thd;
      var x_2_10529 = _implicit_fs_thd_fs_show(_x0);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10114 /* string */ ) {
          return tuple3_fs__mlift_show_10203(x_0_10523, x_1_10526, _y_x10114);
        });
      }
      else {
        return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(x_0_10523, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_1_10526, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_2_10529, ")"))))));
      }
    }
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_show_10206(_y_x10115, _y_x10116, _y_x10117, _y_x10118) /* forall<e> (string, string, string, string) -> e string */  {
  return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(_y_x10115, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10116, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10117, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10118, ")"))))))));
}
 
 
// monadic lift
export function tuple4_fs__mlift_show_10207(_y_x10115, _y_x10116, _implicit_fs_field4_fs_show, x, _y_x10117) /* forall<a,b,c,d,e> (string, string, ?field4/show : (d) -> e string, x : (a, b, c, d), string) -> e string */  {
   
  var _x0 = x.field4;
  var x_0_10532 = _implicit_fs_field4_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10118 /* string */ ) {
      return tuple4_fs__mlift_show_10206(_y_x10115, _y_x10116, _y_x10117, _y_x10118);
    });
  }
  else {
    return tuple4_fs__mlift_show_10206(_y_x10115, _y_x10116, _y_x10117, x_0_10532);
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_show_10208(_y_x10115, _implicit_fs_field4_fs_show, _implicit_fs_thd_fs_show, x, _y_x10116) /* forall<a,b,c,d,e> (string, ?field4/show : (d) -> e string, ?thd/show : (c) -> e string, x : (a, b, c, d), string) -> e string */  {
   
  var _x0 = x.thd;
  var x_0_10534 = _implicit_fs_thd_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10117 /* string */ ) {
      return tuple4_fs__mlift_show_10207(_y_x10115, _y_x10116, _implicit_fs_field4_fs_show, x, _y_x10117);
    });
  }
  else {
    return tuple4_fs__mlift_show_10207(_y_x10115, _y_x10116, _implicit_fs_field4_fs_show, x, x_0_10534);
  }
}
 
 
// monadic lift
export function tuple4_fs__mlift_show_10209(_implicit_fs_field4_fs_show, _implicit_fs_snd_fs_show, _implicit_fs_thd_fs_show, x, _y_x10115) /* forall<a,b,c,d,e> (?field4/show : (d) -> e string, ?snd/show : (b) -> e string, ?thd/show : (c) -> e string, x : (a, b, c, d), string) -> e string */  {
   
  var _x0 = x.snd;
  var x_0_10536 = _implicit_fs_snd_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10116 /* string */ ) {
      return tuple4_fs__mlift_show_10208(_y_x10115, _implicit_fs_field4_fs_show, _implicit_fs_thd_fs_show, x, _y_x10116);
    });
  }
  else {
    return tuple4_fs__mlift_show_10208(_y_x10115, _implicit_fs_field4_fs_show, _implicit_fs_thd_fs_show, x, x_0_10536);
  }
}
 
 
// Show a quadruple
export function tuple4_fs_show(x, _implicit_fs_fst_fs_show, _implicit_fs_snd_fs_show, _implicit_fs_thd_fs_show, _implicit_fs_field4_fs_show) /* forall<a,b,c,d,e> (x : (a, b, c, d), ?fst/show : (a) -> e string, ?snd/show : (b) -> e string, ?thd/show : (c) -> e string, ?field4/show : (d) -> e string) -> e string */  {
   
  var _x0 = x.fst;
  var x_0_10538 = _implicit_fs_fst_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10115 /* string */ ) {
      return tuple4_fs__mlift_show_10209(_implicit_fs_field4_fs_show, _implicit_fs_snd_fs_show, _implicit_fs_thd_fs_show, x, _y_x10115);
    });
  }
  else {
     
    var _x0 = x.snd;
    var x_1_10541 = _implicit_fs_snd_fs_show(_x0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10116 /* string */ ) {
        return tuple4_fs__mlift_show_10208(x_0_10538, _implicit_fs_field4_fs_show, _implicit_fs_thd_fs_show, x, _y_x10116);
      });
    }
    else {
       
      var _x0 = x.thd;
      var x_2_10544 = _implicit_fs_thd_fs_show(_x0);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10117 /* string */ ) {
          return tuple4_fs__mlift_show_10207(x_0_10538, x_1_10541, _implicit_fs_field4_fs_show, x, _y_x10117);
        });
      }
      else {
         
        var _x0 = x.field4;
        var x_3_10547 = _implicit_fs_field4_fs_show(_x0);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10118 /* string */ ) {
            return tuple4_fs__mlift_show_10206(x_0_10538, x_1_10541, x_2_10544, _y_x10118);
          });
        }
        else {
          return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(x_0_10538, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_1_10541, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_2_10544, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_3_10547, ")"))))))));
        }
      }
    }
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_show_10210(_y_x10119, _y_x10120, _y_x10121, _y_x10122, _y_x10123) /* forall<e> (string, string, string, string, string) -> e string */  {
  return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(_y_x10119, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10120, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10121, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10122, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(_y_x10123, ")"))))))))));
}
 
 
// monadic lift
export function tuple5_fs__mlift_show_10211(_y_x10119, _y_x10120, _y_x10121, _implicit_fs_field5_fs_show, x, _y_x10122) /* forall<a,b,c,d,e,a1> (string, string, string, ?field5/show : (a1) -> e string, x : (a, b, c, d, a1), string) -> e string */  {
   
  var _x0 = x.field5;
  var x_0_10550 = _implicit_fs_field5_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10123 /* string */ ) {
      return tuple5_fs__mlift_show_10210(_y_x10119, _y_x10120, _y_x10121, _y_x10122, _y_x10123);
    });
  }
  else {
    return tuple5_fs__mlift_show_10210(_y_x10119, _y_x10120, _y_x10121, _y_x10122, x_0_10550);
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_show_10212(_y_x10119, _y_x10120, _implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show, x, _y_x10121) /* forall<a,b,c,d,e,a1> (string, string, ?field4/show : (d) -> e string, ?field5/show : (a1) -> e string, x : (a, b, c, d, a1), string) -> e string */  {
   
  var _x0 = x.field4;
  var x_0_10552 = _implicit_fs_field4_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10122 /* string */ ) {
      return tuple5_fs__mlift_show_10211(_y_x10119, _y_x10120, _y_x10121, _implicit_fs_field5_fs_show, x, _y_x10122);
    });
  }
  else {
    return tuple5_fs__mlift_show_10211(_y_x10119, _y_x10120, _y_x10121, _implicit_fs_field5_fs_show, x, x_0_10552);
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_show_10213(_y_x10119, _implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show, _implicit_fs_thd_fs_show, x, _y_x10120) /* forall<a,b,c,d,e,a1> (string, ?field4/show : (d) -> e string, ?field5/show : (a1) -> e string, ?thd/show : (c) -> e string, x : (a, b, c, d, a1), string) -> e string */  {
   
  var _x0 = x.thd;
  var x_0_10554 = _implicit_fs_thd_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10121 /* string */ ) {
      return tuple5_fs__mlift_show_10212(_y_x10119, _y_x10120, _implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show, x, _y_x10121);
    });
  }
  else {
    return tuple5_fs__mlift_show_10212(_y_x10119, _y_x10120, _implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show, x, x_0_10554);
  }
}
 
 
// monadic lift
export function tuple5_fs__mlift_show_10214(_implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show, _implicit_fs_snd_fs_show, _implicit_fs_thd_fs_show, x, _y_x10119) /* forall<a,b,c,d,e,a1> (?field4/show : (d) -> e string, ?field5/show : (a1) -> e string, ?snd/show : (b) -> e string, ?thd/show : (c) -> e string, x : (a, b, c, d, a1), string) -> e string */  {
   
  var _x0 = x.snd;
  var x_0_10556 = _implicit_fs_snd_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10120 /* string */ ) {
      return tuple5_fs__mlift_show_10213(_y_x10119, _implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show, _implicit_fs_thd_fs_show, x, _y_x10120);
    });
  }
  else {
    return tuple5_fs__mlift_show_10213(_y_x10119, _implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show, _implicit_fs_thd_fs_show, x, x_0_10556);
  }
}
 
 
// Show a quintuple
export function tuple5_fs_show(x, _implicit_fs_fst_fs_show, _implicit_fs_snd_fs_show, _implicit_fs_thd_fs_show, _implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show) /* forall<a,b,c,d,e,a1> (x : (a, b, c, d, a1), ?fst/show : (a) -> e string, ?snd/show : (b) -> e string, ?thd/show : (c) -> e string, ?field4/show : (d) -> e string, ?field5/show : (a1) -> e string) -> e string */  {
   
  var _x0 = x.fst;
  var x_0_10558 = _implicit_fs_fst_fs_show(_x0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10119 /* string */ ) {
      return tuple5_fs__mlift_show_10214(_implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show, _implicit_fs_snd_fs_show, _implicit_fs_thd_fs_show, x, _y_x10119);
    });
  }
  else {
     
    var _x0 = x.snd;
    var x_1_10561 = _implicit_fs_snd_fs_show(_x0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10120 /* string */ ) {
        return tuple5_fs__mlift_show_10213(x_0_10558, _implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show, _implicit_fs_thd_fs_show, x, _y_x10120);
      });
    }
    else {
       
      var _x0 = x.thd;
      var x_2_10564 = _implicit_fs_thd_fs_show(_x0);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10121 /* string */ ) {
          return tuple5_fs__mlift_show_10212(x_0_10558, x_1_10561, _implicit_fs_field4_fs_show, _implicit_fs_field5_fs_show, x, _y_x10121);
        });
      }
      else {
         
        var _x0 = x.field4;
        var x_3_10567 = _implicit_fs_field4_fs_show(_x0);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10122 /* string */ ) {
            return tuple5_fs__mlift_show_10211(x_0_10558, x_1_10561, x_2_10564, _implicit_fs_field5_fs_show, x, _y_x10122);
          });
        }
        else {
           
          var _x0 = x.field5;
          var x_4_10570 = _implicit_fs_field5_fs_show(_x0);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10123 /* string */ ) {
              return tuple5_fs__mlift_show_10210(x_0_10558, x_1_10561, x_2_10564, x_3_10567, _y_x10123);
            });
          }
          else {
            return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(x_0_10558, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_1_10561, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_2_10564, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_3_10567, $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(x_4_10570, ")"))))))))));
          }
        }
      }
    }
  }
}
 
 
// _deprecated_, use `tuple2/show` instead
export function show_tuple(x, showfst, showsnd) /* forall<a,b,e> (x : (a, b), showfst : (a) -> e string, showsnd : (b) -> e string) -> e string */  {
  return tuple2_fs_show(x, showfst, showsnd);
}
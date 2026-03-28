// Koka generated module: handlers/named/unify, koka version: 3.2.4
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
 
// externals
 
// type declarations
 
 
// runtime tag for the effect `:subst`
export var subst_fs__tag;
var subst_fs__tag = "subst@unify";
 
 
// runtime tag for the effect `:variable`
export var variable_fs__tag;
var variable_fs__tag = "variable@unify";
// type ntype
export function Con(tag) /* (tag : string) -> ntype */  {
  return { _tag: 1, tag: tag };
}
export function App(t1, t2) /* (t1 : ntype, t2 : ntype) -> ntype */  {
  return { _tag: 2, t1: t1, t2: t2 };
}
// type variable
export function _Hnd_variable(_cfc, _fun_get, _fun_resolve) /* forall<s,e,a> (int, hnd/clause0<maybe<utype<s>>,variable<s>,e,a>, hnd/clause1<utype<s>,(),variable<s>,e,a>) -> variable<s,e,a> */  {
  return { _cfc: _cfc, _fun_get: _fun_get, _fun_resolve: _fun_resolve };
}
// type utype
export function UVar(v) /* forall<s> (v : hnd/ev<variable<s>>) -> utype<s> */  {
  return { _tag: 1, v: v };
}
export function UCon(tag) /* forall<s> (tag : string) -> utype<s> */  {
  return { _tag: 2, tag: tag };
}
export function UApp(t1, t2) /* forall<s> (t1 : utype<s>, t2 : utype<s>) -> utype<s> */  {
  return { _tag: 3, t1: t1, t2: t2 };
}
// type subst
export function _Hnd_subst(_cfc, _ctl_fresh) /* forall<s,e,a> (int, hnd/clause0<hnd/ev<variable<s>>,subst<s>,e,a>) -> subst<s,e,a> */  {
  return { _cfc: _cfc, _ctl_fresh: _ctl_fresh };
}
 
// declarations
 
 
// Automatically generated. Tests for the `UVar` constructor of the `:utype` type.
export function is_uvar(utype) /* forall<s> (utype : utype<s>) -> bool */  {
  return (utype._tag === 1);
}
 
 
// Automatically generated. Tests for the `UCon` constructor of the `:utype` type.
export function is_ucon(utype) /* forall<s> (utype : utype<s>) -> bool */  {
  return (utype._tag === 2);
}
 
 
// Automatically generated. Tests for the `UApp` constructor of the `:utype` type.
export function is_uapp(utype) /* forall<s> (utype : utype<s>) -> bool */  {
  return (utype._tag === 3);
}
 
 
// Automatically generated. Tests for the `Con` constructor of the `:ntype` type.
export function is_con(ntype) /* (ntype : ntype) -> bool */  {
  return (ntype._tag === 1);
}
 
 
// Automatically generated. Tests for the `App` constructor of the `:ntype` type.
export function is_app(ntype) /* (ntype : ntype) -> bool */  {
  return (ntype._tag === 2);
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:subst` type.
export function subst_fs__cfc(subst_0) /* forall<s,e,a> (subst : subst<s,e,a>) -> int */  {
  return subst_0._cfc;
}
 
 
// handler for the effect `:subst`
export function subst_fs__handle(hnd, ret, action) /* forall<s,a,e,b> (hnd : subst<s,e,b>, ret : (res : a) -> e b, action : forall<s1> () -> <subst<s1>|e> a) -> e b */  {
  return $std_core_hnd._hhandle(subst_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@ctl-fresh` constructor field of the `:subst` type.
export function subst_fs__ctl_fresh(subst_0) /* forall<s,e,a> (subst : subst<s,e,a>) -> hnd/clause0<hnd/ev<variable<s>>,subst<s>,e,a> */  {
  return subst_0._ctl_fresh;
}
 
 
// select `fresh` operation out of effect `:subst`
export function fresh_fs__select(hnd) /* forall<s,e,a> (hnd : subst<s,e,a>) -> hnd/clause0<hnd/ev<variable<s>>,subst<s>,e,a> */  {
  return hnd._ctl_fresh;
}
 
 
// Call the `ctl fresh` operation of the effect `:subst`
export function fresh() /* forall<s> () -> (subst<s>) hnd/ev<variable<s>> */  {
   
  var ev_10143 = $std_core_hnd._evv_at(0);
  return ev_10143.hnd._ctl_fresh(ev_10143.marker, ev_10143);
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:variable` type.
export function variable_fs__cfc(variable) /* forall<s,e,a> (variable : variable<s,e,a>) -> int */  {
  return variable._cfc;
}
 
 
// handler for the effect `:variable`
export function variable_fs__handle(hnd, ret, action) /* forall<s,a,e,b> (hnd : variable<s,e,b>, ret : (res : a) -> e b, action : (hname : hnd/ev<variable<s>>) -> e a) -> e b */  {
  return $std_core_hnd._named_handle(variable_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-get` constructor field of the `:variable` type.
export function variable_fs__fun_get(variable) /* forall<s,e,a> (variable : variable<s,e,a>) -> hnd/clause0<maybe<utype<s>>,variable<s>,e,a> */  {
  return variable._fun_get;
}
 
 
// select `get` operation out of effect `:variable`
export function get_fs__select(hnd) /* forall<s,e,a> (hnd : variable<s,e,a>) -> hnd/clause0<maybe<utype<s>>,variable<s>,e,a> */  {
  return hnd._fun_get;
}
 
 
// named under umbrella effect `:subst`
// Call the `fun get` operation of the effect `:variable`
export function get(_hname) /* forall<s> (hnd/ev<variable<s>>) -> <div,subst<s>,exn> maybe<utype<s>> */  {
  return _hname.hnd._fun_get(_hname.marker, _hname);
}
 
 
// Automatically generated. Retrieves the `@fun-resolve` constructor field of the `:variable` type.
export function variable_fs__fun_resolve(variable) /* forall<s,e,a> (variable : variable<s,e,a>) -> hnd/clause1<utype<s>,(),variable<s>,e,a> */  {
  return variable._fun_resolve;
}
 
 
// select `resolve` operation out of effect `:variable`
export function resolve_fs__select(hnd) /* forall<s,e,a> (hnd : variable<s,e,a>) -> hnd/clause1<utype<s>,(),variable<s>,e,a> */  {
  return hnd._fun_resolve;
}
 
 
// `:(ev<variable<s,a>>)          -> <subst<s>,pure> maybe<utype<s>>`
// Call the `fun resolve` operation of the effect `:variable`
export function resolve(_hname, tp) /* forall<s> (hnd/ev<variable<s>>, tp : utype<s>) -> <div,subst<s>,exn> () */  {
  return _hname.hnd._fun_resolve(_hname.marker, _hname, tp);
}
 
 
// monadic lift
export function _mlift_with_var_10121(mtp, tp, _y_x10029) /* forall<h,s,e> (mtp : local-var<h,maybe<utype<s>>>, tp : utype<s>, maybe<utype<s>>) -> <local<h>,exn|e> () */  {
  if (_y_x10029 === null) {
    return ((mtp).value = ($std_core_types.Just(tp)));
  }
  else {
    return $std_core_hnd._open_at2($std_core_hnd._evv_index($std_core_exn.exn_fs__tag), $std_core_exn.$throw, "cannot resolve a unification variable more than once");
  }
}
 
 
// private (named) handler instance for creating a unification variable
export function with_var(action) /* forall<s,a,e> (action : (hnd/ev<variable<s>>) -> <exn|e> a) -> <exn|e> a */  {
  return function() {
     
    var loc = { value: ($std_core_types.Nothing) };
     
    var res = variable_fs__handle(_Hnd_variable(1, $std_core_hnd.clause_tail0(function() {
          return ((loc).value);
        }), $std_core_hnd.clause_tail1(function(tp /* utype<1136> */ ) {
           
          var x_10153 = ((loc).value);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10029 /* maybe<utype<1136>> */ ) {
              return _mlift_with_var_10121(loc, tp, _y_x10029);
            });
          }
          else {
            return _mlift_with_var_10121(loc, tp, x_10153);
          }
        })), function(_res /* 1137 */ ) {
        return _res;
      }, action);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// umbrella handler for substitution
export function subst(action) /* forall<a,e> (action : forall<s> () -> <subst<s>,pure|e> a) -> <pure|e> a */  {
  return subst_fs__handle(_Hnd_subst(3, function(m /* hnd/marker<<exn,div|1261>,1260> */ , ___wildcard_x688__16 /* hnd/ev<subst<_1176>> */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<hnd/ev<variable<_1176>>,1260>) -> <exn,div|1261> 1260 */ ) {
            return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (hnd/ev<variable<_1176>>) -> <exn,div|1261> 1260 */ ) {
                return with_var(r);
              }, k);
          });
      }), function(_res /* 1260 */ ) {
      return _res;
    }, function() {
      return action();
    });
}
 
 
// monadic lift
export function _mlift_trmc_resolve_all_10122(_acc, tp2, _trmc_x10011) /* forall<s> (ctx<ntype>, tp2 : utype<s>, ntype) -> <pure,subst<s>> ntype */  {
   
  var _trmc_x10012 = undefined;
   
  var _trmc_x10013 = App(_trmc_x10011, _trmc_x10012);
  return _trmc_resolve_all(tp2, $std_core_types._cctx_extend(_acc,_trmc_x10013,({obj: _trmc_x10013, field_name: "t2"})));
}
 
 
// monadic lift
export function _mlift_trmc_resolve_all_10123(_acc_0, _y_x10044) /* forall<s> (ctx<ntype>, ntype) -> <exn,div,subst<s>> ntype */  {
  return $std_core_types._cctx_apply(_acc_0,_y_x10044);
}
 
 
// monadic lift
export function _mlift_trmc_resolve_all_10124(_acc_1, _y_x10043) /* forall<s> (ctx<ntype>, maybe<utype<s>>) -> <div,exn,subst<s>> ntype */  {
  if (_y_x10043 === null) {
     
    var x_10157 = $std_core_hnd._open_at2(0, $std_core_exn.$throw, "unresolved variable");
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10044_0 /* ntype */ ) {
        return _mlift_trmc_resolve_all_10123(_acc_1, _y_x10044_0);
      });
    }
    else {
      return _mlift_trmc_resolve_all_10123(_acc_1, x_10157);
    }
  }
  else {
    return _trmc_resolve_all(_y_x10043.value, _acc_1);
  }
}
 
 
// monadic lift
export function _mlift_trmcm_resolve_all_10125(_accm, tp2_0, _trmc_x10016) /* forall<s> ((ntype) -> ntype, tp2 : utype<s>, ntype) -> <pure,subst<s>> ntype */  {
  return _trmcm_resolve_all(tp2_0, function(_trmc_x10015 /* ntype */ ) {
      return _accm(App(_trmc_x10016, _trmc_x10015));
    });
}
 
 
// monadic lift
export function _mlift_trmcm_resolve_all_10126(_accm_0, _y_x10050) /* forall<s> ((ntype) -> ntype, maybe<utype<s>>) -> <div,exn,subst<s>> ntype */  {
  if (_y_x10050 === null) {
     
    var x_0_10159 = $std_core_hnd._open_at2(0, $std_core_exn.$throw, "unresolved variable");
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_accm_0);
    }
    else {
      return _accm_0(x_0_10159);
    }
  }
  else {
    return _trmcm_resolve_all(_y_x10050.value, _accm_0);
  }
}
 
 
// resolve all unification variables to a non-unifiable type
export function _trmc_resolve_all(tp, _acc_2) /* forall<s> (tp : utype<s>, ctx<ntype>) -> <pure,subst<s>> ntype */  { tailcall: while(1)
{
  if (tp._tag === 2) {
    return $std_core_types._cctx_apply(_acc_2,(Con(tp.tag)));
  }
  else if (tp._tag === 3) {
     
    var x_1_10161 = resolve_all(tp.t1);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10011_0 /* ntype */ ) {
        return _mlift_trmc_resolve_all_10122(_acc_2, tp.t2, _trmc_x10011_0);
      });
    }
    else {
       
      var _trmc_x10012_0 = undefined;
       
      var _trmc_x10013_0 = App(x_1_10161, _trmc_x10012_0);
      {
        // tail call
        var _x0 = $std_core_types._cctx_extend(_acc_2,_trmc_x10013_0,({obj: _trmc_x10013_0, field_name: "t2"}));
        tp = tp.t2;
        _acc_2 = _x0;
        continue tailcall;
      }
    }
  }
  else {
     
    var x_2_10164 = tp.v.hnd._fun_get(tp.v.marker, tp.v);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10043_0 /* maybe<utype<1360>> */ ) {
        return _mlift_trmc_resolve_all_10124(_acc_2, _y_x10043_0);
      });
    }
    else {
      if (x_2_10164 === null) {
         
        var x_3_10169 = $std_core_hnd._open_at2(0, $std_core_exn.$throw, "unresolved variable");
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10044_1 /* ntype */ ) {
            return _mlift_trmc_resolve_all_10123(_acc_2, _y_x10044_1);
          });
        }
        else {
          return $std_core_types._cctx_apply(_acc_2,x_3_10169);
        }
      }
      else {
        {
          // tail call
          tp = x_2_10164.value;
          continue tailcall;
        }
      }
    }
  }
}}
 
 
// resolve all unification variables to a non-unifiable type
export function _trmcm_resolve_all(tp_0, _accm_1) /* forall<s> (tp : utype<s>, (ntype) -> ntype) -> <pure,subst<s>> ntype */  { tailcall: while(1)
{
  if (tp_0._tag === 2) {
    return _accm_1(Con(tp_0.tag));
  }
  else if (tp_0._tag === 3) {
     
    var x_4_10172 = resolve_all(tp_0.t1);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10016_0 /* ntype */ ) {
        return _mlift_trmcm_resolve_all_10125(_accm_1, tp_0.t2, _trmc_x10016_0);
      });
    }
    else {
      {
        // tail call
        var _x3 = function(__at_accm_11 /* (ntype) -> ntype */ , _x_4_101722 /* ntype */ ) {
          return function(_trmc_x10015_0 /* ntype */ ) {
            return __at_accm_11(App(_x_4_101722, _trmc_x10015_0));
          };
        }(_accm_1, x_4_10172);
        tp_0 = tp_0.t2;
        _accm_1 = _x3;
        continue tailcall;
      }
    }
  }
  else {
     
    var x_5_10175 = tp_0.v.hnd._fun_get(tp_0.v.marker, tp_0.v);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10050_0 /* maybe<utype<1360>> */ ) {
        return _mlift_trmcm_resolve_all_10126(_accm_1, _y_x10050_0);
      });
    }
    else {
      if (x_5_10175 === null) {
         
        var x_6_10180 = $std_core_hnd._open_at2(0, $std_core_exn.$throw, "unresolved variable");
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(_accm_1);
        }
        else {
          return _accm_1(x_6_10180);
        }
      }
      else {
        {
          // tail call
          tp_0 = x_5_10175.value;
          continue tailcall;
        }
      }
    }
  }
}}
 
 
// resolve all unification variables to a non-unifiable type
export function resolve_all(tp_1) /* forall<s> (tp : utype<s>) -> <pure,subst<s>> ntype */  {
  var _x4 = $std_core_hnd._evv_is_affine();
  if (_x4) {
    return _trmc_resolve_all(tp_1, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_resolve_all(tp_1, function(_trmc_x10014 /* ntype */ ) {
        return _trmc_x10014;
      });
  }
}
 
 
// monadic lift
export function _mlift_trmc_unify_10127(_acc, tp12, tp22, _trmc_x10017) /* forall<s> (ctx<utype<s>>, tp12 : utype<s>, tp22 : utype<s>, utype<s>) -> <pure,subst<s>> utype<s> */  {
   
  var _trmc_x10018 = undefined;
   
  var _trmc_x10019 = UApp(_trmc_x10017, _trmc_x10018);
  return _trmc_unify(tp12, tp22, $std_core_types._cctx_extend(_acc,_trmc_x10019,({obj: _trmc_x10019, field_name: "t2"})));
}
 
 
// monadic lift
export function _mlift_trmc_unify_10128(_acc_0, tp2, wild__) /* forall<s> (ctx<utype<s>>, tp2 : utype<s>, wild_ : ()) -> <div,exn,subst<s>> utype<s> */  {
  return $std_core_types._cctx_apply(_acc_0,tp2);
}
 
 
// monadic lift
export function _mlift_trmc_unify_10129(_acc_1, tpv1, wild___0) /* forall<s> (ctx<utype<s>>, tpv1 : utype<s>, wild_@0 : ()) -> <div,exn,subst<s>> utype<s> */  {
  return $std_core_types._cctx_apply(_acc_1,tpv1);
}
 
 
// monadic lift
export function _mlift_trmc_unify_10130(_acc_2, tpv1_0, v2_0, _y_x10062) /* forall<s> (ctx<utype<s>>, tpv1 : utype<s>, v2@0 : hnd/ev<variable<s>>, maybe<utype<s>>) -> <div,exn,subst<s>> utype<s> */  {
  if (_y_x10062 === null) {
     
    var x_10182 = v2_0.hnd._fun_resolve(v2_0.marker, v2_0, tpv1_0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___0_0 /* () */ ) {
        return _mlift_trmc_unify_10129(_acc_2, tpv1_0, wild___0_0);
      });
    }
    else {
      return _mlift_trmc_unify_10129(_acc_2, tpv1_0, x_10182);
    }
  }
  else {
    return _trmc_unify(tpv1_0, _y_x10062.value, _acc_2);
  }
}
 
 
// monadic lift
export function _mlift_trmc_unify_10131(_acc_3, tp2_0, v1_0, _y_x10060) /* forall<s> (ctx<utype<s>>, tp2 : utype<s>, v1@0 : hnd/ev<variable<s>>, maybe<utype<s>>) -> <div,exn,subst<s>> utype<s> */  {
  if (_y_x10060 === null) {
     
    var x_1_10187 = v1_0.hnd._fun_resolve(v1_0.marker, v1_0, tp2_0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___1 /* () */ ) {
        return _mlift_trmc_unify_10128(_acc_3, tp2_0, wild___1);
      });
    }
    else {
      return _mlift_trmc_unify_10128(_acc_3, tp2_0, x_1_10187);
    }
  }
  else {
    if (tp2_0._tag === 1) {
       
      var x_3_10192 = tp2_0.v.hnd._fun_get(tp2_0.v.marker, tp2_0.v);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10062_0 /* maybe<utype<1791>> */ ) {
          return _mlift_trmc_unify_10130(_acc_3, _y_x10060.value, tp2_0.v, _y_x10062_0);
        });
      }
      else {
        return _mlift_trmc_unify_10130(_acc_3, _y_x10060.value, tp2_0.v, x_3_10192);
      }
    }
    else {
      return _trmc_unify(_y_x10060.value, tp2_0, _acc_3);
    }
  }
}
 
 
// monadic lift
export function _mlift_trmc_unify_10132(_acc_4, _y_x10069) /* forall<s> (ctx<utype<s>>, utype<s>) -> <exn,div,subst<s>> utype<s> */  {
  return $std_core_types._cctx_apply(_acc_4,_y_x10069);
}
 
 
// monadic lift
export function _mlift_trmcm_unify_10133(_accm, tp12_0, tp22_0, _trmc_x10022) /* forall<s> ((utype<s>) -> utype<s>, tp12 : utype<s>, tp22 : utype<s>, utype<s>) -> <pure,subst<s>> utype<s> */  {
  return _trmcm_unify(tp12_0, tp22_0, function(_trmc_x10021 /* utype<1791> */ ) {
      return _accm(UApp(_trmc_x10022, _trmc_x10021));
    });
}
 
 
// monadic lift
export function _mlift_trmcm_unify_10134(_accm_0, tp2_1, wild___2) /* forall<s> ((utype<s>) -> utype<s>, tp2 : utype<s>, wild_ : ()) -> <div,exn,subst<s>> utype<s> */  {
  return _accm_0(tp2_1);
}
 
 
// monadic lift
export function _mlift_trmcm_unify_10135(_accm_1, tpv1_2, wild___0_1) /* forall<s> ((utype<s>) -> utype<s>, tpv1 : utype<s>, wild_@0 : ()) -> <div,exn,subst<s>> utype<s> */  {
  return _accm_1(tpv1_2);
}
 
 
// monadic lift
export function _mlift_trmcm_unify_10136(_accm_2, tpv1_3, v2_0_1, _y_x10075) /* forall<s> ((utype<s>) -> utype<s>, tpv1 : utype<s>, v2@0 : hnd/ev<variable<s>>, maybe<utype<s>>) -> <div,exn,subst<s>> utype<s> */  {
  if (_y_x10075 === null) {
     
    var x_4_10196 = v2_0_1.hnd._fun_resolve(v2_0_1.marker, v2_0_1, tpv1_3);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___0_2 /* () */ ) {
        return _mlift_trmcm_unify_10135(_accm_2, tpv1_3, wild___0_2);
      });
    }
    else {
      return _mlift_trmcm_unify_10135(_accm_2, tpv1_3, x_4_10196);
    }
  }
  else {
    return _trmcm_unify(tpv1_3, _y_x10075.value, _accm_2);
  }
}
 
 
// monadic lift
export function _mlift_trmcm_unify_10137(_accm_3, tp2_2, v1_0_0, _y_x10073) /* forall<s> ((utype<s>) -> utype<s>, tp2 : utype<s>, v1@0 : hnd/ev<variable<s>>, maybe<utype<s>>) -> <div,exn,subst<s>> utype<s> */  {
  if (_y_x10073 === null) {
     
    var x_6_10201 = v1_0_0.hnd._fun_resolve(v1_0_0.marker, v1_0_0, tp2_2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___3 /* () */ ) {
        return _mlift_trmcm_unify_10134(_accm_3, tp2_2, wild___3);
      });
    }
    else {
      return _mlift_trmcm_unify_10134(_accm_3, tp2_2, x_6_10201);
    }
  }
  else {
    if (tp2_2._tag === 1) {
       
      var x_8_10206 = tp2_2.v.hnd._fun_get(tp2_2.v.marker, tp2_2.v);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10075_0 /* maybe<utype<1791>> */ ) {
          return _mlift_trmcm_unify_10136(_accm_3, _y_x10073.value, tp2_2.v, _y_x10075_0);
        });
      }
      else {
        return _mlift_trmcm_unify_10136(_accm_3, _y_x10073.value, tp2_2.v, x_8_10206);
      }
    }
    else {
      return _trmcm_unify(_y_x10073.value, tp2_2, _accm_3);
    }
  }
}
 
 
// Unify two types under a substitution handler
export function _trmc_unify(tp1, tp2_3, _acc_5) /* forall<s> (tp1 : utype<s>, tp2 : utype<s>, ctx<utype<s>>) -> <pure,subst<s>> utype<s> */  { tailcall: while(1)
{
  if (tp1._tag === 2 && tp2_3._tag === 2) {
    if (((tp1.tag) === (tp2_3.tag))){
      return $std_core_types._cctx_apply(_acc_5,tp1);
    }
  }
  if (tp1._tag === 1 && tp2_3._tag === 1) {
    var _x5 = $std_core_hnd.eq_marker(tp1.v.marker, tp2_3.v.marker);
    if (_x5){
      return $std_core_types._cctx_apply(_acc_5,tp1);
    }
  }
  if (tp1._tag === 3 && tp2_3._tag === 3) {
     
    var x_9_10212 = unify(tp1.t1, tp2_3.t1);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10017_0 /* utype<1791> */ ) {
        return _mlift_trmc_unify_10127(_acc_5, tp1.t2, tp2_3.t2, _trmc_x10017_0);
      });
    }
    else {
       
      var _trmc_x10018_0 = undefined;
       
      var _trmc_x10019_0 = UApp(x_9_10212, _trmc_x10018_0);
      {
        // tail call
        var _x6 = $std_core_types._cctx_extend(_acc_5,_trmc_x10019_0,({obj: _trmc_x10019_0, field_name: "t2"}));
        tp1 = tp1.t2;
        tp2_3 = tp2_3.t2;
        _acc_5 = _x6;
        continue tailcall;
      }
    }
  }
  if (tp1._tag === 1) {
     
    var x_10_10215 = tp1.v.hnd._fun_get(tp1.v.marker, tp1.v);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10060_0 /* maybe<utype<1791>> */ ) {
        return _mlift_trmc_unify_10131(_acc_5, tp2_3, tp1.v, _y_x10060_0);
      });
    }
    else {
      if (x_10_10215 === null) {
         
        var x_11_10220 = tp1.v.hnd._fun_resolve(tp1.v.marker, tp1.v, tp2_3);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(wild___4 /* () */ ) {
            return _mlift_trmc_unify_10128(_acc_5, tp2_3, wild___4);
          });
        }
        else {
          return $std_core_types._cctx_apply(_acc_5,tp2_3);
        }
      }
      else {
        if (tp2_3._tag === 1) {
           
          var x_13_10226 = tp2_3.v.hnd._fun_get(tp2_3.v.marker, tp2_3.v);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10062_1 /* maybe<utype<1791>> */ ) {
              return _mlift_trmc_unify_10130(_acc_5, x_10_10215.value, tp2_3.v, _y_x10062_1);
            });
          }
          else {
            if (x_13_10226 === null) {
               
              var x_14_10231 = tp2_3.v.hnd._fun_resolve(tp2_3.v.marker, tp2_3.v, x_10_10215.value);
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(function(wild___0_3 /* () */ ) {
                  return _mlift_trmc_unify_10129(_acc_5, x_10_10215.value, wild___0_3);
                });
              }
              else {
                return $std_core_types._cctx_apply(_acc_5,(x_10_10215.value));
              }
            }
            else {
              {
                // tail call
                tp1 = x_10_10215.value;
                tp2_3 = x_13_10226.value;
                continue tailcall;
              }
            }
          }
        }
        else {
          {
            // tail call
            tp1 = x_10_10215.value;
            continue tailcall;
          }
        }
      }
    }
  }
   
  var x_16_10237 = $std_core_hnd._open_at2(0, $std_core_exn.$throw, "cannot unify types");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10069_0 /* utype<1791> */ ) {
      return _mlift_trmc_unify_10132(_acc_5, _y_x10069_0);
    });
  }
  else {
    return $std_core_types._cctx_apply(_acc_5,x_16_10237);
  }
}}
 
 
// Unify two types under a substitution handler
export function _trmcm_unify(tp1_0, tp2_4, _accm_4) /* forall<s> (tp1 : utype<s>, tp2 : utype<s>, (utype<s>) -> utype<s>) -> <pure,subst<s>> utype<s> */  { tailcall: while(1)
{
  if (tp1_0._tag === 2 && tp2_4._tag === 2) {
    if (((tp1_0.tag) === (tp2_4.tag))){
      return _accm_4(tp1_0);
    }
  }
  if (tp1_0._tag === 1 && tp2_4._tag === 1) {
    var _x7 = $std_core_hnd.eq_marker(tp1_0.v.marker, tp2_4.v.marker);
    if (_x7){
      return _accm_4(tp1_0);
    }
  }
  if (tp1_0._tag === 3 && tp2_4._tag === 3) {
     
    var x_17_10242 = unify(tp1_0.t1, tp2_4.t1);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10022_0 /* utype<1791> */ ) {
        return _mlift_trmcm_unify_10133(_accm_4, tp1_0.t2, tp2_4.t2, _trmc_x10022_0);
      });
    }
    else {
      {
        // tail call
        var _x10 = function(__at_accm_48 /* (utype<1791>) -> utype<1791> */ , _x_17_102429 /* utype<1791> */ ) {
          return function(_trmc_x10021_0 /* utype<1791> */ ) {
            return __at_accm_48(UApp(_x_17_102429, _trmc_x10021_0));
          };
        }(_accm_4, x_17_10242);
        tp1_0 = tp1_0.t2;
        tp2_4 = tp2_4.t2;
        _accm_4 = _x10;
        continue tailcall;
      }
    }
  }
  if (tp1_0._tag === 1) {
     
    var x_18_10245 = tp1_0.v.hnd._fun_get(tp1_0.v.marker, tp1_0.v);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10073_0 /* maybe<utype<1791>> */ ) {
        return _mlift_trmcm_unify_10137(_accm_4, tp2_4, tp1_0.v, _y_x10073_0);
      });
    }
    else {
      if (x_18_10245 === null) {
         
        var x_19_10250 = tp1_0.v.hnd._fun_resolve(tp1_0.v.marker, tp1_0.v, tp2_4);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(wild___5 /* () */ ) {
            return _mlift_trmcm_unify_10134(_accm_4, tp2_4, wild___5);
          });
        }
        else {
          return _accm_4(tp2_4);
        }
      }
      else {
        if (tp2_4._tag === 1) {
           
          var x_21_10256 = tp2_4.v.hnd._fun_get(tp2_4.v.marker, tp2_4.v);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10075_1 /* maybe<utype<1791>> */ ) {
              return _mlift_trmcm_unify_10136(_accm_4, x_18_10245.value, tp2_4.v, _y_x10075_1);
            });
          }
          else {
            if (x_21_10256 === null) {
               
              var x_22_10261 = tp2_4.v.hnd._fun_resolve(tp2_4.v.marker, tp2_4.v, x_18_10245.value);
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(function(wild___0_4 /* () */ ) {
                  return _mlift_trmcm_unify_10135(_accm_4, x_18_10245.value, wild___0_4);
                });
              }
              else {
                return _accm_4(x_18_10245.value);
              }
            }
            else {
              {
                // tail call
                tp1_0 = x_18_10245.value;
                tp2_4 = x_21_10256.value;
                continue tailcall;
              }
            }
          }
        }
        else {
          {
            // tail call
            tp1_0 = x_18_10245.value;
            continue tailcall;
          }
        }
      }
    }
  }
   
  var x_24_10267 = $std_core_hnd._open_at2(0, $std_core_exn.$throw, "cannot unify types");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_accm_4);
  }
  else {
    return _accm_4(x_24_10267);
  }
}}
 
 
// Unify two types under a substitution handler
export function unify(tp1_1, tp2_5) /* forall<s> (tp1 : utype<s>, tp2 : utype<s>) -> <pure,subst<s>> utype<s> */  {
  var _x11 = $std_core_hnd._evv_is_affine();
  if (_x11) {
    return _trmc_unify(tp1_1, tp2_5, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_unify(tp1_1, tp2_5, function(_trmc_x10020 /* utype<1791> */ ) {
        return _trmc_x10020;
      });
  }
}
 
 
// Helpers to create types
export function inttp() /* forall<s> () -> utype<s> */  {
  return UCon("int");
}
 
export function list(tp1) /* forall<s> (tp1 : utype<s>) -> utype<s> */  {
  return UApp(UCon("list"), tp1);
}
 
export function to(tp1, tp2) /* forall<s> (tp1 : utype<s>, tp2 : utype<s>) -> utype<s> */  {
  return UApp(UApp(UCon("->"), tp1), tp2);
}
 
export function show(tp, top) /* (tp : ntype, top : ? bool) -> string */  {
  if (tp._tag === 2 && tp.t1._tag === 2 && tp.t1.t1._tag === 1 && tp.t1.t1.tag === "->") {
     
    var s_10009 = $std_core_types._lp__plus__plus__rp_(show(tp.t1.t2), $std_core_types._lp__plus__plus__rp_(" -> ", show(tp.t2)));
    if (top !== undefined) {
      if (top) {
        return s_10009;
      }
      else {
        return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(s_10009, ")"));
      }
    }
    else {
      return s_10009;
    }
  }
  else if (tp._tag === 1) {
    return tp.tag;
  }
  else {
     
    var s_10010 = $std_core_types._lp__plus__plus__rp_(show(tp.t1), $std_core_types._lp__plus__plus__rp_(" ", show(tp.t2, false)));
    if (top !== undefined) {
      if (top) {
        return s_10010;
      }
      else {
        return $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_(s_10010, ")"));
      }
    }
    else {
      return s_10010;
    }
  }
}
 
 
// monadic lift
export function _mlift_example_10138(_y_x10092) /* forall<s> (utype<s>) -> <pure,subst<s>> ntype */  {
  return resolve_all(_y_x10092);
}
 
 
// monadic lift
export function _mlift_example_10139(a, b) /* forall<s> (a : hnd/ev<variable<s>>, b : hnd/ev<variable<s>>) -> <subst<s>,div,exn> ntype */  {
   
  var tp1_10271 = UApp(UApp(UCon("->"), UVar(a)), UVar(a));
   
  var tp2_10272 = UApp(UApp(UCon("->"), UVar(b)), UApp(UCon("list"), UCon("int")));
   
  var _x12 = $std_core_hnd._evv_is_affine();
  if (_x12) {
    var x_10269 = _trmc_unify(tp1_10271, tp2_10272, $std_core_types._cctx_empty());
  }
  else {
    var x_10269 = _trmcm_unify(tp1_10271, tp2_10272, function(_trmc_x10020 /* utype<1249> */ ) {
        return _trmc_x10020;
      });
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10092 /* utype<1249> */ ) {
      return resolve_all(_y_x10092);
    });
  }
  else {
    return resolve_all(x_10269);
  }
}
 
 
// monadic lift
export function _mlift_example_10140(a) /* forall<s> (a : hnd/ev<variable<s>>) -> <subst<s>,div,exn> ntype */  {
   
  var x_10274 = $std_core_hnd._open_at0(1, function() {
       
      var ev_10276 = $std_core_hnd._evv_at(0);
      return ev_10276.hnd._ctl_fresh(ev_10276.marker, ev_10276);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(b /* hnd/ev<variable<1249>> */ ) {
      return _mlift_example_10139(a, b);
    });
  }
  else {
    return _mlift_example_10139(a, x_10274);
  }
}
 
 
// Test unification
export function example() /* () -> pure ntype */  {
  return subst_fs__handle(_Hnd_subst(3, function(m /* hnd/marker<<exn,div>,ntype> */ , ___wildcard_x688__16 /* hnd/ev<subst<_1176>> */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<hnd/ev<variable<_1176>>,ntype>) -> <exn,div> ntype */ ) {
            return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (hnd/ev<variable<_1176>>) -> <exn,div> ntype */ ) {
                return with_var(r);
              }, k);
          });
      }), function(_res /* ntype */ ) {
      return _res;
    }, function() {
       
      var x_10279 = $std_core_hnd._open_at0(1, function() {
           
          var ev_10281 = $std_core_hnd._evv_at(0);
          return ev_10281.hnd._ctl_fresh(ev_10281.marker, ev_10281);
        });
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(a /* hnd/ev<variable<1249>> */ ) {
          return _mlift_example_10140(a);
        });
      }
      else {
        return _mlift_example_10140(x_10279);
      }
    });
}
 
 
// monadic lift
export function _mlift_main_10141(tp) /* (tp : ntype) -> pure () */  {
  return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_("unified type: ", $std_core_hnd._open_none2(show, tp)));
}
 
export function main() /* () -> <pure,console/console> () */  {
   
  var x_10283 = example();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_main_10141);
  }
  else {
    return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_("unified type: ", $std_core_hnd._open_none2(show, x_10283)));
  }
}
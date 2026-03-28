// Koka generated module: handlers/named/heap, koka version: 3.2.4
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
 
 
// runtime tag for the effect `:heap`
export var heap_fs__tag;
var heap_fs__tag = "heap@heap";
 
 
// runtime tag for the effect `:ref`
export var ref_fs__tag;
var ref_fs__tag = "ref@heap";
// type ref
export function _Hnd_ref(_cfc, _fun_get, _fun_set) /* forall<s,a,e,b> (int, hnd/clause0<a,ref<s,a>,e,b>, hnd/clause1<a,(),ref<s,a>,e,b>) -> ref<s,a,e,b> */  {
  return { _cfc: _cfc, _fun_get: _fun_get, _fun_set: _fun_set };
}
// type heap
export function _Hnd_heap(_cfc, _ctl_new_ref) /* forall<s,e,a> (int, forall<b> hnd/clause1<b,hnd/ev<ref<s,b>>,heap<s>,e,a>) -> heap<s,e,a> */  {
  return { _cfc: _cfc, _ctl_new_ref: _ctl_new_ref };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:heap` type.
export function heap_fs__cfc(heap_0) /* forall<s,e,a> (heap : heap<s,e,a>) -> int */  {
  return heap_0._cfc;
}
 
 
// handler for the effect `:heap`
export function heap_fs__handle(hnd, ret, action) /* forall<s,a,e,b> (hnd : heap<s,e,b>, ret : (res : a) -> e b, action : forall<s1> () -> <heap<s1>|e> a) -> e b */  {
  return $std_core_hnd._hhandle(heap_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@ctl-new-ref` constructor field of the `:heap` type.
export function heap_fs__ctl_new_ref(heap_0) /* forall<s,e,a,b> (heap : heap<s,e,a>) -> hnd/clause1<b,hnd/ev<ref<s,b>>,heap<s>,e,a> */  {
  return heap_0._ctl_new_ref;
}
 
 
// select `new-ref` operation out of effect `:heap`
export function new_ref_fs__select(hnd) /* forall<s,a,e,b> (hnd : heap<s,e,b>) -> hnd/clause1<a,hnd/ev<ref<s,a>>,heap<s>,e,b> */  {
  return hnd._ctl_new_ref;
}
 
 
// Call the `ctl new-ref` operation of the effect `:heap`
export function new_ref(init) /* forall<s,a> (init : a) -> (heap<s>) hnd/ev<ref<s,a>> */  {
   
  var ev_10044 = $std_core_hnd._evv_at(0);
  var _x0 = ev_10044.hnd._ctl_new_ref;
  return _x0(ev_10044.marker, ev_10044, init);
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:ref` type.
export function ref_fs__cfc(ref) /* forall<s,a,e,b> (ref : ref<s,a,e,b>) -> int */  {
  return ref._cfc;
}
 
 
// handler for the effect `:ref`
export function ref_fs__handle(hnd, ret, action) /* forall<s,a,b,e,c> (hnd : ref<s,a,e,c>, ret : (res : b) -> e c, action : (hname : hnd/ev<ref<s,a>>) -> e b) -> e c */  {
  return $std_core_hnd._named_handle(ref_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-get` constructor field of the `:ref` type.
export function ref_fs__fun_get(ref) /* forall<s,a,e,b> (ref : ref<s,a,e,b>) -> hnd/clause0<a,ref<s,a>,e,b> */  {
  return ref._fun_get;
}
 
 
// select `get` operation out of effect `:ref`
export function get_fs__select(hnd) /* forall<s,a,e,b> (hnd : ref<s,a,e,b>) -> hnd/clause0<a,ref<s,a>,e,b> */  {
  return hnd._fun_get;
}
 
 
// named under umbrella effect `:heap`
// Call the `fun get` operation of the effect `:ref`
export function get(_hname) /* forall<s,a> (hnd/ev<ref<s,a>>) -> <div,heap<s>,exn> a */  {
  return _hname.hnd._fun_get(_hname.marker, _hname);
}
 
 
// Automatically generated. Retrieves the `@fun-set` constructor field of the `:ref` type.
export function ref_fs__fun_set(ref) /* forall<s,a,e,b> (ref : ref<s,a,e,b>) -> hnd/clause1<a,(),ref<s,a>,e,b> */  {
  return ref._fun_set;
}
 
 
// select `set` operation out of effect `:ref`
export function set_fs__select(hnd) /* forall<s,a,e,b> (hnd : ref<s,a,e,b>) -> hnd/clause1<a,(),ref<s,a>,e,b> */  {
  return hnd._fun_set;
}
 
 
// `:(ev<ref<s,a>>)   -> <heap<s>,pure> a`
// Call the `fun set` operation of the effect `:ref`
export function set(_hname, value) /* forall<s,a> (hnd/ev<ref<s,a>>, value : a) -> <div,heap<s>,exn> () */  {
  return _hname.hnd._fun_set(_hname.marker, _hname, value);
}
 
 
// private (named) handler instance for a reference
export function with_ref(init, action) /* forall<a,s,b,e> (init : a, action : (hnd/ev<ref<s,a>>) -> e b) -> e b */  {
  return function() {
     
    var loc = { value: init };
     
    var res = ref_fs__handle(_Hnd_ref(1, $std_core_hnd.clause_tail0(function() {
          return ((loc).value);
        }), $std_core_hnd.clause_tail1(function(x /* 1132 */ ) {
          return ((loc).value = x);
        })), function(_res /* 1134 */ ) {
        return _res;
      }, action);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// handler for a local heap
export function heap(action) /* forall<a,e> (action : forall<s> () -> <heap<s>,div|e> a) -> <div|e> a */  {
  return heap_fs__handle(_Hnd_heap(3, function(m /* hnd/marker<<div|1278>,1277> */ , ___wildcard_x639__16 /* hnd/ev<heap<_1180>> */ , x /* _1162 */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<hnd/ev<ref<_1180,_1162>>,1277>) -> <div|1278> 1277 */ ) {
            return $std_core_hnd.protect(x, function(init /* 1242 */ , resume /* (hnd/ev<ref<_1180,1242>>) -> <div|1278> 1277 */ ) {
                return with_ref(init, resume);
              }, k);
          });
      }), function(_res /* 1277 */ ) {
      return _res;
    }, function() {
      return action();
    });
}
 
 
// return a fresh reference from a function
export function dynamic_ref(init) /* forall<a,s> (init : a) -> (heap<s>) hnd/ev<ref<s,a>> */  {
   
  var ev_10057 = $std_core_hnd._evv_at(0);
  var _x1 = ev_10057.hnd._ctl_new_ref;
  return _x1(ev_10057.marker, ev_10057, init);
}
 
 
// monadic lift
export function _mlift_main_10039(_y_x10029, _y_x10030) /* forall<s> (int, bool) -> <div,exn,heap<s>> () */  {
   
  var _x2 = (_y_x10030) ? 1 : 0;
  var x_10004 = $std_core_types._int_add(_y_x10029,_x2);
   
  var x_10060 = $std_core_int.show(x_10004);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend($std_core_console.printsln);
  }
  else {
    return $std_core_console.printsln(x_10060);
  }
}
 
 
// monadic lift
export function _mlift_main_10040(r2, _y_x10029) /* forall<s> (r2 : hnd/ev<ref<s,bool>>, int) -> <div,exn,heap<s>> () */  {
   
  var x_10062 = r2.hnd._fun_get(r2.marker, r2);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10030 /* bool */ ) {
      return _mlift_main_10039(_y_x10029, _y_x10030);
    });
  }
  else {
    return _mlift_main_10039(_y_x10029, x_10062);
  }
}
 
 
// monadic lift
export function _mlift_main_10041(r1, r2) /* forall<s> (r1 : hnd/ev<ref<s,int>>, r2 : hnd/ev<ref<s,bool>>) -> <heap<s>,console/console,div,exn> () */  {
   
  var x_10066 = r1.hnd._fun_get(r1.marker, r1);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10029 /* int */ ) {
      return _mlift_main_10040(r2, _y_x10029);
    });
  }
  else {
    return _mlift_main_10040(r2, x_10066);
  }
}
 
 
// monadic lift
export function _mlift_main_10042(r1) /* forall<s> (r1 : hnd/ev<ref<s,int>>) -> <heap<s>,console/console,div,exn> () */  {
   
  var x_10070 = $std_core_hnd._open_at1(1, function(init_1 /* bool */ ) {
       
      var ev_10072 = $std_core_hnd._evv_at(0);
      var _x2 = ev_10072.hnd._ctl_new_ref;
      return _x2(ev_10072.marker, ev_10072, init_1);
    }, true);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(r2 /* hnd/ev<ref<1266,bool>> */ ) {
      return _mlift_main_10041(r1, r2);
    });
  }
  else {
    return _mlift_main_10041(r1, x_10070);
  }
}
 
export function main() /* () -> <pure,console/console> () */  {
  return heap_fs__handle(_Hnd_heap(3, function(m /* hnd/marker<<div,console/console,exn>,()> */ , ___wildcard_x639__16 /* hnd/ev<heap<_1180>> */ , x /* _1162 */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<hnd/ev<ref<_1180,_1162>>,()>) -> <div,console/console,exn> () */ ) {
            return $std_core_hnd.protect(x, function(init /* 1242 */ , resume /* (hnd/ev<ref<_1180,1242>>) -> <div,console/console,exn> () */ ) {
                return with_ref(init, resume);
              }, k);
          });
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var x_0_10076 = $std_core_hnd._open_at1(1, function(init_0 /* int */ ) {
           
          var ev_10078 = $std_core_hnd._evv_at(0);
          var _x2 = ev_10078.hnd._ctl_new_ref;
          return _x2(ev_10078.marker, ev_10078, init_0);
        }, 41);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(r1 /* hnd/ev<ref<1266,int>> */ ) {
          return _mlift_main_10042(r1);
        });
      }
      else {
        return _mlift_main_10042(x_0_10076);
      }
    });
}
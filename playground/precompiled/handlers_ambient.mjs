// Koka generated module: handlers/ambient, koka version: 3.2.4
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
 
 
// runtime tag for the effect `:width`
export var width_fs__tag;
var width_fs__tag = "width@ambient";
 
 
// runtime tag for the effect `:emitx`
export var emitx_fs__tag;
var emitx_fs__tag = "emitx@ambient";
 
 
// runtime tag for the effect `:emit`
export var emit_fs__tag;
var emit_fs__tag = "emit@ambient";
 
 
// runtime tag for the effect `:state`
export var state_fs__tag;
var state_fs__tag = "state@ambient";
 
 
// runtime tag for the effect `:total`
export var total_fs__tag;
var total_fs__tag = "total@ambient";
 
 
// runtime tag for the effect `:abort`
export var abort_fs__tag;
var abort_fs__tag = "abort@ambient";
 
 
// runtime tag for the effect `:choice`
export var choice_fs__tag;
var choice_fs__tag = "choice@ambient";
 
 
// runtime tag for the effect `:cwd`
export var cwd_fs__tag;
var cwd_fs__tag = "cwd@ambient";
 
 
// runtime tag for the effect `:marked`
export var marked_fs__tag;
var marked_fs__tag = "marked@ambient";
 
 
// runtime tag for the effect `:mark`
export var mark_fs__tag;
var mark_fs__tag = "mark@ambient";
 
 
// runtime tag for the effect `:children`
export var children_fs__tag;
var children_fs__tag = "children@ambient";
// type abort
export function _Hnd_abort(_cfc, _ctl_abort) /* forall<e,a> (int, hnd/clause0<(),abort,e,a>) -> abort<e,a> */  {
  return { _cfc: _cfc, _ctl_abort: _ctl_abort };
}
// type children
export function _Hnd_children(_cfc, _fun_children) /* forall<e,a> (int, hnd/clause1<vertex,list<vertex>,children,e,a>) -> children<e,a> */  {
  return { _cfc: _cfc, _fun_children: _fun_children };
}
// type choice
export function _Hnd_choice(_cfc, _ctl_choice) /* forall<e,a> (int, hnd/clause0<bool,choice,e,a>) -> choice<e,a> */  {
  return { _cfc: _cfc, _ctl_choice: _ctl_choice };
}
// type cwd
export function _Hnd_cwd(_cfc, _val_cwd) /* forall<e,a> (int, hnd/clause0<string,cwd,e,a>) -> cwd<e,a> */  {
  return { _cfc: _cfc, _val_cwd: _val_cwd };
}
// type emit
export function _Hnd_emit(_cfc, _fun_emit) /* forall<e,a> (int, hnd/clause1<string,(),emit,e,a>) -> emit<e,a> */  {
  return { _cfc: _cfc, _fun_emit: _fun_emit };
}
// type emitx
export function _Hnd_emitx(_cfc, _val_emitx) /* forall<e,a> (int, hnd/clause0<(s : string) -> io (),emitx,e,a>) -> emitx<e,a> */  {
  return { _cfc: _cfc, _val_emitx: _val_emitx };
}
// type graph
export const Graph = 1; // graph
// type mark
export function _Hnd_mark(_cfc, _fun_mark) /* forall<e,a> (int, hnd/clause1<vertex,(),mark,e,a>) -> mark<e,a> */  {
  return { _cfc: _cfc, _fun_mark: _fun_mark };
}
// type marked
export function _Hnd_marked(_cfc, _fun_marked) /* forall<e,a> (int, hnd/clause1<vertex,bool,marked,e,a>) -> marked<e,a> */  {
  return { _cfc: _cfc, _fun_marked: _fun_marked };
}
// type rose
export function Rose(v, sub) /* (v : vertex, sub : list<rose>) -> rose */  {
  return { v: v, sub: sub };
}
// type state
export function _Hnd_state(_cfc, _ctl_get, _ctl_set) /* forall<a,e,b> (int, hnd/clause0<a,state<a>,e,b>, hnd/clause1<a,(),state<a>,e,b>) -> state<a,e,b> */  {
  return { _cfc: _cfc, _ctl_get: _ctl_get, _ctl_set: _ctl_set };
}
// type total
export function _Hnd_total(_cfc, _fun_total) /* forall<e,a> (int, hnd/clause0<int,total,e,a>) -> total<e,a> */  {
  return { _cfc: _cfc, _fun_total: _fun_total };
}
// type width
export function _Hnd_width(_cfc, _val_width) /* forall<e,a> (int, hnd/clause0<int,width,e,a>) -> width<e,a> */  {
  return { _cfc: _cfc, _val_width: _val_width };
}
 
// declarations
 
 
// select `@val-width` operation out of effect `:width`
export function _val_width_fs__select(hnd) /* forall<e,a> (hnd : width<e,a>) -> hnd/clause0<int,width,e,a> */  {
  return hnd._val_width;
}
 
 
// Call the `val width` operation of the effect `:width`
export function _val_width() /* () -> width int */  {
   
  var ev_10177 = $std_core_hnd._evv_at(0);
  return ev_10177.hnd._val_width(ev_10177.marker, ev_10177);
}
 
 
// Automatically generated. Retrieves the `@val-width` constructor field of the `:width` type.
export function width_fs__val_width(width_0) /* forall<e,a> (width : width<e,a>) -> hnd/clause0<int,width,e,a> */  {
  return width_0._val_width;
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:width` type.
export function width_fs__cfc(width_0) /* forall<e,a> (width : width<e,a>) -> int */  {
  return width_0._cfc;
}
 
 
// handler for the effect `:width`
export function width_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : width<e,b>, ret : (res : a) -> e b, action : () -> <width|e> a) -> e b */  {
  return $std_core_hnd._hhandle(width_fs__tag, hnd, ret, action);
}
 
 
// Call the `val width` operation of the effect `:width`
export var width;
var width = $std_core_types._Valueop;
 
export function f() /* () -> <console/console,width> () */  {
   
  var g = width_fs__handle(_Hnd_width(1, $std_core_hnd.clause_tail0(function() {
        return 80;
      })), function(_res /* () -> width int */ ) {
      return _res;
    }, function() {
      return function() {
         
        var ev_10180 = $std_core_hnd._evv_at(0);
         
        var x_10121 = ev_10180.hnd._val_width(ev_10180.marker, ev_10180);
        return $std_core_types._int_add(x_10121,1);
      };
    });
   
  var x_0_10000 = g();
   
  $std_core_console.printsln($std_core_int.show(x_0_10000));
   
  var x_1_10002 = width_fs__handle(_Hnd_width(1, $std_core_hnd.clause_tail0(function() {
        return 80;
      })), function(_res_0 /* int */ ) {
      return _res_0;
    }, function() {
       
      var x_2_10123 = $std_core_hnd._open_at0(0, function() {
           
          var ev_0_10182 = $std_core_hnd._evv_at(0);
          return ev_0_10182.hnd._val_width(ev_0_10182.marker, ev_0_10182);
        });
      return $std_core_types._int_add(x_2_10123,1);
    });
  return $std_core_console.printsln($std_core_int.show(x_1_10002));
}
 
export function half(action) /* forall<a,e> (action : () -> <width,width|e> a) -> <width|e> a */  {
   
  var _value_width_l21_c12 = $std_core_types._int_div(($std_core_hnd._open_at0($std_core_hnd._evv_index(width_fs__tag), function() {
       
      var ev_10184 = $std_core_hnd._evv_at(0);
      return ev_10184.hnd._val_width(ev_10184.marker, ev_10184);
    })),2);
  return width_fs__handle(_Hnd_width(1, $std_core_hnd.clause_tail0(function() {
        return _value_width_l21_c12;
      })), function(_res /* 1031 */ ) {
      return _res;
    }, action);
}
 
 
// select `@val-emitx` operation out of effect `:emitx`
export function _val_emitx_fs__select(hnd) /* forall<e,a> (hnd : emitx<e,a>) -> hnd/clause0<(s : string) -> io (),emitx,e,a> */  {
  return hnd._val_emitx;
}
 
 
// Call the `val emitx` operation of the effect `:emitx`
export function _val_emitx() /* () -> emitx ((s : string) -> io ()) */  {
   
  var ev_10186 = $std_core_hnd._evv_at(0);
  return ev_10186.hnd._val_emitx(ev_10186.marker, ev_10186);
}
 
 
// Automatically generated. Retrieves the `@val-emitx` constructor field of the `:emitx` type.
export function emitx_fs__val_emitx(emitx_0) /* forall<e,a> (emitx : emitx<e,a>) -> hnd/clause0<(s : string) -> io (),emitx,e,a> */  {
  return emitx_0._val_emitx;
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:emitx` type.
export function emitx_fs__cfc(emitx_0) /* forall<e,a> (emitx : emitx<e,a>) -> int */  {
  return emitx_0._cfc;
}
 
 
// handler for the effect `:emitx`
export function emitx_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : emitx<e,b>, ret : (res : a) -> e b, action : () -> <emitx|e> a) -> e b */  {
  return $std_core_hnd._hhandle(emitx_fs__tag, hnd, ret, action);
}
 
 
// Call the `val emitx` operation of the effect `:emitx`
export var emitx;
var emitx = $std_core_types._Valueop;
 
export function emit1(action) /* forall<a,e> (action : () -> <emitx|e> a) -> e a */  {
  return emitx_fs__handle(_Hnd_emitx(1, $std_core_hnd.clause_tail0(function() {
        return $std_core_console.printsln;
      })), function(_res /* 1348 */ ) {
      return _res;
    }, action);
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:emit` type.
export function emit_fs__cfc(emit_0) /* forall<e,a> (emit : emit<e,a>) -> int */  {
  return emit_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@fun-emit` constructor field of the `:emit` type.
export function emit_fs__fun_emit(emit_0) /* forall<e,a> (emit : emit<e,a>) -> hnd/clause1<string,(),emit,e,a> */  {
  return emit_0._fun_emit;
}
 
 
// handler for the effect `:emit`
export function emit_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : emit<e,b>, ret : (res : a) -> e b, action : () -> <emit|e> a) -> e b */  {
  return $std_core_hnd._hhandle(emit_fs__tag, hnd, ret, action);
}
 
 
// select `emit` operation out of effect `:emit`
export function emit_fs__select(hnd) /* forall<e,a> (hnd : emit<e,a>) -> hnd/clause1<string,(),emit,e,a> */  {
  return hnd._fun_emit;
}
 
 
// Call the `fun emit` operation of the effect `:emit`
export function emit(s) /* (s : string) -> emit () */  {
   
  var ev_10190 = $std_core_hnd._evv_at(0);
  return ev_10190.hnd._fun_emit(ev_10190.marker, ev_10190, s);
}
 
 
// monadic lift
export function _mlift_pretty_emit1_10153(action, wild__) /* forall<a,e> (action : () -> <console/console,emit,exn|e> a, wild_ : ()) -> <emit,console/console,exn|e> a */  {
  return action();
}
 
export function pretty_emit1(action) /* forall<a,e> (action : () -> <console/console,emit,exn|e> a) -> <console/console|e> error<a> */  {
  return $std_core_exn.$try(function() {
    return emit_fs__handle(_Hnd_emit(1, $std_core_hnd.clause_tail1($std_core_console.printsln)), function(_res /* 1684 */ ) {
        return _res;
      }, function() {
         
        var x_10193 = $std_core_hnd._open_at1($std_core_hnd._evv_index(emit_fs__tag), function(s_1 /* string */ ) {
             
            var ev_10195 = $std_core_hnd._evv_at(0);
            return ev_10195.hnd._fun_emit(ev_10195.marker, ev_10195, s_1);
          }, "hi");
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
            return action();
          });
        }
        else {
          return action();
        }
      });
  });
}
 
 
// dynamic binding with local reasoning
export function pretty_emit2(action) /* forall<a,e> (action : () -> <console/console,emit,width|e> a) -> <console/console,width|e> a */  {
  return emit_fs__handle(_Hnd_emit(1, $std_core_hnd.clause_tail1(function(s /* string */ ) {
        return $std_core_console.printsln($std_core_sslice.string_fs_truncate(s, $std_core_hnd._open_at0($std_core_hnd._evv_index(width_fs__tag), function() {
               
              var ev_10200 = $std_core_hnd._evv_at(0);
              return ev_10200.hnd._val_width(ev_10200.marker, ev_10200);
            })));
      })), function(_res /* 1812 */ ) {
      return _res;
    }, action);
}
 
 
// monadic lift
export function _mlift_pretty_emit_10154(out, s, _y_x10032) /* forall<h,e> (out : local-var<h,string>, s : string, string) -> <local<h>|e> () */  {
  return ((out).value = ($std_core_types._lp__plus__plus__rp_(_y_x10032, $std_core_types._lp__plus__plus__rp_(s, "\n"))));
}
 
 
// monadic lift
export function _mlift_pretty_emit_10155(out, wild__) /* forall<h,a,e> (out : local-var<h,string>, wild_ : a) -> <local<h>|e> string */  {
  return ((out).value);
}
 
 
// with lexically scoped state
export function pretty_emit(action) /* forall<a,e> (action : () -> <emit|e> a) -> e string */  {
  return function() {
     
    var loc = { value: ("") };
     
    var x_10204 = emit_fs__handle(_Hnd_emit(1, $std_core_hnd.clause_tail1(function(s /* string */ ) {
           
          var x_0_10206 = ((loc).value);
           
          function next_0_10207(_y_x10032) /* (string) -> <local<1996>|1998> () */  {
            return ((loc).value = ($std_core_types._lp__plus__plus__rp_(_y_x10032, $std_core_types._lp__plus__plus__rp_(s, "\n"))));
          }
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(next_0_10207);
          }
          else {
            return next_0_10207(x_0_10206);
          }
        })), function(_res /* 1997 */ ) {
        return _res;
      }, action);
     
    if ($std_core_hnd._yielding()) {
      var res = $std_core_hnd.yield_extend(function(wild__ /* 1997 */ ) {
        return ((loc).value);
      });
    }
    else {
      var res = ((loc).value);
    }
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:state` type.
export function state_fs__cfc(state) /* forall<a,e,b> (state : state<a,e,b>) -> int */  {
  return state._cfc;
}
 
 
// handler for the effect `:state`
export function state_fs__handle(hnd, ret, action) /* forall<a,b,e,c> (hnd : state<a,e,c>, ret : (res : b) -> e c, action : () -> <state<a>|e> b) -> e c */  {
  return $std_core_hnd._hhandle(state_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@ctl-get` constructor field of the `:state` type.
export function state_fs__ctl_get(state) /* forall<a,e,b> (state : state<a,e,b>) -> hnd/clause0<a,state<a>,e,b> */  {
  return state._ctl_get;
}
 
 
// select `get` operation out of effect `:state`
export function get_fs__select(hnd) /* forall<a,e,b> (hnd : state<a,e,b>) -> hnd/clause0<a,state<a>,e,b> */  {
  return hnd._ctl_get;
}
 
 
// Call the `ctl get` operation of the effect `:state`
export function get() /* forall<a> () -> (state<a>) a */  {
   
  var ev_10215 = $std_core_hnd._evv_at(0);
  return ev_10215.hnd._ctl_get(ev_10215.marker, ev_10215);
}
 
 
// Automatically generated. Retrieves the `@ctl-set` constructor field of the `:state` type.
export function state_fs__ctl_set(state) /* forall<a,e,b> (state : state<a,e,b>) -> hnd/clause1<a,(),state<a>,e,b> */  {
  return state._ctl_set;
}
 
 
// select `set` operation out of effect `:state`
export function set_fs__select(hnd) /* forall<a,e,b> (hnd : state<a,e,b>) -> hnd/clause1<a,(),state<a>,e,b> */  {
  return hnd._ctl_set;
}
 
 
// Call the `ctl set` operation of the effect `:state`
export function set(x) /* forall<a> (x : a) -> (state<a>) () */  {
   
  var ev_10217 = $std_core_hnd._evv_at(0);
  return ev_10217.hnd._ctl_set(ev_10217.marker, ev_10217, x);
}
 
export function state1(action) /* forall<a,e> (action : () -> <state<int>|e> a) -> e a */  {
  return function() {
     
    var loc = { value: 0 };
     
    var res = state_fs__handle(_Hnd_state(1, $std_core_hnd.clause_tail0(function() {
          return ((loc).value);
        }), $std_core_hnd.clause_tail1(function(x /* int */ ) {
          return ((loc).value = x);
        })), function(_res /* 2574 */ ) {
        return _res;
      }, action);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:total` type.
export function total_fs__cfc(total_0) /* forall<e,a> (total : total<e,a>) -> int */  {
  return total_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@fun-total` constructor field of the `:total` type.
export function total_fs__fun_total(total_0) /* forall<e,a> (total : total<e,a>) -> hnd/clause0<int,total,e,a> */  {
  return total_0._fun_total;
}
 
 
// handler for the effect `:total`
export function total_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : total<e,b>, ret : (res : a) -> e b, action : () -> <total|e> a) -> e b */  {
  return $std_core_hnd._hhandle(total_fs__tag, hnd, ret, action);
}
 
 
// select `total` operation out of effect `:total`
export function total_fs__select(hnd) /* forall<e,a> (hnd : total<e,a>) -> hnd/clause0<int,total,e,a> */  {
  return hnd._fun_total;
}
 
 
// Call the `fun total` operation of the effect `:total`
export function total() /* () -> total int */  {
   
  var ev_10224 = $std_core_hnd._evv_at(0);
  return ev_10224.hnd._fun_total(ev_10224.marker, ev_10224);
}
 
 
// monadic lift
export function _mlift_emit4_10156(out, s, _y_x10050) /* forall<h,e> (out : local-var<h,string>, s : string, string) -> <local<h>|e> () */  {
  return ((out).value = ($std_core_types._lp__plus__plus__rp_(_y_x10050, $std_core_types._lp__plus__plus__rp_(s, "\n"))));
}
 
 
// monadic lift
export function _mlift_emit4_10157(out, x_5712) /* forall<h,a,e> (out : local-var<h,string>, x@5712 : a) -> <local<h>|e> string */  {
  return ((out).value);
}
 
export function emit4(action) /* forall<a,e> (action : () -> <emit,total|e> a) -> e string */  {
  return function() {
     
    var loc = { value: ("") };
     
    var x_10228 = emit_fs__handle(_Hnd_emit(1, $std_core_hnd.clause_tail1(function(s /* string */ ) {
           
          var x_0_10230 = ((loc).value);
           
          function next_0_10231(_y_x10050) /* (string) -> <local<3041>|3043> () */  {
            return ((loc).value = ($std_core_types._lp__plus__plus__rp_(_y_x10050, $std_core_types._lp__plus__plus__rp_(s, "\n"))));
          }
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(next_0_10231);
          }
          else {
            return next_0_10231(x_0_10230);
          }
        })), function(_res /* 3042 */ ) {
        return _res;
      }, function() {
        return total_fs__handle(_Hnd_total(1, $std_core_hnd.clause_tail0(function() {
               
              var x_1_10235 = ((loc).value);
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend($std_core_string.chars_fs_count);
              }
              else {
                return $std_core_string.chars_fs_count(x_1_10235);
              }
            })), function(_res_0 /* 3042 */ ) {
            return _res_0;
          }, action);
      });
     
    if ($std_core_hnd._yielding()) {
      var res = $std_core_hnd.yield_extend(function(x_5712 /* 3042 */ ) {
        return ((loc).value);
      });
    }
    else {
      var res = ((loc).value);
    }
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:abort` type.
export function abort_fs__cfc(abort_0) /* forall<e,a> (abort : abort<e,a>) -> int */  {
  return abort_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@ctl-abort` constructor field of the `:abort` type.
export function abort_fs__ctl_abort(abort_0) /* forall<e,a> (abort : abort<e,a>) -> hnd/clause0<(),abort,e,a> */  {
  return abort_0._ctl_abort;
}
 
 
// handler for the effect `:abort`
export function abort_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : abort<e,b>, ret : (res : a) -> e b, action : () -> <abort|e> a) -> e b */  {
  return $std_core_hnd._hhandle(abort_fs__tag, hnd, ret, action);
}
 
 
// select `abort` operation out of effect `:abort`
export function abort_fs__select(hnd) /* forall<e,a> (hnd : abort<e,a>) -> hnd/clause0<(),abort,e,a> */  {
  return hnd._ctl_abort;
}
 
 
// Call the `ctl abort` operation of the effect `:abort`
export function abort() /* () -> abort () */  {
   
  var ev_10241 = $std_core_hnd._evv_at(0);
  return ev_10241.hnd._ctl_abort(ev_10241.marker, ev_10241);
}
 
export function pretty_abort(action) /* forall<e> (action : () -> <abort,emit|e> ()) -> e string */  {
  return pretty_emit(function() {
    return abort_fs__handle(_Hnd_abort(3, function(m /* hnd/marker<<emit|3315>,()> */ , ___wildcard_x688__16 /* hnd/ev<abort> */ ) {
          return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<(),()>) -> <emit|3315> () */ ) {
              return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (()) -> <emit|3315> () */ ) {
                  return $std_core_types.Unit;
                }, k);
            });
        }), function(_res /* () */ ) {
        return _res;
      }, action);
  });
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:choice` type.
export function choice_fs__cfc(choice_0) /* forall<e,a> (choice : choice<e,a>) -> int */  {
  return choice_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@ctl-choice` constructor field of the `:choice` type.
export function choice_fs__ctl_choice(choice_0) /* forall<e,a> (choice : choice<e,a>) -> hnd/clause0<bool,choice,e,a> */  {
  return choice_0._ctl_choice;
}
 
 
// handler for the effect `:choice`
export function choice_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : choice<e,b>, ret : (res : a) -> e b, action : () -> <choice|e> a) -> e b */  {
  return $std_core_hnd._hhandle(choice_fs__tag, hnd, ret, action);
}
 
 
// select `choice` operation out of effect `:choice`
export function choice_fs__select(hnd) /* forall<e,a> (hnd : choice<e,a>) -> hnd/clause0<bool,choice,e,a> */  {
  return hnd._ctl_choice;
}
 
 
// Call the `ctl choice` operation of the effect `:choice`
export function choice() /* () -> choice bool */  {
   
  var ev_10245 = $std_core_hnd._evv_at(0);
  return ev_10245.hnd._ctl_choice(ev_10245.marker, ev_10245);
}
 
 
// monadic lift
export function _mlift_pretty_all_10158(_y_x10065, _y_x10066) /* forall<e> (list<string>, list<string>) -> e list<string> */  {
  return $std_core_list.append(_y_x10065, _y_x10066);
}
 
 
// monadic lift
export function _mlift_pretty_all_10159(resume, _y_x10065) /* forall<e> (resume : (bool) -> e list<string>, list<string>) -> e list<string> */  {
   
  var x_10247 = resume(false);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10066 /* list<string> */ ) {
      return $std_core_list.append(_y_x10065, _y_x10066);
    });
  }
  else {
    return $std_core_list.append(_y_x10065, x_10247);
  }
}
 
 
// monadic lift
export function _mlift_pretty_all_10160(_y_x10067) /* forall<e> (string) -> <choice|e> list<string> */  {
  return $std_core_types.Cons(_y_x10067, $std_core_types.Nil);
}
 
export function pretty_all(action) /* forall<e> (action : () -> <abort,choice,emit|e> ()) -> e list<string> */  {
  return choice_fs__handle(_Hnd_choice(3, function(m /* hnd/marker<3597,list<string>> */ , ___wildcard_x688__16 /* hnd/ev<choice> */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<bool,list<string>>) -> 3597 list<string> */ ) {
            return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (bool) -> 3597 list<string> */ ) {
                 
                var x_10252 = r(true);
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(function(_y_x10065 /* list<string> */ ) {
                    return _mlift_pretty_all_10159(r, _y_x10065);
                  });
                }
                else {
                  return _mlift_pretty_all_10159(r, x_10252);
                }
              }, k);
          });
      }), function(_res /* list<string> */ ) {
      return _res;
    }, function() {
       
      var x_0_10254 = pretty_abort(action);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10067 /* string */ ) {
          return $std_core_types.Cons(_y_x10067, $std_core_types.Nil);
        });
      }
      else {
        return $std_core_types.Cons(x_0_10254, $std_core_types.Nil);
      }
    });
}
 
 
// monadic lift
export function _mlift_pretty_all2_10161(_y_x10069, _y_x10070) /* forall<e> (list<string>, list<string>) -> e list<string> */  {
  return $std_core_list.append(_y_x10069, _y_x10070);
}
 
 
// monadic lift
export function _mlift_pretty_all2_10162(resume, _y_x10069) /* forall<e> (resume : (bool) -> e list<string>, list<string>) -> e list<string> */  {
   
  var x_10257 = resume(false);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10070 /* list<string> */ ) {
      return $std_core_list.append(_y_x10069, _y_x10070);
    });
  }
  else {
    return $std_core_list.append(_y_x10069, x_10257);
  }
}
 
 
// monadic lift
export function _mlift_pretty_all2_10163(x_5717) /* forall<e> (x@5717 : string) -> <choice|e> list<string> */  {
  return $std_core_types.Cons(x_5717, $std_core_types.Nil);
}
 
export function pretty_all2(action) /* forall<e> (action : () -> <abort,choice,emit|e> ()) -> e list<string> */  {
  return choice_fs__handle(_Hnd_choice(3, function(m /* hnd/marker<3707,list<string>> */ , ___wildcard_x688__16 /* hnd/ev<choice> */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<bool,list<string>>) -> 3707 list<string> */ ) {
            return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (bool) -> 3707 list<string> */ ) {
                 
                var x_10262 = r(true);
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(function(_y_x10069 /* list<string> */ ) {
                    return _mlift_pretty_all2_10162(r, _y_x10069);
                  });
                }
                else {
                  return _mlift_pretty_all2_10162(r, x_10262);
                }
              }, k);
          });
      }), function(_res /* list<string> */ ) {
      return _res;
    }, function() {
       
      var x_0_10264 = pretty_abort(action);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(x_5717 /* string */ ) {
          return $std_core_types.Cons(x_5717, $std_core_types.Nil);
        });
      }
      else {
        return $std_core_types.Cons(x_0_10264, $std_core_types.Nil);
      }
    });
}
 
 
// monadic lift
export function _mlift_state2_10164(s, _y_x10073) /* forall<a,b,e> (s : a, (a) -> e b) -> e b */  {
  return _y_x10073(s);
}
 
 
// monadic lift
export function _mlift_state2_10165(x, _y_x10075) /* forall<a,b,e> (x : a, (a) -> e b) -> e b */  {
  return _y_x10075(x);
}
 
 
// monadic lift
export function _mlift_state2_10166(x_0) /* forall<a,b,e> (x@0 : b) -> <state<a>|e> ((s@1 : a) -> e b) */  {
  return function(s_1 /* 3858 */ ) {
    return x_0;
  };
}
 
 
// monadic lift
export function _mlift_state2_10167(init, f_5720) /* forall<a,b,e> (init : a, f@5720 : (a) -> e b) -> e b */  {
  return f_5720(init);
}
 
export function state2(init, action) /* forall<a,b,e> (init : a, action : () -> <state<a>|e> b) -> e b */  {
   
  var x_10267 = state_fs__handle(_Hnd_state(3, function(m /* hnd/marker<3860,(s : 3858) -> 3860 3859> */ , ___wildcard_x688__16 /* hnd/ev<state<3858>> */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<3858,(s : 3858) -> 3860 3859>) -> 3860 ((s : 3858) -> 3860 3859) */ ) {
            return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (3858) -> 3860 ((s : 3858) -> 3860 3859) */ ) {
                return function(s /* 3858 */ ) {
                   
                  var x_0_10271 = r(s);
                  if ($std_core_hnd._yielding()) {
                    return $std_core_hnd.yield_extend(function(_y_x10073 /* (3858) -> 3860 3859 */ ) {
                      return _y_x10073(s);
                    });
                  }
                  else {
                    return x_0_10271(s);
                  }
                };
              }, k);
          });
      }, function(m_0 /* hnd/marker<3860,(s : 3858) -> 3860 3859> */ , ___wildcard_x639__16 /* hnd/ev<state<3858>> */ , x_1 /* 3858 */ ) {
        return $std_core_hnd.yield_to(m_0, function(k_0 /* (hnd/resume-result<(),(s : 3858) -> 3860 3859>) -> 3860 ((s : 3858) -> 3860 3859) */ ) {
            return $std_core_hnd.protect(x_1, function(x_2 /* 3858 */ , resume_0 /* (()) -> 3860 ((3858) -> 3860 3859) */ ) {
                return function(s_0_0 /* 3858 */ ) {
                   
                  var x_3_10276 = resume_0($std_core_types.Unit);
                  if ($std_core_hnd._yielding()) {
                    return $std_core_hnd.yield_extend(function(_y_x10075 /* (3858) -> 3860 3859 */ ) {
                      return _y_x10075(x_2);
                    });
                  }
                  else {
                    return x_3_10276(x_2);
                  }
                };
              }, k_0);
          });
      }), function(_res /* (s : 3858) -> 3860 3859 */ ) {
      return _res;
    }, function() {
       
      var x_5_10280 = action();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(x_0_0 /* 3859 */ ) {
          return function(s_1 /* 3858 */ ) {
            return x_0_0;
          };
        });
      }
      else {
        return function(s_1_0 /* 3858 */ ) {
          return x_5_10280;
        };
      }
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(f_5720 /* (3858) -> 3860 3859 */ ) {
      return f_5720(init);
    });
  }
  else {
    return x_10267(init);
  }
}
 
 
// select `@val-cwd` operation out of effect `:cwd`
export function _val_cwd_fs__select(hnd) /* forall<e,a> (hnd : cwd<e,a>) -> hnd/clause0<string,cwd,e,a> */  {
  return hnd._val_cwd;
}
 
 
// Call the `val cwd` operation of the effect `:cwd`
export function _val_cwd() /* () -> cwd string */  {
   
  var ev_10285 = $std_core_hnd._evv_at(0);
  return ev_10285.hnd._val_cwd(ev_10285.marker, ev_10285);
}
 
 
// Automatically generated. Retrieves the `@val-cwd` constructor field of the `:cwd` type.
export function cwd_fs__val_cwd(cwd_0) /* forall<e,a> (cwd : cwd<e,a>) -> hnd/clause0<string,cwd,e,a> */  {
  return cwd_0._val_cwd;
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:cwd` type.
export function cwd_fs__cfc(cwd_0) /* forall<e,a> (cwd : cwd<e,a>) -> int */  {
  return cwd_0._cfc;
}
 
 
// handler for the effect `:cwd`
export function cwd_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : cwd<e,b>, ret : (res : a) -> e b, action : () -> <cwd|e> a) -> e b */  {
  return $std_core_hnd._hhandle(cwd_fs__tag, hnd, ret, action);
}
 
 
// Call the `val cwd` operation of the effect `:cwd`
export var cwd;
var cwd = $std_core_types._Valueop;
 
export function is_home() /* () -> cwd bool */  {
   
  var ev_10288 = $std_core_hnd._evv_at(0);
  var _x0 = ev_10288.hnd._val_cwd(ev_10288.marker, ev_10288);
  return (_x0 === ("/home"));
}
 
export function example1() /* () -> bool */  {
  return cwd_fs__handle(_Hnd_cwd(1, function(___wildcard_x695__14 /* hnd/marker<total,bool> */ , ___wildcard_x695__17 /* hnd/ev<cwd> */ ) {
        return "/";
      }), function(_res /* bool */ ) {
      return _res;
    }, function() {
       
      var ev_10291 = $std_core_hnd._evv_at(0);
      var _x1 = ev_10291.hnd._val_cwd(ev_10291.marker, ev_10291);
      return (_x1 === ("/home"));
    });
}
 
export function gchildren(g, v) /* (g : graph, v : vertex) -> list<vertex> */  {
  return $std_core_types.Nil;
}
 
export function bound(g) /* (g : graph) -> int */  {
  return 10;
}
 
export function graph_fs__copy(_this) /* (graph) -> graph */  {
  return Graph;
}
 
 
// Automatically generated. Retrieves the `sub` constructor field of the `:rose` type.
export function rose_fs_sub(rose) /* (rose : rose) -> list<rose> */  {
  return rose.sub;
}
 
 
// Automatically generated. Retrieves the `v` constructor field of the `:rose` type.
export function rose_fs_v(rose) /* (rose : rose) -> vertex */  {
  return rose.v;
}
 
export function rose_fs__copy(_this, v, sub) /* (rose, v : ? vertex, sub : ? (list<rose>)) -> rose */  {
  if (v !== undefined) {
    var _x2 = v;
  }
  else {
    var _x2 = _this.v;
  }
  if (sub !== undefined) {
    var _x3 = sub;
  }
  else {
    var _x3 = _this.sub;
  }
  return Rose(_x2, _x3);
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:marked` type.
export function marked_fs__cfc(marked_0) /* forall<e,a> (marked : marked<e,a>) -> int */  {
  return marked_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@fun-marked` constructor field of the `:marked` type.
export function marked_fs__fun_marked(marked_0) /* forall<e,a> (marked : marked<e,a>) -> hnd/clause1<vertex,bool,marked,e,a> */  {
  return marked_0._fun_marked;
}
 
 
// handler for the effect `:marked`
export function marked_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : marked<e,b>, ret : (res : a) -> e b, action : () -> <marked|e> a) -> e b */  {
  return $std_core_hnd._hhandle(marked_fs__tag, hnd, ret, action);
}
 
 
// select `marked` operation out of effect `:marked`
export function marked_fs__select(hnd) /* forall<e,a> (hnd : marked<e,a>) -> hnd/clause1<vertex,bool,marked,e,a> */  {
  return hnd._fun_marked;
}
 
 
// Call the `fun marked` operation of the effect `:marked`
export function marked(v) /* (v : vertex) -> marked bool */  {
   
  var ev_10294 = $std_core_hnd._evv_at(0);
  return ev_10294.hnd._fun_marked(ev_10294.marker, ev_10294, v);
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:mark` type.
export function mark_fs__cfc(mark_0) /* forall<e,a> (mark : mark<e,a>) -> int */  {
  return mark_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@fun-mark` constructor field of the `:mark` type.
export function mark_fs__fun_mark(mark_0) /* forall<e,a> (mark : mark<e,a>) -> hnd/clause1<vertex,(),mark,e,a> */  {
  return mark_0._fun_mark;
}
 
 
// handler for the effect `:mark`
export function mark_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : mark<e,b>, ret : (res : a) -> e b, action : () -> <mark|e> a) -> e b */  {
  return $std_core_hnd._hhandle(mark_fs__tag, hnd, ret, action);
}
 
 
// select `mark` operation out of effect `:mark`
export function mark_fs__select(hnd) /* forall<e,a> (hnd : mark<e,a>) -> hnd/clause1<vertex,(),mark,e,a> */  {
  return hnd._fun_mark;
}
 
 
// Call the `fun mark` operation of the effect `:mark`
export function mark(v) /* (v : vertex) -> mark () */  {
   
  var ev_10298 = $std_core_hnd._evv_at(0);
  return ev_10298.hnd._fun_mark(ev_10298.marker, ev_10298, v);
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:children` type.
export function children_fs__cfc(children_0) /* forall<e,a> (children : children<e,a>) -> int */  {
  return children_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@fun-children` constructor field of the `:children` type.
export function children_fs__fun_children(children_0) /* forall<e,a> (children : children<e,a>) -> hnd/clause1<vertex,list<vertex>,children,e,a> */  {
  return children_0._fun_children;
}
 
 
// handler for the effect `:children`
export function children_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : children<e,b>, ret : (res : a) -> e b, action : () -> <children|e> a) -> e b */  {
  return $std_core_hnd._hhandle(children_fs__tag, hnd, ret, action);
}
 
 
// select `children` operation out of effect `:children`
export function children_fs__select(hnd) /* forall<e,a> (hnd : children<e,a>) -> hnd/clause1<vertex,list<vertex>,children,e,a> */  {
  return hnd._fun_children;
}
 
 
// Call the `fun children` operation of the effect `:children`
export function children(v) /* (v : vertex) -> children list<vertex> */  {
   
  var ev_10302 = $std_core_hnd._evv_at(0);
  return ev_10302.hnd._fun_children(ev_10302.marker, ev_10302, v);
}
 
 
// monadic lift
export function _mlift_trmc_dfs_loop_10168(_acc, v, vv, sub) /* (ctx<list<rose>>, v : vertex, vv : list<vertex>, sub : list<rose>) -> <children,div,mark,marked> list<rose> */  {
   
  var _trmc_x10016 = undefined;
   
  var _trmc_x10017 = $std_core_types.Cons(Rose(v, sub), _trmc_x10016);
  return _trmc_dfs_loop(vv, $std_core_types._cctx_extend(_acc,_trmc_x10017,({obj: _trmc_x10017, field_name: "tail"})));
}
 
 
// monadic lift
export function _mlift_trmc_dfs_loop_10169(_acc_0, v_0, vv_0, _y_x10093) /* (ctx<list<rose>>, v : vertex, vv : list<vertex>, list<vertex>) -> <children,mark,marked,div> list<rose> */  {
   
  var x_10305 = dfs_loop(_y_x10093);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(sub_0 /* list<rose> */ ) {
      return _mlift_trmc_dfs_loop_10168(_acc_0, v_0, vv_0, sub_0);
    });
  }
  else {
    return _mlift_trmc_dfs_loop_10168(_acc_0, v_0, vv_0, x_10305);
  }
}
 
 
// monadic lift
export function _mlift_trmc_dfs_loop_10170(_acc_1, v_1, vv_1, wild__) /* (ctx<list<rose>>, v : vertex, vv : list<vertex>, wild_ : ()) -> <mark,children,marked,div> list<rose> */  {
   
  var x_0_10307 = $std_core_hnd._open_at1(0, function(v_2 /* vertex */ ) {
       
      var ev_10309 = $std_core_hnd._evv_at(0);
      return ev_10309.hnd._fun_children(ev_10309.marker, ev_10309, v_2);
    }, v_1);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10093_0 /* list<vertex> */ ) {
      return _mlift_trmc_dfs_loop_10169(_acc_1, v_1, vv_1, _y_x10093_0);
    });
  }
  else {
    return _mlift_trmc_dfs_loop_10169(_acc_1, v_1, vv_1, x_0_10307);
  }
}
 
 
// monadic lift
export function _mlift_trmc_dfs_loop_10171(_acc_2, v_3, vv_2, _y_x10088) /* (ctx<list<rose>>, v : vertex, vv : list<vertex>, bool) -> <marked,children,mark,div> list<rose> */  {
  if (_y_x10088) {
    return _trmc_dfs_loop(vv_2, _acc_2);
  }
  else {
     
    var x_2_10312 = $std_core_hnd._open_at1(1, function(v_1_0 /* vertex */ ) {
         
        var ev_0_10314 = $std_core_hnd._evv_at(0);
        return ev_0_10314.hnd._fun_mark(ev_0_10314.marker, ev_0_10314, v_1_0);
      }, v_3);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___0 /* () */ ) {
        return _mlift_trmc_dfs_loop_10170(_acc_2, v_3, vv_2, wild___0);
      });
    }
    else {
      return _mlift_trmc_dfs_loop_10170(_acc_2, v_3, vv_2, x_2_10312);
    }
  }
}
 
 
// monadic lift
export function _mlift_trmcm_dfs_loop_10172(_accm, v_4, vv_3, sub_1) /* ((list<rose>) -> list<rose>, v : vertex, vv : list<vertex>, sub : list<rose>) -> <children,div,mark,marked> list<rose> */  {
  return _trmcm_dfs_loop(vv_3, function(_trmc_x10019 /* list<rose> */ ) {
      return _accm($std_core_types.Cons(Rose(v_4, sub_1), _trmc_x10019));
    });
}
 
 
// monadic lift
export function _mlift_trmcm_dfs_loop_10173(_accm_0, v_5, vv_4, _y_x10104) /* ((list<rose>) -> list<rose>, v : vertex, vv : list<vertex>, list<vertex>) -> <children,mark,marked,div> list<rose> */  {
   
  var x_4_10317 = dfs_loop(_y_x10104);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(sub_2 /* list<rose> */ ) {
      return _mlift_trmcm_dfs_loop_10172(_accm_0, v_5, vv_4, sub_2);
    });
  }
  else {
    return _mlift_trmcm_dfs_loop_10172(_accm_0, v_5, vv_4, x_4_10317);
  }
}
 
 
// monadic lift
export function _mlift_trmcm_dfs_loop_10174(_accm_1, v_6, vv_5, wild___1) /* ((list<rose>) -> list<rose>, v : vertex, vv : list<vertex>, wild_ : ()) -> <mark,children,marked,div> list<rose> */  {
   
  var x_5_10319 = $std_core_hnd._open_at1(0, function(v_2_0 /* vertex */ ) {
       
      var ev_1_10321 = $std_core_hnd._evv_at(0);
      return ev_1_10321.hnd._fun_children(ev_1_10321.marker, ev_1_10321, v_2_0);
    }, v_6);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10104_0 /* list<vertex> */ ) {
      return _mlift_trmcm_dfs_loop_10173(_accm_1, v_6, vv_5, _y_x10104_0);
    });
  }
  else {
    return _mlift_trmcm_dfs_loop_10173(_accm_1, v_6, vv_5, x_5_10319);
  }
}
 
 
// monadic lift
export function _mlift_trmcm_dfs_loop_10175(_accm_2, v_7, vv_6, _y_x10099) /* ((list<rose>) -> list<rose>, v : vertex, vv : list<vertex>, bool) -> <marked,children,mark,div> list<rose> */  {
  if (_y_x10099) {
    return _trmcm_dfs_loop(vv_6, _accm_2);
  }
  else {
     
    var x_7_10324 = $std_core_hnd._open_at1(1, function(v_1_1 /* vertex */ ) {
         
        var ev_2_10326 = $std_core_hnd._evv_at(0);
        return ev_2_10326.hnd._fun_mark(ev_2_10326.marker, ev_2_10326, v_1_1);
      }, v_7);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___2 /* () */ ) {
        return _mlift_trmcm_dfs_loop_10174(_accm_2, v_7, vv_6, wild___2);
      });
    }
    else {
      return _mlift_trmcm_dfs_loop_10174(_accm_2, v_7, vv_6, x_7_10324);
    }
  }
}
 
export function _trmc_dfs_loop(vs, _acc_3) /* (vs : list<vertex>, ctx<list<rose>>) -> <children,div,mark,marked> list<rose> */  { tailcall: while(1)
{
  if (vs === null) {
    return $std_core_types._cctx_apply(_acc_3,($std_core_types.Nil));
  }
  else {
     
    var x_9_10329 = $std_core_hnd._open_at1(2, function(v_0_0 /* vertex */ ) {
         
        var ev_3_10332 = $std_core_hnd._evv_at(0);
        return ev_3_10332.hnd._fun_marked(ev_3_10332.marker, ev_3_10332, v_0_0);
      }, vs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10088_0 /* bool */ ) {
        return _mlift_trmc_dfs_loop_10171(_acc_3, vs.head, vs.tail, _y_x10088_0);
      });
    }
    else {
      if (x_9_10329) {
        {
          // tail call
          vs = vs.tail;
          continue tailcall;
        }
      }
      else {
         
        var x_11_10335 = $std_core_hnd._open_at1(1, function(v_1_2 /* vertex */ ) {
             
            var ev_4_10338 = $std_core_hnd._evv_at(0);
            return ev_4_10338.hnd._fun_mark(ev_4_10338.marker, ev_4_10338, v_1_2);
          }, vs.head);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(wild___3 /* () */ ) {
            return _mlift_trmc_dfs_loop_10170(_acc_3, vs.head, vs.tail, wild___3);
          });
        }
        else {
           
          var x_13_10341 = $std_core_hnd._open_at1(0, function(v_2_1 /* vertex */ ) {
               
              var ev_5_10344 = $std_core_hnd._evv_at(0);
              return ev_5_10344.hnd._fun_children(ev_5_10344.marker, ev_5_10344, v_2_1);
            }, vs.head);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10093_1 /* list<vertex> */ ) {
              return _mlift_trmc_dfs_loop_10169(_acc_3, vs.head, vs.tail, _y_x10093_1);
            });
          }
          else {
             
            var x_15_10347 = dfs_loop(x_13_10341);
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(function(sub_3 /* list<rose> */ ) {
                return _mlift_trmc_dfs_loop_10168(_acc_3, vs.head, vs.tail, sub_3);
              });
            }
            else {
               
              var _trmc_x10016_0 = undefined;
               
              var _trmc_x10017_0 = $std_core_types.Cons(Rose(vs.head, x_15_10347), _trmc_x10016_0);
              {
                // tail call
                var _x4 = $std_core_types._cctx_extend(_acc_3,_trmc_x10017_0,({obj: _trmc_x10017_0, field_name: "tail"}));
                vs = vs.tail;
                _acc_3 = _x4;
                continue tailcall;
              }
            }
          }
        }
      }
    }
  }
}}
 
export function _trmcm_dfs_loop(vs_0, _accm_3) /* (vs : list<vertex>, (list<rose>) -> list<rose>) -> <children,div,mark,marked> list<rose> */  { tailcall: while(1)
{
  if (vs_0 === null) {
    return _accm_3($std_core_types.Nil);
  }
  else {
     
    var x_16_10350 = $std_core_hnd._open_at1(2, function(v_0_1 /* vertex */ ) {
         
        var ev_6_10353 = $std_core_hnd._evv_at(0);
        return ev_6_10353.hnd._fun_marked(ev_6_10353.marker, ev_6_10353, v_0_1);
      }, vs_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10099_0 /* bool */ ) {
        return _mlift_trmcm_dfs_loop_10175(_accm_3, vs_0.head, vs_0.tail, _y_x10099_0);
      });
    }
    else {
      if (x_16_10350) {
        {
          // tail call
          vs_0 = vs_0.tail;
          continue tailcall;
        }
      }
      else {
         
        var x_18_10356 = $std_core_hnd._open_at1(1, function(v_1_3 /* vertex */ ) {
             
            var ev_7_10359 = $std_core_hnd._evv_at(0);
            return ev_7_10359.hnd._fun_mark(ev_7_10359.marker, ev_7_10359, v_1_3);
          }, vs_0.head);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(wild___4 /* () */ ) {
            return _mlift_trmcm_dfs_loop_10174(_accm_3, vs_0.head, vs_0.tail, wild___4);
          });
        }
        else {
           
          var x_20_10362 = $std_core_hnd._open_at1(0, function(v_2_2 /* vertex */ ) {
               
              var ev_8_10365 = $std_core_hnd._evv_at(0);
              return ev_8_10365.hnd._fun_children(ev_8_10365.marker, ev_8_10365, v_2_2);
            }, vs_0.head);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10104_1 /* list<vertex> */ ) {
              return _mlift_trmcm_dfs_loop_10173(_accm_3, vs_0.head, vs_0.tail, _y_x10104_1);
            });
          }
          else {
             
            var x_22_10368 = dfs_loop(x_20_10362);
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(function(sub_5 /* list<rose> */ ) {
                return _mlift_trmcm_dfs_loop_10172(_accm_3, vs_0.head, vs_0.tail, sub_5);
              });
            }
            else {
              {
                // tail call
                var _x8 = function(__at_accm_35 /* (list<rose>) -> list<rose> */ , _v_96 /* vertex */ , _x_22_103687 /* list<rose> */ ) {
                  return function(_trmc_x10019_0 /* list<rose> */ ) {
                    return __at_accm_35($std_core_types.Cons(Rose(_v_96, _x_22_103687), _trmc_x10019_0));
                  };
                }(_accm_3, vs_0.head, x_22_10368);
                vs_0 = vs_0.tail;
                _accm_3 = _x8;
                continue tailcall;
              }
            }
          }
        }
      }
    }
  }
}}
 
export function dfs_loop(vs_1) /* (vs : list<vertex>) -> <children,div,mark,marked> list<rose> */  {
  var _x9 = $std_core_hnd._evv_is_affine();
  if (_x9) {
    return _trmc_dfs_loop(vs_1, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_dfs_loop(vs_1, function(_trmc_x10018 /* list<rose> */ ) {
        return _trmc_x10018;
      });
  }
}
 
 
// monadic lift
export function _mlift_dfs_10176(v_0_0, _y_x10112) /* forall<h> (v@0@0 : vertex, vector<bool>) -> <local<h>,exn,children,div> bool */  {
  return $std_core_hnd._open_at2(1, $std_core_vector._index, _y_x10112, v_0_0);
}
 
export function dfs(g, vs) /* (g : graph, vs : list<vertex>) -> pure list<rose> */  {
  return function() {
     
    var init_10371 = $std_core_vector.vector_alloc(10, false);
     
    var loc = { value: init_10371 };
     
    var res = children_fs__handle(_Hnd_children(1, $std_core_hnd.clause_tail1(function(v /* vertex */ ) {
          return $std_core_types.Nil;
        })), function(_res /* list<rose> */ ) {
        return _res;
      }, function() {
        return marked_fs__handle(_Hnd_marked(1, $std_core_hnd.clause_tail1(function(v_0_0 /* vertex */ ) {
               
              var x_10373 = ((loc).value);
               
              function next_10374(_y_x10112) /* (vector<bool>) -> <local<5256>,exn,children,div> bool */  {
                return $std_core_hnd._open_at2(1, $std_core_vector._index, _y_x10112, v_0_0);
              }
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(next_10374);
              }
              else {
                return next_10374(x_10373);
              }
            })), function(_res_0 /* list<rose> */ ) {
            return _res_0;
          }, function() {
            return mark_fs__handle(_Hnd_mark(1, $std_core_hnd.clause_tail1(function(v_1 /* vertex */ ) {
                  return (loc).value[v_1] = true;
                })), function(_res_1 /* list<rose> */ ) {
                return _res_1;
              }, function() {
                return $std_core_hnd._open1($std_core_vector.unvlist($std_core_types.Cons(0, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Nil)))), dfs_loop, vs);
              });
          });
      });
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
export function main() /* () -> console/console () */  {
   
  width_fs__handle(_Hnd_width(1, function(___wildcard_x695__14 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17 /* hnd/ev<width> */ ) {
        return 78;
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var x_10010 = f();
      return $std_core_console.printsln($std_core_tuple.unit_fs_show(x_10010));
    });
   
  width_fs__handle(_Hnd_width(1, function(___wildcard_x695__14_0 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17_0 /* hnd/ev<width> */ ) {
        return 78;
      }), function(_res_0 /* () */ ) {
      return _res_0;
    }, function() {
       
      var x_0_10012 = half(function() {
        return $std_core_hnd._open_at0(0, f);
      });
      return $std_core_console.printsln($std_core_tuple.unit_fs_show(x_0_10012));
    });
  return cwd_fs__handle(_Hnd_cwd(1, function(___wildcard_x695__14_1 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17_1 /* hnd/ev<cwd> */ ) {
        return "/";
      }), function(_res_1 /* () */ ) {
      return _res_1;
    }, function() {
       
      var ev_10380 = $std_core_hnd._evv_at(0);
       
      var _x10 = ev_10380.hnd._val_cwd(ev_10380.marker, ev_10380);
      var x_1_10014 = (_x10 === ("/home"));
      return $std_core_console.printsln($std_core_bool.show(x_1_10014));
    });
}
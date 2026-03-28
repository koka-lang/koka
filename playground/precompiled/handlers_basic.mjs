// Koka generated module: handlers/basic, koka version: 3.2.4
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
import * as $std_num_random from './std_num_random.mjs';
import * as $std_num_int32 from './std_num_int32.mjs';
 
// externals
 
// type declarations
 
 
// runtime tag for the effect `:ask`
export var ask_fs__tag;
var ask_fs__tag = "ask@basic";
 
 
// runtime tag for the effect `:raise`
export var raise_fs__tag;
var raise_fs__tag = "raise@basic";
 
 
// runtime tag for the effect `:state`
export var state_fs__tag;
var state_fs__tag = "state@basic";
 
 
// runtime tag for the effect `:yield`
export var yield_fs__tag;
var yield_fs__tag = "yield@basic";
 
 
// runtime tag for the effect `:flip`
export var flip_fs__tag;
var flip_fs__tag = "flip@basic";
// type ask
export function _Hnd_ask(_cfc, _fun_ask) /* forall<e,a> (int, hnd/clause0<string,ask,e,a>) -> ask<e,a> */  {
  return { _cfc: _cfc, _fun_ask: _fun_ask };
}
// type flip
export function _Hnd_flip(_cfc, _ctl_flip) /* forall<e,a> (int, hnd/clause0<bool,flip,e,a>) -> flip<e,a> */  {
  return { _cfc: _cfc, _ctl_flip: _ctl_flip };
}
// type raise
export function _Hnd_raise(_cfc, _ctl_raise) /* forall<e,a> (int, forall<b> hnd/clause1<string,b,raise,e,a>) -> raise<e,a> */  {
  return { _cfc: _cfc, _ctl_raise: _ctl_raise };
}
// type state
export function _Hnd_state(_cfc, _fun_get, _fun_put) /* forall<a,e,b> (int, hnd/clause0<a,state<a>,e,b>, hnd/clause1<a,(),state<a>,e,b>) -> state<a,e,b> */  {
  return { _cfc: _cfc, _fun_get: _fun_get, _fun_put: _fun_put };
}
// type yield
export function _Hnd_yield(_cfc, _ctl_yield) /* forall<a,e,b> (int, hnd/clause1<a,(),yield<a>,e,b>) -> yield<a,e,b> */  {
  return { _cfc: _cfc, _ctl_yield: _ctl_yield };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:ask` type.
export function ask_fs__cfc(ask_0) /* forall<e,a> (ask : ask<e,a>) -> int */  {
  return ask_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@fun-ask` constructor field of the `:ask` type.
export function ask_fs__fun_ask(ask_0) /* forall<e,a> (ask : ask<e,a>) -> hnd/clause0<string,ask,e,a> */  {
  return ask_0._fun_ask;
}
 
 
// handler for the effect `:ask`
export function ask_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : ask<e,b>, ret : (res : a) -> e b, action : () -> <ask|e> a) -> e b */  {
  return $std_core_hnd._hhandle(ask_fs__tag, hnd, ret, action);
}
 
 
// select `ask` operation out of effect `:ask`
export function ask_fs__select(hnd) /* forall<e,a> (hnd : ask<e,a>) -> hnd/clause0<string,ask,e,a> */  {
  return hnd._fun_ask;
}
 
 
// Call the `fun ask` operation of the effect `:ask`
export function ask() /* () -> ask string */  {
   
  var ev_10131 = $std_core_hnd._evv_at(0);
  return ev_10131.hnd._fun_ask(ev_10131.marker, ev_10131);
}
 
 
// monadic lift
export function _mlift_hello_10113(name, _y_x10031) /* (name : string, string) -> ask () */  {
  return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_("Hello ", $std_core_types._lp__plus__plus__rp_(name, $std_core_types._lp__plus__plus__rp_(", ", _y_x10031))));
}
 
 
// monadic lift
export function _mlift_hello_10114(name) /* (name : string) -> ask () */  {
   
  var ev_10135 = $std_core_hnd._evv_at(0);
   
  var x_10133 = ev_10135.hnd._fun_ask(ev_10135.marker, ev_10135);
   
  function next_10134(_y_x10031) /* (string) -> ask () */  {
    return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_("Hello ", $std_core_types._lp__plus__plus__rp_(name, $std_core_types._lp__plus__plus__rp_(", ", _y_x10031))));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10134);
  }
  else {
    return next_10134(x_10133);
  }
}
 
export function hello() /* () -> <ask,console/console> () */  {
   
  var ev_10142 = $std_core_hnd._evv_at(0);
   
  var x_10139 = ev_10142.hnd._fun_ask(ev_10142.marker, ev_10142);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_hello_10114);
  }
  else {
     
    var ev_0_10147 = $std_core_hnd._evv_at(0);
     
    var x_0_10144 = ev_0_10147.hnd._fun_ask(ev_0_10147.marker, ev_0_10147);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10031 /* string */ ) {
        return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_("Hello ", $std_core_types._lp__plus__plus__rp_(x_10139, $std_core_types._lp__plus__plus__rp_(", ", _y_x10031))));
      });
    }
    else {
      return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_("Hello ", $std_core_types._lp__plus__plus__rp_(x_10139, $std_core_types._lp__plus__plus__rp_(", ", x_0_10144))));
    }
  }
}
 
export function example2() /* () -> console/console () */  {
  return ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17 /* hnd/ev<ask> */ ) {
        return "there";
      }), function(_res /* () */ ) {
      return _res;
    }, hello);
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:raise` type.
export function raise_fs__cfc(raise_0) /* forall<e,a> (raise : raise<e,a>) -> int */  {
  return raise_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@ctl-raise` constructor field of the `:raise` type.
export function raise_fs__ctl_raise(raise_0) /* forall<e,a,b> (raise : raise<e,a>) -> hnd/clause1<string,b,raise,e,a> */  {
  return raise_0._ctl_raise;
}
 
 
// handler for the effect `:raise`
export function raise_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : raise<e,b>, ret : (res : a) -> e b, action : () -> <raise|e> a) -> e b */  {
  return $std_core_hnd._hhandle(raise_fs__tag, hnd, ret, action);
}
 
 
// select `raise` operation out of effect `:raise`
export function raise_fs__select(hnd) /* forall<a,e,b> (hnd : raise<e,b>) -> hnd/clause1<string,a,raise,e,b> */  {
  return hnd._ctl_raise;
}
 
 
// Call the `ctl raise` operation of the effect `:raise`
export function raise(s) /* forall<a> (s : string) -> raise a */  {
   
  var ev_10153 = $std_core_hnd._evv_at(0);
  var _x0 = ev_10153.hnd._ctl_raise;
  return _x0(ev_10153.marker, ev_10153, s);
}
 
export function safe_div(x, y) /* (x : int, y : int) -> raise int */  {
  if ($std_core_types._int_eq(y,0)) {
     
    var ev_10156 = $std_core_hnd._evv_at(0);
    var _x1 = ev_10156.hnd._ctl_raise;
    return _x1(ev_10156.marker, ev_10156, "division by zero");
  }
  else {
    return $std_core_types._int_div(x,y);
  }
}
 
export function rcatch(action, h) /* forall<a,e> (action : () -> <raise|e> a, h : (string) -> e a) -> e a */  {
  return raise_fs__handle(_Hnd_raise(3, function(m /* hnd/marker<1065,1064> */ , ___wildcard_x639__16 /* hnd/ev<raise> */ , x /* string */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<_985,1064>) -> 1065 1064 */ ) {
            return $std_core_hnd.protect(x, function(s /* string */ , resume /* (1051) -> 1065 1064 */ ) {
                return h(s);
              }, k);
          });
      }), function(_res /* 1064 */ ) {
      return _res;
    }, action);
}
 
export function zerodiv(x, y) /* (x : int, y : int) -> int */  {
  return raise_fs__handle(_Hnd_raise(3, function(m /* hnd/marker<total,int> */ , ___wildcard_x639__16 /* hnd/ev<raise> */ , x_0 /* string */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<_985,int>) -> int */ ) {
            return $std_core_hnd.protect(x_0, function(s /* string */ , resume /* (1051) -> int */ ) {
                return 0;
              }, k);
          });
      }), function(_res /* int */ ) {
      return _res;
    }, function() {
      return safe_div(x, y);
    });
}
 
 
// monadic lift
export function _mlift_to_maybe_10115(_y_x10039) /* forall<a,e> (a) -> <raise|e> maybe<a> */  {
  return $std_core_types.Just(_y_x10039);
}
 
 
// reify the exception effect back to a `:maybe` value
export function to_maybe(action) /* forall<a,e> (action : () -> <raise|e> a) -> e maybe<a> */  {
  return raise_fs__handle(_Hnd_raise(0, function(m /* hnd/marker<1186,maybe<1185>> */ , ___wildcard_x654__16 /* hnd/ev<raise> */ , x /* string */ ) {
        return $std_core_hnd.yield_to_final(m, function(___wildcard_x654__45 /* (hnd/resume-result<1165,maybe<1185>>) -> 1186 maybe<1185> */ ) {
            return $std_core_types.Nothing;
          });
      }), function(_res /* maybe<1185> */ ) {
      return _res;
    }, function() {
       
      var x_0_10162 = action();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10039 /* 1185 */ ) {
          return $std_core_types.Just(_y_x10039);
        });
      }
      else {
        return $std_core_types.Just(x_0_10162);
      }
    });
}
 
export function example1() /* () -> console/console () */  {
   
  var x_10003 = raise_fs__handle(_Hnd_raise(3, function(m /* hnd/marker<total,int> */ , ___wildcard_x639__16 /* hnd/ev<raise> */ , x /* string */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<_985,int>) -> int */ ) {
            return $std_core_hnd.protect(x, function(s /* string */ , resume /* (1051) -> int */ ) {
                return 0;
              }, k);
          });
      }), function(_res /* int */ ) {
      return _res;
    }, function() {
      return safe_div(10, 0);
    });
  return $std_core_console.printsln($std_core_int.show(x_10003));
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:state` type.
export function state_fs__cfc(state_0) /* forall<a,e,b> (state : state<a,e,b>) -> int */  {
  return state_0._cfc;
}
 
 
// handler for the effect `:state`
export function state_fs__handle(hnd, ret, action) /* forall<a,b,e,c> (hnd : state<a,e,c>, ret : (res : b) -> e c, action : () -> <state<a>|e> b) -> e c */  {
  return $std_core_hnd._hhandle(state_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-get` constructor field of the `:state` type.
export function state_fs__fun_get(state_0) /* forall<a,e,b> (state : state<a,e,b>) -> hnd/clause0<a,state<a>,e,b> */  {
  return state_0._fun_get;
}
 
 
// select `get` operation out of effect `:state`
export function get_fs__select(hnd) /* forall<a,e,b> (hnd : state<a,e,b>) -> hnd/clause0<a,state<a>,e,b> */  {
  return hnd._fun_get;
}
 
 
// Call the `fun get` operation of the effect `:state`
export function get() /* forall<a> () -> (state<a>) a */  {
   
  var ev_10167 = $std_core_hnd._evv_at(0);
  return ev_10167.hnd._fun_get(ev_10167.marker, ev_10167);
}
 
 
// Automatically generated. Retrieves the `@fun-put` constructor field of the `:state` type.
export function state_fs__fun_put(state_0) /* forall<a,e,b> (state : state<a,e,b>) -> hnd/clause1<a,(),state<a>,e,b> */  {
  return state_0._fun_put;
}
 
 
// select `put` operation out of effect `:state`
export function put_fs__select(hnd) /* forall<a,e,b> (hnd : state<a,e,b>) -> hnd/clause1<a,(),state<a>,e,b> */  {
  return hnd._fun_put;
}
 
 
// Call the `fun put` operation of the effect `:state`
export function put(x) /* forall<a> (x : a) -> (state<a>) () */  {
   
  var ev_10169 = $std_core_hnd._evv_at(0);
  return ev_10169.hnd._fun_put(ev_10169.marker, ev_10169, x);
}
 
 
// monadic lift
export function _mlift_counter_10116(wild___0) /* (wild_@0 : ()) -> (state<int>) () */  {
  return counter();
}
 
 
// monadic lift
export function _mlift_counter_10117(i) /* (i : int) -> (state<int>) () */  {
  if ($std_core_types._int_le(i,0)) {
    return $std_core_types.Unit;
  }
  else {
     
    $std_core_console.printsln("hi");
     
    var x_10009 = $std_core_types._int_sub(i,1);
     
    var ev_10174 = $std_core_hnd._evv_at(0);
     
    var x_10172 = ev_10174.hnd._fun_put(ev_10174.marker, ev_10174, x_10009);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_mlift_counter_10116);
    }
    else {
      return _mlift_counter_10116(x_10172);
    }
  }
}
 
export function counter() /* () -> <console/console,div,state<int>> () */  { tailcall: while(1)
{
   
  var ev_0_10180 = $std_core_hnd._evv_at(0);
   
  var x_1_10177 = ev_0_10180.hnd._fun_get(ev_0_10180.marker, ev_0_10180);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_counter_10117);
  }
  else {
    if ($std_core_types._int_le(x_1_10177,0)) {
      return $std_core_types.Unit;
    }
    else {
       
      $std_core_console.printsln("hi");
       
      var x_10009_0 = $std_core_types._int_sub(x_1_10177,1);
       
      var ev_1_10185 = $std_core_hnd._evv_at(0);
       
      var x_2_10182 = ev_1_10185.hnd._fun_put(ev_1_10185.marker, ev_1_10185, x_10009_0);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_counter_10116);
      }
      else {
        {
          // tail call
          continue tailcall;
        }
      }
    }
  }
}}
 
 
// monadic lift
export function _mlift_state_10118(x_0, _y_x10051) /* forall<h,a,e,b> (x@0 : a, b) -> <local<h>,div|e> (a, b) */  {
  return $std_core_types.Tuple2(x_0, _y_x10051);
}
 
 
// State handler.
export function state(init, action) /* forall<a,e,b> (init : b, action : () -> <state<b>,div|e> a) -> <div|e> (a, b) */  {
  return function() {
     
    var loc = { value: init };
     
    var res = state_fs__handle(_Hnd_state(1, $std_core_hnd.clause_tail0(function() {
          return ((loc).value);
        }), $std_core_hnd.clause_tail1(function(x /* 2007 */ ) {
          return ((loc).value = x);
        })), function(x_0 /* 2005 */ ) {
         
        var x_1_10190 = ((loc).value);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10051 /* 2007 */ ) {
            return $std_core_types.Tuple2(x_0, _y_x10051);
          });
        }
        else {
          return $std_core_types.Tuple2(x_0, x_1_10190);
        }
      }, action);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
export function example3() /* () -> <console/console,div> int */  {
   
  var tuple2_10011 = state(1, counter);
  return tuple2_10011.snd;
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:yield` type.
export function yield_fs__cfc(yield_0) /* forall<a,e,b> (yield : yield<a,e,b>) -> int */  {
  return yield_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@ctl-yield` constructor field of the `:yield` type.
export function yield_fs__ctl_yield(yield_0) /* forall<a,e,b> (yield : yield<a,e,b>) -> hnd/clause1<a,(),yield<a>,e,b> */  {
  return yield_0._ctl_yield;
}
 
 
// handler for the effect `:yield`
export function yield_fs__handle(hnd, ret, action) /* forall<a,b,e,c> (hnd : yield<a,e,c>, ret : (res : b) -> e c, action : () -> <yield<a>|e> b) -> e c */  {
  return $std_core_hnd._hhandle(yield_fs__tag, hnd, ret, action);
}
 
 
// select `yield` operation out of effect `:yield`
export function yield_fs__select(hnd) /* forall<a,e,b> (hnd : yield<a,e,b>) -> hnd/clause1<a,(),yield<a>,e,b> */  {
  return hnd._ctl_yield;
}
 
 
// Call the `ctl yield` operation of the effect `:yield`
export function $yield(item) /* forall<a> (item : a) -> (yield<a>) () */  {
   
  var ev_10196 = $std_core_hnd._evv_at(0);
  return ev_10196.hnd._ctl_yield(ev_10196.marker, ev_10196, item);
}
 
 
// monadic lift
export function _mlift_iterate_10119(xx, wild__) /* forall<a> (xx : list<a>, wild_ : ()) -> (yield<a>) () */  {
  return iterate(xx);
}
 
export function iterate(xs) /* forall<a> (xs : list<a>) -> (yield<a>) () */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var ev_10202 = $std_core_hnd._evv_at(0);
     
    var x_0_10199 = ev_10202.hnd._ctl_yield(ev_10202.marker, ev_10202, xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___0 /* () */ ) {
        return _mlift_iterate_10119(xs.tail, wild___0);
      });
    }
    else {
      {
        // tail call
        xs = xs.tail;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types.Unit;
  }
}}
 
 
// monadic lift
export function _mlift_foreach_10120(resume, _y_x10061) /* forall<e> (resume : (()) -> e (), bool) -> e () */  {
  if (_y_x10061) {
    return resume($std_core_types.Unit);
  }
  else {
    return $std_core_types.Unit;
  }
}
 
export function foreach(f, action) /* forall<a,e> (f : (a) -> e bool, action : () -> <yield<a>|e> ()) -> e () */  {
  return yield_fs__handle(_Hnd_yield(3, function(m /* hnd/marker<2466,()> */ , ___wildcard_x639__16 /* hnd/ev<yield<2465>> */ , x /* 2465 */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<(),()>) -> 2466 () */ ) {
            return $std_core_hnd.protect(x, function(x_0 /* 2465 */ , resume /* (()) -> 2466 () */ ) {
                 
                var x_1_10206 = f(x_0);
                 
                function next_10207(_y_x10061) /* (bool) -> 2466 () */  {
                  if (_y_x10061) {
                    return resume($std_core_types.Unit);
                  }
                  else {
                    return $std_core_types.Unit;
                  }
                }
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(next_10207);
                }
                else {
                  return next_10207(x_1_10206);
                }
              }, k);
          });
      }), function(_res /* () */ ) {
      return _res;
    }, action);
}
 
export function example4() /* () -> console/console () */  {
  return yield_fs__handle(_Hnd_yield(3, function(m /* hnd/marker<console/console,()> */ , ___wildcard_x639__16 /* hnd/ev<yield<int>> */ , x /* int */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<(),()>) -> console/console () */ ) {
            return $std_core_hnd.protect(x, function(x_0 /* int */ , resume /* (()) -> console/console () */ ) {
                 
                $std_core_console.printsln($std_core_int.show(x_0));
                if ($std_core_types._int_le(x_0,1)) {
                  return resume($std_core_types.Unit);
                }
                else {
                  return $std_core_types.Unit;
                }
              }, k);
          });
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
      return iterate($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Nil))));
    });
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:flip` type.
export function flip_fs__cfc(flip_0) /* forall<e,a> (flip : flip<e,a>) -> int */  {
  return flip_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@ctl-flip` constructor field of the `:flip` type.
export function flip_fs__ctl_flip(flip_0) /* forall<e,a> (flip : flip<e,a>) -> hnd/clause0<bool,flip,e,a> */  {
  return flip_0._ctl_flip;
}
 
 
// handler for the effect `:flip`
export function flip_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : flip<e,b>, ret : (res : a) -> e b, action : () -> <flip|e> a) -> e b */  {
  return $std_core_hnd._hhandle(flip_fs__tag, hnd, ret, action);
}
 
 
// select `flip` operation out of effect `:flip`
export function flip_fs__select(hnd) /* forall<e,a> (hnd : flip<e,a>) -> hnd/clause0<bool,flip,e,a> */  {
  return hnd._ctl_flip;
}
 
 
// Call the `ctl flip` operation of the effect `:flip`
export function flip() /* () -> flip bool */  {
   
  var ev_10212 = $std_core_hnd._evv_at(0);
  return ev_10212.hnd._ctl_flip(ev_10212.marker, ev_10212);
}
 
 
// handler that randomly flips
export function coinflip(_action) /* forall<a,e> (() -> <flip,ndet|e> a) -> <ndet|e> a */  {
  return flip_fs__handle(_Hnd_flip(1, $std_core_hnd.clause_tail0(function() {
        return (($std_num_random.srandom_int32()) >= 0);
      })), function(_res /* 2901 */ ) {
      return _res;
    }, _action);
}
 
 
// monadic lift
export function _mlift_amb_10121(_y_x10069, _y_x10070) /* forall<a,e> (list<a>, list<a>) -> e list<a> */  {
  return $std_core_list.append(_y_x10069, _y_x10070);
}
 
 
// monadic lift
export function _mlift_amb_10122(resume, _y_x10069) /* forall<a,e> (resume : (bool) -> e list<a>, list<a>) -> e list<a> */  {
   
  var x_10214 = resume(true);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10070 /* list<3007> */ ) {
      return $std_core_list.append(_y_x10069, _y_x10070);
    });
  }
  else {
    return $std_core_list.append(_y_x10069, x_10214);
  }
}
 
 
// handler that returns all possible outcomes
export function amb(_action) /* forall<a,e> (() -> <flip|e> a) -> e list<a> */  {
  return flip_fs__handle(_Hnd_flip(3, function(m /* hnd/marker<3008,list<3007>> */ , ___wildcard_x688__16 /* hnd/ev<flip> */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<bool,list<3007>>) -> 3008 list<3007> */ ) {
            return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (bool) -> 3008 list<3007> */ ) {
                 
                var x_10219 = r(false);
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(function(_y_x10069 /* list<3007> */ ) {
                    return _mlift_amb_10122(r, _y_x10069);
                  });
                }
                else {
                  return _mlift_amb_10122(r, x_10219);
                }
              }, k);
          });
      }), function(x_0 /* 3007 */ ) {
      return $std_core_types.Cons(x_0, $std_core_types.Nil);
    }, _action);
}
 
 
// monadic lift
export function _mlift_example4b_10123(resume, x, _c_x10074) /* (resume : (()) -> <console/console,flip> (), x : int, ()) -> () */  {
  if ($std_core_types._int_le(x,1)) {
    return resume($std_core_types.Unit);
  }
  else {
    return $std_core_types.Unit;
  }
}
 
 
// monadic lift
export function _mlift_example4b_10124(resume, x, _y_x10072) /* (resume : (()) -> <console/console,flip> (), x : int, bool) -> flip () */  {
   
  if (_y_x10072) {
     
    var x_1_10223 = $std_core_int.show(x);
    if ($std_core_hnd._yielding()) {
      var x_0_10221 = $std_core_hnd.yield_extend($std_core_console.printsln);
    }
    else {
      var x_0_10221 = $std_core_console.printsln(x_1_10223);
    }
  }
  else {
    var x_0_10221 = $std_core_console.printsln($std_core_types._lp__plus__plus__rp_("flip false ", $std_core_int.show(x)));
  }
   
  function next_10222(_c_x10074) /* (()) -> () */  {
    if ($std_core_types._int_le(x,1)) {
      return resume($std_core_types.Unit);
    }
    else {
      return $std_core_types.Unit;
    }
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10222);
  }
  else {
    return next_10222(x_0_10221);
  }
}
 
export function example4b() /* () -> console/console () */  {
   
  var x_4195 = amb(function() {
    return yield_fs__handle(_Hnd_yield(3, function(m /* hnd/marker<<console/console,flip>,()> */ , ___wildcard_x639__16 /* hnd/ev<yield<int>> */ , x /* int */ ) {
          return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<(),()>) -> <console/console,flip> () */ ) {
              return $std_core_hnd.protect(x, function(x_0 /* int */ , resume /* (()) -> <console/console,flip> () */ ) {
                   
                  var ev_10231 = $std_core_hnd._evv_at(0);
                   
                  var x_1_10229 = ev_10231.hnd._ctl_flip(ev_10231.marker, ev_10231);
                  if ($std_core_hnd._yielding()) {
                    return $std_core_hnd.yield_extend(function(_y_x10072 /* bool */ ) {
                      return _mlift_example4b_10124(resume, x_0, _y_x10072);
                    });
                  }
                  else {
                    return _mlift_example4b_10124(resume, x_0, x_1_10229);
                  }
                }, k);
            });
        }), function(_res /* () */ ) {
        return _res;
      }, function() {
        return $std_core_hnd._open_at1(1, iterate, $std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Nil))));
      });
  });
  return $std_core_types.Unit;
}
 
 
// monadic lift
export function _mlift_xor_10125(p, q) /* (p : bool, q : bool) -> flip bool */  {
  if (p) {
    var _x2 = (p) ? q : false;
    return $std_core_hnd._open_none1(function(b /* bool */ ) {
        return (b) ? false : true;
      }, _x2);
  }
  else {
    if (q) {
      var _x3 = (p) ? q : false;
      return $std_core_hnd._open_none1(function(b_0 /* bool */ ) {
          return (b_0) ? false : true;
        }, _x3);
    }
    else {
      return false;
    }
  }
}
 
 
// monadic lift
export function _mlift_xor_10126(p) /* (p : bool) -> flip bool */  {
   
  var ev_10235 = $std_core_hnd._evv_at(0);
   
  var x_10233 = ev_10235.hnd._ctl_flip(ev_10235.marker, ev_10235);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(q /* bool */ ) {
      return _mlift_xor_10125(p, q);
    });
  }
  else {
    return _mlift_xor_10125(p, x_10233);
  }
}
 
export function xor() /* () -> flip bool */  {
   
  var ev_10240 = $std_core_hnd._evv_at(0);
   
  var x_10237 = ev_10240.hnd._ctl_flip(ev_10240.marker, ev_10240);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_xor_10126);
  }
  else {
     
    var ev_0_10245 = $std_core_hnd._evv_at(0);
     
    var x_0_10242 = ev_0_10245.hnd._ctl_flip(ev_0_10245.marker, ev_0_10245);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(q /* bool */ ) {
        return _mlift_xor_10125(x_10237, q);
      });
    }
    else {
      if (x_10237) {
        var _x4 = (x_10237) ? x_0_10242 : false;
        return $std_core_hnd._open_none1(function(b /* bool */ ) {
            return (b) ? false : true;
          }, _x4);
      }
      else {
        if (x_0_10242) {
          var _x5 = (x_10237) ? x_0_10242 : false;
          return $std_core_hnd._open_none1(function(b_0 /* bool */ ) {
              return (b_0) ? false : true;
            }, _x5);
        }
        else {
          return false;
        }
      }
    }
  }
}
 
export function example5() /* () -> console/console () */  {
   
  var s_10021 = $std_core_list.show(amb(xor), $std_core_bool.show);
  return $std_core_console.printsln(s_10021);
}
 
 
// monadic lift
export function _mlift_surprising_10127(i, p, wild__) /* (i : int, p : bool, wild_ : ()) -> <state<int>,flip> bool */  {
  if ($std_core_types._int_gt(i,0)) {
    if (p) {
      return $std_core_hnd._open_at0(0, xor);
    }
    else {
      return false;
    }
  }
  else {
    return false;
  }
}
 
 
// monadic lift
export function _mlift_surprising_10128(p, i) /* (p : bool, i : int) -> <state<int>,flip> bool */  {
   
  var _x_x1_10112 = $std_core_types._int_add(i,1);
   
  var x_10247 = $std_core_hnd._open_at1(1, function(x_0 /* int */ ) {
       
      var ev_10249 = $std_core_hnd._evv_at(0);
      return ev_10249.hnd._fun_put(ev_10249.marker, ev_10249, x_0);
    }, _x_x1_10112);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return _mlift_surprising_10127(i, p, wild__);
    });
  }
  else {
    return _mlift_surprising_10127(i, p, x_10247);
  }
}
 
 
// monadic lift
export function _mlift_surprising_10129(p) /* (p : bool) -> <flip,state<int>> bool */  {
   
  var x_10252 = $std_core_hnd._open_at0(1, function() {
       
      var ev_10254 = $std_core_hnd._evv_at(0);
      return ev_10254.hnd._fun_get(ev_10254.marker, ev_10254);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(i /* int */ ) {
      return _mlift_surprising_10128(p, i);
    });
  }
  else {
    return _mlift_surprising_10128(p, x_10252);
  }
}
 
export function surprising() /* () -> <flip,state<int>> bool */  {
   
  var x_10256 = $std_core_hnd._open_at0(0, function() {
       
      var ev_10259 = $std_core_hnd._evv_at(0);
      return ev_10259.hnd._ctl_flip(ev_10259.marker, ev_10259);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_surprising_10129);
  }
  else {
     
    var x_0_10261 = $std_core_hnd._open_at0(1, function() {
         
        var ev_0_10264 = $std_core_hnd._evv_at(0);
        return ev_0_10264.hnd._fun_get(ev_0_10264.marker, ev_0_10264);
      });
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(i /* int */ ) {
        return _mlift_surprising_10128(x_10256, i);
      });
    }
    else {
       
      var _x_x1_10112 = $std_core_types._int_add(x_0_10261,1);
       
      var x_1_10266 = $std_core_hnd._open_at1(1, function(x_2 /* int */ ) {
           
          var ev_1_10269 = $std_core_hnd._evv_at(0);
          return ev_1_10269.hnd._fun_put(ev_1_10269.marker, ev_1_10269, x_2);
        }, _x_x1_10112);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
          return _mlift_surprising_10127(x_0_10261, x_10256, wild__);
        });
      }
      else {
        if ($std_core_types._int_gt(x_0_10261,0)) {
          if (x_10256) {
            return $std_core_hnd._open_at0(0, xor);
          }
          else {
            return false;
          }
        }
        else {
          return false;
        }
      }
    }
  }
}
 
export function example6() /* () -> <console/console,div> () */  {
   
  var s_10022 = $std_core_tuple.tuple2_fs_show(state(0, function() {
        return amb(surprising);
      }), function(_arg_x1 /* list<bool> */ ) {
      return $std_core_list.show(_arg_x1, $std_core_bool.show);
    }, $std_core_int.show);
  return $std_core_console.printsln(s_10022);
}
 
export function example7() /* () -> <console/console,div> () */  {
   
  var s_10023 = $std_core_list.show(amb(function() {
      return state(0, surprising);
    }), function(_arg_x1 /* (bool, int) */ ) {
      return $std_core_tuple.tuple2_fs_show(_arg_x1, $std_core_bool.show, $std_core_int.show);
    });
  return $std_core_console.printsln(s_10023);
}
 
 
// --------------------------------------------------------
//  Testing
// --------------------------------------------------------
export function main() /* () -> <console/console,div> int */  {
   
  var x_10003 = raise_fs__handle(_Hnd_raise(3, function(m /* hnd/marker<total,int> */ , ___wildcard_x639__16 /* hnd/ev<raise> */ , x /* string */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<_985,int>) -> int */ ) {
            return $std_core_hnd.protect(x, function(s /* string */ , resume /* (1051) -> int */ ) {
                return 0;
              }, k);
          });
      }), function(_res /* int */ ) {
      return _res;
    }, function() {
      return safe_div(10, 0);
    });
   
  $std_core_console.printsln($std_core_int.show(x_10003));
   
  ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17 /* hnd/ev<ask> */ ) {
        return "there";
      }), function(_res_0 /* () */ ) {
      return _res_0;
    }, hello);
   
  var tuple2 = state(1, counter);
   
  example4();
   
  $std_core_console.printsln($std_core_list.show(amb(xor), $std_core_bool.show));
   
  $std_core_console.printsln($std_core_tuple.tuple2_fs_show(state(0, function() {
        return amb(surprising);
      }), function(_arg_x1 /* list<bool> */ ) {
      return $std_core_list.show(_arg_x1, $std_core_bool.show);
    }, $std_core_int.show));
  return 42;
}
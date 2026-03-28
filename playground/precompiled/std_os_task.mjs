// Koka generated module: std/os/task, koka version: 3.2.4
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
import * as $std_num_int32 from './std_num_int32.mjs';
 
// externals
 
// type declarations
// type lvar
export function Lvar(lv) /* forall<a> (lv : any) -> lvar<a> */  {
  return lv;
}
// type promise
export function Promise(promise) /* forall<a> (promise : any) -> promise<a> */  {
  return promise;
}
 
// declarations
 
 
// Automatically generated. Retrieves the `promise` constructor field of the `:promise` type.
export function promise_fs_promise(promise) /* forall<a> (promise : promise<a>) -> any */  {
  return promise;
}
 
export function promise_fs__copy(_this, promise) /* forall<a> (promise<a>, promise : ? any) -> promise<a> */  {
  if (promise !== undefined) {
    var _x0 = promise;
  }
  else {
    var _x0 = _this;
  }
  return _x0;
}
 
export function unsafe__task(work) /* forall<a> (work : () -> pure a) -> pure any */  {
  return $std_core._unsupported_external("std/os/task/@extern-unsafe_task");
}
 
export function unsafe__await(p) /* forall<a> (p : any) -> pure a */  {
  return $std_core._unsupported_external("std/os/task/@extern-unsafe_await");
}
 
export function prim_task_set_default_concurrency(thread_count) /* (thread-count : ssize_t) -> io () */  {
  return ((function() {
    try {
      return $std_core._unsupported_external("std/os/task/@extern-prim-task-set-default-concurrency");
    }
    catch(_err){ return $std_core_exn._throw_exception(_err); }
  })());
}
 
export function task_set_default_concurrency(thread_count) /* (thread-count : int) -> io () */  {
  return prim_task_set_default_concurrency($std_core_int.ssize__t(thread_count));
}
 
 
// monadic lift
export function _mlift_task_10024(_y_x10007) /* forall<a> (any) -> pure promise<a> */  {
  return _y_x10007;
}
 
 
// Spark a pure computation in a separate thread of control.
export function task(work) /* forall<a> (work : () -> pure a) -> pure promise<a> */  {
   
  var x_10027 = unsafe__task(work);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10007 /* any */ ) {
      return _y_x10007;
    });
  }
  else {
    return x_10027;
  }
}
 
 
// Await the result of a promise.
export function $await(p) /* forall<a> (p : promise<a>) -> pure a */  {
  return unsafe__await($std_core_hnd._open_none1(function(promise /* promise<257> */ ) {
      return promise;
    }, p));
}
 
 
// Await the result of a list of promises.
export function list_fs_await(ps) /* forall<a> (ps : list<promise<a>>) -> pure list<a> */  {
  return $std_core_list.map(ps, $await);
}
 
 
// monadic lift
export function _mlift_parallel_10025(ps_10001) /* forall<a> (ps@10001 : list<promise<a>>) -> <div,exn> list<a> */  {
  return $std_core_list.map(ps_10001, $await);
}
 
 
// Run a list of pure computations in parallel.
export function parallel(xs) /* forall<a> (xs : list<() -> pure a>) -> pure list<a> */  {
   
  var x_10031 = $std_core_list.map(xs, task);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(ps_10001 /* list<promise<430>> */ ) {
      return $std_core_list.map(ps_10001, $await);
    });
  }
  else {
    return $std_core_list.map(x_10031, $await);
  }
}
 
 
// Automatically generated. Retrieves the `lv` constructor field of the `:lvar` type.
export function lvar_fs_lv(lvar_0) /* forall<a> (lvar : lvar<a>) -> any */  {
  return lvar_0;
}
 
export function lvar_fs__copy(_this, lv) /* forall<a> (lvar<a>, lv : ? any) -> lvar<a> */  {
  if (lv !== undefined) {
    var _x1 = lv;
  }
  else {
    var _x1 = _this;
  }
  return _x1;
}
 
export function unsafe_lvar(init) /* forall<a> (init : a) -> pure any */  {
  return $std_core._unsupported_external("std/os/task/@extern-unsafe-lvar");
}
 
export function unsafe_put(lvar_0, x, monotonic_combine) /* forall<a> (lvar : any, x : a, monotonic-combine : (a, a) -> a) -> pure () */  {
  return $std_core._unsupported_external("std/os/task/@extern-unsafe-put");
}
 
export function unsafe_get(lvar_0, bot, is_gte) /* forall<a> (lvar : any, bot : a, is-gte : (a, a) -> int32) -> pure a */  {
  return $std_core._unsupported_external("std/os/task/@extern-unsafe-get");
}
 
 
// monadic lift
export function _mlift_lvar_10026(_y_x10015) /* forall<a> (any) -> pure lvar<a> */  {
  return _y_x10015;
}
 
export function lvar(init) /* forall<a> (init : a) -> pure lvar<a> */  {
   
  var x_10035 = unsafe_lvar(init);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10015 /* any */ ) {
      return _y_x10015;
    });
  }
  else {
    return x_10035;
  }
}
 
export function put(lvar_0, x, monotonic_combine) /* forall<a> (lvar : lvar<a>, x : a, monotonic-combine : (a, a) -> a) -> pure () */  {
  return unsafe_put($std_core_hnd._open_none1(function(lvar_0_0 /* lvar<606> */ ) {
        return lvar_0_0;
      }, lvar_0), x, monotonic_combine);
}
 
export function get(lvar_0, bot, is_gte) /* forall<a> (lvar : lvar<a>, bot : a, is-gte : (a, a) -> bool) -> pure a */  {
  return unsafe_get($std_core_hnd._open_none1(function(lvar_0_0 /* lvar<647> */ ) {
        return lvar_0_0;
      }, lvar_0), bot, function(x /* 647 */ , y /* 647 */ ) {
      var _x2 = is_gte(x, y);
      return (_x2) ? $std_num_int32.one : $std_num_int32.zero;
    });
}
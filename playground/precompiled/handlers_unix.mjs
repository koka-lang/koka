// Koka generated module: handlers/unix, koka version: 3.2.4
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
 
 
// runtime tag for the effect `:bio`
export var bio_fs__tag;
var bio_fs__tag = "bio@unix";
 
 
// runtime tag for the effect `:exit`
export var exit_fs__tag;
var exit_fs__tag = "exit@unix";
 
 
// runtime tag for the effect `:whoami`
export var whoami_fs__tag;
var whoami_fs__tag = "whoami@unix";
 
 
// runtime tag for the effect `:su`
export var su_fs__tag;
var su_fs__tag = "su@unix";
 
 
// runtime tag for the effect `:fork`
export var fork_fs__tag;
var fork_fs__tag = "fork@unix";
 
 
// runtime tag for the effect `:interrupt`
export var interrupt_fs__tag;
var interrupt_fs__tag = "interrupt@unix";
// type bio
export function _Hnd_bio(_cfc, _fun_write) /* forall<e,a> (int, hnd/clause2<filedesc,string,(),bio,e,a>) -> bio<e,a> */  {
  return { _cfc: _cfc, _fun_write: _fun_write };
}
// type exit
export function _Hnd_exit(_cfc, _ctl_exit) /* forall<e,a> (int, forall<b> hnd/clause1<int,b,exit,e,a>) -> exit<e,a> */  {
  return { _cfc: _cfc, _ctl_exit: _ctl_exit };
}
// type fork
export function _Hnd_fork(_cfc, _ctl_fork) /* forall<e,a> (int, hnd/clause0<bool,fork,e,a>) -> fork<e,a> */  {
  return { _cfc: _cfc, _ctl_fork: _ctl_fork };
}
// type interrupt
export function _Hnd_interrupt(_cfc, _ctl_interrupt) /* forall<e,a> (int, hnd/clause0<(),interrupt,e,a>) -> interrupt<e,a> */  {
  return { _cfc: _cfc, _ctl_interrupt: _ctl_interrupt };
}
// type pstate
export function Done(result) /* forall<e,a> (result : a) -> pstate<e,a> */  {
  return { _tag: 1, result: result };
}
export function Paused(resumption) /* forall<e,a> (resumption : () -> e pstate<e,a>) -> pstate<e,a> */  {
  return { _tag: 2, resumption: resumption };
}
// type user
export const Root = 1; // user
export const Alice = 2; // user
export const Bob = 3; // user
// type su
export function _Hnd_su(_cfc, _ctl_su) /* forall<e,a> (int, hnd/clause1<user,(),su,e,a>) -> su<e,a> */  {
  return { _cfc: _cfc, _ctl_su: _ctl_su };
}
// type whoami
export function _Hnd_whoami(_cfc, _fun_whoami) /* forall<e,a> (int, hnd/clause0<string,whoami,e,a>) -> whoami<e,a> */  {
  return { _cfc: _cfc, _fun_whoami: _fun_whoami };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:bio` type.
export function bio_fs__cfc(bio_0) /* forall<e,a> (bio : bio<e,a>) -> int */  {
  return bio_0._cfc;
}
 
 
// handler for the effect `:bio`
export function bio_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : bio<e,b>, ret : (res : a) -> e b, action : () -> <bio|e> a) -> e b */  {
  return $std_core_hnd._hhandle(bio_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-write` constructor field of the `:bio` type.
export function bio_fs__fun_write(bio_0) /* forall<e,a> (bio : bio<e,a>) -> hnd/clause2<filedesc,string,(),bio,e,a> */  {
  return bio_0._fun_write;
}
 
 
// select `write` operation out of effect `:bio`
export function write_fs__select(hnd) /* forall<e,a> (hnd : bio<e,a>) -> hnd/clause2<filedesc,string,(),bio,e,a> */  {
  return hnd._fun_write;
}
 
 
// Write a string to a file descriptor
// Call the `fun write` operation of the effect `:bio`
export function write(fd, s) /* (fd : filedesc, s : string) -> bio () */  {
   
  var evx_10318 = $std_core_hnd._evv_at(0);
  return evx_10318.hnd._fun_write(evx_10318.marker, evx_10318, fd, s);
}
 
export var stdout;
var stdout = 0;
 
export function echo(s) /* (s : string) -> bio () */  {
   
  var evx_10322 = $std_core_hnd._evv_at(0);
  return evx_10322.hnd._fun_write(evx_10322.marker, evx_10322, 0, s);
}
 
 
// monadic lift
export function _mlift_bio_10272(buf, s, _y_x10066) /* forall<h,e> (buf : local-var<h,string>, s : string, string) -> <local<h>|e> () */  {
  return ((buf).value = ($std_core_types._lp__plus__plus__rp_(_y_x10066, s)));
}
 
 
// monadic lift
export function _mlift_bio_10273(_y_x10068, _y_x10069) /* forall<h,a,e> (a, string) -> <local<h>,bio|e> (a, string) */  {
  return $std_core_types.Tuple2(_y_x10068, _y_x10069);
}
 
 
// monadic lift
export function _mlift_bio_10274(buf, _y_x10068) /* forall<h,a,e> (buf : local-var<h,string>, a) -> <local<h>,bio|e> (a, string) */  {
   
  var x_10326 = ((buf).value);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10069 /* string */ ) {
      return $std_core_types.Tuple2(_y_x10068, _y_x10069);
    });
  }
  else {
    return $std_core_types.Tuple2(_y_x10068, x_10326);
  }
}
 
export function bio(action) /* forall<a,e> (action : () -> <bio|e> a) -> e (a, string) */  {
  return function() {
     
    var loc = { value: ("") };
     
    var res = bio_fs__handle(_Hnd_bio(1, function(m /* hnd/marker<<local<739>|750>,(749, string)> */ , ev /* hnd/ev<bio> */ , x1 /* filedesc */ , x2 /* string */ ) {
          return $std_core_hnd.under2(ev, function(fd /* filedesc */ , s /* string */ ) {
               
              var x_10333 = ((loc).value);
               
              function next_10334(_y_x10066) /* (string) -> <local<739>|750> () */  {
                return ((loc).value = ($std_core_types._lp__plus__plus__rp_(_y_x10066, s)));
              }
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(next_10334);
              }
              else {
                return next_10334(x_10333);
              }
            }, x1, x2);
        }), function(_res /* (749, string) */ ) {
        return _res;
      }, function() {
         
        var x_0_10338 = action();
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10068 /* 749 */ ) {
            return _mlift_bio_10274(loc, _y_x10068);
          });
        }
        else {
          return _mlift_bio_10274(loc, x_0_10338);
        }
      });
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// monadic lift
export function _mlift_bio2_10275(x_4366, _y_x10077) /* forall<h,a,e> (x@4366 : a, string) -> <local<h>|e> (a, string) */  {
  return $std_core_types.Tuple2(x_4366, _y_x10077);
}
 
 
// monadic lift
export function _mlift_bio2_10276(buf, s, _y_x10073) /* forall<h,e> (buf : local-var<h,string>, s : string, string) -> <local<h>|e> () */  {
  return ((buf).value = ($std_core_types._lp__plus__plus__rp_(_y_x10073, s)));
}
 
 
// monadic lift
export function _mlift_bio2_10277(buf, x_4366) /* forall<h,a,e> (buf : local-var<h,string>, x@4366 : a) -> <local<h>|e> (a, string) */  {
   
  var x_10341 = ((buf).value);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10077 /* string */ ) {
      return $std_core_types.Tuple2(x_4366, _y_x10077);
    });
  }
  else {
    return $std_core_types.Tuple2(x_4366, x_10341);
  }
}
 
 
// we can also write the previous `bio` using a `with return`
export function bio2(action) /* forall<a,e> (action : () -> <bio|e> a) -> e (a, string) */  {
  return function() {
     
    var loc = { value: ("") };
     
    var x_10347 = bio_fs__handle(_Hnd_bio(1, function(m /* hnd/marker<<local<944>|955>,954> */ , ev /* hnd/ev<bio> */ , x1 /* filedesc */ , x2 /* string */ ) {
          return $std_core_hnd.under2(ev, function(fd /* filedesc */ , s /* string */ ) {
               
              var x_0_10350 = ((loc).value);
               
              function next_0_10351(_y_x10073) /* (string) -> <local<944>|955> () */  {
                return ((loc).value = ($std_core_types._lp__plus__plus__rp_(_y_x10073, s)));
              }
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(next_0_10351);
              }
              else {
                return next_0_10351(x_0_10350);
              }
            }, x1, x2);
        }), function(_res /* 954 */ ) {
        return _res;
      }, action);
     
    if ($std_core_hnd._yielding()) {
      var res = $std_core_hnd.yield_extend(function(x_4366 /* 954 */ ) {
        return _mlift_bio2_10277(loc, x_4366);
      });
    }
    else {
      var res = _mlift_bio2_10277(loc, x_10347);
    }
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// monadic lift
export function _mlift_example1_10278(wild__) /* (wild_ : ()) -> bio () */  {
   
  var evx_10356 = $std_core_hnd._evv_at(0);
  return evx_10356.hnd._fun_write(evx_10356.marker, evx_10356, 0, "unix world");
}
 
export function example1() /* () -> ((), string) */  {
  return bio(function() {
     
    var evx_10362 = $std_core_hnd._evv_at(0);
     
    var x_10360 = evx_10362.hnd._fun_write(evx_10362.marker, evx_10362, 0, "hi ");
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_mlift_example1_10278);
    }
    else {
      return _mlift_example1_10278(x_10360);
    }
  });
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:exit` type.
export function exit_fs__cfc(exit_0) /* forall<e,a> (exit : exit<e,a>) -> int */  {
  return exit_0._cfc;
}
 
 
// handler for the effect `:exit`
export function exit_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : exit<e,b>, ret : (res : a) -> e b, action : () -> <exit|e> a) -> e b */  {
  return $std_core_hnd._hhandle(exit_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@ctl-exit` constructor field of the `:exit` type.
export function exit_fs__ctl_exit(exit_0) /* forall<e,a,b> (exit : exit<e,a>) -> hnd/clause1<int,b,exit,e,a> */  {
  return exit_0._ctl_exit;
}
 
 
// select `exit` operation out of effect `:exit`
export function exit_fs__select(hnd) /* forall<a,e,b> (hnd : exit<e,b>) -> hnd/clause1<int,a,exit,e,b> */  {
  return hnd._ctl_exit;
}
 
 
// Call the `ctl exit` operation of the effect `:exit`
export function exit(exitcode) /* forall<a> (exitcode : int) -> exit a */  {
   
  var ev_10367 = $std_core_hnd._evv_at(0);
  var _x0 = ev_10367.hnd._ctl_exit;
  return _x0(ev_10367.marker, ev_10367, exitcode);
}
 
 
// monadic lift
export function _mlift_status_10279(wild__) /* forall<a,e> (wild_ : a) -> <exit|e> int */  {
  return 0;
}
 
export function status(action) /* forall<a,e> (action : () -> <exit|e> a) -> e int */  {
  return exit_fs__handle(_Hnd_exit(0, function(m /* hnd/marker<1298,int> */ , ___wildcard_x654__16 /* hnd/ev<exit> */ , x /* int */ ) {
        return $std_core_hnd.yield_to_final(m, function(___wildcard_x654__45 /* (hnd/resume-result<1278,int>) -> 1298 int */ ) {
            return x;
          });
      }), function(_res /* int */ ) {
      return _res;
    }, function() {
       
      var x_0_10371 = action();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(wild__ /* 1297 */ ) {
          return 0;
        });
      }
      else {
        return 0;
      }
    });
}
 
 
// monadic lift
export function _mlift_example2_10280(wild__) /* (wild_ : ()) -> <bio,exit> int */  {
  return 0;
}
 
 
// monadic lift
export function _mlift_example2_10281(wild___0_0) /* forall<_a> (wild_@0@0 : _a) -> <exit,bio> int */  {
   
  var x_10374 = $std_core_hnd._open_at1(0, function(s_1 /* string */ ) {
       
      var evx_10376 = $std_core_hnd._evv_at(0);
      return evx_10376.hnd._fun_write(evx_10376.marker, evx_10376, 0, s_1);
    }, "unix world");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_example2_10280);
  }
  else {
    return 0;
  }
}
 
 
// monadic lift
export function _mlift_example2_10282(wild___0) /* forall<_a> (wild_@0 : ()) -> <bio,exit> int */  {
   
  var x_10380 = $std_core_hnd._open_at1(1, function(exitcode /* int */ ) {
       
      var ev_10382 = $std_core_hnd._evv_at(0);
      var _x1 = ev_10382.hnd._ctl_exit;
      return _x1(ev_10382.marker, ev_10382, exitcode);
    }, 1);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___0_0 /* _1330 */ ) {
      return _mlift_example2_10281(wild___0_0);
    });
  }
  else {
    return _mlift_example2_10281(x_10380);
  }
}
 
export function example2() /* () -> (int, string) */  {
  return bio(function() {
    return exit_fs__handle(_Hnd_exit(0, function(m /* hnd/marker<bio,int> */ , ___wildcard_x654__16 /* hnd/ev<exit> */ , x /* int */ ) {
          return $std_core_hnd.yield_to_final(m, function(___wildcard_x654__45 /* (hnd/resume-result<1278,int>) -> bio int */ ) {
              return x;
            });
        }), function(_res /* int */ ) {
        return _res;
      }, function() {
         
        var x_0_10386 = $std_core_hnd._open_at1(0, function(s /* string */ ) {
             
            var evx_10388 = $std_core_hnd._evv_at(0);
            return evx_10388.hnd._fun_write(evx_10388.marker, evx_10388, 0, s);
          }, "hi ");
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(wild___0 /* () */ ) {
            return _mlift_example2_10282(wild___0);
          });
        }
        else {
          return _mlift_example2_10282(x_0_10386);
        }
      });
  });
}
 
 
// Automatically generated. Tests for the `Root` constructor of the `:user` type.
export function is_root(user) /* (user : user) -> bool */  {
  return (user === 1);
}
 
 
// Automatically generated. Tests for the `Alice` constructor of the `:user` type.
export function is_alice(user) /* (user : user) -> bool */  {
  return (user === 2);
}
 
 
// Automatically generated. Tests for the `Bob` constructor of the `:user` type.
export function is_bob(user) /* (user : user) -> bool */  {
  return (user === 3);
}
 
export function user_fs_show(user) /* (user : user) -> string */  {
  if (user === 1) {
    return "root";
  }
  else if (user === 2) {
    return "alice";
  }
  else {
    return "bob";
  }
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:whoami` type.
export function whoami_fs__cfc(whoami_0) /* forall<e,a> (whoami : whoami<e,a>) -> int */  {
  return whoami_0._cfc;
}
 
 
// handler for the effect `:whoami`
export function whoami_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : whoami<e,b>, ret : (res : a) -> e b, action : () -> <whoami|e> a) -> e b */  {
  return $std_core_hnd._hhandle(whoami_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-whoami` constructor field of the `:whoami` type.
export function whoami_fs__fun_whoami(whoami_0) /* forall<e,a> (whoami : whoami<e,a>) -> hnd/clause0<string,whoami,e,a> */  {
  return whoami_0._fun_whoami;
}
 
 
// select `whoami` operation out of effect `:whoami`
export function whoami_fs__select(hnd) /* forall<e,a> (hnd : whoami<e,a>) -> hnd/clause0<string,whoami,e,a> */  {
  return hnd._fun_whoami;
}
 
 
// Call the `fun whoami` operation of the effect `:whoami`
export function whoami() /* () -> whoami string */  {
   
  var ev_10393 = $std_core_hnd._evv_at(0);
  return ev_10393.hnd._fun_whoami(ev_10393.marker, ev_10393);
}
 
export function intstr_fs_show(_pat_x100__18) /* ((int, string)) -> string */  {
  return $std_core_types._lp__plus__plus__rp_("exit with status ", $std_core_types._lp__plus__plus__rp_($std_core_int.show(_pat_x100__18.fst), $std_core_types._lp__plus__plus__rp_("\n", $std_core_show.string_fs_show(_pat_x100__18.snd))));
}
 
export function env(user, action) /* forall<a,e> (user : user, action : () -> <whoami|e> a) -> e a */  {
  return whoami_fs__handle(_Hnd_whoami(1, $std_core_hnd.clause_tail0(function() {
        if (user === 1) {
          return "root";
        }
        else if (user === 2) {
          return "alice";
        }
        else {
          return "bob";
        }
      })), function(_res /* 1894 */ ) {
      return _res;
    }, action);
}
 
 
// monadic lift
export function _mlift_example3_10283(_y_x10100) /* (string) -> <whoami,bio,exit> () */  {
  return $std_core_hnd._open_at1(0, function(s_1 /* string */ ) {
       
      var evx_10395 = $std_core_hnd._evv_at(0);
      return evx_10395.hnd._fun_write(evx_10395.marker, evx_10395, 0, s_1);
    }, _y_x10100);
}
 
 
// monadic lift
export function _mlift_example3_10284(wild___0) /* (wild_@0 : ()) -> <bio,whoami,exit> () */  {
   
  var x_10399 = $std_core_hnd._open_at0(2, function() {
       
      var ev_10401 = $std_core_hnd._evv_at(0);
      return ev_10401.hnd._fun_whoami(ev_10401.marker, ev_10401);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_example3_10283);
  }
  else {
    return _mlift_example3_10283(x_10399);
  }
}
 
 
// monadic lift
export function _mlift_example3_10285(wild__) /* (wild_ : ()) -> <bio,exit> int */  {
  return 0;
}
 
export function example3() /* () -> (int, string) */  {
  return bio(function() {
    return exit_fs__handle(_Hnd_exit(0, function(m /* hnd/marker<bio,int> */ , ___wildcard_x654__16 /* hnd/ev<exit> */ , x /* int */ ) {
          return $std_core_hnd.yield_to_final(m, function(___wildcard_x654__45 /* (hnd/resume-result<1278,int>) -> bio int */ ) {
              return x;
            });
        }), function(_res /* int */ ) {
        return _res;
      }, function() {
         
        var x_0_10404 = whoami_fs__handle(_Hnd_whoami(1, $std_core_hnd.clause_tail0(function() {
              return $std_core_hnd._open_none1(function(user_0 /* user */ ) {
                  if (user_0 === 1) {
                    return "root";
                  }
                  else if (user_0 === 2) {
                    return "alice";
                  }
                  else {
                    return "bob";
                  }
                }, Alice);
            })), function(_res_0 /* () */ ) {
            return _res_0;
          }, function() {
             
            var x_1_10406 = $std_core_hnd._open_at1(0, function(s /* string */ ) {
                 
                var evx_10408 = $std_core_hnd._evv_at(0);
                return evx_10408.hnd._fun_write(evx_10408.marker, evx_10408, 0, s);
              }, "hi ");
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(_mlift_example3_10284);
            }
            else {
              return _mlift_example3_10284(x_1_10406);
            }
          });
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(_mlift_example3_10285);
        }
        else {
          return 0;
        }
      });
  });
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:su` type.
export function su_fs__cfc(su_0) /* forall<e,a> (su : su<e,a>) -> int */  {
  return su_0._cfc;
}
 
 
// handler for the effect `:su`
export function su_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : su<e,b>, ret : (res : a) -> e b, action : () -> <su|e> a) -> e b */  {
  return $std_core_hnd._hhandle(su_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@ctl-su` constructor field of the `:su` type.
export function su_fs__ctl_su(su_0) /* forall<e,a> (su : su<e,a>) -> hnd/clause1<user,(),su,e,a> */  {
  return su_0._ctl_su;
}
 
 
// select `su` operation out of effect `:su`
export function su_fs__select(hnd) /* forall<e,a> (hnd : su<e,a>) -> hnd/clause1<user,(),su,e,a> */  {
  return hnd._ctl_su;
}
 
 
// Call the `ctl su` operation of the effect `:su`
export function su(u) /* (u : user) -> su () */  {
   
  var ev_10413 = $std_core_hnd._evv_at(0);
  return ev_10413.hnd._ctl_su(ev_10413.marker, ev_10413, u);
}
 
 
// monadic lift
export function _mlift_session_manager1_10286(resume, u, _y_x10106) /* forall<a,e> (resume : (()) -> <whoami|e> a, u : user, hnd/ev-index) -> <whoami|e> a */  {
  return $std_core_hnd._mask_at(_y_x10106, false, function() {
      return whoami_fs__handle(_Hnd_whoami(1, $std_core_hnd.clause_tail0(function() {
            if (u === 1) {
              return "root";
            }
            else if (u === 2) {
              return "alice";
            }
            else {
              return "bob";
            }
          })), function(_res_0 /* 2251 */ ) {
          return _res_0;
        }, function() {
          return resume($std_core_types.Unit);
        });
    });
}
 
export function session_manager1(initial_user, action) /* forall<a,e> (initial-user : user, action : () -> <su,whoami|e> a) -> e a */  {
  return whoami_fs__handle(_Hnd_whoami(1, $std_core_hnd.clause_tail0(function() {
        if (initial_user === 1) {
          return "root";
        }
        else if (initial_user === 2) {
          return "alice";
        }
        else {
          return "bob";
        }
      })), function(_res /* 2251 */ ) {
      return _res;
    }, function() {
      return su_fs__handle(_Hnd_su(3, function(m /* hnd/marker<<whoami|2252>,2251> */ , ___wildcard_x639__16 /* hnd/ev<su> */ , x /* user */ ) {
            return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<(),2251>) -> <whoami|2252> 2251 */ ) {
                return $std_core_hnd.protect(x, function(u /* user */ , resume /* (()) -> <whoami|2252> 2251 */ ) {
                     
                    var x_0_10417 = $std_core_hnd._evv_index(whoami_fs__tag);
                    if ($std_core_hnd._yielding()) {
                      return $std_core_hnd.yield_extend(function(_y_x10106 /* hnd/ev-index */ ) {
                        return _mlift_session_manager1_10286(resume, u, _y_x10106);
                      });
                    }
                    else {
                      return _mlift_session_manager1_10286(resume, u, x_0_10417);
                    }
                  }, k);
              });
          }), function(_res_1 /* 2251 */ ) {
          return _res_1;
        }, action);
    });
}
 
 
// monadic lift
export function _mlift_session_manager2_10287(resume, _y_x10112) /* forall<a,e> (resume : (()) -> <whoami|e> a, hnd/ev-index) -> <whoami,whoami|e> a */  {
  return $std_core_hnd._mask_at(_y_x10112, true, function() {
      return resume($std_core_types.Unit);
    });
}
 
export function session_manager2(initial_user, action) /* forall<a,e> (initial-user : user, action : () -> <su,whoami|e> a) -> e a */  {
  return whoami_fs__handle(_Hnd_whoami(1, $std_core_hnd.clause_tail0(function() {
        if (initial_user === 1) {
          return "root";
        }
        else if (initial_user === 2) {
          return "alice";
        }
        else {
          return "bob";
        }
      })), function(_res /* 2599 */ ) {
      return _res;
    }, function() {
      return su_fs__handle(_Hnd_su(3, function(m /* hnd/marker<<whoami|2600>,2599> */ , ___wildcard_x639__16 /* hnd/ev<su> */ , x /* user */ ) {
            return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<(),2599>) -> <whoami|2600> 2599 */ ) {
                return $std_core_hnd.protect(x, function(u /* user */ , resume /* (()) -> <whoami|2600> 2599 */ ) {
                    return whoami_fs__handle(_Hnd_whoami(1, $std_core_hnd.clause_tail0(function() {
                          return $std_core_hnd._open_none1(function(user_0 /* user */ ) {
                              if (user_0 === 1) {
                                return "root";
                              }
                              else if (user_0 === 2) {
                                return "alice";
                              }
                              else {
                                return "bob";
                              }
                            }, u);
                        })), function(_res_0 /* 2599 */ ) {
                        return _res_0;
                      }, function() {
                         
                        var x_0_10420 = $std_core_hnd._evv_index(whoami_fs__tag);
                         
                        function next_10421(_y_x10112) /* (hnd/ev-index) -> <whoami,whoami|2600> 2599 */  {
                          return $std_core_hnd._mask_at(_y_x10112, true, function() {
                              return resume($std_core_types.Unit);
                            });
                        }
                        if ($std_core_hnd._yielding()) {
                          return $std_core_hnd.yield_extend(next_10421);
                        }
                        else {
                          return next_10421(x_0_10420);
                        }
                      });
                  }, k);
              });
          }), function(_res_1 /* 2599 */ ) {
          return _res_1;
        }, action);
    });
}
 
 
// monadic lift
export function _mlift_session_manager3_10288(_y_x10118) /* forall<h,e> (user) -> <local<h>|e> string */  {
  if (_y_x10118 === 1) {
    return "root";
  }
  else if (_y_x10118 === 2) {
    return "alice";
  }
  else {
    return "bob";
  }
}
 
export function session_manager3(initial_user, action) /* forall<a,e> (initial-user : user, action : () -> <su,whoami|e> a) -> e a */  {
  return function() {
     
    var loc = { value: initial_user };
     
    var res = whoami_fs__handle(_Hnd_whoami(1, $std_core_hnd.clause_tail0(function() {
           
          var x_10426 = ((loc).value);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10118 /* user */ ) {
              if (_y_x10118 === 1) {
                return "root";
              }
              else if (_y_x10118 === 2) {
                return "alice";
              }
              else {
                return "bob";
              }
            });
          }
          else {
            if (x_10426 === 1) {
              return "root";
            }
            else if (x_10426 === 2) {
              return "alice";
            }
            else {
              return "bob";
            }
          }
        })), function(_res /* 2879 */ ) {
        return _res;
      }, function() {
        return su_fs__handle(_Hnd_su(1, $std_core_hnd.clause_tail1(function(u /* user */ ) {
              return ((loc).value = u);
            })), function(_res_0 /* 2879 */ ) {
            return _res_0;
          }, action);
      });
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// monadic lift
export function _mlift_example4_10289(_y_x10137) /* (string) -> <whoami,bio,su,exit> () */  {
  return $std_core_hnd._open_at1(0, function(s_5 /* string */ ) {
       
      var evx_10430 = $std_core_hnd._evv_at(0);
      return evx_10430.hnd._fun_write(evx_10430.marker, evx_10430, 0, s_5);
    }, _y_x10137);
}
 
 
// monadic lift
export function _mlift_example4_10290(wild___2) /* (wild_@2 : ()) -> <bio,whoami,su,exit> () */  {
   
  var x_10434 = $std_core_hnd._open_at0(3, function() {
       
      var ev_10436 = $std_core_hnd._evv_at(0);
      return ev_10436.hnd._fun_whoami(ev_10436.marker, ev_10436);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_example4_10289);
  }
  else {
    return _mlift_example4_10289(x_10434);
  }
}
 
 
// monadic lift
export function _mlift_example4_10291(wild___1) /* (wild_@1 : ()) -> <su,bio,whoami,exit> () */  {
   
  var x_10438 = $std_core_hnd._open_at1(0, function(s_3 /* string */ ) {
       
      var evx_10440 = $std_core_hnd._evv_at(0);
      return evx_10440.hnd._fun_write(evx_10440.marker, evx_10440, 0, s_3);
    }, ", and hi ");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_example4_10290);
  }
  else {
    return _mlift_example4_10290(x_10438);
  }
}
 
 
// monadic lift
export function _mlift_example4_10292(wild___0_0) /* (wild_@0@0 : ()) -> <bio,whoami,su,exit> () */  {
   
  var x_10444 = $std_core_hnd._open_at1(2, function(u /* user */ ) {
       
      var ev_10446 = $std_core_hnd._evv_at(0);
      return ev_10446.hnd._ctl_su(ev_10446.marker, ev_10446, u);
    }, Alice);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_example4_10291);
  }
  else {
    return _mlift_example4_10291(x_10444);
  }
}
 
 
// monadic lift
export function _mlift_example4_10293(_y_x10129) /* (string) -> <whoami,bio,su,exit> () */  {
   
  var x_10449 = $std_core_hnd._open_at1(0, function(s_1 /* string */ ) {
       
      var evx_10451 = $std_core_hnd._evv_at(0);
      return evx_10451.hnd._fun_write(evx_10451.marker, evx_10451, 0, s_1);
    }, _y_x10129);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_example4_10292);
  }
  else {
    return _mlift_example4_10292(x_10449);
  }
}
 
 
// monadic lift
export function _mlift_example4_10294(wild___0) /* (wild_@0 : ()) -> <bio,su,whoami,exit> () */  {
   
  var x_10455 = $std_core_hnd._open_at0(3, function() {
       
      var ev_10457 = $std_core_hnd._evv_at(0);
      return ev_10457.hnd._fun_whoami(ev_10457.marker, ev_10457);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_example4_10293);
  }
  else {
    return _mlift_example4_10293(x_10455);
  }
}
 
 
// monadic lift
export function _mlift_example4_10295(wild__) /* (wild_ : ()) -> <bio,exit> int */  {
  return 0;
}
 
export function example4() /* () -> (int, string) */  {
  return bio(function() {
    return exit_fs__handle(_Hnd_exit(0, function(m /* hnd/marker<bio,int> */ , ___wildcard_x654__16 /* hnd/ev<exit> */ , x /* int */ ) {
          return $std_core_hnd.yield_to_final(m, function(___wildcard_x654__45 /* (hnd/resume-result<1278,int>) -> bio int */ ) {
              return x;
            });
        }), function(_res /* int */ ) {
        return _res;
      }, function() {
         
        var x_0_10460 = session_manager3(Root, function() {
             
            var x_1_10462 = $std_core_hnd._open_at1(0, function(s /* string */ ) {
                 
                var evx_10464 = $std_core_hnd._evv_at(0);
                return evx_10464.hnd._fun_write(evx_10464.marker, evx_10464, 0, s);
              }, "hi ");
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(_mlift_example4_10294);
            }
            else {
              return _mlift_example4_10294(x_1_10462);
            }
          });
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(_mlift_example4_10295);
        }
        else {
          return 0;
        }
      });
  });
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:fork` type.
export function fork_fs__cfc(fork_0) /* forall<e,a> (fork : fork<e,a>) -> int */  {
  return fork_0._cfc;
}
 
 
// handler for the effect `:fork`
export function fork_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : fork<e,b>, ret : (res : a) -> e b, action : () -> <fork|e> a) -> e b */  {
  return $std_core_hnd._hhandle(fork_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@ctl-fork` constructor field of the `:fork` type.
export function fork_fs__ctl_fork(fork_0) /* forall<e,a> (fork : fork<e,a>) -> hnd/clause0<bool,fork,e,a> */  {
  return fork_0._ctl_fork;
}
 
 
// select `fork` operation out of effect `:fork`
export function fork_fs__select(hnd) /* forall<e,a> (hnd : fork<e,a>) -> hnd/clause0<bool,fork,e,a> */  {
  return hnd._ctl_fork;
}
 
 
// Call the `ctl fork` operation of the effect `:fork`
export function fork() /* () -> fork bool */  {
   
  var ev_10469 = $std_core_hnd._evv_at(0);
  return ev_10469.hnd._ctl_fork(ev_10469.marker, ev_10469);
}
 
 
// monadic lift
export function _mlift_forking_10296(_y_x10143, _y_x10144) /* forall<a,e> (list<a>, list<a>) -> e list<a> */  {
  return $std_core_list.append(_y_x10143, _y_x10144);
}
 
 
// monadic lift
export function _mlift_forking_10297(resume, _y_x10143) /* forall<a,e> (resume : (bool) -> e list<a>, list<a>) -> e list<a> */  {
   
  var x_10471 = resume(false);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10144 /* list<3265> */ ) {
      return $std_core_list.append(_y_x10143, _y_x10144);
    });
  }
  else {
    return $std_core_list.append(_y_x10143, x_10471);
  }
}
 
export function forking(action) /* forall<a,e> (action : () -> <fork|e> a) -> e list<a> */  {
  return fork_fs__handle(_Hnd_fork(3, function(m /* hnd/marker<3266,list<3265>> */ , ___wildcard_x688__16 /* hnd/ev<fork> */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<bool,list<3265>>) -> 3266 list<3265> */ ) {
            return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (bool) -> 3266 list<3265> */ ) {
                 
                var x_10476 = r(true);
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(function(_y_x10143 /* list<3265> */ ) {
                    return _mlift_forking_10297(r, _y_x10143);
                  });
                }
                else {
                  return _mlift_forking_10297(r, x_10476);
                }
              }, k);
          });
      }), function(x_0 /* 3265 */ ) {
      return $std_core_types.Cons(x_0, $std_core_types.Nil);
    }, action);
}
 
 
// Automatically generated. Tests for the `Done` constructor of the `:pstate` type.
export function is_done(pstate) /* forall<a,e> (pstate : pstate<e,a>) -> bool */  {
  return (pstate._tag === 1);
}
 
 
// Automatically generated. Tests for the `Paused` constructor of the `:pstate` type.
export function is_paused(pstate) /* forall<a,e> (pstate : pstate<e,a>) -> bool */  {
  return (pstate._tag === 2);
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:interrupt` type.
export function interrupt_fs__cfc(interrupt_0) /* forall<e,a> (interrupt : interrupt<e,a>) -> int */  {
  return interrupt_0._cfc;
}
 
 
// handler for the effect `:interrupt`
export function interrupt_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : interrupt<e,b>, ret : (res : a) -> e b, action : () -> <interrupt|e> a) -> e b */  {
  return $std_core_hnd._hhandle(interrupt_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@ctl-interrupt` constructor field of the `:interrupt` type.
export function interrupt_fs__ctl_interrupt(interrupt_0) /* forall<e,a> (interrupt : interrupt<e,a>) -> hnd/clause0<(),interrupt,e,a> */  {
  return interrupt_0._ctl_interrupt;
}
 
 
// select `interrupt` operation out of effect `:interrupt`
export function interrupt_fs__select(hnd) /* forall<e,a> (hnd : interrupt<e,a>) -> hnd/clause0<(),interrupt,e,a> */  {
  return hnd._ctl_interrupt;
}
 
 
// Call the `ctl interrupt` operation of the effect `:interrupt`
export function interrupt() /* () -> interrupt () */  {
   
  var ev_10479 = $std_core_hnd._evv_at(0);
  return ev_10479.hnd._ctl_interrupt(ev_10479.marker, ev_10479);
}
 
 
// monadic lift
export function _mlift_reify_process_10298(_y_x10149) /* forall<a,e> (a) -> <interrupt|e> pstate<e,a> */  {
  return Done(_y_x10149);
}
 
export function reify_process(action) /* forall<a,e> (action : () -> <interrupt|e> a) -> e pstate<e,a> */  {
  return interrupt_fs__handle(_Hnd_interrupt(3, function(m /* hnd/marker<3620,pstate<3620,3619>> */ , ___wildcard_x680__16 /* hnd/ev<interrupt> */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<(),pstate<3620,3619>>) -> 3620 pstate<3620,3619> */ ) {
            return Paused(function() {
              return k($std_core_hnd.Deep($std_core_types.Unit));
            });
          });
      }), function(_res /* pstate<3620,3619> */ ) {
      return _res;
    }, function() {
       
      var x_0_10484 = action();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10149 /* 3619 */ ) {
          return Done(_y_x10149);
        });
      }
      else {
        return Done(x_0_10484);
      }
    });
}
 
 
// monadic lift
export function _mlift_lift_scheduler_4384_10299(dones, pp_0, ps) /* forall<a,e> (dones : list<a>, pp@0 : list<pstate<<fork,div|e>,a>>, ps : list<pstate<<div,fork|e>,a>>) -> <div|e> list<a> */  {
  return _lift_scheduler_4384($std_core_list.append(pp_0, ps), dones);
}
 
 
// lifted local: scheduler, schedule
export function _lift_scheduler_4384(todos, dones_0) /* forall<a,e> (todos : list<pstate<<fork,div|e>,a>>, dones : list<a>) -> <div|e> list<a> */  { tailcall: while(1)
{
  if (todos === null) {
    return dones_0;
  }
  else if (todos !== null && todos.head._tag === 1) {
    {
      // tail call
      var _x1 = $std_core_types.Cons(todos.head.result, dones_0);
      todos = todos.tail;
      dones_0 = _x1;
      continue tailcall;
    }
  }
  else {
     
    var x_0_10487 = forking(todos.head.resumption);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(ps_0 /* list<pstate<<div,fork|3733>,3732>> */ ) {
        return _mlift_lift_scheduler_4384_10299(dones_0, todos.tail, ps_0);
      });
    }
    else {
      {
        // tail call
        var _x2 = $std_core_list.append(todos.tail, x_0_10487);
        todos = _x2;
        continue tailcall;
      }
    }
  }
}}
 
export function scheduler(pstates) /* forall<a,e> (pstates : list<pstate<<fork,div|e>,a>>) -> <div|e> list<a> */  {
  return _lift_scheduler_4384(pstates, $std_core_types.Nil);
}
 
export function timeshare(action) /* forall<a,e> (action : () -> <fork,interrupt,div|e> a) -> <div|e> list<a> */  {
  return _lift_scheduler_4384($std_core_types.Cons(Paused(function() {
        return reify_process(action);
      }), $std_core_types.Nil), $std_core_types.Nil);
}
 
 
// monadic lift
export function _mlift_ritchie_10300(wild___1) /* forall<e> (wild_@1 : ()) -> <bio|e> () */  {
  return $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s_5 /* string */ ) {
       
      var evx_10490 = $std_core_hnd._evv_at(0);
      return evx_10490.hnd._fun_write(evx_10490.marker, evx_10490, 0, s_5);
    }, "you have to be a genius to understand the simplicity.\n");
}
 
 
// monadic lift
export function _mlift_ritchie_10301(wild___0) /* forall<e> (wild_@0 : ()) -> <bio|e> () */  {
   
  var x_10494 = $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s_3 /* string */ ) {
       
      var evx_10496 = $std_core_hnd._evv_at(0);
      return evx_10496.hnd._fun_write(evx_10496.marker, evx_10496, 0, s_3);
    }, "but ");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___1 /* () */ ) {
      return _mlift_ritchie_10300(wild___1);
    });
  }
  else {
    return _mlift_ritchie_10300(x_10494);
  }
}
 
 
// monadic lift
export function _mlift_ritchie_10302(wild__) /* forall<e> (wild_ : ()) -> <bio|e> () */  {
   
  var x_10500 = $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s_1 /* string */ ) {
       
      var evx_10502 = $std_core_hnd._evv_at(0);
      return evx_10502.hnd._fun_write(evx_10502.marker, evx_10502, 0, s_1);
    }, "a simple operating system, ");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___0 /* () */ ) {
      return _mlift_ritchie_10301(wild___0);
    });
  }
  else {
    return _mlift_ritchie_10301(x_10500);
  }
}
 
export function ritchie() /* forall<e> () -> <bio|e> () */  {
   
  var x_10506 = $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s /* string */ ) {
       
      var evx_10509 = $std_core_hnd._evv_at(0);
      return evx_10509.hnd._fun_write(evx_10509.marker, evx_10509, 0, s);
    }, "UNIX is basically ");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return _mlift_ritchie_10302(wild__);
    });
  }
  else {
     
    var x_1_10513 = $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s_1 /* string */ ) {
         
        var evx_0_10516 = $std_core_hnd._evv_at(0);
        return evx_0_10516.hnd._fun_write(evx_0_10516.marker, evx_0_10516, 0, s_1);
      }, "a simple operating system, ");
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___0 /* () */ ) {
        return _mlift_ritchie_10301(wild___0);
      });
    }
    else {
       
      var x_3_10520 = $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s_3 /* string */ ) {
           
          var evx_1_10523 = $std_core_hnd._evv_at(0);
          return evx_1_10523.hnd._fun_write(evx_1_10523.marker, evx_1_10523, 0, s_3);
        }, "but ");
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(wild___1 /* () */ ) {
          return _mlift_ritchie_10300(wild___1);
        });
      }
      else {
        return $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s_5 /* string */ ) {
             
            var evx_2_10527 = $std_core_hnd._evv_at(0);
            return evx_2_10527.hnd._fun_write(evx_2_10527.marker, evx_2_10527, 0, s_5);
          }, "you have to be a genius to understand the simplicity.\n");
      }
    }
  }
}
 
 
// monadic lift
export function _mlift_hamlet_10303(wild___0) /* forall<e> (wild_@0 : ()) -> <bio|e> () */  {
  return $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s_3 /* string */ ) {
       
      var evx_10531 = $std_core_hnd._evv_at(0);
      return evx_10531.hnd._fun_write(evx_10531.marker, evx_10531, 0, s_3);
    }, "Whether \'tis nobler in the mind to suffer\n");
}
 
 
// monadic lift
export function _mlift_hamlet_10304(wild__) /* forall<e> (wild_ : ()) -> <bio|e> () */  {
   
  var x_10535 = $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s_1 /* string */ ) {
       
      var evx_10537 = $std_core_hnd._evv_at(0);
      return evx_10537.hnd._fun_write(evx_10537.marker, evx_10537, 0, s_1);
    }, "that is the question:\n");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___0 /* () */ ) {
      return _mlift_hamlet_10303(wild___0);
    });
  }
  else {
    return _mlift_hamlet_10303(x_10535);
  }
}
 
export function hamlet() /* forall<e> () -> <bio|e> () */  {
   
  var x_10541 = $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s /* string */ ) {
       
      var evx_10544 = $std_core_hnd._evv_at(0);
      return evx_10544.hnd._fun_write(evx_10544.marker, evx_10544, 0, s);
    }, "To be, or not to be, ");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return _mlift_hamlet_10304(wild__);
    });
  }
  else {
     
    var x_1_10548 = $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s_1 /* string */ ) {
         
        var evx_0_10551 = $std_core_hnd._evv_at(0);
        return evx_0_10551.hnd._fun_write(evx_0_10551.marker, evx_0_10551, 0, s_1);
      }, "that is the question:\n");
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___0 /* () */ ) {
        return _mlift_hamlet_10303(wild___0);
      });
    }
    else {
      return $std_core_hnd._open_at1($std_core_hnd._evv_index(bio_fs__tag), function(s_3 /* string */ ) {
           
          var evx_1_10555 = $std_core_hnd._evv_at(0);
          return evx_1_10555.hnd._fun_write(evx_1_10555.marker, evx_1_10555, 0, s_3);
        }, "Whether \'tis nobler in the mind to suffer\n");
    }
  }
}
 
 
// monadic lift
export function _mlift_example5_10305(wild___0) /* (wild_@0 : ()) -> <su,bio,fork,whoami,exit,div,interrupt,console/console> () */  {
  return ritchie();
}
 
 
// monadic lift
export function _mlift_example5_10306(wild___0_0) /* (wild_@0@0 : ()) -> <su,bio,fork,whoami,exit,div,interrupt,console/console> () */  {
  return hamlet();
}
 
 
// monadic lift
export function _mlift_example5_10307(_y_x10174) /* (bool) -> <fork,bio,su,whoami,exit,div,interrupt,console/console> () */  {
  if (_y_x10174) {
     
    var x_10559 = $std_core_hnd._open_at1(4, function(u /* user */ ) {
         
        var ev_10561 = $std_core_hnd._evv_at(0);
        return ev_10561.hnd._ctl_su(ev_10561.marker, ev_10561, u);
      }, Alice);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_mlift_example5_10305);
    }
    else {
      return ritchie();
    }
  }
  else {
     
    var x_1_10564 = $std_core_hnd._open_at1(4, function(u_0 /* user */ ) {
         
        var ev_0_10566 = $std_core_hnd._evv_at(0);
        return ev_0_10566.hnd._ctl_su(ev_0_10566.marker, ev_0_10566, u_0);
      }, Bob);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_mlift_example5_10306);
    }
    else {
      return hamlet();
    }
  }
}
 
 
// monadic lift
export function _mlift_example5_10308(wild__) /* (wild_ : ()) -> <bio,fork,exit,div,interrupt,console/console> int */  {
  return 0;
}
 
 
// monadic lift
export function _mlift_example5_10309(_y_x10183) /* (int) -> <bio,fork,div,interrupt,console/console> pstate<<div,fork,bio,console/console>,int> */  {
  return Done(_y_x10183);
}
 
export function example5() /* () -> <console/console,div> () */  {
   
  var x_4379 = bio(function() {
    return _lift_scheduler_4384($std_core_types.Cons(Paused(function() {
          return interrupt_fs__handle(_Hnd_interrupt(3, function(m /* hnd/marker<<div,fork,bio,console/console>,pstate<<div,fork,bio,console/console>,int>> */ , ___wildcard_x680__16 /* hnd/ev<interrupt> */ ) {
                return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<(),pstate<<div,fork,bio,console/console>,int>>) -> <div,fork,bio,console/console> pstate<<div,fork,bio,console/console>,int> */ ) {
                    return Paused(function() {
                      return k($std_core_hnd.Deep($std_core_types.Unit));
                    });
                  });
              }), function(_res /* pstate<<div,fork,bio,console/console>,int> */ ) {
              return _res;
            }, function() {
               
              var x_0_10572 = exit_fs__handle(_Hnd_exit(0, function(m_0 /* hnd/marker<<bio,fork,div,interrupt,console/console>,int> */ , ___wildcard_x654__16 /* hnd/ev<exit> */ , x_1 /* int */ ) {
                    return $std_core_hnd.yield_to_final(m_0, function(___wildcard_x654__45 /* (hnd/resume-result<1278,int>) -> <bio,fork,div,interrupt,console/console> int */ ) {
                        return x_1;
                      });
                  }), function(_res_0 /* int */ ) {
                  return _res_0;
                }, function() {
                   
                  var x_2_10575 = session_manager3(Root, function() {
                       
                      var x_3_10577 = $std_core_hnd._open_at0(2, function() {
                           
                          var ev_10579 = $std_core_hnd._evv_at(0);
                          return ev_10579.hnd._ctl_fork(ev_10579.marker, ev_10579);
                        });
                      if ($std_core_hnd._yielding()) {
                        return $std_core_hnd.yield_extend(_mlift_example5_10307);
                      }
                      else {
                        return _mlift_example5_10307(x_3_10577);
                      }
                    });
                  if ($std_core_hnd._yielding()) {
                    return $std_core_hnd.yield_extend(_mlift_example5_10308);
                  }
                  else {
                    return 0;
                  }
                });
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(_mlift_example5_10309);
              }
              else {
                return Done(x_0_10572);
              }
            });
        }), $std_core_types.Nil), $std_core_types.Nil);
  });
  var _x3 = x_4379.snd;
  return $std_core_console.printsln(_x3);
}
 
 
// monadic lift
export function _mlift_interrupt_write_10310(fd, s, wild__) /* forall<e> (fd : filedesc, s : string, wild_ : ()) -> <interrupt,bio|e> () */  {
  return $std_core_hnd._open_at2($std_core_hnd._evv_index(bio_fs__tag), function(fd_0 /* filedesc */ , s_0 /* string */ ) {
       
      var evx_10581 = $std_core_hnd._evv_at(0);
      return evx_10581.hnd._fun_write(evx_10581.marker, evx_10581, fd_0, s_0);
    }, fd, s);
}
 
 
// monadic lift
export function _mlift_interrupt_write_10311(action, _y_x10190) /* forall<a,e> (action : () -> <bio,interrupt|e> a, hnd/ev-index) -> <bio,bio,interrupt|e> a */  {
  return $std_core_hnd._mask_at(_y_x10190, true, action);
}
 
export function interrupt_write(action) /* forall<a,e> (action : () -> <bio,interrupt|e> a) -> <bio,interrupt|e> a */  {
  return bio_fs__handle(_Hnd_bio(1, function(m /* hnd/marker<<bio,interrupt|4160>,4159> */ , ev /* hnd/ev<bio> */ , x1 /* filedesc */ , x2 /* string */ ) {
        return $std_core_hnd.under2(ev, function(fd /* filedesc */ , s /* string */ ) {
             
            var x_10586 = $std_core_hnd._open_at0($std_core_hnd._evv_index(interrupt_fs__tag), function() {
                 
                var ev_0_10588 = $std_core_hnd._evv_at(0);
                return ev_0_10588.hnd._ctl_interrupt(ev_0_10588.marker, ev_0_10588);
              });
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
                return _mlift_interrupt_write_10310(fd, s, wild__);
              });
            }
            else {
              return _mlift_interrupt_write_10310(fd, s, x_10586);
            }
          }, x1, x2);
      }), function(_res /* 4159 */ ) {
      return _res;
    }, function() {
       
      var x_0_10590 = $std_core_hnd._evv_index(bio_fs__tag);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10190 /* hnd/ev-index */ ) {
          return $std_core_hnd._mask_at(_y_x10190, true, action);
        });
      }
      else {
        return $std_core_hnd._mask_at(x_0_10590, true, action);
      }
    });
}
 
 
// monadic lift
export function _mlift_example6_10312(wild___0) /* (wild_@0 : ()) -> <su,bio,fork,whoami,exit,interrupt,div,console/console> () */  {
  return ritchie();
}
 
 
// monadic lift
export function _mlift_example6_10313(wild___0_0) /* (wild_@0@0 : ()) -> <su,bio,fork,whoami,exit,interrupt,div,console/console> () */  {
  return hamlet();
}
 
 
// monadic lift
export function _mlift_example6_10314(_y_x10195) /* (bool) -> <fork,bio,su,whoami,exit,interrupt,div,console/console> () */  {
  if (_y_x10195) {
     
    var x_10594 = $std_core_hnd._open_at1(4, function(u /* user */ ) {
         
        var ev_10596 = $std_core_hnd._evv_at(0);
        return ev_10596.hnd._ctl_su(ev_10596.marker, ev_10596, u);
      }, Alice);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_mlift_example6_10312);
    }
    else {
      return ritchie();
    }
  }
  else {
     
    var x_1_10599 = $std_core_hnd._open_at1(4, function(u_0 /* user */ ) {
         
        var ev_0_10601 = $std_core_hnd._evv_at(0);
        return ev_0_10601.hnd._ctl_su(ev_0_10601.marker, ev_0_10601, u_0);
      }, Bob);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_mlift_example6_10313);
    }
    else {
      return hamlet();
    }
  }
}
 
 
// monadic lift
export function _mlift_example6_10315(wild__) /* (wild_ : ()) -> <bio,fork,exit,interrupt,div,console/console> int */  {
  return 0;
}
 
 
// monadic lift
export function _mlift_example6_10316(_y_x10205) /* (int) -> <bio,interrupt,fork,div,console/console> pstate<<div,fork,bio,console/console>,int> */  {
  return Done(_y_x10205);
}
 
export function example6() /* () -> <console/console,div> () */  {
   
  var x_4383 = bio(function() {
    return _lift_scheduler_4384($std_core_types.Cons(Paused(function() {
          return interrupt_fs__handle(_Hnd_interrupt(3, function(m /* hnd/marker<<div,fork,bio,console/console>,pstate<<div,fork,bio,console/console>,int>> */ , ___wildcard_x680__16 /* hnd/ev<interrupt> */ ) {
                return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<(),pstate<<div,fork,bio,console/console>,int>>) -> <div,fork,bio,console/console> pstate<<div,fork,bio,console/console>,int> */ ) {
                    return Paused(function() {
                      return k($std_core_hnd.Deep($std_core_types.Unit));
                    });
                  });
              }), function(_res /* pstate<<div,fork,bio,console/console>,int> */ ) {
              return _res;
            }, function() {
               
              var x_0_10607 = interrupt_write(function() {
                return exit_fs__handle(_Hnd_exit(0, function(m_0 /* hnd/marker<<bio,fork,interrupt,div,console/console>,int> */ , ___wildcard_x654__16 /* hnd/ev<exit> */ , x_1 /* int */ ) {
                      return $std_core_hnd.yield_to_final(m_0, function(___wildcard_x654__45 /* (hnd/resume-result<1278,int>) -> <bio,fork,interrupt,div,console/console> int */ ) {
                          return x_1;
                        });
                    }), function(_res_0 /* int */ ) {
                    return _res_0;
                  }, function() {
                     
                    var x_2_10610 = session_manager3(Root, function() {
                         
                        var x_3_10612 = $std_core_hnd._open_at0(2, function() {
                             
                            var ev_10614 = $std_core_hnd._evv_at(0);
                            return ev_10614.hnd._ctl_fork(ev_10614.marker, ev_10614);
                          });
                        if ($std_core_hnd._yielding()) {
                          return $std_core_hnd.yield_extend(_mlift_example6_10314);
                        }
                        else {
                          return _mlift_example6_10314(x_3_10612);
                        }
                      });
                    if ($std_core_hnd._yielding()) {
                      return $std_core_hnd.yield_extend(_mlift_example6_10315);
                    }
                    else {
                      return 0;
                    }
                  });
              });
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(_mlift_example6_10316);
              }
              else {
                return Done(x_0_10607);
              }
            });
        }), $std_core_types.Nil), $std_core_types.Nil);
  });
  var _x4 = x_4383.snd;
  return $std_core_console.printsln(_x4);
}
 
export function main() /* () -> <console/console,div> () */  {
  return example5();
}
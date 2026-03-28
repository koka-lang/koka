// Koka generated module: handlers/named/file-scoped, koka version: 3.2.4
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
import * as $std_os_path from './std_os_path.mjs';
import * as $std_os_file from './std_os_file.mjs';
 
// externals
 
// type declarations
 
 
// runtime tag for the effect `:file`
export var file_fs__tag;
var file_fs__tag = "file@file-scoped";
// type file
export function _Hnd_file(_cfc, _fun_read_line) /* forall<s,e,a> (int, hnd/clause0<string,file<s>,e,a>) -> file<s,e,a> */  {
  return { _cfc: _cfc, _fun_read_line: _fun_read_line };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:file` type.
export function file_fs__cfc(file_0) /* forall<s,e,a> (file : file<s,e,a>) -> int */  {
  return file_0._cfc;
}
 
 
// handler for the effect `:file`
export function file_fs__handle(hnd, ret, action) /* forall<s,a,e,b> (hnd : file<s,e,b>, ret : (res : a) -> e b, action : forall<s1> (hname : hnd/ev<file<s1>>) -> <file<s1>|e> a) -> e b */  {
  return $std_core_hnd._named_handle(file_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-read-line` constructor field of the `:file` type.
export function file_fs__fun_read_line(file_0) /* forall<s,e,a> (file : file<s,e,a>) -> hnd/clause0<string,file<s>,e,a> */  {
  return file_0._fun_read_line;
}
 
 
// select `read-line` operation out of effect `:file`
export function read_line_fs__select(hnd) /* forall<s,e,a> (hnd : file<s,e,a>) -> hnd/clause0<string,file<s>,e,a> */  {
  return hnd._fun_read_line;
}
 
 
// Call the `fun read-line` operation of the effect `:file`
export function read_line(_hname) /* forall<s> (hnd/ev<file<s>>) -> (file<s>) string */  {
  return _hname.hnd._fun_read_line(_hname.marker, _hname);
}
 
 
// monadic lift
export function _mlift_file_10021(fname, _y_x10005) /* forall<h,e> (fname : string, int) -> <local<h>|e> string */  {
  return $std_core_types._lp__plus__plus__rp_(fname, $std_core_types._lp__plus__plus__rp_(": line ", $std_core_int.show(_y_x10005)));
}
 
 
// monadic lift
export function _mlift_file_10022(fname, i, wild__) /* forall<h,e> (fname : string, i : local-var<h,int>, wild_ : ()) -> <local<h>|e> string */  {
   
  var x_10029 = ((i).value);
   
  function next_10030(_y_x10005) /* (int) -> <local<604>|616> string */  {
    return $std_core_types._lp__plus__plus__rp_(fname, $std_core_types._lp__plus__plus__rp_(": line ", $std_core_int.show(_y_x10005)));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10030);
  }
  else {
    return next_10030(x_10029);
  }
}
 
 
// monadic lift
export function _mlift_file_10023(fname, i, _y_x10003) /* forall<h,e> (fname : string, i : local-var<h,int>, int) -> <local<h>|e> string */  {
   
  var x_10033 = ((i).value = ($std_core_types._int_add(_y_x10003,1)));
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return _mlift_file_10022(fname, i, wild__);
    });
  }
  else {
    return _mlift_file_10022(fname, i, x_10033);
  }
}
 
 
// a handler instance for files
export function file(fname, action) /* forall<a,e> (fname : string, action : forall<s> (hnd/ev<file<s>>) -> <file<s>|e> a) -> e a */  {
  return function() {
     
    var loc = { value: 0 };
     
    var res = file_fs__handle(_Hnd_file(1, $std_core_hnd.clause_tail0(function() {
           
          var x_10037 = ((loc).value);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10003 /* int */ ) {
              return _mlift_file_10023(fname, loc, _y_x10003);
            });
          }
          else {
            return _mlift_file_10023(fname, loc, x_10037);
          }
        })), function(_res /* 615 */ ) {
        return _res;
      }, function(f /* hnd/ev<file<597>> */ ) {
        return action(f);
      });
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// monadic lift
export function _mlift_read_both_10024(_y_x10012, _y_x10014) /* forall<e,s,s1> (string, string) -> <file<s1>,file<s>,console/console|e> () */  {
  return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(_y_x10012, $std_core_types._lp__plus__plus__rp_("\n", _y_x10014)));
}
 
 
// monadic lift
export function _mlift_read_both_10025(f2, _y_x10012) /* forall<e,s,s1> (f2 : hnd/ev<file<s1>>, string) -> <file<s>,file<s1>,console/console|e> () */  {
   
  var x_10040 = f2.hnd._fun_read_line(f2.marker, f2);
   
  function next_10041(_y_x10014) /* (string) -> <file<724>,file<723>,console/console|722> () */  {
    return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(_y_x10012, $std_core_types._lp__plus__plus__rp_("\n", _y_x10014)));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10041);
  }
  else {
    return next_10041(x_10040);
  }
}
 
export function read_both(f1, f2) /* forall<e,s,s1> (f1 : hnd/ev<file<s>>, f2 : hnd/ev<file<s1>>) -> <console/console,file<s>,file<s1>|e> () */  {
   
  var x_10046 = f1.hnd._fun_read_line(f1.marker, f1);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10012 /* string */ ) {
      return _mlift_read_both_10025(f2, _y_x10012);
    });
  }
  else {
     
    var x_0_10051 = f2.hnd._fun_read_line(f2.marker, f2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10014 /* string */ ) {
        return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(x_10046, $std_core_types._lp__plus__plus__rp_("\n", _y_x10014)));
      });
    }
    else {
      return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(x_10046, $std_core_types._lp__plus__plus__rp_("\n", x_0_10051)));
    }
  }
}
 
export function main() /* () -> console/console () */  {
  return file("package.yaml", function(f1 /* hnd/ev<file<785>> */ ) {
      return file("stack.yaml", function(f2 /* hnd/ev<file<778>> */ ) {
          return read_both(f1, f2);
        });
    });
}
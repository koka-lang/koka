// Koka generated module: handlers/named/file, koka version: 3.2.4
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
var file_fs__tag = "file@file";
// type file
export function _Hnd_file(_cfc, _fun_read_line) /* forall<e,a> (int, hnd/clause0<string,file,e,a>) -> file<e,a> */  {
  return { _cfc: _cfc, _fun_read_line: _fun_read_line };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:file` type.
export function file_fs__cfc(file_0) /* forall<e,a> (file : file<e,a>) -> int */  {
  return file_0._cfc;
}
 
 
// handler for the effect `:file`
export function file_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : file<e,b>, ret : (res : a) -> e b, action : (hname : hnd/ev<file>) -> e a) -> e b */  {
  return $std_core_hnd._named_handle(file_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-read-line` constructor field of the `:file` type.
export function file_fs__fun_read_line(file_0) /* forall<e,a> (file : file<e,a>) -> hnd/clause0<string,file,e,a> */  {
  return file_0._fun_read_line;
}
 
 
// select `read-line` operation out of effect `:file`
export function read_line_fs__select(hnd) /* forall<e,a> (hnd : file<e,a>) -> hnd/clause0<string,file,e,a> */  {
  return hnd._fun_read_line;
}
 
 
// Call the `fun read-line` operation of the effect `:file`
export function read_line(_hname) /* (hnd/ev<file>) -> exn string */  {
  return _hname.hnd._fun_read_line(_hname.marker, _hname);
}
 
 
// monadic lift
export function _mlift_file_10028(x, wild__) /* forall<h,e> (x : string, wild_ : ()) -> <local<h>,exn,fsys|e> string */  {
  return x;
}
 
 
// monadic lift
export function _mlift_file_10029(content, _y_x10007) /* forall<h,e> (content : local-var<h,list<string>>, list<string>) -> <local<h>,exn,fsys|e> string */  {
  if (_y_x10007 === null) {
    return "";
  }
  else {
     
    var x_0_10037 = ((content).value = (_y_x10007.tail));
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
        return _y_x10007.head;
      });
    }
    else {
      return _y_x10007.head;
    }
  }
}
 
 
// monadic lift
export function _mlift_file_10030(action, _y_x10006) /* forall<h,a,e> (action : (hnd/ev<file>) -> <exn,fsys|e> a, string) -> <exn,fsys,local<h>|e> a */  {
   
  var init_10041 = $std_core_hnd._open_none1(function(s /* string */ ) {
       
      var v_10011 = ((s).split(("\n")));
      return $std_core_vector.vlist(v_10011);
    }, _y_x10006);
   
  var loc = { value: init_10041 };
   
  var res = file_fs__handle(_Hnd_file(1, $std_core_hnd.clause_tail0(function() {
         
        var x_10043 = ((loc).value);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10007 /* list<string> */ ) {
            return _mlift_file_10029(loc, _y_x10007);
          });
        }
        else {
          return _mlift_file_10029(loc, x_10043);
        }
      })), function(_res /* 388 */ ) {
      return _res;
    }, action);
  return $std_core_hnd.prompt_local_var(loc, res);
}
 
 
// a (named) handler instance for files
export function file(fname, action) /* forall<a,e> (fname : string, action : (hnd/ev<file>) -> <exn,fsys|e> a) -> <exn,fsys|e> a */  {
  return function() {
     
    var _x_x1_10025 = $std_core_hnd._open_none1($std_os_path.path, fname);
     
    var x_10046 = $std_core_hnd._open_at1($std_core_hnd._evv_index($std_core_exn.exn_fs__tag), $std_os_file.read_text_file, _x_x1_10025);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10006 /* string */ ) {
        return _mlift_file_10030(action, _y_x10006);
      });
    }
    else {
      return _mlift_file_10030(action, x_10046);
    }
  }();
}
 
 
// monadic lift
export function _mlift_main_10031(_y_x10015, _y_x10016) /* (string, string) -> exn () */  {
  return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(_y_x10015, $std_core_types._lp__plus__plus__rp_("\n", _y_x10016)));
}
 
 
// monadic lift
export function _mlift_main_10032(f2, _y_x10015) /* (f2 : hnd/ev<file>, string) -> exn () */  {
   
  var x_10048 = f2.hnd._fun_read_line(f2.marker, f2);
   
  function next_10049(_y_x10016) /* (string) -> exn () */  {
    return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(_y_x10015, $std_core_types._lp__plus__plus__rp_("\n", _y_x10016)));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10049);
  }
  else {
    return next_10049(x_10048);
  }
}
 
export function main() /* () -> <console/console,exn,fsys> () */  {
  return file("package.yaml", function(f1 /* hnd/ev<file> */ ) {
      return file("stack.yaml", function(f2 /* hnd/ev<file> */ ) {
           
          var x_10054 = f1.hnd._fun_read_line(f1.marker, f1);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10015 /* string */ ) {
              return _mlift_main_10032(f2, _y_x10015);
            });
          }
          else {
            return _mlift_main_10032(f2, x_10054);
          }
        });
    });
}
 
export function wrong_escape1() /* () -> <exn,fsys> hnd/ev<file> */  {
  return file("stack.yaml", function(f /* hnd/ev<file> */ ) {
      return f;
    });
}
 
 
// monadic lift
export function _mlift_example_wrong_10033(f_0) /* (f@0 : hnd/ev<file>) -> <exn,fsys> () */  {
   
  var x_10058 = f_0.hnd._fun_read_line(f_0.marker, f_0);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend($std_core_console.printsln);
  }
  else {
    return $std_core_console.printsln(x_10058);
  }
}
 
export function example_wrong() /* () -> <console/console,exn,fsys> () */  {
   
  var x_10062 = file("stack.yaml", function(f /* hnd/ev<file> */ ) {
      return f;
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_example_wrong_10033);
  }
  else {
     
    var x_0_10065 = x_10062.hnd._fun_read_line(x_10062.marker, x_10062);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend($std_core_console.printsln);
    }
    else {
      return $std_core_console.printsln(x_0_10065);
    }
  }
}
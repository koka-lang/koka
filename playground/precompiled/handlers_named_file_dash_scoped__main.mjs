// Koka generated module: handlers/named/file-scoped/@main, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_undiv from './std_core_undiv.mjs';
import * as $std_text_parse from './std_text_parse.mjs';
import * as $std_os_dir from './std_os_dir.mjs';
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
import * as $std_os_path from './std_os_path.mjs';
import * as $std_os_file from './std_os_file.mjs';
import * as $std_core from './std_core.mjs';
import * as $handlers_named_file_dash_scoped from './handlers_named_file_dash_scoped.mjs';
 
// externals
 
// type declarations
 
// declarations
 
export function _expr() /* () -> console/console () */  {
  return $handlers_named_file_dash_scoped.file("package.yaml", function(f1 /* hnd/ev<handlers/named/file-scoped/file<785>> */ ) {
      return $handlers_named_file_dash_scoped.file("stack.yaml", function(f2 /* hnd/ev<handlers/named/file-scoped/file<778>> */ ) {
          return $handlers_named_file_dash_scoped.read_both(f1, f2);
        });
    });
}
 
export function _main() /* () -> <st<global>,console/console,div,fsys,ndet,net,ui> () */  {
  return $handlers_named_file_dash_scoped.file("package.yaml", function(f1 /* hnd/ev<handlers/named/file-scoped/file<785>> */ ) {
      return $handlers_named_file_dash_scoped.file("stack.yaml", function(f2 /* hnd/ev<handlers/named/file-scoped/file<778>> */ ) {
          return $handlers_named_file_dash_scoped.read_both(f1, f2);
        });
    });
}
 
// main entry:
_main($std_core.id);
// Koka generated module: std/core/console, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_unsafe from './std_core_unsafe.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_string from './std_core_string.mjs';
import * as $std_core_show from './std_core_show.mjs';
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
// internal exports
const $std_core_console = { "_host": _host, "_trace": _trace }
export var _host = "unknown"
if (typeof window !== 'undefined' && window.document) {
  _host = "browser";
}
else if (typeof importScripts !== 'undefined') {
  _host = "webworker"
}
else if (typeof process !== undefined && typeof window === 'undefined') {
  _host = "node"
}
export var _trace = function(s) { };
export var _trace_any = function(s,x) { _trace("" + s + x); };
export var _print = function(s) { _trace(s); };
if (typeof console !== undefined && typeof console.log === "function") {
  _trace = function(s) {
    console.log(s);
  };
}
if (typeof console !== undefined && typeof console.info === "function") {
  _trace_any = function(s,x) {
    console.info(s,x);
  };
}
function _println(msg) {
  _print(msg + "\n");
}
/*------------------------------------------------
  Console for Node
------------------------------------------------*/
if (_host === "node") {
  _print = function(s) {
    process.stdout.write(s);
  };
}
/*------------------------------------------------
  Console for Browser
------------------------------------------------*/
if (_host === "browser") {
  (function(){
    var escapes = {
        '&': '&amp;', // & first!
        '<': '&lt;',
        '>': '&gt;',
        '\'': '&apos;',
        '"': '&quot;',
        '\n': '<br>',
        '\r': '',
    };
    var escapes_regex = new RegExp("[" + Object.keys(escapes).join("") + "]", "g");
    function html_escape(txt) {
      return txt.replace(escapes_regex, function (s) {
        var r = escapes[s];
        return (r ? r : "");
      });
    }
    function get_console() {
      var cons = document.getElementById("koka-console");
      if (cons==null) {
        cons = document.createElement("div");
        cons.id = "koka-console";
        cons.style.fontFamily = "Consolas,Monaco,'Ubuntu Mono','Droid Sans Mono','Source Code Pro',monospace"
        cons.style.fontSize = "12pt";
        cons.style.width = "99%";
        document.body.appendChild(cons);
      }
      if (cons.display == "none") return null;
      return cons;
    }
    var output = null;
    function get_console_out()
    {
      if (output) return output;
      output = document.getElementById("koka-console-out");
      if (!output) {
        var cons = get_console();
        if (!cons) return null;
        output = document.createElement("div");
        output.id = "koka-console-out";
        output.style.fontFamily = "Consolas,Monaco,'Ubuntu Mono','Droid Sans Mono','Source Code Pro',monospace"
        output.style.fontSize = "12pt";
        output.style.width = "99%";
        output.style.height = "30ex";
        output.style.border = "gray solid 1px";
        output.wrap="off";
        output.style.overflow = "auto";
        output.style.whiteSpace = "pre";
        output.style.padding = "2px";
        output.style.margin = "2px";
        output.style.paddingBottom = "4px";
        output.readOnly = true;
        cons.appendChild(output);
      }
      if (!output.print) {
        output.print_html = function(s) {
          output.innerHTML = output.innerHTML + s;
          // try to scroll to the end
          if (output.createTextRange) {
            output.createTextRange().scrollIntoView(false);
          }
          else if (output.scrollTop !== undefined) {
            output.scrollTop = output.scrollHeight;
          }
        };
        output.print = function(s) {
          output.print_html(html_escape(s));
        };
      }
      return output;
    }
    _print = function(s) {
      var out = get_console_out();
      if (out && out.print) {
        out.print(s);
      }
    };
    _print.print_html = function(s) {
      var out = get_console_out();
      if (out && out.print_html) {
        out.print_html(s);
      }
    };
  })();
}
 
// type declarations
// type console
 
// declarations
 
export var redirect;
var redirect = { value: ($std_core_types.Nothing) };
 
 
// Redirect `print` and `println` calls to a specified function.
export function print_redirect(print) /* (print : (msg : string) -> console ()) -> <st<global>,console,ndet> () */  {
  return ((redirect).value = ($std_core_types.Just(print)));
}
 
 
// Print a string to the console, including a final newline character.
export function xprintsln(s) /* (s : string) -> console () */  {
  return _println(s);
}
 
 
// Print a string to the console
export function xprints(s) /* (s : string) -> console () */  {
  return _print(s);
}
 
export function prints(s) /* (s : string) -> console () */  {
  var _x0 = redirect.value;
  if (_x0 === null) {
    return xprints(s);
  }
  else {
    return _x0.value(s);
  }
}
 
export function printsln(s) /* (s : string) -> console () */  {
  var _x1 = redirect.value;
  if (_x1 === null) {
    return xprintsln(s);
  }
  else {
    return _x1.value($std_core_types._lp__plus__plus__rp_(s, "\n"));
  }
}
 
 
// Print a string to the console.
export function string_fs_print(s) /* (s : string) -> console () */  {
  return prints(s);
}
 
 
// Print a value that has a `show` function
export function default_fs_show_fs_print(x, _implicit_fs_show) /* forall<a,e> (x : a, ?show : (a) -> <console|e> string) -> <console|e> () */  {
   
  var x_0_10003 = _implicit_fs_show(x);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(prints);
  }
  else {
    return prints(x_0_10003);
  }
}
 
 
// Print a string to the console, including a final newline character.
export function string_fs_println(s) /* (s : string) -> console () */  {
  return printsln(s);
}
 
 
// Print a value that has a `show` function, including a final newline character.
export function default_fs_show_fs_println(x, _implicit_fs_show) /* forall<a,e> (x : a, ?show : (a) -> <console|e> string) -> <console|e> () */  {
   
  var x_0_10005 = _implicit_fs_show(x);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(printsln);
  }
  else {
    return printsln(x_0_10005);
  }
}
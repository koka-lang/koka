// Koka generated module: std/os/path, koka version: 3.2.4
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
import * as $std_text_parse from './std_text_parse.mjs';
 
// externals
/*---------------------------------------------------------------------------
    Copyright 2012-2021, Microsoft Research, Daan Leijen.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
var _get_pathsep;
var _get_dirsep;
var _get_realpath;
var _get_homedir;
var _get_tempdir;
function _get_apppath() {
  if (typeof module === "undefined" || module==null) return "";
  var m = module; 
  while(m.parent != null) { m = m.parent; }; 
  return (m.filename != null ? m.filename : "");    
};
if ($std_core.host() === "node") {
  var _fs   = await import("fs");
  var _path = await import("path");
  var _os   = await import("os");
  _get_dirsep  = function() { return _path.sep; };
  _get_pathsep = function() { return _path.delimiter; };
  _get_realpath = function(p) { return _fs.realpathSync(p); };
  _get_homedir  = function() { return _os.homedir(); }
  _get_tempdir  = function() { return _os.tmpdir(); }
}
else {
  // browser?
  _get_dirsep = function() { return "/"; };
  _get_pathsep = function() { return ":"; };
  _get_realpath = function(p) { return p; };
  _get_homedir  = function() { return "."; };
  _get_tempdir  = function() { return "."; };
}
 
// type declarations
// type path
export function Path(root, parts) /* (root : string, parts : list<string>) -> path */  {
  return { root: root, parts: parts };
}
 
// declarations
 
 
// A `:path` represents a file system path.\
export function _create_Path(root, parts) /* (root : ? string, parts : ? (list<string>)) -> path */  {
  var _x0 = (root !== undefined) ? root : "";
  var _x1 = (parts !== undefined) ? parts : $std_core_types.Nil;
  return Path(_x0, _x1);
}
 
 
// Automatically generated. Retrieves the `root` constructor field of the `:path` type.
export function path_fs_root(path_0) /* (path : path) -> string */  {
  return path_0.root;
}
 
 
// Automatically generated. Retrieves the `parts` constructor field of the `:path` type.
export function path_fs_parts(path_0) /* (path : path) -> list<string> */  {
  return path_0.parts;
}
 
export function path_fs__copy(_this, root, parts) /* (path, root : ? string, parts : ? (list<string>)) -> path */  {
  if (root !== undefined) {
    var _x2 = root;
  }
  else {
    var _x2 = _this.root;
  }
  if (parts !== undefined) {
    var _x3 = parts;
  }
  else {
    var _x3 = _this.parts;
  }
  return Path(_x2, _x3);
}
 
 
// Return the base name of a path (stem name + extension)\
// `"/foo/bar.txt".path.basename === "bar.txt"` \
// `"/foo".path.basename === "foo"`
export function basename(p) /* (p : path) -> string */  {
  if (p.parts !== null) {
    return p.parts.head;
  }
  else {
    var _x4 = $std_core_types.Nothing;
    return (_x4 === null) ? "" : _x4.value;
  }
}
 
 
// Return the directory part of a path (including the rootname)
// `"/foo/bar.txt".path.dirname === "/foo"` \
// `"/foo".path.dirname === "/"`
export function dirname(p) /* (p : path) -> string */  {
   
  var _x5 = (p.parts !== null) ? p.parts.tail : $std_core_types.Nil;
  var xs_10007 = $std_core_list.reverse_acc($std_core_types.Nil, _x5);
  var _x5 = p.root;
  return $std_core_types._lp__plus__plus__rp_(_x5, $std_core_list.joinsep(xs_10007, "/"));
}
 
 
// Is a path empty?
export function is_empty(p) /* (p : path) -> bool */  {
  var _x7 = p.root;
  var _x6 = (_x7 === (""));
  if (_x6) {
    return (p.parts === null);
  }
  else {
    return false;
  }
}
 
 
// Convert a `:path` to a normalized `:string` path.\
// If this results in an empty string, the current directory path `"."` is returned.
// `"c:/foo/test.txt".path.string -> "c:/foo/test.txt"`\
// `"c:\\foo\\test.txt".path.string -> "c:/foo/test.txt"`\
// `"/foo//./bar/../test.txt".path.string -> "/foo/test.txt"`
export function string(p) /* (p : path) -> string */  {
   
  var _x8 = p.parts;
  var xs_10017 = $std_core_list.reverse_acc($std_core_types.Nil, _x8);
   
  var _x9 = p.root;
  var s = $std_core_types._lp__plus__plus__rp_(_x9, $std_core_list.joinsep(xs_10017, "/"));
  return ((s === (""))) ? "." : s;
}
 
export function split_base(basename_0) /* (basename : string) -> (string, string) */  {
  var _x8 = $std_core_sslice.find_last(basename_0, ".");
  if (_x8 !== null) {
    var _x9 = $std_core_sslice.Sslice(_x8.value.str, 0, _x8.value.start);
    return $std_core_types.Tuple2($std_core_sslice.string(_x9), $std_core_sslice.string($std_core_sslice.after(_x8.value)));
  }
  else {
    return $std_core_types.Tuple2(basename_0, "");
  }
}
 
 
// Return the extension of path (without the preceding dot (`'.'`))\
// `"/foo/bar.svg.txt".path.extname === "txt"`
export function extname(p) /* (p : path) -> string */  {
   
  if (p.parts !== null) {
    var _x11 = p.parts.head;
  }
  else {
    var _x12 = $std_core_types.Nothing;
    var _x11 = (_x12 === null) ? "" : _x12.value;
  }
  var _x10 = $std_core_sslice.find_last(_x11, ".");
  if (_x10 !== null) {
    var _x13 = $std_core_sslice.Sslice(_x10.value.str, 0, _x10.value.start);
    var tuple2_10023 = $std_core_types.Tuple2($std_core_sslice.string(_x13), $std_core_sslice.string($std_core_sslice.after(_x10.value)));
  }
  else {
    if (p.parts !== null) {
      var _x14 = p.parts.head;
    }
    else {
      var _x15 = $std_core_types.Nothing;
      var _x14 = (_x15 === null) ? "" : _x15.value;
    }
    var tuple2_10023 = $std_core_types.Tuple2(_x14, "");
  }
  return tuple2_10023.snd;
}
 
 
// Return the stem name of path.\
// `"/foo/bar.svg.txt".path.extname === "foo.svg"`
export function stemname(p) /* (p : path) -> string */  {
   
  if (p.parts !== null) {
    var _x11 = p.parts.head;
  }
  else {
    var _x12 = $std_core_types.Nothing;
    var _x11 = (_x12 === null) ? "" : _x12.value;
  }
  var _x10 = $std_core_sslice.find_last(_x11, ".");
  if (_x10 !== null) {
    var _x13 = $std_core_sslice.Sslice(_x10.value.str, 0, _x10.value.start);
    var tuple2_10024 = $std_core_types.Tuple2($std_core_sslice.string(_x13), $std_core_sslice.string($std_core_sslice.after(_x10.value)));
  }
  else {
    if (p.parts !== null) {
      var _x14 = p.parts.head;
    }
    else {
      var _x15 = $std_core_types.Nothing;
      var _x14 = (_x15 === null) ? "" : _x15.value;
    }
    var tuple2_10024 = $std_core_types.Tuple2(_x14, "");
  }
  return tuple2_10024.fst;
}
 
 
// Return the root name of path.
// `"c:\\foo".path.rootname === "c:/"`\
// `"/foo".path.rootname === "/"`
export function rootname(p) /* (p : path) -> string */  {
  return p.root;
}
 
export function split_parts(parts) /* (parts : list<string>) -> (string, list<string>) */  {
  if (parts !== null) {
    var _x10 = parts.head;
  }
  else {
    var _x11 = $std_core_types.Nothing;
    var _x10 = (_x11 === null) ? "" : _x11.value;
  }
  var _x12 = (parts !== null) ? parts.tail : $std_core_types.Nil;
  return $std_core_types.Tuple2(_x10, _x12);
}
 
 
// Show a path as a string.
export function show(p) /* (p : path) -> string */  {
  return $std_core_show.string_fs_show(string(p));
}
 
 
// Is a path relative?
export function is_relative(p) /* (p : path) -> bool */  {
  var _x13 = p.root;
  return (_x13 === (""));
}
 
 
// Is a path absolute?
export function is_absolute(p) /* (p : path) -> bool */  {
   
  var _x14 = p.root;
  var b_10032 = (_x14 === (""));
  return (b_10032) ? false : true;
}
 
 
// Return the first path if it is not empty, otherwise return the second one.
export function _lp__bar__bar__rp_(p1, p2) /* (p1 : path, p2 : path) -> path */  {
  var _x15 = p1.root;
  var _x14 = (_x15 === (""));
  if (_x14) {
    return (p1.parts === null) ? p2 : p1;
  }
  else {
    return p1;
  }
}
 
export function push_part(dir, dirs) /* (dir : string, dirs : list<string>) -> list<string> */  {
  if ((dir === ("."))) {
    return dirs;
  }
  else {
    if ((dir === (""))) {
      return dirs;
    }
    else {
      if ((dir === (".."))) {
        if (dirs !== null) {
          return (dirs !== null) ? dirs.tail : $std_core_types.Nil;
        }
        else {
          return $std_core_types.Cons(dir, dirs);
        }
      }
      else {
        return $std_core_types.Cons(dir, dirs);
      }
    }
  }
}
 
export function push_parts(parts, dirs) /* (parts : list<string>, dirs : list<string>) -> list<string> */  { tailcall: while(1)
{
  if (parts !== null) {
    {
      // tail call
      var _x16 = push_part(parts.head, dirs);
      parts = parts.tail;
      dirs = _x16;
      continue tailcall;
    }
  }
  else {
    return dirs;
  }
}}
 
export function path_parts(root, s, dirs) /* (root : string, s : string, dirs : ? (list<string>)) -> path */  {
   
  var v_10011 = ((s).split(("/")));
   
  var _x17 = (dirs !== undefined) ? dirs : $std_core_types.Nil;
  var parts = push_parts($std_core_vector.vlist(v_10011), _x17);
  return Path(root, parts);
}
 
 
// monadic lift
export function _mlift_proot_10184(wild___4) /* (wild_@4 : char) -> std/text/parse/parse bool */  {
  return false;
}
 
 
// monadic lift
export function _mlift_proot_10185(wild___5) /* (wild_@5 : ()) -> std/text/parse/parse bool */  {
  return true;
}
 
 
// monadic lift
export function _mlift_proot_10186(wild___0) /* (wild_@0 : char) -> std/text/parse/parse () */  {
  return $std_core_types.Unit;
}
 
 
// monadic lift
export function _mlift_proot_10187(wild__) /* (wild_ : char) -> std/text/parse/parse () */  {
   
  var x_10199 = $std_text_parse.char(0x003A);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_proot_10186);
  }
  else {
    return $std_core_types.Unit;
  }
}
 
 
// monadic lift
export function _mlift_proot_10188(_y_x10139) /* (list<char>) -> std/text/parse/parse () */  {
  return $std_core_types.Unit;
}
 
 
// monadic lift
export function _mlift_proot_10189(_y_x10137) /* (char) -> std/text/parse/parse () */  {
   
  var x_10201 = $std_text_parse.many_acc(function() {
      return $std_text_parse.none_of("/");
    }, $std_core_types.Nil);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_proot_10188);
  }
  else {
    return $std_core_types.Unit;
  }
}
 
 
// monadic lift
export function _mlift_proot_10190(wild___1) /* (wild_@1 : char) -> std/text/parse/parse () */  {
   
  var x_10203 = $std_text_parse.none_of("/");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_proot_10189);
  }
  else {
    return _mlift_proot_10189(x_10203);
  }
}
 
 
// monadic lift
export function _mlift_proot_10191(wild___3) /* (wild_@3 : ()) -> std/text/parse/parse bool */  {
  return $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_10205 = $std_text_parse.char(0x002F);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_proot_10184);
      }
      else {
        return false;
      }
    }, function() {
       
      var x_0_10207 = $std_text_parse.eof();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_proot_10185);
      }
      else {
        return true;
      }
    });
}
 
export function proot() /* () -> std/text/parse/parse bool */  {
   
  var x_10209 = $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_0_10212 = $std_text_parse.alpha();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_proot_10187);
      }
      else {
        return _mlift_proot_10187(x_0_10212);
      }
    }, function() {
       
      var x_1_10214 = $std_text_parse.char(0x002F);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_proot_10190);
      }
      else {
        return _mlift_proot_10190(x_1_10214);
      }
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_proot_10191);
  }
  else {
    return $std_text_parse._lp__bar__bar__rp_(function() {
         
        var x_2_10216 = $std_text_parse.char(0x002F);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(_mlift_proot_10184);
        }
        else {
          return false;
        }
      }, function() {
         
        var x_3_10218 = $std_text_parse.eof();
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(_mlift_proot_10185);
        }
        else {
          return true;
        }
      });
  }
}
 
 
// Create a normalized `:path` from a path string.
export function path(s) /* (s : string) -> path */  {
  if ((s === (""))) {
    return Path("", $std_core_types.Nil);
  }
  else {
     
    var t = (s).replace(new RegExp((("\\")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("/"));
    var _x17 = $std_text_parse.starts_with(t, proot);
    if (_x17 === null) {
       
      var v_10011 = ((t).split(("/")));
       
      var _x19 = undefined;
      var _x18 = (_x19 !== undefined) ? _x19 : $std_core_types.Nil;
      var parts = push_parts($std_core_vector.vlist(v_10011), _x18);
      return Path("", parts);
    }
    else {
       
      var _x18 = $std_core_sslice.Sslice(_x17.value.snd.str, 0, _x17.value.snd.start);
      var _x19 = (_x17.value.fst) ? "/" : "";
      var root_0_10104 = $std_core_types._lp__plus__plus__rp_($std_core_sslice.string(_x18), _x19);
       
      var s_1_10105 = $std_core_sslice.string(_x17.value.snd);
       
      var v_10011_0 = ((s_1_10105).split(("/")));
       
      var _x21 = undefined;
      var _x20 = (_x21 !== undefined) ? _x21 : $std_core_types.Nil;
      var parts_0 = push_parts($std_core_vector.vlist(v_10011_0), _x20);
      return Path(root_0_10104, parts_0);
    }
  }
}
 
export function _trmc_paths_collect(ps, _acc) /* (ps : list<string>, ctx<list<path>>) -> list<path> */  { tailcall: while(1)
{
  if (ps !== null && ps.tail !== null) {
    var _x19 = $std_core_types._int_eq(($std_core_string.chars_fs_count(ps.head)),1);
    if (_x19) {
       
      var m_10047 = $std_core_sslice.slice_fs_foreach_while($std_core_sslice.Sslice(ps.head, 0, (ps.head).length), $std_core_types.Just);
      var _x21 = (m_10047 === null) ? 0x0020 : m_10047.value;
      var _x20 = $std_core_char.is_alpha(_x21);
      if (_x20) {
         
        var b_10050 = ((ps.tail.head) === (""));
        if (b_10050) {
          var _x18 = false;
        }
        else {
          var _x18 = ((("/\\")).indexOf(($std_core_sslice.head(ps.tail.head))) >= 0);
        }
      }
      else {
        var _x18 = false;
      }
    }
    else {
      var _x18 = false;
    }
    if (_x18){
       
      var _trmc_x10128 = path($std_core_types._lp__plus__plus__rp_(ps.head, $std_core_types._lp__plus__plus__rp_(":", ps.tail.head)));
       
      var _trmc_x10129 = undefined;
       
      var _trmc_x10130 = $std_core_types.Cons(_trmc_x10128, _trmc_x10129);
      {
        // tail call
        var _x22 = $std_core_types._cctx_extend(_acc,_trmc_x10130,({obj: _trmc_x10130, field_name: "tail"}));
        ps = ps.tail.tail;
        _acc = _x22;
        continue tailcall;
      }
    }
  }
  if (ps !== null) {
     
    var _trmc_x10131 = path(ps.head);
     
    var _trmc_x10132 = undefined;
     
    var _trmc_x10133 = $std_core_types.Cons(_trmc_x10131, _trmc_x10132);
    {
      // tail call
      var _x23 = $std_core_types._cctx_extend(_acc,_trmc_x10133,({obj: _trmc_x10133, field_name: "tail"}));
      ps = ps.tail;
      _acc = _x23;
      continue tailcall;
    }
  }
  return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
}}
 
export function paths_collect(ps_0) /* (ps : list<string>) -> list<path> */  {
  return _trmc_paths_collect(ps_0, $std_core_types._cctx_empty());
}
 
 
// Parse a list of paths seperated by colon (`':'`) or semi-colon (`';'`)
//
// Colon separated paths can be ambiguous with Windows style root names (`c:\\`)
// In particular, a single letter path followed by an absolute path, e.g. ``c:/foo:/bar`` is
// parsed as ``c:/foo`` and ``/bar``.
export function paths(s) /* (s : string) -> list<path> */  {
   
  var s_0_10052 = (s).replace(new RegExp(((";")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),(":"));
   
  var v_10011 = ((s_0_10052).split((":")));
   
  var ps_10220 = $std_core_vector.vlist(v_10011);
  return _trmc_paths_collect(ps_10220, $std_core_types._cctx_empty());
}
 
 
// Add two paths together using left-associative operator `(/)`. \
// Keeps the root of `p1` and discards the root name of `p2`.\
// `"/a/" / "b/foo.txt"          === "/a/b/foo.txt"`\
// `"/a/foo.txt" / "/b/bar.txt"  === "/a/foo.txt/b/bar.txt"`\
// `"c:/foo" / "d:/bar"          === "c:/foo/bar"`
export function _lp__fs__rp_(p1, p2) /* (p1 : path, p2 : path) -> path */  {
   
  var _x24 = p2.parts;
  var _x25 = p1.parts;
  var parts_10055 = push_parts($std_core_list.reverse_acc($std_core_types.Nil, _x24), _x25);
  var _x24 = p1.root;
  var _x25 = (parts_10055 !== undefined) ? parts_10055 : $std_core_types.Nil;
  return Path(_x24, _x25);
}
 
 
// Convenience function that adds a string path.
export function pathstring_fs__lp__fs__rp_(p1, p2) /* (p1 : path, p2 : string) -> path */  {
   
  var p2_0_10108 = path(p2);
   
  var _x26 = p2_0_10108.parts;
  var _x27 = p1.parts;
  var parts_10055 = push_parts($std_core_list.reverse_acc($std_core_types.Nil, _x26), _x27);
  var _x26 = p1.root;
  var _x27 = (parts_10055 !== undefined) ? parts_10055 : $std_core_types.Nil;
  return Path(_x26, _x27);
}
 
 
// Convenience function that adds two strings into a path.
export function string_fs__lp__fs__rp_(p1, p2) /* (p1 : string, p2 : string) -> path */  {
   
  var p1_0_10109 = path(p1);
   
  var p2_0_10110 = path(p2);
   
  var _x28 = p2_0_10110.parts;
  var _x29 = p1_0_10109.parts;
  var parts_10055 = push_parts($std_core_list.reverse_acc($std_core_types.Nil, _x28), _x29);
  var _x28 = p1_0_10109.root;
  var _x29 = (parts_10055 !== undefined) ? parts_10055 : $std_core_types.Nil;
  return Path(_x28, _x29);
}
 
 
// Combine multiple paths using `(/)`.
export function combine(ps) /* (ps : list<path>) -> path */  {
  if (ps === null) {
    var _x31 = undefined;
    var _x30 = (_x31 !== undefined) ? _x31 : "";
    var _x33 = undefined;
    var _x32 = (_x33 !== undefined) ? _x33 : $std_core_types.Nil;
    return Path(_x30, _x32);
  }
  else {
    return $std_core_list.foldl(ps.tail, ps.head, _lp__fs__rp_);
  }
}
 
 
// Remove the directory and root and only keep the base name (file name) portion of the path.\
// `nodir("foo/bar.ext".path) === "bar.ext"`
export function nodir(p) /* (p : path) -> path */  {
   
  var _x34 = p.parts;
  var parts_10113 = $std_core_list.take(_x34, 1);
  if (parts_10113 !== undefined) {
    var _x34 = parts_10113;
  }
  else {
    var _x34 = p.parts;
  }
  return Path("", _x34);
}
 
 
// Remove the basename and only keep the root and directory name portion of the path.\
// `nobase("foo/bar.ext".path) == "foo")`
export function nobase(p) /* (p : path) -> path */  {
  var _x36 = undefined;
  if (_x36 !== undefined) {
    var _x35 = _x36;
  }
  else {
    var _x35 = p.root;
  }
  var _x37 = (p.parts !== null) ? p.parts.tail : $std_core_types.Nil;
  return Path(_x35, _x37);
}
 
 
// Change the extension of a path.
// Only adds a dot if the extname does not already start with a dot.
export function change_ext(p, extname_0) /* (p : path, extname : string) -> path */  {
  if (p.parts !== null) {
    var _x39 = p.parts.head;
  }
  else {
    var _x40 = $std_core_types.Nothing;
    var _x39 = (_x40 === null) ? "" : _x40.value;
  }
  var _x38 = $std_core_sslice.find_last(_x39, ".");
  if (_x38 !== null) {
     
    var _x41 = $std_core_sslice.Sslice(_x38.value.str, 0, _x38.value.start);
    var stemname_0 = $std_core_sslice.string(_x41);
     
    var _pat_1_2 = $std_core_sslice.string($std_core_sslice.after(_x38.value));
     
    var maybe_10071 = $std_core_sslice.starts_with(extname_0, ".");
     
    if (maybe_10071 !== null) {
      var newext = extname_0;
    }
    else {
      var newext = $std_core_types._lp__plus__plus__rp_(".", extname_0);
    }
     
    var s_0_10116 = $std_core_types._lp__plus__plus__rp_(stemname_0, newext);
     
    var v_10011 = ((s_0_10116).split(("/")));
     
    var _x42 = (p.parts !== null) ? p.parts.tail : $std_core_types.Nil;
    var parts = push_parts($std_core_vector.vlist(v_10011), _x42);
    var _x41 = p.root;
    return Path(_x41, parts);
  }
  else {
     
    var maybe_10071_0 = $std_core_sslice.starts_with(extname_0, ".");
     
    if (maybe_10071_0 !== null) {
      var newext_0 = extname_0;
    }
    else {
      var newext_0 = $std_core_types._lp__plus__plus__rp_(".", extname_0);
    }
     
    if (p.parts !== null) {
      var _x42 = p.parts.head;
    }
    else {
      var _x43 = $std_core_types.Nothing;
      var _x42 = (_x43 === null) ? "" : _x43.value;
    }
    var s_0_10116_0 = $std_core_types._lp__plus__plus__rp_(_x42, newext_0);
     
    var v_10011_0 = ((s_0_10116_0).split(("/")));
     
    var _x44 = (p.parts !== null) ? p.parts.tail : $std_core_types.Nil;
    var parts_0 = push_parts($std_core_vector.vlist(v_10011_0), _x44);
    var _x42 = p.root;
    return Path(_x42, parts_0);
  }
}
 
 
// Remove the extension from a path.
export function noext(p) /* (p : path) -> path */  {
  return change_ext(p, "");
}
 
 
// If a path has no extension, set it to the provided one.
export function default_ext(p, newext) /* (p : path, newext : string) -> path */  {
   
  if (p.parts !== null) {
    var _x44 = p.parts.head;
  }
  else {
    var _x45 = $std_core_types.Nothing;
    var _x44 = (_x45 === null) ? "" : _x45.value;
  }
  var _x43 = $std_core_sslice.find_last(_x44, ".");
  if (_x43 !== null) {
    var _x46 = $std_core_sslice.Sslice(_x43.value.str, 0, _x43.value.start);
    var tuple2_10075 = $std_core_types.Tuple2($std_core_sslice.string(_x46), $std_core_sslice.string($std_core_sslice.after(_x43.value)));
  }
  else {
    if (p.parts !== null) {
      var _x47 = p.parts.head;
    }
    else {
      var _x48 = $std_core_types.Nothing;
      var _x47 = (_x48 === null) ? "" : _x48.value;
    }
    var tuple2_10075 = $std_core_types.Tuple2(_x47, "");
  }
  var _x44 = tuple2_10075.snd;
  var _x43 = (_x44 === (""));
  if (_x43) {
    return change_ext(p, newext);
  }
  else {
    return p;
  }
}
 
 
// Change the base name of a path
export function change_base(p, basename_0) /* (p : path, basename : string) -> path */  {
   
  var _x46 = undefined;
  if (_x46 !== undefined) {
    var _x45 = _x46;
  }
  else {
    var _x45 = p.root;
  }
  var _x47 = (p.parts !== null) ? p.parts.tail : $std_core_types.Nil;
  var q = Path(_x45, _x47);
   
  var v_10011 = ((basename_0).split(("/")));
   
  var _x48 = q.parts;
  var parts = push_parts($std_core_vector.vlist(v_10011), _x48);
  var _x45 = q.root;
  return Path(_x45, parts);
}
 
 
// Change the stem name of a path
export function change_stem(p, stemname_0) /* (p : path, stemname : string) -> path */  {
   
  if (p.parts !== null) {
    var _x47 = p.parts.head;
  }
  else {
    var _x48 = $std_core_types.Nothing;
    var _x47 = (_x48 === null) ? "" : _x48.value;
  }
  var _x46 = $std_core_sslice.find_last(_x47, ".");
  if (_x46 !== null) {
    var _x49 = $std_core_sslice.Sslice(_x46.value.str, 0, _x46.value.start);
    var tuple2_10079 = $std_core_types.Tuple2($std_core_sslice.string(_x49), $std_core_sslice.string($std_core_sslice.after(_x46.value)));
  }
  else {
    if (p.parts !== null) {
      var _x50 = p.parts.head;
    }
    else {
      var _x51 = $std_core_types.Nothing;
      var _x50 = (_x51 === null) ? "" : _x51.value;
    }
    var tuple2_10079 = $std_core_types.Tuple2(_x50, "");
  }
   
  var _x54 = tuple2_10079.snd;
  var _x53 = (_x54 === (""));
  if (_x53) {
    var _x52 = "";
  }
  else {
    var _x55 = tuple2_10079.snd;
    var _x52 = $std_core_types._lp__plus__plus__rp_(".", _x55);
  }
  var basename_0_10081 = $std_core_types._lp__plus__plus__rp_(stemname_0, _x52);
   
  var _x57 = undefined;
  if (_x57 !== undefined) {
    var _x56 = _x57;
  }
  else {
    var _x56 = p.root;
  }
  var _x58 = (p.parts !== null) ? p.parts.tail : $std_core_types.Nil;
  var q = Path(_x56, _x58);
   
  var v_10011 = ((basename_0_10081).split(("/")));
   
  var _x59 = q.parts;
  var parts = push_parts($std_core_vector.vlist(v_10011), _x59);
  var _x46 = q.root;
  return Path(_x46, parts);
}
 
 
// Return a list of all directory components (excluding the root but including the basename).\
// `"/foo/bar/test.txt".path.dirparts === ["foo","bar","test.txt"]`
export function dirparts(p) /* (p : path) -> list<string> */  {
  var _x47 = p.parts;
  return $std_core_list.reverse_acc($std_core_types.Nil, _x47);
}
 
 
// Return the last directory component name (or the empty string).\
// `"c:/foo/bar/tst.txt".path.parentname === "bar"
export function parentname(p) /* (p : path) -> string */  {
  var _x48 = (p.parts !== null) ? p.parts.tail : $std_core_types.Nil;
  if (_x48 !== null) {
    return _x48.head;
  }
  else {
    var _x49 = $std_core_types.Nothing;
    return (_x49 === null) ? "" : _x49.value;
  }
}
 
export function xrealpath(p) /* (p : string) -> io string */  {
  return ((function() {
    try {
      return _get_realpath(p);
    }
    catch(_err){ return $std_core_exn._throw_exception(_err); }
  })());
}
 
 
// monadic lift
export function string_fs__mlift_realpath_10192(_y_x10146) /* (string) -> io path */  {
  return $std_core_hnd._open_none1(path, _y_x10146);
}
 
 
// Convert a path to the absolute path on the file system.\
// The overload on a plain string is necessary as it allows
// for unnormalized paths with `".."` parts. For example
// `"/foo/symlink/../test.txt"` may resolve to `"/bar/test.txt"` if
// ``symlink`` is a symbolic link to a sub directory of `"/bar"`.
export function string_fs_realpath(s) /* (s : string) -> io path */  {
   
  var x_10223 = xrealpath(s);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(string_fs__mlift_realpath_10192);
  }
  else {
    return $std_core_hnd._open_none1(path, x_10223);
  }
}
 
 
// monadic lift
export function _mlift_realpath_10193(_y_x10147) /* (string) -> io path */  {
  return $std_core_hnd._open_none1(path, _y_x10147);
}
 
 
// Convert a path to the absolute path on the file system.
// The path is not required to exist on disk. However, if it
// exists any permissions and symbolic links are resolved fully.\
// `".".realpath` (to get the current working directory)\
// `"/foo".realpath` (to resolve the full root, like `"c:/foo"` on windows)
export function realpath(p) /* (p : path) -> io path */  {
   
  var s_10092 = $std_core_hnd._open_none1(string, p);
   
  var x_10226 = xrealpath(s_10092);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_realpath_10193);
  }
  else {
    return $std_core_hnd._open_none1(path, x_10226);
  }
}
 
 
// monadic lift
export function _mlift_cwd_10194(_y_x10148) /* (string) -> io path */  {
  return $std_core_hnd._open_none1(path, _y_x10148);
}
 
 
// Returns the current working directory.\
// Equal to `".".realpath`.
export function cwd() /* () -> io path */  {
   
  var x_10229 = xrealpath(".");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_cwd_10194);
  }
  else {
    return $std_core_hnd._open_none1(path, x_10229);
  }
}
 
 
// Return the OS specific directory separator (`"/"` or `"\\"`)
export function partsep() /* () -> ndet string */  {
  return _get_partsep();
}
 
 
// Return the OS specific path separator (`';'` or `':'`)
export function pathsep() /* () -> ndet string */  {
  return _get_pathsep();
}
 
export function xapp_path() /* () -> io string */  {
  return ((function() {
    try {
      return _get_apppath();
    }
    catch(_err){ return $std_core_exn._throw_exception(_err); }
  })());
}
 
 
// monadic lift
export function _mlift_app_path_10195(_y_x10150) /* (string) -> io path */  {
  return $std_core_hnd._open_none1(path, _y_x10150);
}
 
 
// Return the path to the currently executing application.
export function app_path() /* () -> io path */  {
   
  var x_10232 = xapp_path();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_app_path_10195);
  }
  else {
    return $std_core_hnd._open_none1(path, x_10232);
  }
}
 
 
// monadic lift
export function _mlift_appdir_10196(_y_x10151) /* (string) -> io path */  {
   
  var _x_x1_10176 = $std_core_hnd._open_none1(path, _y_x10151);
   
  var p_0 = $std_core_hnd._open_none1(function(p /* path */ ) {
      var _x51 = undefined;
      if (_x51 !== undefined) {
        var _x50 = _x51;
      }
      else {
        var _x50 = p.root;
      }
      var _x52 = (p.parts !== null) ? p.parts.tail : $std_core_types.Nil;
      return Path(_x50, _x52);
    }, _x_x1_10176);
  var _x50 = (($std_core_hnd._open_none1(function(p_1 /* path */ ) {
      if (p_1.parts !== null) {
        return p_1.parts.head;
      }
      else {
        var _x51 = $std_core_types.Nothing;
        return (_x51 === null) ? "" : _x51.value;
      }
    }, p_0)) === ("bin"));
  if (_x50) {
    return $std_core_hnd._open_none1(nobase, p_0);
  }
  else {
    var _x52 = (($std_core_hnd._open_none1(function(p_2 /* path */ ) {
        if (p_2.parts !== null) {
          return p_2.parts.head;
        }
        else {
          var _x53 = $std_core_types.Nothing;
          return (_x53 === null) ? "" : _x53.value;
        }
      }, p_0)) === ("exe"));
    if (_x52) {
      return $std_core_hnd._open_none1(nobase, p_0);
    }
    else {
      return p_0;
    }
  }
}
 
 
// Return the base directory that contains the currently running application.
// First tries `app-path().nobase`; if that ends in the ``bin`` or ``exe`` directory it
// returns the parent of that directory.
export function appdir() /* () -> io path */  {
   
  var x_10235 = xapp_path();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_appdir_10196);
  }
  else {
     
    var _x_x1_10176 = $std_core_hnd._open_none1(path, x_10235);
     
    var p_0 = $std_core_hnd._open_none1(function(p /* path */ ) {
        var _x55 = undefined;
        if (_x55 !== undefined) {
          var _x54 = _x55;
        }
        else {
          var _x54 = p.root;
        }
        var _x56 = (p.parts !== null) ? p.parts.tail : $std_core_types.Nil;
        return Path(_x54, _x56);
      }, _x_x1_10176);
    var _x54 = (($std_core_hnd._open_none1(function(p_1 /* path */ ) {
        if (p_1.parts !== null) {
          return p_1.parts.head;
        }
        else {
          var _x55 = $std_core_types.Nothing;
          return (_x55 === null) ? "" : _x55.value;
        }
      }, p_0)) === ("bin"));
    if (_x54) {
      return $std_core_hnd._open_none1(nobase, p_0);
    }
    else {
      var _x56 = (($std_core_hnd._open_none1(function(p_2 /* path */ ) {
          if (p_2.parts !== null) {
            return p_2.parts.head;
          }
          else {
            var _x57 = $std_core_types.Nothing;
            return (_x57 === null) ? "" : _x57.value;
          }
        }, p_0)) === ("exe"));
      if (_x56) {
        return $std_core_hnd._open_none1(nobase, p_0);
      }
      else {
        return p_0;
      }
    }
  }
}
 
export function xhomedir() /* () -> io string */  {
  return ((function() {
    try {
      return _get_homedir();
    }
    catch(_err){ return $std_core_exn._throw_exception(_err); }
  })());
}
 
 
// monadic lift
export function _mlift_homedir_10197(_y_x10155) /* (string) -> io path */  {
  return $std_core_hnd._open_none1(path, _y_x10155);
}
 
 
// Return the home directory of the current user.
export function homedir() /* () -> io path */  {
   
  var x_10238 = xhomedir();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_homedir_10197);
  }
  else {
    return $std_core_hnd._open_none1(path, x_10238);
  }
}
 
export function xtempdir() /* () -> io string */  {
  return ((function() {
    try {
      return _get_tempdir();
    }
    catch(_err){ return $std_core_exn._throw_exception(_err); }
  })());
}
 
 
// monadic lift
export function _mlift_tempdir_10198(_y_x10157) /* (string) -> io path */  {
  return $std_core_hnd._open_none1(path, _y_x10157);
}
 
 
// Return the temporary directory for the current user.
export function tempdir() /* () -> io path */  {
   
  var x_10241 = xtempdir();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_tempdir_10198);
  }
  else {
    return $std_core_hnd._open_none1(path, x_10241);
  }
}
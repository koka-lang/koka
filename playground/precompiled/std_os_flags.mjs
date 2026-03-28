// Koka generated module: std/os/flags, koka version: 3.2.4
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
import * as $std_os_env from './std_os_env.mjs';
 
// externals
 
// type declarations
// type flag-parser
export function Bool($default) /* forall<a> (default : (a, bool) -> a) -> flag-parser<a> */  {
  return { _tag: 1, $default: $default };
}
export function Req(parse, help) /* forall<a> (parse : (a, string) -> a, help : string) -> flag-parser<a> */  {
  return { _tag: 2, parse: parse, help: help };
}
export function Opt(parse, help) /* forall<a> (parse : (a, maybe<string>) -> a, help : string) -> flag-parser<a> */  {
  return { _tag: 3, parse: parse, help: help };
}
// type flag
export function Flag(short_names, long_names, parser, help) /* forall<a> (short-names : string, long-names : list<string>, parser : flag-parser<a>, help : string) -> flag<a> */  {
  return { short_names: short_names, long_names: long_names, parser: parser, help: help };
}
// type flag-kind
export function Flg(set) /* forall<a> (set : (a) -> a) -> flag-kind<a> */  {
  return { _tag: 1, set: set };
}
export function Arg(arg) /* forall<a> (arg : string) -> flag-kind<a> */  {
  return { _tag: 2, arg: arg };
}
export const End = { _tag: 3 }; // forall<a> flag-kind<a>
export function Unknown(arg) /* forall<a> (arg : string) -> flag-kind<a> */  {
  return { _tag: 4, arg: arg };
}
export function $Error(msg) /* forall<a> (msg : string) -> flag-kind<a> */  {
  return { _tag: 5, msg: msg };
}
// type flag-order
export const Permute = { _tag: 1 }; // forall<a> flag-order<a>
export const Preorder = { _tag: 2 }; // forall<a> flag-order<a>
export function Wrap(wrap) /* forall<a> (wrap : (string) -> a) -> flag-order<a> */  {
  return { _tag: 3, wrap: wrap };
}
// type myflags
export function Myflags(verbose, version, name, output, $arguments) /* (verbose : bool, version : bool, name : string, output : string, arguments : list<string>) -> myflags */  {
  return { verbose: verbose, version: version, name: name, output: output, $arguments: $arguments };
}
 
// declarations
 
export function _create_Myflags(verbose, version, name, output, $arguments) /* (verbose : ? bool, version : ? bool, name : ? string, output : ? string, arguments : ? (list<string>)) -> myflags */  {
  var _x0 = (verbose !== undefined) ? verbose : false;
  var _x1 = (version !== undefined) ? version : false;
  var _x2 = (name !== undefined) ? name : "";
  var _x3 = (output !== undefined) ? output : "";
  var _x4 = ($arguments !== undefined) ? $arguments : $std_core_types.Nil;
  return Myflags(_x0, _x1, _x2, _x3, _x4);
}
 
 
// Automatically generated. Retrieves the `verbose` constructor field of the `:myflags` type.
export function myflags_fs_verbose(myflags_0) /* (myflags : myflags) -> bool */  {
  return myflags_0.verbose;
}
 
 
// Automatically generated. Retrieves the `version` constructor field of the `:myflags` type.
export function myflags_fs_version(myflags_0) /* (myflags : myflags) -> bool */  {
  return myflags_0.version;
}
 
 
// Automatically generated. Retrieves the `name` constructor field of the `:myflags` type.
export function myflags_fs_name(myflags_0) /* (myflags : myflags) -> string */  {
  return myflags_0.name;
}
 
 
// Automatically generated. Retrieves the `output` constructor field of the `:myflags` type.
export function myflags_fs_output(myflags_0) /* (myflags : myflags) -> string */  {
  return myflags_0.output;
}
 
 
// Automatically generated. Retrieves the `arguments` constructor field of the `:myflags` type.
export function myflags_fs_arguments(myflags_0) /* (myflags : myflags) -> list<string> */  {
  return myflags_0.$arguments;
}
 
export function myflags_fs__copy(_this, verbose, version, name, output, $arguments) /* (myflags, verbose : ? bool, version : ? bool, name : ? string, output : ? string, arguments : ? (list<string>)) -> myflags */  {
  if (verbose !== undefined) {
    var _x5 = verbose;
  }
  else {
    var _x5 = _this.verbose;
  }
  if (version !== undefined) {
    var _x6 = version;
  }
  else {
    var _x6 = _this.version;
  }
  if (name !== undefined) {
    var _x7 = name;
  }
  else {
    var _x7 = _this.name;
  }
  if (output !== undefined) {
    var _x8 = output;
  }
  else {
    var _x8 = _this.output;
  }
  if ($arguments !== undefined) {
    var _x9 = $arguments;
  }
  else {
    var _x9 = _this.$arguments;
  }
  return Myflags(_x5, _x6, _x7, _x8, _x9);
}
 
export function show_flags(o) /* (o : myflags) -> string */  {
   
  var _x10 = (o.verbose) ? "True" : "False";
  var _x11 = (o.version) ? "True" : "False";
  var _x12 = o.name;
  var _x13 = o.output;
  var _x14 = o.$arguments;
  var xs_10005 = $std_core_types.Cons($std_core_types._lp__plus__plus__rp_("verbose=", _x10), $std_core_types.Cons($std_core_types._lp__plus__plus__rp_("version=", _x11), $std_core_types.Cons($std_core_types._lp__plus__plus__rp_("name=", $std_core_show.string_fs_show(_x12)), $std_core_types.Cons($std_core_types._lp__plus__plus__rp_("output=", $std_core_show.string_fs_show(_x13)), $std_core_types.Cons($std_core_types._lp__plus__plus__rp_("arguments=", $std_core_list.joinsep(_x14, ",")), $std_core_types.Nil)))));
  return $std_core_types._lp__plus__plus__rp_("{", $std_core_types._lp__plus__plus__rp_($std_core_list.joinsep(xs_10005, ";"), "}"));
}
 
export function set_name(t, name) /* (t : myflags, name : string) -> myflags */  {
  var _x11 = undefined;
  if (_x11 !== undefined) {
    var _x10 = _x11;
  }
  else {
    var _x10 = t.verbose;
  }
  var _x13 = undefined;
  if (_x13 !== undefined) {
    var _x12 = _x13;
  }
  else {
    var _x12 = t.version;
  }
  var _x15 = undefined;
  if (_x15 !== undefined) {
    var _x14 = _x15;
  }
  else {
    var _x14 = t.output;
  }
  var _x17 = undefined;
  if (_x17 !== undefined) {
    var _x16 = _x17;
  }
  else {
    var _x16 = t.$arguments;
  }
  return Myflags(_x10, _x12, name, _x14, _x16);
}
 
export function set_verbose(t, v) /* (t : myflags, v : bool) -> myflags */  {
  var _x19 = undefined;
  if (_x19 !== undefined) {
    var _x18 = _x19;
  }
  else {
    var _x18 = t.version;
  }
  var _x21 = undefined;
  if (_x21 !== undefined) {
    var _x20 = _x21;
  }
  else {
    var _x20 = t.name;
  }
  var _x23 = undefined;
  if (_x23 !== undefined) {
    var _x22 = _x23;
  }
  else {
    var _x22 = t.output;
  }
  var _x25 = undefined;
  if (_x25 !== undefined) {
    var _x24 = _x25;
  }
  else {
    var _x24 = t.$arguments;
  }
  return Myflags(v, _x18, _x20, _x22, _x24);
}
 
export function set_version(t, v) /* (t : myflags, v : bool) -> myflags */  {
  var _x27 = undefined;
  if (_x27 !== undefined) {
    var _x26 = _x27;
  }
  else {
    var _x26 = t.verbose;
  }
  var _x29 = undefined;
  if (_x29 !== undefined) {
    var _x28 = _x29;
  }
  else {
    var _x28 = t.name;
  }
  var _x31 = undefined;
  if (_x31 !== undefined) {
    var _x30 = _x31;
  }
  else {
    var _x30 = t.output;
  }
  var _x33 = undefined;
  if (_x33 !== undefined) {
    var _x32 = _x33;
  }
  else {
    var _x32 = t.$arguments;
  }
  return Myflags(_x26, v, _x28, _x30, _x32);
}
 
export function set_output(t, mbs) /* (t : myflags, mbs : maybe<string>) -> myflags */  {
  if (mbs === null) {
    var _x35 = undefined;
    if (_x35 !== undefined) {
      var _x34 = _x35;
    }
    else {
      var _x34 = t.verbose;
    }
    var _x37 = undefined;
    if (_x37 !== undefined) {
      var _x36 = _x37;
    }
    else {
      var _x36 = t.version;
    }
    var _x39 = undefined;
    if (_x39 !== undefined) {
      var _x38 = _x39;
    }
    else {
      var _x38 = t.name;
    }
    var _x41 = undefined;
    if (_x41 !== undefined) {
      var _x40 = _x41;
    }
    else {
      var _x40 = t.$arguments;
    }
    return Myflags(_x34, _x36, _x38, "stdout", _x40);
  }
  else {
    var _x43 = undefined;
    if (_x43 !== undefined) {
      var _x42 = _x43;
    }
    else {
      var _x42 = t.verbose;
    }
    var _x45 = undefined;
    if (_x45 !== undefined) {
      var _x44 = _x45;
    }
    else {
      var _x44 = t.version;
    }
    var _x47 = undefined;
    if (_x47 !== undefined) {
      var _x46 = _x47;
    }
    else {
      var _x46 = t.name;
    }
    var _x49 = undefined;
    if (_x49 !== undefined) {
      var _x48 = _x49;
    }
    else {
      var _x48 = t.$arguments;
    }
    return Myflags(_x42, _x44, _x46, mbs.value, _x48);
  }
}
 
export var myflags;
var myflags = $std_core_types.Cons(Flag("V?", $std_core_types.Cons("version", $std_core_types.Nil), Bool(set_version), "display version information"), $std_core_types.Cons(Flag("v", $std_core_types.Cons("verbose", $std_core_types.Nil), Bool(set_verbose), "verbosely list files"), $std_core_types.Cons(Flag("o", $std_core_types.Cons("output", $std_core_types.Nil), Opt(set_output, "FILE"), "use FILE for dump"), $std_core_types.Cons(Flag("n", $std_core_types.Cons("name", $std_core_types.Nil), Req(set_name, "USER"), "only show USER files"), $std_core_types.Nil))));
 
 
// Automatically generated. Tests for the `Permute` constructor of the `:flag-order` type.
export function is_permute(flag_order) /* forall<a> (flag-order : flag-order<a>) -> bool */  {
  return (flag_order._tag === 1);
}
 
 
// Automatically generated. Tests for the `Preorder` constructor of the `:flag-order` type.
export function is_preorder(flag_order) /* forall<a> (flag-order : flag-order<a>) -> bool */  {
  return (flag_order._tag === 2);
}
 
 
// Automatically generated. Tests for the `Wrap` constructor of the `:flag-order` type.
export function is_wrap(flag_order) /* forall<a> (flag-order : flag-order<a>) -> bool */  {
  return (flag_order._tag === 3);
}
 
 
// Automatically generated. Retrieves the `short-names` constructor field of the `:flag` type.
export function flag_fs_short_names(flag) /* forall<a> (flag : flag<a>) -> string */  {
  return flag.short_names;
}
 
 
// Automatically generated. Retrieves the `long-names` constructor field of the `:flag` type.
export function flag_fs_long_names(flag) /* forall<a> (flag : flag<a>) -> list<string> */  {
  return flag.long_names;
}
 
 
// Automatically generated. Retrieves the `parser` constructor field of the `:flag` type.
export function flag_fs_parser(flag) /* forall<a> (flag : flag<a>) -> flag-parser<a> */  {
  return flag.parser;
}
 
 
// Automatically generated. Retrieves the `help` constructor field of the `:flag` type.
export function flag_fs_help(flag) /* forall<a> (flag : flag<a>) -> string */  {
  return flag.help;
}
 
export function flag_fs__copy(_this, short_names, long_names, parser, help) /* forall<a> (flag<a>, short-names : ? string, long-names : ? (list<string>), parser : ? (flag-parser<a>), help : ? string) -> flag<a> */  {
  if (short_names !== undefined) {
    var _x50 = short_names;
  }
  else {
    var _x50 = _this.short_names;
  }
  if (long_names !== undefined) {
    var _x51 = long_names;
  }
  else {
    var _x51 = _this.long_names;
  }
  if (parser !== undefined) {
    var _x52 = parser;
  }
  else {
    var _x52 = _this.parser;
  }
  if (help !== undefined) {
    var _x53 = help;
  }
  else {
    var _x53 = _this.help;
  }
  return Flag(_x50, _x51, _x52, _x53);
}
 
 
// Automatically generated. Tests for the `Bool` constructor of the `:flag-parser` type.
export function is_bool(flag_parser) /* forall<a> (flag-parser : flag-parser<a>) -> bool */  {
  return (flag_parser._tag === 1);
}
 
 
// Automatically generated. Tests for the `Req` constructor of the `:flag-parser` type.
export function is_req(flag_parser) /* forall<a> (flag-parser : flag-parser<a>) -> bool */  {
  return (flag_parser._tag === 2);
}
 
 
// Automatically generated. Tests for the `Opt` constructor of the `:flag-parser` type.
export function is_opt(flag_parser) /* forall<a> (flag-parser : flag-parser<a>) -> bool */  {
  return (flag_parser._tag === 3);
}
 
export function show_short_flag(parser) /* forall<a> (parser : flag-parser<a>) -> string */  {
  if (parser._tag === 1) {
    return "";
  }
  else if (parser._tag === 2) {
    return $std_core_types._lp__plus__plus__rp_("<", $std_core_types._lp__plus__plus__rp_(parser.help, ">"));
  }
  else {
    return $std_core_types._lp__plus__plus__rp_("[", $std_core_types._lp__plus__plus__rp_(parser.help, "]"));
  }
}
 
export function show_long_flag(parser) /* forall<a> (parser : flag-parser<a>) -> string */  {
  if (parser._tag === 1) {
    return "";
  }
  else if (parser._tag === 2) {
    return $std_core_types._lp__plus__plus__rp_("=", parser.help);
  }
  else {
    return $std_core_types._lp__plus__plus__rp_("[=", $std_core_types._lp__plus__plus__rp_(parser.help, "]"));
  }
}
 
export function show_flag(flag) /* forall<a> (flag : flag<a>) -> list<(string, string, string)> */  {
   
  var _x54 = flag.short_names;
  var xs_10020 = $std_core_list.map($std_core_string.list(_x54), function(c /* char */ ) {
      if (flag.parser._tag === 1) {
        var _x55 = "";
      }
      else if (flag.parser._tag === 2) {
        var _x55 = $std_core_types._lp__plus__plus__rp_("<", $std_core_types._lp__plus__plus__rp_(flag.parser.help, ">"));
      }
      else {
        var _x55 = $std_core_types._lp__plus__plus__rp_("[", $std_core_types._lp__plus__plus__rp_(flag.parser.help, "]"));
      }
      return $std_core_types._lp__plus__plus__rp_("-", $std_core_types._lp__plus__plus__rp_($std_core_string.char_fs_string(c), _x55));
    });
   
  var short = $std_core_list.joinsep(xs_10020, " ");
   
  var _x56 = flag.long_names;
  var xs_0_10025 = $std_core_list.map(_x56, function(name /* string */ ) {
      if (flag.parser._tag === 1) {
        var _x57 = "";
      }
      else if (flag.parser._tag === 2) {
        var _x57 = $std_core_types._lp__plus__plus__rp_("=", flag.parser.help);
      }
      else {
        var _x57 = $std_core_types._lp__plus__plus__rp_("[=", $std_core_types._lp__plus__plus__rp_(flag.parser.help, "]"));
      }
      return $std_core_types._lp__plus__plus__rp_("--", $std_core_types._lp__plus__plus__rp_(name, _x57));
    });
   
  var long = $std_core_list.joinsep(xs_0_10025, " ");
   
  var _x58 = flag.help;
  var v_10011 = ((_x58).split(("\n")));
  var _x54 = $std_core_vector.vlist(v_10011);
  if (_x54 !== null) {
     
    var ys_10033 = $std_core_list.map(_x54.tail, function(s_0 /* string */ ) {
        return $std_core_types.Tuple3("", "", s_0);
      });
    return $std_core_list.append($std_core_types.Cons($std_core_types.Tuple3(short, long, _x54.head), $std_core_types.Nil), ys_10033);
  }
  else {
    return $std_core_types.Cons($std_core_types.Tuple3(short, long, ""), $std_core_types.Nil);
  }
}
 
export function breakon(s, c) /* (s : string, c : string) -> (string, string) */  {
   
  var v_10012 = (s).split(c, 2);
   
  var parts = $std_core_vector.vlist(v_10012);
  if (parts !== null && parts.tail !== null) {
    return $std_core_types.Tuple2(parts.head, parts.tail.head);
  }
  else {
    return $std_core_types.Tuple2(s, "");
  }
}
 
 
// monadic lift
export function _mlift_zipWith3Acc_10118(acc, f, xx, yy, zz, _y_x10109) /* forall<a,b,c,d,e> (acc : list<d>, f : (a, b, c) -> e d, xx : list<a>, yy : list<b>, zz : list<c>, d) -> e list<d> */  {
  return zipWith3Acc(f, $std_core_types.Cons(_y_x10109, acc), xx, yy, zz);
}
 
export function zipWith3Acc(f_0, acc_0, xs, ys, zs) /* forall<a,b,c,d,e> (f : (a, b, c) -> e d, acc : list<d>, xs : list<a>, ys : list<b>, zs : list<c>) -> e list<d> */  { tailcall: while(1)
{
  if (xs === null) {
    return $std_core_list.reverse_acc($std_core_types.Nil, acc_0);
  }
  else {
    if (ys === null) {
      return $std_core_list.reverse_acc($std_core_types.Nil, acc_0);
    }
    else {
      if (zs !== null) {
         
        var x_0_10149 = f_0(xs.head, ys.head, zs.head);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10109_0 /* 2217 */ ) {
            return _mlift_zipWith3Acc_10118(acc_0, f_0, xs.tail, ys.tail, zs.tail, _y_x10109_0);
          });
        }
        else {
          {
            // tail call
            var _x55 = $std_core_types.Cons(x_0_10149, acc_0);
            acc_0 = _x55;
            xs = xs.tail;
            ys = ys.tail;
            zs = zs.tail;
            continue tailcall;
          }
        }
      }
      else {
        return $std_core_list.reverse_acc($std_core_types.Nil, acc_0);
      }
    }
  }
}}
 
export function zipWith3(f, xs, ys, zs) /* forall<a,b,c,d,e> (f : (a, b, c) -> e d, xs : list<a>, ys : list<b>, zs : list<c>) -> e list<d> */  {
  return zipWith3Acc(f, $std_core_types.Nil, xs, ys, zs);
}
 
 
// Return a nicely formatted string describing the usage of a command,
// consisting of a `header` followed by the descriptions of the `flags`.
// The default header is ``"usage:\n program [flags] arguments\n\nflags:"``.
export function usage(flags, header) /* forall<a> (flags : list<flag<a>>, header : ? string) -> string */  {
   
  function align_left(xs) /* (xs : list<string>) -> list<string> */  {
     
    var xs_0_10037 = $std_core_list.map(xs, $std_core_string.chars_fs_count);
     
    if (xs_0_10037 === null) {
      var _x56 = undefined;
      var n = (_x56 !== undefined) ? _x56 : 0;
    }
    else {
      var n = $std_core_list.foldl(xs_0_10037.tail, xs_0_10037.head, $std_core_int.max);
    }
    return $std_core_list.map(xs, function(s_0 /* string */ ) {
        return $std_core_string.pad_right(s_0, n);
      });
  }
   
  var xss_10040 = $std_core_list.map(flags, show_flag);
   
  var xs_1_10039 = $std_core_list._lift_concat_6012($std_core_types.Nil, xss_10040);
  var _x56 = $std_core_list._lift_unzip3_6009(xs_1_10039, $std_core_types._cctx_empty(), $std_core_types._cctx_empty(), $std_core_types._cctx_empty());
   
  var xs_2_10042 = align_left(_x56.fst);
   
  var ys_10043 = align_left(_x56.snd);
   
  var table = zipWith3Acc(function(x_0 /* string */ , y /* string */ , z /* string */ ) {
      return $std_core_types._lp__plus__plus__rp_(" ", $std_core_types._lp__plus__plus__rp_(x_0, $std_core_types._lp__plus__plus__rp_("  ", $std_core_types._lp__plus__plus__rp_(y, $std_core_types._lp__plus__plus__rp_("  ", z)))));
    }, $std_core_types.Nil, xs_2_10042, ys_10043, _x56.thd);
  var _x57 = (header !== undefined) ? header : "usage:\n program [flags] arguments\n\nflags:";
  return $std_core_types._lp__plus__plus__rp_(_x57, $std_core_types._lp__plus__plus__rp_("\n", $std_core_list.unlines(table)));
}
 
 
// Automatically generated. Tests for the `Flg` constructor of the `:flag-kind` type.
export function is_flg(flag_kind) /* forall<a> (flag-kind : flag-kind<a>) -> bool */  {
  return (flag_kind._tag === 1);
}
 
 
// Automatically generated. Tests for the `Arg` constructor of the `:flag-kind` type.
export function is_arg(flag_kind) /* forall<a> (flag-kind : flag-kind<a>) -> bool */  {
  return (flag_kind._tag === 2);
}
 
 
// Automatically generated. Tests for the `End` constructor of the `:flag-kind` type.
export function is_end(flag_kind) /* forall<a> (flag-kind : flag-kind<a>) -> bool */  {
  return (flag_kind._tag === 3);
}
 
 
// Automatically generated. Tests for the `Unknown` constructor of the `:flag-kind` type.
export function is_unknown(flag_kind) /* forall<a> (flag-kind : flag-kind<a>) -> bool */  {
  return (flag_kind._tag === 4);
}
 
 
// Automatically generated. Tests for the `Error` constructor of the `:flag-kind` type.
export function is_error(flag_kind) /* forall<a> (flag-kind : flag-kind<a>) -> bool */  {
  return (flag_kind._tag === 5);
}
 
export function error_ambiguous(applicable, opt) /* forall<a,b> (applicable : list<flag<a>>, opt : string) -> flag-kind<b> */  {
   
  var header = $std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(opt, "\" is ambiguous. It could be one of:"));
  return $Error(usage(applicable, header));
}
 
export function error_required(help, opt) /* forall<a> (help : string, opt : string) -> flag-kind<a> */  {
  return $Error($std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(opt, $std_core_types._lp__plus__plus__rp_("\" requires an argument ", help))));
}
 
export function error_negate(flagname) /* forall<a> (flagname : string) -> flag-kind<a> */  {
  return $Error($std_core_types._lp__plus__plus__rp_("flag \"--", $std_core_types._lp__plus__plus__rp_(flagname, "\" cannot be negated")));
}
 
export function error_noarg(opt) /* forall<a> (opt : string) -> flag-kind<a> */  {
  return $Error($std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(opt, "\" does not take an argument")));
}
 
export function error_unknown_message(opt) /* (opt : string) -> string */  {
  return $std_core_types._lp__plus__plus__rp_("unrecognized flag \"", $std_core_types._lp__plus__plus__rp_(opt, "\""));
}
 
export function error_unknown(opt) /* forall<a> (opt : string) -> flag-kind<a> */  {
  return $Error($std_core_types._lp__plus__plus__rp_("unrecognized flag \"", $std_core_types._lp__plus__plus__rp_(opt, "\"")));
}
 
export function parse_long(s, flags) /* forall<a> (s : string, flags : list<flag<a>>) -> flag-kind<a> */  {
   
  var v_10012 = (s).split(("="), 2);
   
  var parts = $std_core_vector.vlist(v_10012);
  if (parts !== null && parts.tail !== null) {
     
    var optstr = $std_core_types._lp__plus__plus__rp_("--", s);
     
    var flagname = $std_core_string.to_lower(parts.head);
    var _x58 = $std_core_sslice.starts_with(flagname, "no-");
    if (_x58 !== null) {
       
      var baseflagname = $std_core_sslice.string(_x58.value);
       
      var exacts = $std_core_list.filter(flags, function(opt /* flag<4159> */ ) {
          var _x59 = opt.long_names;
          return $std_core_list.any($std_core_list.map(_x59, $std_core_string.to_lower), function(name /* string */ ) {
              return ((name === flagname)) ? true : (name === baseflagname);
            });
        });
       
      var prefixes = $std_core_list.filter(flags, function(opt_0 /* flag<4159> */ ) {
          var _x60 = opt_0.long_names;
          return $std_core_list.any($std_core_list.map(_x60, $std_core_string.to_lower), function(name_0 /* string */ ) {
               
              var maybe_10053 = $std_core_sslice.starts_with(name_0, flagname);
              if (maybe_10053 !== null) {
                return true;
              }
              else {
                 
                var maybe_0_10054 = $std_core_sslice.starts_with(name_0, baseflagname);
                return (maybe_0_10054 !== null);
              }
            });
        });
      var _x59 = (exacts === null) ? prefixes : exacts;
      if (_x59 === null) {
        return $Error($std_core_types._lp__plus__plus__rp_("unrecognized flag \"", $std_core_types._lp__plus__plus__rp_(optstr, "\"")));
      }
      else if (_x59 !== null && _x59.tail !== null) {
         
        var header = $std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr, "\" is ambiguous. It could be one of:"));
        var _x60 = (exacts === null) ? prefixes : exacts;
        return $Error(usage(_x60, header));
      }
      else {
        if (_x59.head.parser._tag === 1) {
          if (((parts.tail.head) === (""))) {
            return Flg(function(o /* 4159 */ ) {
              return _x59.head.parser.$default(o, false);
            });
          }
          else {
            var _x61 = (($std_core_string.to_lower(parts.tail.head)) === ("true"));
            if (_x61) {
              return Flg(function(o_0 /* 4159 */ ) {
                return _x59.head.parser.$default(o_0, true);
              });
            }
            else {
              var _x62 = (($std_core_string.to_lower(parts.tail.head)) === ("false"));
              if (_x62) {
                return Flg(function(o_1 /* 4159 */ ) {
                  return _x59.head.parser.$default(o_1, false);
                });
              }
              else {
                return $Error($std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr, "\" does not take an argument")));
              }
            }
          }
        }
        else if (_x59.head.parser._tag === 2) {
          return $Error($std_core_types._lp__plus__plus__rp_("flag \"--", $std_core_types._lp__plus__plus__rp_(baseflagname, "\" cannot be negated")));
        }
        else {
          return $Error($std_core_types._lp__plus__plus__rp_("flag \"--", $std_core_types._lp__plus__plus__rp_(baseflagname, "\" cannot be negated")));
        }
      }
    }
    else {
       
      var exacts_0 = $std_core_list.filter(flags, function(opt_1_0 /* flag<4159> */ ) {
          var _x63 = opt_1_0.long_names;
          return $std_core_list.any($std_core_list.map(_x63, $std_core_string.to_lower), function(name_1 /* string */ ) {
              return ((name_1 === flagname)) ? true : (name_1 === flagname);
            });
        });
       
      var prefixes_0 = $std_core_list.filter(flags, function(opt_0_0 /* flag<4159> */ ) {
          var _x64 = opt_0_0.long_names;
          return $std_core_list.any($std_core_list.map(_x64, $std_core_string.to_lower), function(name_0_0 /* string */ ) {
               
              var maybe_1_10065 = $std_core_sslice.starts_with(name_0_0, flagname);
              if (maybe_1_10065 !== null) {
                return true;
              }
              else {
                 
                var maybe_2_10066 = $std_core_sslice.starts_with(name_0_0, flagname);
                return (maybe_2_10066 !== null);
              }
            });
        });
      var _x63 = (exacts_0 === null) ? prefixes_0 : exacts_0;
      if (_x63 === null) {
        return $Error($std_core_types._lp__plus__plus__rp_("unrecognized flag \"", $std_core_types._lp__plus__plus__rp_(optstr, "\"")));
      }
      else if (_x63 !== null && _x63.tail !== null) {
         
        var header_0 = $std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr, "\" is ambiguous. It could be one of:"));
        var _x64 = (exacts_0 === null) ? prefixes_0 : exacts_0;
        return $Error(usage(_x64, header_0));
      }
      else {
        if (_x63.head.parser._tag === 1) {
          if (((parts.tail.head) === (""))) {
            return Flg(function(o_2 /* 4159 */ ) {
              return _x63.head.parser.$default(o_2, true);
            });
          }
          else {
            var _x65 = (($std_core_string.to_lower(parts.tail.head)) === ("true"));
            if (_x65) {
              return Flg(function(o_0_0 /* 4159 */ ) {
                return _x63.head.parser.$default(o_0_0, true);
              });
            }
            else {
              var _x66 = (($std_core_string.to_lower(parts.tail.head)) === ("false"));
              if (_x66) {
                return Flg(function(o_1_0 /* 4159 */ ) {
                  return _x63.head.parser.$default(o_1_0, false);
                });
              }
              else {
                return $Error($std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr, "\" does not take an argument")));
              }
            }
          }
        }
        else if (_x63.head.parser._tag === 2) {
          if (((parts.tail.head) !== (""))) {
            return Flg(function(o_2_0 /* 4159 */ ) {
              return _x63.head.parser.parse(o_2_0, parts.tail.head);
            });
          }
          else {
            return $Error($std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr, $std_core_types._lp__plus__plus__rp_("\" requires an argument ", _x63.head.parser.help))));
          }
        }
        else {
          if (((parts.tail.head) !== (""))) {
            return Flg(function(o_3 /* 4159 */ ) {
              return _x63.head.parser.parse(o_3, $std_core_types.Just(parts.tail.head));
            });
          }
          else {
            return Flg(function(o_4 /* 4159 */ ) {
              return _x63.head.parser.parse(o_4, $std_core_types.Nothing);
            });
          }
        }
      }
    }
  }
  else {
     
    var optstr_0 = $std_core_types._lp__plus__plus__rp_("--", s);
     
    var flagname_0 = $std_core_string.to_lower(s);
    var _x67 = $std_core_sslice.starts_with(flagname_0, "no-");
    if (_x67 !== null) {
       
      var baseflagname_0 = $std_core_sslice.string(_x67.value);
       
      var exacts_1 = $std_core_list.filter(flags, function(opt_1_1 /* flag<4159> */ ) {
          var _x68 = opt_1_1.long_names;
          return $std_core_list.any($std_core_list.map(_x68, $std_core_string.to_lower), function(name_2 /* string */ ) {
              return ((name_2 === flagname_0)) ? true : (name_2 === baseflagname_0);
            });
        });
       
      var prefixes_1 = $std_core_list.filter(flags, function(opt_0_1 /* flag<4159> */ ) {
          var _x69 = opt_0_1.long_names;
          return $std_core_list.any($std_core_list.map(_x69, $std_core_string.to_lower), function(name_0_1 /* string */ ) {
               
              var maybe_10053_0 = $std_core_sslice.starts_with(name_0_1, flagname_0);
              if (maybe_10053_0 !== null) {
                return true;
              }
              else {
                 
                var maybe_0_10054_0 = $std_core_sslice.starts_with(name_0_1, baseflagname_0);
                return (maybe_0_10054_0 !== null);
              }
            });
        });
      var _x68 = (exacts_1 === null) ? prefixes_1 : exacts_1;
      if (_x68 === null) {
        return $Error($std_core_types._lp__plus__plus__rp_("unrecognized flag \"", $std_core_types._lp__plus__plus__rp_(optstr_0, "\"")));
      }
      else if (_x68 !== null && _x68.tail !== null) {
         
        var header_1 = $std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr_0, "\" is ambiguous. It could be one of:"));
        var _x69 = (exacts_1 === null) ? prefixes_1 : exacts_1;
        return $Error(usage(_x69, header_1));
      }
      else {
        if (_x68.head.parser._tag === 1) {
          if ((("") === (""))) {
            return Flg(function(o_5 /* 4159 */ ) {
              return _x68.head.parser.$default(o_5, false);
            });
          }
          else {
            var _x70 = (($std_core_string.to_lower("")) === ("true"));
            if (_x70) {
              return Flg(function(o_0_1 /* 4159 */ ) {
                return _x68.head.parser.$default(o_0_1, true);
              });
            }
            else {
              var _x71 = (($std_core_string.to_lower("")) === ("false"));
              if (_x71) {
                return Flg(function(o_1_1 /* 4159 */ ) {
                  return _x68.head.parser.$default(o_1_1, false);
                });
              }
              else {
                return $Error($std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr_0, "\" does not take an argument")));
              }
            }
          }
        }
        else if (_x68.head.parser._tag === 2) {
          return $Error($std_core_types._lp__plus__plus__rp_("flag \"--", $std_core_types._lp__plus__plus__rp_(baseflagname_0, "\" cannot be negated")));
        }
        else {
          return $Error($std_core_types._lp__plus__plus__rp_("flag \"--", $std_core_types._lp__plus__plus__rp_(baseflagname_0, "\" cannot be negated")));
        }
      }
    }
    else {
       
      var exacts_0_0 = $std_core_list.filter(flags, function(opt_1_0_0 /* flag<4159> */ ) {
          var _x72 = opt_1_0_0.long_names;
          return $std_core_list.any($std_core_list.map(_x72, $std_core_string.to_lower), function(name_1_0 /* string */ ) {
              return ((name_1_0 === flagname_0)) ? true : (name_1_0 === flagname_0);
            });
        });
       
      var prefixes_0_0 = $std_core_list.filter(flags, function(opt_0_0_0 /* flag<4159> */ ) {
          var _x73 = opt_0_0_0.long_names;
          return $std_core_list.any($std_core_list.map(_x73, $std_core_string.to_lower), function(name_0_0_0 /* string */ ) {
               
              var maybe_1_10065_0 = $std_core_sslice.starts_with(name_0_0_0, flagname_0);
              if (maybe_1_10065_0 !== null) {
                return true;
              }
              else {
                 
                var maybe_2_10066_0 = $std_core_sslice.starts_with(name_0_0_0, flagname_0);
                return (maybe_2_10066_0 !== null);
              }
            });
        });
      var _x72 = (exacts_0_0 === null) ? prefixes_0_0 : exacts_0_0;
      if (_x72 === null) {
        return $Error($std_core_types._lp__plus__plus__rp_("unrecognized flag \"", $std_core_types._lp__plus__plus__rp_(optstr_0, "\"")));
      }
      else if (_x72 !== null && _x72.tail !== null) {
         
        var header_2 = $std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr_0, "\" is ambiguous. It could be one of:"));
        var _x73 = (exacts_0_0 === null) ? prefixes_0_0 : exacts_0_0;
        return $Error(usage(_x73, header_2));
      }
      else {
        if (_x72.head.parser._tag === 1) {
          if ((("") === (""))) {
            return Flg(function(o_2_1 /* 4159 */ ) {
              return _x72.head.parser.$default(o_2_1, true);
            });
          }
          else {
            var _x74 = (($std_core_string.to_lower("")) === ("true"));
            if (_x74) {
              return Flg(function(o_0_0_0 /* 4159 */ ) {
                return _x72.head.parser.$default(o_0_0_0, true);
              });
            }
            else {
              var _x75 = (($std_core_string.to_lower("")) === ("false"));
              if (_x75) {
                return Flg(function(o_1_0_0 /* 4159 */ ) {
                  return _x72.head.parser.$default(o_1_0_0, false);
                });
              }
              else {
                return $Error($std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr_0, "\" does not take an argument")));
              }
            }
          }
        }
        else if (_x72.head.parser._tag === 2) {
          if ((("") !== (""))) {
            return Flg(function(o_2_0_0 /* 4159 */ ) {
              return _x72.head.parser.parse(o_2_0_0, "");
            });
          }
          else {
            return $Error($std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr_0, $std_core_types._lp__plus__plus__rp_("\" requires an argument ", _x72.head.parser.help))));
          }
        }
        else {
          if ((("") !== (""))) {
            return Flg(function(o_3_0 /* 4159 */ ) {
              return _x72.head.parser.parse(o_3_0, $std_core_types.Just(""));
            });
          }
          else {
            return Flg(function(o_4_0 /* 4159 */ ) {
              return _x72.head.parser.parse(o_4_0, $std_core_types.Nothing);
            });
          }
        }
      }
    }
  }
}
 
export function parse_shorts(s, flags) /* forall<a> (s : string, flags : list<flag<a>>) -> list<flag-kind<a>> */  {
  return function() {
     
    var loc = { value: false };
     
    var fs = $std_core_list.map_indexed($std_core_string.list(s), function(i /* int */ , c /* char */ ) {
        var _x76 = ((loc).value);
        if (_x76) {
          return $std_core_types.Nothing;
        }
        else {
           
          var optstr = $std_core_types._lp__plus__plus__rp_("-", $std_core_string.char_fs_string(c));
           
          var applicable = $std_core_list.filter(flags, function(opt /* flag<4602> */ ) {
              var _x77 = opt.short_names;
              return ((_x77).indexOf(($std_core_string.char_fs_string(c))) >= 0);
            });
          if (applicable === null) {
            return $std_core_types.Just($Error($std_core_types._lp__plus__plus__rp_("unrecognized flag \"", $std_core_types._lp__plus__plus__rp_(optstr, "\""))));
          }
          else if (applicable !== null && applicable.tail !== null) {
            return $std_core_types.Just(error_ambiguous(applicable, optstr));
          }
          else {
            if (applicable.head.parser._tag === 1) {
              return $std_core_types.Just(Flg(function(o /* 4602 */ ) {
                return applicable.head.parser.$default(o, true);
              }));
            }
            else if (applicable.head.parser._tag === 2) {
               
              var arg = $std_core_sslice.string($std_core_sslice.after($std_core_sslice.advance($std_core_sslice.first(s), i)));
              if ((arg !== (""))) {
                 
                ((loc).value = true);
                return $std_core_types.Just(Flg(function(o_0 /* 4602 */ ) {
                  return applicable.head.parser.parse(o_0, arg);
                }));
              }
              else {
                return $std_core_types.Just($Error($std_core_types._lp__plus__plus__rp_("flag \"", $std_core_types._lp__plus__plus__rp_(optstr, $std_core_types._lp__plus__plus__rp_("\" requires an argument ", applicable.head.parser.help)))));
              }
            }
            else {
               
              var arg_0 = $std_core_sslice.string($std_core_sslice.after($std_core_sslice.advance($std_core_sslice.first(s), i)));
              if ((arg_0 !== (""))) {
                 
                ((loc).value = true);
                return $std_core_types.Just(Flg(function(o_1 /* 4602 */ ) {
                  return applicable.head.parser.parse(o_1, $std_core_types.Just(arg_0));
                }));
              }
              else {
                return $std_core_types.Just(Flg(function(o_2 /* 4602 */ ) {
                  return applicable.head.parser.parse(o_2, $std_core_types.Nothing);
                }));
              }
            }
          }
        }
      });
     
    var res = $std_core_list.flatmap(fs, $std_core_list.maybe_fs_list);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
export function process_next(arg, flags) /* forall<a> (arg : string, flags : list<flag<a>>) -> list<flag-kind<a>> */  {
  var _x76 = $std_core_sslice.starts_with(arg, "--");
  if (_x76 !== null) {
     
    var _x77 = _x76.value.len;
    var b_10032 = $std_core_types._int_gt(_x77,0);
    if (b_10032) {
      return $std_core_types.Cons(parse_long($std_core_sslice.string(_x76.value), flags), $std_core_types.Nil);
    }
    else {
      return $std_core_types.Cons(End, $std_core_types.Nil);
    }
  }
  else {
    var _x77 = $std_core_sslice.starts_with(arg, "-");
    if (_x77 !== null) {
       
      var _x78 = _x77.value.len;
      var b_10032_0 = $std_core_types._int_gt(_x78,0);
      var _x78 = (b_10032_0);
      if (_x78){
        return parse_shorts($std_core_sslice.string(_x77.value), flags);
      }
    }
    return $std_core_types.Cons(Arg(arg), $std_core_types.Nil);
  }
}
 
 
// Parse the command line arguments `args` (see `std/env/get-args`)
// according to the flag descriptions `flags`. Takes an optional argument
// `ordering` (=`Permute`) that specifies how flags are handled that follow non-flag arguments.
// Returns three lists: the list of parsed flags,
// a list of non-flag arguments, and a list of potential error messages.
export function parse(initial, flags, args, ordering) /* forall<a> (initial : a, flags : list<flag<a>>, args : list<string>, ordering : ? (flag-order<a>)) -> (a, list<string>, list<string>) */  {
  return function() {
     
    var loc = { value: false };
     
    var opts_0 = $std_core_list.map(args, function(arg /* string */ ) {
         
        var _x79 = ((loc).value);
        if (_x79) {
          var opts = $std_core_types.Cons(Arg(arg), $std_core_types.Nil);
        }
        else {
          var opts = process_next(arg, flags);
        }
         
        $std_core_list.foreach(opts, function(opt /* flag-kind<5158> */ ) {
            if (opt._tag === 3) {
              return ((loc).value = true);
            }
            if (opt._tag === 2) {
              var _x81 = (ordering !== undefined) ? ordering : Permute;
              var _x80 = (_x81._tag === 2);
              if (_x80){
                return ((loc).value = true);
              }
            }
            return $std_core_types.Unit;
          });
        return opts;
      });
     
    var res = $std_core_list.foldl($std_core_list._lift_concat_6012($std_core_types.Nil, opts_0), $std_core_types.Tuple3(initial, $std_core_types.Nil, $std_core_types.Nil), function(acc /* (5158, list<string>, list<string>) */ , opt_0 /* flag-kind<5158> */ ) {
        if (opt_0._tag === 1) {
          return $std_core_types.Tuple3(opt_0.set(acc.fst), acc.snd, acc.thd);
        }
        else if (opt_0._tag === 4) {
          return $std_core_types.Tuple3(acc.fst, acc.snd, $std_core_types.Cons($std_core_types._lp__plus__plus__rp_("unrecognized flag \"", $std_core_types._lp__plus__plus__rp_(opt_0.arg, "\"")), acc.thd));
        }
        else if (opt_0._tag === 5) {
          return $std_core_types.Tuple3(acc.fst, acc.snd, $std_core_types.Cons(opt_0.msg, acc.thd));
        }
        else if (opt_0._tag === 2) {
          return $std_core_types.Tuple3(acc.fst, $std_core_types.Cons(opt_0.arg, acc.snd), acc.thd);
        }
        else {
          return $std_core_types.Tuple3(acc.fst, acc.snd, acc.thd);
        }
      });
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
export function test(cmdargs) /* (cmdargs : list<string>) -> console/console () */  {
  var _x81 = undefined;
  var _x80 = (_x81 !== undefined) ? _x81 : false;
  var _x83 = undefined;
  var _x82 = (_x83 !== undefined) ? _x83 : false;
  var _x85 = undefined;
  var _x84 = (_x85 !== undefined) ? _x85 : "";
  var _x87 = undefined;
  var _x86 = (_x87 !== undefined) ? _x87 : "";
  var _x89 = undefined;
  var _x88 = (_x89 !== undefined) ? _x89 : $std_core_types.Nil;
  var _x79 = parse(Myflags(_x80, _x82, _x84, _x86, _x88), myflags, cmdargs);
  if (_x79.thd === null) {
     
    $std_core_console.printsln("\nsuccess!");
     
    var s_0_10100 = $std_core_types._lp__plus__plus__rp_("flags: ", show_flags(_x79.fst));
     
    $std_core_console.printsln(s_0_10100);
     
    var s_1_10101 = $std_core_types._lp__plus__plus__rp_("arguments: ", $std_core_list.joinsep(_x79.snd, " "));
     
    $std_core_console.printsln(s_1_10101);
    if (_x79.fst.version) {
       
      var s_2_10105 = usage(myflags, "usage:\n program [options] files\n\noptions:");
      return $std_core_console.printsln(s_2_10105);
    }
    else {
      return $std_core_types.Unit;
    }
  }
  else {
     
    var s_3_10106 = $std_core_types._lp__plus__plus__rp_($std_core_list.joinsep(_x79.thd, "\n"), $std_core_types._lp__plus__plus__rp_("\n", usage(myflags, "usage:\n program [options] files\n\noptions:")));
    return $std_core_console.printsln(s_3_10106);
  }
}
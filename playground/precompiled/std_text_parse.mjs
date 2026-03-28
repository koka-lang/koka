// Koka generated module: std/text/parse, koka version: 3.2.4
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
import * as $std_core_undiv from './std_core_undiv.mjs';
 
// externals
 
// type declarations
 
 
// runtime tag for the effect `:parse`
export var parse_fs__tag;
var parse_fs__tag = "parse@parse";
// type parse
export function _Hnd_parse(_cfc, _fun_current_input, _brk_fail, _ctl_pick, _fun_satisfy) /* forall<e,a> (int, hnd/clause0<sslice/sslice,parse,e,a>, forall<b> hnd/clause1<string,b,parse,e,a>, hnd/clause0<bool,parse,e,a>, forall<b> hnd/clause1<(sslice/sslice) -> maybe<(b, sslice/sslice)>,maybe<b>,parse,e,a>) -> parse<e,a> */  {
  return { _cfc: _cfc, _fun_current_input: _fun_current_input, _brk_fail: _brk_fail, _ctl_pick: _ctl_pick, _fun_satisfy: _fun_satisfy };
}
// type parse-error
export function ParseOk(result, rest) /* forall<a> (result : a, rest : sslice/sslice) -> parse-error<a> */  {
  return { _tag: 1, result: result, rest: rest };
}
export function ParseError(msg, rest) /* forall<a> (msg : string, rest : sslice/sslice) -> parse-error<a> */  {
  return { _tag: 2, msg: msg, rest: rest };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:parse` type.
export function parse_fs__cfc(parse_0) /* forall<e,a> (parse : parse<e,a>) -> int */  {
  return parse_0._cfc;
}
 
 
// handler for the effect `:parse`
export function parse_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : parse<e,b>, ret : (res : a) -> e b, action : () -> <parse|e> a) -> e b */  {
  return $std_core_hnd._hhandle(parse_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-satisfy` constructor field of the `:parse` type.
export function parse_fs__fun_satisfy(parse_0) /* forall<e,a,b> (parse : parse<e,a>) -> hnd/clause1<(sslice/sslice) -> maybe<(b, sslice/sslice)>,maybe<b>,parse,e,a> */  {
  return parse_0._fun_satisfy;
}
 
 
// select `satisfy` operation out of effect `:parse`
export function satisfy_fs__select(hnd) /* forall<a,e,b> (hnd : parse<e,b>) -> hnd/clause1<(sslice/sslice) -> maybe<(a, sslice/sslice)>,maybe<a>,parse,e,b> */  {
  return hnd._fun_satisfy;
}
 
 
// See also `satisfy-fail` which invokes `fail` when `pred` returns `Nothing`.
// Call the `fun satisfy` operation of the effect `:parse`
export function satisfy(pred) /* forall<a> (pred : (sslice/sslice) -> maybe<(a, sslice/sslice)>) -> parse maybe<a> */  {
   
  var ev_10203 = $std_core_hnd._evv_at(0);
  var _x0 = ev_10203.hnd._fun_satisfy;
  return _x0(ev_10203.marker, ev_10203, pred);
}
 
 
// Automatically generated. Retrieves the `@brk-fail` constructor field of the `:parse` type.
export function parse_fs__brk_fail(parse_0) /* forall<e,a,b> (parse : parse<e,a>) -> hnd/clause1<string,b,parse,e,a> */  {
  return parse_0._brk_fail;
}
 
 
// select `fail` operation out of effect `:parse`
export function fail_fs__select(hnd) /* forall<a,e,b> (hnd : parse<e,b>) -> hnd/clause1<string,a,parse,e,b> */  {
  return hnd._brk_fail;
}
 
 
// Fails the current branch of parsing.
// Call the `final ctl fail` operation of the effect `:parse`
export function fail(msg) /* forall<a> (msg : string) -> parse a */  {
   
  var ev_10206 = $std_core_hnd._evv_at(0);
  var _x1 = ev_10206.hnd._brk_fail;
  return _x1(ev_10206.marker, ev_10206, msg);
}
 
 
// Automatically generated. Retrieves the `@ctl-pick` constructor field of the `:parse` type.
export function parse_fs__ctl_pick(parse_0) /* forall<e,a> (parse : parse<e,a>) -> hnd/clause0<bool,parse,e,a> */  {
  return parse_0._ctl_pick;
}
 
 
// select `pick` operation out of effect `:parse`
export function pick_fs__select(hnd) /* forall<e,a> (hnd : parse<e,a>) -> hnd/clause0<bool,parse,e,a> */  {
  return hnd._ctl_pick;
}
 
 
// Handlers usually resume up to twice with `True` and `False`.
// Call the `ctl pick` operation of the effect `:parse`
export function pick() /* () -> parse bool */  {
   
  var ev_10209 = $std_core_hnd._evv_at(0);
  return ev_10209.hnd._ctl_pick(ev_10209.marker, ev_10209);
}
 
 
// Automatically generated. Retrieves the `@fun-current-input` constructor field of the `:parse` type.
export function parse_fs__fun_current_input(parse_0) /* forall<e,a> (parse : parse<e,a>) -> hnd/clause0<sslice/sslice,parse,e,a> */  {
  return parse_0._fun_current_input;
}
 
 
// select `current-input` operation out of effect `:parse`
export function current_input_fs__select(hnd) /* forall<e,a> (hnd : parse<e,a>) -> hnd/clause0<sslice/sslice,parse,e,a> */  {
  return hnd._fun_current_input;
}
 
 
// The current position of parse on the input.
// Call the `fun current-input` operation of the effect `:parse`
export function current_input() /* () -> parse sslice/sslice */  {
   
  var ev_10211 = $std_core_hnd._evv_at(0);
  return ev_10211.hnd._fun_current_input(ev_10211.marker, ev_10211);
}
 
 
// Automatically generated. Tests for the `ParseOk` constructor of the `:parse-error` type.
export function is_parseOk(parse_error) /* forall<a> (parse-error : parse-error<a>) -> bool */  {
  return (parse_error._tag === 1);
}
 
 
// Automatically generated. Retrieves the `rest` constructor field of the `:parse-error` type.
export function parse_error_fs_rest(_this) /* forall<a> (parse-error<a>) -> sslice/sslice */  {
  return (_this._tag === 1) ? _this.rest : _this.rest;
}
 
 
// Automatically generated. Tests for the `ParseError` constructor of the `:parse-error` type.
export function is_parseError(parse_error) /* forall<a> (parse-error : parse-error<a>) -> bool */  {
  return (parse_error._tag === 2);
}
 
export function either(perr) /* forall<a> (perr : parse-error<a>) -> either<string,a> */  {
  if (perr._tag === 1) {
    return $std_core_types.Right(perr.result);
  }
  else {
    return $std_core_types.Left(perr.msg);
  }
}
 
export function maybe(perr) /* forall<a> (perr : parse-error<a>) -> maybe<a> */  {
  if (perr._tag === 1) {
    return $std_core_types.Just(perr.result);
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// monadic lift
export function _mlift_parse_10176(msg, _y_x10075) /* forall<h,a,e> (msg : string, sslice/sslice) -> <local<h>|e> parse-error<a> */  {
  return ParseError(msg, _y_x10075);
}
 
 
// monadic lift
export function _mlift_parse_10177(err1, _y_x10079) /* forall<h,a,e> (err1 : parse-error<a>, parse-error<a>) -> <local<h>|e> parse-error<a> */  {
  if (_y_x10079._tag === 1) {
    return ParseOk(_y_x10079.result, _y_x10079.rest);
  }
  else {
    return err1;
  }
}
 
 
// monadic lift
export function _mlift_parse_10178(err1, resume, wild__) /* forall<h,a,e> (err1 : parse-error<a>, resume : (bool) -> <local<h>|e> parse-error<a>, wild_ : ()) -> <local<h>|e> parse-error<a> */  {
   
  var x_10213 = resume(false);
   
  function next_10214(_y_x10079) /* (parse-error<1460>) -> <local<1450>|1461> parse-error<1460> */  {
    if (_y_x10079._tag === 1) {
      return ParseOk(_y_x10079.result, _y_x10079.rest);
    }
    else {
      return err1;
    }
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10214);
  }
  else {
    return next_10214(x_10213);
  }
}
 
 
// monadic lift
export function _mlift_parse_10179(input, resume, save, _y_x10077) /* forall<h,a,e> (input : local-var<h,sslice/sslice>, resume : (bool) -> <local<h>|e> parse-error<a>, save : sslice/sslice, parse-error<a>) -> <local<h>|e> parse-error<a> */  {
  if (_y_x10077._tag === 1) {
    return ParseOk(_y_x10077.result, _y_x10077.rest);
  }
  else {
     
    var x_10217 = ((input).value = save);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
        return _mlift_parse_10178(_y_x10077, resume, wild__);
      });
    }
    else {
      return _mlift_parse_10178(_y_x10077, resume, x_10217);
    }
  }
}
 
 
// monadic lift
export function _mlift_parse_10180(input, resume, save) /* forall<h,a,e> (input : local-var<h,sslice/sslice>, resume : (bool) -> <local<h>|e> parse-error<a>, save : sslice/sslice) -> <local<h>|e> parse-error<a> */  {
   
  var x_10219 = resume(true);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10077 /* parse-error<1460> */ ) {
      return _mlift_parse_10179(input, resume, save, _y_x10077);
    });
  }
  else {
    return _mlift_parse_10179(input, resume, save, x_10219);
  }
}
 
 
// monadic lift
export function _mlift_parse_10181(x, wild___0) /* forall<a,h,e> (x : a, wild_@0 : ()) -> <local<h>|e> maybe<a> */  {
  return $std_core_types.Just(x);
}
 
 
// monadic lift
export function _mlift_parse_10182(input, pred, inp) /* forall<a,h,e> (input : local-var<h,sslice/sslice>, pred : (sslice/sslice) -> maybe<(a, sslice/sslice)>, inp : sslice/sslice) -> <local<h>|e> maybe<a> */  {
  var _x2 = pred(inp);
  if (_x2 !== null) {
     
    var x_0_10221 = ((input).value = (_x2.value.snd));
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___0 /* () */ ) {
        return $std_core_types.Just(_x2.value.fst);
      });
    }
    else {
      return $std_core_types.Just(_x2.value.fst);
    }
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// monadic lift
export function _mlift_parse_10183(x_0, _y_x10084) /* forall<h,a,e> (x@0 : a, sslice/sslice) -> <local<h>|e> parse-error<a> */  {
  return ParseOk(x_0, _y_x10084);
}
 
 
// Parses `input0` with `p`.
//
// This implements the `pick` ctl using a depth-first search,
//   short-circuiting if the `True` branch succeeds.
// If both alternatives of a `pick` invoke `fail`, 
//   the first error will be returned.
export function parse(input0, p) /* forall<a,e> (input0 : sslice/sslice, p : () -> <parse|e> a) -> e parse-error<a> */  {
  return function() {
     
    var loc = { value: input0 };
     
    var res = parse_fs__handle(_Hnd_parse(3, $std_core_hnd.clause_tail0(function() {
          return ((loc).value);
        }), function(m /* hnd/marker<<local<1450>|1461>,parse-error<1460>> */ , ___wildcard_x654__16 /* hnd/ev<parse> */ , x /* string */ ) {
          return $std_core_hnd.yield_to_final(m, function(___wildcard_x654__45 /* (hnd/resume-result<1228,parse-error<1460>>) -> <local<1450>|1461> parse-error<1460> */ ) {
               
              var x_0_10228 = ((loc).value);
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(function(_y_x10075 /* sslice/sslice */ ) {
                  return ParseError(x, _y_x10075);
                });
              }
              else {
                return ParseError(x, x_0_10228);
              }
            });
        }, function(m_0 /* hnd/marker<<local<1450>|1461>,parse-error<1460>> */ , ___wildcard_x688__16 /* hnd/ev<parse> */ ) {
          return $std_core_hnd.yield_to(m_0, function(k /* (hnd/resume-result<bool,parse-error<1460>>) -> <local<1450>|1461> parse-error<1460> */ ) {
              return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (bool) -> <local<1450>|1461> parse-error<1460> */ ) {
                   
                  var x_1_10233 = ((loc).value);
                  if ($std_core_hnd._yielding()) {
                    return $std_core_hnd.yield_extend(function(save /* sslice/sslice */ ) {
                      return _mlift_parse_10180(loc, r, save);
                    });
                  }
                  else {
                    return _mlift_parse_10180(loc, r, x_1_10233);
                  }
                }, k);
            });
        }, $std_core_hnd.clause_tail1(function(pred /* (sslice/sslice) -> maybe<(1399, sslice/sslice)> */ ) {
           
          var x_2_10235 = ((loc).value);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(inp /* sslice/sslice */ ) {
              return _mlift_parse_10182(loc, pred, inp);
            });
          }
          else {
            return _mlift_parse_10182(loc, pred, x_2_10235);
          }
        })), function(x_0_0 /* 1460 */ ) {
         
        var x_3_10237 = ((loc).value);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10084 /* sslice/sslice */ ) {
            return ParseOk(x_0_0, _y_x10084);
          });
        }
        else {
          return ParseOk(x_0_0, x_3_10237);
        }
      }, p);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
export function starts_with(s, p) /* forall<a> (s : string, p : () -> parse a) -> maybe<(a, sslice/sslice)> */  {
  var _x3 = parse($std_core_sslice.Sslice(s, 0, s.length), p);
  if (_x3._tag === 1) {
    return $std_core_types.Just($std_core_types.Tuple2(_x3.result, _x3.rest));
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// monadic lift
export function _lp__at_mlift_x_10184_bar__bar__rp_(p1, p2, _y_x10090) /* forall<a,e> (p1 : parser<e,a>, p2 : parser<e,a>, bool) -> <parse|e> a */  {
  if (_y_x10090) {
    return p1();
  }
  else {
    return p2();
  }
}
 
export function _lp__bar__bar__rp_(p1, p2) /* forall<a,e> (p1 : parser<e,a>, p2 : parser<e,a>) -> <parse|e> a */  {
   
  var x_10242 = $std_core_hnd._open_at0($std_core_hnd._evv_index(parse_fs__tag), function() {
       
      var ev_10245 = $std_core_hnd._evv_at(0);
      return ev_10245.hnd._ctl_pick(ev_10245.marker, ev_10245);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10090 /* bool */ ) {
      if (_y_x10090) {
        return p1();
      }
      else {
        return p2();
      }
    });
  }
  else {
    if (x_10242) {
      return p1();
    }
    else {
      return p2();
    }
  }
}
 
export function optional($default, p) /* forall<a,e> (default : a, p : parser<e,a>) -> <parse|e> a */  {
  return _lp__bar__bar__rp_(p, function() {
      return $default;
    });
}
 
 
// monadic lift
export function _mlift_choose_10185(p_0, pp, _y_x10099) /* forall<a,e> (p@0 : parser<e,a>, pp : list<parser<e,a>>, bool) -> <parse|e> a */  {
  if (_y_x10099) {
    return p_0();
  }
  else {
    return choose(pp);
  }
}
 
export function choose(ps) /* forall<a,e> (ps : list<parser<e,a>>) -> <parse|e> a */  { tailcall: while(1)
{
  if (ps === null) {
    return $std_core_hnd._open_at1($std_core_hnd._evv_index(parse_fs__tag), function(msg /* string */ ) {
         
        var ev_10250 = $std_core_hnd._evv_at(0);
        var _x4 = ev_10250.hnd._brk_fail;
        return _x4(ev_10250.marker, ev_10250, msg);
      }, "no further alternatives");
  }
  else if (ps !== null && ps.tail === null) {
    return ps.head();
  }
  else {
     
    var x_0_10253 = $std_core_hnd._open_at0($std_core_hnd._evv_index(parse_fs__tag), function() {
         
        var ev_0_10256 = $std_core_hnd._evv_at(0);
        return ev_0_10256.hnd._ctl_pick(ev_0_10256.marker, ev_0_10256);
      });
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10099_0 /* bool */ ) {
        return _mlift_choose_10185(ps.head, ps.tail, _y_x10099_0);
      });
    }
    else {
      if (x_0_10253) {
        return ps.head();
      }
      else {
        {
          // tail call
          ps = ps.tail;
          continue tailcall;
        }
      }
    }
  }
}}
 
 
// monadic lift
export function _mlift_satisfy_fail_10186(msg, _y_x10104) /* forall<a> (msg : string, maybe<a>) -> parse a */  {
  if (_y_x10104 === null) {
     
    var ev_10258 = $std_core_hnd._evv_at(0);
    var _x5 = ev_10258.hnd._brk_fail;
    return _x5(ev_10258.marker, ev_10258, msg);
  }
  else {
    return _y_x10104.value;
  }
}
 
export function satisfy_fail(msg, pred) /* forall<a> (msg : string, pred : (sslice/sslice) -> maybe<(a, sslice/sslice)>) -> parse a */  {
   
  var ev_10264 = $std_core_hnd._evv_at(0);
   
  var _x6 = ev_10264.hnd._fun_satisfy;
  var x_10261 = _x6(ev_10264.marker, ev_10264, pred);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10104 /* maybe<1687> */ ) {
      return _mlift_satisfy_fail_10186(msg, _y_x10104);
    });
  }
  else {
    if (x_10261 === null) {
       
      var ev_0_10267 = $std_core_hnd._evv_at(0);
      var _x6 = ev_0_10267.hnd._brk_fail;
      return _x6(ev_0_10267.marker, ev_0_10267, msg);
    }
    else {
      return x_10261.value;
    }
  }
}
 
 
// monadic lift
export function _mlift_eof_10187(_y_x10107) /* (maybe<()>) -> parse () */  {
  if (_y_x10107 === null) {
     
    var ev_10270 = $std_core_hnd._evv_at(0);
    var _x7 = ev_10270.hnd._brk_fail;
    return _x7(ev_10270.marker, ev_10270, "expecting end-of-input");
  }
  else {
    return $std_core_types.Unit;
  }
}
 
export function eof() /* () -> parse () */  {
   
  var ev_10276 = $std_core_hnd._evv_at(0);
   
  var _x8 = ev_10276.hnd._fun_satisfy;
  var x_10273 = _x8(ev_10276.marker, ev_10276, function(s /* sslice/sslice */ ) {
       
      var _x9 = s.len;
      var b_10032 = $std_core_types._int_gt(_x9,0);
      if (b_10032) {
        return $std_core_types.Nothing;
      }
      else {
        return $std_core_types.Just($std_core_types.Tuple2($std_core_types.Unit, s));
      }
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_eof_10187);
  }
  else {
    if (x_10273 === null) {
       
      var ev_0_10279 = $std_core_hnd._evv_at(0);
      var _x8 = ev_0_10279.hnd._brk_fail;
      return _x8(ev_0_10279.marker, ev_0_10279, "expecting end-of-input");
    }
    else {
      return $std_core_types.Unit;
    }
  }
}
 
 
// monadic lift
export function _mlift_parse_eof_10188(x, wild__) /* forall<a,e> (x : a, wild_ : ()) -> <parse|e> a */  {
  return x;
}
 
 
// monadic lift
export function _mlift_parse_eof_10189(x) /* forall<a,e> (x : a) -> <parse|e> a */  {
   
  var x_0_10282 = $std_core_hnd._open_at0($std_core_hnd._evv_index(parse_fs__tag), eof);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return x;
    });
  }
  else {
    return x;
  }
}
 
export function parse_eof(input, p) /* forall<a,e> (input : sslice/sslice, p : () -> <parse|e> a) -> e parse-error<a> */  {
  return parse(input, function() {
       
      var x_10286 = p();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(x_0 /* 1784 */ ) {
          return _mlift_parse_eof_10189(x_0);
        });
      }
      else {
        return _mlift_parse_eof_10189(x_10286);
      }
    });
}
 
export function char_is(msg, pred) /* (msg : string, pred : (char) -> bool) -> parse char */  {
  return satisfy_fail(msg, function(slice /* sslice/sslice */ ) {
      var _x9 = $std_core_sslice.next(slice);
      if (_x9 !== null) {
        if (pred(_x9.value.fst)){
          return $std_core_types.Just($std_core_types.Tuple2(_x9.value.fst, _x9.value.snd));
        }
      }
      return $std_core_types.Nothing;
    });
}
 
export function next_while0(slice, pred, acc) /* (slice : sslice/sslice, pred : (char) -> bool, acc : list<char>) -> (list<char>, sslice/sslice) */  { tailcall: while(1)
{
  var _x10 = $std_core_sslice.next(slice);
  if (_x10 !== null) {
    if (pred(_x10.value.fst)){
      {
        // tail call
        var _x11 = $std_core_types.Cons(_x10.value.fst, acc);
        slice = _x10.value.snd;
        acc = _x11;
        continue tailcall;
      }
    }
  }
  return $std_core_types.Tuple2($std_core_list.reverse_acc($std_core_types.Nil, acc), slice);
}}
 
export function chars_are(msg, pred) /* (msg : string, pred : (char) -> bool) -> parse list<char> */  {
  return satisfy_fail(msg, function(slice /* sslice/sslice */ ) {
      var _x12 = next_while0(slice, pred, $std_core_types.Nil);
      if (_x12.fst === null) {
        return $std_core_types.Nothing;
      }
      else {
        return $std_core_types.Just($std_core_types.Tuple2(_x12.fst, _x12.snd));
      }
    });
}
 
export function next_match(slice, cs) /* (slice : sslice/sslice, cs : list<char>) -> maybe<sslice/sslice> */  { tailcall: while(1)
{
  if (cs === null) {
    return $std_core_types.Just(slice);
  }
  else {
    var _x13 = $std_core_sslice.next(slice);
    if (_x13 !== null) {
      if (((cs.head) === (_x13.value.fst))){
        {
          // tail call
          slice = _x13.value.snd;
          cs = cs.tail;
          continue tailcall;
        }
      }
    }
    return $std_core_types.Nothing;
  }
}}
 
export function pstring(s) /* (s : string) -> parse string */  {
  return satisfy_fail(s, function(slice /* sslice/sslice */ ) {
      var _x14 = next_match(slice, $std_core_string.list(s));
      if (_x14 !== null) {
        return $std_core_types.Just($std_core_types.Tuple2(s, _x14.value));
      }
      else {
        return $std_core_types.Nothing;
      }
    });
}
 
export function char(c) /* (c : char) -> parse char */  {
   
  var msg_10010 = $std_core_types._lp__plus__plus__rp_("\'", $std_core_types._lp__plus__plus__rp_($std_core_show.show_char(c), "\'"));
  return satisfy_fail(msg_10010, function(slice /* sslice/sslice */ ) {
      var _x15 = $std_core_sslice.next(slice);
      if (_x15 !== null) {
        if ((c === (_x15.value.fst))){
          return $std_core_types.Just($std_core_types.Tuple2(_x15.value.fst, _x15.value.snd));
        }
      }
      return $std_core_types.Nothing;
    });
}
 
export function no_digit() /* () -> parse char */  {
  return satisfy_fail("not a digit", function(slice /* sslice/sslice */ ) {
      var _x16 = $std_core_sslice.next(slice);
      if (_x16 !== null) {
         
        var b_10014 = (((_x16.value.fst) >= 0x0030)) ? ((_x16.value.fst) <= 0x0039) : false;
        var _x17 = (b_10014) ? false : true;
        if (_x17){
          return $std_core_types.Just($std_core_types.Tuple2(_x16.value.fst, _x16.value.snd));
        }
      }
      return $std_core_types.Nothing;
    });
}
 
 
// monadic lift
export function _mlift_digit_10190(c_0_0) /* (c@0@0 : char) -> parse int */  {
  return ($std_core_hnd._open_none2(function(c_1 /* char */ , d /* char */ ) {
       
      var x_10002 = c_1;
       
      var y_10003 = d;
      return (($std_core_types._int_sub(x_10002,y_10003)));
    }, c_0_0, 0x0030));
}
 
export function digit() /* () -> parse int */  {
   
  var x_10288 = satisfy_fail("digit", function(slice /* sslice/sslice */ ) {
      var _x18 = $std_core_sslice.next(slice);
      if (_x18 !== null) {
        var _x19 = (((_x18.value.fst) >= 0x0030)) ? ((_x18.value.fst) <= 0x0039) : false;
        if (_x19){
          return $std_core_types.Just($std_core_types.Tuple2(_x18.value.fst, _x18.value.snd));
        }
      }
      return $std_core_types.Nothing;
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_digit_10190);
  }
  else {
    return ($std_core_hnd._open_none2(function(c_1 /* char */ , d /* char */ ) {
         
        var x_10002 = c_1;
         
        var y_10003 = d;
        return (($std_core_types._int_sub(x_10002,y_10003)));
      }, x_10288, 0x0030));
  }
}
 
export function alpha() /* () -> parse char */  {
  return satisfy_fail("alpha", function(slice /* sslice/sslice */ ) {
      var _x18 = $std_core_sslice.next(slice);
      if (_x18 !== null) {
        if ($std_core_char.is_alpha(_x18.value.fst)){
          return $std_core_types.Just($std_core_types.Tuple2(_x18.value.fst, _x18.value.snd));
        }
      }
      return $std_core_types.Nothing;
    });
}
 
export function alpha_num() /* () -> parse char */  {
  return satisfy_fail("alpha-num", function(slice /* sslice/sslice */ ) {
      var _x19 = $std_core_sslice.next(slice);
      if (_x19 !== null) {
        if ($std_core_char.is_alpha_num(_x19.value.fst)){
          return $std_core_types.Just($std_core_types.Tuple2(_x19.value.fst, _x19.value.snd));
        }
      }
      return $std_core_types.Nothing;
    });
}
 
export function white() /* () -> parse char */  {
  return satisfy_fail("", function(slice /* sslice/sslice */ ) {
      var _x20 = $std_core_sslice.next(slice);
      if (_x20 !== null) {
        if ($std_core_char.is_white(_x20.value.fst)){
          return $std_core_types.Just($std_core_types.Tuple2(_x20.value.fst, _x20.value.snd));
        }
      }
      return $std_core_types.Nothing;
    });
}
 
export function whitespace() /* () -> parse string */  {
   
  var x_10291 = satisfy_fail("", function(slice /* sslice/sslice */ ) {
      var _x21 = next_while0(slice, $std_core_char.is_white, $std_core_types.Nil);
      if (_x21.fst === null) {
        return $std_core_types.Nothing;
      }
      else {
        return $std_core_types.Just($std_core_types.Tuple2(_x21.fst, _x21.snd));
      }
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend($std_core_string.listchar_fs_string);
  }
  else {
    return $std_core_string.listchar_fs_string(x_10291);
  }
}
 
export function whitespace0() /* () -> parse string */  {
  return _lp__bar__bar__rp_(function() {
       
      var x_10293 = satisfy_fail("", function(slice /* sslice/sslice */ ) {
          var _x21 = next_while0(slice, $std_core_char.is_white, $std_core_types.Nil);
          if (_x21.fst === null) {
            return $std_core_types.Nothing;
          }
          else {
            return $std_core_types.Just($std_core_types.Tuple2(_x21.fst, _x21.snd));
          }
        });
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend($std_core_string.listchar_fs_string);
      }
      else {
        return $std_core_string.listchar_fs_string(x_10293);
      }
    }, function() {
      return "";
    });
}
 
export function digits() /* () -> parse string */  {
   
  var x_10295 = satisfy_fail("digit", function(slice /* sslice/sslice */ ) {
      var _x21 = next_while0(slice, $std_core_char.is_digit, $std_core_types.Nil);
      if (_x21.fst === null) {
        return $std_core_types.Nothing;
      }
      else {
        return $std_core_types.Just($std_core_types.Tuple2(_x21.fst, _x21.snd));
      }
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend($std_core_string.listchar_fs_string);
  }
  else {
    return $std_core_string.listchar_fs_string(x_10295);
  }
}
 
export function digits0() /* () -> parse string */  {
  return _lp__bar__bar__rp_(function() {
       
      var x_10297 = satisfy_fail("digit", function(slice /* sslice/sslice */ ) {
          var _x21 = next_while0(slice, $std_core_char.is_digit, $std_core_types.Nil);
          if (_x21.fst === null) {
            return $std_core_types.Nothing;
          }
          else {
            return $std_core_types.Just($std_core_types.Tuple2(_x21.fst, _x21.snd));
          }
        });
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend($std_core_string.listchar_fs_string);
      }
      else {
        return $std_core_string.listchar_fs_string(x_10297);
      }
    }, function() {
      return "0";
    });
}
 
 
// monadic lift
export function _mlift_pnat_10191(_y_x10128) /* (list<char>) -> parse int */  {
   
  var _x_x1_0_10174 = $std_core_string.listchar_fs_string(_y_x10128);
   
  var _x_x1_10172 = $std_core_hnd._open_none2(function(s /* string */ , hex /* ? bool */ ) {
      var _x21 = (hex !== undefined) ? hex : false;
      return $std_core_int.xparse(s, _x21);
    }, _x_x1_0_10174);
  return $std_core_hnd._open_none2(function(m /* maybe<int> */ , nothing /* int */ ) {
      return (m === null) ? nothing : m.value;
    }, _x_x1_10172, 0);
}
 
export function pnat() /* () -> parse int */  {
   
  var x_10299 = satisfy_fail("digit", function(slice /* sslice/sslice */ ) {
      var _x21 = next_while0(slice, $std_core_char.is_digit, $std_core_types.Nil);
      if (_x21.fst === null) {
        return $std_core_types.Nothing;
      }
      else {
        return $std_core_types.Just($std_core_types.Tuple2(_x21.fst, _x21.snd));
      }
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pnat_10191);
  }
  else {
     
    var _x_x1_0_10174 = $std_core_string.listchar_fs_string(x_10299);
     
    var _x_x1_10172 = $std_core_hnd._open_none2(function(s /* string */ , hex /* ? bool */ ) {
        var _x21 = (hex !== undefined) ? hex : false;
        return $std_core_int.xparse(s, _x21);
      }, _x_x1_0_10174);
    return $std_core_hnd._open_none2(function(m /* maybe<int> */ , nothing /* int */ ) {
        return (m === null) ? nothing : m.value;
      }, _x_x1_10172, 0);
  }
}
 
export function none_of(chars) /* (chars : string) -> parse char */  {
  return satisfy_fail("", function(slice /* sslice/sslice */ ) {
      var _x21 = $std_core_sslice.next(slice);
      if (_x21 !== null) {
         
        var b_10040 = ((chars).indexOf(($std_core_string.char_fs_string(_x21.value.fst))) >= 0);
        var _x22 = (b_10040) ? false : true;
        if (_x22){
          return $std_core_types.Just($std_core_types.Tuple2(_x21.value.fst, _x21.value.snd));
        }
      }
      return $std_core_types.Nothing;
    });
}
 
export function none_of_many1(chars) /* (chars : string) -> parse string */  {
   
  var x_10302 = satisfy_fail("", function(slice /* sslice/sslice */ ) {
      var _x23 = next_while0(slice, function(c /* char */ ) {
           
          var b_10043 = ((chars).indexOf(($std_core_string.char_fs_string(c))) >= 0);
          return (b_10043) ? false : true;
        }, $std_core_types.Nil);
      if (_x23.fst === null) {
        return $std_core_types.Nothing;
      }
      else {
        return $std_core_types.Just($std_core_types.Tuple2(_x23.fst, _x23.snd));
      }
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend($std_core_string.listchar_fs_string);
  }
  else {
    return $std_core_string.listchar_fs_string(x_10302);
  }
}
 
export function one_of(chars) /* (chars : string) -> parse char */  {
  return satisfy_fail(chars, function(slice /* sslice/sslice */ ) {
      var _x23 = $std_core_sslice.next(slice);
      if (_x23 !== null) {
        if (((chars).indexOf(($std_core_string.char_fs_string(_x23.value.fst))) >= 0)){
          return $std_core_types.Just($std_core_types.Tuple2(_x23.value.fst, _x23.value.snd));
        }
      }
      return $std_core_types.Nothing;
    });
}
 
 
// monadic lift
export function _mlift_many_acc_10192(acc, p, x) /* forall<a,e> (acc : list<a>, p : parser<e,a>, x : a) -> <parse|e> list<a> */  {
  return many_acc(p, $std_core_types.Cons(x, acc));
}
 
export function many_acc(p_0, acc_0) /* forall<a,e> (p : parser<e,a>, acc : list<a>) -> <parse|e> list<a> */  {
  return _lp__bar__bar__rp_(function() {
       
      var x_0_10304 = p_0();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(x_1 /* 2754 */ ) {
          return _mlift_many_acc_10192(acc_0, p_0, x_1);
        });
      }
      else {
        return _mlift_many_acc_10192(acc_0, p_0, x_0_10304);
      }
    }, function() {
      return $std_core_list.reverse_acc($std_core_types.Nil, acc_0);
    });
}
 
 
// The `many` combinator parses `p` until it fails, returning a list of the results of `p`.
// The `many` combinator is non-divergent only when `p` always consumes input or `fail`s.
export function many(p) /* forall<a,e> (p : parser<e,a>) -> <parse|e> list<a> */  {
  return many_acc(p, $std_core_types.Nil);
}
 
 
// monadic lift
export function _mlift_many1_10193(_y_x10136, _y_x10137) /* forall<a,e> (a, list<a>) -> <parse|e> list<a> */  {
  return $std_core_types.Cons(_y_x10136, _y_x10137);
}
 
 
// monadic lift
export function _mlift_many1_10194(p, _y_x10136) /* forall<a,e> (p : parser<e,a>, a) -> <parse|e> list<a> */  {
   
  var x_10306 = many_acc(p, $std_core_types.Nil);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10137 /* list<2822> */ ) {
      return $std_core_types.Cons(_y_x10136, _y_x10137);
    });
  }
  else {
    return $std_core_types.Cons(_y_x10136, x_10306);
  }
}
 
 
// The `many1` combinator parses `p` at least once and then until it fails, returning a list of the results of `p`.
// The `many1` combinator is non-divergent only when `p` always consumes input or `fail`s.
export function many1(p) /* forall<a,e> (p : parser<e,a>) -> <parse|e> list<a> */  {
   
  var x_10310 = p();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10136 /* 2822 */ ) {
      return _mlift_many1_10194(p, _y_x10136);
    });
  }
  else {
     
    var x_0_10313 = many_acc(p, $std_core_types.Nil);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10137 /* list<2822> */ ) {
        return $std_core_types.Cons(x_10310, _y_x10137);
      });
    }
    else {
      return $std_core_types.Cons(x_10310, x_0_10313);
    }
  }
}
 
 
// monadic lift
export function _mlift_count_acc_10195(acc, n, p, x) /* forall<a,e> (acc : list<a>, n : int, p : parser<e,a>, x : a) -> <parse|e> list<a> */  {
  return count_acc($std_core_types._int_sub(n,1), $std_core_types.Cons(x, acc), p);
}
 
export function count_acc(n_0, acc_0, p_0) /* forall<a,e> (n : int, acc : list<a>, p : parser<e,a>) -> <parse|e> list<a> */  { tailcall: while(1)
{
  if ($std_core_types._int_le(n_0,0)) {
    return $std_core_list.reverse_acc($std_core_types.Nil, acc_0);
  }
  else {
     
    var x_0_10318 = p_0();
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(x_1 /* 2923 */ ) {
        return _mlift_count_acc_10195(acc_0, n_0, p_0, x_1);
      });
    }
    else {
      {
        // tail call
        var _x24 = $std_core_types._int_sub(n_0,1);
        var _x25 = $std_core_types.Cons(x_0_10318, acc_0);
        n_0 = _x24;
        acc_0 = _x25;
        continue tailcall;
      }
    }
  }
}}
 
export function count(n, p) /* forall<a,e> (n : int, p : parser<e,a>) -> <parse|e> list<a> */  {
  return count_acc(n, $std_core_types.Nil, p);
}
 
export function one_of_or(chars, $default) /* (chars : string, default : char) -> parse char */  {
  return _lp__bar__bar__rp_(function() {
      return one_of(chars);
    }, function() {
      return $default;
    });
}
 
 
// monadic lift
export function _mlift_sign_10196(c_0) /* (c@0 : char) -> parse bool */  {
  return (c_0 === 0x002D);
}
 
export function sign() /* () -> parse bool */  {
   
  var x_10321 = _lp__bar__bar__rp_(function() {
      return satisfy_fail("+-", function(slice /* sslice/sslice */ ) {
          var _x26 = $std_core_sslice.next(slice);
          if (_x26 !== null) {
            if (((("+-")).indexOf(($std_core_string.char_fs_string(_x26.value.fst))) >= 0)){
              return $std_core_types.Just($std_core_types.Tuple2(_x26.value.fst, _x26.value.snd));
            }
          }
          return $std_core_types.Nothing;
        });
    }, function() {
      return 0x002B;
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_sign_10196);
  }
  else {
    return (x_10321 === 0x002D);
  }
}
 
 
// monadic lift
export function _mlift_pint_10197(neg, i) /* (neg : bool, i : int) -> parse int */  {
  return (neg) ? $std_core_types._int_negate(i) : i;
}
 
 
// monadic lift
export function _mlift_pint_10198(c_0) /* (c@0 : char) -> parse int */  {
   
  var neg = (c_0 === 0x002D);
   
  var x_10324 = pnat();
   
  function next_10325(i) /* (int) -> parse int */  {
    return (neg) ? $std_core_types._int_negate(i) : i;
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10325);
  }
  else {
    return next_10325(x_10324);
  }
}
 
export function pint() /* () -> parse int */  {
   
  var x_10328 = _lp__bar__bar__rp_(function() {
      return satisfy_fail("+-", function(slice /* sslice/sslice */ ) {
          var _x26 = $std_core_sslice.next(slice);
          if (_x26 !== null) {
            if (((("+-")).indexOf(($std_core_string.char_fs_string(_x26.value.fst))) >= 0)){
              return $std_core_types.Just($std_core_types.Tuple2(_x26.value.fst, _x26.value.snd));
            }
          }
          return $std_core_types.Nothing;
        });
    }, function() {
      return 0x002B;
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pint_10198);
  }
  else {
     
    var neg = (x_10328 === 0x002D);
     
    var x_0_10331 = pnat();
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(i /* int */ ) {
        return (neg) ? $std_core_types._int_negate(i) : i;
      });
    }
    else {
      return (neg) ? $std_core_types._int_negate(x_0_10331) : x_0_10331;
    }
  }
}
 
 
// monadic lift
export function _mlift_sep_by1_10199(p, wild__) /* forall<a,b,e> (p : parser<e,a>, wild_ : b) -> <parse|e> a */  {
  return p();
}
 
 
// monadic lift
export function _mlift_sep_by1_10200(_y_x10150, _y_x10153) /* forall<a,e> (a, list<a>) -> <parse|e> list<a> */  {
  return $std_core_types.Cons(_y_x10150, _y_x10153);
}
 
 
// monadic lift
export function _mlift_sep_by1_10201(p, sep, _y_x10150) /* forall<a,b,e> (p : parser<e,a>, sep : parser<e,b>, a) -> <parse|e> list<a> */  {
   
  var x_10336 = many_acc(function() {
       
      var x_0_10338 = sep();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(wild__ /* 3184 */ ) {
          return p();
        });
      }
      else {
        return p();
      }
    }, $std_core_types.Nil);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10153 /* list<3183> */ ) {
      return $std_core_types.Cons(_y_x10150, _y_x10153);
    });
  }
  else {
    return $std_core_types.Cons(_y_x10150, x_10336);
  }
}
 
 
// The `sep-by1` parses one or more occurrences of `p`, separated by `sep`. Returns a list of the results of `p`.
// The `sep-by1` combinator is non-divergent only when `p` always consumes input or `fail`s.
export function sep_by1(p, sep) /* forall<a,b,e> (p : parser<e,a>, sep : parser<e,b>) -> <parse|e> list<a> */  {
   
  var x_10344 = p();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10150 /* 3183 */ ) {
      return _mlift_sep_by1_10201(p, sep, _y_x10150);
    });
  }
  else {
     
    var x_0_10347 = many_acc(function() {
         
        var x_1_10350 = sep();
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(wild__ /* 3184 */ ) {
            return p();
          });
        }
        else {
          return p();
        }
      }, $std_core_types.Nil);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10153 /* list<3183> */ ) {
        return $std_core_types.Cons(x_10344, _y_x10153);
      });
    }
    else {
      return $std_core_types.Cons(x_10344, x_0_10347);
    }
  }
}
 
 
// The `sep-by` parses zero or more occurrences of `p`, separated by `sep`. Returns a list of the results of `p`.
// The `sep-by` combinator is non-divergent only when `p` always consumes input or `fail`s.
export function sep_by(p, sep) /* forall<a,b,e> (p : parser<e,a>, sep : parser<e,b>) -> <parse|e> list<a> */  {
  return _lp__bar__bar__rp_(function() {
      return sep_by1(p, sep);
    }, function() {
      return $std_core_types.Nil;
    });
}
 
export function hex_digits() /* () -> parse string */  {
   
  var x_10356 = satisfy_fail("digit", function(slice /* sslice/sslice */ ) {
      var _x26 = next_while0(slice, $std_core_char.is_hex_digit, $std_core_types.Nil);
      if (_x26.fst === null) {
        return $std_core_types.Nothing;
      }
      else {
        return $std_core_types.Just($std_core_types.Tuple2(_x26.fst, _x26.snd));
      }
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend($std_core_string.listchar_fs_string);
  }
  else {
    return $std_core_string.listchar_fs_string(x_10356);
  }
}
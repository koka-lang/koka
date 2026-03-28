// Koka generated module: std/time/format, koka version: 3.2.4
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
import * as $std_text_parse from './std_text_parse.mjs';
import * as $std_num_ddouble from './std_num_ddouble.mjs';
import * as $std_time_timestamp from './std_time_timestamp.mjs';
import * as $std_time_duration from './std_time_duration.mjs';
import * as $std_time_instant from './std_time_instant.mjs';
import * as $std_time_date from './std_time_date.mjs';
import * as $std_time_calendar from './std_time_calendar.mjs';
import * as $std_time_time from './std_time_time.mjs';
import * as $std_time_locale from './std_time_locale.mjs';
import * as $std_num_float64 from './std_num_float64.mjs';
 
// externals
 
// type declarations
 
// declarations
 
export var fmt_iso_date;
var fmt_iso_date = "YYYY-MM-DD";
 
export var fmt_iso_time;
var fmt_iso_time = "HH:mm:ssFFFFFFFFF";
 
export var fmt_iso_timezone;
var fmt_iso_timezone = "Z C";
 
export function format_weekday(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
   
  var wd = $std_time_time.weekday(t);
  if ($std_core_types._int_eq(n,1)) {
    return $std_time_date.weekday_fs_show(wd);
  }
  else {
     
    if ($std_core_types._int_eq(n,2)) {
      var days = locale.day_names_min;
    }
    else {
      if ($std_core_types._int_eq(n,3)) {
        var days = locale.day_names_short;
      }
      else {
        var days = locale.day_names;
      }
    }
     
    var x_0_10005 = $std_time_date.int(wd);
     
    var m_10003 = $std_core_list._index(days, $std_core_types._int_sub(x_0_10005,1));
     
    var nothing_10004 = $std_core_types._lp__plus__plus__rp_("D", $std_time_date.weekday_fs_show(wd));
    return (m_10003 === null) ? nothing_10004 : m_10003.value;
  }
}
 
export function format_absyear(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  var _x0 = t.date.year;
  return $std_core_int.show($std_core_types._int_abs(_x0));
}
 
export function format_calname(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  var _x2 = t.calendar.month_prefix;
  var _x1 = (_x2 !== (""));
  if (_x1) {
    return "";
  }
  else {
    if ($std_core_types._int_eq(n,2)) {
      return t.calendar.long_name;
    }
    else {
      return t.calendar.name;
    }
  }
}
 
export function format_ampm(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  var _x5 = t.clock.hours;
  var _x4 = $std_core_types._int_le(_x5,11);
  var _x3 = (_x4) ? "am" : "pm";
  return $std_core_sslice.string($std_core_sslice.first(_x3, n));
}
 
 
// monadic lift
export function _mlift_pquoted_10171(quote, s, wild___0) /* (quote : char, s : string, wild_@0 : char) -> std/text/parse/parse string */  {
  return $std_core_types._lp__plus__plus__rp_($std_core_string.char_fs_string(quote), $std_core_types._lp__plus__plus__rp_(s, $std_core_string.char_fs_string(quote)));
}
 
 
// monadic lift
export function _mlift_pquoted_10172(quote, s) /* (quote : char, s : string) -> std/text/parse/parse string */  {
   
  var x_10181 = $std_text_parse.char(quote);
   
  function next_10182(wild___0) /* (char) -> std/text/parse/parse string */  {
    return $std_core_types._lp__plus__plus__rp_($std_core_string.char_fs_string(quote), $std_core_types._lp__plus__plus__rp_(s, $std_core_string.char_fs_string(quote)));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10182);
  }
  else {
    return next_10182(x_10181);
  }
}
 
 
// monadic lift
export function _mlift_pquoted_10173(quote, wild__) /* (quote : char, wild_ : char) -> std/text/parse/parse string */  {
   
  var x_10186 = $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_0_10188 = $std_text_parse.chars_are("", function(c /* char */ ) {
          return (c !== quote);
        });
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend($std_core_string.listchar_fs_string);
      }
      else {
        return $std_core_string.listchar_fs_string(x_0_10188);
      }
    }, function() {
      return "";
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(s /* string */ ) {
      return _mlift_pquoted_10172(quote, s);
    });
  }
  else {
    return _mlift_pquoted_10172(quote, x_10186);
  }
}
 
export function pquoted(quote) /* (quote : char) -> std/text/parse/parse string */  {
   
  var x_10190 = $std_text_parse.char(quote);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* char */ ) {
      return _mlift_pquoted_10173(quote, wild__);
    });
  }
  else {
     
    var x_0_10193 = $std_text_parse._lp__bar__bar__rp_(function() {
         
        var x_1_10196 = $std_text_parse.chars_are("", function(c /* char */ ) {
            return (c !== quote);
          });
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend($std_core_string.listchar_fs_string);
        }
        else {
          return $std_core_string.listchar_fs_string(x_1_10196);
        }
      }, function() {
        return "";
      });
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(s /* string */ ) {
        return _mlift_pquoted_10172(quote, s);
      });
    }
    else {
       
      var x_2_10198 = $std_text_parse.char(quote);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(wild___0 /* char */ ) {
          return $std_core_types._lp__plus__plus__rp_($std_core_string.char_fs_string(quote), $std_core_types._lp__plus__plus__rp_(x_0_10193, $std_core_string.char_fs_string(quote)));
        });
      }
      else {
        return $std_core_types._lp__plus__plus__rp_($std_core_string.char_fs_string(quote), $std_core_types._lp__plus__plus__rp_(x_0_10193, $std_core_string.char_fs_string(quote)));
      }
    }
  }
}
 
 
// monadic lift
export function _mlift_plocale_t_10174(locale, wild___0) /* (locale : std/time/locale/time-locale, wild_@0 : char) -> std/text/parse/parse string */  {
  return $std_core_hnd._open_none1(function(_this_0 /* std/time/locale/time-locale */ ) {
      return _this_0.format_tt;
    }, locale);
}
 
 
// monadic lift
export function _mlift_plocale_t_10175(locale, wild__) /* (locale : std/time/locale/time-locale, wild_ : char) -> std/text/parse/parse string */  {
   
  var default_10018 = $std_core_hnd._open_none1(function(_this /* std/time/locale/time-locale */ ) {
      return _this.format_t;
    }, locale);
  return $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_10204 = $std_text_parse.char(0x0074);
       
      function next_10205(wild___0) /* (char) -> std/text/parse/parse string */  {
        return $std_core_hnd._open_none1(function(_this_0 /* std/time/locale/time-locale */ ) {
            return _this_0.format_tt;
          }, locale);
      }
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(next_10205);
      }
      else {
        return next_10205(x_10204);
      }
    }, function() {
      return default_10018;
    });
}
 
export function plocale_t(locale) /* (locale : std/time/locale/time-locale) -> std/text/parse/parse string */  {
   
  var x_10208 = $std_text_parse.char(0x0074);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* char */ ) {
      return _mlift_plocale_t_10175(locale, wild__);
    });
  }
  else {
     
    var default_10018 = $std_core_hnd._open_none1(function(_this /* std/time/locale/time-locale */ ) {
        return _this.format_t;
      }, locale);
    return $std_text_parse._lp__bar__bar__rp_(function() {
         
        var x_0_10211 = $std_text_parse.char(0x0074);
         
        var next_0_10212 = function(wild___0 /* char */ ) {
          return $std_core_hnd._open_none1(function(_this_0 /* std/time/locale/time-locale */ ) {
              return _this_0.format_tt;
            }, locale);
        };
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(next_0_10212);
        }
        else {
          return next_0_10212(x_0_10211);
        }
      }, function() {
        return default_10018;
      });
  }
}
 
 
// monadic lift
export function _mlift_plocale_l_10176(l, _c_x10144) /* (l : char, string) -> string */  {
  if ((l === 0x004C)) {
    return _c_x10144;
  }
  else {
    return (((((((((((_c_x10144).replace(new RegExp((("dd")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("d")))).replace(new RegExp((("dd")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("ddd")))).replace(new RegExp((("MM")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("M")))).replace(new RegExp((("MM")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("MMM")))).replace(new RegExp((("DD")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("D")))).replace(new RegExp((("DD")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("DDD"));
  }
}
 
 
// monadic lift
export function _mlift_plocale_l_10177(_y_x10139, l, locale, _y_x10141) /* (char, l : char, locale : std/time/locale/time-locale, list<char>) -> std/text/parse/parse string */  {
   
  var n = $std_core_list._lift_length_6003($std_core_types.Cons(_y_x10139, _y_x10141), 0);
   
  if ($std_core_types._int_ge(n,4)) {
    var x_10215 = $std_core_hnd._open_none1(function(_this /* std/time/locale/time-locale */ ) {
        return _this.format_llll;
      }, locale);
  }
  else {
    if ($std_core_types._int_eq(n,3)) {
      var x_10215 = $std_core_hnd._open_none1(function(_this_0 /* std/time/locale/time-locale */ ) {
          return _this_0.format_lll;
        }, locale);
    }
    else {
      if ($std_core_types._int_eq(n,2)) {
        var x_10215 = $std_core_hnd._open_none1(function(_this_1 /* std/time/locale/time-locale */ ) {
            return _this_1.format_ll;
          }, locale);
      }
      else {
        var x_10215 = $std_core_hnd._open_none1(function(_this_2 /* std/time/locale/time-locale */ ) {
            return _this_2.format_l;
          }, locale);
      }
    }
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_c_x10144 /* string */ ) {
      return _mlift_plocale_l_10176(l, _c_x10144);
    });
  }
  else {
    return _mlift_plocale_l_10176(l, x_10215);
  }
}
 
 
// monadic lift
export function _mlift_plocale_l_10178(l, locale, _y_x10139) /* (l : char, locale : std/time/locale/time-locale, char) -> std/text/parse/parse string */  {
   
  var x_10217 = $std_text_parse.many_acc(function() {
      return $std_text_parse.char(l);
    }, $std_core_types.Nil);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10141 /* list<char> */ ) {
      return _mlift_plocale_l_10177(_y_x10139, l, locale, _y_x10141);
    });
  }
  else {
    return _mlift_plocale_l_10177(_y_x10139, l, locale, x_10217);
  }
}
 
export function plocale_l(locale, l) /* (locale : std/time/locale/time-locale, l : char) -> std/text/parse/parse string */  {
   
  var x_10219 = $std_text_parse.char(l);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10139 /* char */ ) {
      return _mlift_plocale_l_10178(l, locale, _y_x10139);
    });
  }
  else {
     
    var x_0_10222 = $std_text_parse.many_acc(function() {
        return $std_text_parse.char(l);
      }, $std_core_types.Nil);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10141 /* list<char> */ ) {
        return _mlift_plocale_l_10177(x_10219, l, locale, _y_x10141);
      });
    }
    else {
       
      var n = $std_core_list._lift_length_6003($std_core_types.Cons(x_10219, x_0_10222), 0);
       
      if ($std_core_types._int_ge(n,4)) {
        var x_1_10225 = $std_core_hnd._open_none1(function(_this /* std/time/locale/time-locale */ ) {
            return _this.format_llll;
          }, locale);
      }
      else {
        if ($std_core_types._int_eq(n,3)) {
          var x_1_10225 = $std_core_hnd._open_none1(function(_this_0 /* std/time/locale/time-locale */ ) {
              return _this_0.format_lll;
            }, locale);
        }
        else {
          if ($std_core_types._int_eq(n,2)) {
            var x_1_10225 = $std_core_hnd._open_none1(function(_this_1 /* std/time/locale/time-locale */ ) {
                return _this_1.format_ll;
              }, locale);
          }
          else {
            var x_1_10225 = $std_core_hnd._open_none1(function(_this_2 /* std/time/locale/time-locale */ ) {
                return _this_2.format_l;
              }, locale);
          }
        }
      }
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_c_x10144 /* string */ ) {
          return _mlift_plocale_l_10176(l, _c_x10144);
        });
      }
      else {
        if ((l === 0x004C)) {
          return x_1_10225;
        }
        else {
          return (((((((((((x_1_10225).replace(new RegExp((("dd")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("d")))).replace(new RegExp((("dd")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("ddd")))).replace(new RegExp((("MM")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("M")))).replace(new RegExp((("MM")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("MMM")))).replace(new RegExp((("DD")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("D")))).replace(new RegExp((("DD")).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),("DDD"));
        }
      }
    }
  }
}
 
export function plocale(locale) /* (locale : std/time/locale/time-locale) -> std/text/parse/parse string */  {
  return $std_text_parse.choose($std_core_types.Cons(function() {
      return $std_text_parse.none_of_many1("\'\"tLl");
    }, $std_core_types.Cons(function() {
        return pquoted(0x0027);
      }, $std_core_types.Cons(function() {
          return pquoted(0x0022);
        }, $std_core_types.Cons(function() {
            return plocale_t(locale);
          }, $std_core_types.Cons(function() {
              return plocale_l(locale, 0x004C);
            }, $std_core_types.Cons(function() {
                return plocale_l(locale, 0x006C);
              }, $std_core_types.Nil)))))));
}
 
export function plocales(locale) /* (locale : std/time/locale/time-locale) -> std/text/parse/parse string */  {
   
  var x_10228 = $std_text_parse.many_acc(function() {
      return plocale(locale);
    }, $std_core_types.Nil);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend($std_core_list.concat_fs_join);
  }
  else {
    return $std_core_list.concat_fs_join(x_10228);
  }
}
 
 
// monadic lift
export function _mlift_expand_locales_10179(x_1, wild__) /* (x@1 : string, wild_ : ()) -> std/text/parse/parse string */  {
  return x_1;
}
 
 
// monadic lift
export function _mlift_expand_locales_10180(_y_x10156) /* (list<string>) -> std/text/parse/parse string */  {
   
  var x_1 = $std_core_list.concat_fs_join(_y_x10156);
   
  var x_10230 = $std_text_parse.eof();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return x_1;
    });
  }
  else {
    return x_1;
  }
}
 
export function expand_locales(fmt, locale) /* (fmt : string, locale : std/time/locale/time-locale) -> string */  {
   
  var input_10025 = $std_core_sslice.Sslice(fmt, 0, fmt.length);
   
  var perr_10024 = $std_text_parse.parse(input_10025, function() {
       
      var x_10234 = $std_text_parse.many_acc(function() {
          return plocale(locale);
        }, $std_core_types.Nil);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_expand_locales_10180);
      }
      else {
        return _mlift_expand_locales_10180(x_10234);
      }
    });
  if (perr_10024._tag === 1) {
    return perr_10024.result;
  }
  else {
    var _x6 = $std_core_types.Nothing;
    return (_x6 === null) ? fmt : _x6.value;
  }
}
 
export function showl(i, width) /* (i : int, width : ? int) -> string */  {
  var _x7 = (width !== undefined) ? width : 2;
  return $std_core_string.pad_left($std_core_int.show(i), _x7, 0x0030);
}
 
export function format_day(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  if ($std_core_types._int_eq(n,3)) {
     
    var i_10028 = $std_time_time.day_of_year(t);
    return $std_core_string.pad_left($std_core_int.show(i_10028), 3, 0x0030);
  }
  else {
    var _x9 = t.calendar.month_prefix;
    var _x8 = (_x9 === ("W"));
    if (_x8) {
      var _x10 = t.date.day;
      return $std_core_int.show(_x10);
    }
    else {
      var _x11 = t.date.day;
      return $std_core_string.pad_left($std_core_int.show(_x11), n, 0x0030);
    }
  }
}
 
export function format_month(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  if ($std_core_types._int_le(n,2)) {
    var _x12 = t.calendar.month_prefix;
    var _x13 = t.date.month;
    return $std_core_types._lp__plus__plus__rp_(_x12, $std_core_string.pad_left($std_core_int.show(_x13), n, 0x0030));
  }
  else {
     
    if ($std_core_types._int_eq(n,3)) {
      var months = locale.month_names_short;
    }
    else {
      var months = locale.month_names;
    }
     
    var _x14 = t.date.month;
    var m_10043 = $std_core_list._index(months, $std_core_types._int_sub(_x14,1));
     
    var _x15 = t.date.month;
    var nothing_10044 = $std_core_types._lp__plus__plus__rp_("M", $std_core_string.pad_left($std_core_int.show(_x15), 2, 0x0030));
    return (m_10043 === null) ? nothing_10044 : m_10043.value;
  }
}
 
export function format_year(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  if ($std_core_types._int_eq(n,1)) {
    var _x14 = t.date.year;
    return $std_core_int.show(_x14);
  }
  else {
    if ($std_core_types._int_lt(n,5)) {
       
      var _x15 = t.date.year;
      var i_10052 = $std_core_types._int_abs(_x15);
      return $std_core_sslice.string($std_core_sslice.last($std_core_string.pad_left($std_core_int.show(i_10052), n, 0x0030), n));
    }
    else {
      var _x16 = t.date.year;
      var _x15 = $std_core_types._int_ge(_x16,0);
      if (_x15) {
        var _x18 = t.date.year;
        var _x17 = $std_core_types._int_le(_x18,9999);
        if (_x17) {
           
          var _x19 = t.date.year;
          var i_0_10057 = $std_core_types._int_abs(_x19);
          return $std_core_sslice.string($std_core_sslice.last($std_core_string.pad_left($std_core_int.show(i_0_10057), n, 0x0030), n));
        }
        else {
           
          var _x19 = t.date.year;
          var i_1_10060 = $std_core_types._int_abs(_x19);
           
          var y = $std_core_string.pad_left($std_core_int.show(i_1_10060), n, 0x0030);
          var _x20 = t.date.year;
          var _x19 = $std_core_types._int_lt(_x20,0);
          if (_x19) {
            return $std_core_types._lp__plus__plus__rp_("-", y);
          }
          else {
            return $std_core_types._lp__plus__plus__rp_("+", y);
          }
        }
      }
      else {
         
        var _x21 = t.date.year;
        var i_3_10065 = $std_core_types._int_abs(_x21);
         
        var y_0 = $std_core_string.pad_left($std_core_int.show(i_3_10065), n, 0x0030);
        var _x22 = t.date.year;
        var _x21 = $std_core_types._int_lt(_x22,0);
        if (_x21) {
          return $std_core_types._lp__plus__plus__rp_("-", y_0);
        }
        else {
          return $std_core_types._lp__plus__plus__rp_("+", y_0);
        }
      }
    }
  }
}
 
export function format_era(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  var _x23 = t.date;
  return t.calendar.show_era(_x23);
}
 
export function format_seconds(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
   
  var _x26 = t.clock.seconds.hi;
  var _x25 = (_x26 < (0.0));
  if (_x25) {
    var _x27 = t.clock.seconds;
    var _x24 = $std_num_ddouble.ceiling(_x27);
  }
  else {
    var _x28 = t.clock.seconds;
    var _x24 = $std_num_ddouble.floor(_x28);
  }
  var i_10073 = $std_num_ddouble.int(_x24);
  return $std_core_string.pad_left($std_core_int.show(i_10073), n, 0x0030);
}
 
export function format_minutes(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  var _x24 = t.clock.minutes;
  return $std_core_string.pad_left($std_core_int.show(_x24), n, 0x0030);
}
 
export function format_hours(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  var _x25 = t.clock.hours;
  return $std_core_string.pad_left($std_core_int.show(_x25), n, 0x0030);
}
 
export function format_hours12(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  var _x27 = t.clock.hours;
  var _x26 = $std_core_types._int_eq(_x27,0);
  if (_x26) {
    return "12";
  }
  else {
    var _x29 = t.clock.hours;
    var _x28 = $std_core_types._int_eq(_x29,12);
    if (_x28) {
      return "12";
    }
    else {
      var _x31 = t.clock.hours;
      var _x30 = $std_core_types._int_le(_x31,11);
      if (_x30) {
        var _x32 = t.clock.hours;
        return $std_core_string.pad_left($std_core_int.show(_x32), n, 0x0030);
      }
      else {
         
        var _x33 = t.clock.hours;
        var i_0_10086 = $std_core_types._int_sub(_x33,12);
        return $std_core_string.pad_left($std_core_int.show(i_0_10086), n, 0x0030);
      }
    }
  }
}
 
export function format_upper_ampm(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  return $std_core_string.to_upper(format_ampm(t, n, locale));
}
 
export function format_frac(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
   
  var _x33 = t.clock.seconds;
  var frac = $std_num_ddouble.fraction(_x33);
  var _x34 = frac.hi;
  var _x33 = (_x34 === (0.0));
  if (_x33) {
    return "";
  }
  else {
    return $std_core_sslice.tail($std_num_ddouble.show_fixed(frac, n));
  }
}
 
export function format_frac0(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
   
  var _x35 = t.clock.seconds;
  var frac = $std_num_ddouble.fraction(_x35);
  var _x36 = frac.hi;
  var _x35 = (_x36 === (0.0));
  if (_x35) {
    return "";
  }
  else {
    return $std_core_string.pad_right($std_core_sslice.tail($std_num_ddouble.show_fixed(frac, n)), $std_core_types._int_add(n,1), 0x0030);
  }
}
 
export function format_tz_offset(t, n, hmsep, utc) /* (t : std/time/time/time, n : int, hmsep : string, utc : string) -> string */  {
  var _x37 = t.tzdelta;
  return $std_time_time.show_tzdelta(_x37, utc, hmsep);
}
 
export function format_timezone(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
   
  var hmsep_10099 = ($std_core_types._int_eq(n,2)) ? "" : ":";
   
  var utc_10100 = ($std_core_types._int_eq(n,2)) ? "+0000" : "+00:00";
  var _x38 = t.tzdelta;
  return $std_time_time.show_tzdelta(_x38, utc_10100, hmsep_10099);
}
 
export function format_utc_timezone(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
   
  var utc_10105 = ($std_core_types._int_eq(n,2)) ? "" : "Z";
  var _x39 = t.tzdelta;
  return $std_time_time.show_tzdelta(_x39, utc_10105, ":");
}
 
export function format_timestamp(t, n, locale) /* (t : std/time/time/time, n : int, locale : std/time/locale/time-locale) -> string */  {
  var _x40 = t.instant;
  return $std_time_instant.show_raw(_x40, 3);
}
 
export var formats;
var formats = $std_core_vector.vlist([$std_core_types.Tuple3(0x0059, 6, format_year), $std_core_types.Tuple3(0x004D, 4, format_month), $std_core_types.Tuple3(0x0044, 3, format_day), $std_core_types.Tuple3(0x0048, 2, format_hours), $std_core_types.Tuple3(0x006D, 2, format_minutes), $std_core_types.Tuple3(0x0073, 2, format_seconds), $std_core_types.Tuple3(0x007A, 2, format_timezone), $std_core_types.Tuple3(0x005A, 2, format_utc_timezone), $std_core_types.Tuple3(0x0066, 8, format_frac0), $std_core_types.Tuple3(0x0046, 8, format_frac), $std_core_types.Tuple3(0x0043, 2, format_calname), $std_core_types.Tuple3(0x0045, 1, format_era), $std_core_types.Tuple3(0x0079, 1, format_absyear), $std_core_types.Tuple3(0x0064, 4, format_weekday), $std_core_types.Tuple3(0x0068, 2, format_hours12), $std_core_types.Tuple3(0x0061, 2, format_ampm), $std_core_types.Tuple3(0x0041, 2, format_upper_ampm), $std_core_types.Tuple3(0x0078, 1, format_timestamp)], $std_core_types.Nil);
 
export function format_pat(t, h, fmt, locale) /* (t : std/time/time/time, h : char, fmt : list<char>, locale : std/time/locale/time-locale) -> (string, list<char>) */  {
  if ((h === 0x0022)) {
    var _x41 = $std_core_list.span(fmt, function(c /* char */ ) {
        return (c !== h);
      });
    if (_x41.snd === null) {
      return $std_core_types.Tuple2($std_core_string.char_fs_string(h), fmt);
    }
    else {
      return $std_core_types.Tuple2($std_core_string.listchar_fs_string(_x41.fst), _x41.snd.tail);
    }
  }
  else {
    if ((h === 0x0027)) {
      var _x42 = $std_core_list.span(fmt, function(c_0 /* char */ ) {
          return (c_0 !== h);
        });
      if (_x42.snd === null) {
        return $std_core_types.Tuple2($std_core_string.char_fs_string(h), fmt);
      }
      else {
        return $std_core_types.Tuple2($std_core_string.listchar_fs_string(_x42.fst), _x42.snd.tail);
      }
    }
    else {
      var _x43 = $std_core_char.is_alpha(h);
      if (_x43) {
        var _x44 = $std_core_list.span(fmt, function(c_0_0 /* char */ ) {
            return (c_0_0 === h);
          });
         
        var x_10108 = $std_core_list._lift_length_6003(_x44.fst, 0);
         
        var n = $std_core_types._int_add(x_10108,1);
         
        var m_10111 = $std_core_list.foreach_while(formats, function(pattern /* (char, int, (std/time/time/time, int, std/time/locale/time-locale) -> string) */ ) {
            if (((pattern.fst) !== h)) {
              return $std_core_types.Nothing;
            }
            else {
              var _x45 = ($std_core_types._int_le(n,(pattern.snd))) ? n : pattern.snd;
              return $std_core_types.Just($std_core_types.Tuple2(pattern.thd(t, _x45, locale), _x44.snd));
            }
          });
        if (m_10111 === null) {
          return $std_core_types.Tuple2("", _x44.snd);
        }
        else {
          return m_10111.value;
        }
      }
      else {
        return $std_core_types.Tuple2($std_core_string.char_fs_string(h), fmt);
      }
    }
  }
}
 
export function format_list(t, fmt, locale) /* (t : std/time/time/time, fmt : list<char>, locale : std/time/locale/time-locale) -> string */  {
  if (fmt === null) {
    return "";
  }
  else {
    var _x45 = format_pat(t, fmt.head, fmt.tail, locale);
    return $std_core_types._lp__plus__plus__rp_(_x45.fst, format_list(t, _x45.snd, locale));
  }
}
 
 
/* Format a time according to format string `fmt` and using a optional
   time locale (= `time-locale-en-iso`).
Letters (``a`` to ``z``) are always interpreted as a pattern where unknown letter
patterns are ignored. Any literal text should be quote-escaped i.e. use `"'GMT'ZZ"`
to display as `"GMT-07:00"` (in the PST time zone). Any characters other then
an ascii letter are displayed as is.
Patterns of 2 letters are zero-padded on the left to always
display as 2 digits. Allowed patterns:
* ``Y``: the year as a number (without zero padding) (``1970``, ``203``)
* ``YY``, ```YYYY``: the year in upto 4 digits (``70``, ``1970``, ``0203``).
  If the year is smaller than zero or larger than 9999, the year is displayed
  with 5 or more digits and prepended with an explicit sign (``-00030``, ``+10345``).
* ``M``, ``MM``: the month. (``1``, ``03``). In case of an ISO week (`cal-iso-week`)
  or ISO month (`cal-iso-month`) calendar, the month is prefixed with ``W`` or ``M``.
* ``MMM``, ``MMMM``: name of the month in the specified `locale`. (``Jan``, ``January``)
* ``D``, ``DD``: the day of the month. (``1``, ``08``). If ``DD`` is used and this
  is an ISO week calendar (`cal-iso-week`) just one digit is used for the week day.
* ``DDD``: the day of the year. (``087``)
* ``d``: the ISO day of the week, 1 for Monday ending in 7 for Sunday.
* ``dd``, ``ddd``,``dddd``: the day of the week in the current `locale`. (``We``,``Wed``,``Wednesday``)
* ``h``, ``hh``: the hours using a 12-hour clock (with am/pm). (``9``, ``09``)
* ``H``, ``HH``: the hours using a 24-hour clock. (``21``, ``09``)
* ``m``, ``mm``: the minutes. (``9``, ``09``)
* ``s``, ``ss``: the whole seconds. (``8``, ``08``)
* ``a``, ``aa``: AM/PM designation. (``a``, ``am``)
* ``A``, ``AA``: AM/PM designation in upper-case. (``A``, ``AM``)
* ``f``,...,``fffffffff``: upto 9 digits of a fraction of a second. Starts with a dot. (``.320``,  ``.000``)
* ``F``,...,``FFFFFFFFF``: upto 9 digits of a fraction of a second. If not zero, starts with a dot.
  In contrast to the ``f`` patterns displays the minimal number of required digits
  (and is not right-padded with zeros). (``.32``)
* ``z``: timezone offset in hours and minutes separated by a colon. Use ``+00:00`` for UTC time. (``+01:00``)
* ``zz``: timezone offset in hours and minutes without a separator. Use ``+0000`` for UTC time. (``-0700``)
* ``Z``: timezone offset in hours and minutes separated by a colon, use ``Z`` for UTC time.
* ``ZZ``: timezone offset in hours and minutes separated by colon, use an empty string for UTC time.
* ``x``: fractional seconds since `min-time`. (``63610768799.429``)
* ``YYYYYY``: the year in [ECMAscript](https://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15.1) 6 digits, prepended with the sign. (``+002016``,``-000023``,``+000000``)
* ``y``: the absolute value of the year as a number (without zero padding). Useful when
  displaying Julian (`cal-julian`) negative years as ``10 BC`` for example (e.g. ``"y E"``).
* ``C``, ``CC``: the short or long calendar name. (The short name is empty for the standard ISO calendars).
* ``E``: the era name, for example ``CE`` for the Gregorian calendar.
* ``'...'``, ``"..."``: anything between quotes is displayed as is. (``'M'M`` becomes ``M11`` for November)
There are also various forms to display dates and times in a locale specific way.
We give examples in English and Dutch. The lower-case ``l`` variants use short
names for month- and day names.
* ``t``: hours and minutes (``3:21pm, 15:21``)
* ``tt``: hours, minutes, and seconds (``3:21:01pm, 15:21:01``)
* ``L``, ``l``: a date (``09/29/2016, 29.09.2016``) and (``9/29/2016, 29.9.2016``)
* ``LL``, ``ll``: date with month name (``29 September 2016, 29 september 2016``) and (``29 Sep 2016, 29 sep 2016``)
* ``LLL``, ``lll``: date with month name and time (``29 September 2016 1:15pm, 29 september 2016 13:15``)
* ``LLLL``, ``llll``: date with day name, month name, and time (``Thursday, 29 September 2016 1:15pm``) and (``Thu, 29 Sep 2016 1:15pm``)
After formatting, any left- or right white space is trimmed. This allows specifiers
like `"YYYY E C"` that display correctly even if the era or calendar name is empty.
For example, to display a time in the standard [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3)
you can use `now().format("ddd, D MMM Y HH:mm:ss zz")` displayed as `"Tue, 27 Sep 2016 06:36:55 -0700"` for example.\
A standard ISO string can be formatted as, `"YYYY-MM-DD'T'HH:mm:ssFFFFFFFFFZ C"`.
*/
export function format(t, fmt, locale) /* (t : std/time/time/time, fmt : string, locale : ? std/time/locale/time-locale) -> string */  {
  var _x46 = (locale !== undefined) ? locale : $std_time_locale.time_locale_en_iso;
  var _x47 = (locale !== undefined) ? locale : $std_time_locale.time_locale_en_iso;
  var _x48 = (locale !== undefined) ? locale : $std_time_locale.time_locale_en_iso;
  return format_list(t, $std_core_string.list(expand_locales(expand_locales(fmt, _x46), _x47)), _x48);
}
 
 
// Show a time as a standard ISO date. Will use automatic week date
// or month date format for the  `cal-iso-week` and `cal-iso-month` calendars.
export function show_iso_date(t) /* (t : std/time/time/time) -> string */  {
  var _x50 = undefined;
  var _x49 = (_x50 !== undefined) ? _x50 : $std_time_locale.time_locale_en_iso;
  var _x52 = undefined;
  var _x51 = (_x52 !== undefined) ? _x52 : $std_time_locale.time_locale_en_iso;
  var _x54 = undefined;
  var _x53 = (_x54 !== undefined) ? _x54 : $std_time_locale.time_locale_en_iso;
  return format_list(t, $std_core_string.list(expand_locales(expand_locales("YYYY-MM-DD", _x49), _x51)), _x53);
}
 
 
// Show a time as a standard ISO string. Will use automatic week date
// or month date format for the `cal-iso-week` and `cal-iso-month` calendars.
export function show_iso(t) /* (t : std/time/time/time) -> string */  {
   
  var fmt_10119 = $std_core_types._lp__plus__plus__rp_("HH:mm:ssFFFFFFFFF", "Z C");
  var _x56 = undefined;
  var _x55 = (_x56 !== undefined) ? _x56 : $std_time_locale.time_locale_en_iso;
  var _x58 = undefined;
  var _x57 = (_x58 !== undefined) ? _x58 : $std_time_locale.time_locale_en_iso;
  var _x60 = undefined;
  var _x59 = (_x60 !== undefined) ? _x60 : $std_time_locale.time_locale_en_iso;
  return $std_core_types._lp__plus__plus__rp_(show_iso_date(t), $std_core_types._lp__plus__plus__rp_("T", format_list(t, $std_core_string.list(expand_locales(expand_locales(fmt_10119, _x55), _x57)), _x59)));
}
 
 
// Show time as a standard [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3) date.\
// For example `now().show-imf` returns `"Fri, 9 Oct 2016 11:57:45 -0700"`
export function show_imf(t) /* (t : std/time/time/time) -> string */  {
   
  var t_0_10121 = $std_time_time.time_fs_time(t, undefined, $std_time_calendar.cal_iso);
  var _x62 = undefined;
  var _x61 = (_x62 !== undefined) ? _x62 : $std_time_locale.time_locale_en_iso;
  var _x64 = undefined;
  var _x63 = (_x64 !== undefined) ? _x64 : $std_time_locale.time_locale_en_iso;
  var _x66 = undefined;
  var _x65 = (_x66 !== undefined) ? _x66 : $std_time_locale.time_locale_en_iso;
  return format_list(t_0_10121, $std_core_string.list(expand_locales(expand_locales("ddd, D MMM Y HH:mm:ss zz", _x61), _x63)), _x65);
}
 
 
// Show the time as a human readable string in the given `locale` (=`time-locale-en`)
// For example `now().show-en` -> `"Thu, 8 Oct 2016, 12:20pm"`. Uses the `"llll"` format string.
export function show_in_locale(t, locale) /* (t : std/time/time/time, locale : ? std/time/locale/time-locale) -> string */  {
  var _x67 = (locale !== undefined) ? locale : $std_time_locale.time_locale_en;
  var _x68 = (locale !== undefined) ? locale : $std_time_locale.time_locale_en;
  var _x69 = (locale !== undefined) ? locale : $std_time_locale.time_locale_en;
  return format_list(t, $std_core_string.list(expand_locales(expand_locales("llll", _x67), _x68)), _x69);
}
 
 
// Show the date in human readable string in the given `locale` (=`time-locale-en`).
// For example `now().show-en-date` -> `"Thu, 8 Oct 2016"`. Uses the `"ll"` format string.
export function show_in_locale_date(t, locale) /* (t : std/time/time/time, locale : ? std/time/locale/time-locale) -> string */  {
  var _x70 = (locale !== undefined) ? locale : $std_time_locale.time_locale_en;
  var _x71 = (locale !== undefined) ? locale : $std_time_locale.time_locale_en;
  var _x72 = (locale !== undefined) ? locale : $std_time_locale.time_locale_en;
  return format_list(t, $std_core_string.list(expand_locales(expand_locales("ll", _x70), _x71)), _x72);
}
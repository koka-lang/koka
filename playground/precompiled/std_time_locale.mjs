// Koka generated module: std/time/locale, koka version: 3.2.4
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
// type time-locale
export function Time_locale(lang_name, day_names, month_names, month_names_short, day_names_short, day_names_min, format_t, format_tt, format_l, format_ll, format_lll, format_llll) /* (lang-name : string, day-names : list<string>, month-names : list<string>, month-names-short : list<string>, day-names-short : list<string>, day-names-min : list<string>, format-t : string, format-tt : string, format-l : string, format-ll : string, format-lll : string, format-llll : string) -> time-locale */  {
  return { lang_name: lang_name, day_names: day_names, month_names: month_names, month_names_short: month_names_short, day_names_short: day_names_short, day_names_min: day_names_min, format_t: format_t, format_tt: format_tt, format_l: format_l, format_ll: format_ll, format_lll: format_lll, format_llll: format_llll };
}
 
// declarations
 
 
// The `:time-locale` describes time and date formats for a specific locale.
export function _create_Time_locale(lang_name, day_names, month_names, month_names_short, day_names_short, day_names_min, format_t, format_tt, format_l, format_ll, format_lll, format_llll) /* (lang-name : string, day-names : list<string>, month-names : list<string>, month-names-short : ? (list<string>), day-names-short : ? (list<string>), day-names-min : ? (list<string>), format-t : ? string, format-tt : ? string, format-l : ? string, format-ll : ? string, format-lll : ? string, format-llll : ? string) -> time-locale */  {
   
  if (month_names_short !== undefined) {
    var _uniq_month_names_short_96 = month_names_short;
  }
  else {
    var _uniq_month_names_short_96 = $std_core_list.map(month_names, function(m /* string */ ) {
        return $std_core_sslice.string($std_core_sslice.first(m, 3));
      });
  }
   
  if (day_names_short !== undefined) {
    var _uniq_day_names_short_161 = day_names_short;
  }
  else {
    var _uniq_day_names_short_161 = $std_core_list.map(day_names, function(m_0 /* string */ ) {
        return $std_core_sslice.string($std_core_sslice.first(m_0, 3));
      });
  }
   
  if (day_names_min !== undefined) {
    var _uniq_day_names_min_226 = day_names_min;
  }
  else {
    var _uniq_day_names_min_226 = $std_core_list.map(day_names, function(m_1 /* string */ ) {
        return $std_core_sslice.string($std_core_sslice.first(m_1, 2));
      });
  }
  var _x0 = (format_t !== undefined) ? format_t : "HH:mm";
  var _x1 = (format_tt !== undefined) ? format_tt : "HH:mm:ss";
  var _x2 = (format_l !== undefined) ? format_l : "YYYY-MM-DD";
  var _x3 = (format_ll !== undefined) ? format_ll : "D MMMM YYYY";
  var _x4 = (format_lll !== undefined) ? format_lll : "D MMMM YYYY t";
  var _x5 = (format_llll !== undefined) ? format_llll : "dddd D MMMM YYYY t";
  return Time_locale(lang_name, day_names, month_names, _uniq_month_names_short_96, _uniq_day_names_short_161, _uniq_day_names_min_226, _x0, _x1, _x2, _x3, _x4, _x5);
}
 
 
// Automatically generated. Retrieves the `lang-name` constructor field of the `:time-locale` type.
export function time_locale_fs_lang_name(_this) /* (time-locale) -> string */  {
  return _this.lang_name;
}
 
 
// Automatically generated. Retrieves the `day-names` constructor field of the `:time-locale` type.
export function time_locale_fs_day_names(_this) /* (time-locale) -> list<string> */  {
  return _this.day_names;
}
 
 
// Automatically generated. Retrieves the `month-names` constructor field of the `:time-locale` type.
export function time_locale_fs_month_names(_this) /* (time-locale) -> list<string> */  {
  return _this.month_names;
}
 
 
// Automatically generated. Retrieves the `month-names-short` constructor field of the `:time-locale` type.
export function time_locale_fs_month_names_short(_this) /* (time-locale) -> list<string> */  {
  return _this.month_names_short;
}
 
 
// Automatically generated. Retrieves the `day-names-short` constructor field of the `:time-locale` type.
export function time_locale_fs_day_names_short(_this) /* (time-locale) -> list<string> */  {
  return _this.day_names_short;
}
 
 
// Automatically generated. Retrieves the `day-names-min` constructor field of the `:time-locale` type.
export function time_locale_fs_day_names_min(_this) /* (time-locale) -> list<string> */  {
  return _this.day_names_min;
}
 
 
// Automatically generated. Retrieves the `format-t` constructor field of the `:time-locale` type.
export function time_locale_fs_format_t(_this) /* (time-locale) -> string */  {
  return _this.format_t;
}
 
 
// Automatically generated. Retrieves the `format-tt` constructor field of the `:time-locale` type.
export function time_locale_fs_format_tt(_this) /* (time-locale) -> string */  {
  return _this.format_tt;
}
 
 
// Automatically generated. Retrieves the `format-l` constructor field of the `:time-locale` type.
export function time_locale_fs_format_l(_this) /* (time-locale) -> string */  {
  return _this.format_l;
}
 
 
// Automatically generated. Retrieves the `format-ll` constructor field of the `:time-locale` type.
export function time_locale_fs_format_ll(_this) /* (time-locale) -> string */  {
  return _this.format_ll;
}
 
 
// Automatically generated. Retrieves the `format-lll` constructor field of the `:time-locale` type.
export function time_locale_fs_format_lll(_this) /* (time-locale) -> string */  {
  return _this.format_lll;
}
 
 
// Automatically generated. Retrieves the `format-llll` constructor field of the `:time-locale` type.
export function time_locale_fs_format_llll(_this) /* (time-locale) -> string */  {
  return _this.format_llll;
}
 
export function time_locale_fs__copy(_this, lang_name, day_names, month_names, month_names_short, day_names_short, day_names_min, format_t, format_tt, format_l, format_ll, format_lll, format_llll) /* (time-locale, lang-name : ? string, day-names : ? (list<string>), month-names : ? (list<string>), month-names-short : ? (list<string>), day-names-short : ? (list<string>), day-names-min : ? (list<string>), format-t : ? string, format-tt : ? string, format-l : ? string, format-ll : ? string, format-lll : ? string, format-llll : ? string) -> time-locale */  {
  if (lang_name !== undefined) {
    var _x6 = lang_name;
  }
  else {
    var _x6 = _this.lang_name;
  }
  if (day_names !== undefined) {
    var _x7 = day_names;
  }
  else {
    var _x7 = _this.day_names;
  }
  if (month_names !== undefined) {
    var _x8 = month_names;
  }
  else {
    var _x8 = _this.month_names;
  }
  if (month_names_short !== undefined) {
    var _x9 = month_names_short;
  }
  else {
    var _x9 = _this.month_names_short;
  }
  if (day_names_short !== undefined) {
    var _x10 = day_names_short;
  }
  else {
    var _x10 = _this.day_names_short;
  }
  if (day_names_min !== undefined) {
    var _x11 = day_names_min;
  }
  else {
    var _x11 = _this.day_names_min;
  }
  if (format_t !== undefined) {
    var _x12 = format_t;
  }
  else {
    var _x12 = _this.format_t;
  }
  if (format_tt !== undefined) {
    var _x13 = format_tt;
  }
  else {
    var _x13 = _this.format_tt;
  }
  if (format_l !== undefined) {
    var _x14 = format_l;
  }
  else {
    var _x14 = _this.format_l;
  }
  if (format_ll !== undefined) {
    var _x15 = format_ll;
  }
  else {
    var _x15 = _this.format_ll;
  }
  if (format_lll !== undefined) {
    var _x16 = format_lll;
  }
  else {
    var _x16 = _this.format_lll;
  }
  if (format_llll !== undefined) {
    var _x17 = format_llll;
  }
  else {
    var _x17 = _this.format_llll;
  }
  return Time_locale(_x6, _x7, _x8, _x9, _x10, _x11, _x12, _x13, _x14, _x15, _x16, _x17);
}
 
 
// English time locale (`en`)
export var time_locale_en;
 
var day_names_10013 = $std_core_types.Cons("Monday", $std_core_types.Cons("Tuesday", $std_core_types.Cons("Wednesday", $std_core_types.Cons("Thursday", $std_core_types.Cons("Friday", $std_core_types.Cons("Saturday", $std_core_types.Cons("Sunday", $std_core_types.Nil)))))));
 
var month_names_10014 = $std_core_vector.vlist(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], $std_core_types.Nil);
 
var _x18 = undefined;
if (_x18 !== undefined) {
  var _uniq_month_names_short_96 = _x18;
}
else {
  var _uniq_month_names_short_96 = $std_core_list.map(month_names_10014, function(m /* string */ ) {
      return $std_core_sslice.string($std_core_sslice.first(m, 3));
    });
}
 
var _x19 = undefined;
if (_x19 !== undefined) {
  var _uniq_day_names_short_161 = _x19;
}
else {
  var _uniq_day_names_short_161 = $std_core_list.map(day_names_10013, function(m_0 /* string */ ) {
      return $std_core_sslice.string($std_core_sslice.first(m_0, 3));
    });
}
 
var _x20 = undefined;
if (_x20 !== undefined) {
  var _uniq_day_names_min_226 = _x20;
}
else {
  var _uniq_day_names_min_226 = $std_core_list.map(day_names_10013, function(m_1 /* string */ ) {
      return $std_core_sslice.string($std_core_sslice.first(m_1, 2));
    });
}
var _x19 = undefined;
var _x18 = (_x19 !== undefined) ? _x19 : "D MMMM YYYY";
var _x21 = undefined;
var _x20 = (_x21 !== undefined) ? _x21 : "D MMMM YYYY t";
var time_locale_en = Time_locale("en", day_names_10013, month_names_10014, _uniq_month_names_short_96, _uniq_day_names_short_161, _uniq_day_names_min_226, "h:mmaa", "h:mm:ssaa", "MM/DD/YYYY", _x18, _x20, "dddd, D MMMM YYYY t");
 
 
// ISO English time locale (`en-iso`). Uses English names for
// months and days but displays numeric dates and times using unambigious ISO format.
export var time_locale_en_iso;
 
var _x22 = undefined;
if (_x22 !== undefined) {
  var _uniq_month_names_short_96 = _x22;
}
else {
  var _x23 = time_locale_en.month_names;
  var _uniq_month_names_short_96 = $std_core_list.map(_x23, function(m /* string */ ) {
      return $std_core_sslice.string($std_core_sslice.first(m, 3));
    });
}
 
var _x24 = undefined;
if (_x24 !== undefined) {
  var _uniq_day_names_short_161 = _x24;
}
else {
  var _x25 = time_locale_en.day_names;
  var _uniq_day_names_short_161 = $std_core_list.map(_x25, function(m_0 /* string */ ) {
      return $std_core_sslice.string($std_core_sslice.first(m_0, 3));
    });
}
 
var _x26 = undefined;
if (_x26 !== undefined) {
  var _uniq_day_names_min_226 = _x26;
}
else {
  var _x27 = time_locale_en.day_names;
  var _uniq_day_names_min_226 = $std_core_list.map(_x27, function(m_1 /* string */ ) {
      return $std_core_sslice.string($std_core_sslice.first(m_1, 2));
    });
}
var _x22 = time_locale_en.day_names;
var _x23 = time_locale_en.month_names;
var _x25 = undefined;
var _x24 = (_x25 !== undefined) ? _x25 : "HH:mm";
var _x27 = undefined;
var _x26 = (_x27 !== undefined) ? _x27 : "HH:mm:ss";
var _x29 = undefined;
var _x28 = (_x29 !== undefined) ? _x29 : "YYYY-MM-DD";
var _x31 = undefined;
var _x30 = (_x31 !== undefined) ? _x31 : "D MMMM YYYY";
var time_locale_en_iso = Time_locale("en-iso", _x22, _x23, _uniq_month_names_short_96, _uniq_day_names_short_161, _uniq_day_names_min_226, _x24, _x26, _x28, _x30, "D MMMM YYYY, tt", "dddd, D MMMM YYYY, tt");
 
 
// French time locale (`fr`)
export var time_locale_fr;
var _x33 = undefined;
var _x32 = (_x33 !== undefined) ? _x33 : "HH:mm";
var _x35 = undefined;
var _x34 = (_x35 !== undefined) ? _x35 : "HH:mm:ss";
var _x37 = undefined;
var _x36 = (_x37 !== undefined) ? _x37 : "D MMMM YYYY";
var _x39 = undefined;
var _x38 = (_x39 !== undefined) ? _x39 : "D MMMM YYYY t";
var _x41 = undefined;
var _x40 = (_x41 !== undefined) ? _x41 : "dddd D MMMM YYYY t";
var time_locale_fr = Time_locale("fr", $std_core_types.Cons("lundi", $std_core_types.Cons("mardi", $std_core_types.Cons("mercredi", $std_core_types.Cons("jeudi", $std_core_types.Cons("vendredi", $std_core_types.Cons("samedi", $std_core_types.Cons("dimanche", $std_core_types.Nil))))))), $std_core_vector.vlist(["janvier", "f\u00E9vrier", "mars", "avril", "mai", "juin", "juillet", "ao\u00FBt", "septembre", "octobre", "novembre", "d\u00E9cembre"], $std_core_types.Nil), $std_core_vector.vlist(["janv.", "f\u00E9vr.", "mars", "avr.", "mai", "juin", "juil.", "ao\u00FBt", "sept.", "oct.", "nov.", "d\u00E9c."], $std_core_types.Nil), $std_core_types.Cons("lun.", $std_core_types.Cons("mar.", $std_core_types.Cons("mer.", $std_core_types.Cons("jeu.", $std_core_types.Cons("ven.", $std_core_types.Cons("sam.", $std_core_types.Cons("dim.", $std_core_types.Nil))))))), $std_core_types.Cons("Lu", $std_core_types.Cons("Ma", $std_core_types.Cons("Me", $std_core_types.Cons("Je", $std_core_types.Cons("Ve", $std_core_types.Cons("Sa", $std_core_types.Cons("Di", $std_core_types.Nil))))))), _x32, _x34, "DD/MM/YYYY", _x36, _x38, _x40);
 
 
// German time locale (`de`)
export var time_locale_de;
var _x43 = undefined;
var _x42 = (_x43 !== undefined) ? _x43 : "HH:mm";
var _x45 = undefined;
var _x44 = (_x45 !== undefined) ? _x45 : "HH:mm:ss";
var time_locale_de = Time_locale("de", $std_core_types.Cons("Montag", $std_core_types.Cons("Dienstag", $std_core_types.Cons("Mittwoch", $std_core_types.Cons("Donnerstag", $std_core_types.Cons("Freitag", $std_core_types.Cons("Samstag", $std_core_types.Cons("Sonntag", $std_core_types.Nil))))))), $std_core_vector.vlist(["Januar", "Februar", "M\u00E4rz", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"], $std_core_types.Nil), $std_core_vector.vlist(["Jan.", "Febr.", "Mrz.", "Apr.", "Mai", "Jun.", "Jul.", "Aug.", "Sept.", "Okt.", "Nov.", "Dez."], $std_core_types.Nil), $std_core_types.Cons("Mo.", $std_core_types.Cons("Di.", $std_core_types.Cons("Mi.", $std_core_types.Cons("Do.", $std_core_types.Cons("Fr.", $std_core_types.Cons("Sa.", $std_core_types.Cons("So.", $std_core_types.Nil))))))), $std_core_types.Cons("Mo", $std_core_types.Cons("Di", $std_core_types.Cons("Mi", $std_core_types.Cons("Do", $std_core_types.Cons("Fr", $std_core_types.Cons("Sa", $std_core_types.Cons("So", $std_core_types.Nil))))))), _x42, _x44, "DD.MM.YYYY", "D. MMMM YYYY", "D. MMMM YYYY t", "dddd, D. MMMM YYYY t");
 
 
// Spanish time locale (`es`)
export var time_locale_es;
 
var day_names_10051 = $std_core_types.Cons("lunes", $std_core_types.Cons("martes", $std_core_types.Cons("mi\u00E9rcoles", $std_core_types.Cons("jueves", $std_core_types.Cons("viernes", $std_core_types.Cons("s\u00E1bado", $std_core_types.Cons("domingo", $std_core_types.Nil)))))));
 
var month_names_10052 = $std_core_vector.vlist(["enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"], $std_core_types.Nil);
 
var _x46 = undefined;
if (_x46 !== undefined) {
  var _uniq_month_names_short_96 = _x46;
}
else {
  var _uniq_month_names_short_96 = $std_core_list.map(month_names_10052, function(m /* string */ ) {
      return $std_core_sslice.string($std_core_sslice.first(m, 3));
    });
}
 
var _x47 = undefined;
if (_x47 !== undefined) {
  var _uniq_day_names_short_161 = _x47;
}
else {
  var _uniq_day_names_short_161 = $std_core_list.map(day_names_10051, function(m_0 /* string */ ) {
      return $std_core_sslice.string($std_core_sslice.first(m_0, 3));
    });
}
 
var _x48 = undefined;
if (_x48 !== undefined) {
  var _uniq_day_names_min_226 = _x48;
}
else {
  var _uniq_day_names_min_226 = $std_core_list.map(day_names_10051, function(m_1 /* string */ ) {
      return $std_core_sslice.string($std_core_sslice.first(m_1, 2));
    });
}
var time_locale_es = Time_locale("es", day_names_10051, month_names_10052, _uniq_month_names_short_96, _uniq_day_names_short_161, _uniq_day_names_min_226, "H:mm", "H:mm:ss", "DD/MM/YYYY", "D \'de\' MMMM \'de\' YYYY", "D \'de\' MMMM \'de\' YYYY H:mm", "dddd, D \'de\' MMMM \'de\' YYYY H:mm");
 
 
// Dutch time locale (`nl`)
export var time_locale_nl;
var _x47 = undefined;
var _x46 = (_x47 !== undefined) ? _x47 : "HH:mm";
var _x49 = undefined;
var _x48 = (_x49 !== undefined) ? _x49 : "HH:mm:ss";
var _x51 = undefined;
var _x50 = (_x51 !== undefined) ? _x51 : "D MMMM YYYY";
var _x53 = undefined;
var _x52 = (_x53 !== undefined) ? _x53 : "D MMMM YYYY t";
var _x55 = undefined;
var _x54 = (_x55 !== undefined) ? _x55 : "dddd D MMMM YYYY t";
var time_locale_nl = Time_locale("nl", $std_core_types.Cons("maandag", $std_core_types.Cons("dinsdag", $std_core_types.Cons("woensdag", $std_core_types.Cons("donderdag", $std_core_types.Cons("vrijdag", $std_core_types.Cons("zaterdag", $std_core_types.Cons("zondag", $std_core_types.Nil))))))), $std_core_vector.vlist(["januari", "februari", "maart", "april", "mei", "juni", "juli", "augustus", "september", "oktober", "november", "december"], $std_core_types.Nil), $std_core_vector.vlist(["jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"], $std_core_types.Nil), $std_core_types.Cons("ma.", $std_core_types.Cons("di.", $std_core_types.Cons("wo.", $std_core_types.Cons("do.", $std_core_types.Cons("vr.", $std_core_types.Cons("za.", $std_core_types.Cons("zo.", $std_core_types.Nil))))))), $std_core_types.Cons("Ma", $std_core_types.Cons("Di", $std_core_types.Cons("Wo", $std_core_types.Cons("Do", $std_core_types.Cons("Vr", $std_core_types.Cons("Za", $std_core_types.Cons("Zo", $std_core_types.Nil))))))), _x46, _x48, "DD-MM-YYYY", _x50, _x52, _x54);
 
export var locales;
var locales = $std_core_types.Cons(time_locale_en_iso, $std_core_types.Cons(time_locale_de, $std_core_types.Cons(time_locale_en, $std_core_types.Cons(time_locale_es, $std_core_types.Cons(time_locale_fr, $std_core_types.Cons(time_locale_nl, $std_core_types.Nil))))));
 
 
// Return a builtin locale given a locale string (e.g. ``en-GB``, ``es``);
// Use `time-locale-iso` if no particular match is found.
// Supports ``de``,``en``,``es``,``fr``,``nl`` and ``en-iso``.
export function get_time_locale(locale) /* (locale : string) -> time-locale */  {
   
  var res = $std_core_list.find(locales, function(l /* time-locale */ ) {
       
      var _x56 = l.lang_name;
      var maybe_10074 = $std_core_sslice.starts_with(locale, _x56);
      return (maybe_10074 !== null);
    });
  return (res === null) ? time_locale_en_iso : res.value;
}
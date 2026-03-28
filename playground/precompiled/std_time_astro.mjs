// Koka generated module: std/time/astro, koka version: 3.2.4
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
import * as $std_num_float64 from './std_num_float64.mjs';
import * as $std_num_ddouble from './std_num_ddouble.mjs';
import * as $std_time_timestamp from './std_time_timestamp.mjs';
import * as $std_time_duration from './std_time_duration.mjs';
import * as $std_time_instant from './std_time_instant.mjs';
import * as $std_time_calendar from './std_time_calendar.mjs';
import * as $std_time_utc from './std_time_utc.mjs';
import * as $std_time_calendars from './std_time_calendars.mjs';
import * as $std_num_int32 from './std_num_int32.mjs';
 
// externals
 
// type declarations
 
// declarations
 
export var secs_per_day;
var secs_per_day = $std_num_ddouble.ddouble_int_exp(86400, 0);
 
export var offset_j2000;
var offset_j2000 = $std_num_ddouble.ddouble_int_exp(43200, 0);
 
 
// Return the instant corresponding to a J2000 julian date: this is the number of days, and
// the fraction of the day since J2000 (`epoch-j2000`) in terrestrial time (`ts-tt`).\
// `instant-at-j2000(zero).time.show == "2000-01-01T11:58:55.816Z"`\
// `instant-at-j2000(zero).time(cal=cal-tt).show == "2000-01-01T12:00:00Z TT"`
export function instant_at_j2000(jd) /* (jd : std/num/ddouble/ddouble) -> std/time/instant/instant */  {
   
  var t_0_10004 = $std_num_ddouble._lp__plus__rp_($std_num_ddouble._lp__star__rp_(jd, secs_per_day), offset_j2000);
   
  var _x1 = undefined;
  var _x0 = (_x1 !== undefined) ? _x1 : 0;
  var t_10003 = $std_time_timestamp.Timestamp(t_0_10004, $std_core_types._int_clamp32(_x0));
  return $std_time_instant.Instant(t_10003, $std_time_instant.ts_tt);
}
 
 
// The [J2000] epoch is defined as 2000-01-01T12:00:00 TT (Terrestrial time).
//
// [J2000]: https://en.wikipedia.org/wiki/Equinox_(celestial_coordinates)#J2000.0
export var epoch_j2000;
var epoch_j2000 = instant_at_j2000($std_num_ddouble.zero);
 
 
// Return the J2000 julian date for an instant. This is the number of days, and
// the fraction of the day since J2000 (`epoch-j2000`) in terrestrial time (`ts-tt`).\
// `instant(2000,1,1,12,0,0).j2000.show == "0.00074287037037"`\
// `instant(2000,1,1,12,0,0,cal=cal-tt).j2000.show == "0"`
export function j2000(i) /* (i : std/time/instant/instant) -> std/num/ddouble/ddouble */  {
   
  var _x0 = i.since;
  var _x1 = i.ts;
  var ts_10009 = $std_time_timestamp._lp__dash__rp_($std_time_instant.convert(_x0, _x1, $std_time_instant.ts_tt), offset_j2000);
  var _x0 = ts_10009.since;
  return $std_num_ddouble._lp__fs__rp_(_x0, secs_per_day);
}
 
export var lg;
var lg = $std_num_ddouble.int_fs_ddouble_exp(6969290134, $std_core_types._int_negate(19));
 
 
// 6.969290134e-10  // 1991: 6.969291e-10  // now: 6.969290134
export var tcg_epoch_delta;
var tcg_epoch_delta = $std_num_ddouble.int_fs_ddouble_exp($std_core_types._int_negate(725759967816), $std_core_types._int_negate(3));
 
 
// IAU 1991 recommendation IV :\
// TCG - TT = L~g~&times;(JD~TT~ - TCG~epoch~)
// =>
// TCG = TT + Lg*(TT - TCG~epoch~)
export function tcg_from_tai(tai) /* (tai : std/time/duration/duration) -> std/time/timestamp/timestamp */  {
   
  var _x1 = tai;
  var _x3 = undefined;
  var _x2 = (_x3 !== undefined) ? _x3 : 0;
  var tt = $std_time_instant.convert($std_time_timestamp.Timestamp(_x1, $std_core_types._int_clamp32(_x2)), $std_time_instant.ts_tai, $std_time_instant.ts_tt);
   
  var ts_1_10016 = $std_time_timestamp._lp__dash__rp_(tt, tcg_epoch_delta);
   
  var _x4 = ts_1_10016.since;
  var t_10015 = $std_num_ddouble._lp__star__rp_(lg, _x4);
  var _x1 = tt.since;
  var _x2 = tt.leap32;
  return $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x1, t_10015), _x2);
}
 
 
// IAU 1991 recommendation [IV](https://www.iers.org/IERS/EN/Science/Recommendations/recommendation4.html?nn=12932):\
// TCG - TT = L~g~&times;(JD~TT~ - TCG~epoch~)
// =>
// TT = TCG - Lg*(TCG - TCG~epoch~)
export function tcg_to_tai(t) /* (t : std/time/timestamp/timestamp) -> std/time/duration/duration */  {
   
  var ts_10017 = $std_time_timestamp._lp__dash__rp_(t, tcg_epoch_delta);
   
  var _x3 = ts_10017.since;
  var tt = $std_time_timestamp._lp__dash__rp_(t, $std_num_ddouble._lp__star__rp_(lg, _x3));
   
  var t_0_10075 = $std_time_instant.convert(tt, $std_time_instant.ts_tt, $std_time_instant.ts_tai);
  return $std_time_timestamp.unsafe_timespan_withleap(t_0_10075);
}
 
 
// The [TCG](https://en.wikipedia.org/wiki/Geocentric_Coordinate_Time) (Geocentric coordinate time) time scale.
// The unit of TCG is the SI second in a coordinate reference frame at the center of the Earth.
export var ts_tcg;
var ts_tcg = $std_time_instant.timescale("TCG", tcg_from_tai, tcg_to_tai);
 
 
/*----------------------------------------------------------------------------
  TDB: Barycentric dynamical time
----------------------------------------------------------------------------*/
export var j2k_epoch;
var j2k_epoch = $std_time_timestamp.int_fs_timespan(43200);
 
 
// 2000-01-01T12:00:00 TT
export var secs_per_julian_year;
var secs_per_julian_year = 3.15576e7;
 
 
// Compute tdb-delta(TT) = TDB - TT. This is the default function used for `ts-tdb`.
// Fairhead & Bretagnon (1990) initial coefficents.
// See also USNO circular 179 ([@Kaplan:circ], section 2.6).
// The error in this approximation is a maximum of 10 micro-seconds in the time period 1600 to 2200.
// (and probably less as we expanded to more terms and do JPL planetary mass adjustment)
export function tdb_delta(tt) /* (tt : std/time/timestamp/timestamp) -> std/time/timestamp/timespan */  {
   
  var ts_10021 = $std_time_timestamp._lp__dash__rp_(tt, j2k_epoch);
   
  var _x3 = ts_10021.since.hi;
  var t = (_x3 / (((3.15576e7) * (1000.0))));
   
  var wf = ((((((((1.6566746e-3) * (Math.sin((((((6283.0758) * t)) + (6.2401))))))) + (((((((((2.24175e-5) * (Math.sin((((((5753.3859) * t)) + (4.297))))))) + (((1.38398e-5) * (Math.sin((((((12566.1527) * t)) + (6.1969))))))))) + (((4.7701e-6) * (Math.sin((((((529.691) * t)) + (0.4444))))))))) + (((4.6767e-6) * (Math.sin((((((6069.7768) * t)) + (4.0212))))))))))) + (((((((((((2.2567e-6) * (Math.sin((((((213.2991) * t)) + (5.5431))))))) + (((1.6942e-6) * (Math.sin(((((((-(3.5232))) * t)) + (5.0251))))))))) + (((1.5549e-6) * (Math.sin((((((77713.7715) * t)) + (5.1985))))))))) + (((1.2768e-6) * (Math.sin((((((7860.4194) * t)) + (5.9888))))))))) + (((1.1934e-6) * (Math.sin((((((5223.6939) * t)) + (3.6498))))))))))) + ((t * (((1.021567e-4) * (Math.sin((((((6283.0758) * t)) + (4.249))))))))));
   
  var wj = ((((((((((6.5e-10) * (Math.sin((((((6069.776754) * t)) + (4.021194))))))) + (((3.3e-10) * (Math.sin((((((213.299095) * t)) + (5.543132))))))))) + ((((-(1.96e-9))) * (Math.sin((((((6208.294251) * t)) + (5.696701))))))))) + ((((-(1.73e-9))) * (Math.sin((((((74.781599) * t)) + (2.4359))))))))) + (((((3.638e-8) * t)) * t)));
   
  var d_10022 = (((wf + wj)) + (0.0));
  return $std_num_ddouble.Ddouble(d_10022, 0.0);
}
 
export var pi2;
var pi2 = 6.283185307179586;
 
 
// 2pi
export var dd2r;
var dd2r = 1.7453292519943295e-2;
 
 
// From the ERFA routines
export function tdb_topocentric(t, ut, elong, u, v) /* (t : float64, ut : ? float64, elong : ? float64, u : ? float64, v : ? float64) -> float64 */  {
   
  var _x3 = (ut !== undefined) ? ut : 0.0;
  var _x4 = (elong !== undefined) ? elong : 0.0;
  var tsol = (((($std_num_float64.fraction(_x3)) * (6.283185307179586))) + ((_x4 * (1.7453292519943295e-2))));
   
  var w = (t / (3600.0));
   
  var elsun = ((((((280.46645683) + (((1.29602771103429e9) * w)))) % (360.0))) * (1.7453292519943295e-2));
   
  var emsun = ((((((357.52910918) + (((1.295965810481e9) * w)))) % (360.0))) * (1.7453292519943295e-2));
   
  var d = ((((((297.85019547) + (((1.602961601209e10) * w)))) % (360.0))) * (1.7453292519943295e-2));
   
  var elj = ((((((34.35151874) + (((1.0930689989453e8) * w)))) % (360.0))) * (1.7453292519943295e-2));
   
  var els = ((((((50.0774443) + (((4.404639847038e7) * w)))) % (360.0))) * (1.7453292519943295e-2));
  var _x3 = (u !== undefined) ? u : 0.0;
  var _x4 = (u !== undefined) ? u : 0.0;
  var _x5 = (u !== undefined) ? u : 0.0;
  var _x6 = (u !== undefined) ? u : 0.0;
  var _x7 = (u !== undefined) ? u : 0.0;
  var _x8 = (v !== undefined) ? v : 0.0;
  var _x9 = (u !== undefined) ? u : 0.0;
  var _x10 = (u !== undefined) ? u : 0.0;
  var _x11 = (v !== undefined) ? v : 0.0;
  var _x12 = (u !== undefined) ? u : 0.0;
  return ((((((((2.9e-14) * _x3)) * (Math.sin(((((tsol + elsun)) - els)))))) + (((((((((((((1.0e-13) * _x4)) * (Math.sin(((tsol - (((2.0) * emsun)))))))) + (((((1.33e-13) * _x5)) * (Math.sin(((tsol - d)))))))) + (((((1.33e-13) * _x6)) * (Math.sin(((tsol + ((elsun - elj)))))))))) - (((((2.29e-13) * _x7)) * (Math.sin(((((tsol + (((2.0) * elsun)))) + emsun)))))))) - (((((2.2e-12) * _x8)) * (Math.cos(((elsun + emsun)))))))))) + (((((((((((5.312e-12) * _x9)) * (Math.sin(((tsol - emsun)))))) - (((((1.3677e-11) * _x10)) * (Math.sin(((tsol + (((2.0) * elsun)))))))))) - (((((1.3184e-10) * _x11)) * (Math.cos(elsun)))))) + (((((3.17679e-10) * _x12)) * (Math.sin(tsol)))))));
}
 
export function tai_to_tdb(tai, delta) /* (tai : std/time/duration/duration, delta : ? ((std/time/timestamp/timestamp) -> std/time/timestamp/timespan)) -> std/time/timestamp/timestamp */  {
   
  var _x13 = tai;
  var _x15 = undefined;
  var _x14 = (_x15 !== undefined) ? _x15 : 0;
  var tt = $std_time_instant.convert($std_time_timestamp.Timestamp(_x13, $std_core_types._int_clamp32(_x14)), $std_time_instant.ts_tai, $std_time_instant.ts_tt);
   
  if (delta !== undefined) {
    var t_10026 = delta(tt);
  }
  else {
    var t_10026 = tdb_delta(tt);
  }
  var _x13 = tt.since;
  var _x14 = tt.leap32;
  return $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x13, t_10026), _x14);
}
 
export function tdb_to_tai(tdb, delta) /* (tdb : std/time/timestamp/timestamp, delta : ? ((std/time/timestamp/timestamp) -> std/time/timestamp/timespan)) -> std/time/duration/duration */  {
   
  if (delta !== undefined) {
    var _x15 = delta(tdb);
  }
  else {
    var _x15 = tdb_delta(tdb);
  }
  var t_10028 = $std_time_timestamp._lp__dash__rp_(tdb, _x15);
   
  var t_0_10075 = $std_time_instant.convert(t_10028, $std_time_instant.ts_tt, $std_time_instant.ts_tai);
  return $std_time_timestamp.unsafe_timespan_withleap(t_0_10075);
}
 
 
// Creates a new TDB timescale given a user provided function to calculate TDB-TT.
// The default function used for `ts-tdb` uses the Fairhead &amp; Bretagnon approximation
// corrected for JPL planetary masses but assumes the geo center. Use this function
// to create a new TDB timescale if you need more precision.
export function ts_tdb_create(tdb_minus_tt) /* (tdb-minus-tt : (std/time/timestamp/timestamp) -> std/time/timestamp/timespan) -> std/time/instant/timescale */  {
  return $std_time_instant.timescale("TDB", function(tai /* std/time/duration/duration */ ) {
      return tai_to_tdb(tai, tdb_minus_tt);
    }, function(tdb /* std/time/timestamp/timestamp */ ) {
       
      var t_10032 = $std_time_timestamp._lp__dash__rp_(tdb, tdb_minus_tt(tdb));
       
      var t_0_10075 = $std_time_instant.convert(t_10032, $std_time_instant.ts_tt, $std_time_instant.ts_tai);
      return $std_time_timestamp.unsafe_timespan_withleap(t_0_10075);
    });
}
 
 
// The [TDB](https://en.wikipedia.org/wiki/Barycentric_Coordinate_Time) (Barycentric dynamical time) time scale
// (which can be considered equivalent T~eph~).
//
// TDB is approximated from TT using an initial set of coefficients by
// Fairhead &amp; Bretagnon [@Fairhead:tt] assuming the geo center (longitude and latitude of 0 degrees).
// See also USNO circular 179 ([@Kaplan:circ], equation 2.6).
// The approximation is within 10&mu;s precision in the time span 1600 to 2200 (and probably a bit less).
export var ts_tdb;
var ts_tdb = ts_tdb_create(tdb_delta);
 
export var lb;
var lb = $std_num_ddouble.int_fs_ddouble_exp(155051976772, $std_core_types._int_negate(19));
 
 
// 1.55051976772e-8
export var tdb0;
var tdb0 = $std_num_ddouble.int_fs_ddouble_exp($std_core_types._int_negate(655), $std_core_types._int_negate(7));
 
export function tai_to_tcb(tai, tstdb) /* (tai : std/time/duration/duration, tstdb : std/time/instant/timescale) -> std/time/timestamp/timestamp */  {
   
  var _x15 = tai;
  var _x17 = undefined;
  var _x16 = (_x17 !== undefined) ? _x17 : 0;
  var tdb = $std_time_instant.convert($std_time_timestamp.Timestamp(_x15, $std_core_types._int_clamp32(_x16)), $std_time_instant.ts_tai, tstdb);
   
  var ts_0_10035 = $std_time_timestamp._lp__dash__rp_(tdb, tcg_epoch_delta);
   
  var _x18 = ts_0_10035.since;
  var adj = $std_num_ddouble._lp__star__rp_(lb, _x18);
   
  var _x19 = tdb0.hi;
  var _x20 = tdb0.lo;
  var t_10037 = $std_num_ddouble._lp__plus__rp_(adj, $std_num_ddouble.Ddouble((-_x19), (-_x20)));
  var _x15 = tdb.since;
  var _x16 = tdb.leap32;
  return $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x15, t_10037), _x16);
}
 
export function tcb_to_tai(tcb, tstdb) /* (tcb : std/time/timestamp/timestamp, tstdb : std/time/instant/timescale) -> std/time/duration/duration */  {
   
  var ts_10040 = $std_time_timestamp._lp__dash__rp_(tcb, tcg_epoch_delta);
   
  var _x17 = ts_10040.since;
  var adj = $std_num_ddouble._lp__star__rp_(lb, _x17);
   
  var _x18 = tdb0.hi;
  var _x19 = tdb0.lo;
  var tdb = $std_time_timestamp._lp__dash__rp_(tcb, $std_num_ddouble._lp__plus__rp_(adj, $std_num_ddouble.Ddouble((-_x18), (-_x19))));
   
  var t_0_10075 = $std_time_instant.convert(tdb, tstdb, $std_time_instant.ts_tai);
  return $std_time_timestamp.unsafe_timespan_withleap(t_0_10075);
}
 
 
// Create a new TCB timescale given a TDB timescale (`tstdb`). The default
// `ts-tcb` uses the default `ts-tdb` timescale, but if you created
// a different TCB time scale (using `ts-tdb-create`) you can use
// this function to create a TCB time scale from it. It computes
// TCB from the provided TDB using the IAU 2006 resolution
// [B3](https://www.iau.org/static/resolutions/IAU2006_Resol3.pdf):\ TDB = TCB -
// L~b~ &times; (JD~TCB~ - T~0~) &times; 86400 + TDB~0~, with T~0~ =
// 2443144.5003725, L~b~ = 1.55051976772&times;10^-8^, TDB~0~ =
// -6.55&times;10^-5^.
export function ts_tcb_create(tstdb) /* (tstdb : std/time/instant/timescale) -> std/time/instant/timescale */  {
  return $std_time_instant.timescale("TCB", function(tai /* std/time/duration/duration */ ) {
      return tai_to_tcb(tai, tstdb);
    }, function(tcb /* std/time/timestamp/timestamp */ ) {
      return tcb_to_tai(tcb, tstdb);
    });
}
 
 
// The [TCB](https://en.wikipedia.org/wiki/Geocentric_Coordinate_Time)
// (Barycentric coordinate time) time scale.\ The unit of TCB is the SI
// second, but in a coordinate reference frame at the barycenter of the solar
// system.
export var ts_tcb;
var ts_tcb = $std_time_instant.timescale("TCB", function(tai /* std/time/duration/duration */ ) {
    return tai_to_tcb(tai, ts_tdb);
  }, function(tcb /* std/time/timestamp/timestamp */ ) {
    return tcb_to_tai(tcb, ts_tdb);
  });
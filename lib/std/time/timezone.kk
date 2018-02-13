/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   copyan be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Timezone support.

Can load timezone information from a time zone file using `load-timezones`.
The format of that file is [packed json]([packed json format](http://momentjs.com/timezone/docs/#/data-formats/packed-format).
The latest version can be found at: <https://github.com/moment/moment-timezone/blob/develop/data/packed/latest.json>.

```
val tzs = load-timezones()
println( now().time(tzs.timezone("US/Pacific")).show )
val zlocal = tzs.guess-local-timezone()
println( now().time(zlocal).show + " (" + zlocal.tz-name + ")")
println( now().time(tz-local()).show )
```
*/
module std/time/timezone

import std/async
import std/data/dict
import std/text/regex
import std/os/path
import std/os/file

import std/time/timestamp
import std/time/duration
import std/time/instant
import std/time/utc
import std/time/download
import std/time/calendar
import std/time/time
import std/time/chrono

extern include {
  js file "timezone-inline.js"
}



// Load a time zone information from a [packed json](http://momentjs.com/timezone/docs/#/data-formats/packed-format)
// format. Looks by default for the file `fname` (= `"config/timezones.json"` in the application directory).
// If the file is not present, it will be downloaded from `url`. The default `url` is:\
// <https://raw.githubusercontent.com/moment/moment-timezone/develop/data/packed/latest.json>\
// Set the `url` to the empty string to suppress automatic downloading.
// If the file is older than `download-delay` (=`4.weeks`) the file is automatically refreshed from `url`.
// The `download-timeout` (=`10.seconds`) gives the maximum time for downloading to finish.
// Set `verbose` to `True` to see trace messages.
public fun load-timezones( fname : path = path(""),
                           url   : string = "auto",
                           download-delay : duration   = 4.weeks,
                           download-timeout : duration = 10.seconds,
                           verbose : bool = False
                         ) : <io,async> timezones
{
  fun parse(s :string) : maybe<timezones> {
    (try{
       Just(parse-packed-zones(s))
     }
     fun(exn) {
       trace("failed parse: " + exn.show)
       Nothing
     })
  }
  val xfname = (fname || appdir() + path("config/timezones.json"))
  val xurl   = (if (url=="auto") then "https://raw.githubusercontent.com/moment/moment-timezone/develop/data/packed/latest.json" else url)
  load-latest( xfname, xurl, parse, download-delay=download-delay, download-timeout=download-timeout,
               error-prefix="load timezone table", verbose=verbose)
}


/*----------------------------------------------------------------------------
  Time zone info
----------------------------------------------------------------------------*/

// Describes time zone information with a name, population count, and a list of periods.
public struct timezone-info(
  name      : string,
  population: int,
  periods   : () -> list<timezone-period>, // time decreasing list of time periods
  aliased   : bool = False                 // is this an alias to another timezone? (used for timezone guessing)
)


// A period in a timezone where a specific time zone offset holds.
public struct timezone-period(
  start : maybe<instant>, // Nothing = beginning of time
  abbrv : string,
  offset: duration
)

// Zero time period
val timezone-period0 = Timezone-period( Nothing, "UTC", zero )

// The time zone info for the UTC time zone.
val tzi-utc : timezone-info = Timezone-info( "UTC", 0, fun(){ [] } )

// Find the DST offset for a particular instant `i` given a list of time zone periods
fun find-offset( i : instant, periods : list<timezone-period> ) : timezone-period {
  match(periods) {
    Nil -> timezone-period0
    Cons(tzp,earlier) ->
      if (i >= tzp.start.default(i)) then {
        //trace("timezone: " + tzp.abbrv + ", start: " + tzp.start.default(i).time.show)
        tzp
      } else find-offset(i,earlier)
  }
}

// Convert time zone information `:timezone-info` into a `:timezone` structure.
public fun timezone( tzi : timezone-info ) : timezone {
  fun utc-offset(i : instant) : (duration,string) {
    val tzp = find-offset(i.use-timescale(ts-unix),(tzi.periods)())
    (tzp.offset,tzp.abbrv)
  }
  calendar/Timezone(
    tzi.name,
    utc-offset,
    fun(i) { Nothing }
  )
}

// Show a time zone period.
public fun show( tzp : timezone-period) : string {
  tzp.abbrv.pad-right(4) + " " + tzp.offset.show-tzdelta(utc="+00:00") + " from " + tzp.start.map(fun(i){ i.time.show }).default("-infinity")
}

// Show time zone information.
public fun show (tzi : timezone-info, show-periods : bool = False  ) : string {
  tzi.name + ", population: " + tzi.population.show
    + (if (!show-periods) then "" else ":\n  " + (tzi.periods)().map(show : timezone-period -> string).join("\n  "))
}


/*----------------------------------------------------------------------------
  Parse timezone info from packed JSON format as used by momentjs.
  Latest files can be found at:\
  <https://github.com/moment/moment-timezone/blob/develop/data/packed/latest.json>
----------------------------------------------------------------------------*/

// Parse time zone information from a [packed json format](http://momentjs.com/timezone/docs/#/data-formats/packed-format).\
public fun parse-packed-zones( s : string ) : exn timezones {
  // zones
  val zonesfld = match(s.find(rxzones)) {
    Nothing   -> error("invalid timezone data; cannot find zones array.")
    Just(cap) -> cap.groups[1].trim
  }
  val zonesarr = zonesfld.lines.map( fun(line) {
    match(line.find(rxzone)) {
      Nothing   -> error("invalid timezone info:\n  " + line)
      Just(cap) -> cap.groups[1]
    }
  })
  val zones = zonesarr.map(parse-packed-zone)
  val zdict = zones.map(fun(tzi){ (tzi.name.normalize, tzi) }).dict

  // links
  val linksfld = match(s.find(rxlinks)) {
    Nothing   -> error("invalid timezone data; cannot find links array.")
    Just(cap) -> cap.groups[1].trim
  }
  val links = linksfld.lines.map( fun(line) {
    match(line.find(rxlink)) {
      Nothing   -> error("invalid timezone link:\n  " + line)
      Just(cap) -> (cap.groups[1],cap.groups[2])
    }
  })

  val ldict = links.map( fun(link) {
    val (key,other) = link
    match(zdict[key.normalize]) {
      Nothing  -> error("invalid timezone link: unknown time zone: " + key.normalize + ", to: " + other + ", " + zdict.keys.join("\n"))
      Just(tz) -> (other.normalize,tz(name=other,aliased=True))
    }
  }).dict

  // return zones + links
  Timezones( zdict + ldict )
}
val rxzones = regex(@"""zones""\s*:\s*\[([\s\S]*?)^[ \t]*\]", multiLine=True)
val rxzone  = regex(@"^\s*""([^""]+)"",?\s*$")
val rxlinks = regex(@"""links""\s*:\s*\[([\s\S]*?)^[ \t]*\]", multiLine=True)
val rxlink  = regex(@"^\s*""([^""\|]+)\|([^""\|]+)"",?\s*$")


fun parse-packed-zone( s : string ) : exn timezone-info {
  //trace("parseing: " + s)
  val parts     = s.split("|")
  val name      = parts[0].default("")
  val abbrs     = parts[1].default("")
  val offsets   = parts[2].default("0")
  val indices   = parts[3].default("0")
  val xdiffs    = parts[4].default("")
  val population= parts[5].default("0").parse-base60
  // delay parsing the periods until needed
  val periods   = once{ parse-packed-periods(name, abbrs,offsets,indices,xdiffs) }
  Timezone-info( name, population, periods, False )
}

fun parse-packed-periods( name : string, sabbrs : string, soffsets :string, sindices : string, sxdiffs :string ) : list<timezone-period>
{
  try {
    val abbrs   = sabbrs.split(" ").filter(notempty?)
    val offsets = soffsets.split(" ").filter(notempty?).map(parse-fixed-base60)
    val indices = sindices.list.map(digit60)
    val xdiffs  = sxdiffs.split(" ").filter(notempty?).map(parse-fixed-base60)
    fun from-indices( xs : list<a>, def : a ) : list<a> {
      indices.map( fun(idx) {xs[idx].default(def) }).reverse
    }
    val (start0,diffs) = match(xdiffs) {
      Nil -> (0.0,[])
      Cons(start,rest) -> (start * 60.0, Cons(0.0,rest))
    }
    if (diffs.length + 1 != indices.length ) error("invalid timezone data:\n  diffs: " + sxdiffs + "\n  ind  : " + sindices  )
    val endings = diffs.foldl((start0,[]), fun(acc:(double,list<maybe<instant>>),diff) {
                    val iu = acc.fst + diff*60.0
                    val i = unsafe-instant(timestamp(iu),ts-unix)
                    (iu,Cons(Just(i),acc.snd))
                  }).snd
    val ofsabrs  = indices.reverse.map( fun(i) { (offsets[i].default(0.0), abbrs[i].default("") ) } )
    val periods  = zipwith( endings + [Nothing], ofsabrs, fun(start,ofsabbr) {
                     Timezone-period(start,ofsabbr.snd, duration(ofsabbr.fst * ~60.0))
                   })
    periods
  }
  fun(exn) {
    trace("invalid timezone database entry for " + name.show + ":\n " + exn.show)
    []
  }
}

fun parse-fixed-base60( s : string ) : exn double {
  match(s.find(rxfixbase60)) {
    Nothing -> error("invalid fixed base60 number: " + s)
    Just(cap) -> {
      val sign = if (cap.groups[1]=="-") then ~1 else 1
      val num  = parse-base60(cap.groups[2])
      val frac = cap.groups[3].list.foldr(0.0, fun(d,n) {
                    (n + d.digit60.double) / 60.0
                 })
      (sign.double * num.double) + frac
    }
  }
}
val rxfixbase60 = regex(@"^([\-\+])?([\da-zA-X]*)(?:\.([\da-zA-X]+))?$")

fun parse-base60( s : string ) : exn int {
  if (s.empty?) return 0
  match(s.find(rxbase60)) {
    Nothing -> error("invalid base60 number: " + s)
    Just(cap) -> {
      val sign = if (cap.groups[1]=="-") then ~1 else 1
      val num  = cap.groups[2].list.foldl(0, fun(n,d) {
                    n*60 + d.digit60
                  })
      sign*num
    }
  }
}
val rxbase60 = regex(@"^([\-\+])?([\da-zA-X]+)$")

fun digit60( d : char ) : exn int {
  if (d.digit?) then (d - '0').int
  elif (d.lower?) then (d - 'a').int + 10
  elif (d.upper?) then (d - 'A').int + 36
  else error("invalid digit in base 60: " + d.show)
}


/*----------------------------------------------------------------------------
  Global timezone info
----------------------------------------------------------------------------*/

// The abstract `:timezones` represents time zone information.
abstract struct timezones( tzones : dict<timezone-info> )

// Create time zone information from a list of `:timezone-info`.
public fun timezones( zones : list<timezone-info> ) {
  Timezones( zones.map(fun(tzi) { (tzi.name.normalize, tzi) }).dict )
}

// Empty `:timezones`.
public val timezones-empty = Timezones([].dict)

// Load time zone information from a [packed json](http://momentjs.com/timezone/docs/#/data-formats/packed-format)
// file. Looks by default for the file `"config/timezones.json"` in the application directory
// but an optional file name can be supplied.
// The latest version can be found at: <https://github.com/moment/moment-timezone/blob/develop/data/packed/latest.json>.
public fun load-timezones-from( fname : string = "" ) : io timezones {
  val fpath = if (fname.empty?) then appdir() + path("config/timezones.json") else fname.path
  parse-packed-zones(read-text-file-sync(fpath.string))
}

fun normalize( name : string ) : string {
  name.to-lower.replace-all(rxsep,"")
}
val rxsep = regex(@"[\/\\\s\-_]+")

// Return all available IANA time zone identifiers.
public fun timezone-names( tzs : timezones ) : list<string> {
  tzs.tzones.list.map(fun(kv) { kv.snd.name })
}

// Return a `:timezone-info` for a IANA time zone identifier (if it exists)
public fun timezone-info( tzs : timezones, tz-name : string  ) : maybe<timezone-info> {
  //val tzs = unsafe-total(get-timezones)
  tzs.tzones[tz-name.normalize]
}

// Return a `:timezone` given a IANA time zone identifier, for example `tzs.timezone("US/Pacific")`
// `tzs.timezone("Asia/Singapore")`. Return `default` (= `tz-utc`) if the time zone is not found.
public fun timezone( tzs : timezones, tz-name : string, def : timezone = tz-utc ) : timezone {
  tzs.timezone-info(tz-name).map(timezone).default(def)
}


/*----------------------------------------------------------------------------
   Guess the local time zone
----------------------------------------------------------------------------*/

fun sample-dates( year : int ) : list<instant> {
  list(year - 1, year).flatmap( fun(y) {
    list(0,3).map( fun(m) { instant(y,m*4,15) } )
  })
}

fun sample-offsets( dates : list<instant>, tz : timezone ) : list<duration> {
  dates.map( fun(i) {
    i.time(tz=tz).tzdelta
  })
}

fun match-offsets( dates : list<instant>, offsets : list<duration>, tz : timezone ) : bool {
  val offsets2 = dates.sample-offsets(tz)
  zipwith(offsets,offsets2,fun(x,y) { x == y }).all(id)
}

// Guess the local timezone. Not as reliable as `tz-local` but will contain
// more information on certain platforms (i.e. the timezone abbreviation).
// Returns `tz-local()` if no match can be found (or a user supplied `default`).
public fun guess-local-timezone( tzs : timezones, default : maybe<timezone> = Nothing) : ndet timezone {
  match(tzs.timezone-info(local-timezone-name())) {
    Just(tzi) -> tzi.timezone
    Nothing {
      val year    = now().time.year
      val dates   = sample-dates(year)
      val offsets = dates.sample-offsets(tz-local())
      val matches = tzs.tzones.list.map(snd).filter( fun(tzi){
        (!tzi.aliased &&  dates.match-offsets(offsets,tzi.timezone))
      })
      // trace("timezone guess matches: " + matches.show-list(fun(tzi){ tzi.show }))
      match(matches) {
        Nil -> match(default) {
          Nothing  -> tz-local()
          Just(tz) -> tz
        }
        Cons(tz1,rest) -> {
          var pick := tz1
          rest.foreach fun(tz) {
            if (tz.population == 0 || (tz.population > pick.population && pick.population > 0)) {
              pick := tz
            }
          }
          pick.timezone
        }
      }
    }
  }
}

extern local-timezone-name() : ndet string {
  cs inline "TimeZoneInfo.Local.DisplayName"
  js "_local_timezone_name"
}

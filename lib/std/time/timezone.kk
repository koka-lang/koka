/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   copyan be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Timezone support.

  Looks by default for a file ``config/timezones.json`` in the application directory.
  The format of that file is _packed json_ as used by the Momentjs library. The latest
  version can be found at: <https://github.com/moment/moment-timezone/blob/develop/data/packed/latest.json>.
*/
module std/time/timezone

import std/dict
import std/regex
import std/path
import std/file
import std/fixed
import std/time/duration
import std/time/instant
import std/time/calendar

/*----------------------------------------------------------------------------
  Time zone info
----------------------------------------------------------------------------*/

// Describes time zone information with a name, population count, and a list of periods.
public struct timezone-info(
  name      : string,
  population: int,
  periods   : list<timezone-period>  // time decreasing list of time periods
)


// A period in a timezone where a specific time zone offset holds.
public struct timezone-period(
  start : maybe<instant>, // Nothing = beginning of time
  abbrv : string,         
  offset: duration
)

// Zero time period
val timezone-period0 = Timezone-period( Nothing, "UTC", duration0 )

// The time zone info for the UTC time zone.
val tzi-utc : timezone-info = Timezone-info( "UTC", 0, [] ) 

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
    val tzp = find-offset(i.use(ts-unix),tzi.periods)
    (tzp.offset,tzp.abbrv)
  }
  Timezone( 
    tzi.name, 
    utc-offset,
    fun(i) { Nothing }
  )
}

// Show a time zone period.
public fun show( tzp : timezone-period ) : string {
  tzp.abbrv.pad-right(4) + " " + tzp.offset.show-tzofs(utc="+00:00") + " from " + tzp.start.map(fun(i){ i.time.show }).default("-infinity")
}

// Show time zone information.
public fun show (tzi : timezone-info ) : string {
  tzi.name + ", population: " + tzi.population.show + ":\n  " + tzi.periods.map(show).join("\n  ")
}


/*----------------------------------------------------------------------------
  Parse timezone info from packed JSON format as used by momentjs.
  Latest files can be found at:\
  <https://github.com/moment/moment-timezone/blob/develop/data/packed/latest.json>
----------------------------------------------------------------------------*/

// Parse time zone information from a [packed json format](http://momentjs.com/timezone/docs/#/data-formats/packed-format).\
public fun parse-packed-zones( s : string ) : exn dict<timezone-info> {
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
  val zdict = zones.map(fun(tzi){ (tzi.name, tzi) }).dict

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
    match(zdict[key]) {
      Nothing  -> error("invalid timezone link: unknown time zone: " + key)
      Just(tz) -> (other,tz)
    }
  }).dict

  // return zones + links
  zdict + ldict
}
val rxzones = regex(@"""zones""\s*:\s*\[([\s\S]*?)^[ \t]*\]", multiLine=True)
val rxzone  = regex(@"^\s*""([^""]+)"",?\s*$")
val rxlinks = regex(@"""links""\s*:\s*\[([\s\S]*?)^[ \t]*\]", multiLine=True)
val rxlink  = regex(@"^\s*""([^""\|]+)\|([^""\|]+)"",?\s*$")


fun parse-packed-zone( s : string ) : exn timezone-info {
  //trace("parseing: " + s)
  val parts   = s.split("|")
  val name    = parts[0].default("")
  val abbrs   = parts[1].default("").split(" ").filter(notempty?)
  val offsets = parts[2].default("0").split(" ").filter(notempty?).map(parse-fixed-base60)
  val indices = parts[3].default("0").list.map(digit60)
  val xdiffs  = parts[4].default("").split(" ").filter(notempty?).map(parse-fixed-base60)
  fun from-indices( xs : list<a>, def : a ) : list<a> {
    indices.map( fun(idx) {xs[idx].default(def) }).reverse
  }
  val (start0,diffs) = match(xdiffs) {
    Nil -> (zero,[])
    Cons(start,rest) -> (fixed(start) * fixed(60), Cons(0.0,rest))
  }
  val population = parts[5].default("0").parse-base60
  if (diffs.length + 1 != indices.length ) error("invalid timezone data:\n  " + s )
  val endings = diffs.foldl((start0,[]), fun(acc:(fixed,list<maybe<instant>>),diff) {
                  val iu = acc.fst + fixed(diff * 60.0)
                  val i = instant-at(timestamp(iu),ts-unix)
                  (iu,Cons(Just(i),acc.snd))
                }).snd
  val ofsabrs  = indices.reverse.map( fun(i) { (offsets[i].default(0.0), abbrs[i].default("") ) } )
  val periods  = zipwith( endings + [Nothing], ofsabrs, fun(start,ofsabbr) {
                   Timezone-period(start,ofsabbr.snd, duration(ofsabbr.fst * ~60.0))
                 })
  Timezone-info(name,population,periods)
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

alias timezones = dict<timezone-info>

val global-timezones : () -> ref<global,timezones> = once{
  unsafe-total{
    val fname = appdir() + path("config/timezones.json")
    catch {
      ref(parse-packed-zones(read-text-file-sync(fname.string)))
    }
    fun(exn) {
      trace("timezone init: " + fname.string + ": " + exn.show)
      ref(dict())
    }
  }
}

fun get-timezones() : st<global> timezones {
  !(global-timezones())
}

// Return all available IANA time zone identifiers.
public fun timezones() : list<string> {
  unsafe-total(get-timezones).list.map(fst)
}


// Return a `:timezone-info` for a IANA time zone identifier (if it exists)
public fun timezone-info( tz-name : string  ) : maybe<timezone-info> {
  val tzs = unsafe-total(get-timezones)
  tzs[tz-name]
}

// Return a `:timezone` given a IANA time zone identifier, for example "US/Pacific" or "Asia/Singapore". 
// Return `default` (= `tz-utc`) if the time zone is not found. 
public fun timezone( tz-name : string, def : timezone = tz-utc ) : timezone {
  timezone-info(tz-name).map(timezone).default(def)
}


/*----------------------------------------------------------------------------
   Guess the local time zone
----------------------------------------------------------------------------*/

fun sample-dates( year : int ) : list<instant> {
  list(1,12).flatmap( fun(month) {
    [instant-at(year,month,1),instant-at(year,month,15)]
  })
}

fun sample-offsets( dates : list<instant>, tz : timezone ) : list<duration> {
  dates.map( fun(i) {
    i.time(tz=tz).tzofs
  })
}

fun match-offsets( dates : list<instant>, offsets : list<duration>, tz : timezone ) : bool {
  val offsets2 = dates.sample-offsets(tz)
  zipwith(offsets,offsets2,fun(x,y) { x == y }).all(id)
}

// Guess the local timezone. Not as reliable as `tz-local` but will contain
// more information on certain platforms (i.e. the timezone abbreviation)
public fun guess-local-timezone( default : timezone = tz-utc) : ndet timezone {
  val year    = now().time.year
  val dates   = sample-dates(year)
  val offsets = dates.sample-offsets(tz-local())
  val matches = unsafe-total(get-timezones).list.map(snd).filter( fun(tzi){ dates.match-offsets(offsets,tzi.timezone) } )
  match(matches) {
    Nil -> default
    Cons(tz1,rest) -> {
      var pick := tz1
      rest.foreach fun(tz) {
        if (tz.population==0 || (tz.population > pick.population && pick.population > 0)) {
          pick := tz
        }
      }
      pick.timezone
    }
  }

}


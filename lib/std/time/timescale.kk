/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Time scales


*/
module std/time/timescale

import std/fixed
import std/time/xinstant
import std/time/xleaps


// A `:time-scale` represents a duration relative to a certain epoch,
// and can convert to- and from TAI seconds.
public struct timescale(
  ts-name         : string,
  from-tai        : instant -> timestamp,
  to-tai          : timestamp -> instant,
  has-leap-seconds: bool = False,
  epoch-shift     : fixed = zero
)

// Create a new time scale based on SI seconds with a given `name` and `epoch-shift`
// duration away from the standard `epoch`.
public fun tai-timescale( name : string, offset : fixed = zero, epoch-shift : fixed = zero ) : timescale {
  val total-offset  = epoch-shift + offset
  fun from-tai(i:instant) { timestamp(i.since-epoch.seconds - total-offset) }
  fun to-tai(t:timestamp) { instant-at(duration(t.seconds + total-offset)) }
  Timescale(
    name,
    from-tai,
    to-tai,
    False,
    epoch-shift
  )
}

// Create a new time scale based on SI seconds with a given `name` and `epoch-shift`
// duration away from the standard `epoch`.
public fun utc-timescale( name : string, offset : fixed = zero, epoch-shift : fixed = zero ) : timescale {
  val total-offset  = epoch-shift + offset
  fun from-tai(i:instant) { tai-to-utc(i) - total-offset }
  fun to-tai(t:timestamp) { utc-to-tai(t + total-offset) }
  Timescale(
    name,
    from-tai,
    to-tai,
    False,
    epoch-shift
  )
}

// The TAI time scale based on SI seconds with 1900-01-01 epoch.
public val ts-tai : timescale = tai-timescale( "TAI" )
 
// The GPS time scale based on SI seconds with 1980-01-06 epoch.\
// GPS = TAI - 19.
public val ts-gps : timescale = tai-timescale( "GPS", fixed(19), fixed(2524953600) )

// The Terrestrial time scale based on SI seconds with 1900-01-01 epoch.\
// TT = TAI + 32.182
public val ts-tt : timescale = tai-timescale( "TT", fixed(32184,3), zero )

// The UTC time scale with a 1900-01-01 epoch. This uses 'UTC seconds' where
// one day consists of 86400 'solar' seconds. Leap seconds are encoded by using a
// fraction `>= 1.0`. 
public val ts-utc : timescale = utc-timescale( "" )

// Unix time scale based on UTC seconds but with 1970-01-01 epoch.
public val ts-unix : timescale = utc-timescale( "UNIX", zero, fixed(2208988800) )

// NTP time scale is equal to the UTC time scale (`ts-utc`) but named `"NTP"`.
public val ts-ntp : timescale = utc-timescale( "NTP" )

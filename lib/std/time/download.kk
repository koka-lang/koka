/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Automatic downloading of time information.

   Provides auto download functions to use the latest leap second tables,
   time zone information, and UT1 tables.
*/
module std/time/download

import std/async
import std/os/file
import std/os/path
import std/time/duration
import std/time/instant
import std/time/utc
import std/time/chrono
import std/time/calendar

// Construct a smoothed UTC-SLS based ISO calendar (see `ts-utc-sls-create`),
// using the latest IERS leap second information from file `fname`
// (= `"config/leap-seconds.list"` in the application directory).
// If the file is not present, it will be downloaded from `url`. The default `url` is:\
// <https://www.ietf.org/timezones/data/leap-seconds.list>\
// Set the `url` to the empty string to suppress automatic downloading.
// If the file is older than `download-delay` (=`4.weeks`) the file is automatically refreshed from `url`.
// The `download-timeout` (=`5.seconds`) gives the maximum time for downloading to finish.
// Set the optional parameter `verbose` to `True` to see trace messages on downloads or errors.
public fun cal-utc-sls-load( fname : path = path(""),
                         url   : string = "auto",
                         download-delay : duration   = 4.weeks,
                         download-timeout : duration = 5.seconds,
                         verbose : bool = False
                       ) : <io,async> calendar
{
  val leaps = load-leaps-table(fname,url,download-delay,download-timeout,verbose)
  cal-utc-sls-create(leaps)
}

// Construct a UTC based ISO calendar using the latest IERS leap second information from file `fname`
// (= `"config/leap-seconds.list"` in the application directory).
// If the file is not present, it will be downloaded from `url`. The default `url` is:\
// <https://www.ietf.org/timezones/data/leap-seconds.list>\
// Set the `url` to the empty string to suppress automatic downloading.
// If the file is older than `download-delay` (=`4.weeks`) the file is automatically refreshed from `url`.
// The `download-timeout` (=`5.seconds`) gives the maximum time for downloading to finish.
// Set the optional parameter `verbose` to `True` to see trace messages on downloads or errors.
public fun cal-utc-load( fname : path = path(""),
                         url   : string = "auto",
                         download-delay : duration   = 4.weeks,
                         download-timeout : duration = 5.seconds,
                         verbose : bool = False
                       ) : <io,async> calendar
{
  //val fname1 = fname
  val leaps = load-leaps-table(fname,url,download-delay,download-timeout,verbose)
  cal-utc-create(leaps)
}

// Construct a UTC time scale using the latest IERS leap second information from file `fname`
// (= `"config/leap-seconds.list"` in the application directory).
// If the file is not present, it will be downloaded from `url`. The default `url` is:\
// <https://www.ietf.org/timezones/data/leap-seconds.list>\
// Set the `url` to the empty string to suppress automatic downloading.
// If the file is older than `download-delay` (=`4.weeks`) the file is automatically refreshed from `url`.
// The `download-timeout` (=`5.seconds`) gives the maximum time for downloading to finish.
// Set the optional parameter `verbose` to `True` to see trace messages on downloads or errors.
public fun ts-utc-load( fname : path = path(""),
                         url   : string = "auto",
                         download-delay : duration   = 4.weeks,
                         download-timeout : duration = 5.seconds,
                         verbose : bool = False
                       ) : <io,async> timescale
{
  cal-utc-load(fname,url,download-delay,download-timeout,verbose).timescale
}

// Load the latest leap second information from file `fname`
// (= `"config/leap-seconds.list"` in the application directory).
// If the file is not present, it will be downloaded from `url`. The default `url` is:\
// <https://www.ietf.org/timezones/data/leap-seconds.list>\
// Set the `url` to the empty string to suppress automatic downloading.
// If the file is older than `download-delay` (=`4.weeks`) the file is automatically refreshed from `url`.
// The `download-timeout` (=`5.seconds`) gives the maximum time for downloading to finish.
// Set the optional parameter `verbose` to `True` to see trace messages on downloads or errors.
public fun load-leaps-table( fname : path = path(""),
                               url   : string = "auto",
                               download-delay : duration   = 4.weeks,
                               download-timeout : duration = 5.seconds,
                               verbose : bool = False
                             ) : <io,async> leaps-table
{
  val xfname = (fname || appdir() + path("config/leap-seconds.list"))
  val xurl   = (if (url=="auto") then "https://www.ietf.org/timezones/data/leap-seconds.list" else url)
  load-latest( xfname, xurl, parse-leaps-table, fun(lt:leaps-table){ Just(lt.expire) },
               Just(leaps-table-ti),
               download-timeout=download-timeout,download-delay=download-delay,
               error-prefix="load IETF leap seconds table",
               verbose=verbose
             )
}


// Load a file from a cached file `fname` or URL `url`. The file is parsed with
// the `parse` function. Given the result of `parse`, a future expiration date may
// be given by the file through the `expire` function. This is used for example in leap second
// files to prevent downloading too often. There may also be a `current` value that is returned
// if any error occurs. The `download-timeout` gives the download time out, while the
// `download-delay` gives the minimum amount of time that must have passed before a newer
// file is downloaded again. The `error-prefix` is prepended to any error messages (=`load cached`).
// Set the optional parameter `verbose` to `True` to see trace messages on downloads or errors.
//
// The function first checks if the `current` data is not yet expired and returns it directly if
// it is still valid. Otherwise, it attempts to read and parse the file from `fname`. If it is
// valid and newer than `current` and not yet expired, it is returned.
// Otherwise, if `url` is not empty, it calls `download-latest` to download and cache the file.
public fun load-latest( fname  : path,
                         url    : string,
                         parse  : string -> maybe<a>,
                         expire : a -> maybe<instant> = fun(_){ Nothing },
                         current: maybe<a> = Nothing,
                         download-timeout: duration = 5.seconds,
                         download-delay: duration = 2.weeks,
                         error-prefix: string = "load cached",
                         verbose : bool = False  ) : <ndet,file,net,exn,async> a
{
  fun full(msg) {
    error-prefix + ": " + msg + ".\n  cached: " + fname.show + "\n  url   : " + url.show
  }
  fun warn(msg) : ndet () {
    if (verbose) then trace("warning: " + msg.full) else ()
  }

  // is the current data still valid?
  val unow  = now()
  match(current) {
    Just(cur) | cur.expire.default(unow) > unow -> {
      warn("current data still valid")
      return cur // still valid; don't read from disk
    }
    _ -> ()
  }
  // try to read a cached local file first
  val mbrecent = match(try{read-text(fname)}) {
    // can't read the file?
    Exn(exn) -> {
      warn(exn.show)
      current // try to download more recent one
    }
    // we read the cached file, let's parse it
    Ok(text) -> match(text.parse) {
      // unable to parse
      Nothing -> {
        warn("invalid data")
        current
      }
      // parsed
      Just(data) -> {
        // check expiration date against current data and now()
        val exp = data.expire.default(unow)
        val recent = match(current) {
                      Just(cur) | cur.expire.default(exp) > exp -> {
                        warn("builtin data is more recent than cached data")
                        cur
                      }
                      _ -> {
                        if (exp > unow)
                         then return data  // return with valid cached data :-)
                         else data
                      }
                    }
        // check file mod time to see if we tried downloading recently
        val fi = try-default(epoch){ fname.mod-time }
        if (download-delay.pos? && (fi + download-delay > unow)) {
          warn("using cached file since it was recently updated.\n  " + fname.show)
          return recent  // expired, but use anyway since we recently updated the file
          // Just(recent)
        }
        else {
          warn("local cache expired: " + fi.show)
          Just(recent)  // expired, try to download more recent version
        }
      }
    }
  }
  // only download if we have a url
  if (url.empty?) then {
    match(mbrecent) {
      Nothing      -> error(full("unable to read from cache"))
      Just(recent) -> recent
    }
  }
  else {
    // try to touch the file so we won't retry the download too often
    try{ fname.utimes(unow,unow) }
    // go download (and prefer a possibly expired file if the download fails)
    download-latest(url,parse,expire,fname,mbrecent,download-timeout,error-prefix,verbose)
  }
}

// Download a file from `url` and cache it locally in file `cache` (=`path("")`) if
// that path is not empty. The `parse` function parses the downloaded content. The
// optional `expire` (=`{Nothing}`) function can return an optional expiration date from the content
// which is used to check if the downloaded content is newer than the optional current
// data `current` (=`Nothing`). The `download-timeout` gives the download timeout.
// The `error-prefix` is prepended to any error messages (=`download failed`).
// Set the optional parameter `verbose` to `True` to see trace messages on downloads or errors.
public fun download-latest(  url   : string,
                             parse : string -> maybe<a>,
                             expire : a -> maybe<instant> = fun(_){ Nothing },
                             cache  : path = path(""),
                             current: maybe<a> = Nothing,
                             download-timeout: duration = 5.seconds,
                             error-prefix: string = "download failed",
                             verbose : bool = False
                           ) : <ndet,file,net,async,exn> a
{
  fun warn(msg) {
    val full-msg = error-prefix + ": " + msg + ".\n  url: " + url.show
    // trace(full-msg)
    match(current) {
      Just(cur) -> { if (verbose) then trace(full-msg); cur }
      Nothing   -> { error(full-msg) }
    }
  }
  try {
    if (verbose) then trace("start download: " + url.show)
    val text = url.download-text(download-timeout)
    // trace("downloaded: " + url + ": " + text.first(10).string + "...")
    match(text.parse) {
      Just(data) -> {
        val exp = data.expire.default(now())
        match(current) {
          Just(data-cur) | data-cur.expire.default(exp) > exp -> {
            warn("downloaded data is less recent than the cached data.")
          }
          _ -> {
            if (!cache.empty?) then {
              if (verbose) then trace("cache to file: " + cache.string)
              try{ write-text(cache,text) }
              ()
            }
            data
          }
        }
      }
      _ -> warn("invalid data")
    }
    //}
  } fun(exn) { warn(exn.show) }
}

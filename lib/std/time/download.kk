/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Automatic downloading of time information.

   Provides auto download functions to download leap second tables (`update-leaps-table`),
   time zone information, and UT1 tables.
*/
module std/time/download

import std/file
import std/path
import std/time/duration
import std/time/instant
import std/time/utc


// Update the internal leap second table precise UTC time calculation. If the internal table is not
// yet expired (after 2017-01-01) nothing is done. 
// Otherwise, it updates the internal table with the information in the file `fname` 
// (= `"config/leap-seconds.list"` in the application directory).
// If the file is not present, it will be downloaded from `url`. The default `url` is:\
// <https://www.ietf.org/timezones/data/leap-seconds.list>\
// Set the `url` to the empty string to suppress automatic downloading. 
// If the file is older than `download-delay` (=`4.weeks`) the file is automatically refreshed from `url`.
// The `download-timeout` (=`5.seconds`) gives the maximum time for downloading to finish.
public fun update-leaps-table( fname : path = path(""), 
                               url   : string = "auto",
                               download-delay : duration   = 4.weeks,
                               download-timeout : duration = 5.seconds
                             ) : <io,async> () 
{
  val xfname = (fname || appdir() + path("config/leap-seconds.list"))
  val xurl   = (if (url=="auto") then "https://www.ietf.org/timezones/data/leap-seconds.list" else url)
  val leaps  = load-latest( xfname, xurl, parse-leaps-table, fun(lt:leaps-table){ Just(lt.expire) }, 
                            Just(get-leaps-table()),
                            download-timeout=download-timeout,download-delay=download-delay,
                            error-prefix="load IETF leap seconds table" 
                          )
  set-leaps-table(leaps)
}


// Load a file from a cached file `fname` or URL `url`. The file is parsed with
// the `parse` function. Given the result of `parse`, a future expiration date may
// be given by the file through the `expire` function. This is used for example in leap second
// files to prevent downloading too often. There may also be a `current` value that is returned
// if any error occurs. The `download-timeout` gives the download time out, while the 
// `download-delay` gives the minimum amount of time that must have passed before a newer
// file is downloaded again. The `error-prefix` is prepended to any error messages (=`load cached`).
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
                         error-prefix: string = "load cached"  ) : <ndet,file,net,exn,async> a 
{
  fun full(msg) {
    error-prefix + ": " + msg + ".\n  cached: " + fname.show + "\n  url   : " + url.show
  }
  fun warn(msg) {    
    trace(msg.full)
  }
  // is the current data still valid? 
  val unow  = now()
  match(current) {
    Just(cur) | cur.expire.default(unow) > unow -> return cur // still valid; don't read from disk
    _ -> ()
  }
  // trace("read: " + fname.string)
  // try to read a cached local file first
  val mbrecent = match(try{read-text(fname)}) {
    // can't read the file?
    Left(exn) -> {
      warn(exn.show) 
      current // try to download more recent one
    }
    // we read the cached file, let's parse it
    Right(text) -> match(text.parse) {
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
                        if (data.expire.default(unow) > unow) 
                         then return data  // return with valid cached data :-)
                         else data
                      }
                    }
        // check file mod time to see if we tried downloading recently
        val fi = on-exn(epoch){ fname.mod-time }
        if (download-delay.pos? && (on-exn(epoch){ fname.mod-time } + download-delay > unow)) {
          warn("using cached file since it was recently updated.\n  " + fname.show)
          return recent  // expired, but use anyway since we recently updated the file
        }
        else Just(recent)  // expired, try to download more recent version
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
    download-latest(url,parse,expire,fname,mbrecent,download-timeout,error-prefix)
  }
}

// Download a file from `url` and cache it locally in file `cache` (=`path("")`) if
// that path is not empty. The `parse` function parses the downloaded content. The
// optional `expire` (=`{Nothing}`) function can return an optional expiration date from the content
// which is used to check if the downloaded content is newer than the optional current
// data `current` (=`Nothing`). The `download-timeout` gives the download timeout.
// The `error-prefix` is prepended to any error messages (=`download failed`).
public fun download-latest(  url   : string,
                             parse : string -> maybe<a>, 
                             expire : a -> maybe<instant> = fun(_){ Nothing },
                             cache  : path = path(""),
                             current: maybe<a> = Nothing,
                             download-timeout: duration = 5.seconds,
                             error-prefix: string = "download failed"
                           ) : <ndet,file,net,async,exn> a 
{
  fun warn(msg) {
    val full-msg = error-prefix + ": " + msg + ".\n  url: " + url.show
    match(current) {
      Just(cur) -> { trace(full-msg); cur }
      Nothing   -> { error(full-msg) }
    }
  }
  catch {
    trace("start download: " + url.show)
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
              trace("cache to file: " + cache.string)
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


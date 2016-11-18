// --------------------------------------------------------
//
// --------------------------------------------------------
module file1

public import std/file
public import std/path
public import std/time/timespan
public import std/xtime
public import std/time/ut1

public fun test-readme() {
  val f = path("readme.md")
  f.mod-time.local-time.show.println
  // f.read-text.println
  val l = "https://www.ietf.org/timezones/data/leap-seconds.list"
  download-text(l,duration(1)).println
}

public fun main() {
  match(update-leaps()) {
    Nothing -> "no success"
    Just    -> "ok"
  }
}

public fun update-ut1() : <io,async> maybe<_> {
  load-latest( 
    appdir() + path("config/finals2000A.all"),
    "http://toshi.nofs.navy.mil/ser7/finals2000A.all",
    parse-finals2000A-ex,
    download-delay   = duration(10*86400)
  )
}

public fun update-leaps( ) : <io,async> maybe<leaps-table> {
  load-latest( 
    appdir() + path("config/leap-seconds.list"),
    "https://www.ietf.org/timezones/data/leap-seconds.list",
    parse-leap-seconds,
    download-delay = duration0
  )
}


fun load-latest( fname  : path, 
                 url    : string, 
                 parse  : string -> maybe<(instant,a)>,
                 after  : instant = epoch,
                 download-timeout: duration = duration(5),
                 download-delay: duration = duration(7*86400) ) : <ndet,file,net,async> maybe<a> 
{
  val unow  = now()
  if (unow < after) then return Nothing
  // trace("read: " + fname.string)
  // try to read a cached local file first
  val mbdata = match(try{read-text(fname)}) {
    Right(text) -> match(text.parse) {
      Nothing -> warn("load-latest: invalid data.\n  " + fname.show)
      Just((expire,data)) -> {
        if (expire >= unow) then return Just(data)  // exit with result :-)
        // check file mod time to see if we tried downloading recently
        val fi = on-exn(epoch){ fname.mod-time }
        if (download-delay.pos? && (on-exn(epoch){ fname.mod-time } + download-delay > unow)) {
          warn("load-latest: using cached file since it was recently updated.\n  " + fname.show)
          return Just(data)
        }
        Just(data)  // expired, try download
      }
    }
    Left(exn) -> warn("load-latest: " + exn.show + ".\n  " + fname.show)
  }
  // only download if we have a url
  if (url.empty?) return mbdata
  // try to touch the file so we won't retry the download too often
  try{ fname.utimes(unow,unow) }
  // go download (and prefer a possibly expired file if the download fails)
  download-latest(url,parse,after,fname,download-timeout,download-delay) + mbdata
}

fun download-latest( url   : string,
                     parse : string -> maybe<(instant,a)>, 
                     after : instant = epoch,
                     cache : path = path(""),
                     download-timeout: duration = duration(5),
                     download-delay  : duration = duration(10*86400) // just for warning on expired data
                   ) : <ndet,file,net,async> maybe<a> 
{
  catch {
    trace("start download: " + url.show)
    val text = url.download-text(download-timeout)
    // trace("downloaded: " + url + ": " + text.first(10).string + "...")
    match(text.parse) {
      Just((expire,data)) -> {  
        val unow = now()
        //trace("download parsed: expire: " + expire.show + ", unow: " + unow.show)              
        if (expire + download-delay < unow) then {
          warn("download-latest: downloaded data has already expired.\n  " + url.show) 
        }
        else {
          if (!cache.empty?) then {
            trace("cache to file: " + cache.string)
            try{ write-text(cache,text) }
            ()
          }
          Just(data)
        }
      }
      _ -> warn("download-latest: invalid data.\n  " + url.show) 
    }
    //}
  } fun(exn) { warn("download-latest: " + exn.show) }
}

fun warn( s : string ) : ndet maybe<a> {
  trace("warning: " + s)
  Nothing
}

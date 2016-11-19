// --------------------------------------------------------
//
// --------------------------------------------------------
module file1

public import std/file
public import std/path
public import std/time/timespan
public import std/xtime
public import std/time/ut1
public import std/time/timezone

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



public fun update-packed-zones() : <io,async> maybe<_> {
  fun parse(s :string) : maybe<timezones> {
    try{ parse-packed-zones(s) }.maybe
  }
  load-latest( 
    appdir() + path("config/timezones.json"),
    "https://raw.githubusercontent.com/moment/moment-timezone/develop/data/packed/latest.json",
    parse,
    download-delay = 1.days
  )
}

public fun update-ut1() : <io,async> maybe<_> {
  load-latest( 
    appdir() + path("config/finals2000A.all"),
    "http://toshi.nofs.navy.mil/ser7/finals2000A.all",
    parse-finals2000A-ex,    
    fun(dtt:dut-table){ Just(dtt.expire) },
    download-timeout = 10.seconds   // 3mb file! 
  )
}

public fun update-leaps( ) : <io,async> maybe<leaps-table> {
  load-latest( 
    appdir() + path("config/leap-seconds.list"),
    "https://www.ietf.org/timezones/data/leap-seconds.list",
    parse-leap-seconds,
    fun(lt:leaps-table){ Just(lt.expire) },
    download-delay = 4.weeks
  )
}


fun load-latest( fname  : path, 
                 url    : string, 
                 parse  : string -> maybe<a>,
                 expire : a -> maybe<instant> = fun(_){ Nothing },
                 current: maybe<a> = Nothing,
                 download-timeout: duration = 5.seconds,
                 download-delay: duration = 2.weeks ) : <ndet,file,net,async> maybe<a> 
{
  val unow  = now()
  match(current) {
    Just(cur) | cur.expire.default(unow) > unow -> return current // still valid
    _ -> ()
  }
  // trace("read: " + fname.string)
  // try to read a cached local file first
  val mbdata = match(try{read-text(fname)}) {
    Right(text) -> match(text.parse) {
      Nothing -> {
        warn("load-latest: invalid data.\n  " + fname.show)
        current
      }
      Just(data) -> {
        val exp = data.expire.default(unow)
        val recent = match(current) {
                      Just(cur) | cur.expire.default(exp) > exp -> {
                        warn("load-latest: builtin data is more recent than cached data")
                        cur
                      }
                      _ -> {
                        if (data.expire.default(unow) > unow) then {
                          return Just(data)  // return with valid cached data :-)
                        }
                        else data
                      }
                    }
        // check file mod time to see if we tried downloading recently
        val fi = on-exn(epoch){ fname.mod-time }
        if (download-delay.pos? && (on-exn(epoch){ fname.mod-time } + download-delay > unow)) {
          warn("load-latest: using cached file since it was recently updated.\n  " + fname.show)
          return Just(recent)
        }
        else Just(recent)  // expired, try download
      }
    }
    Left(exn) -> {
      warn("load-latest: " + exn.show + ".\n  " + fname.show)
      current
    }
  }
  // only download if we have a url
  if (url.empty?) return mbdata
  // try to touch the file so we won't retry the download too often
  try{ fname.utimes(unow,unow) }
  // go download (and prefer a possibly expired file if the download fails)
  download-latest(url,parse,expire,fname,mbdata,download-timeout)
}

fun download-latest( url   : string,
                     parse : string -> maybe<a>, 
                     expire : a -> maybe<instant> = fun(_){ Nothing },
                     cache  : path = path(""),
                     current: maybe<a> = Nothing,
                     download-timeout: duration = 5.seconds
                   ) : <ndet,file,net,async> maybe<a> 
{
  catch {
    trace("start download: " + url.show)
    val text = url.download-text(download-timeout)
    // trace("downloaded: " + url + ": " + text.first(10).string + "...")
    match(text.parse) {
      Just(data) -> {  
        val exp = data.expire.default(now())
        match(current) {
          Just(data-cur) | data-cur.expire.default(exp) > exp -> {
            warn("download-latest: downloaded data is less recent than the cached data.\n  " + url.show) 
            current
          }
          _ -> {
            if (!cache.empty?) then {
              trace("cache to file: " + cache.string)
              try{ write-text(cache,text) }
              ()
            }
            Just(data)
          }
        }
      }
      _ -> {
        warn("download-latest: invalid data.\n  " + url.show) 
        current
      }
    }
    //}
  } fun(exn) { 
    warn("download-latest: " + exn.show)
    current
  }
}

fun warn( s : string ) : ndet maybe<a> {
  trace("warning: " + s)
  Nothing
}

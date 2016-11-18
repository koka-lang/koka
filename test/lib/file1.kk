// --------------------------------------------------------
//
// --------------------------------------------------------
module file1

public import std/file
public import std/path
public import std/time/timespan
public import std/xtime

public fun test-readme() {
  val f = path("readme.md")
  f.mod-time.local-time.show.println
  // f.read-text.println
  val l = "https://www.ietf.org/timezones/data/leap-seconds.list"
  download-text(l,1.0).println
}

public fun main() {
  match(update-leaps()) {
    Nothing -> "no success"
    Just    -> "ok"
  }
}

public fun update-leaps( ) : <io,async> maybe<leaps-table> {
  update-data( 
    parse-leap-seconds,
    epoch,
    appdir() + path("config/leap-seconds.list"),
    Just("https://www.ietf.org/timezones/data/leap-seconds.list")
  )
}

fun update-data( parse  : string -> maybe<(instant,a)>, 
                 builtin: instant,
                 fname  : path, 
                 url    : maybe<string>, 
                 download-timeout: double = 5.0, 
                 download-delay: double = 7.0) : <ndet,file,net,async,console> maybe<a> 
{
  val unow  = now()
  if (unow < builtin) then return Nothing
    trace("read: " + fname.string)
    match(try-read-text(fname).map(parse)) {
      Right(Just((expire,info))) | expire > unow -> Just(info)  // found good data
      _ -> match(url) {
        Just(urlname) -> {
          catch {
            trace("try download? " + urlname)
            //if (fname.mod-time.timestamp + (download-delay*86400.0).timespan <= unow) then Nothing else {
            trace("touch file: " + fname.string)
            //val mtime = unow.double 
            //fname.utimes(mtime,mtime) // touch so we don't download right away again
            //trace("and download from: " + urlname)
            val text = urlname.download-text(download-timeout)              
            println("downloaded: " + urlname + ": " + text.first(10).string + "...")
            match(text.parse) {
              Just((expire,info)) -> {  
                println("download parsed: expire: " + expire.show + ", unow: " + unow.show)              
                if (expire > unow) then {
                  try-write-text(fname,text)
                  Just(info)
                }
                else Nothing
              }
              _ -> {
                println("downloaded: but no parse")              
                Nothing
              }
            }
            //}
          } fun(exn) {
            trace("error: " + exn.show)
            Nothing
          }
        }
        _ -> Nothing
      }
    }  
}
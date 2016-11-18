// --------------------------------------------------------
//
// --------------------------------------------------------
module file1

public import std/file
public import std/path
public import std/time/timespan
public import std/xtime

public fun main() {
  val f = path("readme.md")
  f.mod-time.unix-instant.local-time.show.println
  // f.read-text.println
  val l = "https://www.ietf.org/timezones/data/leap-seconds.list"
  download-text(l,1.0).println
}

public fun update-leaps( ) : <io,async> maybe<leaps-table> {
  fun parse(text) {
    trace("leap table:\n" + text)
    parse-leap-seconds(text)
  }
  update-data( 
    parse,
    timestamp0,
    appdir() + path("config/leap-seconds.list"),
    Just("https://www.ietf.org/timezones/data/leap-seconds.list")
  )
}

fun update-data( parse  : string -> maybe<(timestamp,a)>, 
                 builtin: timestamp,
                 fname  : path, 
                 url    : maybe<string>, 
                 download-timeout: double = 5.0, 
                 download-delay: double = 7.0) : <ndet,file,net,async> maybe<a> 
{
  val (s,f) = unix-now()
  val unow  = timestamp(s+f)
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
            trace("downloaded: " + urlname + ": " + text)              
            match(text.parse) {
              Just((expire,info)) | expire > unow -> {  
                trace("downloaded: " + text)              
                try-write-text(fname,text)
                Just(info)
              }
              _ -> {
                trace("downloaded: " + text)              
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
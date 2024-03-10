import std/os/path
import std/os/dir
import std/os/file
import std/os/process
import std/os/env
import std/os/flags

struct iflags
  asian-wide: bool = False

val header = "usage:\n stack exec koka -- -e util/update-unicode [-- [options]]\n\noptions:"

val flag-descs = 
  fun set-asian-wide( f : iflags, b : bool ) : iflags { f(asian-wide = b) }
  [ Flag( "a", ["asian-wide"], Bool(set-asian-wide), "print updated asian wide information" )]

pub fun process-flags() : <ndet,console> maybe<iflags>
  val (flags,args,errs) = parse( Iflags(), flag-descs, get-args() )
  if errs.is-nil && args.is-nil then Just(flags) else
    println( errs.join("\n") ++ "\n" ++ flag-descs.usage(header) )
    Nothing

fun main()
  val flags = process-flags().unjust
  if flags.asian-wide then 
    val file = run-system(r#"curl "https://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt" > EastAsianWidth.txt"#)
    if "EastAsianWidth.txt".path.is-file then 
      val lines = "EastAsianWidth.txt".path.read-text-file().split("\n")
      with line <- lines.foreach()
      val values = line.split(";")
      if length(values) >= 2 then
        val width = values[1].unjust.split("#")[0].unjust.trim-right().trim-left()
        if width == "W" then
          val charrange = values[0].unjust.trim-right().split("..")
          if length(charrange) == 2 then
            println("    single(0x" ++ charrange[0].unjust ++ ",0x"  ++ charrange[1].unjust ++ "),")
          elif length(charrange) == 1 then
            println("    single(0x" ++ charrange[0].unjust ++ ",0x"  ++ charrange[0].unjust ++ "),")
          else
            throw("Error unsupported range " ++ charrange.show)
      // else
      //   println("Unrecognized format " ++ line)
    else
      println("Could not find EastAsianWidth.txt\n\tPlease download from https://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt to your current directory") 
            
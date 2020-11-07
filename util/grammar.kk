import std/os/path
import std/os/file

fun main() {
  build()
}

fun build() {
  // Install paths
  val outdir = "out/grammar".path
  val gdir   = "doc/spec/grammar".path

  println("copying...")
  copy-directory(gdir,outdir)
  println("compile grammar...")
  run-echo("cd " + outdir.string + " && bison -vd -W -Wno-empty-rule -Wcounterexamples parser.y")
  println("compile lexer...")
  run-echo("cd " + outdir.string + " && flex -8 lexer.lex")
  println("compile generated code...")
  run-echo("cd " + outdir.string + " && stack exec ghc -- -no-hs-main -o koka-parser lex.yy.c parser.tab.c")
  val parser = (outdir + "koka-parser".path).string
  println("created: " + parser)
}

fun run-echo(cmd : string) : io () {
  // println("> " + cmd)
  val exit-code = run-system(cmd)
  println( if (exit-code==0) then "ok" else "error: " + exit-code.show )
}

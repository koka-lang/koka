import std/num/double
import std/os/file
import std/os/path
import std/os/env
import std/os/flags

// ----------------------------------------------------
// Flags
// ----------------------------------------------------

val all-test-names = ["rbtree","rbtree-ck","deriv","nqueens","cfold"]
val all-lang-names = [
  ("koka","kk"),("ocaml","ml"),
  ("haskell","hs"),("swift","sw"),
  ("java","jv"),("cpp","cpp")
]

struct iflags {
  tests : string = ""
  langs : string = ""
  chart : bool = False
}

val flag-descs : list<flag<iflags>> = {
  fun set-tests( f : iflags, s : string ) : iflags { f(tests = s) }
  fun set-langs( f : iflags, s : string ) : iflags { f(langs = s) }
  fun set-chart( f : iflags, b : bool ) : iflags { f(chart = b) }
  [ Flag( "t", ["test"], Req(set-tests,"test"), "comma separated list of tests" ),
    Flag( "l", ["lang"], Req(set-langs,"lang"),  "comma separated list of languages"),
    Flag( "c", ["chart"], Bool(set-chart),        "generate latex chart")
  ]
}

fun flag-usage() {
  flag-descs.usage("usage:\n koka run -- [options]\n\noptions:").println
  println([
    "\nnotes:",
    "  tests    : " + all-test-names.join(", "),
    "  languages: " + all-lang-names.map(snd).join(", ")
  ].unlines)
}

public fun process-flags() : <ndet,console> maybe<iflags> {
  val (flags,args,errs) = parse( Iflags(), flag-descs, get-args() )
  if (errs.is-nil && args.is-nil) {
    Just(flags)
  }
  else {
    println( errs.join("\n") )
    flag-usage()
    Nothing
  }
}


// ----------------------------------------------------
// Test structure
// ----------------------------------------------------

struct test {
  name: string
  lang: string
  elapsed: double = 0.0
  rss: int = 0
  err: string = ""
  norm-elapsed: double = 0.0
  norm-rss: double = 0.0
}

fun show( test : test ) {
  val xs = if (test.err.is-empty) then [test.elapsed.core/show(2) + "s",test.rss.core/show + "kb"] else ["error: " + test.err]
  ([test.name,test.lang.pad-left(3)] + xs).join(", ")
}

fun show-norm( test : test ) {
  val xs = if (test.err.is-empty) then [test.norm-elapsed.core/show(2) + "x", test.norm-rss.core/show(2) + "x"] else ["error: " + test.err]
  ([test.name,test.lang.pad-left(3)] + xs).join(", ")
}

// ----------------------------------------------------
// main
// ----------------------------------------------------
public fun main() {
  match (process-flags()) {
    Nothing -> ()
    Just(flags) {
      val test-names = if (flags.tests.is-empty) then all-test-names
                        else flags.tests.split(",")
      val lang-names = if (flags.langs.is-empty) then all-lang-names
                        else all-lang-names.filter(fn(l){ flags.langs.contains(l.snd) || flags.langs.contains(l.fst) })
      run-tests(test-names,lang-names,flags.chart)
    }
  }
}

fun run-tests(test-names : list<string>, lang-names : list<(string,string)>, gen-chart : bool ) {
  println("tests    : " + test-names.join(", "))
  println("languages: " + lang-names.map(fst).join(", "))

  // run tests
  val alltests = test-names.flatmap fn(test-name){
                   lang-names.map fn(lang){
                     run-test( test-name, lang )
                   }
                 }

  // todo: average over multiple runs

  // show test results
  test-names.foreach fn(test-name){
    val tests = alltests.filter(fn(t){ t.name == test-name })
    println("\n--- " + test-name + " ----------------")
    println(tests.map(show).join("\n"))
  }

  // exit if koka is not part of the tests (since we need it to normalize)
  if (!lang-names.map(fst).join(",").contains("koka")) return ()

  // normalize tests
  val all-ntests = test-names.flatmap fn(test-name){
    val tests = alltests.filter(fn(t){ t.name == test-name })

    // normalize to koka
    val koka = match(tests.filter(fn(t){t.lang == "kk"})) { Cons(t,Nil) -> t }
    val ntests = tests.map fn(t) {
      t(norm-elapsed = if (koka.elapsed==0.0) then 1.0 else t.elapsed / koka.elapsed,
        norm-rss     = if (koka.rss==0)       then 1.0 else t.rss.double / koka.rss.double)
    }
    println("\n--- normalized " + test-name + " ----------------")
    println(ntests.map(show-norm).join("\n"))
    ntests
  }

  // group by language
  val lang-ntests = lang-names.map(fn(l) {
    val lang-name = l.snd
    (lang-name, all-ntests.filter(fn(t:test){ t.lang == lang-name }))
  })

  // emit latex chart
  if (gen-chart) {
    val ymax       = 2.0
    val chart-desc = @"AMD 3600XT 6-core 3.5Ghz\\Ubuntu 20.04, Gcc 9.3.0"
    val chart-elapsed = chart("time", norm-elapsed, test-names, lang-ntests, ymax, chart-desc)
    val chart-rss     = chart("rss", norm-rss, test-names, lang-ntests, ymax, chart-desc)
    println("\n--- chart elapsed ---------------")
    println(chart-elapsed)
    println("\n--- chart rss ---------------")
    println(chart-rss)
  }
  ()
}


// ----------------------------------------------------
// Latex chart
// ----------------------------------------------------
fun chart( kind : string, norm : test -> double, test-names : list<string>, lang-ntests : list<(string,list<test>)>, ymax : double = 2.0, desc : string = "" ) : string {
  [ tikz-header(test-names,".bench" + kind)
  , lang-ntests.flatmap(fn(l){ tikz-data(kind, norm, l, ymax = ymax ) })
  , tikz-picture(kind, test-names, lang-ntests.map(fst), ymax = ymax, desc = desc )
  , tikz-footer(test-names) ].concat.join("\n")
}


fun tikz-header( test-names : list<string>, attr : string ) : list<string> {
  ["~ Begin Snippet { .benchmark " + attr + " }",
   "\\pgfplotsset{",
   "  xticklabels = {" + test-names.map(fn(n){ "\\strut " + n.replace-all("_","\\_")}).join(",") + "}",
   "}"]
}

fun tikz-footer( test-names : list<string> ) : list<string> {
  [ "~ End Snippet" ]
}

fun tikz-picture( kind : string, test-names : list<string>, lang-names : list<string>, ymax : double = 2.0, desc : string = "", height:string = "6cm", width:string = "9cm" ) {
  val n = test-names.length - 1
  val header = [
    @"\begin{tikzpicture}\sffamily",
    @"\begin{axis}[Chart" + kind + ",ymax=" + ymax.show(1) + ",height=" + height + ",xmax=" + n.show + ".5,width=" + width + "]",
    @"  \draw (axis cs:-0.5,1) -- (axis cs:" + n.show + ".5,1);"
  ]
  val mid = lang-names.map(fn(l){ @"  \draw" + kind + @"{\color" + kind + l + @"{0}}{\data" + kind + l + "};"})
  val footer = [
     @"  \legend{" + lang-names.map(fn(l){ "\\lang" + l }).join(",") + "};",
     if (desc.is-empty) then "" else @"  \chartdesc{" + desc.replace-all("\n",@"\\") + "};",
     @"\end{axis}",
     @"\end{tikzpicture}"
  ]
  (header + mid + footer)
}


fun tikz-data( kind:string, norm : test -> double, lang-ntests : (string,list<test>), ymax : double = 2.0 ) : list<string> {
  val (lang,ntests) = lang-ntests
  ["",
   @"\pgfplotstableread{"] +
    ntests.map-indexed(fn(i,t){
      if (t.err.is-empty) then {
        [i.show,
         if (t.norm <= ymax) then t.norm.show(3) else ymax.show(3),
         "0.000",
         t.norm.show(2) // else @"{\outerlabel{" + t.norm-elapsed.show(2) + "}}"
        ].join(" ")
      }
      else (i.show + @" 0.100 0.000 " + (if (i==0) then "0" elif (t.err=="NA") then "{NA}" else @"{\ensuremath{\times}}"))
    }) +
    [@"}\data" + kind + lang]
}


// ----------------------------------------------------
// Run a single test
// ----------------------------------------------------

fun run-test( test-name : string, langt : (string,string) ) : io test {
  val (lang-long,lang) = langt
  val pre  = lang.pad-left(3) + ", " + test-name.pad-left(12) + ", "
  val dir  = if (lang-long=="koka") then "koka/out/RelWithDebInfo" else lang-long
  val base = lang + "-" + test-name
  val prog = if (lang-long=="java")
              then "java --class-path=" + dir + " " + test-name.replace-all("-","")
              else dir + "/" + base
  val progpath = if (lang-long=="java") then (dir.path + (test-name.replace-all("-","") + ".class").path)
                  else prog.path
  println("\nrun: " + prog)

  if (!is-file(progpath)) {
    return Test(test-name,lang,err="NA")
  }

  val timef= "time-" + base + ".txt"
  val cmd  = "/usr/bin/time -f'%e,%M' -o" + timef + " " + prog
  val out  = run-command(cmd).exn
  print(out)
  val time = read-text-file(timef.path).trim
  match(time.split("\n")) {
    Nil            -> Test(test-name,lang,err="no output")
    Cons(err,Cons) -> Test(test-name,lang,err=err)
    Cons(tim,Nil) {
      match(tim.split(",")) {
        Cons(elapsed,Cons(rss,Nil)) {
          println("elapsed: " + elapsed + "s, rss: " + rss + "kb" )
          Test(test-name,lang,parse-double(elapsed).default(0.0),parse-int(rss).default(0))
        }
        _ -> Test(test-name,lang,err="bad format")
      }
    }
  }
}

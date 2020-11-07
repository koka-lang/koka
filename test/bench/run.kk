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
  ("koka","kk"),
  ("kokax","kkx"),
  ("ocaml","ml"),
  ("haskell","hs"),
  ("swift","sw"),
  ("java","jv"),
  ("cpp","cpp")
]

struct iflags {
  tests : string = ""
  langs : string = ""
  chart : bool = False
  iter  : int  = 1
}

val flag-descs : list<flag<iflags>> = {
  fun set-tests( f : iflags, s : string ) : iflags { f(tests = s) }
  fun set-langs( f : iflags, s : string ) : iflags { f(langs = s) }
  fun set-chart( f : iflags, b : bool ) : iflags { f(chart = b) }
  fun set-iter( f : iflags, i : string ) : iflags { f(iter = i.parse-int().default(1)) }
  [ Flag( "t", ["test"], Req(set-tests,"test"), "comma separated list of tests" ),
    Flag( "l", ["lang"], Req(set-langs,"lang"),  "comma separated list of languages"),
    Flag( "c", ["chart"], Bool(set-chart),       "generate latex chart"),
    Flag( "i", ["iter"], Req(set-iter,"N"),      "use N iterations per test"),
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
  elapsed-sdev : double = 0.0
  rss: int = 0
  err: string = ""
  norm-elapsed: double = 0.0
  norm-rss: double = 0.0
  norm-elapsed-sdev : double = 0.0
}

fun show( test : test ) {
  val xs = if (test.err.is-empty) then [
    test.elapsed.core/show(2) + "s ~" + test.elapsed-sdev.core/show-fixed(3),
    test.rss.core/show + "kb"
  ] else ["error: " + test.err]
  ([test.name,test.lang.pad-left(3)] + xs).join(", ")
}

fun show-norm( test : test ) {
  val xs = if (test.err.is-empty) then [
      test.norm-elapsed.core/show(2) + "x ~" + test.elapsed-sdev.core/show-fixed(3),
      test.norm-rss.core/show(2) + "x"
    ] else ["error: " + test.err]
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
      run-tests(test-names,lang-names,flags.chart,flags.iter)
    }
  }
}

fun run-tests(test-names : list<string>, lang-names : list<(string,string)>, gen-chart : bool, iterations : int ) {
  println("tests    : " + test-names.join(", "))
  println("languages: " + lang-names.map(fst).join(", "))

  // run tests
  val alltests = test-names.flatmap fn(test-name){
                   lang-names.map fn(lang){
                     run-test( test-name, lang, iterations )
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
      val norm = if (koka.elapsed==0.0) then 1.0 else t.elapsed / koka.elapsed
      t(norm-elapsed = norm,
        norm-rss     = if (koka.rss==0) then 1.0 else t.rss.double / koka.rss.double,
        norm-elapsed-sdev = norm * t.elapsed-sdev)
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
    val chart-desc = @"6-core AMD 3600XT at 3.8Ghz\\Ubuntu 20.04, Gcc 9.3.0"
    val chart-elapsed = chart("time", norm-elapsed, norm-elapsed-sdev, test-names, lang-ntests, ymax, chart-desc)
    val chart-rss     = chart("rss", norm-rss, fn(t){ 0.0 }, test-names, lang-ntests, ymax, chart-desc)
    println("\n<!-- chart elapsed ------------- -->")
    println(chart-elapsed)
    println("\n<!-- chart rss --------------- -->")
    println(chart-rss)
  }
  ()
}


// ----------------------------------------------------
// Latex chart
// ----------------------------------------------------
fun chart( kind : string, norm : test -> double, norm-sdev : test -> double, test-names : list<string>, lang-ntests : list<(string,list<test>)>, ymax : double = 2.0, desc : string = "" ) : string {
  [ tikz-header(test-names,".bench" + kind)
  , lang-ntests.flatmap(fn(l){ tikz-data(kind, norm, norm-sdev, l, ymax = ymax ) })
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
    @"",
    @"\begin{tikzpicture}\sffamily",
    @"\begin{axis}[Chart" + kind + ",ymax=" + ymax.show(1) + ",height=" + height + ",xmax=" + n.show + ".5,width=" + width + "]",
    @"  \draw (axis cs:-0.5,1) -- (axis cs:" + n.show + ".5,1);"
  ]
  val mid = lang-names.map(fn(l){ @"  \draw" + kind + @"{\color" + kind + l + @"{0}}{\data" + kind + l + "};"})
  val footer = [
     if (kind=="time")
       then @"  \legend{" + lang-names.map(fn(l){ "\\lang" + l }).join(",") + "};"
       else "",
     // if (desc.is-empty) then "" else @"  \chartdesc{" + desc.replace-all("\n",@"\\") + "};",
     @"\end{axis}",
     @"\end{tikzpicture}"
  ]
  (header + mid + footer)
}


fun tikz-data( kind:string, norm : test -> double, norm-sdev : test -> double, lang-ntests : (string,list<test>), ymax : double = 2.0 ) : list<string> {
  val (lang,ntests) = lang-ntests
  ["",
   @"\pgfplotstableread{"] +
    ntests.map-indexed(fn(i,t){
      if (t.err.is-empty) then {
        [i.show,
         if (t.norm <= ymax) then t.norm.show-fixed(3) else ymax.show-fixed(3),
         if (t.norm > ymax || t.norm-sdev < 0.001) then "0.000" else t.norm-sdev.show-fixed(3),
         t.norm.show-fixed(2) // else @"{\outerlabel{" + t.norm-elapsed.show(2) + "}}"
        ].join(" ")
      }
      else (i.show + @" 0.100 0.000 " + (if (i==0) then "0" elif (t.err=="NA") then "{NA}" else @"{\ensuremath{\times}}"))
    }) +
    [@"}\data" + kind + lang]
}


// ----------------------------------------------------
// Run a single test
// ----------------------------------------------------

fun run-test( test-name : string, langt : (string,string), iterations : int ) : io test {
  val (lang-long,lang) = langt
  val pre  = lang.pad-left(3) + ", " + test-name.pad-left(12) + ", "
  val dir  = if (lang=="kk") then "koka/out/bench"
             elif (lang=="kkx") then "koka/outx/bench"
             else lang-long
  val base = lang + "-" + test-name
  val prog = if (lang-long=="java")
              then "java --enable-preview --class-path=" + dir + " " + test-name.replace-all("-","")
              else dir + "/" + base
  val progpath = if (lang-long=="java") then (dir.path + (test-name.replace-all("-","") + ".class").path)
                  else prog.path
  println("\nrun: " + prog)

  if (!is-file(progpath)) {
    return Test(test-name,lang,err="NA")
  }

  val results = list(1,iterations)
                .map( fn(i){ execute-test(i,base,prog)} )
                .map( fn(r){
                   match(r) {
                     Left(err)            -> Test(test-name,lang,err=err)
                     Right((elapsed,rss)) -> Test(test-name,lang,elapsed = elapsed, rss = rss)
                 }})
  match(results.filter(fn(t){ !t.err.is-empty })) {
    Cons(t) -> return t
    _       -> ()
  }

  // filter out worst time if more than 2 iterations
  val slowest  = results.map(fn(t){ t.elapsed }).maximum
  val m        = results.filter(fn(t){ t.elapsed == slowest }).length
  val resultsf = if (m==1 && results.length > 2)
                   then results.filter(fn(t){ t.elapsed < slowest })
                   else results
  // take the average of the rest
  val n        = resultsf.length
  val test     = resultsf.foldl1( fn(t1,t2){ t1( elapsed = t1.elapsed + t2.elapsed, rss = t1.rss + t2.rss ) } )

  // calc. stddev
  val avg   = test.elapsed / n.double
  val sdev  = sqrt( resultsf.map( fn(t){ sqr(t.elapsed - avg) } ).sum / n.double )

  test( elapsed = avg, elapsed-sdev = sdev, rss = test.rss / n )
}

fun test-sum( t1 : test, t2 : test) : test {
  t1( elapsed = t1.elapsed + t2.elapsed, rss = t1.rss + t2.rss )
}

fun execute-test( run : int, base : string, prog : string ) : io either<string,(double,int)> {
  val timef= "time-" + base + ".txt"
  val cmd  = if (get-env("SHELL").default("").contains("zsh"))
               then "/usr/bin/time -l 2> " + timef + " " + prog
               else "/usr/bin/time -f'%e %M' -o" + timef + " " + prog
  val out  = run-command(cmd).exn
  print(out)
  val time = read-text-file(timef.path).trim
  if (time=="") return Left("no output")
  match(time.list) {
    Nil -> Left("no output")
    Cons(d) | !d.is-digit -> Left(time)  // error
    _ -> {
      val parts = time.replace-all("\n"," ").replace-all("\t"," ").split(" ").filter(fn(p){ !p.is-empty })
      // println( parts.join(",") )
      match(parts) {
        Cons(elapsed,Cons(rss,Nil)) { // linux
          println(run.show + ": elapsed: " + elapsed + "s, rss: " + rss + "kb" )
          Right( (parse-double(elapsed).default(0.0), parse-int(rss).default(0)) )
        }
        Cons(elapsed,Cons("real",Cons(_,Cons(_user,Cons(_,Cons(_sys,Cons(rss,_))))))) {  // on macOS
          println(run.show + ": elapsed: " + elapsed + "s, rss: " + rss + "b" )
          Right( (parse-double(elapsed).default(0.0), parse-int(rss).default(0)/1024) )
        }
        _ -> Left("bad format")
      }
    }
  }
}

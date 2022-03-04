//---------------------------------------------------------------------------
// Copyright 2021, Daan Leijen.
//---------------------------------------------------------------------------

// ----------------------------------------------------
// Benchmark script
// ----------------------------------------------------

import std/num/float64
import std/os/file
import std/os/path
import std/os/env
import std/os/dir
import std/os/process
import std/os/flags

// ----------------------------------------------------
// Flags
// ----------------------------------------------------

val all-test-names = ["rbtree","rbtree-fbip", "rbtree-fbip-poly","rbtree-ck","binarytrees","deriv","nqueens","cfold"]

val all-lang-names = [
  ("koka","kk"),
  ("kokax","kkx"),
  ("kokadev","kkdev"),
  ("ocaml","ml"),
  ("haskell","hs"),
  ("swift","sw"),
  ("java","jv"),
  ("cpp","cpp")
]


struct iflags {
  tests : string = ""
  langs : string = ""
  iter  : int  = 1
  chart : bool = False
  normalize : bool = False
  help  : bool = False
}

val flag-descs : list<flag<iflags>> = {
  fun set-tests( f : iflags, s : string ) : iflags { f(tests = s) }
  fun set-langs( f : iflags, s : string ) : iflags { f(langs = s) }
  fun set-norm( f : iflags, b : bool ) : iflags { f(normalize = b) }
  fun set-chart( f : iflags, b : bool ) : iflags { f(chart = b) }
  fun set-help( f : iflags, b : bool ) : iflags { f(help = b) }
  fun set-iter( f : iflags, i : string ) : iflags { f(iter = i.parse-int().default(1)) }
  [ Flag( "t", ["test"], Req(set-tests,"test"),  "comma separated list of tests" ),
    Flag( "l", ["lang"], Req(set-langs,"lang"),  "comma separated list of languages"),
    Flag( "i", ["iter"], Req(set-iter,"N"),      "use N (=1) iterations per test"),
    Flag( "c", ["chart"], Bool(set-chart),       "generate latex chart"),
    Flag( "n", ["norm"], Bool(set-norm),         "normalize results relative to Koka"),
    Flag( "h", ["help"], Bool(set-help),         "show this information"),
  ]
}

fun flag-usage() {
  flag-descs.usage("usage:\n koka -e bench.kk -- [options]\n\noptions:").println
  println([
    "\nnotes:",
    "  tests    : " ++ all-test-names.join(", "),
    "  languages: " ++ all-lang-names.map(snd).join(", ")
  ].unlines)
}

pub fun process-flags() : <ndet,console> maybe<iflags> {
  val (flags,args,errs) = parse( Iflags(), flag-descs, get-args() )
  if (flags.help) then {
    flag-usage()
    Nothing
  }
  elif (errs.is-nil && args.is-nil) then {
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
  elapsed: float64 = 0.0
  elapsed-sdev : float64 = 0.0
  rss: int = 0
  err: string = ""
  norm-elapsed: float64 = 0.0
  norm-rss: float64 = 0.0
  norm-elapsed-sdev : float64 = 0.0
}

fun rss-float64(t : test) : float64 {
  t.rss.float64
}

fun show( test : test ) {
  val xs = if (test.err.is-empty) then [
    "" ++ test.elapsed.core/show(2).pad-left(5) ++ "s ~" ++ test.elapsed-sdev.core/show-fixed(3),
    "" ++ test.rss.core/show ++ "kb"
  ] else ["error: " ++ test.err]
  ([test.name,test.lang.pad-left(5)] ++ xs).join(", ")
}

fun show-norm( test : test ) {
  val xs = if (test.err.is-empty) then [
      "" ++ test.norm-elapsed.core/show(2).pad-left(5) ++ "x ~" ++ test.elapsed-sdev.core/show-fixed(3),
      "" ++ test.norm-rss.core/show(2) ++ "x"
    ] else ["error: " ++ test.err]
  ([test.name,test.lang.pad-left(5)] ++ xs).join(", ")
}

// ----------------------------------------------------
// main
// ----------------------------------------------------
pub fun main()
  match (process-flags())
    Nothing -> ()
    Just(flags) ->
      val test-names = if (flags.tests.is-empty) then all-test-names
                        else flags.tests.split(",")
      val lang-names = if (flags.langs.is-empty) then all-lang-names else { 
                          val lnames = flags.langs.split(",")
                          all-lang-names.filter(fn(l){ lnames.any(fn(nm){ nm == l.snd || nm == l.fst }) })
                        }
      run-tests(test-names,lang-names,flags.chart,flags.iter,flags.normalize)


fun run-tests(test-names : list<string>, lang-names : list<(string,string)>, gen-chart : bool, iterations : int, normalize : bool ) {
  println("tests    : " ++ test-names.join(", "))
  println("languages: " ++ lang-names.map(fst).join(", "))

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
    println("\n--- " ++ test-name ++ " ----------------")
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
        norm-rss     = if (koka.rss==0) then 1.0 else t.rss.float64 / koka.rss.float64,
        norm-elapsed-sdev = norm * t.elapsed-sdev)
    }
    println("\n--- normalized " ++ test-name ++ " ----------------")
    println(ntests.map(show-norm).join("\n"))
    ntests
  }

  // group by language
  val lang-ntests = lang-names.map(fn(l) {
    val lang-name = l.snd
    (lang-name, all-ntests.filter(fn(t:test){ t.lang == lang-name }))
  })

  // emit latex chart
  if (gen-chart) then {
    val ymax       = 3.0
    val chart-desc = r"16-core AMD 5950X at 3.4Ghz\\Ubuntu 20.04, gcc 9.3.0"
    val chart-elapsed = if (normalize)
                          then chart("time", True, norm-elapsed, norm-elapsed-sdev, elapsed, test-names, lang-ntests, ymax, chart-desc)
                          else chart("time", False, elapsed, elapsed-sdev, elapsed, test-names, lang-ntests, ymax, chart-desc)
    val chart-rss     = if (True || normalize)
                          then chart("rss", True, norm-rss, fn(t){ 0.0 }, rss-float64, test-names, lang-ntests, ymax, chart-desc)
                          else chart("rss", False, rss-float64, fn(t){ 0.0 }, rss-float64, test-names, lang-ntests, ymax, chart-desc)
    println("\n<!-- chart elapsed --------------- -->")
    println(chart-elapsed)
    println("\n<!-- chart rss --------------- -->")
    println(chart-rss)
  }
  ()
}


// ----------------------------------------------------
// Latex chart
// ----------------------------------------------------
fun chart( kind : string, normalize : bool, norm : test -> float64, norm-sdev : test -> float64, abs : test -> float64, test-names : list<string>, lang-ntests : list<(string,list<test>)>, ymax : float64 = 2.0, desc : string = "" ) : string {
  [ tikz-header(test-names,".bench" ++ kind)
  , lang-ntests.flatmap(fn(l){ tikz-data(kind, normalize, norm, norm-sdev, abs, l, ymax = ymax ) })
  , tikz-picture(kind, normalize, test-names, lang-ntests.map(fst), ymax = ymax, desc = desc )
  , tikz-footer(test-names) ].concat.join("\n")
}


fun tikz-header( test-names : list<string>, attr : string ) : list<string> {
  ["~ Begin Snippet { .benchmark " ++ attr ++ " }",
   "\\pgfplotsset{",
   "  xticklabels = {" ++ test-names.map(fn(n){ "\\strut " ++ n.replace-all("_","\\_")}).join(",") ++ "}",
   "}"]
}

fun tikz-footer( test-names : list<string> ) : list<string> {
  [ "~ End Snippet" ]
}

fun tikz-picture( kind : string, normalize : bool, test-names : list<string>, lang-names : list<string>, ymax : float64 = 5.0, 
                  desc : string = "", height:string = "5cm", width:string = "6cm" ) {
  val n = test-names.length - 1
  val header = [
    r"",
    r"\begin{tikzpicture}\sffamily",
    r"\begin{axis}[Chart" ++ (if (normalize) then "norm" else "abs") ++ kind ++ ",ymax=" ++ ymax.show(1) ++ ",height=" ++ height ++ ",xmax=" ++ n.show ++ ".5,width=" ++ width ++ "]",
    if (normalize) then r"  \draw (axis cs:-0.5,1) -- (axis cs:" ++ n.show ++ ".5,1);" else ""
  ]
  val mid = lang-names.map(fn(l){ r"  \draw" ++ kind ++ r"{color" ++ l ++ r"}{\data" ++ kind ++ l ++ "};"})
  val footer = [
     if (kind=="time") then r"  \legend{" ++ lang-names.map(fn(l){ "\\lang" ++ l }).join(",") ++ "};"
                       else "",
     // if (desc.is-empty) then "" else r"  \chartdesc{" ++ desc.replace-all("\n",r"\\") ++ "};",
     r"\end{axis}",
     r"\end{tikzpicture}"
  ]
  (header ++ mid ++ footer)
}


fun tikz-data( kind:string, normalize : bool, norm : test -> float64, norm-sdev : test -> float64, abs : test -> float64, lang-ntests : (string,list<test>), ymax : float64 = 5.0 ) : list<string> {
  val (lang,ntests) = lang-ntests
  ["",
   r"\pgfplotstableread{",
   r"x y y-error meta"] ++
    ntests.map-indexed(fn(i:int,t:test){
      val tval = if (normalize) then t.norm else t.abs
      if (t.err.is-empty) then {
        [i.show,
         if (tval <= ymax) then tval.show-fixed(3) else ymax.show-fixed(3),
         if (tval > ymax || t.norm-sdev < 0.001) then "0.000" else t.norm-sdev.show-fixed(3),
         if (tval <= ymax) 
           then (if (normalize) then (
                   if (lang == "kk") 
                   then (if (kind=="rss") 
                            then r"{\absrssnormlabel{" ++ (t.abs / 1024.0).round.int.show ++ "mb}}" // megabytes  
                            else r"{\absnormlabel{" ++ t.abs.show-fixed(2) ++ "}}" )
                   else r"{\normlabel{" ++ t.norm.show-fixed(2) ++ "}}" )
                else r"{\abslabel{" ++ t.abs.show-fixed(2) ++ "}}")
           else (if (normalize) 
                   then (r"{\outernormlabel{" ++ t.norm.show(2) ++ "}}")
                   else (r"{\outerlabel{" ++ t.abs.show(2) ++ "}}"))
         ].join(" ")
      }
      else "" // ("" ++ i.show ++ r" 0.100 0.000 " ++ (if (i==0) then "0" elif (t.err=="NA") then "{NA}" else r"{\ensuremath{\times}}"))
    }) ++
    [r"}\data" ++ kind ++ lang]
}


// ----------------------------------------------------
// Run a single test
// ----------------------------------------------------
fun insert(xs:list<float64>, y :float64) : list<float64> {
  match(xs) {
    Cons(x,xx) | y > x -> Cons(x,xx.insert(y))
    _  -> Cons(y,xs)               
  }
}

fun sort(xs : list<float64> ) : list<float64> {
  match(xs) {
    Cons(x,xx) -> xx.sort.insert(x)
    Nil -> Nil
  }
}

fun median( xs : list<float64> ) : float64 {
  val n = xs.length
  val ys = xs.sort
  match(ys.drop(n/2 - 1)) {
    Cons(x1,Cons(x2)) -> (x1 + x2)/2.0
    Cons(x,Nil)       -> x
    _                 -> 0.0
  }
}

fun run-test( test-name : string, langt : (string,string), iterations : int ) : io test {
  val (lang-long,lang) = langt
  val pre  = "" ++ lang.pad-left(4) ++ ", " ++ test-name.pad-left(12) ++ ", "
  // val dir  = "out/" ++ lang
  val dir  = if (lang=="kk") then "koka/out/bench"
             elif (lang=="kkx") then "koka/outx/bench"
             elif (lang=="kkdev") then "koka/outdev/bench"
             else lang-long  
  val base = lang ++ "-" ++ test-name
  val prog = if (lang-long=="java")
              then "java --enable-preview --class-path=" ++ dir ++ " " // ++ "-Xmx1G "
                    ++ (if (test-name=="cfold") then "-Xss128m " else "")
                    ++ test-name.replace-all("-","")             
              else dir ++ "/" ++ base
  val envvars  = if (lang-long=="ocaml" && test-name=="cfold")
                   then "OCAMLRUNPARAM=\"l=2560000\""
                   else ""
  val progpath = if (lang-long=="java") then (dir.path / (test-name.replace-all("-","") ++ ".class"))
                  else prog.path
  println("\nrun: " ++ prog)

  if (!is-file(progpath)) then {
    return Test(test-name,lang,err="NA")
  }

  val results = list(1,iterations)
                .map( fn(i){ execute-test(i,base,prog,envvars)} )
                .map( fn(r){
                   match(r) {
                     Left(err)            -> Test(test-name,lang,err=err)
                     Right((elapsed,rss)) -> {
                       // println("elapsed: " ++ elapsed.show ++ ", rss: " ++ rss.show ++ "k")
                       Test(test-name,lang,elapsed = elapsed, rss = rss)
                     }
                 }})
  match(results.filter(fn(t){ !t.err.is-empty })) {
    Cons(t) -> return t
    _       -> ()
  }

  val melapsed   = results.map(elapsed).median
  val mrss       = results.map(fn(t){ t.rss.float64 }).median.int
  val sdev       = sqrt( results.map( fn(t){ sqr(t.elapsed - melapsed) } ).sum / results.length.float64 )
  // println("melapsed: " ++ melapsed.show ++ ", mrss: " ++ mrss.show ++ "k")

  Test(test-name, lang, elapsed=melapsed, rss=mrss, elapsed-sdev=sdev)  
}

fun test-sum( t1 : test, t2 : test) : test {
  t1( elapsed = t1.elapsed + t2.elapsed, rss = t1.rss + t2.rss )
}

fun execute-test( run : int, base : string, prog : string, envvars : string ) : io either<string,(float64,int)> 
  val timef= "out/time-" ++ base ++ ".txt"
  val cmd  = (if envvars.is-empty then "" else ("env " ++ envvars ++ " ")) ++
             (if get-env("SHELL").default("").contains("zsh")
                then "/usr/bin/time -l 2> " ++ timef ++ " " ++ prog
                else "/usr/bin/time -f'%e %M' -o" ++ timef ++ " " ++ prog)
  ensure-dir("out".path)
  run-system("sleep 2")
  val out  = run-system-read(cmd).exn
  print(out)
  val time = read-text-file(timef.path).trim
  if  time=="" return Left("no output")
  match time.list
    Nil -> Left("no output")
    Cons(d) | !d.is-digit -> Left(time)  // error
    _ -> 
      val parts = time.replace-all("\n"," ").replace-all("\t"," ").split(" ").filter(fn(p) !p.is-empty )
      // println( parts.join(",") )
      match(parts)
        Cons(elapsed,Cons(rss,Nil)) -> // linux
          println("" ++ run.show ++ ": elapsed: " ++ elapsed ++ "s, rss: " ++ rss ++ "kb" )
          Right( (parse-float64(elapsed).default(0.0), parse-int(rss).default(0)) )

        Cons(elapsed,Cons("real",Cons(_,Cons(_user,Cons(_,Cons(_sys,Cons(rss,_))))))) ->  // on macOS
          println("" ++ run.show ++ ": elapsed: " ++ elapsed ++ "s, rss: " ++ rss ++ "b" )
          Right( (parse-float64(elapsed).default(0.0), parse-int(rss).default(0)/1024) )
        
        _ -> Left("bad format")

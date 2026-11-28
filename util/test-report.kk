/*---------------------------------------------------------------------------
  Copyright 2026, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Generate an HTML report of the test run times and compare them against a baseline.

   The run times are written by the test suite with `--runtimes=<dir>/<config>/<file>.json`
   as `{"tests": {"<dir>/<test>": <ms>, ...}}`; all json files under a `<config>` directory
   are merged (e.g. the shards of a CI run).

   Usage:
     koka -e util/test-report.kk -- --current=<dir> [--baseline=<dir>] [--out=report.html] [--summary=summary.md]
*/
module test-report

import std/os/path
import std/os/dir
import std/os/file
import std/os/env
import std/os/flags
import std/num/float64

// ---------------------------------------------------------------------------
// Options
// ---------------------------------------------------------------------------

struct options
  current  : string = ""
  baseline : string = ""
  out      : string = "test-report.html"
  summary  : string = ""
  min-ms   : int = 10       // only compare tests that take at least this long
  noise    : int = 15       // changes within this percentage are considered noise
  all      : bool = False   // show all tests (instead of only those taking at least `min-ms`)
  help     : bool = False

val option-descs : list<flag<options>> =
  fun set-current( o : options, s : string ) : options  { o(current = s) }
  fun set-baseline( o : options, s : string ) : options { o(baseline = s) }
  fun set-out( o : options, s : string ) : options      { o(out = s) }
  fun set-summary( o : options, s : string ) : options  { o(summary = s) }
  fun set-min( o : options, s : string ) : options      { o(min-ms = s.parse-int.default(o.min-ms)) }
  fun set-noise( o : options, s : string ) : options    { o(noise = s.parse-int.default(o.noise)) }
  fun set-all( o : options, b : bool ) : options        { o(all = b) }
  fun set-help( o : options, b : bool ) : options       { o(help = b) }
  [ Flag("c", ["current"],  Req(set-current,"DIR"),  "directory with the current run times"),
    Flag("b", ["baseline"], Req(set-baseline,"DIR"), "directory with the baseline run times"),
    Flag("o", ["out"],      Req(set-out,"FILE"),     "output html file (default: test-report.html)"),
    Flag("s", ["summary"],  Req(set-summary,"FILE"), "also write a markdown summary"),
    Flag("",  ["min"],      Req(set-min,"MS"),       "only compare tests that take at least MS milliseconds (default: 10)"),
    Flag("",  ["noise"],    Req(set-noise,"PCT"),    "ignore changes within PCT percent (default: 15)"),
    Flag("a", ["all"],      Bool(set-all),           "show all tests"),
    Flag("h", ["help"],     Bool(set-help),          "show this help")
  ]

// ---------------------------------------------------------------------------
// Minimal JSON parser
// ---------------------------------------------------------------------------

type json
  JNull
  JBool( b : bool )
  JNum( d : float64 )
  JStr( s : string )
  JArr( items : list<json> )
  JObj( fields : list<(string,json)> )

// Recursive-descent parser that consumes a list of characters (linear time, no backtracking)
fun parse-json( s : string ) : <div,exn> json
  val (v,rest) = jvalue(s.list)
  if rest.skip-ws.is-cons then throw("json: expecting end of input")
  v

fun skip-ws( cs : list<char> ) : list<char>
  match cs
    Cons(c,cc) | c.is-white -> skip-ws(cc)
    _ -> cs

fun expect( cs : list<char>, c : char ) : exn list<char>
  match cs.skip-ws
    Cons(d,cc) | d == c -> cc
    _ -> throw("json: expecting '" ++ c.string ++ "'")

fun jvalue( cs : list<char> ) : <div,exn> (json, list<char>)
  match cs.skip-ws
    Cons('{',cc) -> jfields(cc, [])
    Cons('[',cc) -> jitems(cc, [])
    Cons('"',cc) -> { val (str,rest) = jstring(cc, []); (JStr(str), rest) }
    Cons('t',_)  -> cs.skip-ws.jliteral("true", JBool(True))
    Cons('f',_)  -> cs.skip-ws.jliteral("false", JBool(False))
    Cons('n',_)  -> cs.skip-ws.jliteral("null", JNull)
    ws -> jnumber(ws)

fun jfields( cs : list<char>, acc : list<(string,json)> ) : <div,exn> (json, list<char>)
  match cs.skip-ws
    Cons('}',cc) | acc.is-nil -> (JObj([]), cc)
    ws ->
      val (k,cs1) = jstring(ws.expect('"'), [])
      val (v,cs2) = jvalue(cs1.expect(':'))
      val fs = Cons((k,v),acc)
      match cs2.skip-ws
        Cons(',',cc) -> jfields(cc, fs)
        Cons('}',cc) -> (JObj(fs.reverse), cc)
        _ -> throw("json: expecting ',' or '}'")

fun jitems( cs : list<char>, acc : list<json> ) : <div,exn> (json, list<char>)
  match cs.skip-ws
    Cons(']',cc) | acc.is-nil -> (JArr([]), cc)
    ws ->
      val (v,cs1) = jvalue(ws)
      val xs = Cons(v,acc)
      match cs1.skip-ws
        Cons(',',cc) -> jitems(cc, xs)
        Cons(']',cc) -> (JArr(xs.reverse), cc)
        _ -> throw("json: expecting ',' or ']'")

fun jstring( cs : list<char>, acc : list<char> ) : exn (string, list<char>)
  match cs
    Cons('"',cc) -> (acc.reverse.string, cc)
    Cons('\\',Cons(c,cc)) ->
      val d = match c
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'
                _   -> c
      jstring(cc, Cons(d,acc))
    Cons(c,cc) -> jstring(cc, Cons(c,acc))
    Nil -> throw("json: unterminated string")

fun jnumber( cs : list<char> ) : exn (json, list<char>)
  val (ds,rest) = cs.span(fn(c) c.is-digit || c=='-' || c=='+' || c=='.' || c=='e' || c=='E')
  match ds.string.parse-float64
    Just(d) | ds.is-cons -> (JNum(d), rest)
    _ -> throw("json: invalid value")

fun jliteral( cs : list<char>, lit : string, v : json ) : exn (json, list<char>)
  val n = lit.count
  if cs.take(n).string == lit then (v, cs.drop(n)) else throw("json: invalid value")

// ---------------------------------------------------------------------------
// Run times
// ---------------------------------------------------------------------------

alias times = list<(string,int)>   // test name to milliseconds

struct config
  name  : string
  times : times

fun lookup( ts : times, name : string ) : maybe<int>
  match ts.find(fn(t) t.fst == name)
    Just((_,ms)) -> Just(ms)
    Nothing      -> Nothing

fun parse-times( fname : path, content : string ) : <div,exn> times
  val res = try{ parse-json(content) }
  match res
    Ok(JObj(fields)) -> match fields.find(fn(f) f.fst == "tests")
      Just((_,JObj(tests))) -> tests.flatmap fn(nv)
                                 match nv.snd
                                   JNum(d) -> [(nv.fst, d.int)]
                                   _       -> []
      _ -> throw(fname.string ++ ": missing \"tests\" field")
    Ok(_)    -> throw(fname.string ++ ": expecting a json object")
    Error(e) -> throw(fname.string ++ ": " ++ e.message)

// Load all `<config>/*.json` files under `dir` (recursively)
fun load-configs( dir : string ) : <fsys,div,exn> list<config>
  if dir.is-empty || !dir.path.is-directory then return []
  val files = list-directory-recursive(dir.path).filter(fn(p) p.extname == "json" && p.is-file)
  val tagged = files.map(fn(p) (p.parentname, parse-times(p, read-text-file(p))))
  tagged.map(fst).sort-unique.map fn(name)
    Config(name, tagged.filter(fn(t) t.fst == name).flatmap(snd))

// ---------------------------------------------------------------------------
// Sorting
// ---------------------------------------------------------------------------

fun msort( xs : list<a>, lt : (a,a) -> bool ) : div list<a>
  match xs
    Nil -> Nil
    Cons(_,Nil) -> xs
    _ ->
      val (ys,zs) = xs.split(xs.length / 2)
      merge(msort(ys,lt), msort(zs,lt), lt)

fun merge( xs : list<a>, ys : list<a>, lt : (a,a) -> bool ) : div list<a>
  match (xs,ys)
    (Nil,_) -> ys
    (_,Nil) -> xs
    (Cons(x,xx),Cons(y,yy)) -> if lt(y,x) then Cons(y,merge(xs,yy,lt)) else Cons(x,merge(xx,ys,lt))

fun sort-unique( xs : list<string> ) : div list<string>
  fun dedup( ys : list<string> ) : list<string>
    match ys
      Cons(y1,yy as Cons(y2)) | y1 == y2 -> dedup(yy)
      Cons(y,yy) -> Cons(y,dedup(yy))
      Nil -> Nil
  xs.msort(fn(x,y) x < y).dedup

// ---------------------------------------------------------------------------
// Comparison
// ---------------------------------------------------------------------------

// A single measurement of a test in a configuration
struct entry
  config : string
  test   : string
  cur    : maybe<int>
  base   : maybe<int>

// ratio of the current time over the baseline, if both are significant
fun ratio( e : entry, opts : options ) : maybe<float64>
  match (e.cur, e.base)
    (Just(c), Just(b)) | max(c,b) >= opts.min-ms -> Just(c.float64 / max(b,1).float64)
    _ -> Nothing

fun is-regression( r : float64, opts : options ) : bool
  r >= 1.0 + opts.noise.float64 / 100.0

fun is-improvement( r : float64, opts : options ) : bool
  r <= 1.0 / (1.0 + opts.noise.float64 / 100.0)

// red for regressions and green for improvements, saturating at a factor 2
fun ratio-style( r : maybe<float64>, opts : options ) : string
  match r
    Just(x) | x.is-regression(opts) || x.is-improvement(opts) ->
      val f = min(1.0, if x >= 1.0 then log2(x) else 0.0 - log2(x))
      val hue = if x >= 1.0 then "0 80%" else "130 50%"
      " style=\"background: hsl(" ++ hue ++ " " ++ (94.0 - 26.0*f).show-fixed(0) ++ "%)\""
    _ -> ""

fun show-ratio( r : float64 ) : string
  val pct = ((r - 1.0) * 100.0).round.int
  (if pct >= 0 then "+" else "") ++ pct.show ++ "%"

fun show-ms( ms : maybe<int> ) : string
  match ms
    Just(t) -> t.show
    Nothing -> "&ndash;"

// ---------------------------------------------------------------------------
// HTML
// ---------------------------------------------------------------------------

fun html( s : string ) : string
  s.replace-all("&","&amp;").replace-all("<","&lt;").replace-all(">","&gt;")

fun cell( e : entry, opts : options ) : string
  val r = e.ratio(opts)
  val title = match e.base
    Just(b) -> " title=\"baseline: " ++ b.show ++ " ms\""
    Nothing -> ""
  val delta = match r
    Just(x) -> " <small>" ++ x.show-ratio ++ "</small>"
    Nothing -> ""
  "<td" ++ r.ratio-style(opts) ++ title ++ ">" ++ e.cur.show-ms ++ delta ++ "</td>"

val style = r#"
:root  { --fg: #1f2328; --muted: #59636e; --border: #d1d9e0; --line: #eaeef2; --head: #f6f8fa; --stripe: #fafbfc; --hover: #eef4ff; }
body   { margin: 0; color: var(--fg); background: #fff;
         font: 13px/1.45 -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif; }
header { padding: 20px 32px 16px; background: var(--head); border-bottom: 1px solid var(--border); }
h1     { margin: 0 0 6px; font-size: 20px; font-weight: 600; }
header p { margin: 0; max-width: 90em; color: var(--muted); }
main   { padding: 4px 32px 32px; }
h2     { margin: 28px 0 10px; padding-bottom: 6px; font-size: 15px; font-weight: 600; border-bottom: 1px solid var(--border); }
table  { border-collapse: separate; border-spacing: 0; border: 1px solid var(--border); border-radius: 6px; font-variant-numeric: tabular-nums; }
th, td { padding: 4px 12px; text-align: right; white-space: nowrap; border-bottom: 1px solid var(--line); }
th     { position: sticky; top: 0; z-index: 1; background: var(--head); color: var(--muted); font-size: 12px; font-weight: 600; border-bottom-color: var(--border); }
th:first-child { border-top-left-radius: 6px; }
th:last-child  { border-top-right-radius: 6px; }
tbody tr:last-child td      { border-bottom: none; }
tbody tr:nth-child(even) td { background-color: var(--stripe); }
tbody tr:hover td           { background-color: var(--hover); }
.name    { text-align: left; }
td.name  { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; font-size: 12px; }
td small { margin-left: 4px; color: #3d444d; font-size: 11px; }
td[title] { cursor: help; }
.legend  { display: inline-block; margin-left: 4px; padding: 0 8px; border-radius: 10px; color: var(--fg); }
"#

fun table( header : string, rows : list<string> ) : string
  "<table><thead><tr>" ++ header ++ "</tr></thead><tbody>\n" ++ rows.join ++ "</tbody></table>\n"

fun totals-row( c : config, entries : list<entry>, opts : options ) : string
  fun sum( es : list<entry>, f : entry -> maybe<int> ) : int
    es.foldl(0, fn(acc,e) acc + f(e).default(0))
  val es     = entries.filter(fn(e) e.config == c.name)
  val both   = es.filter(fn(e) e.cur.is-just && e.base.is-just)
  val cur-ms = both.sum(cur)
  val bas-ms = both.sum(base)
  val r      = if both.is-nil then Nothing else Just(cur-ms.float64 / max(bas-ms,1).float64)
  val rs     = es.flatmap(fn(e) e.ratio(opts).list)
  val compared = if both.is-nil then "&ndash;" else cur-ms.show ++ " / " ++ bas-ms.show ++ r.map(fn(x) " <small>" ++ x.show-ratio ++ "</small>").default("")
  "<tr><td class=\"name\">" ++ c.name.html ++ "</td><td>" ++ es.length.show ++ "</td><td>" ++ es.sum(cur).show ++ "</td>" ++
  "<td" ++ r.ratio-style(opts) ++ ">" ++ compared ++ "</td>" ++
  "<td>" ++ rs.filter(fn(x) x.is-regression(opts)).length.show ++ "</td>" ++
  "<td>" ++ rs.filter(fn(x) x.is-improvement(opts)).length.show ++ "</td></tr>"

fun changes( entries : list<entry>, opts : options, pred : float64 -> bool ) : div list<(entry,float64)>
  entries.flatmap(fn(e) e.ratio(opts).list.filter(pred).map(fn(r) (e,r)))
         .msort(fn(x,y) x.snd > y.snd)

fun changes-table( title : string, cs : list<(entry,float64)>, opts : options ) : string
  if cs.is-nil then return ""
  "<h2>" ++ title ++ "</h2>\n" ++
  table("<th class=\"name\">test</th><th class=\"name\">configuration</th><th>current (ms)</th><th>baseline (ms)</th>",
        cs.map(fn(er) val e = er.fst in "<tr><td class=\"name\">" ++ e.test.html ++ "</td><td class=\"name\">" ++ e.config.html ++ "</td>" ++
                                        e.cell(opts) ++ "<td>" ++ e.base.show-ms ++ "</td></tr>\n"))

fun report( configs : list<config>, tests : list<string>, entries : list<entry>, has-base : bool, opts : options ) : div string
  val shown = if opts.all then tests else
                tests.filter(fn(t) entries.any(fn(e) e.test == t && max(e.cur.default(0),e.base.default(0)) >= opts.min-ms))
  val regressions  = entries.changes(opts, fn(r) r.is-regression(opts))
  val improvements = entries.changes(opts, fn(r) r.is-improvement(opts)).reverse
  "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\"><title>Koka test run times</title><style>" ++ style ++ "</style></head><body>\n" ++
  "<header><h1>Koka test run times</h1>\n" ++
  "<p>Run times in milliseconds" ++
    (if has-base then ", compared against the baseline (hover a cell to see the baseline time). " ++
                      "Only tests taking at least " ++ opts.min-ms.show ++ " ms are compared, and changes within " ++ opts.noise.show ++ "% are ignored." ++
                      " <span class=\"legend\"" ++ Just(2.0).ratio-style(opts) ++ ">slower</span><span class=\"legend\"" ++ Just(0.5).ratio-style(opts) ++ ">faster</span>"
                 else " (no baseline available).") ++ "</p></header>\n<main>\n" ++
  "<h2>Totals</h2>\n" ++
  table("<th class=\"name\">configuration</th><th>tests</th><th>total (ms)</th><th>compared (ms)</th><th>slower</th><th>faster</th>",
        configs.map(fn(c) c.totals-row(entries,opts) ++ "\n")) ++
  changes-table("Regressions", regressions, opts) ++
  changes-table("Improvements", improvements, opts) ++
  "<h2>" ++ (if opts.all then "All tests" else "Tests taking at least " ++ opts.min-ms.show ++ " ms") ++ "</h2>\n" ++
  table("<th class=\"name\">test</th>" ++ configs.map(fn(c) "<th>" ++ c.name.html ++ "</th>").join,
        shown.map(fn(t) "<tr><td class=\"name\">" ++ t.html ++ "</td>" ++
                        configs.map(fn(c) entries.find(fn(e) e.config == c.name && e.test == t)
                                                 .map(fn(e) e.cell(opts)).default("<td></td>")).join ++ "</tr>\n")) ++
  "</main>\n</body></html>\n"

// ---------------------------------------------------------------------------
// Markdown summary (e.g. for $GITHUB_STEP_SUMMARY)
// ---------------------------------------------------------------------------

fun summary( entries : list<entry>, has-base : bool, opts : options ) : div string
  if !has-base then return "No baseline run times available.\n"
  val regressions  = entries.changes(opts, fn(r) r.is-regression(opts))
  val improvements = entries.changes(opts, fn(r) r.is-improvement(opts))
  fun row( er : (entry,float64) ) : string
    val (e,r) = er
    "| " ++ e.test ++ " | " ++ e.config ++ " | " ++ e.cur.default(0).show ++ " | " ++ e.base.default(0).show ++ " | " ++ r.show-ratio ++ " |\n"
  val table = if regressions.is-nil then "" else
                "| test | configuration | current (ms) | baseline (ms) | change |\n|:--|:--|--:|--:|--:|\n" ++ regressions.take(20).map(row).join
  "### Test run times\n\n" ++ regressions.length.show ++ " slower and " ++ improvements.length.show ++
  " faster (at least " ++ opts.min-ms.show ++ " ms and more than " ++ opts.noise.show ++ "% change); see the test-report.html artifact for details.\n\n" ++
  table

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

pub fun main()
  val header = "usage:\n koka -e util/test-report.kk -- [options]\n\noptions:"
  val (opts, _rest, errs) = parse(Options(), option-descs, get-args())
  if errs.is-cons || opts.help || opts.current.is-empty then
    errs.foreach(println)
    println(option-descs.usage(header))
    return ()
  val configs  = load-configs(opts.current)
  val baseline = load-configs(opts.baseline)
  val has-base = baseline.is-cons
  val tests    = configs.flatmap(fn(c) c.times.map(fst)).sort-unique
  val entries  = configs.flatmap fn(c)
                   val btimes = baseline.find(fn(b) b.name == c.name).map(times).default([])
                   c.times.map(fn(tm) Entry(c.name, tm.fst, Just(tm.snd), btimes.lookup(tm.fst)))
  write-text-file(opts.out.path, report(configs, tests, entries, has-base, opts))
  println("written: " ++ opts.out)
  if !opts.summary.is-empty then
    write-text-file(opts.summary.path, summary(entries, has-base, opts))
    println("written: " ++ opts.summary)

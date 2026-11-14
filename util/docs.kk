/*-----------------------------------------------------------------------------
Copyright 2020-2024, Microsoft Research, Daan Leijen.
This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.

Run this to generate documentation from the `doc/spec` directory
using the Madoko program (install: `> npm install -g madoko`)

With `--gh-pages=<site>` it instead places documentation builds (`--publish`)
and playground builds (web/playground/dist) in a GitHub Pages site directory
(https://koka-lang.github.io/koka/...); see `gh-pages` below for the layout.
-----------------------------------------------------------------------------*/
import std/os/path
import std/os/flags
import std/os/env
import std/os/file
import std/os/dir
import std/os/process

val header = "usage:\n stack exec koka -- util/docs [-- [options]]\n\noptions:"

struct iflags
  publish  : bool  = False
  version  : string = ""
  speconly : bool  = False
  site     : string = "https://koka-lang.github.io/koka/doc/"
  pubdir   : string = "../koka-pages/doc"
  relative : bool  = False
  ghpages  : string = ""
  channel  : string = ""
  latest   : bool  = False
  docsdir  : string = ""
  playground : string = ""


val flag-descs : list<flag<iflags>> =
  fun set-publish( f : iflags, b : bool ) : iflags
    f(publish = b)
  fun set-speconly( f : iflags, b : bool ) : iflags
    f(speconly = b)
  fun set-version( f : iflags, s : string ) : iflags
    f(version = s)
  fun set-site( f : iflags, s : string ) : iflags
    f(site = s)
  fun set-pubdir( f : iflags, s : string ) : iflags
    f(pubdir = s)
  fun set-relative( f : iflags, b : bool ) : iflags
    f(relative = b)
  fun set-ghpages( f : iflags, s : string ) : iflags
    f(ghpages = s)
  fun set-channel( f : iflags, s : string ) : iflags
    f(channel = s)
  fun set-latest( f : iflags, b : bool ) : iflags
    f(latest = b)
  fun set-docsdir( f : iflags, s : string ) : iflags
    f(docsdir = s)
  fun set-playground( f : iflags, s : string ) : iflags
    f(playground = s)

  [ Flag( "p", ["publish"], Bool(set-publish), "publish to the --pubdir directory" ),
    Flag( "v", ["version"], Req(set-version,"<version>"), "latest koka version" ),
    Flag( "s", ["speconly"], Bool(set-speconly), "specification only"),
    Flag( "", ["site"], Req(set-site,"<url>"), "url the published documentation is served from (default: https://koka-lang.github.io/koka/doc/)" ),
    Flag( "", ["pubdir"], Req(set-pubdir,"<dir>"), "directory to publish to (default: ../koka-pages/doc)" ),
    Flag( "", ["relative"], Bool(set-relative), "link the pages to each other relatively instead of through --site, so they work wherever they are copied" ),
    Flag( "", ["gh-pages"], Req(set-ghpages,"<site>"), "instead of building, place the --docs and --playground builds in the GitHub Pages site directory <site> and rewrite its version index" ),
    Flag( "", ["channel"], Req(set-channel,"<channel>"), "with --gh-pages: dev, or a release tag (v<version>)" ),
    Flag( "", ["latest"], Bool(set-latest), "with --gh-pages: the release is the newest stable one, also placed in stable/ and at the root" ),
    Flag( "", ["docs"], Req(set-docsdir,"<dir>"), "with --gh-pages: a documentation build (--publish --relative)" ),
    Flag( "", ["playground"], Req(set-playground,"<dir>"), "with --gh-pages: a playground build (web/playground/dist)" )
  ]


fun process-flags() : <ndet,console> maybe<iflags>
  val (flags,args,errs) = parse( Iflags(), flag-descs, get-args() )
  if errs.is-nil && args.is-nil then
    Just(flags)
  else
    println( errs.join("\n") ++ "\n" ++ flag-descs.usage(header) )
    Nothing

pub fun main()
  match process-flags()
    Nothing -> ()
    Just(flags) | !flags.ghpages.is-empty -> gh-pages(flags.ghpages.path, flags.channel, flags.latest, flags.docsdir, flags.playground)
    Just(flags) -> build(flags.publish, flags.version || get-compiler-version(), flags.speconly, (if flags.relative then "" else flags.site), flags.pubdir.path)

fun build( publish : bool, version : string, speconly : bool, docsite : string, doclocal : path ) : io ()

  val outspec  = ".koka/spec".path;
  //val outspec   = outspecx ++ "js-debug"
  val outstyles = outspec / "styles"
  val outscripts = outspec / "scripts"
  val outimages = outspec / "images"
  val specdir   = "doc/spec".path
  val docflags  = "--htmlcss=styles/madoko.css --htmlcss=styles/koka.css " ++ (if (publish && !docsite.is-empty) then "--htmlbases=" ++ docsite ++ " " else "")
  val cmd = "koka -l --outputdir=" ++ outspec.string ++ " -i" ++ specdir.string ++ " --html " ++ docflags
  if !speconly then
    println("compile standard libraries...")
    run-echo(cmd ++ " toc.kk")

  println("compile documentation inlined code...")
  run-echo(cmd ++ " book.kk.md getstarted.kk.md why.kk.md tour.kk.md spec.kk.md index.kk.md")
  println("patch book.md")
  file-replace-all(outspec / "book.md", ".kk.md", ".md");

  println("copy web files...")
  ensure-dir(outstyles)
  ensure-dir(outimages)
  ensure-dir(outscripts)
  copy-directory(specdir / "images", outimages)
  copy-directory(specdir / "styles", outstyles)
  copy-directory(specdir / "scripts", outscripts)
  copy-file-to-dir("doc/koka.css".path, outstyles)
  copy-file-to-dir("doc/logo/koka-logo.png".path, outimages)
  copy-file-to-dir("doc/logo/koka-logo-filled.png".path, outimages)
  copy-file-to-dir(specdir / "koka.bib", outspec)
  copy-file-to-dir("lib/std/time/timescales.png".path, outspec)
  copy-file-to-dir("doc/spec/book-style.md".path, outspec)
  copy-file-to-dir("doc/spec/news.mdk".path, outspec)
  copy-file-to-dir("doc/spec/install.mdk".path, outspec)
  copy-file-to-dir("doc/spec/arm64.json".path, outspec)

  val xmps = list-directory(outspec).
             filter(fn(p){ p.string.ends-with(".xmp.html").bool }).
             map(fn(p){ p.nodir.string })
  println("run madoko...")
  run-echo( "cd " ++ outspec.string 
             // ++ " && node ../../../madoko/lib/cli.js"
             ++ " && madoko"
             ++ " --odir=. -v -mline-no:false -mlogo:false -mkokaversion:" ++ version
             ++ " " ++ xmps.join(" ") ++ " book.md index.md" )

  copy-file-to-dir(outspec / "madoko.css", outstyles)
  if publish then
    println("copy files to output folder: " ++ doclocal.string)
    ensure-dir(doclocal)
    val htmls = list-directory(outspec).
                filter(fn(p){ p.extname == "html" && !p.string.ends-with(".xmp.html").bool })
    htmls.map fn(p) copy-file-to-dir(p,doclocal)
    copy-directory(outstyles,doclocal / "styles")
    copy-directory(outimages,doclocal / "images")

  println("done.")


// Places documentation and playground builds in a GitHub Pages site directory
// and rewrites its version index. Layout, where <channel> is `dev` or a release tag:
//
//   doc/, playground/   the newest stable release, the default that everything links to
//   stable/             the newest stable release
//   dev/                the latest build of the dev branch (earlier ones are not kept)
//   <tag>/              every release tag, e.g. v3.2.9/
//   versions.html       the list of all of them (and versions.json, for a version switcher)
//   index.html          a redirect to doc/
//
// Each channel directory holds doc/ and, for versions that have it, playground/.
// `latest` marks the tag as the newest stable release, which also places it in
// stable/ and at the root. A channel's directories are replaced, except the
// root doc/, which is copied over without deleting so that links to pages a
// release dropped keep working. The doc pages link to each other relatively and
// the playground uses relative paths, so one build serves every location.
//
// The playground's service worker (coi-serviceworker.js) controls every page
// under the directory it is served from, adding cross-origin isolation headers
// that block cross-origin resources the doc pages use, so it must stay inside a
// playground/ directory: never place a playground at the site root.
fun gh-pages( site : path, channel : string, latest : bool, docs : string, playground : string ) : io ()
  if channel != "dev" && !channel.starts-with("v").bool then throw("gh-pages: unknown channel: " ++ channel)
  if !docs.is-empty && !is-file(docs.path / "book.html") then throw("gh-pages: no book.html in " ++ docs)
  if !playground.is-empty && !is-file(playground.path / "index.html") then throw("gh-pages: no index.html in " ++ playground)
  val targets = if latest then [channel, "stable"] else [channel]
  targets.foreach fn(t)
    if !docs.is-empty then put(docs, site / t / "doc", True)
    if !playground.is-empty then put(playground, site / t / "playground", True)
  if latest then
    write-text-file(site / "stable" / "version.txt", channel ++ "\n")
    if !docs.is-empty then put(docs, site / "doc", False)
    if !playground.is-empty then put(playground, site / "playground", True)
  write-index(site)

fun put( build : string, target : path, delete : bool ) : io ()
  ensure-dir(target)
  // source maps double the size of a playground (35 of 90 MB) and are not needed to run it
  run-echo("rsync -a " ++ (if delete then "--delete " else "") ++ "--exclude '*.map' " ++ build ++ "/ " ++ target.string ++ "/")

fun write-index( site : path ) : io ()
  val stable = if is-file(site / "stable" / "version.txt") then read-text-file(site / "stable" / "version.txt").trim else ""
  val tags = version-sort(list-directory(site).map(fn(p) p.nodir.string)
                          .filter(fn(t) t.starts-with("v").bool && is-directory(site / t / "doc"))).reverse
  val channels = ["stable", "dev"] ++ tags
  // The root leads to the default documentation. Relative, so that a fork's
  // site leads to its own pages; the script keeps the #fragment.
  write-text-file(site / "index.html",
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">\n" ++
    "<meta http-equiv=\"refresh\" content=\"1; url=doc/index.html\"><title>Koka</title>\n" ++
    "<script>location.replace(\"doc/index.html\" + location.hash)</script>\n" ++
    "</head><body><a href=\"doc/index.html\">The Koka Programming Language</a></body></html>\n")
  // A channel directory gets a landing page rather than a redirect: an instant
  // redirect is what phishing pages look like to browsers' safe-browsing checks.
  channels.foreach fn(c)
    if is-directory(site / c / "doc") then
      val label = if c == "stable" then "stable (" ++ stable ++ ")" elif c == "dev" then "dev (development version)" else c
      write-text-file(site / c / "index.html",
        page-start("Koka " ++ label) ++
        "<p>" ++ links(site, c, "") ++ "</p>\n" ++
        "<p><a href=\"../versions.html\">All versions</a></p></body></html>\n")
  val rows = (if stable.is-empty then "" else "<tr><td class=\"v\">stable</td><td>" ++ stable ++ "</td><td>" ++ links(site, "stable", "stable/") ++ "</td></tr>\n") ++
             (if is-directory(site / "dev" / "doc") then "<tr><td class=\"v\">dev</td><td>development</td><td>" ++ links(site, "dev", "dev/") ++ "</td></tr>\n" else "") ++
             tags.map(fn(t) "<tr><td class=\"v\">" ++ t ++ "</td><td></td><td>" ++ links(site, t, t ++ "/") ++ "</td></tr>\n").join
  write-text-file(site / "versions.html", page-start("Koka versions") ++ "<table>\n" ++ rows ++ "</table></body></html>\n")
  val versions = channels.filter(fn(c) is-directory(site / c / "doc")).map fn(c)
    "{\"name\": \"" ++ c ++ "\", \"doc\": \"" ++ c ++ "/doc/\", \"playground\": " ++
    (if is-file(site / c / "playground" / "index.html") then "true" else "false") ++ "}"
  write-text-file(site / "versions.json", "{\"stable\": \"" ++ stable ++ "\", \"versions\": [" ++ versions.join(", ") ++ "]}\n")

// the documentation and playground links of a channel directory, relative to `base`
fun links( site : path, dir : string, base : string ) : io string
  "<a href=\"" ++ base ++ "doc/index.html\">documentation</a>" ++
  (if is-file(site / dir / "playground" / "index.html") then " &middot; <a href=\"" ++ base ++ "playground/\">playground</a>" else "")

// the start of a generated page
fun page-start( title : string ) : string
  "<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">\n" ++
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n" ++
  "<title>" ++ title ++ "</title>\n" ++
  "<style>body{font-family:system-ui,sans-serif;max-width:40em;margin:2em auto;padding:0 1em;line-height:1.7}td{padding-right:1.5em}td.v{font-weight:600}</style>\n" ++
  "</head><body><h1>" ++ title ++ "</h1>\n"

// sorts release tags like `sort -V`: runs of digits compare as numbers, so v3.10.0 follows v3.9.0
fun version-sort( tags : list<string> ) : div list<string>
  fun insert( t : string, xs : list<string> ) : div list<string>
    match xs
      Cons(x,xx) -> if version-less(digit-runs(x.list), digit-runs(t.list)) then Cons(x, insert(t,xx)) else Cons(t,xs)
      Nil        -> [t]
  tags.foldl([], fn(acc,t) insert(t,acc))

fun version-less( xs : list<string>, ys : list<string> ) : bool
  match xs
    Nil -> !ys.is-nil
    Cons(x,xx) -> match ys
      Nil -> False
      Cons(y,yy) ->
        if x == y then version-less(xx,yy)
        else match (x.parse-int, y.parse-int)
          (Just(i), Just(j)) -> i < j
          _ -> x < y

// splits into alternating runs of digits and non-digits
fun digit-runs( cs : list<char> ) : div list<string>
  match cs
    Nil -> []
    Cons(c,_) ->
      val (run,rest) = cs.span(fn(d) d.is-digit == c.is-digit)
      Cons(run.string, digit-runs(rest))


fun file-replace-all( fname : path, pat : string, rep : string ) : io ()
  val content = read-text-file(fname)
  write-text-file( fname, content.replace-all(pat, rep) )


fun run-echo(cmd : string) : io ()
  // println("> " ++ cmd)
  val exit-code = run-system(cmd)
  if exit-code==0 then
    println("ok")
  else
    println("> " ++ cmd)
    throw("error: " ++ exit-code.show )
/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Parsing of command line flags.

For example:

```
struct myflags(
  verbose : bool = False,
  version : bool = False, 
  name : string = "",
  output : string = "",
  arguments : list<string> = []
)

val myflags : list<flag<myflags>>
  = [ Flag( "V?", ["version"], Bool(set-version),     "display version information" ),
      Flag( "v",  ["verbose"], Bool(set-verbose),     "verbosely list files"),
      Flag( "o",  ["output"],  Opt(set-output,"FILE"),"use FILE for dump" ),
      Flag( "n",  ["name"],    Req(set-name,"USER"),  "only show USER files" ),
    ]

fun set-name( t : myflags, name )  { t(name = name)
fun set-verbose( t : myflags, v )  { t(verbose = v)
fun set-version( t : myflags, v )  { t(version = v)
fun set-output( t : myflags, mbs : maybe<string> ) : myflags

  match(mbs)
    Nothing -> t(output = "stdout")
    Just(s) -> t(output = s)


pub fun test( cmdargs )

  val header = "usage:\n program [options] files\n\noptions:"
  // testflags.usageInfo( header ).println
  val (flags,args,errs) = parse( Myflags(), myflags, cmdargs )
  if (errs.is-nil)
    println( "\nsuccess!" );
    println( "flags: " ++ flags.show-flags)
    println( "arguments: " ++ args.join(" ") );
    if (flags.version) myflags.usage(header).println

  else
    println( errs.join("\n") ++ "\n" ++ myflags.usage(header) )


fun show-flags( o : myflags )
  "{" ++ ["verbose=" ++ o.verbose.show,
         "version=" ++ o.version.show,
         "name=" ++ o.name.show,
         "output=" ++ o.output.show,
         "arguments=" ++ o.arguments.join(",")].join(";") ++ "}"

```
\/
*/
module std/os/flags

import std/os/env

// ----------------------------------------------------
// Usage example
// ----------------------------------------------------

struct myflags(
  verbose : bool = False,
  version : bool = False, 
  name : string = "",
  output : string = "",
  arguments : list<string> = []
)

fun show-flags( o : myflags )
  "{" ++ ["verbose=" ++ o.verbose.show,
         "version=" ++ o.version.show,
         "name=" ++ o.name.show,
         "output=" ++ o.output.show,
         "arguments=" ++ o.arguments.join(",")].join(";") ++ "}"


fun set-name( t : myflags, name )  { t(name = name) }
fun set-verbose( t : myflags, v : bool) : myflags  { t(verbose = v) }
fun set-version( t : myflags, v )  { t(version = v) }
fun set-output( t : myflags, mbs : maybe<string> ) : myflags
  match(mbs)
    Nothing -> t(output = "stdout")
    Just(s) -> t(output = s)


val myflags : list<flag<myflags>>
  = [ Flag( "V?", ["version"], Bool(set-version),     "display version information" ),
      Flag( "v",  ["verbose"], Bool(set-verbose),     "verbosely list files"),
      Flag( "o",  ["output"],  Opt(set-output,"FILE"),"use FILE for dump" ),
      Flag( "n",  ["name"],    Req(set-name,"USER"),  "only show USER files" ),
    ]

pub fun test( cmdargs )
  val header = "usage:\n program [options] files\n\noptions:"
  // testflags.usageInfo( header ).println
  val (flags,args,errs) = parse( Myflags(), myflags, cmdargs )
  if errs.is-nil then
    println( "\nsuccess!" );
    println( "flags: " ++ flags.show-flags)
    println( "arguments: " ++ args.join(" ") );
    if flags.version then myflags.usage(header).println  
  else 
    println( errs.join("\n") ++ "\n" ++ myflags.usage(header) )  


// Specifies how to handle flags that follow non-flag command line arguments.
pub value type flag-order<a>
  // Allow flags to be permuted with non-flag arguments (default)
  Permute
  // flags following non-flag arguments are treated as arguments
  Preorder
  // Wrap each non-flag argument into an flag
  Wrap( wrap : (string) -> a )


// Specifies a single command line flag
// For example: `flag("h?",["help"],Bool(Help),"show help information")`.
pub struct flag<a>
  short-names : string
  long-names  : list<string>
  parser      : flag-parser<a>
  help        : string


// Specifies the argument of an flag
pub type flag-parser<a>
  // Boolean flag without an argument.
  // For a flag `foo` Automatically enables forms `--no-foo` and `--foo=true|false`.
  Bool( default : (a,bool) -> a)
  // A required argument.
  Req( parse : (a,string) -> a, help : string )
  // An flagal argument.
  Opt( parse : (a,maybe<string>) -> a, help : string )


// Return a nicely formatted string describing the usage of a command,
// consisting of a `header` followed by the descriptions of the `flags`.
// The default header is ``"usage:\n program [flags] arguments\n\nflags:"``.
pub fun usage( flags : list<flag<a>>, header : string = "usage:\n program [flags] arguments\n\nflags:" ) : string

  fun align-left( xs : list<string> )
    val n = xs.map(fn(s){ s.count }).maximum
    xs.map( fn(s){ s.pad-right(n) } )

  fun paste(x,y,z)
    (" " ++ x ++ "  " ++ y ++ "  " ++ z)

  val (shorts,longs,helps) = flags.map(show-flag).concat.unzip3
  val table = zipWith3( paste, shorts.align-left, longs.align-left, helps )
  (header ++ "\n" ++ table.unlines)

fun show-flag( flag : flag<a> ) : list<(string,string,string)>
  val short = flag.short-names.list.map(fn(c){ "-" ++ c.string ++ show-short-flag(flag.parser) }).join(" ")
  val long  = flag.long-names.map(fn(name){ "--" ++ name ++ show-long-flag(flag.parser) }).join(" ")
  match (lines(flag.help))
    Cons(hd,tl) -> [(short,long,hd)] ++ tl.map( fn(s){ ("","",s)}) 
    Nil         -> [(short,long,"")]


fun show-short-flag( parser : flag-parser<a> )
  match(parser)
    Bool -> ""
    Req( help=h ) -> "<" ++ h ++ ">"
    Opt( help=h ) -> "[" ++ h ++ "]"


fun show-long-flag( parser : flag-parser<a> )
  match(parser)
    Bool(_) -> ""
    Req( help=h ) -> "=" ++ h
    Opt( help=h ) -> "[=" ++ h ++ "]"


type flag-kind<a>
  Flg( set : a -> a )
  Arg( arg : string )
  End
  Unknown( arg : string )
  Error( msg : string )

// Parse the command line arguments `args` (see `std/env/get-args`)
// according to the flag descriptions `flags`. Takes an flagal argument
// `ordering` (=`Permute`) that specifies how optare handled that follow non-flag arguments.
// Returns three lists: the list of parsed flags,
// a list of non-flag arguments, and a list of potential error messages.
pub fun parse( initial : a, flags : list<flag<a>>, args : list<string>, ordering : flag-order<a> = Permute ) 
                        : total (a, list<string>, list<string>)

  var done := False  // done scanning flags? (the rest is treated as an argument)
  val opts = args.map fn(arg) 

    val opts = if (done) then [Arg(arg)] else process-next(arg,flags)
    opts.foreach fn(opt)
      match(opt)
        End -> done := True
        Arg(_) | ordering.is-preorder -> done := True
        _   -> ()

    opts

  opts.concat.foldl((initial,[],[])) fn(acc,opt)
    val (o,xs,errs) = acc
    match(opt)
      Flg(set)   -> (set(o),xs,errs)
      Unknown(e) -> (o,xs,Cons(error-unknown-message(e),errs))
      flags/Error(e)   -> (o,xs,Cons(e,errs))
      Arg(x)     -> (o,Cons(x,xs),errs)
      End        -> (o,xs,errs)



fun process-next( arg : string, flags : list<flag<a>> ) : list<flag-kind<a>>
  match(arg.starts-with("--"))
    Just(slice) -> if (slice.is-empty) then [End] else [parse-long(slice.string,flags)]      
    _ -> match(arg.starts-with("-"))
      Just(slice) | !slice.is-empty -> parse-shorts(slice.string,flags)
      _ -> [Arg(arg)]



fun break( s : string, c : string ) : (string,string)
  val parts = s.split(c,2)
  match (parts)
    Cons(x,Cons(y,_)) -> (x,y)
    _                 -> (s,"")


fun parse-long( s : string, flags : list<flag<a>> ) : total flag-kind<a>
  val (cflagname,flagarg) = s.break("=")
  val optstr   = "--" ++ s
  val flagname = cflagname.to-lower
  val (baseflagname,negated) = match (flagname.starts-with("no-"))
                                 Just(slice) -> (slice.string,True)
                                 _           -> (flagname,False)
  val exacts   = flags.filter fn(opt)    
                    opt.long-names.map(to-lower).any fn(name)
                      name==flagname || name==baseflagname
  val prefixes = flags.filter fn(opt) 
                    opt.long-names.map(to-lower).any fn(name)  
                      name.starts-with(flagname).bool || name.starts-with(baseflagname).bool 
  val applicable = if (exacts.is-nil) then prefixes else exacts  
  match(applicable)
    Nil -> error-unknown(optstr) 
    Cons(_,Cons(_,_)) -> error-ambiguous(applicable,optstr) 
    Cons(desc,_) ->
      match(desc.parser)
          Bool(set)
            -> if (flagarg == "")
                then Flg(fn(o){ set(o, !negated) })
               elif (flagarg.to-lower == "true")
                then Flg(fn(o) { set(o,True) })
               elif (flagarg.to-lower == "false")
                then Flg(fn(o) { set(o,False) })
                else error-noarg(optstr)
          Req(parse,help)
            -> if (negated)
                then error-negate(baseflagname)
               elif (flagarg.is-notempty)
                then Flg(fn(o) { parse(o,flagarg) })
                else error-required(help,optstr)
          Opt(parse)
            -> if (negated)
                then error-negate(baseflagname)
               elif (flagarg.is-notempty)
                then Flg(fn(o) { parse(o,Just(flagarg)) })
                else Flg(fn(o) { parse(o,Nothing) } )


  

fun parse-shorts( s : string,  flags : list<flag<a>> ) : list<flag-kind<a>>
  var done := False
  val fs = s.list.map-indexed fn(i,c)
    if (done) return Nothing
    val optstr = "-" ++ c.string
    val applicable = flags.filter( fn(opt){ opt.short-names.contains(c.string) } )
    match(applicable)
      Nil -> Just(error-unknown(optstr))
      Cons(_,Cons(_,_))   
        -> Just(error-ambiguous(applicable,optstr))
      Cons(desc,_)
        -> match(desc.parser)
              Bool(set)
                -> Just(Flg(fn(o){ set(o,True) }))
              Req(parse,help) ->
                val arg = s.first.advance(i).after.string
                if (arg.is-notempty)
                 then { done := True; Just(Flg(fn(o){ parse(o,arg) })) }
                 else Just(error-required(help,optstr))

              Opt(parse) ->
                val arg = s.first.advance(i).after.string
                if (arg.is-notempty )
                 then { done := True; Just(Flg(fn(o){ parse(o,Just(arg)) })) }
                 else Just(Flg(fn(o){parse(o,Nothing)}))


  fs.flatmap(list)

fun error-ambiguous( applicable, opt )
  val header = "flag \"" ++ opt ++ "\" is ambiguous. It could be one of:"
  Error( usage( applicable, header ) )

fun error-required( help, opt )
  Error( "flag \"" ++ opt ++ "\" requires an argument " ++ help)

fun error-negate( flagname )
  Error( "flag \"--" ++ flagname ++ "\" cannot be negated" );

fun error-noarg( opt )
  Error( "flag \"" ++ opt ++ "\" does not take an argument")

fun error-unknown( opt )
  Error( error-unknown-message(opt) )

fun error-unknown-message( opt )
  ( "unrecognized flag \"" ++ opt ++ "\"")


// Unzip a list of triples into three lists
fun unzip3( xs : list<(a,b,c)> ) : (list<a>,list<b>,list<c>)
  fun iter( ys, acc1, acc2, acc3 )
    match(ys)
      Cons((x,y,z),xx) -> iter(xx,Cons(x,acc1),Cons(y,acc2),Cons(z,acc3))
      Nil              -> (reverse(acc1),reverse(acc2),reverse(acc3))
  iter(xs,[],[],[])

fun zipWith3(f : (a,b,c) -> e d, xs : list<a>, ys : list<b>, zs : list<c> ) : e list<d>
  zipWith3Acc(f, [], xs, ys, zs )


fun zipWith3Acc( f, acc, xs, ys, zs )
  match(xs)
    Nil -> reverse(acc)
    Cons(x,xx) ->
      match(ys)
        Nil -> reverse(acc)
        Cons(y,yy) -> 
          match (zs)
            Cons(z,zz) -> zipWith3Acc(f, Cons( f(x,y,z), acc), xx, yy, zz)
            _          -> reverse(acc)



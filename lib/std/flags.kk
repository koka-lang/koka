/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/* Parsing of command line flags.

For example:

```
struct myoptions(
  verbose : bool = False,
  version : bool = False, 
  name : string = "",
  output : string = "",
  arguments : list<string> = []
)

val myoptions : list<option<myoptions>>
  = [ Option( "V?", ["version"], Flag(set-version),     "display version information" ),
      Option( "v",  ["verbose"], Flag(set-verbose),     "verbosely list files"),
      Option( "o",  ["output"],  Opt(set-output,"FILE"),"use FILE for dump" ),
      Option( "n",  ["name"],    Req(set-name,"USER"),  "only show USER files" ),
    ]

fun set-name( t : myoptions, name )  { t(name = name) }
fun set-verbose( t : myoptions, v )  { t(verbose = v) }
fun set-version( t : myoptions, v )  { t(version = v) }
fun set-output( t : myoptions, mbs : maybe<string> ) : myoptions
{
  match(mbs) {
    Nothing -> t(output = "stdout")
    Just(s) -> t(output = s)
  } 
}

fun test( cmdargs )
{
  val header = "usage:\n program [options] files\n\noptions:"
  // testOptions.usageInfo( header ).println
  val (options,args,errs) = parse( Myoptions(), myoptions, cmdargs )
  if (errs.nil?) {
    println( "\nsuccess!" );
    println( "options: " + options.show-options)
    println( "arguments: " + args.join(" ") );
    if (options.version) myoptions.usage(header).println
  }
  else {
    println( errs.join("\n") + "\n" + myoptions.usage(header) )
  }
}

fun show-options( o : myoptions ) {
  "{" + ["verbose=" + o.verbose.show,
         "version=" + o.version.show,
         "name=" + o.name.show,
         "output=" + o.output.show,
         "arguments=" + o.arguments.join(",")].join(";") + "}"
}
```
\/
*/
module std/flags

import std/env

// ----------------------------------------------------
// Usage example
// ----------------------------------------------------

struct myoptions(
  verbose : bool = False,
  version : bool = False, 
  name : string = "",
  output : string = "",
  arguments : list<string> = []
)

fun show-options( o : myoptions ) {
  "{" + ["verbose=" + o.verbose.show,
         "version=" + o.version.show,
         "name=" + o.name.show,
         "output=" + o.output.show,
         "arguments=" + o.arguments.join(",")].join(";") + "}"
}


val myoptions : list<option<myoptions>>
  = [ Option( "V?", ["version"], Flag(set-version),     "display version information" ),
      Option( "v",  ["verbose"], Flag(set-verbose),     "verbosely list files"),
      Option( "o",  ["output"],  Opt(set-output,"FILE"),"use FILE for dump" ),
      Option( "n",  ["name"],    Req(set-name,"USER"),  "only show USER files" ),
    ]

fun set-name( t : myoptions, name )  { t(name = name) }
fun set-verbose( t : myoptions, v )  { t(verbose = v) }
fun set-version( t : myoptions, v )  { t(version = v) }
fun set-output( t : myoptions, mbs : maybe<string> ) : myoptions
{
  match(mbs) {
    Nothing -> t(output = "stdout")
    Just(s) -> t(output = s)
  } 
}

fun test( cmdargs )
{
  val header = "usage:\n program [options] files\n\noptions:"
  // testOptions.usageInfo( header ).println
  val (options,args,errs) = parse( Myoptions(), myoptions, cmdargs )
  if (errs.nil?) {
    println( "\nsuccess!" );
    println( "options: " + options.show-options)
    println( "arguments: " + args.join(" ") );
    if (options.version) myoptions.usage(header).println
  }
  else {
    println( errs.join("\n") + "\n" + myoptions.usage(header) )
  }
}


// Specifies how to handle options that follow non-option command line arguments.
public type option-order<a> {
  // Allow options to be permuted with non-option arguments
  con Permute
  // Options following non-option arguments are treated as arguments
  con Preorder
  // Wrap each non-option argument into an option
  con Wrap( wrap : (string) -> a )
}


// Specifies a single option
// For example: `Option("h?",["help"],Flag(Help),"show help information")`.
public struct option<a>( 
  short-names : string,
  long-names  : list<string>,
  parser      : option-parser<a>,
  help        : string
)

// Specifies the argument of an option
public type option-parser<a> {
  // Boolean flag without an argument.
  // For a flag `foo` Automatically enables forms `--no-foo` and `--foo=true|false`.
  con Flag( default : (a,bool) -> a)
  // A required argument.
  con Req( parse : (a,string) -> a, help : string )
  // An optional argument.
  con Opt( parse : (a,maybe<string>) -> a, help : string )
}


// Return a nicely formatted string describing the usage of a command,
// consisting of a `header` followed by the descriptions of the `flags`.
// The default header is `"usage:\n program [options] arguments\n\noptions:"`.
public fun usage( options : list<option<a>>, header : string = "usage:\n program [options] arguments\n\noptions:" ) : string
{
  fun align-left( xs : list<string> )
  {
    val n = xs.map(fun(s){ s.count }).maximum
    xs.map( fun(s){ s.pad-right(n) } )
  }

  fun paste(x,y,z) {
    return (" " + x + "  " + y + "  " + z)
  }

  val (shorts,longs,helps) = options.map(show-option).concat.unzip3
  val table = zipWith3( paste, shorts.align-left, longs.align-left, helps )
  return (header + "\n" + table.unlines)
}

fun show-option( option : option<a> ) : list<(string,string,string)>
{
  val short = option.short-names.list.map(fun(c){ "-" + c.string + show-short-option(option.parser) }).join(" ")
  val long  = option.long-names.map(fun(name){ "--" + name + show-long-option(option.parser) }).join(" ")
  match (lines(option.help)) {
    Cons(hd,tl) -> [(short,long,hd)] + tl.map( fun(s){ ("","",s)}) 
    Nil         -> [(short,long,"")]
  }
}

fun show-short-option( parser : option-parser<a> )
{
  match(parser) {
    Flag -> ""
    Req( help=h ) -> "<" + h + ">"
    Opt( help=h ) -> "[" + h + "]"
  }
}

fun show-long-option( parser : option-parser<a> )
{
  match(parser) {
    Flag(_) -> ""
    Req( help=h ) -> "=" + h
    Opt( help=h ) -> "[=" + h + "]"
  }
}

type flag-kind<a>
{
  Flg( set : a -> a )
  Arg( arg : string )
  End
  Unknown( arg : string )
  Error( msg : string )
}

// Parse the command line arguments `args` (see `std/env/get-args`)
// according to the option descriptions `options`. Takes an optional argument
// `ordering` (=`Permute`) that specifies how optare handled that follow non-option arguments.
// Returns three lists: the list of parsed options,
// a list of non-option arguments, and a list of potential error messages.
public fun parse( initial : a, options : list<option<a>>, args : list<string>, ordering : option-order<a> = Permute ) 
                        : total (a, list<string>, list<string>)
{
  var done := False  // done scanning options? (the rest is treated as an argument)
  val opts = args.map fun(arg) 
  {
    val opts = if (done) then [Arg(arg)] else process-next(arg,options)
    opts.foreach fun(opt) {
      match(opt) {
        End -> done := True
        Arg(_) | ordering.preorder? -> done := True
        _   -> ()
      }      
    }
    opts
  }

  opts.concat.foldl((initial,[],[])) fun(acc,opt) {
    val (o,xs,errs) = acc
    match(opt) {
      Flg(set)   -> (set(o),xs,errs)
      Unknown(e) -> (o,xs,Cons(error-unknown-message(e),errs))
      Error(e)   -> (o,xs,Cons(e,errs))
      Arg(x)     -> (o,Cons(x,xs),errs)
      End        -> (o,xs,errs)
    }
  }
}


fun process-next( arg : string, options : list<option<a>> ) : list<flag-kind<a>>
{
  match(arg.starts-with("--")) {
    Just(slice) -> if (slice.empty?) then [End] else [parse-long(slice.string,options)]      
    _ -> match(arg.starts-with("-")) {
      Just(slice) | slice.notempty? -> parse-shorts(slice.string,options)
      _ -> [Arg(arg)]
    }
  }  
}


fun break( s : string, c : string ) : (string,string)
{
  val parts = s.split(c,2)
  match (parts) {
    Cons(x,Cons(y,_)) -> (x,y)
    _                 -> (s,"")
  }
}

fun parse-long( s : string, options : list<option<a>> ) : total flag-kind<a>
{
  val (cflagname,flagarg) = s.break("=")
  val optstr   = "--" + s
  val flagname = cflagname.to-lower
  val (baseflagname,negated) = match (flagname.starts-with("no-")) {
                                 Just(slice) -> (slice.string,True)
                                 _           -> (flagname,False)
                               }
  
  val exacts   = options.filter( fun(opt){ opt.long-names.map(to-lower).any( fun(name) { 
    name==flagname || name==baseflagname 
  })})
  val prefixes = options.filter( fun(opt){ opt.long-names.map(to-lower).any( fun(name) { 
    name.starts-with(flagname).bool || name.starts-with(baseflagname).bool 
  })})
  val applicable = if (exacts.nil?) then prefixes else exacts
  
  match(applicable) {
    Nil -> error-unknown(optstr) 
    Cons(_,Cons(_,_)) -> error-ambiguous(applicable,optstr) 
    Cons(desc,_) -> {
      match(desc.parser) {
          Flag(set)
            -> if (flagarg == "")
                then Flg(fun(o){ set(o, !negated) })
               elif (flagarg.to-lower == "true")
                then Flg(fun(o) { set(o,True) })
               elif (flagarg.to-lower == "false")
                then Flg(fun(o) { set(o,False) })
                else error-noarg(optstr)
          Req(parse,help)
            -> if (negated)
                then error-negate(baseflagname)
               elif (flagarg.notempty?)
                then Flg(fun(o) { parse(o,flagarg) })
                else error-required(help,optstr)
          Opt(parse)
            -> if (negated)
                then error-negate(baseflagname)
               elif (flagarg.notempty?)
                then Flg(fun(o) { parse(o,Just(flagarg)) })
                else Flg(fun(o) { parse(o,Nothing) } )
      }
    }        
  }  
}
  

fun parse-shorts( s : string,  options : list<option<a>> ) : list<flag-kind<a>>
{
  var done := False
  val fs = s.list.map-indexed fun(i,c) {
    if (done) then return Nothing
    val optstr = "-" + c.string
    val applicable = options.filter( fun(opt){ opt.short-names.contains(c.string) } )
    match(applicable) {
      Nil -> Just(error-unknown(optstr))
      Cons(_,Cons(_,_))   
        -> Just(error-ambiguous(applicable,optstr))
      Cons(desc,_)
        -> match(desc.parser) {
              Flag(set)
                -> Just(Flg(fun(o){ set(o,True) }))
              Req(parse,help) -> {
                val arg = s.first.advance(i).after.string
                if (arg.notempty?)
                 then { done := True; Just(Flg(fun(o){ parse(o,arg) })) }
                 else Just(error-required(help,optstr))
              }
              Opt(parse) -> {
                val arg = s.first.advance(i).after.string
                if (arg.notempty? )
                 then { done := True; Just(Flg(fun(o){ parse(o,Just(arg)) })) }
                 else Just(Flg(fun(o){parse(o,Nothing)}))
              }
          }
    }
  }    
  fs.flatmap(list)
}

fun error-ambiguous( applicable, opt ) {
  val header = "option \"" + opt + "\" is ambiguous. It could be one of:"
  return Error( usage( applicable, header ) )
}

fun error-required( help, opt ) {
  return Error( "option \"" + opt + "\" requires an argument " + help)
}

fun error-negate( flagname ) {
  return Error( "option \"--" + flagname + "\" cannot be negated" );
}

fun error-noarg( opt ) {
  return Error( "option \"" + opt + "\" does not take an argument")
}

fun error-unknown( opt ) {
  return Error( error-unknown-message(opt) )
}

fun error-unknown-message( opt ) {
  return ( "unrecognized option \"" + opt + "\"")
}



// Unzip a list of triples into three lists
fun unzip3( xs : list<(a,b,c)> ) : (list<a>,list<b>,list<c>)
{
  fun iter( ys, acc1, acc2, acc3 )
  {
    match(ys) {
      Cons((x,y,z),xx) -> iter(xx,Cons(x,acc1),Cons(y,acc2),Cons(z,acc3))
      Nil              -> (reverse(acc1),reverse(acc2),reverse(acc3))
    }
  }

  iter(xs,[],[],[])
}

fun zipWith3(f : (a,b,c) -> e d, xs : list<a>, ys : list<b>, zs : list<c> ) : e list<d>
{
  zipWith3Acc(f, [], xs, ys, zs )
}


fun zipWith3Acc( f, acc, xs, ys, zs )
{
  match(xs) {
    Nil -> reverse(acc)
    Cons(x,xx) ->
      match(ys) {
        Nil -> reverse(acc)
        Cons(y,yy) -> 
          match (zs) {
            Cons(z,zz) -> zipWith3Acc(f, Cons( f(x,y,z), acc), xx, yy, zz)
            _          -> reverse(acc)
          }
      }
  }
}

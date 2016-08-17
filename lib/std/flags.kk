/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/* Parsing of command line flags.
*/
module std/flags

import std/env

// Specifies how to handle options that follow nonOption command line arguments.
public type optionOrder<a> {
  // Allow options to be permuted with nonFlag arguments
  con Permute
  // Flags following nonOption arguments are treated as arguments
  con Preorder
  // Wrap each nonOption argument into an option
  con Wrap( wrap : (string) -> a )
}


// Specifies a single option
// For example: `Option("h?",["help"],Flag(Help),"show help information")`.
public struct option<a>( 
  shortNames : string,
  longNames  : list<string>,
  arg        : optionArg<a>,
  help       : string,
  llongNames : list<string> = longNames.map(to-lower)
)

// Specifies the argument of an option
public type optionArg<a> {
  // Boolean flag without an argument.
  // For a flag `foo` Automatically enables forms `--no-foo` and `--foo=true|false`.
  con Flag( default : (a,bool) -> a)
  // A required argument.
  con Req( parse : (a,string) -> a, help : string )
  // An optional argument.
  con Opt( parse : (a,maybe<string>) -> a, help : string )
}



struct testOptions(
  verbose : bool = False,
  version : bool = False, 
  name : string = "",
  output : string = "",
  arguments : list<string> = []
)

fun showOptions( o : testOptions ) {
  "{" + ["verbose=" + o.verbose.show,
         "version=" + o.version.show,
         "name=" + o.name.show,
         "output=" + o.output.show,
         "arguments=" + o.arguments.join(",")].join(";") + "}"
}

fun setOutput( t : testOptions, mbs : maybe<string> ) : testOptions
{
  match(mbs) {
    Nothing -> t(output = "stdout")
    Just(s) -> t(output = s)
  } 
}
fun setName( t : testOptions, name )  -> t(name = name)
fun setVerbose( t : testOptions, v )  -> t(verbose = v)
fun setVersion( t : testOptions, v )  -> t(version = v)

val testOptions : list<option<testOptions>>
  = [ Option( "V?", ["version"], Flag(setVersion),     "display version information" ),
      Option( "v",  ["verbose"], Flag(setVerbose),     "verbosely list files"),
      Option( "o",  ["output"],  Opt(setOutput,"FILE"),"use FILE for dump" ),
      Option( "n",  ["name"],    Req(setName,"USER"),  "only show USER files" ),
    ]


public fun test( cmdargs )
{
  val header = "usage:\n program [options] files\n\noptions:"
  // testOptions.usageInfo( header ).println
  val (options,args,errs) = parse( TestOptions(), testOptions, cmdargs )
  if (errs.nil?) {
    println( "\nsuccess!" );
    println( "options: " + options.showOptions)
    println( "arguments: " + args.join(" ") );
    if (options.version) testOptions.usageInfo(header).println
  }
  else {
    println( errs.join("\n") + "\n" + testOptions.usageInfo(header) )
  }
}

// Return a nicely formatted string describing the usage of a command,
// consisting of a "header" followed by the descriptions of the "flags".
public fun usageInfo( flags : list<option<a>>, header : string ) : string
{
  fun alignLeft( xs : list<string> )
  {
    val n = xs.map(fun(s){ s.count }).maximum
    xs.map( fun(s){ s.pad-right(n) } )
  }

  fun paste(x,y,z) {
    return (" " + x + "  " + y + "  " + z)
  }

  val (shorts,longs,helps) = flags.map(showFlag).concat.unzip3
  val table = zipWith3( paste, shorts.alignLeft, longs.alignLeft, helps )
  return (header + "\n" + table.unlines)
}

fun showFlag( flag : option<a> ) : list<(string,string,string)>
{
  val short = flag.shortNames.list.map(fun(c){ "-" + c.string + showShortArg(flag.arg) }).join(" ")
  val long  = flag.longNames.map(fun(name){ "--" + name + showLongArg(flag.arg) }).join(" ")
  match (lines(flag.help)) {
    Cons(hd,tl) -> [(short,long,hd)] + tl.map( fun(s){ ("","",s)}) 
    Nil         -> [(short,long,"")]
  }
}

fun showShortArg( arg : optionArg<a> )
{
  match(arg) {
    Flag -> ""
    Req( help=h ) -> "<" + h + ">"
    Opt( help=h ) -> "[" + h + "]"
  }
}

fun showLongArg( arg : optionArg<a> )
{
  match(arg) {
    Flag(_) -> ""
    Req( help=h ) -> "=" + h
    Opt( help=h ) -> "[=" + h + "]"
  }
}

type flagKind<a>
{
  Flg( set : a -> a )
  Arg( arg : string )
  End
  Unknown( arg : string )
  Error( msg : string )
}

// Parse the command line arguments "args" (see "std/env/argv")
// according to the flag descriptions "flags". Takes an optional argument
// "ordering" that specifies how flags are handled that follow nonFlag arguments.
// The default ordering is "Permute". Returns three lists: the list of parsed options,
// a list of nonOption arguments, and a list of potential error messages.
public fun parse( initial : a, flags : list<option<a>>, args : list<string>, ordering : optionOrder<a> = Permute ) 
                        : total (a, list<string>, list<string>)
{
  var done := False  // done scanning flags? (the rest is treated as an argument)
  val opts = args.map fun(arg) 
  {
    val opts = if (done) then [Arg(arg)] else processNext(arg,flags)
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
      Unknown(e) -> (o,xs,Cons(errorUnknownMessage(e),errs))
      Error(e)   -> (o,xs,Cons(e,errs))
      Arg(x)     -> (o,Cons(x,xs),errs)
      End        -> (o,xs,errs)
    }
  }
}


fun processNext( arg : string, flags : list<option<a>> ) : list<flagKind<a>>
{
  match(arg.starts-with("--")) {
    Just(slice) -> if (slice.empty?) then [End] else [parseLong(slice.string,flags)]      
    _ -> match(arg.starts-with("-")) {
      Just(slice) | slice.notempty? -> parseShorts(slice.string,flags)
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

fun parseLong( s : string, flags : list<option<a>> ) : total flagKind<a>
{
  val (cflagname,flagarg) = s.break("=")
  val opt = "--" + s
  val flagname     = cflagname.to-lower
  val (baseflagname,negated) = match (flagname.starts-with("no-")) {
                                 Just(slice) -> (slice.string,True)
                                 _           -> (flagname,False)
                               }
  
  val lnames   = flags.flatmap(fun(flag) { flag.llongNames } )
  val exacts   = flags.filter( fun(flag){ flag.llongNames.any( fun(name){ name==flagname || name==baseflagname } ) } )
  val prefixes = flags.filter( fun(flag){ flag.llongNames.any( fun(name){ name.starts-with(flagname).bool || name.starts-with(baseflagname).bool } ) } )
  val applicable = if (exacts.nil?) then prefixes else exacts
  
  match(applicable) {
    Nil -> errorUnknown(opt) 
    Cons(_,Cons(_,_)) -> errorAmbiguous(applicable,opt) 
    Cons(desc,_) -> {
      match(desc.arg) {
          Flag(set)
            -> if (flagarg == "")
                then Flg(fun(o){ set(o, !negated) })
               elif (flagarg.to-lower == "true")
                then Flg(fun(o) { set(o,True) })
               elif (flagarg.to-lower == "false")
                then Flg(fun(o) { set(o,False) })
                else errorNoarg(opt)
          Req(parse,help)
            -> if (negated)
                then errorNegate(baseflagname)
               elif (flagarg.notempty?)
                then Flg(fun(o) { parse(o,flagarg) })
                else errorRequired(help,opt)
          Opt(parse)
            -> if (negated)
                then errorNegate(baseflagname)
               elif (flagarg.notempty?)
                then Flg(fun(o) { parse(o,Just(flagarg)) })
                else Flg(fun(o) { parse(o,Nothing) } )
      }
    }        
  }  
}
  

fun parseShorts( s : string,  flags : list<option<a>> ) : list<flagKind<a>>
{
  var done := False
  val fs = s.list.map-indexed fun(i,c) {
    if (done) then return Nothing
    val opt = "-" + c.string
    val applicable = flags.filter( fun(flag){ flag.shortNames.contains(c.string) } )
    match(applicable) {
      Nil -> Just(errorUnknown(opt))
      Cons(_,Cons(_,_))   
        -> Just(errorAmbiguous(applicable,opt))
      Cons(desc,_)
        -> match(desc.arg) {
              Flag(set)
                -> Just(Flg(fun(o){ set(o,True) }))
              Req(parse,help) -> {
                val arg = s.first.advance(i).after.string
                if (arg.notempty?)
                 then { done := True; Just(Flg(fun(o){ parse(o,arg) })) }
                 else Just(errorRequired(help,opt))
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
  fs.map(list).concat
}

fun errorAmbiguous( applicable, opt ) {
  val header = "option \"" + opt + "\" is ambiguous. It could be one of:"
  return Error( usageInfo( applicable, header ) )
}

fun errorRequired( help, opt ) {
  return Error( "option \"" + opt + "\" requires an argument " + help)
}

fun errorNegate( flagname ) {
  return Error( "option \"--" + flagname + "\" cannot be negated" );
}

fun errorNoarg( opt ) {
  return Error( "option \"" + opt + "\" does not take an argument")
}

fun errorUnknown( opt ) {
  return Error( errorUnknownMessage(opt) )
}

fun errorUnknownMessage( opt ) {
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

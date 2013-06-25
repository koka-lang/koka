/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/*
Parsing of command line flags.
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
// For example: "Option(""h?"",[""help""],Flag(Help),""show help information"")".
public struct option<a>( 
  shortNames : string,
  longNames  : list<string>,
  arg         : optionArg<a>,
  help        : string
)

// Specifies the argument of an option
public type optionArg<a> {
  // Boolean flag without an argument.
  con Flag( default : a)
  // A required argument.
  con Req( parse : string -> a, help : string )
  // An optional argument.
  con Opt( parse : maybe<string> -> a, help : string )
}



type testOptions {
  Verbose
  Version
  Name(s : string )
  Output( s : string )
  Argument( s : string )
}

function showOptions( o : testOptions ) {
  match(o) {
    Verbose -> "verbose"
    Version -> "version"
    Name(s) -> "name: " + show(s)
    Output(s) -> "output: " + show(s)
    Argument(s) -> "arg: " + show(s)
  }
}

fun out( mbs : maybe<string> ) : testOptions
{
  match(mbs) {
    Nothing -> Output("stdout")
    Just(s) -> Output(s)
  } 
}

val testOptions
  = [ Option( "V?", ["version"], Flag(Version),     "display version information" ),
      Option( "v",  ["verbose"], Flag(Verbose),     "verbosely list files"),
      Option( "o",  ["output"],  Opt(out,"FILE"),   "use FILE for dump" ),
      Option( "n",  ["name"],    Req(Name,"USER"),  "only show USER files" ),
    ]

public function test( cmdargs )
{
  header = "usage:\n program [options] files\n\noptions:"
  // testOptions.usageInfo( header ).println
  val (options,args,errs) = parse( testOptions, cmdargs )
  if (errs.isNil) {
    println( "\nsuccess!" );
    println( "options: " + options.map(showOptions).join(", "))
    println( "arguments: " + args.join(" ") );
    if (options.any(isVersion)) testOptions.usageInfo(header).println
  }
  else {
    println( errs.join("\n") + "\n" + testOptions.usageInfo(header) )
  }
}

// Return a nicely formatted string describing the usage of a command,
// consisting of a "header" followed by the descriptions of the "flags".
public function usageInfo( flags : list<option<a>>, header : string ) : string
{
  function flushLeft(s : string, width : int ) {
    if (s.length >= width) return s
    return (s + string(width - s.length,' '))
  }

  function alignLeft( xs : list<string> )
  {
    val n = xs.map(fun(s){ s.length }).maximum
    xs.map( fun(s){ s.flushLeft(n) } )
  }

  function paste(x,y,z) {
    return (" " + x + "  " + y + "  " + z)
  }

  val (shorts,longs,helps) = flags.map(showFlag).concat.unzip3
  val table = zipWith3( paste, shorts.alignLeft, longs.alignLeft, helps )
  return (header + "\n" + table.vector.unlines)
}

function showFlag( flag : option<a> ) :  list<(string,string,string)>
{
  val short = flag.shortNames.list.map(fun(c){ "-" + c.string + showShortArg(flag.arg) }).join(" ")
  long  = flag.longNames.map(fun(name){ "--" + name + showLongArg(flag.arg) }).join(" ")
  match (lines(flag.help).list) {
    Cons(hd,tl) -> [(short,long,hd)] + tl.map( fun(s){ ("","",s)}) 
    Nil         -> [(short,long,"")]
  }
}

function showShortArg( arg : optionArg<a> )
{
  match(arg) {
    Flag -> ""
    Req( help=h ) -> "<" + h + ">"
    Opt( help=h ) -> "[" + h + "]"
  }
}

function showLongArg( arg : optionArg<a> )
{
  match(arg) {
    Flag(_) -> ""
    Req( help=h ) -> "=" + h
    Opt( help=h ) -> "[=" + h + "]"
  }
}

type flagKind<a>
{
  Flg( value : a )
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
public function parse( flags : list<option<a>>, args : list<string>, ordering : optionOrder<a> = Permute ) 
                        : total (list<a>, list<string>, list<string>)
{
  var done := False  // done scanning flags? (the rest is treated as an argument)

  val opts = args.map fun(arg) 
  {
    val opts = if (done) then [Arg(arg)] else processNext(arg,flags)
    opts.foreach fun(opt) {
      match(opt) {
        End -> done := True
        Arg(_) | ordering.isPreorder -> done := True
        _   -> ()
      }      
    }
    opts
  }

  opts.concat.foldr(([],[],[])) fun(opt,acc) {
    val (fs,xs,errs) = acc
    match(opt) {
      Flg(f)     -> (Cons(f,fs),xs,errs)
      Unknown(e) -> (fs,xs,Cons(errorUnknownMessage(e),errs))
      Error(e)   -> (fs,xs,Cons(e,errs))
      Arg(x)     -> (fs,Cons(x,xs),errs)
      End        -> (fs,xs,errs)
    }
  }
}


function processNext( arg : string, flags : list<option<a>> ) : list<flagKind<a>>
{
  if ("--" == arg)
   then [End]
  elif (arg.startsWith("--"))
   then [parseLong( arg.substr(2), flags )]
  elif (arg.startsWith("-") && arg.length >= 2)
   then parseShorts( arg.substr(1), flags )
   else [Arg(arg)]
}


function break( s : string, c : char ) : (string,string)
{
  parts = s.split(c.string,2).list
  match (parts) {
    Cons(x,Cons(y,_)) -> (x,y)
    _                 -> (s,"")
  }
}

function parseLong( s : string, flags : list<option<a>> ) : total flagKind<a>
{
  val (flagname,flagarg) = s.break('=')
  opt      = "--" + s
  exacts   = flags.filter( fun(flag){ flag.longNames.any( fun(name){ name == flagname } ) } )
  prefixes = flags.filter( fun(flag){ flag.longNames.any( fun(name){ name.startsWith(flagname) } ) } )
  applicable = if (exacts.isNil) then prefixes else exacts
  
  match(applicable) {
    Nil -> errorUnknown(opt) 
    Cons(_,Cons(_,_)) -> errorAmbiguous(applicable,opt) 
    Cons(desc,_) -> {
      match(desc.arg) {
          Flag(default)
            -> if (flagarg.length == 0)
                then Flg(default)
                else errorNoarg(opt)
          Req(parse,help)
            -> if (flagarg.length>0)
                then Flg(parse(flagarg))
                else errorRequired(help,opt)
          Opt(parse)
            -> if (flagarg.length>0)
                then Flg(parse(Just(flagarg)))
                else Flg(parse(Nothing))
      }
    }        
  }  
}
  

function parseShorts( s : string,  flags : list<option<a>> ) : list<flagKind<a>>
{
  var done := False
  val fs = s.list.mapIndexed fun(i,c) {
    if (done) then return Nothing
    opt = "-" + c.string
    applicable = flags.filter( fun(flag){ flag.shortNames.contains(c) } )
    match(applicable) {
      Nil -> Just(errorUnknown(opt))
      Cons(_,Cons(_,_))   
        -> Just(errorAmbiguous(applicable,opt))
      Cons(desc,_)
        -> match(desc.arg) {
              Flag(default)
                -> Just(Flg(default))
              Req(parse,help) -> {
                arg = s.substr(i+1)
                if (arg.length > 0)
                 then { done := True; Just(Flg(parse(arg))) }
                 else Just(errorRequired(help,opt))
              }
              Opt(parse) -> {
                arg = s.substr(i+1)
                if (arg.length > 0)
                 then { done := True; Just(Flg(parse(Just(arg)))) }
                 else Just(Flg(parse(Nothing)))
              }
          }
    }
  }    
  fs.map(list).concat
}

function errorAmbiguous( applicable, opt ) {
  header = "option \"" + opt + "\" is ambiguous. It could be one of:"
  return Error( usageInfo( applicable, header ) )
}

function errorRequired( help, opt ) {
  return Error( "option \"" + opt + "\" requires an argument " + help + "\n")
}

function errorUnknown( opt ) {
  return Error( errorUnknownMessage(opt) )
}

function errorUnknownMessage( opt ) {
  return ( "unrecognized option \"" + opt + "\"\n")
}

function errorNoarg( opt ) {
  return Error( "option \"" + opt + "\" does not take an argument")
}




// Unzip a list of triples into three lists
function unzip3( xs : list<(a,b,c)> ) : (list<a>,list<b>,list<c>)
{
  function iter( ys, acc1, acc2, acc3 )
  {
    match(ys) {
      Cons((x,y,z),xx) -> iter(xx,Cons(x,acc1),Cons(y,acc2),Cons(z,acc3))
      Nil              -> (reverse(acc1),reverse(acc2),reverse(acc3))
    }
  }

  iter(xs,[],[],[])
}

function zipWith3(f : (a,b,c) -> e d, xs : list<a>, ys : list<b>, zs : list<c> ) : e list<d>
{
  zipWith3Acc(f, [], xs, ys, zs )
}


function zipWith3Acc( f, acc, xs, ys, zs )
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

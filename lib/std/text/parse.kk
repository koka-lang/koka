/*---------------------------------------------------------------------------
  Copyright 2020, Daan Leijen Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Parser combinators
*/  
public module std/text/parse

alias parser<e,a> = () -> <parse|e> a

effect parse {
  fun satisfy( pred : sslice -> total maybe<(a,sslice)> ) : maybe<a>
  control fail(msg : string) : a
  control pick() : bool
}

type parse-error<a> {
  ParseOk( result: a, rest : sslice )
  ParseError( msg : string, rest : sslice )
}

fun maybe( perr : parse-error<a> ) : maybe<a> {
  perr.either.maybe
}

fun either( perr : parse-error<a> ) : either<string,a> {  
  match(perr) {
    ParseOk(x,_) -> Right(x)
    ParseError(msg,_) -> Left(msg)
  }
}

fun parse-eof( input : sslice, p : () -> <parse|e> a) : e parse-error<a> {
  parse(input) {
    val x = p()
    eof()
    x
  }
}

fun parse( input0 : sslice, p : () -> <parse|e> a ) : e parse-error<a> {
  var input := input0
  with {
    return x          { ParseOk(x,input) }
    control fail(msg) { ParseError(msg,input) }
    fun satisfy(pred) {
      val inp = input
      val m = fun() : total _ { pred(inp) }
      match(m()) {
        Just((x,cap)) { input := cap; Just(x) }
        Nothing       { Nothing }
      }
    }
    control pick() {
      match(resume(True)) {
        ParseOk(x1,rest1) { ParseOk(x1,rest1) }
        err1 -> match(resume(False)) {  // todo: limit lookahead?
          ParseOk(x2,rest2) { ParseOk(x2,rest2) }
          _err2 -> err1  // todo: merge or pick closest?
        }
      }
    }    
  }  
  mask<local>(p)
}


fun (||)( p1 : parser<e,a>, p2 : parser<e,a> ) : <parse|e> a {
  if (pick()) then p1() else p2()
}

fun choose( ps : list<parser<e,a>> ) : <parse|e> a {
  match(ps) {
    Nil -> fail("no further alternatives")
    Cons(p,Nil) -> p()
    Cons(p,pp)  -> if (pick()) then p() else choose(pp)
  }
}

fun satisfy-fail( msg : string, pred : sslice -> maybe<(a,sslice)> ) : parse a {
  match(satisfy(pred)) {
    Nothing -> fail(msg)
    Just(x) -> x
  }
}

fun eof() : parse () {
  match(satisfy(fun(s){ if (s.empty?) then Just(((),s)) else Nothing })) {
    Nothing -> fail("expecting end-of-input")
    Just    -> ()
  }
}


fun char-is( msg :string, pred : char -> bool ) : parse char {
  satisfy-fail(msg) fun(slice) {
    match(slice.next) {
      Just((c,rest)) | pred(c) -> Just((c,rest))
      _ -> Nothing
    } 
  }
}

private fun next-while( slice : sslice, pred : char -> bool, acc : list<char> ) : (list<char>,sslice) {
  match(slice.next) {
    Just((c,rest)) | pred(c) -> next-while(unsafe-decreasing(rest), pred, Cons(c,acc) )
    _ -> (acc.reverse,slice)
  }
}

fun chars-are( msg :string, pred : char -> bool ) : parse list<char> {
  satisfy-fail(msg) fun(slice) { 
    match( slice.next-while(pred,[]) ) {
      ([],_) -> Nothing
      (xs,rest) -> Just((xs,rest))
    }
  }  
}

private fun next-match( slice : sslice, cs : list<char> ) : maybe<sslice> {
  match(cs) {
    Nil -> Just(slice)
    Cons(c,cc) -> match (slice.next) {
      Just((d,rest)) | c==d -> rest.next-match( cc )
      _ -> Nothing
    }
  }
}

fun pstring( s : string ) : parse string {
  satisfy-fail(s) fun(slice) {
    match (slice.next-match(s.list)) {
      Just(rest) -> Just((s,rest))
      Nothing    -> Nothing
    }
  }  
}

fun char( c : char ) : parse char {
  char-is( show(c), fun(c0){ c == c0 } )
}

fun digit() : parse int {
  val c = char-is("digit", digit?)
  (c - '0').int
}

fun digits() : parse string {
  chars-are("digit", digit? ).string
}

fun sign() : parse bool {
  val c = one-of-or("+-", '+')
  (c=='-')
}


fun pint() : parse int {
  val neg = sign()
  val i = digits().parse-int-default(0)
  if (neg) then ~i else i
}

fun optional( default : a, p : parser<e,a> ) : <parse|e> a {
  (p || ({ default }))
}

fun one-of( chars : string ) : parse char {
  chars.list.map(fun(c){ (fun(){ char(c) } ) }).choose
}

fun one-of-or( chars : string, default : char ) : parse char {
  optional( default, { one-of(chars) } )
}
//private val rx-float	= regex(@"^([\-\+])?(\d+(?:\.\d+)?(?:[eE][\-\+]\d+)?)$")
//private val rx-hexfloat	= regex(@"^([\-\+]?0[xX][0-9a-fA-F]+)(?:\.([0-9a-fA-F]+))?(?:[pP]([\-\+]?\d+))?$")

fun hex-digits() : parse string {
  chars-are("digit", hex-digit? ).string
}

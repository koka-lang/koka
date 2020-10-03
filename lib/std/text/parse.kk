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
  fun current-input() : sslice
}

fun starts-with( s : string, p : () -> parse a )  : maybe<(a,sslice)> {
  match (s.slice.parse(p)) {
    ParseOk(x,rest) -> Just((x,rest))
    _               -> Nothing
  }
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
    return x            { ParseOk(x,input) }
    fun current-input() { input }
    control fail(msg)   { ParseError(msg,input) }
    fun satisfy(pred) {
      val inp = input
      val m = fun() : total _ { pred(inp) }
      match(m()) {
        Just((x,cap)) { input := cap; Just(x) }
        Nothing       { Nothing }
      }
    }
    control pick() {
      val save = input;
      match(resume(True)) {
        ParseOk(x1,rest1) { ParseOk(x1,rest1) }
        err1 -> {
          input := save
          match(resume(False)) {  // todo: limit lookahead?
            ParseOk(x2,rest2) { ParseOk(x2,rest2) }
            _err2 -> err1  // todo: merge or pick closest?
          }
        }
      }
    }    
  }  
  mask<local>(p)
}


fun (||)( p1 : parser<e,a>, p2 : parser<e,a> ) : <parse|e> a {
  if (pick()) then p1() else p2()
}

fun optional( default : a, p : parser<e,a> ) : <parse|e> a {
  (p || ({ default }))
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
  match(satisfy(fun(s){ if (s.is-empty) then Just(((),s)) else Nothing })) {
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

private fun next-while0( slice : sslice, pred : char -> bool, acc : list<char> ) : (list<char>,sslice) {
  match(slice.next) {
    Just((c,rest)) | pred(c) -> next-while0(unsafe-decreasing(rest), pred, Cons(c,acc) )
    _ -> (acc.reverse,slice)
  }
}


fun chars-are( msg :string, pred : char -> bool ) : parse list<char> {
  satisfy-fail(msg) fun(slice) { 
    match( slice.next-while0(pred,[]) ) {
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

fun no-digit() : parse char {
  char-is("not a digit", fun(c){ !c.is-digit })
}

fun digit() : parse int {
  val c = char-is("digit", is-digit)
  (c - '0').int
}

fun alpha() : parse char {
  char-is("alpha", is-alpha)
}

fun alpha-num() : parse char {
  char-is("alpha-num", is-alpha-num)
}

fun white() : parse char {
  char-is("", is-white)
}

fun whitespace() : parse string {
  chars-are("", is-white).string  
}

fun whitespace0() : parse string {
  optional("", whitespace)
}

fun digits() : parse string {
  chars-are("digit", is-digit ).string
}

fun digits0() : parse string {
  optional("0", digits)
}

fun sign() : parse bool {
  val c = one-of-or("+-", '+')
  (c=='-')
}

fun pnat() : parse int {
  digits().parse-int-default(0)
}

fun pint() : parse int {
  val neg = sign()
  val i = pnat()
  if (neg) then ~i else i
}


fun none-of( chars : string ) : parse char {
  char-is("", fun(c){ !chars.contains(c.string)  })
}

fun none-of-many1( chars : string ) : parse string {
  chars-are("", fun(c){ !chars.contains(c.string)  }).string
}

fun one-of( chars : string ) : parse char {
  char-is(chars, fun(c){ chars.contains(c.string)  })
  // chars.list.map(fun(c){ (fun(){ char(c) } ) }).choose
}

private fun many-acc( p : parser<e,a>, acc : list<a> ) : <parse|e> list<a> {
  ({ val x = p(); many-acc(unsafe-decreasing(p),Cons(x,acc)) }) || ({ acc.reverse })
}

fun many( p : parser<e,a> ) : <parse|e> list<a> {
  many-acc(p,[])
}

fun many1( p : parser<e,a> ) : <parse|e> list<a> {
  Cons(p(), many(p))
}

private fun count-acc( n : int, acc : list<a>, p : parser<e,a> ) : <parse|e> list<a> {
  if (n <= 0) then acc.reverse else {
    val x = p()
    count-acc(unsafe-decreasing(n - 1), Cons(x,acc), p)
  }  
}

fun count( n : int, p : parser<e,a> ) : <parse|e> list<a> {
  count-acc(n, [], p)
}

fun one-of-or( chars : string, default : char ) : parse char {
  optional( default){ one-of(chars) }
}
//private val rx-float	= regex(@"^([\-\+])?(\d+(?:\.\d+)?(?:[eE][\-\+]\d+)?)$")
//private val rx-hexfloat	= regex(@"^([\-\+]?0[xX][0-9a-fA-F]+)(?:\.([0-9a-fA-F]+))?(?:[pP]([\-\+]?\d+))?$")

fun hex-digits() : parse string {
  chars-are("digit", is-hex-digit ).string
}

/*
fun test(s : string, x : a, p : () -> parse a ) : a {
  parse-eof(s.slice,p).maybe.default(x)
}
*/

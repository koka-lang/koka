/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Parser combinators
*/  
module std/text/parse

pub alias parser<e,a> = () -> <parse|e> a

pub effect parse
  fun satisfy( pred : sslice -> total maybe<(a,sslice)> ) : maybe<a>
  ctl fail(msg : string) : a
  ctl pick() : bool
  fun current-input() : sslice

pub fun starts-with( s : string, p : () -> parse a )  : maybe<(a,sslice)> 
  match s.slice.parse(p)
    ParseOk(x,rest) -> Just((x,rest))
    _               -> Nothing
  

pub type parse-error<a> 
  ParseOk( result: a, rest : sslice )
  ParseError( msg : string, rest : sslice )

pub fun maybe( perr : parse-error<a> ) : maybe<a> 
  perr.either.maybe

pub fun either( perr : parse-error<a> ) : either<string,a> 
  match perr 
    ParseOk(x,_) -> Right(x)
    ParseError(msg,_) -> Left(msg)
  
pub fun parse-eof( input : sslice, p : () -> <parse|e> a) : e parse-error<a> 
  parse(input)
    val x = p()
    eof()
    x

pub fun parse( input0 : sslice, p : () -> <parse|e> a ) : e parse-error<a> 
  var input := input0
  handle p 
    return(x)
      ParseOk(x,input) 

    fun current-input()  
      input 

    brk fail(msg)     
      ParseError(msg,input) 

    fun satisfy(pred) 
      val inp = input
      val m   = fn() : total _ { pred(inp) }
      match m() 
        Just((x,cap)) -> { input := cap; Just(x) }
        Nothing       -> Nothing

    ctl pick() 
      val save = input;
      match resume(True)
        ParseOk(x1,rest1) -> ParseOk(x1,rest1)
        err1 -> 
          input := save
          match resume(False)   // todo: limit lookahead?
            ParseOk(x2,rest2) -> ParseOk(x2,rest2) 
            _err2 -> err1  // todo: merge or pick closest?


pub fun (||)( p1 : parser<e,a>, p2 : parser<e,a> ) : <parse|e> a 
  if pick() then p1() else p2()

pub fun optional( default : a, p : parser<e,a> ) : <parse|e> a 
  p || (fn() default)

pub fun choose( ps : list<parser<e,a>> ) : <parse|e> a 
  match ps
    Nil         -> fail("no further alternatives")
    Cons(p,Nil) -> p()
    Cons(p,pp)  -> if pick() then p() else choose(pp)
  
pub fun satisfy-fail( msg : string, pred : sslice -> maybe<(a,sslice)> ) : parse a 
  match satisfy(pred)
    Nothing -> fail(msg)
    Just(x) -> x
  
pub fun eof() : parse () 
  match satisfy( fn(s) if s.is-empty then Just(((),s)) else Nothing ) 
    Nothing -> fail("expecting end-of-input")
    Just    -> ()
  
pub fun char-is( msg :string, pred : char -> bool ) : parse char 
  satisfy-fail(msg) fn(slice) 
    match slice.next
      Just((c,rest)) | pred(c) -> Just((c,rest))
      _ -> Nothing
    
  
fun next-while0( slice : sslice, pred : char -> bool, acc : list<char> ) : (list<char>,sslice) 
  match slice.next
    Just((c,rest)) | pred(c) -> next-while0(unsafe-decreasing(rest), pred, Cons(c,acc) )
    _ -> (acc.reverse,slice)
  

pub fun chars-are( msg :string, pred : char -> bool ) : parse list<char> 
  satisfy-fail(msg) fn(slice) 
    match slice.next-while0(pred,[]) 
      ([],_)    -> Nothing
      (xs,rest) -> Just((xs,rest))
    
fun next-match( slice : sslice, cs : list<char> ) : maybe<sslice> 
  match cs
    Nil -> Just(slice)
    Cons(c,cc) -> match slice.next 
      Just((d,rest)) | c==d -> rest.next-match( cc )
      _ -> Nothing
    
pub fun pstring( s : string ) : parse string 
  satisfy-fail(s) fn(slice) 
    match slice.next-match(s.list)
      Just(rest) -> Just((s,rest))
      Nothing    -> Nothing

pub fun char( c : char ) : parse char 
  char-is( show(c), fn(c0) c == c0  )

pub fun no-digit() : parse char 
  char-is("not a digit", fn(c) !c.is-digit )

pub fun digit() : parse int 
  val c = char-is("digit", is-digit)
  (c - '0').int

pub fun alpha() : parse char 
  char-is("alpha", is-alpha)

pub fun alpha-num() : parse char 
  char-is("alpha-num", is-alpha-num)

pub fun white() : parse char 
  char-is("", is-white)

pub fun whitespace() : parse string 
  chars-are("", is-white).string  

pub fun whitespace0() : parse string 
  optional("", whitespace)

pub fun digits() : parse string 
  chars-are("digit", is-digit ).string

pub fun digits0() : parse string 
  optional("0", digits)

pub fun sign() : parse bool 
  val c = one-of-or("+-", '+')
  (c=='-')

pub fun pnat() : parse int 
  digits().parse-int-default(0)

pub fun pint() : parse int 
  val neg = sign()
  val i = pnat()
  if neg then ~i else i

pub fun none-of( chars : string ) : parse char 
  char-is("", fn(c) !chars.contains(c.string) )

pub fun none-of-many1( chars : string ) : parse string 
  chars-are("", fn(c) !chars.contains(c.string)).string

pub fun one-of( chars : string ) : parse char 
  char-is(chars, fn(c) chars.contains(c.string))
  // chars.list.map(fn(c){ (fn(){ char(c) } ) }).choose

fun many-acc( p : parser<e,a>, acc : list<a> ) : <parse|e> list<a> 
  (fn() val x = p() in many-acc(unsafe-decreasing(p),Cons(x,acc))) || (fn() acc.reverse)

pub fun many( p : parser<e,a> ) : <parse|e> list<a> 
  many-acc(p,[])

pub fun many1( p : parser<e,a> ) : <parse|e> list<a> 
  Cons(p(), many(p))

fun count-acc( n : int, acc : list<a>, p : parser<e,a> ) : <parse|e> list<a> 
  if n <= 0 then acc.reverse else 
    val x = p()
    count-acc(unsafe-decreasing(n - 1), Cons(x,acc), p)
  

pub fun count( n : int, p : parser<e,a> ) : <parse|e> list<a> 
  count-acc(n, [], p)

pub fun one-of-or( chars : string, default : char ) : parse char 
  optional(default){ one-of(chars) }

//val rx-float	= regex(r"^([\-\+])?(\d+(?:\.\d+)?(?:[eE][\-\+]\d+)?)$")
//val rx-hexfloat	= regex(r"^([\-\+]?0[xX][0-9a-fA-F]+)(?:\.([0-9a-fA-F]+))?(?:[pP]([\-\+]?\d+))?$")

pub fun hex-digits() : parse string 
  chars-are("digit", is-hex-digit ).string

/*
pub fun test(s : string, x : a, p : () -> parse a ) : a 
  parse-eof(s.slice,p).maybe.default(x)
*/


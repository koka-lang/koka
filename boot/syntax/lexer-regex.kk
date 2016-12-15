module syntax.lexer

import text.regex

public struct lexeme( token :token, range :range )

public type token {
  TokenInt( int :int )
  TokenId( name :name )
  TokenCon( name :name )
  TokenKeyword( name : string, doc: string )
  TokenComment( content :string)
  TokenWhite( content :string)
  TokenError( content :string, message :string )
}

public struct range( 
  source     :string = "", 
  start-line :int = 0, 
  end-line   :int = 0,
  start-col  :int = 1,
  end-col    :int = 1)

abstract struct name( id :string, mod :string = "" )

public function to-string(name) {
  return ((if (name.mod == "") then "" else name.mod + ".") + name.id)
}

public function show(name :name) : string {
  return name.to-string
}

public function show( lexeme :lexeme ) : string {
  return show(lexeme.range) + ": " + show(lexeme.token)
}

public function show( range :range ) :string {
  return "(" + range.start-line.show + "," + range.start-col.show + "-" +
               range.end-line.show + "," + range.end-col.show + ")";
}

public function show( token :token ) : string
{
  match(token) {
    TokenInt(i) -> show(i)
    TokenId(n)  -> show(n)
    TokenCon(c) -> show(c)
    TokenKeyword(k) -> "keyword " + k.show
    TokenComment(c) -> "comment " + c.show
    TokenWhite(c)   -> "white " + c.show
    TokenError(_,m) -> "error " + m.show
  }
}

public function lex( source : string ) : list<lexeme>
{
  return [Lexeme( TokenComment(source), Range() )]
}



public type list1<a> {
  Single( head: a )
  Cons1( head: a, tail :list1<a> )
}

function safe-tail( xs : list1<a> ) : list1<a> 
{
  match(xs) {
    Cons1(_,xx) -> xx
    _           -> xs
  }
}

function []( xs : list1<a>, i : int) : a
{
  match(xs) {
    Single(x)   -> x
    Cons1(x,xx) -> if (i <= 0) then x else xx[i-1]
  }
}



public type result<a> {
  Token( token :a )
  Continue( retain :string )
}

abstract rectype rule<a> {
  Rule( regex :regex, action : (lexer<a>,matched) -> result<a>, next : (lexer<a>,matched) -> exn lexer<a> = fun(l,m){ return l })
}

function rule( r: string, action : (lexer<a>,matched) -> result<a>, next : (lexer<a>,matched) -> exn lexer<a> = fun(l,m){ return l }) : exn rule<a>
{
  return Rule( regex("^" + r,multiline=True), action, next )
}

abstract struct state<a>( name :string, rules : list<rule<a>> )

val state-empty = State("",[])


abstract struct lexer<a>( def: list<state<a>>, states :list1<state<a>>, index :int, retain :string  )

function lexer( root: state<a>, others: list<state<a>> ) : lexer<a>
{
  return Lexer( Cons(root,others), Single(root), 0, "")
}

function top( lexer :lexer<a> ) : state<a>
{
  lexer.states.head
}

function pop( lexer :lexer<a> ) : lexer<a>
{
  lexer( states = lexer.states.safe-tail )
}

function push( lexer :lexer<a>, state-name :string ) : exn lexer<a>
{
  val Just(s) = lexer.def.find( fun(s) { s.name == state-name } )
  lexer( states = Cons1(s,lexer.states) )
}

function (==)( s1 : state<a>, s2 : state<a> ) : bool {
  return (s1.name == s2.name)
}

function find-match( input :string, index :int, rules : list<rule<a>> ) : maybe<(matched,rule<a>)>
{
  match(rules) {
    Nil -> Nothing
    Cons(rul,rest) -> {
      match(input.find(regex(rul), index)) {
        Nothing -> find-match( input, index, rest)
        Just(m) -> return Just((m,rul))
      }
    }
  }
}

public function tokenize( lexer :lexer<a>, input :string ) : pure (lexer<a>, a)
{
  match (find-match( input.substring(lexer.index), 0, lexer.states[0].rules)) {
    Nothing -> error("no rule matches: " + lexer.index.show + ", " + show(input.substring(lexer.index)))
    Just((m,rul)) -> {
      lexer1 = lexer( index = lexer.index + m.matched.length)
      m2     = m( matched = lexer1.retain + m.matched )
      lexer2 = (rul.next)(lexer1,m2)
      match((rul.action)(lexer1,m2)) {
        Token(x)    -> return (lexer2,x)
        Continue(s) -> tokenize( lexer2(retain = s), input )
      }  
    }
  }
}

public function tokenize-all( lexer :lexer<a>, input :string, acc :list<a> = [] ) : pure (lexer<a>, list<a>)
{
  val (lexer2,x) = tokenize(lexer,input)
  if (lexer2.index >= input.length) 
   then return (lexer2, Cons(x,acc).reverse)
   else tokenize-all( lexer2, input, Cons(x,acc) )
}

public fun mylexer() {
  val root = "root"
  val comment = "comment"
  
  val root-state = State(root,[
      rule(@"[a-z]([\w]|\-[a-zA-Z])+", fun(l,m) { Token(TokenId(Name(m.matched))) } ),
      rule(@"[ \r\n]+", fun(l,m) { Token(TokenWhite(m.matched)) } ),
      rule(@"/*", fun(l,m) { Continue(m.matched) }, fun(l,m) { l.push(comment) } ) 
  ])

  val comment-state = State(comment, [
      rule(@"[^/*]+", fun(l,m) { Continue(m.matched) } ),
      rule(@"/*",     fun(l,m) { Continue(m.matched) }, fun(l,m) { l.pop() } ),
      rule(@"*/",     fun(l,m) { if (l.states[1].name == comment) then Continue(m.matched) else Token(TokenComment(m.matched)) }, fun(l,m) { l.pop() } ) 
  ])

  return lexer( root-state, [comment-state])
}

import std/text/parse
import std/text/regex
import std/text/unicode
import std/os/path
import std/os/file
import common/name
import common/range
import std/num/float64
import syntax/lexeme

inline fun list(p: parser<e,a>): parser<e,list<a>>
  fn () [p()]
// Character sets

inline fun dot()
  char-is("dot", fn(c) c == '.')

inline fun question()
  char-is("question", fn(c) c == '?')

inline fun forward-slash()
  char-is("forward-slash", fn(c) c == '/')

inline fun open-paren()
  char-is("open-paren", fn(c) c == '(')

inline fun close-paren()
  char-is("close-paren", fn(c) c == ')')

inline fun underscore()
  char-is("underscore", fn(c) c == '_')

inline fun dash()
  char-is("dash", fn(c) c == '-')

inline fun digitChar()
  char-is("digit", fn(c) c >= '0' && c <= '9')

inline fun hexdigit()
  char-is("hex-digit", fn(c) c.is-hex-digit)

inline fun lower()
  char-is("lower-case", fn(c) c.is-lower)

inline fun upper()
  char-is("upper-case", fn(c) c.is-upper)

inline fun letter()
  char-is("letter", fn(c) c.is-upper || c.is-lower)

inline fun space()
  char-is("space", fn(c) c == ' ')

inline fun tab()
  char-is("tab", fn(c) c == '\t')

inline fun ret()
  char-is("return", fn(c) c == '\r')

inline fun line-feed()
  char-is("line-feed", fn(c) c == '\n')

inline fun graphic()
  char-is("graphic", fn(c) c >= '\x21' && c <= '\x7E')

inline fun cont()
  char-is("cont", fn(c) c >= '\x80' && c <= '\xBF')

inline fun symbol()
  one-of("$%&*+~!\\^#=.:-?|<>")

inline fun special()
  one-of("()[]{};,")

inline fun anglebar()
  char-is("anglebar", fn(c) c == '<' || c == '>')

inline fun finalid()
  char-is("finalid", fn(c) c == '\'')

inline fun char-esc()
  char-is("escape", fn(c) c != '\\' || c != '\'' || c != '"')

// Regular expressions

fun newline()
  (optional(Nil, ret.list) ++ [line-feed()])

fun utf8valid1()
  [char-is("utf8valid1", fn(c) c >= '\xC2' && c <= '\xDF'), cont()]

fun utf8valid2()
  [char-is("utf8valid2", fn(c) c == '\xE0'), char-is("utf8valid2.1", fn(c) c >= '\xA0' && c <= '\xBF'), cont()]

fun utf8valid3()
  [char-is("utf8valid3", fn(c) c >= '\xE1' && c <= '\xEC'), cont(), cont()]

// More utf8
fun utf8valid()
  [utf8valid1, utf8valid2, utf8valid3].choose

inline fun utf8()
  utf8valid()

fun linechar()
  [graphic.list, space.list, tab.list, utf8].choose

inline fun graphic-comment()
  char-is("graphic-comment", fn(c) c != '/' && c != '*' && c >= '\x21' && c <= '\x7E')

fun commentchar()
  [graphic-comment.list, space.list, tab.list, newline, utf8].choose

inline fun hexdigit2()
  [hexdigit(), hexdigit()]

inline fun hexdigit4()
  [hexdigit(), hexdigit(), hexdigit(), hexdigit()]

fun hexesc1()
  Cons(char-is("hex-prefix", fn(c) c == 'x'), hexdigit2())

fun hexesc2()
  Cons(char-is("hex-prefix", fn(c) c == 'u'), hexdigit4())

fun hexesc3()
  Cons(char-is("hex-prefix", fn(c) c == 'U'), hexdigit4() ++ hexdigit2())

fun hex-esc()
  [hexesc1, hexesc2, hexesc3].choose

fun escape()
  Cons(char-is("escape", fn(c) c == '\\'), [hex-esc, char-esc.list].choose)

inline fun graphic-string()
  [char-is("graphic-string", fn(c) c != '"' && c != '\\' && c >= '\x21' && c <= '\x7E')]

fun stringchar()
  [graphic-string, space.list, utf8].choose

inline fun graphic-char()
  char-is("graphic-char", fn(c) c != '\'' && c != '\\' && c >= '\x21' && c <= '\x7E')

inline fun charchar()
  [graphic-char.list, space.list, utf8].choose

fun graphic-stringraw()
  char-is("graphic-stringraw", fn(c) c != '"' && c >= '\x21' && c <= '\x7E')

fun stringraw()
  [graphic-stringraw.list, space.list, tab.list, newline, utf8].choose

fun idchar()
  [letter, digitChar, underscore, dash].choose

fun lowerid()
  Cons(lower(), many(idchar) ++ many(finalid))

fun upperid()
  Cons(upper(), many(idchar) ++ many(finalid))

fun conid()
  upperid()

fun modulepath()
  many1({lowerid() ++ (forward-slash.list)()}).concat

fun qvarid()
  modulepath() ++ lowerid()

fun qconid()
  modulepath() ++ upperid()

fun psymbols()
  [{many1(symbol)}, forward-slash.list].choose

fun qidop()
  modulepath() ++ Cons(open-paren(), psymbols() ++ [close-paren()])

fun idop()
  Cons(open-paren(), psymbols() ++ [close-paren()])

fun digitsep(): parse list<char>
  Cons(underscore(), many1(digitChar))

fun hexdigitsep()
  Cons(underscore(), many1(hexdigit))

fun kdigits()
  many1(digitChar) ++ many(digitsep).concat

fun hexdigits()
  many1(hexdigit) ++ many(hexdigitsep).concat

fun zero()
  char-is("zero", fn(c) c == '0')

fun non-zero()
  Cons(char-is("1-9", fn(c) c >= '1' && c < '9'),
    optional(Nil, fn() Cons(underscore(), kdigits())))

fun decimal()
  [zero.list, non-zero].choose

fun hexadecimal()
  [char-is("hex-prefix", fn(c) c == '0'),
  char-is("hex-prefix", fn(c) c == 'x' || c == 'X')] ++
  hexdigits()

fun integer()
  Cons(one-of-or("+-", '+'),
  [decimal, hexadecimal].choose)

fun exp()
  val x = optional(Nil)
    [char-is("prefix", fn(c) c == '+' || c == '-')]
  x ++ many1(digitChar)

fun kexp10()
  Cons(char-is("exp10", fn(c) c == 'e' || c == 'E'), exp())

fun kexp2()
  Cons(char-is("exp2", fn(c) c == 'p' || c == 'P'), exp())

fun decfloat()
  Cons(one-of-or("+-", '+'), decimal()) ++ 
  [{Cons(dot(), kdigits()) ++ optional(Nil, kexp10)}, kexp10].choose

fun hexfloat()
  Cons(one-of-or("+-", '+'), hexadecimal()) ++
  [{Cons(dot(), hexdigits()) ++ optional(Nil, kexp2)}, kexp2].choose

// Start on tokenizing
fun lex-space()
  LexWhite(many1(space).string)

fun lex-newline()
  newline()
  LexWhite("\n")

fun lex-start-comment()
  pstring("/*")
  val x = many(symbol)
  LexError("TODO")

fun lex-start-line-comment()
  pstring("//")
  val x = many(symbol)
  LexError("TODO")

fun lex-linedirective()
  newline()
  pstring("#")
  val x = many(symbol)
  LexError("TODO")

fun lex-qconid()
  LexCons(qconid().newQName)

fun lex-qvarid()
  LexId(qvarid().newQName)

fun lex-qidop()
  LexOp(qidop().stripParens.newQName)

fun lex-lowerid()
  val l = lowerid()
  val s = l.string
  if s.isReserved then
    LexKeyword(s, "")
  else if l.isMalformed then
    LexError(messageMalformed)
  else
    LexId(s.newName)

fun lex-conid()
  LexCons(conid().string.newName)

fun lex-idchars()
  pstring("_")
  LexWildCard(many(idchar).string.newName)

fun lex-special()
  LexSpecial(special().string)

fun lex-decfloat()
  val d = decfloat().filter(fn(c) c != '_').string
  LexFloat(d.parse-float64.default(-1.0), d)

fun lex-hexfloat()
  val d = hexfloat().filter(fn(c) c != '_').string
  LexFloat(d.parse-float64.default(-1.0), d)

fun lex-integer()
  val d = integer().filter(fn(c) c != '_').string
  LexInt(d.parse-int.default(-1), d)

fun lex-or()
  LexOp(pstring("||").newName)

fun lex-anglebar() // TODO: Fix this
  val x = Cons(anglebar(), many1(anglebar)).string
  match x
    "|" -> LexKeyword(x, "")
    _ -> LexOp(x.newName)

fun lex-idop()
  LexIdOp(idop().stripParens.string.newName)

fun lex-symbols()
  val s = psymbols().string
  if s.isReserved then
    LexKeyword(s, "")
  else if s.isPrefixOp then
    LexPrefix(s.newName)
  else
    LexOp(s.newName)

fun lex-stringbegin()
  pstring("\"")
  val inner = many({[stringchar, {pstring("\\").list ++ [char-esc()]}, {pstring("\\").list ++ hex-esc()}, newline].choose}).concat
  val err = illegal-str.many()
  pstring("\"")
  match err
    Cons(e, _) -> e
    Nil -> LexString(inner.string)

fun illegal-str()
  val res = many1({char-is("Illegal string character", fn(c) -> c != '"')})
  LexError(res.string)

fun lex-rstringbegin()
  pstring("r").list ++ many({char-is("Raw Comment", fn(c) -> c == '#')}) ++ pstring("\"").list
  val inner = many({[stringraw].choose}).concat
  val err = illegal-str.many
  pstring("\"").list ++ many({char-is("Raw Comment", fn(c) -> c == '#')})
  match err
    Cons(e, _) -> e
    Nil -> LexString(inner.string)

fun lex-linecomment()
  val start = pstring("//").list ++ many(symbol)
  val inner = many({[linechar].choose})
  val err = many({char-is("Anything", fn(c) -> c != '\n')})
  newline()
  match err
    Cons(_, __) -> LexError(err.string)
    _ -> LexComment((start ++ inner.concat).string)

fun lex-charesc()
  pstring("\'\\")
  val c = char-esc()
  pstring("'")
  LexChar(c.string)

fun lex-hexesc()
  pstring("\'\\")
  val c = hex-esc()
  pstring("'")
  LexChar(c.drop(3).head('*').fromCharEsc.string)

fun lex-char()
  pstring("\'")
  val c = charchar()
  pstring("\'")
  LexChar(c.string)

fun lex-emptychar()
  pstring("\'")
  val c = char-is("any", fn(_) -> True)
  pstring("\'")
  LexError("Illegal character literal: " ++ c.string)
  
fun lex-tab()
  val t = many1(tab)
  LexError("Illegal tab character: configure your editor to use spaces instead")

fun lex-unknown()
  val c = char-is("any", fn(_) -> True)
  LexError("Illegal character: " ++ c.string)

fun lex-default()
  [lex-space,lex-newline,lex-linecomment,lex-qconid,lex-qvarid,lex-qidop,lex-lowerid,lex-conid,lex-idchars,lex-special,lex-charesc,lex-hexesc,lex-char,lex-decfloat,lex-hexfloat,lex-integer,lex-or,lex-idop,lex-symbols,lex-stringbegin,lex-rstringbegin].choose

fun stripParens(s: list<char>)
  match s.reverse
    Cons(')', cs) -> 
      match cs.span(fn(c) { c != '(' })
        (op, Cons('(', qual)) -> (op ++ qual).reverse
        _ -> s
    _ -> s

fun newQName(s: list<char>) 
  val (rname, rsmod) = s.reverse.span(fn(c) { c != '/' })
  match rsmod // TODO: First case needs condition on rname == Nil
    Cons('/', Cons('/', rmod)) -> newQualified(rmod.reverse.string, "/")
    Cons('/', rmod) -> newQualified(rmod.reverse.string, rname.reverse.string)
    _ -> s.string.newName

fun fromCharEsc(c)
  match c
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _ -> c

fun unsafeChar(k: string, s: string): lex
  LexError("Unsafe character in " ++ k ++ ": " ++ s) //TODO: ++ showHex(4, s.list.head))

// Reserved
val specialNames = [ "{", "}"
    , "(", ")"
    , "<", ">"
    , "[", "]"
    , ";", ","
]

val reservedNames =  ["infix", "infixr", "infixl", "prefix", "postfix"
    , "type", "alias"
    , "struct", "enum", "con"
    , "val", "fun", "fn", "extern", "var"
    , "ctl", "final", "raw"
    , "if", "then", "else", "elif"
    , "return", "match", "with", "in"
    , "forall", "exists", "some"
    , "pub", "abstract"
    , "module", "import", "as"

    // effect handlers
    , "handler", "handle"
    , "effect", "receffect"
    , "named"
    , "mask"
    , "override"   

    // deprecated
    , "private", "public"  // use pub
    , "rawctl", "brk"      // use raw ctl, and final ctl

    // alternative names for backwards paper compatability
    , "control", "rcontrol", "except"
    , "ambient", "context" // use effcet
    , "inject"       // use mask
    , "use", "using" // use with instead
    , "function"     // use fun
    , "instance"     // use named

    // future reserved
    , "interface"
    , "unsafe"

    // operators
    , "="
    , "."
    , ":"
    , "->"
    , "<-"
    , ":="
    , "|"]

val symbols = "$%&*+~!/\\^~=.:-?<>|".list

fun isReserved(name: string)
  reservedNames.any(fn(n) -> n == name) // TODO: Set

fun isPrefixOp(name: string)
  name == "!" || name == "~"

fun isMalformed(name: list<char>)
  match name
    Cons('-', Cons(c, cs)) -> !c.is-alpha || cs.isMalformed
    Cons(c, Cons('-', cs)) -> !c.is-alpha || c.is-digit || cs.isMalformed
    Cons(_, cs) -> cs.isMalformed
    Nil -> False

val messageMalformed
  = "malformed identifier: a dash must be preceded by a letter or digit, and followed by a letter"

// Lexer state and actions
struct lex-state
  pos: pos
  startPos: pos
  retained: list<list<char>>
  previous: char
  current: list<char>
  previousLex: lex
  rawEnd: list<char>

linear effect lexE
  fun curr-state(): lex-state

type lex-part
  Default
  StringLit
  StringRaw
  Comment
  LineComment
  LineDirective

fun lexer(sourceName: string, lineNo: int, input: string)
  lexing(Source(sourceName, input), lineNo, input)

fun lex(source: source, lineNo: int, input: string): div list<lexeme>
  match lexing(source, lineNo, input)
    ParseOk(u, _) -> u.map(fn(x) Lexeme(rangeNull, x))
    _ -> Cons(Lexeme(rangeNull, LexError("Unexpected end of input")), Nil)

fun lexEHandler(initialState:lex-state, action: () -> <lexE| e> a): e a
  var state := initialState
  with handler
    fun curr-state()
      state
  action()

fun lexing(source: source, lineNo: int, input: string): <div> parse-error<list<lex>>
  // with parse(input.slice)
  with parseGreedy(input.slice)
  var initPos := Pos(source, 0, lineNo, 1)
  with lexEHandler(Lex-state(initPos, initPos, [], '\n', input.list, LexWhite(""), "\"".list))
  var result := Nil
  while { ! current-input().is-empty() }
    result := Cons(lex-default(), result)
  result.reverse

fun main()
  // Make sure to set $ulimit -s unlimited$ to allow more stack space
  val r = read-text-file("src-koka/syntax/lexer.kk".path)
  val x = lexer("test", 0, r)
  match x
    ParseOk(u, _) -> u.map(show).join(",").println
    ParseError(msg, _) -> msg.println

pub fun parseGreedy(input0 : sslice, action : () -> <parse|e> a ) : e parse-error<a> 
  var input := input0
  with handler
    return (x)
      ParseOk(x, input)
    fun current-input()
      input
    brk fail(msg)
      ParseError(msg, input)
    fun satisfy(pred)
      val inp = input
      val m   = fn() : total _ { pred(inp) }
      match m() 
        Just((x,cap)) -> { input := cap; Just(x) }
        Nothing       -> Nothing
    ctl pick()
      val save = input;
      match resume(True)
        ParseOk(x1, rest1) -> ParseOk(x1, rest1)
        ParseError(msg, restErr1) -> 
          input := save
          match resume(False)   // todo: limit lookahead?
            ParseOk(x2, rest2) -> 
              if restErr1.count < rest2.count then ParseError(msg, restErr1) else ParseOk(x2, rest2) 
            ParseError(msg2, restErr2) -> 
              if restErr2.count < restErr1.count then ParseError(msg2, restErr2) else ParseError(msg, restErr1)
  action()
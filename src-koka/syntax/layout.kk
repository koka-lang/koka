import syntax/lexeme
import syntax/lexer
import syntax/syntax
import common/range
import common/name
import std/os/file
import std/os/path


fun testFile(fname: string)
  val f = read-text-file(("test/" ++ fname).path)
  testEx(fname, f)

fun test(xs: string)
  testEx("", xs)

fun testEx(fname: string, input: string)
  val tokens = lex(Source(fname, input), 1, input)
  layout(True, tokens).map(show).unlines.println

fun layout(semiInsert: bool, lexemes: list<lexeme>): list<lexeme>
  fun semi(v: list<lexeme>, f: (list<lexeme>) -> list<lexeme>) if semiInsert then f(v) else v
  lexemes.semi(checkComments).combineLineComments.removeWhiteSpace.associateComments.removeWhite.semi(indentLayout)

fun isLexError(x)
  match x
    LexError(_) -> True
    _ -> False

fun removeWhite(lexemes: list<lexeme>): list<lexeme>
  lexemes.filter(fn(x) ! lexemeIsWhite(x))

fun removeWhiteSpace(lexemes: list<lexeme>): list<lexeme>
  with x <- lexemes.filter
  match x
    Lexeme(_, LexWhite(_)) -> True
    _ -> False

fun endLine(r)
  r.end.line

fun startLine(r)
  r.start.line

fun startCol(r)
  r.start.col

fun endCol(r)
  r.end.col

fun before(r)
  Range(r.start, r.start)

fun after(r)
  Range(r.end, r.end)

fun isAdjacent(comment: string, r1: range, r2: range): bool
  if comment.ends-with("\n").bool then r1.end.line == r2.start.line else r1.end.line == r2.start.line - 1

fun isAttr(l: lexeme)
    match l
      Lexeme(_, LexKeyword(_)) -> True
      Lexeme(_, LexId(_)) -> True
      _ -> False


fun isDocKeyword(lex: string)
  ["fun","val","ctl","final","raw"
                    ,"type","effect","struct","con","alias"
                    ,"extern","module"
                    // deprecated:
                    ,"control","rcontrol","except","rawctl","brk"
                    ,"cotype","rectype"
                    ,"external","function"].find(fn(x) x == lex).bool

// Associate comments that precede a declaration
// to the corresponding keyword
fun associateComments(lexs: list<lexeme>): list<lexeme>
  match lexs
    // Special comments
    Cons(Lexeme(r1, LexComment(c1)), ls) | (c1.starts-with("//.").bool 
      && ! (c1.slice.substr(3, c1.count).string.trim-right.list.any(fn(c) -> c == ' '))) -> 
        Cons(Lexeme(r1, LexSpecial(c1.trim-right)), ls.associateComments)
    // Comment association
    Cons(Lexeme(r1, LexComment(comment)), Cons(Lexeme(r2,LexKeyword(k, _)), ls)) | (k.isDocKeyword 
      && comment.isAdjacent(r1, r2)) ->
        Cons(Lexeme(r1, LexComment(comment)), Cons(Lexeme(r2, LexKeyword(k, comment)), associateComments(ls)))
    Cons(Lexeme(r1, LexComment(comment)), Cons(l, Cons(Lexeme(r2, LexKeyword(k, _)), ls))) | (k.isDocKeyword
      && comment.isAdjacent(r1, r2) && l.isAttr) ->  // Pub type / Inline Fun
        Cons(Lexeme(r1, LexComment(comment)), Cons(l, Cons(Lexeme(r2, LexKeyword(k, comment)), associateComments(ls))))
    Cons(Lexeme(r1, LexComment(comment)), Cons(l1, Cons(l2, Cons(Lexeme(r2, LexKeyword(k, _)), ls)))) | (k.isDocKeyword
      && comment.isAdjacent(r1, r2) && l1.isAttr && l2.isAttr) ->  // Pub inline type / Pub value type
        Cons(Lexeme(r1, LexComment(comment)), Cons(l1, Cons(l2, Cons(Lexeme(r2, LexKeyword(k, comment)), associateComments(ls)))))
    Cons(l,ls) -> Cons(l, ls.associateComments)
    Nil -> Nil

// Combine adjacent line comments into one block comment (for html output)
fun combineLineComments(lexs: list<lexeme>): list<lexeme>
  match lexs
    Cons(Lexeme(r1, LexComment(c1)), Cons(Lexeme(r2, LexComment(c2)), ls)) | (c1.starts-with("//").bool 
      && c2.starts-with("//").bool) -> combineLineComments(Cons(Lexeme(r1.combine(r2), LexComment(c1 ++ c2)), ls).unsafe-decreasing)
    Cons(l, ls) -> Cons(l, combineLineComments(ls))
    Nil -> Nil

fun checkIndent(prevLine, rng, commentRng)
    if rng.startLine > prevLine && rng.startLine == commentRng.endLine && commentRng.endCol > 1 then 
      [Lexeme(commentRng, LexError("layout: comments cannot be placed in the indentation of a line"))]
    else Nil

fun check(prevLine: int, commentRng: range, l: list<lexeme>): list<lexeme>
  match l
    Nil -> Nil
    Cons(Lexeme(rng, lex), ls) -> 
      val newR = match lex
        LexComment -> check(prevLine, rng, ls)
        LexWhite -> check(prevLine, commentRng, ls)
        _ -> checkIndent(prevLine, rng, commentRng) ++ check(rng.endLine, commentRng, ls)
      Cons(Lexeme(rng, lex), newR)

// Check for comments in indentation
fun checkComments(lexs: list<lexeme>): list<lexeme>
  check(0, rangeNull, lexs)

struct layout
  open: lexeme
  column: int

fun indentLayout(lexs: list<lexeme>): list<lexeme>
  match lexs
    Nil -> Cons(Lexeme(rangeNull, LexInsSemi), Nil)
    Cons(l, _) -> 
      val start = Lexeme(l.range.before, LexWhite("")) // ignored
      unsafe-total
        brace(Layout(start, 1), [], start, lexs)

fun brace(layout: layout, layouts: list<layout>, prev: lexeme, lexemes: list<lexeme>): <div> list<lexeme>
  match lexemes
    Nil -> match layouts
      Nil -> Nil
      // End of file ending braces
      Cons(ly, lys) -> 
        val rCurly = layout.insertRCurly(prev)
        prev.insertSemi ++ rCurly ++ brace(ly, lys, Lexeme(prev.range.after, LexInsRCurly), [])
    // ignore error lexemes
    Cons(l, ls) | l.lex.isError -> Cons(l, brace(layout, layouts, prev, ls))
    Cons(lexeme, ls) -> 
      val Layout(Lexeme(_, layoutLex), layoutCol) = layout
      val Lexeme(prevRng, prevLex) = prev
      val Lexeme(rng, lex) = lexeme
      val newline = prevRng.endLine < rng.startLine
      val indent = rng.startCol
      val nextIndent = match ls
        Cons(Lexeme(r, _), _) -> r.startCol
        _ -> 1
      if newline && indent > layoutCol && !(prevLex.isExprContinuation(lex)) then
        brace(layout, layouts, prev, prev.insertLCurly ++ lexemes)
      elif newline && indent < layoutCol && !(lex.isCloseBrace && layoutLex == LexSpecial("{")) then
        brace(layout, layouts, prev, layout.insertRCurly(prev) ++ lexemes)
      elif lex.isOpenBrace then
        val v = if (nextIndent > layoutCol) then [] else [Lexeme(rng, LexError("layout: line must be indented more than the enclosing layout context (column " ++ layoutCol.show ++ ")"))]
        [lexeme] ++ v ++ brace(Layout(lexeme, nextIndent), Cons(layout, layouts), lexeme, ls)
      elif lex.isCloseBrace then
        val rest = match layouts
          Nil -> Cons(Lexeme(rng.before, LexError("unmatched closing brace '}'")), brace(layout, [], lexeme, ls))
          Cons(ly, lys) -> brace(ly, lys, lexeme, ls)
        prev.insertSemi ++ Cons(lexeme, rest)
      elif newline && indent == layoutCol && !(prevLex.isExprContinuation(lex)) then
        prev.insertSemi ++ [lexeme] ++ brace(layout, layouts, lexeme, ls)
      else
        Cons(lexeme, brace(layout, layouts, lexeme, ls))

fun insertLCurly(Lexeme(prevRng, _))
  [Lexeme(prevRng.after, LexInsLCurly)]

fun insertRCurly(Layout(Lexeme(layoutRng, layoutLex), layoutCol), Lexeme(prevRng, _)): list<lexeme>
  if layoutLex == LexInsLCurly then [] else
    Cons(
      Lexeme(prevRng.after, 
        LexError(
          "layout: an open brace '{' (at " ++ layoutRng.showRange(True) ++ 
          ", layout column " ++ layoutCol.show  ++ ") is matched by an implicit closing brace"
        )), 
      Cons(Lexeme(prevRng.after, LexInsRCurly), Nil)
    )

fun insertSemi(Lexeme(prevRng, prevLex))
  if prevLex.isSemi then [] else [Lexeme(prevRng.after, LexInsSemi)]

fun isExprContinuation(prevLex, lex)
  lex.isStartContinuationToken || prevLex.isEndContinuationToken

fun isStartContinuationToken(lex)
  match lex
    LexSpecial(s) -> [")",">","]",",","{","}"].find(fn(c) -> c == s).bool
    LexKeyword(k, _) -> ["then","else","elif","->","=","|",":",".",":="].find(fn(c) -> c == k).bool
    LexOp(op) -> op.nameId != "<"
    LexInsLCurly -> True
    LexInsRCurly -> True
    _ -> False

fun isEndContinuationToken(lex)
  match lex
    LexSpecial(s) -> ["(","<","[",",","{"].find(fn(c) -> c == s).bool
    LexKeyword(k, _) -> k == "."
    LexInsLCurly -> True
    LexOp(op) -> op.nameId != ">"
    _ -> False

fun isCloseBrace(lex)
  match lex
    LexSpecial("}") -> True
    LexInsRCurly -> True
    _ -> False

fun isOpenBrace(lex)
  match lex
    LexSpecial("{") -> True
    LexInsLCurly -> True
    _ -> False

fun isSemi(lex)
  match lex
    LexSpecial(";") -> True
    LexInsSemi -> True
    _ -> False


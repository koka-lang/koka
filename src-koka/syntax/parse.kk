import std/text/parse
import std/os/path
import std/os/file
import syntax/lexer
import syntax/lexeme
import syntax/layout
import syntax/syntax
import common/syntax
import common/range

fun lexParse(insSemi: bool, preprocess: list<lexeme> -> list<lexeme>, p: (source) -> <parse,fsys,exn|e> a, sourceName: string): <exn,fsys|e> parse-error<a>
  val r = read-text-file(sourceName.path)
  with parse(r.slice)
  p(Source(sourceName, r))
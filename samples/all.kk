// Various examples of Koka programs and features
// In VS Code, hold `ctrl` and click on any import to open that module.
//
// - `basic`:    small typical Koka programs
// - `learn`:    explain various specific features, like with-syntax or implicit parameters
// - `handlers`: show effect handler programs
//
module all

import basic/caesar
import basic/fibonacci
import basic/garsia-wachs
import basic/rbtree

import learn/basic
import learn/handler
import learn/with
import learn/qualifiers
import learn/implicits
import learn/contexts
import learn/fip
import learn/lazycons

import handlers/ambient
import handlers/basic
import handlers/nim
import handlers/vec
import handlers/yield
import handlers/parser
import handlers/scoped
import handlers/unix

import handlers/named/ask
import handlers/named/ask-poly
import handlers/named/file
import handlers/named/file-scoped
import handlers/named/heap
import handlers/named/unify

fun run( name : string, action : () -> <console|e> a ) : <console|e> ()
  println("run " ++ name ++ "\n--------------------------")
  action()
  println("")

pub fun main()
  // basic
  run("caesar",caesar/main)
  run("fibonacci",fibonacci/main)
  run("garsia-wachs",garsia-wachs/main)

  // learn
  run("basic",learn/basic/main)
  run("contexts",contexts/main)
  run("handler",learn/handler/main)
  run("fip",fip/main)
  run("lazycons",lazycons/main)
  run("implicits",implicits/main)
  run("qualifiers",qualifiers/main)
  run("with",learn/with/main)

  // named handlers
  run("ask-poly",handlers/named/ask-poly/main)
  run("heap",handlers/named/heap/main)
  run("unify",handlers/named/unify/main)
  run("ask",handlers/named/ask/main)

  // handlers
  run("ambient",ambient/main)
  run("nim",nim/main)
  run("parser",parser/main)
  run("scoped",scoped/main)
  run("vec",vec/main)
  run("yield",handlers/yield/main)
  run("unix",unix/main)

  // last as it takes a bit longer
  run("rbtree (this can take a bit longer..)",rbtree/main)


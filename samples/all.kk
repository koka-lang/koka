// Various examples of Koka programs and features
// In VS Code, hold `ctrl` and click on any import to open that module.
//
// - `basic`:    small typical Koka programs
// - `syntax`:   explain various specific features, like with-syntax or implicit parameters
// - `handlers`: show effect handler programs
module all

import basic/caesar
import basic/fibonacci
import basic/garsia-wachs
import basic/rbtree

import syntax/basic
import syntax/handler
import syntax/with
import syntax/qualifiers
import syntax/implicits
import syntax/contexts
import syntax/fip

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
  run("caesar",caesar/main)
  run("fibonacci",fibonacci/main)
  run("garsia-wachs",garsia-wachs/main)
  run("contexts",contexts/main)
  run("fip",fip/main)
  run("implicits",implicits/main)

  run("unify",unify/main)
  run("heap",heap/main)
  run("ask-poly",ask-poly/main)

  run("nim",nim/main)
  run("parser",parser/main)
  run("scoped",scoped/main)
  run("ambient",ambient/main)
  run("unix",unix/main)

  // last as it takes a bit longer
  run("rbtree (this can take a bit longer..)",rbtree/main)


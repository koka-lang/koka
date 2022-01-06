module samples/all

import basic/caesar
import basic/fibonacci
import basic/garsia-wachs
import basic/rbtree

import syntax/basic
import syntax/effect
import syntax/handler
import syntax/var
import syntax/with

import handlers/ambient
import handlers/basic
import handlers/nim
import handlers/parser
import handlers/scoped
import handlers/vec
import handlers/yield

import named-handlers/ask
import named-handlers/file
import named-handlers/file-scoped
import named-handlers/heap

pub fun main() 
  caesar/main()
  fibonacci/main()
  garsia-wachs/main()
  rbtree/main()
  nim/main()
  parser/main()
  scoped/main()
  ambient/main()


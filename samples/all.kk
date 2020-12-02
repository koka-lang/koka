module samples/all

import basic/caesar
import basic/fibonacci
import basic/garsia-wachs

import syntax/basic
import syntax/effect
import syntax/handler
import syntax/var
import syntax/with

import handlers/basic
import handlers/ambient
import handlers/nim
import handlers/scoped
import handlers/vec

import named-handlers/ask
import named-handlers/file
import named-handlers/heap

public fun main() {
  fibonacci/main()
  garsia-wachs/main()
}

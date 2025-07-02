// https://github.com/koka-lang/koka/issues/730

struct scope( : list<int> )

fun main()
  val empty-scope = Scope([])
  match empty-scope
    Scope(Cons(head, _)) ->
      println("nonempty!")
      println(head.show)
    Scope(Nil) ->
      println("empty!")
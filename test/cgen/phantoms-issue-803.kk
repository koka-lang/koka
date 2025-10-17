fun do-something(action)
  val cb = ref(fn() ())
  (!cb)()
  fun setup-cb() // : maybe<() -> <div,read<_h>,alloc<_h>|_e> _a>
    Just(action) 
  action()

fun main()
  "hello".println
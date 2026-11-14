// issue #598: alias with effect parameter applied to a single effect label
// crashed with Type.TypeVar.subNew.KindMismatch instead of a kind error
effect yield<a>
  ctl yield(elem : a) : ()

alias stream<a,e> = () -> <yield<a>|e> ()

fun repeat(value : a) : stream<a,div>
  fun rec()
    yield(value)
    rec()
  rec

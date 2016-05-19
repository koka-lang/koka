/* There are the "comparative examples" on page 51 of
   the paper "The Type and Effect Discipline" by Jean-Pierre Talpin and Pierre Jouvelot
   We accept all variants in contrast to all the other compared systems in that paper.
*/

function rid(x) {
  val r = ref(x)
  !r
}

function nop(f,x) {
  fun g(y) { f(x) }
  x
}

val id1 = { val x = id(1); rid }

function id2(y) = rid(id)(y)

val id3 = nop(rid, id)

function foo4(z) {
  function id4(x) {
    val t = if (True) z else (fun(y){ var r := x; y })
    return x
  }
  id4(id4)
}

function foo5(f) {
  function id5(y) {
    val r = ref(y)
    val t = if(True) then (fun(z){ if(True) then r else ref(y); z }) else f
    y
  }
  id5(id5)
}

function etaRef(f) {
  val r = ref(f)
  if(True) f else (fun(x : ref<_h,_t>){ val s = if(True) r else ref(f); !x})
}

function t1() = id1(id1)
function t2() = id2(id2)
function t3() = id3(id3)

val t4 = if (True) (fun(x){x}) else (fun(x) { !(ref(x)) })

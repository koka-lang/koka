import common/name
import common/name-prim

type kind
  KCon(constant: kindCon) // Kind constants: "*","->","!","H","P"
  KApp(a: kind, b: kind) // Application (only allowed for functions as yet)

fun (==)(a: kind, b: kind): bool
  match (a, b)
    (KCon(c1), KCon(c2)) -> c1 == c2
    (KApp(a1, b1), KApp(a2, b2)) -> unsafe-decreasing(a1) == a2 && unsafe-decreasing(b1) == b2
    _ -> False


// Kind constant
alias kindCon = name

pub fun hasKindStarResult(k: kind): bool
  k.extractKindFun().snd == kindStar

pub fun hasKindLabelResult(k: kind): bool
  k.extractKindFun().snd.isKindAnyLabel

val kindStar = KCon(nameKindStar) // *
val kindLabel = KCon(nameKindLabel) // l
val kindArrow = KCon(nameKindFun) // -> 
val kindPred = KCon(nameKindPred)
val kindEffect = KCon(nameKindEffect) // X
val kindScope = KCon(nameKindScope) // S
val kindHeap = KCon(nameKindHeap) // H
val kindLocal = kindFun(kindHeap, kindLabel)
val kindHandled = KCon(nameKindHandled) // HX
val kindHandled1 = KCon(nameKindHandled1) // HX1
val kindExtend = kindFun(kindLabel, kindFun(kindEffect, kindEffect))
fun kindCon(n: int): kind
  kindConOver(replicate(kindStar, n))
fun kindConOver(ks: list<kind>): kind
  ks.foldr(kindStar, kindFun)
fun kindFunN(ks: list<kind>, k: kind): kind
  ks.foldr(k, kindFun)
fun kindFun(k1: kind, k2: kind): kind
  KApp(KApp(kindArrow, k1), k2)
fun isKindFun(k: kind): bool
  match k
    KApp(KApp(k0, _), _) -> k0 == kindArrow
    _ -> False
pub fun extractKindFun(k: kind): (list<kind>, kind)
  match k
    KApp(KApp(k0, k1), k2) ->
      if k0 == kindArrow then
        val (args, res) = unsafe-decreasing(k2).extractKindFun()
        (Cons(k1, args), res)
      else
        ([], k)
    _ -> ([], k)


fun isKindAnyLabel(k: kind): bool
  k == kindHandled || k == kindHandled1 || k == kindLabel

// Standard kind constant with their kind
val buildinKinds = [
  (nameKindStar, kindStar),
  (nameKindFun, kindArrow),
  (nameKindPred, kindPred),
  (nameKindEffect, kindEffect),
  (nameKindLabel, kindLabel),
  (nameKindHeap, kindHeap),
  (nameKindScope, kindScope),
  (nameKindHandled, kindHandled),
  (nameKindHandled1, kindHandled1),
]
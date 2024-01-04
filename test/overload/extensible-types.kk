value struct subtype<s0,s1>
  inj: s0 -> s1 // injection
  prj: s1 -> maybe<s0> // projection

value type or<a,b>
  L(a:a)
  R(b:b)

val or/subtype = Subtype(fn(a) L(a), fn(b) match b {L(x) -> Just(x); _ -> Nothing})

fun convert/subtype(??subtype: subtype<a,b>): subtype<a,or<c,b>>
  Subtype(fn(a) R(inj(a)), fn(b) match b {R(a) -> prj(a); _ -> Nothing})

alias value = or<int, or<func, ()>>;
alias term = or<terma, ()> // , or<termf, or<termr, or<terml, or<termt, or<termc, termn>>>>>>;

type terma
  Num(i: int)
  Add(l: term, r: term)

rec type func
  Funct(f: some<e> interpm<value> -> e interpm<value>)

struct interpm<t>
  interp: some<e> t -> e interpm<t>

fun main()
  val x: value = (or/subtype.inj)(1)
  x.show.println

fun or/show(or: or<a,b>, ?showa: a -> string = show, ?showb: b -> string = show): string
  match or
    L(a) -> showa(a)
    R(b) -> showb(b)


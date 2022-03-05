import std/data/map
import std/data/set
import common/name
import common/failure
import platform/config

alias name-map<a> = map<name, a>;

fun find(m: name-map<a>, n: name): <div,exn> a
  with name_compare
  match m.lookup(n)
    Just(x) -> x
    _ -> throw("common/name-map.find: could not find: " ++ n.show)

// alias name-set<a> = set<name>;

alias qname-map<a> = map<name,list<(name,a)>>;

type lookup<a>
  Found(name: (name, a))
  Ambiguous(names: list<name>)
  NotFound

fun single(n: name, a: a): qname-map<a>
  singleton(n.unqualify, [(n, a)])

val name_compare = handler
  fun std/data/map/compare(n1: name, n2: name) 
    common/name/compare(n1, n2)

fun insertName(m: qname-map<a>, n: name, a: a): <compFlags,exn> qname-map<a>
  with name_compare
  m.insertWith(n.unqualify, Cons((n, a), Nil)) fn(xs, ys)
    safeCombine("insert", xs, ys)

fun fromList(l: list<(name,a)>): <compFlags,exn> qname-map<a>
  l.foldl(empty) fn(qm, (name, x))
    qm.insertName(name, x)

fun safeCombine(s: string, xs: list<(name,a)>, ys: list<(name,a)>): <compFlags,exn> list<(name,a)>
  if xs.any(fn(x) -> ys.any(fn(y) -> y.fst == x.fst))
    fail("common/name-collections " ++ s ++ ": overlapping names: " ++ xs.map(fn(x) -> x.fst.show).show ++ ys.map(fn(y) -> y.fst.show).show)
  xs ++ ys


// Lookup a fully qualified name
fun lookupQ(m: qname-map<a>, n: name): maybe<a>
  with name_compare
  match m.lookup(n.unqualify)
    Just(xs) -> xs.lookup(fn(o) -> o == n)
    _ -> Nothing

// Lookup a potentially unqualified name within a module context. 
// (The module context is ignored if a qualified name is looked up)
fun lookup(m: qname-map<a>, c: name, name: name): lookup<a>
  with name_compare
  match m.lookup(name.unqualify)
    Just(Cons((qname,x), _)) | !name.isQualified -> Found((qname, x))
    Just(xs) ->
      val qname = if name.isQualified then name else c.qualify(name)
      match xs.filter(fn(p) -> p.fst == qname)
        Cons((realname, x), _) -> Found((realname, x))
        _ -> Ambiguous(xs.map(fst))
    _ -> NotFound

// fun filterNames(m: qname-map<a>, p: (name) -> bool): qname-map<a>
// TODO: The rest of the name collection functions
import lib/pprint
import common/range

struct name
  pub nameModule: string
  pub hashModule: int
  pub nameId: string
  pub hashId: int

fun isEarlyBindName(n: name)
  n.isHandleName //TODO? Other stuff

fun nameCaseEqual(n1: name, n2: name): bool
  n1.nameId == n2.nameId && n1.splitModuleName.reverse.zipwith(n2.splitModuleName.reverse, fn(c1, c2) -> c1 == c2).all(fn(x) -> x)

fun nameCaseOverlap(n1: name, n2: name): bool
  !n1.nameCaseEqual(n2) && n1.isSameNamespace(n2)
    
fun isSameNamespace(n1: name, n2: name): bool
  match (n1.nameId.list, n2.nameId.list)
    (Cons(c, _), Cons(d, _)) -> c.is-upper && d.is-upper
    _ -> True

fun lowerCompare(Name(m1, _, n1, _), Name(m2, _, n2, _)): order
  match compare(m1.to-lower, m2.to-lower)
    Eq -> compare(n1.to-lower, n2.to-lower)
    lg -> lg

fun (==)(n1: name, n2: name): bool
  n1.hashId == n2.hashId && n1.hashModule == n2.hashModule && n1.lowerCompare(n2) == Eq

fun compare(n1: name, n2: name): order
  match n1.hashModule.compare(n2.hashModule)
    Eq -> 
      match n1.hashId.compare(n2.hashId)
        Eq -> n1.lowerCompare(n2)
        x -> x
    x -> x

fun labelNameCompare(n1: name, n2: name): order
  match n1.hashId.compare(n2.hashId)
    Eq -> 
      match n1.nameId.to-lower.compare(n2.nameId.to-lower)
        Eq -> 
          match n1.hashModule.compare(n2.hashModule)
            Eq -> n1.nameModule.to-lower.compare(n2.nameModule.to-lower)
            x -> x
        x -> x
    x -> x

val canonicalSep = '.'

fun show(Name(m, _, n, _)): string
  val pre = if m == "" then "" else m ++ "/"
  val (mid, _) = match n.list.reverse.span(is-digit)
    (Cons(p, ps), Cons(c, rest)) | (c == canonicalSep) -> (rest.reverse, Cons(c, Cons(p, ps).reverse))
    _ -> (n.list, "".list)
  val rest = match mid
    Cons(c, _) | !(c.is-alpha || c == '_' || c == '(' || c == '.') -> "(" ++ n ++ ")"
    _ -> n
  pre ++ rest

fun showPlain(n: name): string // TODO: better name show
  if n.nameModule.is-empty then n.nameId else n.nameModule ++ "/" ++ n.nameId

fun pretty(n: name): doc
  n.show.text

fun showTupled(Name(m, _, n, _)): string
  "(" ++ m.show ++ "," ++ n.show ++ ")"

fun readTupled(s: string): exn name
  val Cons(t1, Cons(t2, _)) = s.split(",") // TODO: Strip parens
  newQualified(t1, t2)

fun readQualified(s: string): exn name
  val sl = s.list
  if (sl.take(1).head.default(' ') == '(') then readTupled(s)
  else
    val (n, m) = sl.reverse.span(fn(c) -> c != '/')
    newQualified(m.drop(1).reverse.string, n.reverse.string)

// show quotes around the name
fun showName(n: name): string
  n.show.show

fun newName(s: string): name
  newQualified("", s)

fun newQualified(mod: string, name: string): name
  Name(mod, hash(mod), name, hash(name))

fun hash(s: string)
  val sn = (s ++ "____").list.take(4).string.to-lower.list.map(fn (c) { c.int })
  sn.foldl(0, fn(c, h) { h * 256 + c})

val nameNil = "".newName

fun isNil(Name(_, _, n, _)): bool
  n == ""

fun qualify(a: name, b: name): name
  if a.nameModule.is-empty && b.nameModule.is-empty then
    Name(a.nameId, a.hashId, b.nameId, b.hashId)
  elif a.nameModule.is-empty && a.nameId == b.nameModule then
    Name(a.nameId, a.hashId, b.nameId, b.hashId)
  else
    trace("Cannot use qualify on qualified names " ++ a.show ++ ", " ++ b.show)
    newName("")

fun unqualify(Name(_, _, n, hn)): name
  Name("", 0, n, hn)

fun isQualified(Name(m, _, _, _)): bool
  m != ""

fun qualifier(Name(m, hm, _, _)): name
  Name("", 0, m, hm)

// Module paths
fun splitModuleName(n: name): list<name>
  if n.isQualified then unsafe-decreasing(n.qualifier).splitModuleName
  else n.show.split("/").map(fn(s) -> s.newName)

fun unsplitModuleName(l: list<name>): name
  l.map(fn(n) -> n.show).join("/").newName

fun merge(l1: list<name>, l2: list<name>): list<name>
  match (l1, l2)
    (Cons(m, ms), Cons(n, ns)) -> 
      if m == n && ms.zipwith(ns, fn(i, j) -> i == j).all(fn(i) -> i) then
        Cons(m, ms) ++ ns.drop(ms.length)
      else
        Cons(m, merge(unsafe-decreasing(ms), Cons(n, ns)))
    (Cons(m, ms), ns) -> Cons(m, merge(unsafe-decreasing(ms), ns))
    (_, ns) -> ns

fun mergeCommonPath(n1: name, n2: name): name
  val ns = n2.splitModuleName
  val ms = n1.splitModuleName
  val new = ms.merge(ns).unsplitModuleName
  new

fun isWildcard(Name(_, _, n, _)): bool
  match n.list
    Cons('_', _) -> True
    Cons('.', Cons('_', _)) -> True
    _ -> False

fun isConstructorName(Name(_, _, n, _)): bool
  match n.list
    Cons('.', Cons(c, _)) -> c.is-upper || c == '('
    Cons(c, _) -> c.is-upper || c == '('
    _ -> False

// TODO: Move to std library
fun to-upper(c: char): char
  c.string.to-upper.head-char.default(c)
fun to-lower(c: char): char
  c.string.to-lower.head-char.default(c)

fun toConstructorName(Name(m, _, n, _)): name
  val nl = n.list
  val new = match nl
    Cons('.', Cons(c, cs)) -> Cons('.', Cons(c.to-upper, cs)) // keep hidden names hidden
    Cons(c, cs) -> Cons(c.to-upper, cs)
    _ -> nl
  m.newQualified(new.string)

fun toLowers(l: list<char>): list<char>
    match l
      Cons(c, cs) | c.is-upper -> Cons(c.to-lower, toLowers(cs))
      _ -> l

fun toVarName(Name(m, _, n, _)): name 
  val new = match n.list
    Cons('.', cs) -> Cons('.', cs.toLowers) // keep hidden names hidden
    cs -> cs.toLowers
  m.newQualified(new.string)

// Various special names
fun newHiddenName(s: string): name
  newName("." ++ s)

fun isHiddenName(n: name): bool
  match n.nameId.list
    Cons('.', _) -> True
    _ -> False

fun makeHiddenName(s: string, n: name): name
  val xname = match n.nameId.list
    Cons('.', cs) -> newQualified(n.nameModule, cs.string)
    _ -> n
  prepend("." ++ s ++ "-", xname)

fun makeFreshHiddenName(s: string, n: name, r: range)
  val idFromPos = fn(p) -> "-l" ++ p.line.show ++ "-c" ++ p.col.show
  makeHiddenName(s, postpend(r.start.idFromPos, n))

fun hiddenNameStartsWith(n: name, pre: string): bool
  n.nameId.starts-with("." ++ pre ++ "-").bool


fun toUniqueName(i: int, n: name): name
  fun insert(l: list<char>): list<char>
    match l
      Cons(c, cs) | c == '\'' || c == '?' -> Cons(c, cs.insert)
      _ -> (i.show ++ l.string).list.reverse
  n.nameModule.newQualified(n.nameId.list.reverse.insert.reverse.string)

fun toHiddenUniqueName(i: int, s: string, n: name): name
  if s == "" then
    prepend(".", toUniqueName(i, n))
  else
    val c = n.nameId.head-char.default(' ')
    val xname = if c.is-alpha || c == '.' then n else n.nameModule.newQualified("op")
    makeHiddenName(s ++ i.show, xname)

fun newFieldName(i: int): name
  newHiddenName("field" ++ i.show)

fun isFieldName(n: name): bool
  n.isHiddenName

fun newImplicitTypeVarName(i: int): name
  newHiddenName("t" ++ i.show)

fun isImplicitTypeVarName(n: name): bool
  n.isHiddenName

fun newHiddenExternalName(n: name): name
  makeHiddenName("extern", n)

fun isHiddenExternalName(n: name): bool
  n.hiddenNameStartsWith("extern")

// Create a constructor creator name from the constructor name.
// Used if special creation functions are used for the constructor.
// in particular for the case of optional arguments.
fun newCreatorName(n: name): name
  makeHiddenName("create", n)

// Create a handler type name from an effect type name.
fun toHandlerName(n: name): name
  makeHiddenName("hnd", n)

fun isHandlerName(n: name): bool
  n.nameId.starts-with(".hnd-").bool

// Create an effect type name from an operations type name.
fun fromHandlerName(n: name): name
  n.nameModule.newQualified(n.nameId.list.drop(5).string)

// Create a handle function name from an effect type name.
fun toHandleName(n: name): name
  makeHiddenName("handle", n)

fun isHandleName(n: name): bool
  n.hiddenNameStartsWith("handle")

// Create an operations type name from an effect type name.
fun toOperationsName(n: name): name
  makeHiddenName("ops", n)

// Is this an operations name?
fun isOperationsName(n: name): bool
  n.nameId.starts-with(".ops-").bool

fun toOpSelectorName(n: name): name
  makeHiddenName("select", n)

// Is this an operations name?
fun isOpSelectorName(n: name): bool
  n.nameId.starts-with(".select-").bool

//  Create an effect type name from an operations type name.
fun fromOpSelectorName(n: name): name
  n.nameModule.newQualified(n.nameId.list.drop(8).string)

// Create an effect tag name from an effect type name.
fun toEffectTagName(n: name): name
  makeHiddenName("tag", n)

// Create an operation type name from an operation name.
fun toOpTypeName(n: name): name
  makeHiddenName("op", n)

// Create an operation constructor name from an operation name.
fun toOpConName(n: name): name
  makeHiddenName("Op", n)

// Create an open tag name from a constructor name in an open type
fun toOpenTagName(n: name): name
  makeHiddenName("tag", n)

fun isOpenTagName(n: name): bool
  n.nameId.starts-with(".tag-").bool

// Create a name for a value operation
fun toValueOperationName(n: name): name
  makeHiddenName("val", n)

// Is this an name of a value operation?
fun isValueOperationName(n: name): bool
  n.nameId.starts-with(".val-").bool

// Create an operation name from a value operation name
fun fromValueOperationsName(n: name): name
  n.nameModule.newQualified(n.nameId.list.drop(5).string)

fun prepend(s: string, n: name): name
  val sl = s.list
  val new = match n.nameId.list
    Cons('.', t) -> match sl
      Cons('.', _) -> sl ++ t // Keep hidden names hidden
      _ -> Cons('.', sl ++ t)
    t -> sl ++ t
  newQualified(n.nameModule, new.string)

fun postpend(s: string, cname: name): name
  val (name, post) = cname.canonicalSplit
  newQualified(name.nameModule, name.nameId ++ s ++ post)

fun canonicalName(n: int, name: name): name
  if n == 0 then name else postpend(canonicalSep.string ++ n.show, name)

fun nonCanonicalName(n: name): name
  n.canonicalSplit.fst

fun canonicalSplit(n: name): (name, string)
  match n.nameId.list.reverse.span(is-digit)
    (Cons(p, ps), Cons(c, rest)) | c == canonicalSep -> (newQualified(n.nameModule, rest.reverse.string), Cons(c, Cons(p, ps).reverse).string)
    _ -> (n, "")

// camel-case to dash-case
fun camelToDash(s: string): string
  match s.splitCamel
    Cons(x, xs) -> x ++ xs.map(fn(y) -> "-" ++ y.to-lower).join
    _ -> ""

fun splitCamel(s: string): list<string>
  fun is-break(c: char): bool
    c.is-alpha || c == '-'
  if s == "" then Nil
  else
    match s.list
      Cons('-', cs) -> unsafe-decreasing(cs.string).splitCamel
      Cons(c, cs) -> 
        val (pre, post) = cs.span(is-break)
        if post.is-empty then
          val (pre2, post2) = post.span(fn(i) -> i.is-upper)
          if pre2.is-empty || post.is-empty && post2.head.default(' ').is-break then
            Cons(Cons(c, pre2).string, unsafe-decreasing(post2.string).splitCamel)
          else
            Cons(Cons(c, pre2.init).string, unsafe-decreasing(Cons(pre2.last(' '), post2).string).splitCamel)
        else
          Cons(Cons(c, pre).string, unsafe-decreasing(post.string).splitCamel)
      Nil -> Nil

// name to file path
fun moduleNameToPath(n: name): string
  n.show.asciiEncode(True)

fun pathToModuleName(p: string): name
  fun decode(s: string): string // TODO: Performance
    val sl = s.list
    match sl
      _ | s.starts-with("_dash_").bool -> "-" ++ decode(unsafe-decreasing(sl.drop(6).string))
      Cons('_', Cons('_', t)) -> "_" ++ decode(unsafe-decreasing(t.string))
      Cons('_', t) -> "/" ++ decode(unsafe-decreasing(t.string))
      Cons('.', t) -> decode(unsafe-decreasing(t.string))
      Cons('\\', t) -> "/" ++ decode(unsafe-decreasing(t.string))
      Cons(c, t) -> c.string ++ decode(unsafe-decreasing(t.string))
      Nil -> ""
  p.decode.list.drop-while(fn(c) -> c == '_' || c == '.' || c == '/').string.newName

fun asciiEncode(n: string, isModule: bool): string
  fun encode-char(pre: char, c: char, post: char): string
    match c
      '/' | isModule -> "_"
      '-' | !isModule && post.is-alpha-num -> "_"
      '.' | post.is-digit || post == ' ' || pre == ' ' -> "_"
      '_' -> "__"
      '.' -> "_dot_"
      '-' -> "_dash_"
      '/' -> "_fs_"

      '+' -> "_plus_"
      '*' -> "_star_"
      '&' -> "_amp_"
      '~' -> "_tilde_"
      '!' -> "_excl_"
      '@' -> "_at_"
      '#' -> "_hash_"
      '$' -> "_dollar_"
      '%' -> "_perc_"
      '^' -> "_hat_"
      '=' -> "_eq_"
      ':' -> "_colon_"
      '<' -> "_lt_"
      '>' -> "_gt_"
      '[' -> "_lb_"
      ']' -> "_rb_"
      '?' -> "_ques_"
      '\\'-> "_bs_"
      '(' -> "_lp_"
      ')' -> "_rp_"
      ',' -> "_comma_"
      ' ' -> "_space_"
      '\'' -> "_sq_"
      '\"' -> "_dq_"
      '`'  -> "_bq_"
      '{'  -> "_lc_"
      '}'  -> "_rc_"
      '|'  -> "_bar_"
      _ -> "_x" ++ c.int.show-hex(pre="", width=2) ++ "_"

  fun encodeChars2(s: list<char>): list<string>
    match s
      Cons(pre, Cons(c, Cons(post, rest))) -> Cons(encode-char(pre, c, post), encodeChars2(rest))
      Cons(pre, Cons(c, Nil)) -> Cons(encode-char(pre, c, ' '), Nil)
      Cons(pre, Nil) -> Cons(pre.string, Nil)
      Nil -> Nil
  
  fun encodeChars(s: string): string
    encodeChars2((" " ++ s).list).join

  if n.head-char.default(' ').is-alpha-num then
    return n.encodeChars
  match n
    ""      -> "_null_"
    ".<>"   -> "_total_"
    ".<|>"  -> "_extend_"
    ".()"   -> "_unit_"
    ".(,)"  -> "_tuple2_"
    ".(,,)" -> "_tuple3_"
    ".(,,,)"-> "_tuple4_"
    "()"    -> "_Unit_"
    "(,)"   -> "_Tuple2_"
    "(,,)"  -> "_Tuple3_"
    "(,,,)" -> "_Tuple4_"
    "[]"    -> "_index_"
    _ -> n.encodeChars
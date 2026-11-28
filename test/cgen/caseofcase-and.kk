// Regression test for exponential core-size blow-up in the simplifier's
// case-of-case transformation (Core/Simplify.hs).
//
// An `if/elif` chain whose conditions use `&&` desugars, per branch, to a
// `case` whose *scrutinee* is itself a `case` (the short-circuit `&&`). The
// case-of-case rule pushes the outer branches into the scrutinee's branches;
// since the outer `else` arm is the entire rest of the chain, an unguarded
// rule duplicates it at every level and the core grows as 2^n. With ~12
// `&&`-guarded branches this made the module effectively uncompilable.
//
// This function has enough such branches that, without the size guard on the
// duplicated branches, compiling it does not terminate in reasonable time.

struct st
  seed: int
  acc: int

fun next(s: st): div (st, int)
  val s2 = St((s.seed * 6364136223846793005 + 1442695040888963407) % 18446744073709551616, s.acc)
  (s2, (s2.seed / 65536) % 100)

fun step(s: st, k: int): div (st, int)
  val (s2, r) = next(s)
  if k >= 0 && r < 8 then
    val (s3, a) = next(s2)
    (s3, a + 1)
  elif k >= 1 && r < 16 then
    val (s3, a) = next(s2)
    (s3, a + 2)
  elif k >= 2 && r < 24 then
    val (s3, a) = next(s2)
    (s3, a + 3)
  elif k >= 3 && r < 32 then
    val (s3, a) = next(s2)
    (s3, a + 4)
  elif k >= 4 && r < 40 then
    val (s3, a) = next(s2)
    (s3, a + 5)
  elif k >= 5 && r < 48 then
    val (s3, a) = next(s2)
    (s3, a + 6)
  elif k >= 6 && r < 56 then
    val (s3, a) = next(s2)
    (s3, a + 7)
  elif k >= 7 && r < 64 then
    val (s3, a) = next(s2)
    (s3, a + 8)
  elif k >= 8 && r < 72 then
    val (s3, a) = next(s2)
    (s3, a + 9)
  elif k >= 9 && r < 80 then
    val (s3, a) = next(s2)
    (s3, a + 10)
  elif k >= 10 && r < 88 then
    val (s3, a) = next(s2)
    (s3, a + 11)
  elif k >= 11 && r < 96 then
    val (s3, a) = next(s2)
    (s3, a + 12)
  else (s2, r)

fun run(s: st, n: int): div st
  if n <= 0 then s
  else
    val (s2, v) = step(s, n % 12)
    run(St(s2.seed, s2.acc + v), n - 1)

pub fun main()
  val s = run(St(1, 0), 200)
  println(s.acc)

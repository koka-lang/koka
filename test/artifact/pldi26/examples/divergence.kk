// divergence.kk — Phantom Implicit for Divergence Detection
//
// "Syntactic Implicit Parameters with Static Overloading"
// Section 4.3: Phantom Implicits — Divergence
//
// Koka uses a single phantom implicit resolved by TYPE (not just name): 
//   ?hdiv : hdiv<h,a,e>
//
// The type :hdiv<h,a,e> is abstract — users cannot create values of it.
// Only the compiler may construct evidence for it, and only when:
//   - The value type :a contains a reference to heap :h
//   - The effect :e must include :div (divergence)
//
// This allows Koka to detect "sneaky divergence" via self-referential
// heap-allocated functions (Landin's knot pattern).
//
// Run with: stack run koka -- -e test/artifact/pldi26/examples/divergence.kk

module divergence


// ─────────────────────────────────────────────────────────────────────────────
// Landin's Knot
//
// Even though `landin` has no syntactic recursion, it diverges because
// `self` calls whatever is stored in `r`, and we store `self` in `r`.
//
// Koka infers the `:div` effect because the dereference `(!)` is defined as:
//
//   fun ref/(!)( r : ref<h,a>, ?hdiv : hdiv<h,a,e>) : <read<h>|e> a
//
// At the call `!r` inside `self`, the value type is `() -> <read<h>|e> ()`.
// Since this type CONTAINS `:h`, the compiler unifies `:e` with `:<div|_>`,
// and supplies an internal dummy as evidence for `?hdiv`.
// This is why the type of `landin` becomes `div ()` even without explicit annotation.
// ─────────────────────────────────────────────────────────────────────────────

fun landin() : _ ()           // :div is inferred by the compiler!
  val r = ref(fn() ())          // initialize r with a trivial function
  fun self()
    (!r)()                      // dereference r and call it — uses ?hdiv phantom
  r := self                     // overwrite r with the self-referential function
  self()                        // call it — diverges!


// ─────────────────────────────────────────────────────────────────────────────
// Abstracting over ?hdiv
//
// Just like ?kk-line can be threaded through functions, ?hdiv can be passed
// along to delay the divergence check to the call site.
//
// In `read2`, the type of `:a` at the dereference `!r` is polymorphic, so
// the compiler cannot immediately determine whether `:a` contains `:h`.
// By adding `?hdiv : hdiv<h,a,e>` as a parameter, we defer the check to
// whoever calls `read2`. The dereference elaborates to `ref/(!)(r, hdiv)`.
// ─────────────────────────────────────────────────────────────────────────────

fun read2( r : ref<h,a>, ?hdiv : hdiv<h,a,e> ) : <read<h>|e> (a,a)
  val x = !r        // elaborated to: ref/(!)(r, hdiv) — defers divergence check
  (x, x)


// ─────────────────────────────────────────────────────────────────────────────
// A non-diverging use of the dereference operator
//
// If the stored value type is simple (e.g., :int), Koka knows `:int` cannot
// contain heap `:h`, so no `:div` effect is added.
// ─────────────────────────────────────────────────────────────────────────────

fun read-int( r : ref<h,int> ) : read<h> int
  !r    // Non-diverging: int cannot contain h, no ?hdiv evidence needed

fun main() : <console, div, read<global>, write<global>> ()
  println("Divergence phantom implicit examples:")
  println("  `landin` would diverge if called — skipped")
  val r = ref(42)
  val (x, y) = read2(r)
  println("  read2 result: (" ++ x.show ++ ", " ++ y.show ++ ")")

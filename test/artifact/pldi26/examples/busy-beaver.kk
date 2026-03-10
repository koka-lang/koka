// busy-beaver.kk — Turing Machine Encoding in the Type System
//
// "Syntactic Implicit Parameters with Static Overloading"
// Appendix A: Turing Encoding of a 3-state Busy Beaver
//
// This file demonstrates that type checking with syntactic implicit parameters
// and static overloading is UNDECIDABLE in general. We can encode a Turing
// machine at the type level, where type checking reduces to deciding whether
// the machine halts.
//
// The 3-state busy beaver (by Ivars Peterson) has this transition table:
//
//   State | Sym0        | Sym1
//   ------+-------------+-------------
//     A   | write 1, R, B | write 1, L, C
//     B   | write 1, L, A | write 1, R, B
//     C   | write 1, L, B | write 1, N, HALT
//
// Encoding:
//   - Tape symbols are phantom types: `sym0`, `sym1`
//   - Tape is a cons list in the type language: `cons<head,tail>`
//   - `inf` represents an infinite tape of `sym0` values
//   - Machine state is encoded as types: `state-a`, `state-b`, `state-c`
//   - Configuration is `config<state, left-tape, right-tape>`
//   - Each transition is a FUNCTION definition, used by [var-qualify] to
//     elaborate `trans(start)` into a chain of implicit function calls
//
// If the type checker can find a valid elaboration of `trans(start)`, the
// Turing machine halts. Type checking == Turing machine simulation.
//
// Run with: stack run koka -- -e test/artifact/pldi26/examples/busy-beaver.kk

module busy-beaver

// ─────────────────────────────────────────────────────────────────────────────
// Tape symbol and state types (all phantom — carry no runtime values)
// ─────────────────────────────────────────────────────────────────────────────

type sym0
type sym1

// Tape: cons list of symbols at the type level
type cons<a,b>   // tape cell with head `a` and tail `b`
type inf         // infinite tape of sym0 (lazy representation)

// Machine states
type state-a
type state-b
type state-c


// ─────────────────────────────────────────────────────────────────────────────
// Machine configuration: (current-state, left-tape, right-tape)
// The head reads/writes the first symbol of the right tape.
// ─────────────────────────────────────────────────────────────────────────────

abstract type config<s,a,c>
  Start

// Initial configuration: state A, empty (infinite-zero) tape on both sides
val start : config<state-a, inf, cons<sym0,inf>> = Start


// ─────────────────────────────────────────────────────────────────────────────
// Transition functions
//
// Each function encodes one cell of the transition table.
// The implicit parameter `?trans` requires a proof that the NEXT configuration
// can be handled. The type checker searches for matching trans functions,
// building up the full execution chain implicitly.
// ─────────────────────────────────────────────────────────────────────────────

// Halting: no further implicit needed
fun halt/trans() : ()
  ()

// State A, sym0 → write 1, move Right, go to state B
fun a/b/trans( st : config<state-a, cons<x,l>, cons<sym0,r>>,
               ?trans : (config<state-b, l, cons<x, cons<sym1,r>>>) -> () ) : ()
  ()

// State A, sym1 → write 1, move Left, go to state C
fun a/c/trans( st : config<state-a, l, cons<sym1, cons<x,r>>>,
               ?trans : (config<state-c, cons<sym1,l>, cons<x,r>>) -> () ) : ()
  ()

// State B, sym0 → write 1, move Left, go to state A
fun b/a/trans( st : config<state-b, l, cons<sym0, cons<x,r>>>,
               ?trans : (config<state-a, cons<sym1,l>, cons<x,r>>) -> () ) : ()
  ()

// State B, sym1 → write 1, move Right, stay in state B
fun b/b/trans( st : config<state-b, cons<x,l>, cons<sym1,r>>,
               ?trans : (config<state-b, l, cons<x, cons<sym1,r>>>) -> () ) : ()
  ()

// State C, sym0 → write 1, move Left, go to state B
fun c/b/trans( st : config<state-c, l, cons<sym0, cons<x,r>>>,
               ?trans : (config<state-b, cons<sym1,l>, cons<x,r>>) -> () ) : ()
  ()

// State C, sym1 → write 1, no move, HALT
fun c/end/trans( st : config<state-c, l, cons<sym1,r>>,
                 ?trans : () -> () ) : ()
  ()


// ─────────────────────────────────────────────────────────────────────────────
// Infinite tape expansion rules
//
// The tape encoding is finite, so we must lazily expand `inf` when a
// transition requires reading/writing beyond the current finite portion.
// To avoid ambiguity, expansion is only triggered on the side that needs it.
// ─────────────────────────────────────────────────────────────────────────────

// Expand the left inf tape for state A shifting right (A+sym0 case)
fun inf/a/b/trans( st : config<state-a, inf, cons<sym0,r>>,
                   ?trans : (config<state-a, cons<sym0,inf>, cons<sym0,r>>) -> () ) : ()
  ()

// Expand the right inf tape for state A shifting left with sym1 on right
fun inf/a/c/trans( st : config<state-a, l, cons<sym1,inf>>,
                   ?trans : (config<state-a, l, cons<sym1, cons<sym0,inf>>>) -> () ) : ()
  ()

// Expand the left inf tape for state B shifting right with sym1
fun inf/b/b/trans( st : config<state-b, inf, cons<sym1,r>>,
                   ?trans : (config<state-b, cons<sym0,inf>, cons<sym1,r>>) -> () ) : ()
  ()

// Expand right inf tape for state B shifting left with sym0
fun inf/b/a/trans( st : config<state-b, l, cons<sym0,inf>>,
                   ?trans : (config<state-b, l, cons<sym0, cons<sym0,inf>>>) -> () ) : ()
  ()

// Expand right inf tape for state C shifting left with sym0
fun inf/c/b/trans( st : config<state-c, l, cons<sym0,inf>>,
                   ?trans : (config<state-c, l, cons<sym0, cons<sym0,inf>>>) -> () ) : ()
  ()

// C with sym1 halts, no need for transition

// ─────────────────────────────────────────────────────────────────────────────
// Run the machine!
//
// The type checker implicitly resolves the full 18-step execution chain:
//
//   inf/a/b/trans(?trans=a/b/trans(_,
//     b/a/trans(_,
//     inf/a/c/trans(_, a/c/trans(_,
//     inf/c/b/trans(_, c/b/trans(_,
//     inf/b/a/trans(_, b/a/trans(_,
//     a/b/trans(_,
//     b/b/trans(_,
//     b/b/trans(_,
//     b/b/trans(_,
//     inf/b/b/trans(_, b/b/trans(_,
//     b/a/trans(_,
//     a/c/trans(_,
//     c/end/trans(_,
//     halt/trans))))))))))))))))))
//
// This matches the well-known 13-step execution of the 3-state busy beaver (with 5 additional steps for infinite expansion).
// Hover over `trans(start)` in VS Code to see the inferred implicit chain.
// ─────────────────────────────────────────────────────────────────────────────

fun main()
  trans(start)    // The compiler must find a valid execution trace as elaboration
  println("done.")

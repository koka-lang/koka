# Koka quirks

Behaviour of the current compiler and standard library that is surprising, unsound, or a known bug. Each entry says how to avoid it.
Remove an entry when the underlying issue is fixed, and add one only after reproducing it.
Idioms and style are in [koka-best-practices.md](koka-best-practices.md).

## Compiler

- **`return` inside a `val`-bound `match` is miscompiled.** The compiler lifts the `match` into a helper function, and the `return` exits the helper instead of the enclosing function; the C backend then reports type mismatches or an undeclared `_return`. Hoist the early return above the `val`.

  ```koka
  val (a, b) = match x
    Nothing -> return (Nothing, Nothing)   // miscompiled
    Just(t) -> ...
  ```

- **Termination checking is factorial in the arity of a recursive function.** Adding a parameter to a large recursive function can make compilation dramatically slower. Split the function if checking it becomes slow.
- **Constructors larger than 128 words (roughly 100+ fields) crash when allocated.** Allocation always uses the small-object path. Split very wide records.

## Standard library

- **`std/os/task`'s `parallel` is unsound.** Its closures may use `exn`, a handled effect whose evidence lives on the spawning thread, so a worker that raises reads a foreign evidence vector and crashes. Use `compute` from `std/async/thread`, whose closures are `ioc` (async/thread not merged yet).
- **The effect system does not catch shared mutable state across threads.** `ioc` permits global state, so a top-level `delay` or a `var` captured in a global named handler, forced from two `compute` workers, is a cross-thread use-after-free. Keep top-level values immutable and strict in code that runs in parallel.

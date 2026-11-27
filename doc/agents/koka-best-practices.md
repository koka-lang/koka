# Writing Koka: best practices

How to write good Koka, for agents and people new to the language.
Behaviour that is surprising or wrong in the current compiler is in [koka-quirks.md](koka-quirks.md); working on the compiler itself is in the root [AGENTS.md](../../AGENTS.md).

Learn from the code first: `samples/`, `test/` and `lib/std` show the idioms below in use, and are more reliable than memories of other languages.

## Style

- Match the style of `lib/std`: as few braces as possible. Use indentation for blocks, `match` and `if` rather than `{ ... }` and `;`, and pass a final function argument as a trailing lambda (`xs.foreach fn(x)` followed by an indented body).
- Names are `kebab-case`. Use copy syntax to update a record: `obj(field = new-value)`.
- In compiler code, let the compiler infer effect rows: write `_` (or leave the row off) rather than spelling out rows that will drift.
- In library code (`lib/`), write effect rows out in full. They are part of the library's interface and its documentation.
- Do not try hard to remove `div`. Termination checking is best effort, and an inferred `div` is not a defect.
- Use static overloading. Functions are overloaded on the types of all their arguments, so the same name can serve several types. Disambiguate with a qualifier (`list/show`, `int/show`) only where the compiler asks for one; qualifiers are module or local (`fun string/cmp`) prefixes on the name.
- Use implicit parameters (`?(==) : (a,a) -> e bool`) for type-directed operations such as equality, comparison and `show`, where another language would use a type class.

## Effects

- Model state, configuration, logging, errors and other context as effects and handlers. Do not hand-thread state through parameters or return `either`/`result` values to avoid an effect; doing that throws away what the language is for.
- Prefer effects specific to your library or subsystem (`log`, `diag`, `fresh`) over general ones. Calling `println` or throwing `exn` everywhere cannot distinguish a subsystem effect from a true exception by a caller; a library effect can. `exn` and `console` fit the program's top level and calls into the standard library.
- Diagnostics raised as effect operations are not exceptions: `try` catches only `exn`. Contain them with a handler for that effect.
- Make handlers cheap: tail-resumptive operations (`fun`, `val`) cost far less than ones that capture the continuation (`ctl`). Use `final ctl` when the handler never resumes.
- If a profile blames effect machinery, reduce the number of handler installs or fix the compiler; do not rewrite the code out of effects.
- Use `std/async` for concurrency. For CPU parallelism use `compute` from `std/async/thread` (to be merged) with `interleaved`. Its work closure is `ioc`, which rules out handled effects whose evidence lives on another thread.

## Data and performance

- Koka uses reference counting with reuse: a uniquely owned value is updated in place. Write functional updates and let reuse fire, but avoid keeping an old value alive after you build its replacement, since that forces a copy.
- `var` and `ref` cells block reuse of what they hold. A `var` captured by a handler clause or closure becomes a heap `ref`, and every read duplicates its contents.
- To update a value held in a `var`, use `local-var/update` (ownership passes to the function, to be merged).
- Value types help only where the value lives inside another unboxed value. Elements of a `list` are boxed regardless.
- Effect operations cost a few nanoseconds each, roughly 10x a direct call. That is fine per declaration and too much per input character.
- Check whether reuse fired with `--showfcore`: look for `@reuse`, `@assign-reuse`, `@alloc-at` and `@reuse-drop`.

## Porting from Haskell

- Haskell is lazy and Koka is strict. Code that generates a large list and consumes it with a short-circuiting fold (`any`, `all`, `find`, `head`) holds one element at a time in Haskell and the whole list in Koka. Port such code as a search that stops early.
- Translate idioms rather than transliterating: type classes become overloading and implicits, monads become effects, `IORef` threading becomes `var` or an effect.
- Top-level values are shared by every thread. Never put lazy (`delay`) or mutable state at the top level of code that may run in parallel; forcing a thunk mutates it.

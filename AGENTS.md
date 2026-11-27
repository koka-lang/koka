# Working on the Koka compiler

Rules for anyone, human or agent, changing the compiler in this repository. Short on purpose.
Writing Koka itself (library code, tests, the self-hosted compiler) has its own guides: [doc/agents/koka-best-practices.md](doc/agents/koka-best-practices.md) and [doc/agents/koka-quirks.md](doc/agents/koka-quirks.md).

This repository holds two compilers, and this file names them the same way throughout:

- **The Haskell compiler** is `src/`. It is the compiler that ships, and the one `stack` builds and tests.
- **The self-hosted compiler** is the Koka compiler written in Koka. It is developed in full in [koka-community/compiler](https://github.com/koka-community/compiler); `compiler/` here holds the subset of its modules that build against this repository's standard library, so that a change to the Haskell compiler and the matching change to the self-hosted compiler can land together.

Each section below says which compiler it applies to. Sections marked **both** apply to all work in the repository.

## 1. Layout

| directory | what it is |
| --- | --- |
| `src/` | the Haskell compiler |
| `compiler/` | modules of the self-hosted compiler, ported from `src/` |
| `pretty/` | the pretty-printing library the self-hosted compiler prints through (`pretty/printer`, `pretty/pprint`); not part of `std` |
| `lib/` | the standard library (`std/...`), shared by both compilers |
| `kklib/` | the C runtime, shared by both compilers |
| `test/` | the test corpus, driven by `stack test` |

See [compiler/README.md](compiler/README.md) for which self-hosted modules are here, what each corresponds to in `src/`, and what blocks the rest.

## 2. Environment (both)

- Commands may run in `zsh` rather than `bash`; check which shell you have and write commands that work in both. zsh does not word-split unquoted parameter expansions, so do not rely on bash-style splitting: build argument lists as arrays and quote expansions.
- In zsh, a failed glob aborts that one command, not the rest of a `;` chain. Check the result before acting on it.
- New worktrees do not populate the mimalloc submodule. Run `git submodule update --init --recursive kklib/mimalloc` from the worktree root before compiling generated C, or `mimalloc.h` is missing.
- Run one heavy build at a time. Concurrent builds that share a build directory corrupt each other and invalidate timings.
- A BUILD DIRECTORY DOES NOT KNOW WHICH COMPILER WROTE IT. Its hash covers the flags and the compiler version, not the compiler binary, so artifacts written by a different compiler are reused silently. After changing the compiler, delete `.koka/v<version>-test` before running the suite, and give each experiment its own `--buildtag`.
- The compiler and `kklib` must come from the same tree. A compiler from another branch can emit calls to runtime functions this `kklib` does not have; the C compiler then reports undeclared `kk_*` functions.
- To compile a module that has no `main`, with either compiler, pass `--library`. Without it, `-c` builds a program and fails with `identifier <mod>/main cannot be found`.

## 3. Building and testing the Haskell compiler

- Build with `stack build --fast`. The compiler is `$(stack path --local-install-root)/bin/koka`; run it with `--sharedir=<repo root>` so it uses this checkout's `lib` and `kklib`.
- Test with `stack test --fast`, with `export LANG=C` set. Without it, the two Unicode tests (`cgen/unicode1`, `cgen/unicode2`) fail; do not investigate those failures.
- Keep `--fast` consistent across `stack` invocations. `stack.yaml` sets `rebuild-ghc-options: true` and `--fast` adds `-O0`, so switching between the two rebuilds the local packages. Type-check with `stack build --fast` and then run `stack test --fast`, so the test run reuses that build. A plain `stack test` rebuilds the compiler with `-O`, which takes about 15 minutes.
- Do not truncate the test output too far. Stack's closing error takes about 10 lines and hspec lists each failure in about 7, so use `2>&1 | tail -n 100`, or grep the full output for `FAILED`.
- `lib/time4` has failed intermittently and is slow even when it passes. If it fails and the failure does not obviously come from your change, report it rather than investigating.
- Never add regression tests to `test/Spec.hs`. Add a Koka test under `test/` with a `.kk.out` fixture, following the existing tests.
- When extending Core, keep its typing and the checkers that validate it (`Core/Check.hs`, `Core/CheckFBIP.hs`) in sync in the same change.
- Put a helper used by several passes in the module for its topic, not inside one of the passes.

## 4. The self-hosted compiler

### Building and checking

- Check every self-hosted module with `compiler/check.sh` after `stack build --fast`. It compiles each module in `compiler/` and `pretty/` with the Haskell compiler, and fails on an error, a warning, or a missing object file. CI runs it.
- Self-hosted modules build against `lib/` as it is in this repository. Do not add to `std` to make a module build; a needed addition is discussed and merged on its own first (see the blocker list in `compiler/README.md`).

### Keeping it in sync with the Haskell compiler

- A change to a file in `src/` that has a counterpart in `compiler/` updates the counterpart in the same pull request. If that is not possible, the pull request says why, and the module's row in `compiler/README.md` records that it is behind.
- Each ported module's header records the Haskell compiler commit it was last brought up to date with (`Updated as of <date> Commit: <hash>`). Update it when you sync the module.
- Port structurally, so the two can be diffed: keep function and module names recognisable, and translate idioms rather than redesigning:

  | Haskell compiler | self-hosted compiler |
  | --- | --- |
  | type classes | overloading and implicit parameters |
  | monads and monad transformers | effects and handlers |
  | `IORef` / `MVar` state threading | `var` / `ref`, or an effect |
  | explicit dictionary passing | implicits |

- DIVERGE ONLY WITH A REASON, AND WRITE THE REASON DOWN, in a comment at the divergence. An undocumented divergence is a bug that will be found much later.
- Before concluding "the Haskell compiler does X", read its source in `src/`, not its output or your memory of it.
- If porting turns up a bug, fix it in the Haskell compiler first, with a test, then in the self-hosted compiler.
- Do not remove code from a self-hosted module because nothing here calls it. The module is a copy of koka-community/compiler's, where the code may be used.
- Changes made to a module here are made to the same module in koka-community/compiler too, so that the copies stay identical.
- Known problems in self-hosted modules that are not fixed yet are listed in `compiler/port-todo.md`. Add an entry when you find one you are not fixing, and remove it when the fix lands in both repositories.

## 5. Coding standards (both)

- Aim for elegant code: relatively simple functions and small changes that keep or improve the quality of what they touch.
- Match the surrounding code: naming, comment density, layout. A diff should be hard to pick out of the file it lands in.
- Do not reformat or reorganise code you are not otherwise changing.
- Koka names are `kebab-case`. Qualify a name (`list/is-prefix-of`) when it is ambiguous; the compiler tells you when it is.
- Comments say why, not what. A comment that records a measurement or a non-obvious constraint is the most valuable thing in a file; one that restates the code is noise.
- COMMENTS STATE INVARIANTS, NOT LINEAGE OR HISTORY. Say what must hold and why, in the present tense. No "used to be X", "previously we Y", or stale invariants left beside the rule that replaced them. History belongs in commit messages.
- In the self-hosted compiler, cite the Haskell compiler only where the port differs from it, or where behaviour exists only because the Haskell compiler requires it. "Matches the Haskell compiler" says nothing about the code.
- When you record a number, record how it was measured.
- There is no line-length limit, in Koka or in Markdown. Break comments and Markdown paragraphs naturally at sentence punctuation, never at a fixed column, and keep bullet points short and on one line.

## 6. Validation (both)

- Correctness comes from an argument, not from tests passing. Derive a fix from the invariant it restores; a test confirms the argument, it does not replace it. Prefer the simple general rule to a special case.
- Make all the changes first; there is no need to run tests while coding. Then use focused checks (one test, `compiler/check.sh`, a small program), and run the whole suite only when a change could plausibly break something beyond them.
- Report exactly what was tested, including when nothing was.
- MINIMISE FIRST. Find the smallest input that shows the problem before investigating it.
- REPRODUCE BEFORE DIAGNOSING. A failure you cannot reproduce on demand is one you cannot verify you fixed. Seen once is not a bug yet: write down the exact conditions and try to reproduce them.
- Assert a positive signal. The binary exists, the count went up, the symbol is present. Exit 0 with no output has meant "silently did nothing" more than once.
- A guard that fails open is worse than no guard. Never suppress stderr in a check, and confirm a new check fires on a known-bad case before trusting it to be quiet.
- Add a regression test for each fixed bug, under `test/`, with a `.kk.out` fixture.
- CI on `dev` passes. If the suite fails on your branch, your branch caused it; find out how rather than calling the failure flaky or pre-existing. Confirm a pre-existing failure by running the suite on `dev`.

## 7. Debugging (both)

- Trace, do not guess. When a hypothesis costs a build cycle to test, instrument the code and read the actual state, or use a debugger (`lldb`) first.
- When a fix does not change the symptom, the mechanism is elsewhere. Do not layer a second fix on the first; revert it.
- Do not leave a half-fix in the tree. Revert it and record what you found.
- `--showcore` / `--showfcore` and `--checkcore` show and validate the Core after each phase; check a pass's output there before blaming a later one.

## 8. Performance (both)

- Measure before claiming a win, and measure the workload that is actually the target.
- Profile before blaming algorithmic complexity. Confirm an optimisation with a profile breakdown before and after: the targeted symbol's share must move, not just the wall clock.
- A wall-time difference smaller than the run-to-run spread is noise.
- On macOS, profile Koka binaries with `--cclinkopts=-Wl,-no_deduplicate`. Identical-code folding merges `kklib`'s refcount helpers into one symbol and makes the attribution wrong.
- For comparable macOS benchmarks, select the C compiler explicitly (`--cc=/usr/bin/clang` for the Apple Clang baseline) and give each C compiler its own build directory. Apple Clang and Homebrew LLVM share the `clang-release` cache name, so a cached `kklib.o` from the other compiler is reused otherwise.
- Do not give up guarantees for unmeasurable gains. An optimisation that the correctness of something depends on must not depend on the optimisation level.

## 9. Commits and pull requests (both)

- Commit a change once it looks good and has been approved. Do not push until asked.
- Stage files explicitly, so local plans and notes are never committed.
- One logical change per pull request, with a test where behaviour changes.
- The pull request description states what changed, why, and how it was verified (which tests, which measurements).

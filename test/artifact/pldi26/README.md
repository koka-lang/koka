# PLDI 2026 Paper Artifact: Syntactic Implicit Parameters with Static Overloading

[dockerhub]:  https://hub.docker.com/repository/docker/daanx/pldi26-implicits/general
[Zenodo]:     https://zenodo.org/records/TODO

# Getting Started

We provide a Docker image based on Ubuntu 22.04 (for both `x64` and `arm64`).
The image includes a pre-built version of Koka and all the example files from
the paper. For convenience we also uploaded the image to [dockerhub]:

```
> docker pull daanx/pldi26-implicits:1.0-x64
> docker run -it daanx/pldi26-implicits:1.0-x64
```
or on macOS Apple silicon:
```
> docker pull daanx/pldi26-implicits:1.0-arm64
> docker run -it daanx/pldi26-implicits:1.0-arm64
```

Once inside the container, the working directory is `/artifact/koka` (the koka repository root).

## Local Installation

It is also straightforward to run the artifact directly on Linux or macOS.
Install [Stack](https://docs.haskellstack.org/en/stable/) and then:

```
> git clone --recursive https://github.com/koka-lang/koka -b artifact/syntactic-implicits
> cd koka
> stack build
> stack run koka -- --version
```

You can then run any of the example files as:
```
> stack run koka -- -e test/artifact/pldi26/examples/intro.kk
```

Or open the examples in VS Code with the [Koka extension](https://marketplace.visualstudio.com/items?itemName=koka.language-koka)
to see inlay hints displaying inferred implicits inline (press `Ctrl+Alt` to toggle / `Ctrl+Option` on MacOS).


> **Tip (Docker / CI):** To avoid building the language server (which is not
> needed to run examples), build only the `koka-plain` target:
> ```
> > stack build :koka-plain
> > stack run koka-plain -- -e test/artifact/pldi26/examples/intro.kk
> ```

# Claims

## Claims Validated by This Artifact

The following claims from the paper are demonstrated empirically by running the
example files. Each example compiles, elaborates, and produces the expected output.

| Claim | Section | Example file |
|---|---|---|
| Implicit parameters are resolved by name from the caller's scope | §2 / Fig. 3 `[inst-implicit]` | `intro.kk` |
| Static overloading: a plain name is elaborated to a unique qualified name based on type context | §2 / Fig. 4 `[var-qualify]` | `overloading.kk` |
| Ambiguous overloaded names are rejected when the type context is insufficient | §2 | `overloading.kk` (`wrong/ambiguous`) |
| Inner-scope definitions take priority over outer-scope ones without ambiguity errors | §2.9 `[var-scope]` | `scope.kk` |
| The full system (without termination restriction) can encode a Turing machine at type-check time | §3.1 / Appendix A | `busy-beaver.kk` |
| History-based termination prevents divergence during implicit resolution | §3.3 | `busy-beaver.kk` (terminates) |
| The `default/` namespace is treated as scope depth −1, losing to any concrete definition | §4.1 | `comparison.kk`, `scope.kk` |
| Equality can be derived from comparison via `default/(===)`, with `int/(===)` overriding it | §4.1 | `comparison.kk` |
| Struct grouping and `.?` dot-unpacking expose struct fields as plain implicit names | §4.2 | `grouping.kk` |
| Deriving a sub-"class" from a super-"class" via a `base` field and a `default/` derivation rule | §4.2 | `grouping.kk` (`monadplus`) |
| Phantom implicits (`?kk-line`, `?kk-file`) are supplied by the compiler at the call site | §4.3 | `phantom.kk` |
| Phantom implicits can be abstracted over: if in scope as a parameter, they are threaded through | §4.3 | `phantom.kk` (`assert-fline`) |
| The `?hdiv` phantom implicit detects sneaky divergence via heap-reference analysis | §4.3 | `divergence.kk` |
| The features are deployed in the Koka standard library | §4 | `lib/std/core.kk`, `samples/learn/implicits.kk` |

## Claims Not Validated by This Artifact

The following claims appear in the paper but are **not** verified by running the examples:

- **Formal metatheory (type soundness, coherence).** The paper states and proves type-soundness and coherence results for the elaboration calculus (Section 2, Theorems in the appendix). These are pen-and-paper proofs; there is no machine-checked proof assistant (Coq, Agda, etc.) included in this artifact. Though we are working on one.

- **General decidability with history-based termination.** The paper proves that implicit resolution always terminates when the history-based chain check is enabled (§3.3). The `busy-beaver.kk` example illustrates this works for a concrete 14-step Turing execution, but the general termination theorem is not verified by this artifact. We have a cutoff parameter to safeguard just in case.

- **Uniqueness of elaboration (∃!e).** The paper claims that when a unique implicit candidate exists it is found unambiguously. This is demonstrated by the examples but not formally proven by the artifact.

- **Performance.** No benchmarks are included. The artifact does not make or validate any claims about compile-time or run-time performance.


# Step-by-Step Guide

All Koka example files for this artifact are in `test/artifact/pldi26/examples/`.

**All commands below assume you are in the root of the koka repository** (the `koka/` directory after cloning, or `/artifact/koka` inside the Docker container).

**Note: names for many examples are changed to avoid overlapping with the standard library (e.g. `showx` instead of `show`). The paper uses the unqualified names illustratively; the semantics are identical.**

**Reading elaboration notation.** Throughout the example files and the VS Code inlay hints, elaborated terms are written in comments using `?name=value` syntax to show which implicit argument was inferred. When the inferred value is a function with implicit parameters of its own, it is written in *eta-abbreviated* form using `_` as a placeholder for the regular argument:

```
list/showx(_, ?showx=int/showx)
```

This is shorthand for the lambda expression:
```
fn(xs) list/showx(xs, ?showx=int/showx)
```

The `_` makes it clear that this is a partially applied function — the regular argument `xs` is not shown because it is redundant. Nested implicits appear recursively:
```
list/showx(_, ?showx=list/showx(_, ?showx=int/showx))
-- meaning: fn(xs) list/showx(xs, ?showx=fn(ys) list/showx(ys, ?showx=int/showx))
```

## Section 1 – Introduction: Basic Implicits and Overloading

Run the introductory examples:
```
stack run koka -- -e test/artifact/pldi26/examples/intro.kk
```

This file demonstrates:

- `show-int` with an implicit `?base` parameter (base-conversion with dynamic base)
- `int/showx`, `float64/showx` — statically overloaded `showx` functions
- `list/showx` — combining static overloading with implicit parameters
- `showx([1])` elaborating to `list/showx([1], ?showx=int/showx)` automatically
- `showx([[1],[2]])` elaborating recursively to `list/showx([[1],[2]], ?showx=list/showx(_, ?showx=int/showx))`
- `tuple/showx` — multiple implicit parameters via qualified names `?fst/showx` and `?snd/showx`

## Section 2 – Static Overloading (§2 of the paper)

Run the overloading examples:
```
stack run koka -- -e test/artifact/pldi26/examples/overloading.kk
```

This demonstrates the `[var-qualify]` rule: a plain name `showx` is elaborated
to a fully qualified name `int/showx` or `list/showx` based on the local type context.
The file also shows that `wrong/ambiguous` (using the `wrong/` namespace so the error
becomes a warning) is correctly _rejected_ as ambiguous when the element type is unknown.

## Section 3 – Scope-Based Disambiguation (§2.9 of the paper)

Run the scope disambiguation examples:
```
stack run koka -- -e test/artifact/pldi26/examples/scope.kk
```

This demonstrates that inner-scope definitions are preferred over outer-scope
ones (the `[var-scope]` rule):

- A local `myint/showx` is preferred over the global `int/showx`
- The `default/` namespace is treated as outside the global scope (scope depth −1), so specific definitions like `int/(===)` always win over defaults for matching types

## Section 4 – Undecidability and History-Based Termination (§3.1 / Appendix A of the paper)

The paper shows that the full system (without restriction) can encode a Turing machine
at the type level. Run the 3-state busy-beaver example from the supplementary material:

```
stack run koka -- -e test/artifact/pldi26/examples/busy-beaver.kk
```

This program elaborates `trans(start)` into a chain of 18 state transitions entirely
at type-checking time. Hovering over `trans(start)` in VS Code shows the full
inferred implicit chain.

A generalized `inf/l/trans` rule showing how higher-order
implicit parameters can subsume individual inf-expansion rules is shown in the `busy-beaver-higher-order.kk` file:

```
stack run koka -- -e test/artifact/pldi26/examples/busy-beaver-higher-order.kk
```

## Section 5 – Equality, Comparison, and Default Implementations (§4.1 of the paper)

Run the comparison examples:
```
stack run koka -- -e test/artifact/pldi26/examples/comparison.kk
```

This file shows:

- `int/cmpx`, `char/cmpx`, `list/cmpx` — comparison functions
- `default/(===)` — deriving equality from comparison for free
- `int/(===)` — a more specific, efficient equality for `int` that is preferred over default via scope disambiguation
- `(!==)` — derived inequality using `?(===)` implicit

## Section 6 – Grouping Operations (§4.2 of the paper)

Run the grouping examples:
```
stack run koka -- -e test/artifact/pldi26/examples/grouping.kk
```

This shows how to group multiple operations into a single implicit parameter
using a struct, and how Koka's dot-unpacking syntax (`.?num`) automatically
exposes the struct fields:

- `num<a>` struct grouping `(+)` and `(*)`
- `int/num`, `float/num` "instances"
- `fadd` using `.?num` dot-unpacking
- `monoid<a>` struct grouping a zero and a combining operation
- `intplus/monoid`, `intmul/monoid`, `string/monoid` "instances"
- `monad<m>` struct grouping `pure` and `bind`, using `.?monad` dot-unpacking
- `monadplus<m>` extending monad with a `base : monad<m>` field and a choice operation
- `default/monadplus/monad` — automatically deriving `monad` from `monadplus`
- `list/monad`, `maybe/monad`, `list/monadplus` "instances"

## Section 7 – Phantom Implicits (§4.3 of the paper)

Run the phantom implicit examples:
```
stack run koka -- -e test/artifact/pldi26/examples/phantom.kk
```

This demonstrates phantom implicits — implicit parameters whose values are supplied by the compiler rather than by name lookup:

- `?kk-line : string` — the current source line number (the paper uses `:int` for illustration)
- `?kk-file : string` — the current source file name
- `assert-line` and `assert-fline` — can abstract over phantom implicits, allowing them to delay resolution to the original call site

Checking divergence with `?hdiv` is illustrated in `examples/divergence.kk`:
```
stack run koka -- -e test/artifact/pldi26/examples/divergence.kk
```

## Section 8 – Implementation in the Koka Standard Library

The features described in the paper are heavily used in the Koka standard library.
Relevant files:

- `lib/std/core.kk` — `show`, `(==)`, `(<)`, `(<=)`, etc. using overloading and implicits
- `lib/std/core/types.kk` — `order` type and `cmp` functions
- `samples/learn/implicits.kk` — a guided tutorial on implicits in Koka
- `samples/learn/qualifiers.kk` — a tutorial on qualified names and static overloading

To run the learning sample:
```
stack run koka -- -e samples/learn/implicits.kk
```


# Structure of the Artifact

```
test/artifact/pldi26/
  README.md               -- this file
  Dockerfile              -- build instructions for the Docker image
  examples/
    intro.kk              -- §1: introduction examples (show-int, list/showx, tuple/showx)
    overloading.kk        -- §2: static overloading and implicit parameters
    scope.kk              -- §2.9: scope-based disambiguation
    busy-beaver.kk              -- §3.1 / Appendix A: Turing machine encoding (specific rules)
    busy-beaver-higher-order.kk -- §3.1 / Appendix A: generalized higher-order Turing encoding
    comparison.kk         -- §4.1: comparison, equality, default namespace
    grouping.kk           -- §4.2: grouping with structs and dot-unpacking
    phantom.kk            -- §4.3: phantom implicits (?kk-line, ?kk-file)
    divergence.kk         -- §4.3: divergence phantom implicit (?hdiv)
```

## Where the Features Are Implemented

The implementation of syntactic implicit parameters and static overloading lives primarily in the Koka type inference code:

### Syntactic Implicit Parameters (`[inst-implicit]` rule — Figure 3 in the paper)

**`src/Type/InferMonad.hs`**
- `resolveImplicitName` (line ~884): entry point for resolving an implicit parameter by name
- `resolveImplicitArg` (line ~1047): resolves an implicit argument
- `resolveImplicitArgEx` (line ~1057): the core search with history tracking
- `resolveUniquely` (line ~1086): implements the `∃!e` condition — explores all candidates and selects a unique non-ambiguous, non-infinite result

**`src/Type/Infer.hs`**
- `inferImplicitParam` (line ~2000): handles `?x` parameters in function definitions
- `inferImplicitUnpack` (line ~2017): implements `.?` dot-unpacking of struct implicits
- `inferAppFunFirst` (line ~1251): the `[app-var]` rule — infers enough argument types to disambiguate an overloaded function name

### Static Overloading (`[var-qualify]` rule — Figure 4 in the paper)

**`src/Type/InferMonad.hs`**
- `lookupAppName` (line ~845): top-level dispatch for name resolution
- `lookupNameCtx` (line ~1277): looks up a name in the type environment, filters candidates using the type context to resolve qualified names

**`src/Type/Infer.hs`**
- `inferVar` (line ~1559): the main variable inference function; uses `resolveName` to trigger `[var-direct]` or `[var-qualify]`
- `inferVarName` (line ~1597): finishes resolving and instantiates variables, possibly replacing a compilation constant (`kk-line`, etc).

### Scope-Based Disambiguation (§2.9 of the paper)

**`src/Type/InferMonad.hs`**
- `resolveUniquely` (line ~1086): prefers definitions from inner scopes; once a solution from a deeper scope is found, outer alternatives are skipped (condition on line ~1098)

### History-Based Termination (§3.3 of the paper)

The termination check spans three tightly coupled pieces in **`src/Type/InferMonad.hs`**:

**Chain construction** — `resolveImplicitArgEx` (line ~1057) and `resolveImplicitParameters` (line ~1129). Every time a candidate is selected and its own implicit parameters are recursively resolved, the candidate's `(Name, NameInfo, Rho)` triple is prepended to the `chain` list before recursing. The chain therefore records the full sequence of `(qualified-name, instantiated-type)` pairs explored so far in this derivation.

**Termination check** — `isDecreasingChain` (line ~1175) Before recursing into a candidate in `resolveUniquely` (line ~1103–1110), the guard `not (isDecreasingChain chain ctx qname rho)` is evaluated.`isDecreasingChain` filters the chain to prior occurrences of the same qualified name, and requires that the instantiated type's *weight* is strictly decreasing relative to the maximum weight seen in the last `decreasingWithin = 4` occurrences. If the type is not decreasing, the candidate is marked `Infty` (an infinite-chain failure) and pruned rather than explored. This becomes immediately ambiguous.

**Weight measure** — `weight` / `weightType` / `weightParams` (line ~1185) The weight of an instantiated type is the total number of type constructors (`TCon` nodes) appearing in the implicit-parameter positions of that type. This is a structural size measure on types that is guaranteed to be a well-founded order, ensuring that any strictly decreasing sequence is finite.

**Safety depth cutoff** — `resolveMaxChainDepth = 32` (line ~1001) As an additional safeguard, if the chain exceeds 32 entries all remaining candidates are immediately marked `Infty` and exploration stops. The comment notes this is no longer strictly required once the decreasing-chain check is in place, but it prevents pathological compile times during development.

**Flag to disable the check** — `--infchain` (`src/Compile/Options.hs` line ~521) The compiler flag `--infchain` sets `allowInfiniteChains = True` in the `Env`, read at line ~1049 and threaded through all recursive calls. When set, the `isDecreasingChain` guard is skipped entirely. The `busy-beaver.kk` example compiles successfully *without* this flag because the chain of instantiated types is genuinely decreasing in weight. The flag exists to allow programs whose derivations are finite but happen not to satisfy the decreasing-weight criterion (at the cost of potentially non-terminating type-checking). It is off by default.

### Default Namespace (§4.1 of the paper)

**`src/Type/Assumption.hs`**
- `createNameInfoEx` (line ~421): when creating a `NameInfo` for a name that `isInDefaultNameSpace`, it sets the `infoScopeDepthX` field to `−1` (rather than the module-level `0`). The key line is: `d = if scopeDepth == 0 && isInDefaultNameSpace name then -1 else scopeDepth`
- `infoScopeDepth` (line ~85): accessor for the `infoScopeDepthX` field used everywhere scope depths are compared

**`src/Type/InferMonad.hs`**
- `lookupNameCtx` / `compareScopeDepth` (line ~1277): reads `infoScopeDepth` from each candidate's `NameInfo` and, via `filterInnerScopes`, drops any candidate whose scope depth is less than another candidate's — so `default/` definitions (depth −1) are always outcompeted by any concrete user definition (depth ≥ 0)

### Phantom Implicits (§4.3 of the paper)

**`src/Type/Infer.hs`**
- `compilationConstants` (line ~1660): a table mapping `kk-line`, `kk-file`, and `kk-module` to their types and compile-time value generators. `kk-line` and `kk-file` are regular named values in `lib/std/core/debug.kk`; normal implicit name lookup resolves them, but when `inferVarName` evaluates the resolved name it checks `compilationConstants` and replaces the variable with the current source location literal.

**`src/Type/InferMonad.hs`**
- `resolveHeapDivConstraint` (line ~1766): the `hdiv` phantom implicit is resolved by checking whether the value type can contain a heap reference, then unifying the effect with `:div`

### Syntax and Parsing

**`src/Syntax/Parse.hs`** — parses `?name` implicit parameter syntax, `.?name` dot-unpacking, and the `default/` namespace prefix.

**`src/Common/Name.hs`** — qualified name representation, `isImplicitParamName`, `isInDefaultNameSpace`, `unqualify`, `qualify` helpers.

# Notes

## Installing from Scratch

See the `Dockerfile` for precise build instructions on Ubuntu 22.04.
The essential steps on macOS or Linux are:

1. Install [Stack](https://docs.haskellstack.org/en/stable/)
2. Clone the artifact branch:
   ```
   git clone --recursive https://github.com/koka-lang/koka -b artifact/syntactic-implicits
   cd koka && stack build
   ```
3. Run any example:
   ```
   stack run koka -- -e test/artifact/pldi26/examples/intro.kk
   ```

## VS Code Inlay Hints

The Koka VS Code extension can display inferred implicit arguments as inlay hints. Press `Ctrl+Alt` (or `Ctrl+Option` on macOS) to toggle them. This makes it easy to see how `showx([1])` elaborates to `list/showx([1], ?showx=int/showx)` without modifying the source file. For nested cases, the hints use the same eta-abbreviated `_` notation described in the **Reading elaboration notation** note above — for instance, `showx([[1]])` shows `?showx=list/showx(_, ?showx=int/showx)`. The extension is available on the [VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=koka.language-koka).

# PLDI 2026 Paper Artifact: Syntactic Implicit Parameters with Static Overloading

[dockerhub]:  https://hub.docker.com/repository/docker/daanx/pldi26-implicits/general
[Zenodo]:     https://zenodo.org/records/19057777

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

When using [Zenodo] (doi:`10.5281/zenodo.19057777`) provided `tar.gz` files, 
use the `docker load -i <image>` command instead of `docker pull`, for example:
```
> gunzip pldi26-implicits-1.0-x64.tar.gz
> docker load -i pldi26-implicits-1.0-x64.tar
> docker run -it daanx/pldi26-implicits:1.0-x64
```

Once inside the container, the working directory is `/artifact/koka` (the koka repository root).


## Local Installation

It is also straightforward to build the artifact directly on Linux or macOS.
Install [Stack](https://docs.haskellstack.org/en/stable/) and then:

```
> git clone --recursive https://github.com/koka-lang/koka -b artifact/syntactic-implicits koka
> cd koka
> stack build --fast
> stack exec koka -- --version
```

If installed locally, one can also load the examples in VS Code while using the 
[Koka extension][koka-vscode]. This is nice as it uses inlay hints to directly 
show the full elaboration inside the editor (press `Ctrl+Alt` (or `Ctrl+Option` 
on MacOS) to toggle inlay hints).

[koka-vscode]: https://marketplace.visualstudio.com/items?itemName=koka.language-koka


## Running the Examples

You can then run all examples from the paper as:

```
> stack exec koka -- -e test/artifact/pldi26/implicits.kk
```

and the 3-state busy beaver example from the appendix as:

```
> stack exec koka -- -e test/artifact/pldi26/busy-beaver.kk
```


# Validation

We have two example files that validate the claims in the paper:

- `implicits.kk`: this contains all code examples in the paper and shows:
  1. We have a full implementation of the system described the paper.
  2. We have also examples of programs that should be rejected in our system.
  3. We have implemented all extensions suggested in Section 4 of the paper
     (including the `default/` namespace).
  4. We also have implemented `kk-line`/`kk-file` that use special constants
     that can be supplied by the compiler (Section 4.3)
  5. We also have implemented the `hdiv` implicit that requires type information
     to be resolved and is essential to detect potential divergence when
     using mutable state (Section 4.3.1)

- `busy-beaver.kk`: an implementation of a 3-state busy beaver program
  that executes as part of type checking. The history-based termination check 
  as desrcibed in Section 3.3 is essential here to allow it to compile.


## Implicits.kk

The `test/artifact/pldi26/examples/implicits.kk` file contains all code examples
from the paper in order of appearance and organized per section.  It type checks
the examples, elaborates them, compiles, and executes:

```
> stack exec koka -- -e test/artifact/pldi26/implicits.kk
```

The expected output is:

```
...
test/artifact/pldi26/implicits.kk(56,19): type warning: identifier myshow cannot be resolved.
  context      :                   myshow(x)
  inferred type: _
  candidates   : bool/myshow     : (x : bool) -> string
                 character/myshow: (x : char) -> string
                 int/myshow      : (x : int) -> string
                 tuple/myshow    : forall<a,b> ((a, b), ?fst/myshow : (a) -> string, ?snd/myshow : (b) -> string) -> string
                 list/myshow     : forall<a> (xs : list<a>, ?myshow : (a) -> string) -> string
  hint         : give a type annotation or qualify the name?

test/artifact/pldi26/implicits.kk(76, 3): type warning: identifier myshow cannot be resolved
  context      :   myshow([])
  inferred type: (list<_1594>) -> _
  candidates   : list/myshow(_,bool/myshow)
                 list/myshow(_,character/myshow)
                 list/myshow(_,int/myshow)
                 list/myshow(_,list/myshow)
                 list/myshow(_,tuple/myshow)
  hint         : qualify the name?

test/artifact/pldi26/implicits.kk(115, 3): type warning: identifier foo cannot be resolved
  context      :   foo(1)
  inferred type: (int) -> _
  candidates   : foo(_,foo(_,foo(_,foo(_,foo(_,...)))))
  hint         : qualify the name?

section 1:
42
2A
'a'
'a'
1
1::2::[]
1::2::[]
1::[]::2::[]::[]

section 2:
3
(1,'a')
(1,'a'::[])::(2,'b'::[])::[]

section 4:
(1,*)
3
False
custom assertion failed at 188: test1
()
custom assertion failed at 195: test/artifact/pldi26/implicits.kk: test2
()
(1,1)

done.
```

The three type warnings show examples from the paper that should be rejected.
Such examples are qualified with the name `wrong/` in Koka such that the errors
become warnings and their definitions are omitted from the executable.


## 3-State Busy Beaver

Section 3.1 of the paper discusses termination of the type checker and
shows how we can encode a 3-state busy beaver program on the type level.
Type checking essentially executes the Turing machine at compilation time. 
The `busy-beaver.kk` file contains the full implementation and can be
run as:

```
> stack exec koka -- -e test/artifact/pldi26/busy-beaver.kk
```

The expected output is:

```
...
done.
```

showing that it type checked. The interesting part is of course the 
elaboration which follows the execution steps of the corresponding 
Turing machine. This can be seen nicely in the VS Code editor in 
the `main` function:

```koka
fun main()
  trans(start)    // The compiler must find a valid execution trace as elaboration
  println("done.")
```

When hovering over the `trans` identifier, VS Code will show the 
full elaboration of the implicit parameters, namely:

```koka
inf/a/b/trans(start,?trans=a/b/trans(_,
  b/a/trans(_,
  inf/a/c/trans(_, a/c/trans(_,
  inf/c/b/trans(_, c/b/trans(_,
  inf/b/a/trans(_, b/a/trans(_,
  a/b/trans(_,
  b/b/trans(_,
  b/b/trans(_,
  b/b/trans(_,
  inf/b/b/trans(_, b/b/trans(_,
  b/a/trans(_,
  a/c/trans(_,
  c/end/trans(_,
  halt/trans))))))))))))))))))
```

This information is not readily available from the command line though. 
One way to see the elaboration is to display the initial core 
generated from the type checker using the `--showicore` option:
```
> stack exec koka -- --showicore -e test/artifact/pldi26/busy-beaver.kk
``` 


## Usage in the Koka Standard Library

The features described in the paper are heavily used in the Koka standard library.
Some relevant files:

- `lib/std/core/*.kk`: `show`, `cmp`, `(==)` etc. using overloading and implicits
- `samples/learn/implicits.kk`: a tutorial on implicits in Koka
- `samples/learn/qualifiers.kk`: a tutorial on qualified names and static overloading


## Implementation Notes

The implementation of syntactic implicit parameters and static 
overloading lives primarily in the Koka type inference code. The following
descriptions are a bit beyond the scope of the artifact but we hope it can 
be useful when trying to understand how it is implemented.

- Syntactic Implicit Parameters 

  (`[inst-implicit]` rule — Figure 3 in the paper)

  `src/Type/InferMonad.hs`
  - `resolveImplicitName` (line ~884): entry point for resolving an implicit parameter by name
  - `resolveImplicitArg` (line ~1047): resolves an implicit argument
  - `resolveImplicitArgEx` (line ~1057): the core search with history tracking
  - `resolveUniquely` (line ~1086): implements the `∃!e` condition — explores all candidates and selects a unique non-ambiguous, non-infinite result

  `src/Type/Infer.hs`
  - `inferImplicitParam` (line ~2000): handles `?x` parameters in function definitions
  - `inferImplicitUnpack` (line ~2017): implements `.?` dot-unpacking of struct implicits
  - `inferAppFunFirst` (line ~1251): the `[app-var]` rule — infers enough argument types to disambiguate an overloaded function name

- Scope-Based Disambiguation (§2.9 of the paper)
  `src/Type/InferMonad.hs`
  - `resolveUniquely` (line ~1086): prefers definitions from inner scopes; once a solution from a deeper scope is found, outer alternatives are skipped (condition on line ~1098)

- History-Based Termination (§3.3 of the paper)

  The termination check spans three tightly coupled pieces in 
  `src/Type/InferMonad.hs`:

  - Chain construction: `resolveImplicitArgEx` (line ~1057) and `resolveImplicitParameters` (line ~1129). Every time a candidate is selected and its own implicit parameters are recursively resolved, the candidate's `(Name, NameInfo, Rho)` triple is prepended to the `chain` list before recursing. The chain therefore records the full sequence of `(qualified-name, instantiated-type)` pairs explored so far in this derivation.

  - Termination check: `isDecreasingChain` (line ~1175) Before recursing into a candidate in `resolveUniquely` (line ~1103–1110), the guard `not (isDecreasingChain chain ctx qname rho)` is evaluated.`isDecreasingChain` filters the chain to prior occurrences of the same qualified name, and requires that the instantiated type's *weight* is strictly decreasing relative to the maximum weight seen in the last `decreasingWithin = 4` occurrences. If the type is not decreasing, the candidate is marked `Infty` (an infinite-chain failure) and pruned rather than explored.

  - Weight measure: `weight` / `weightType` / `weightParams` (line ~1185) The weight of an instantiated type is the total number of type constructors (`TCon` nodes) appearing in the implicit-parameter positions of that type. This is a structural size measure on types that is guaranteed to be a well-founded order, ensuring that any strictly decreasing sequence is finite.


- Static Overloading 
  (`[var-qualify]` rule — Figure 4 in the paper)

  `src/Type/InferMonad.hs`
  - `lookupAppName` (line ~845): top-level dispatch for name resolution
  - `lookupNameCtx` (line ~1277): looks up a name in the type environment, filters candidates using the type context to resolve qualified names

  `src/Type/Infer.hs`
  - `inferVar` (line ~1559): the main variable inference function; uses `resolveName` to trigger `[var-direct]` or `[var-qualify]`
  - `inferVarName` (line ~1597): finishes resolving and instantiates variables, possibly replacing a compilation constant (`kk-line`, etc).


- Default Namespace (§4.1 of the paper)

  - `src/Type/Assumption.hs`
    - `createNameInfoEx` (line ~421): when creating a `NameInfo` for a name that `isInDefaultNameSpace`, it sets the `infoScopeDepthX` field to `−1` (rather than the module-level `0`). The key line is: `d = if scopeDepth == 0 && isInDefaultNameSpace name then -1 else scopeDepth`
    - `infoScopeDepth` (line ~85): accessor for the `infoScopeDepthX` field used everywhere scope depths are compared

  - `src/Type/InferMonad.hs`
    - `lookupNameCtx` / `compareScopeDepth` (line ~1277): reads `infoScopeDepth` from each candidate's `NameInfo` and, via `filterInnerScopes`, drops any candidate whose scope depth is less than another candidate's — so `default/` definitions (depth −1) are always outcompeted by any concrete user definition (depth ≥ 0)

- Phantom Implicits (§4.3 of the paper)

  - `src/Type/Infer.hs`
    - `compilationConstants` (line ~1660): a table mapping `kk-line`, `kk-file`, and `kk-module` to their types and compile-time value generators. `kk-line` and `kk-file` are regular named values in `lib/std/core/debug.kk`; normal implicit name lookup resolves them, but when `inferVarName` evaluates the resolved name it checks `compilationConstants` and replaces the variable with the current source location literal.

  - `src/Type/InferMonad.hs`
    - `resolveHeapDivConstraint` (line ~1766): the `hdiv` phantom implicit is resolved by checking whether the value type can contain a heap reference, then unifying the effect with `:div`


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

The Koka VS Code extension can display inferred implicit arguments as inlay hints. Press `Ctrl+Alt` (or `Ctrl+Option` on macOS) to toggle them. This makes it easy to see how `showx([1])` elaborates to `list/showx([1], ?showx=int/showx)` without modifying the source file. For nested cases, the hints use eta-abbreviated `_` notation. For instance, `showx([[1]])` shows `?showx=list/showx(_, ?showx=int/showx)`. The extension is available on the [VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=koka.language-koka).

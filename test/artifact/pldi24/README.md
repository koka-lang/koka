# PLDI 2024 Paper Artifact: The Functional Essence of Imperative Binary Search Trees

[dockerhub]:  https://hub.docker.com/repository/docker/daanx/pldi24-tree/general
[Zenodo]:     https://zenodo.org/records/10790231

# Getting Started

We provide two docker images based on Ubuntu 22.04, one for `x64` and one for `arm64` 
(for use on an Apple M1 for example)
The [Zenodo] tar contains both images and this readme. For convenience and to reduce 
download times, we also uploaded the exact same images to [dockerhub] so they can be 
used directly with `docker pull`. Please use either `x64` or `arm64` since under 
emulation the benchmarks vary too much.

```
> docker pull daanx/pldi24-tree:1.0-x64
> docker run -it daanx/pldi24-tree:1.0-x64
```
or
```
> docker pull daanx/pldi24-tree:1.0-arm64
> docker run -it daanx/pldi24-tree:1.0-arm64
```
(use this on macOS M1). 

When using the Zenodo tar use the `docker load -i <image>` command instead of `docker pull`, for example:
```
> tar -xvf artifact_the_functional_essence_of_imperative_binary_search_trees.tar
> cd pldi24
> docker load -i daanx/pldi24-tree:1.0-x64
> docker run -it daanx/pldi24-tree:1.0-x64
```

We now see the docker prompt:
```
> root@xxx:/artifact/koka/test/artifact/pldi24#
```

We will shorten this to `test#` in the guide.
This directory also contains this `README.md`.
From this prompt, we can run our benchmarks as:
```
test# ./bench.sh zip run
```

```
/artifact/koka /artifact/koka/test/artifact/pldi24
/artifact/koka/test/artifact/pldi24

using koka: /artifact/koka/.stack-work/install/x86_64-linux-tinfo6/88c40a7dc919e28f6f4ab737212a15c4528a6ca9dfecb6d1de4f487b4bed2f20/9.6.4/bin/koka

expanded benches:  zip/zip-td.kk zip/zip-td.c zip/zip-td-p.c zip/zip-bu.kk zip/zip-bu.c zip/zip-bu-p.c zip/zip-bu.ml zip/zip-bu.hs

run kk__zip-td__100000, iter 1, cmd: .koka/v3.1.2-bench/clang-release/zip-td
sum: 4999950000, height: 42/7, top: 13652, final access: 2015542571
elapsed: 0.98s, user: 0.98s, sys: 0.00s, rss: 8956kb

run c__zip-td__100000, iter 1, cmd: .koka/ccomp/zip-td
sum: 4999950000, height: 42/7, top: 13652, final access: 2015542571
elapsed: 1.15s, user: 1.15s, sys: 0.00s, rss: 6348kb

run cp__zip-td__100000, iter 1, cmd: .koka/ccomp/zip-td-p
sum: 4999950000, height: 42/7, top: 13652, final access: 2015542571
elapsed: 0.97s, user: 0.97s, sys: 0.00s, rss: 7992kb
...

#    benchmark  variant  param   elapsed  relative  stddev     rss
...
##
kk  zip-td     -        100000  0.98     1.000     0          8956
c   zip-td     -        100000  1.15     1.173     0          6348
cp  zip-td     -        100000  0.97     .989      0          7992
ml  zip-td     -        100000  NA       0         0          0
hs  zip-td     -        100000  NA       0         0          0
##
kk  zip-bu     -        100000  0.98     1.000     0          9000
c   zip-bu     -        100000  1.13     1.153     0          6396
cp  zip-bu     -        100000  0.93     .948      0          7864
ml  zip-bu     -        100000  4.52     4.612     0          13500
hs  zip-bu     -        100000  4.78     4.877     0          26740
...
```

This runs the `zip` benchmark on the top-down (`td`) and bottom-up (`bu`)
variants. Eventually the bench provides a summary in absolute runtimes (and rss), 
and normalized runtimes relative to the Koka variant (`kk`).
The above results are on Ubuntu 22.0.4 with 16-core AMD 7950X @4.5Ghz (outside Docker).

### Troubleshooting

We observed that on some machines the Koka benchmarks are terminated by signal 4.
The probable reason for the signal 4 is that we pre-built all benchmarks with the -march=native flag to the C compiler
(this can be important for benchmarks like the ziptree as these need fast bit-count operations).
By doing a full rebuild everything should now be built using your particular architecture and hopefully work as intended:
```
test#  rm -rf .koka
test#  ./bench.sh build allb
```

# Step-by-step Guide

## Run All Benchmarks

The `./bench.sh` script runs each benchmark using `/usr/bin/time` to measure
the runtime and rss. For the benchmark figures in our paper we used
the following command: 

```
test# ./bench.sh allb run -n=10
```

to run all benchmarks 10 times for each available language, and use the median
of those runs (and calculate the standard error interval). 

The benchmark results should correspond closely to the results in Section 7 of the 
paper, in particular Figure 3, and support the conclusions drawn there. Note that 
the results can differ quite bit among different systems, but if not running in 
emulation, the relative times should be quite similar. 

To support the conclusions of the paper, "performance on-par with the best C algorithms",
the Koka variant should generally be within 25% of the C variant (`c`) and
the "equalized C" (`cp`) variant (see Section 7 of the paper for an explanation).

For reference, we included our benchmark results on bare metal (outside Docker)

- `bench-res-ubuntu-x64.txt`: on Ubuntu22 on an AMD7950X.
- `bench-res-macos-M1.txt`: on an Apple M1.

and results running in Docker

- `bench-res-docker-x64.txt`: Docker on an AMD7950X (running Windows 11)
- `bench-res-docker-arm64.txt`: Docker on an Apple M1 (running macOSX Sonoma 14.2.1)

A difference we found with respect to the benchmarks on x64, is that on bare metal 
macOS M1 for `zip-td` we are ~15% slower than `c` and `cp`. On macOS the allocator 
is better too and `c` and `cp` are generally very close in performance.


## Benchmark Descriptions

The benchmarks are described in detail in the paper (Section 7).
We use the following systems:

- `c`: The C programming language, compiled using clang 14.0.0-1ubuntu1.1
    with the default allocator.
- `cp`: "equalized C", compiled using clang 14.0.0-1ubuntu1.1
    with the mimalloc allocator and an extra header field on the `td` variant.
- `hs`: The Haskell programming language, compiled using GHC 8.8.4
- `ml`: The OCaml programming language, version 4.13.1
- `kk`: The Koka programming language, version 3.1.2

We benchmark the following variants:

- `bu`: The bottom-up algorithm. We use parent pointers for `c` and `cmi`
    and zippers for the functional algorithms.
- `td`: The imperative top-down algorithm (not implemented in Haskell and OCaml)
    using constructor contexts in Koka.

We benchmark the following algorithms:

- `mtr-(td|bu)`: Move-to-root trees (sources are in the `mtr` directory)
- `splay-(td|bu)`: Splay trees (sources are in the `splay` directory)
- `zip-(td|bu)`: Zip trees (sources are in the `zip` directory)
- `rbtree-(td|bu)`: Red-black trees (sources are in the `rbtree` directory)

Each benchmark performs 10 million insertions starting with an empty tree,
using a pseudo random sequence of keys between 0 and 100 000.
We use the same pseudo random number generator (`sfc32`) for all benchmarks
and the same seed (42,43) to ensure fairness.

You can select to run particular benchmarks instead of all:
```
test# ./bench.sh mtr zip  run -n=5
```
would run the `mtr` and `zip` variants 5 times. Use `./bench.sh -h` to see all options.


## Checking the proofs

The proofs (Section 4) are all included in the AddressC directory.
You can check the proofs using (where `4` is the number of threads):
```
test# cd AddressC/
test/AddressC# eval $(opam env)
test/AddressC# make -j 4
```

The proofs make heavy use of Diaframe proof-search and may take
up to 20 minutes to check.

# Structure of the Artifact

This artifact contains the following files:

 - `README.md`: this file
 - `Dockerfile`: contains all installation commands for Ubuntu 22.04
     In particular, you can find the precise version numbers of the
     dependencies of the Coq proofs and the opam commands to install them.
 - `bench-res-{system}-{processor}.txt`: These four files contain the
     benchmark results we obtained running on the respective system
     and processor.

 - `bench.sh`: Our main benchmarking script.
 - `daanx/pldi24-tree:1.0-{processor}.tar.gz`: Copies of the docker
      containers created using `docker save`.
 - `{benchmark}/{benchmark}-{td/bu}.{language-extension}`:
      The entry point of the benchmark for the given language.
      This file contains the main insertion algorithm and usually
      imports the main benchmarking code from one of those files:
 - `zip/ziptree.{h/kk}`, `splay/tree.{h/hs/kk}`, `mtr/tree.{h/hs/kk/ml}`,
   `rbtree/rbtree.{h/kk}`: The definition of the benchmarking code
      including random number generation. We have implemented
      the same RNG `sfc32` for all languages.

Aside from the benchmarks reported in the paper, we also include
several experimental benchmarks that we used to get a better sense
for the performance characteristics of our main benchmarks.
These benchmarks are unsupported but included for completeness:

 - `{benchmark}/{benchmark}-rec.{language-extension}`:
      The typical recursive functional implementation for that language.
      You can enable these benchmarks by adding "mtr-rec splay-rec rb-rec"
      to the `benchmarks` variables on line 5 of `build.sh`

 - `mtr/mtr-bu-rev.c`: Move-to-root trees implemented using pointer
      reversal instead of parent pointers (as in `mtr/mtr-bu.c`)

 - `mtr/mtr-cps.kk`, `mtr/mtr-defun.kk`, `mtr/mtr-spec.kk`:
      Move-to-root trees in Koka using continuation-passing-style /
      defunctionalized continuations / separate functions for the
      smaller and bigger tree.

 - `rbtree/rb-td2.kk`: A different implementation for top-down red-black
      trees not reported in the paper. Notice also the commented out
      version in `rbtree/rb-td.kk` -- it is not so easy to define fast
      top-down redblack trees in a functional language!

 - `zip-spec.kk`: Equal to `zip-rec.kk` where `unzip` is split
      into a `smaller` and `bigger` function as in `mtr/mtr-spec.kk`

 - `{benchmark}/*.icl`, `mtr/clean_env.c`,`mtr/CommandLine.{dcl/icl}`:
      Implementations of our benchmarks for the Clean language.
      You may attempt to run the Clean benchmarks by downloading
      Clean 3.1 from https://wiki.clean.cs.ru.nl/Download_Clean,
      adding `clm` to your path and adding "icl" to the
      `languages` variable on line 7 of `build.sh`

The proofs are contained in the `AddressC/` directory:

 - `README.md`: A short introduction to AddressC
 - `Makefile`: Makefile for the proofs (see above).
 - `_CoqProject`, `Makefile.coq.conf`, `LICENSE`, `.gitignore`:
    The usual boilerplate.
 - `theories/ident.v`: A copy of the `ident_to_string!` tactic
      due to the `coqutil` project.
 - `theories/lang.v`: Tactics and notation for the AddressC language.
 - `theories/{benchmark}.v`: Definition of the functional recursive,
      bottom-up and top-down versions in Coq.
 - `theories/{benchmark}_td.v`: Verification of the imperative
      top-down algorithm of the respective benchmark.
 - `theories/{benchmark}_bu.v`: Verification of the imperative
      bottom-up algorithm of the respective benchmark.
 - `theories/tree.v`, `theories/tree_bu.v`, `theories/tree_td.v`:
      Useful lemmas common to both move-to-root and splay trees.
      This includes the definition of constructor contexts.

We also include some proof experiments not necessary for this paper:

 - `theories/reverse.v`, `theories/tmap.v`: Verifications of the
      imperative versions of simple functional programming idioms.
 - `theories/append.v`, `theories/append_wand.v`:
     Verification of an imperative append function due to
     https://viper.ethz.ch/tutorial/#magic-wands
     using contexts and implication wands respectively.

# Notes

## Installing from Scratch

It is not too difficult to install directly on Linux or MacOS.
See the `Dockerfile` for precise build instructions on Linux.
Essentially one only needs to install `mimalloc`, `OCaml`, `GHC`, 
and checkout and build the `artifact-pldi24` branch of Koka.

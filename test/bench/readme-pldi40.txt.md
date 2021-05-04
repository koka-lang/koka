# PLDI paper #40: Perceus: Garbage Free Reference Counting with Reuse

Alex Reinking, Ningning Xie, Leonardo de Moura, and Daan Leijen

Docker image: daanx/pldi-perceus:1.0
Digest      : sha256:155395bc575a6bd8969ca3bf00ef78c322b8b732289608f006f203b0a39f40b7 

# Getting Started

We provide a docker image (based on Ubuntu 20.04, about 11GiB) to run the benchmarks:
```
> docker pull daanx/pldi-perceus:1.0
> docker run -it daanx/pldi-perceus:1.0
```

We now see the docker prompt as:
```
> root@a78d3fc4dbf6:/build/koka/test/bench/build#
```
We will shorten this to `/build/koka/test/bench/build#` in the guide.

From this prompt, we can test if we can run our benchmarks as:
```
/build/koka/test/bench/build# koka ../run.kk -- --test=rbtree
compile: ../run.kk
loading: std/core
...

tests    : rbtree
languages: koka, kokax, ocaml, haskell, swift, java, cpp

run: koka/out/bench/kk-rbtree
420000
1: elapsed: 0.64s, rss: 168156kb

...

--- rbtree ----------------
rbtree,  kk, 0.64s ~0.000, 168156kb
rbtree, kkx, 1.54s ~0.000, 168068kb
rbtree,  ml, 0.73s ~0.000, 204888kb
rbtree,  hs, 1.61s ~0.000, 540516kb
rbtree,  sw, 4.63s ~0.000, 269968kb
rbtree,  jv, 1.43s ~0.000, 2512520kb
rbtree, cpp, 0.62s ~0.000, 200264kb

--- normalized rbtree ----------------
rbtree,  kk, 1.00x ~0.000, 1.00x
rbtree, kkx, 2.41x ~0.000, 1.00x
rbtree,  ml, 1.14x ~0.000, 1.22x
rbtree,  hs, 2.52x ~0.000, 3.21x
rbtree,  sw, 7.23x ~0.000, 1.61x
rbtree,  jv, 2.23x ~0.000, 14.94x
rbtree, cpp, 0.97x ~0.000, 1.19x
```

This runs the `rbtree` benchmark for all systems (koka, kokax, ocaml, haskell, swift, java, cpp),
and eventually provides a summary in absolute runtimes (and rss), and normalized
runtimes (and rss) relative to `koka`.  

(Note that the precise results depend quite a bit on the host system (the above results
are on a 16-core AMD 5950X @ 3.4Ghz, while the paper uses a 6-core AMD 3600XT @ 3.8Ghz). 
Nevertheless, on all systems the relative results are comparable and support the 
conclusions in the paper (Section 4) that Perceus can be competitive.)


# Step-by-step Guide

## Run benchmarks

The `../run.kk` script runs each benchmark using `/usr/bin/time` to measure
the runtime and rss. For the benchmark figures in our paper (Figure 9, Section 4) we used
the following command: 
```
/build/koka/test/bench/build# koka ../run.kk -- -i11
```
to run all benchmarks 11 times for each available language, throw out the worst score, 
and average the remaining 10 scores (and calculate the error interval)

Running all benchmarks over all systems takes a while (10 to 30min); we can use the `--lang=<langs>` and
`--test=<tests>` options to run a particular test for a specific language, for example:
```
/build/koka/test/bench/build# koka ../run.kk -- -i2 --test=rbtree,deriv --lang=kk,ml,cpp
```

Available languages are:

- `kk`  : Koka v2.1.1 compiling using gcc 9.3.0.
- `kkx` : Koka v2.1.1 compiling using gcc 9.3.0 but without reuse optimization (Section 2.4).
- `ml`  : OCaml v4.08.1 using the optimizing compiler (`ocamlopt`)
- `hs`  : Haskell GHC 8.6.5.
- `sw`  : Swift 5.3.3.
- `jv`  : Java 15.0.2, Java(TM) SE Runtime Environment (build 15.0.2+7-27), 
          Java HotSpot(TM) 64-Bit Server VM (build 15.0.2+7-27, mixed mode, sharing).
- `cpp` : GCC 9.3.0, 

Available tests are described in detail in Section 4 and are:

- `rbtree`    : inserts 42 million items into a red-black tree.
- `rbtree-ck` : a variant of rbtree that keeps a list of every 5th subtree and thus shares many subtrees.
- `deriv`     : the symbolic derivative of a large expression.
- `nqueens`   : calculates all solutions for the n-queens problem of size 13 into a list, and returns the length of that list.  
- `cfold`     : constant-folding over a large symbolic expression.


## Benchmark Sources

All the sources are in the `/build/koka/test/bench/<lang>` directories. For example:
```
/build/koka/test/bench/build# ls ../java
CMakeLists.txt  cfold.java  deriv.java  nqueens.java  rbtree.java  rbtreeck.java
```

## Re-build the Benchmarks

All tests can be recompiled using:
```
/build/koka/test/bench/build# cmake --build .
```

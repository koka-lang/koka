# ICFP Paper Artifact: Reference Counting with Frame Limited Reuse

Anton Lorenzen and Daan Leijen

Docker image: daanx/icfp-reuse:1.0
Digest      : sha256:... 

# Getting Started

We provide a docker image (based on Ubuntu 20.04, about 4GiB) to run the benchmarks:
```
> docker pull daanx/icfp-reuse:1.0
> docker run -it daanx/icfp-reuse:1.0
```

We now see the docker prompt as:
```
> root@a78d3fc4dbf6:/build/koka/test/bench/build#
```
We will shorten this to `/build/koka/test/bench/build#` in the guide.

From this prompt, we can test if we can run our benchmarks as:
```
/build/koka/test/bench/build# koka -e ../bench.kk -- --norm --iter=3 --test=rbtree
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

Note that the precise results depend quite a bit on the host system -- the above results
are on a 16-core AMD 5950X @ 3.4Ghz.


# Step-by-step Guide

## Run benchmarks

The `../bench.kk` script runs each benchmark using `/usr/bin/time` to measure
the runtime and rss. For the benchmark figures in our paper we used
the following command: 
```
/build/koka/test/bench/build# koka -e ../bench.kk -- --norm --iter=10
```
to run all benchmarks 10 times for each available language, and use the median
of those runs (and calculate the error interval)

Running all benchmarks over all systems takes a while (10 to 30min); we can use the `--lang=<langs>` and
`--test=<tests>` options to run a particular test for a specific language, for example:
```
/build/koka/test/bench/build# koka -e ../bench.kk -- --norm --iter=2 --test=rbtree,binarytrees --lang=kk,ml,cpp
```

Available languages are:

- `kk`  : Koka v2.4.1 compiling using gcc 9.4.0.
- `kkx` : Koka v2.4.1 compiling using gcc 9.4.0 but without reuse optimization.
- `ml`  : OCaml v4.14.0 using the optimizing compiler (`ocamlopt`)
- `hs`  : Haskell GHC 8.6.5
- `sw`  : Swift 5.6.1.
- `jv`  : java 17.0.1 2021-10-19 LTS
          Java(TM) SE Runtime Environment (build 17.0.1+12-LTS-39)
          Java HotSpot(TM) 64-Bit Server VM (build 17.0.1+12-LTS-39, mixed mode, sharing)
- `cpp` : GCC 9.4.0, 

Available tests are described in detail in Section 4 and are:

- `rbtree`      : inserts 42 million items into a red-black tree.
- `rbtree-ck`   : a variant of rbtree that keeps a list of every 5th subtree and thus shares many subtrees.
- `deriv`       : the symbolic derivative of a large expression.
- `nqueens`     : calculates all solutions for the n-queens problem of size 13 into a list, and returns the length of that list.  
- `cfold`       : constant-folding over a large symbolic expression.
- `binarytrees` : the binarytrees benchmark from the computer benchmark game 
                  <https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/binarytrees.html#binarytrees>


## Benchmark Sources

All the sources are in the `/build/koka/test/bench/<lang>` directories. For example:
```
/build/koka/test/bench/build# ls ../java
CMakeLists.txt  binarytrees.java  cfold.java  deriv.java  nqueens.java  rbtree.java  rbtreeck.java
```

## Re-build the Benchmarks

All tests can be recompiled using:
```
/build/koka/test/bench/build# cmake --build .
```

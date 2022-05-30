# ICFP Paper Artifact: Reference Counting with Frame Limited Reuse

Anton Lorenzen and Daan Leijen

Docker image: daanx/icfp22-reuse:1.0
Digest      : sha256:0f6b9912e78f6bc1416c9ac44ea3454c51c5a4f04784a5cff25b47a6c1560c2f

# Getting Started

We provide a docker image (based on Ubuntu 20.04, about 4GiB) to run the benchmarks:

```
> docker pull daanx/icfp22-reuse:1.0
> docker run -it daanx/icfp22-reuse:1.0
```

We now see the docker prompt as:

```
> root@1c23a03bab34:/build/koka/test/bench/build#
```

We will shorten this to `/build/koka/test/bench/build#` in the guide.
From this prompt, we can test if we can run our benchmarks as:

```
/build/koka/test/bench/build# koka -e ../bench.kk -- --norm --iter=3 --test=rbtree
compile: ../bench.kk
loading: std/core
...

tests    : rbtree
languages: koka, kokax, kokaold, ocaml, haskell, swift, java, cpp, kokafbip

run: koka/out/bench/kk-rbtree
420000
1: elapsed: 0.43s, rss: 135428kb

...

--- rbtree ----------------
rbtree,    kk,  0.41s ~0.010, 135412kb
rbtree,   kkx,  0.52s ~0.005, 135426kb
rbtree, kkold,  0.62s ~0.005, 170260kb
rbtree,    ml,  0.65s ~0.010, 205834kb
rbtree,    hs,  1.48s ~0.015, 540478kb
rbtree,    sw,  5.22s ~0.141, 269242kb
rbtree,    jv,  1.04s ~0.029, 1576906kb
rbtree,   cpp,  0.54s ~0.019, 200170kb
rbtree, kkfbip,  0.38s ~0.017, 135476kb

--- normalized rbtree ----------------
rbtree,    kk,  1.00x ~0.010, 1.00x
rbtree,   kkx,  1.24x ~0.005, 1.00x
rbtree, kkold,  1.51x ~0.005, 1.26x
rbtree,    ml,  1.57x ~0.010, 1.52x
rbtree,    hs,  3.55x ~0.015, 3.99x
rbtree,    sw, 12.58x ~0.141, 1.99x
rbtree,    jv,  2.52x ~0.029, 11.65x
rbtree,   cpp,  1.30x ~0.019, 1.48x
rbtree, kkfbip,  0.89x ~0.017, 1.00x
```

This runs the `rbtree` benchmark for all systems (koka, kokax, kokaold, kokafbip, ocaml, haskell, swift, java, cpp),
and eventually provides a summary in absolute runtimes (and rss), and normalized
runtimes (and rss) relative to `koka`. 

Note that the precise results depend quite a bit on the host system -- the above results
are on a 16-core AMD 5950X @ 3.4Ghz inside the Docker container.


# Step-by-step Guide

## Run All Benchmarks

The `../bench.kk` script runs each benchmark using `/usr/bin/time` to measure
the runtime and rss. For the benchmark figures in our paper we used
the following command: 

```
/build/koka/test/bench/build# koka -e ../bench.kk -- --norm --iter=10
```

to run all benchmarks 10 times for each available language, and use the median
of those runs (and calculate the standard error interval).
The expected results on an AMD5950X are at the bottom of this readme.

Running all benchmarks over all systems takes a while (10 to 30min); we can use the `--lang=<langs>` and
`--test=<tests>` options to run a particular test for a specific language, for example:

```
/build/koka/test/bench/build# koka -e ../bench.kk -- --norm --iter=2 --test=rbtree,binarytrees --lang=kk,ml,cpp
```

Available languages are:

- `kk`    : Koka v2.3.3 compiling using gcc 9.4.0.
- `kkx`   : Koka v2.3.3 compiling using gcc 9.4.0 but without TRMC (tail-mod-cons) optimization.
- `kkold` : Koka v2.3.3-old compiling using gcc 9.4.0. This is the compiler exactly like `kk` but
            (1) using the old reuse algorithm K and (2) no borrowing, as described in the paper.
- `kkfbip`: Koka v2.3.3 compiling using gcc 9.4.0 but using the FBIP variants of the `rbtree`,
            `rbtree-ck`, and `binarytrees` benchmarks as described in the paper (Section 7).
- `ml`  : Multicore OCaml v4.14.0 using the optimizing compiler (`ocamlopt`)
- `hs`  : Haskell GHC 8.6.5
- `sw`  : Swift 5.6.1.
- `jv`  : Java 17.0.1 2021-10-19 LTS
          Java(TM) SE Runtime Environment (build 17.0.1+12-LTS-39)
          Java HotSpot(TM) 64-Bit Server VM (build 17.0.1+12-LTS-39, mixed mode, sharing)
- `cpp` : GCC 9.4.0

Available tests are described in detail in Section 4 and are:

- `rbtree`      : inserts 42 million items into a red-black tree.
- `rbtree-ck`   : a variant of rbtree that keeps a list of every 5th subtree and thus shares many subtrees.
- `deriv`       : the symbolic derivative of a large expression.
- `nqueens`     : calculates all solutions for the n-queens problem of size 13 into a list, and returns the length of that list.  
- `cfold`       : constant-folding over a large symbolic expression.
- `binarytrees` : the binarytrees benchmark from the computer benchmark game 
                  <https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/binarytrees.html#binarytrees>


The `koka`/`kk` version is using the compiler in `/build/koka-v2.3.3` with the new reuse 
algorithm, while `kokaold`/`kkold` is based on the `/build/koka-v2.3.3-old` compiler
that is exactly like `koka` except with the old reuse algorithm. (This is why
we cannot use the more recent (and better) koka v2.4.x versions since we only
carefully maintained the `kokaold` to track `koka` up to the v2.3.3 version).


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


## Expected Results in Docker on Windows:

These were obtained running on Windows inside a Ubuntu 20.04 x86_64 Docker 
container on a 16-core AMD 5950X @ 3.4Ghz.

```
root@...:/build/koka/test/bench/build# koka -e ../bench.kk -- --iter=10 --norm
```

```
--- rbtree ----------------
rbtree,    kk,  0.42s ~0.006, 135468kb
rbtree,   kkx,  0.51s ~0.000, 135464kb
rbtree, kkold,  0.61s ~0.003, 170290kb
rbtree,    ml,  0.64s ~0.007, 206424kb
rbtree,    hs,  1.48s ~0.015, 540504kb
rbtree,    sw,  5.25s ~0.095, 269428kb
rbtree,    jv,  1.05s ~0.037, 1595502kb
rbtree,   cpp,  0.56s ~0.013, 200186kb
rbtree, kkfbip,  0.39s ~0.007, 135468kb

--- rbtree-ck ----------------
rbtree-ck,    kk,  1.12s ~0.035, 1181986kb
rbtree-ck,   kkx,  1.12s ~0.015, 1181980kb
rbtree-ck, kkold,  1.58s ~0.024, 1181930kb
rbtree-ck,    ml,  1.94s ~0.039, 1413220kb
rbtree-ck,    hs, 14.71s ~0.321, 11591426kb
rbtree-ck,    sw,  5.36s ~0.155, 1883714kb
rbtree-ck,    jv,  2.16s ~0.010, 2322116kb
rbtree-ck,   cpp, error: Command exited with non-zero status 1
0.05 86368
rbtree-ck, kkfbip,  1.01s ~0.043, 1182032kb

--- binarytrees ----------------
binarytrees,    kk,  0.81s ~0.035, 682008kb
binarytrees,   kkx,  0.84s ~0.036, 673168kb
binarytrees, kkold,  0.83s ~0.034, 691124kb
binarytrees,    ml,  1.62s ~0.034, 175520kb
binarytrees,    hs,  7.13s ~0.023, 422944kb
binarytrees,    sw,  3.71s ~0.126, 736962kb
binarytrees,    jv,  1.90s ~0.042, 2403894kb
binarytrees,   cpp,  0.58s ~0.024, 1029256kb
binarytrees, kkfbip,  0.74s ~0.022, 657548kb

--- deriv ----------------
deriv,    kk,  0.61s ~0.007, 469404kb
deriv,   kkx,  0.63s ~0.006, 469376kb
deriv, kkold,  0.74s ~0.004, 469356kb
deriv,    ml,  0.81s ~0.005, 433988kb
deriv,    hs,  1.40s ~0.009, 499498kb
deriv,    sw,  1.62s ~0.016, 930968kb
deriv,    jv,  0.57s ~0.013, 818774kb
deriv,   cpp,  0.79s ~0.013, 1053314kb
deriv, kkfbip, error: NA

--- nqueens ----------------
nqueens,    kk,  0.48s ~0.009, 98648kb
nqueens,   kkx,  0.49s ~0.003, 98594kb
nqueens, kkold,  0.74s ~0.010, 98592kb
nqueens,    ml,  0.79s ~0.003, 181108kb
nqueens,    hs,  6.27s ~0.015, 347984kb
nqueens,    sw,  2.34s ~0.011, 326944kb
nqueens,    jv,  0.77s ~0.008, 317028kb
nqueens,   cpp,  0.56s ~0.008, 295512kb
nqueens, kkfbip, error: NA

--- cfold ----------------
cfold,    kk,  0.09s ~0.003, 143756kb
cfold,   kkx,  0.11s ~0.005, 143676kb
cfold, kkold,  0.08s ~0.006, 143716kb
cfold,    ml,  0.35s ~0.003, 137096kb
cfold,    hs,  0.37s ~0.005, 156426kb
cfold,    sw,  0.68s ~0.004, 227808kb
cfold,    jv,  0.22s ~0.004, 488270kb
cfold,   cpp,  0.28s ~0.004, 421158kb
cfold, kkfbip, error: NA

--- normalized rbtree ----------------
rbtree,    kk,  1.00x ~0.006, 1.00x
rbtree,   kkx,  1.21x ~0.000, 1.00x
rbtree, kkold,  1.45x ~0.003, 1.26x
rbtree,    ml,  1.51x ~0.007, 1.52x
rbtree,    hs,  3.52x ~0.015, 3.99x
rbtree,    sw, 12.49x ~0.095, 1.99x
rbtree,    jv,  2.50x ~0.037, 11.78x
rbtree,   cpp,  1.33x ~0.013, 1.48x
rbtree, kkfbip,  0.92x ~0.007, 1.00x

--- normalized rbtree-ck ----------------
rbtree-ck,    kk,  1.00x ~0.035, 1.00x
rbtree-ck,   kkx,  1.00x ~0.015, 1.00x
rbtree-ck, kkold,  1.41x ~0.024, 1.00x
rbtree-ck,    ml,  1.73x ~0.039, 1.20x
rbtree-ck,    hs, 13.14x ~0.321, 9.81x
rbtree-ck,    sw,  4.79x ~0.155, 1.59x
rbtree-ck,    jv,  1.93x ~0.010, 1.96x
rbtree-ck,   cpp, error: Command exited with non-zero status 1
0.05 86368
rbtree-ck, kkfbip,  0.90x ~0.043, 1.00x

--- normalized binarytrees ----------------
binarytrees,    kk,  1.00x ~0.035, 1.00x
binarytrees,   kkx,  1.04x ~0.036, 0.99x
binarytrees, kkold,  1.02x ~0.034, 1.01x
binarytrees,    ml,  1.99x ~0.034, 0.26x
binarytrees,    hs,  8.75x ~0.023, 0.62x
binarytrees,    sw,  4.55x ~0.126, 1.08x
binarytrees,    jv,  2.34x ~0.042, 3.52x
binarytrees,   cpp,  0.72x ~0.024, 1.51x
binarytrees, kkfbip,  0.91x ~0.022, 0.96x

--- normalized deriv ----------------
deriv,    kk,  1.00x ~0.007, 1.00x
deriv,   kkx,  1.03x ~0.006, 1.00x
deriv, kkold,  1.21x ~0.004, 1.00x
deriv,    ml,  1.33x ~0.005, 0.92x
deriv,    hs,  2.30x ~0.009, 1.06x
deriv,    sw,  2.66x ~0.016, 1.98x
deriv,    jv,  0.93x ~0.013, 1.74x
deriv,   cpp,  1.30x ~0.013, 2.24x
deriv, kkfbip, error: NA

--- normalized nqueens ----------------
nqueens,    kk,  1.00x ~0.009, 1.00x
nqueens,   kkx,  1.01x ~0.003, 1.00x
nqueens, kkold,  1.53x ~0.010, 1.00x
nqueens,    ml,  1.63x ~0.003, 1.84x
nqueens,    hs, 12.93x ~0.015, 3.53x
nqueens,    sw,  4.82x ~0.011, 3.31x
nqueens,    jv,  1.59x ~0.008, 3.21x
nqueens,   cpp,  1.15x ~0.008, 3.00x
nqueens, kkfbip, error: NA

--- normalized cfold ----------------
cfold,    kk,  1.00x ~0.003, 1.00x
cfold,   kkx,  1.22x ~0.005, 1.00x
cfold, kkold,  0.89x ~0.006, 1.00x
cfold,    ml,  3.89x ~0.003, 0.95x
cfold,    hs,  4.11x ~0.005, 1.09x
cfold,    sw,  7.56x ~0.004, 1.58x
cfold,    jv,  2.44x ~0.004, 3.40x
cfold,   cpp,  3.11x ~0.004, 2.93x
cfold, kkfbip, error: NA
```

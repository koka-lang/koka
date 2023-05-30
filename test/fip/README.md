# ICFP 2023 Paper Artifact: FP^2: Fully in-Place Functional Programming

Anton Lorenzen, Daan Leijen, and Wouter Swierstra

# Getting Started

Go to the test directory:

```
> cd koka/test/fip
```

We will shorten this directory to `test#` in the guide.
This directory also contains this README.md.

From this prompt, we can run our benchmarks as:

```
test# ./bench.sh rbtree run

~/home/dev/koka ~/home/dev/koka/test/fip
~/home/dev/koka/test/fip
using koka: /mnt/c/Users/daan/dev/koka/.stack-work/install/x86_64-linux-tinfo6/8f1dbd1b92c17da66792bc77d6f502c989021e266b5032fa
expanded benches:  rbtree/rbtree-fip.kk rbtree/rbtree-fip-icfp.kk rbtree/rbtree-std-reuse.kk rbtree/rbtree-std.kk ...

run kk__rbtree-fip__100000, iter 1, cmd: .koka/v2.4.1-bench/clang-release/rbtree-fip
total: 1000000
elapsed: 0.60s, user: 0.59s, sys: 0.00s, rss: 6988kb

...

#    benchmark  variant       param   elapsed  relative  stddev  rss
kk   rbtree     fip           100000  0.60     1.000     0       6988
kk   rbtree     fip-icfp      100000  0.53     .883      0       6928
kk   rbtree     std-reuse     100000  0.61     1.016     0       6940
kk   rbtree     std           100000  1.53     2.550     0       7048
kk   rbtree     fip-clrs      100000  0.78     1.300     0       7056
c    rbtree     clrs          100000  0.68     1.133     0       6252
c    rbtree     clrs-mi       100000  0.57     .950      0       8080
c    rbtree     clrs-full     100000  0.68     1.133     0       6404
c    rbtree     clrs-full-mi  100000  0.57     .950      0       8084
cpp  rbtree     stl           100000  0.88     1.466     0       8528
cpp  rbtree     stl-mi        100000  0.58     .966      0       10136
```

This runs the `rbtree` benchmark on various variants
and eventually provides a summary in absolute runtimes (and rss), and normalized
runtimes relative to the Koka fip variant.

Note that the precise results depend quite a bit on the host system -- the above results are on Ubuntu 22.0.4 with 16-core AMD 7950X @ 4.5Ghz.


# Step-by-step Guide

## Run All Benchmarks

The `../bench.kk` script runs each benchmark using `/usr/bin/time` to measure
the runtime and rss. For the benchmark figures in our paper we used
the following command: 

```
test# ./bench.sh allb run -n=10
```

to run all benchmarks 10 times for each available language, and use the median
of those runs (and calculate the standard error interval). 

The expected results on an AMD7950X are at the bottom of this readme.
These should correspond closely to the results in Section 6 of the paper and
support the conclusions drawn there. Note that the results can differ quite
bit among different systems but if not running in emulation, the relative times 
should be quite similar. 


## Benchmark Descriptions

The benchmarks are described in detail in the paper.

- `rbtree`  : inserts 42 million items into a red-black tree.
- `ftree`   : 
- `msort`   :
- `qsort`   : 
- `tmap`    : 

Each benchmark comes in different variants:

- `fip`
- `std-reuse`
- `std`
- C `stl`/`std`
- C `stl-mi`/`std-mi`
- `rbtree-clrs`


## Benchmark Sources

All the sources are in the `test/src` directories. For example:
```
test# ls src/msort
msort-fip.kk  msort-std.kk
```

## Re-build the Benchmarks

All tests can be recompiled using:
```
test# ./bench.sh allb build
```

Further options:

* `allb`: all benchmarks (also `allkk` and `allc` to select a subset, or `rbtree`, `msort`, `qsort`, `ftree`, and `tmap`).
* `build`: build benchmarks.
* `run`: run benchmarks and show benchmark scores (calculating median and stddev).
* `-n=<`N`>`: run each benchmark N times.
* `koka=<cmd>`: set koka compiler command explicitly.
* `ccomp=<cc>`: set C compiler, either `clang` or `gcc` (or `gcc-<version>`).

The benchmarks are given the problem size `N` and run for `100_000_000/N` iterations.


## Expected Results 

These were obtained running on Ubuntu 22.0.4 on a 16-core AMD 7950X @ 4.5Ghz.

```
test# ./bench allb build run -n=10
...
```

```
#    benchmark  variant       param   elapsed  relative  stddev     rss
kk   rbtree     fip           100000  0.59     1.000     .0057735   6944
kk   rbtree     fip-icfp      100000  0.53     .898      .0051846   6916
kk   rbtree     std-reuse     100000  0.61     1.033     .0059640   6900
kk   rbtree     std           100000  1.48     2.508     .1023885   6936
kk   rbtree     fip-clrs      100000  0.78     1.322     .0076325   6940
c    rbtree     clrs          100000  0.68     1.152     .0066510   6368
c    rbtree     clrs-mi       100000  0.57     .966      0          7944
c    rbtree     clrs-full     100000  0.67     1.135     .0065529   6404
c    rbtree     clrs-full-mi  100000  0.57     .966      0          7948
cpp  rbtree     stl           100000  0.88     1.491     .0086082   8440
cpp  rbtree     stl-mi        100000  0.58     .983      0          10168
##
kk   ftree      fip           100000  0.83     1.000     .0057735   7036
kk   ftree      std-reuse     100000  0.90     1.084     .075101    6912
kk   ftree      std           100000  1.32     1.590     .0091798   6808
##
kk   msort      fip           100000  0.92     1.000     .0057735   9064
kk   msort      std-reuse     100000  0.90     .978      .0056464   11588
kk   msort      std           100000  1.17     1.271     .01037767  11552
##
kk   qsort      fip           100000  1.13     1.000     .0057735   14588
kk   qsort      std-reuse     100000  1.48     1.309     .0226725   15140
kk   qsort      std           100000  2.13     1.884     .0543863   15116
##
kk   tmap       fip           100000  1.13     1.000     .0238048   11144
kk   tmap       std-reuse     100000  0.80     .707      .00577263  11016
kk   tmap       std           100000  0.82     .725      .0041857   11152
c    tmap       fip           100000  1.36     1.203     .0208365   7968
c    tmap       fip-mi        100000  0.59     .522      .0030137   9992
c    tmap       std           100000  1.44     1.274     .0073554   7912
c    tmap       std-mi        100000  0.63     .557      .0032158   9952
```


# Prerequisites

## GNU time
Install gnu time if you don't have it:
```
# /usr/bin/time --version
GNU time 1.7
```

## Mimalloc

The C benchmarks rely on mimalloc:

```
# git clone https://github.com/microsoft/mimalloc
# cd mimalloc
# mkdir -p out/release
# cd out/release
# cmake ../..
# make
# sudo make install
```


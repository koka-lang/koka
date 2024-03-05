# PLDI 2024 Paper Artifact: The Functional Essence of Imperative Binary Search Trees

# Getting Started

We provide a docker image (based on Ubuntu 22.04, x64) to run the benchmarks:

> docker pull daanx/pldi24-tree:1.0
> docker run -it daanx/pldi24-tree:1.0

We now see the docker prompt as:

> root@xxx:/artifact/koka/test/artifact/pldi24#

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

For reference, we included our benchmark results on Ubuntu22 on an AMD7950X @4.5Ghz
in `bench-res-ubuntu-x64.txt` (outside of Docker). 
We also included benchmark results on an Apple M1 in `bench-res-macos-M1.txt` (outside Docker).
A difference we found with respect to the benchmarks on x64, is that on macOS M1 
for `zip-td` we are ~15% slower than `c` and `cp`. On macOS the allocator is better
too and `c` and `cp` are generally very close in performance.


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
would run the `mtr` and `zip` variants 5 times.


# Notes

## Installing from Scratch

It is not too difficult to install directly on Linux or MacOS.
See the `Dockerfile` for precise build instructions on Linux.
Essentially one only needs to install `mimalloc`, `OCaml`, `GHC`, 
and checkout and build the `artifact-pldi24` branch of Koka.

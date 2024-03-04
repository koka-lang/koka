# PLDI 2024 Paper Artifact: The Functional Essence of Imperative Binary Search Trees

# Getting Started

Go to the artifact directory:

```
# cd koka/test/artifact/pldi24
```

We will shorten this directory to `test#` in the guide.
This directory also contains this `README.md`.

From this prompt, we can run our benchmarks as:

```
test# ./bench.sh zip run
```
```
~/dev/koka ~/dev/koka/test/artifact/pldi24
~/dev/koka/test/artifact/pldi24

using koka: /home/daan/dev/koka/.stack-work/install/x86_64-linux-tinfo6/88c40a7dc919e28f6f4ab737212a15c4528a6ca9dfecb6d1de4f487b4bed2f20/9.6.4/bin/koka

clean compiler not found: clm
expanded benches:  zip/zip-td.kk zip/zip-td.c zip/zip-td-p.c zip/zip-td-mi.c zip/zip-bu.kk zip/zip-bu.c zip/zip-bu-p.c zip/zip-bu-mi.c zip/zip_bu.icl zip/zip-bu.ml zip/zip-bu.hs

run kk__zip-td__100000, iter 1, cmd: .koka/v3.1.2-bench/clang-release/zip-td
sum: 4999950000, height: 42/7, top: 13652, final access: 2015542571
elapsed: 1.19s, user: 1.19s, sys: 0.00s, rss: 8940kb

run c__zip-td__100000, iter 1, cmd: .koka/ccomp/zip-td
sum: 4999950000, height: 42/7, top: 13652, final access: 2015542571
elapsed: 1.11s, user: 1.10s, sys: 0.00s, rss: 6260kb

...

#    benchmark  variant  param   elapsed  relative  stddev     rss
...
kk   zip-td     -        100000  1.19     1.000     0          8940
c    zip-td     -        100000  1.11     .932      0          6260
ml   zip-td     -        100000  NA       0         0          0
hs   zip-td     -        100000  NA       0         0          0
cmi  zip-td     -        100000  0.97     .815      0          5948
##
kk   zip-bu     -        100000  1.14     1.000     0          8872
c    zip-bu     -        100000  1.12     .982      0          6308
ml   zip-bu     -        100000  4.52     3.964     0          13600
hs   zip-bu     -        100000  4.15     3.640     0          30428
cmi  zip-bu     -        100000  1.07     .938      0          8112
...
```

This runs the `zip` benchmark on the top-down (`td`) and bottom-up (`bu`)
variants. There is also a recursive `rec` variant for other benchmarks.
Eventually the bench provides a summary in absolute runtimes (and rss), 
and normalized runtimes relative to the Koka fip variant.

Note that the precise results depend quite a bit on the host system, but the 
relative performance should be similar (except when running in emulation).
The above results are on Ubuntu 22.0.4 with 16-core AMD 7950X @ 4.5Ghz.


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

The full expected results on an AMD7950X are at the bottom of this readme.
These should correspond closely to the results in Section 7 of the paper (Figure 3)
and support the conclusions drawn there. Note that the results can differ quite
bit among different systems, but if not running in emulation, the relative times 
should be quite similar. 


## Benchmark Descriptions

The benchmarks are described in detail in the paper.
TODO: describe each benchmark and system

- `mtr-(rec|td|bu)`
- `splay-(rec|td|bu)`
- `zip-(td|bu)`
- `rbtree-(rec|td|bu)`

- `c`
- `cmi`
- `hs`
- `ml`
- `kk`

# Notes

## Installing from Scratch

Prerequisites:

```
$ sudo apt install build-essential bc
$ sudo apt install clang g++
$ sudo apt install cmake
$ sudo apt install ocaml
```

We need Ghc 9.4.8+: use `ghcup` to install it:

```
$ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Install mimalloc:

```
$ git clone https://github.com/microsoft/mimalloc -b v2.1.1
$ cd mimalloc
$ mkdir -p out/release
$ cd out/release
$ cmake ../..
$ make
$ sudo make install
```

Build koka:
(paper benchmarks were with Koka v2.4.2 (or 2.4.3), use the tag `-b v2.4.2` on clone or checkout)

```
$ git clone --recursive https://github.com/koka-lang/koka -b artifact-pldi24
$ cd koka
$ stack build --fast
```
Benchmarking:

```
$ cd koka/test/artifact/pldi24
$ ./bench.sh build allb           # build all benchmarks
$ ./bench.sh run allb             # run all
$ ./bench.sh run zip              # just the zip benchmarks
$ ./bench.sh run allb -n=5        # all benchmarks avg over 5 runs
```

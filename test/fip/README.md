# Benchmarking

Run in the `bench` directory as:
```
./bench.sh allb build run
```

Options:

* `allb`: all benchmarks (also `allkk` and `allml` to select a subset, or `tmapkk`, `mapkk`, `rbtreekk`, or `kskk`).
* `build`: build benchmarks.
* `run`: run benchmarks and show benchmark scores (calculating median and stddev).
* `-n=<`N`>`: run each benchmark N times.
* `koka=<cmd>`: set koka compiler command explicitly.
* `ocamlopt=<cmd>`: set ocamlopt command explicitly.
* `ccomp=<cc>`: set C compiler for Koka, either `clang` or `gcc` (or `gcc-<version>`).
* `small`: do a small run for lists 0, 1, and 10.

The benchmarks are given the problem size `N` and run for `100_000_000/N` iterations.

# Prerequisites

## GNU time
Install gnu time if you don't have it:
```
# /usr/bin/time --version
GNU time 1.7
```

## Koka Dev.

Pull the `dev` branch of koka and build it.
Modify the `bench.sh` script to let `koka_dev_dir` point
to the development directory.

## Mimalloc

The `*_mimalloc.c` benchmarks rely on mimalloc:

```
# git clone https://github.com/microsoft/mimalloc
# cd mimalloc
# mkdir -p out/release
# cd out/release
# cmake ../..
# make
# sudo make install
```

The `build_c` function in `bench.sh` links against mimalloc.
This was only tested on Mac OS X and may have to be modified for other systems.
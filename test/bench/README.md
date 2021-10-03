# Build and run benchmarks

This contains the standard benchmark suite (discussed in detail in [Perceus] paper).
It is still basic but more benchmarks
with effect handlers are coming. The suite can run on (Ubuntu Linux), WSL2, and macOS,
and the benchmarks need:

- `gcc`. Should be there, otherwise use `sudo apt install gcc`,
- `ghc`. Use `sudo apt install ghc`,
- `ocamlopt`. Use `sudo apt install ocaml`,
- `swiftc`. The Swift compiler can be downloaded [here](https://swift.org/download/).
   The benchmarks expect `swiftc` to be installed at `/opt/swift/bin`,
   so unpack and copy everything under `swift-.../usr` to `/opt/swift/bin`:
   ```
   > tar -xzf swift-5.3-RELEASE-ubuntu20.04.tar.gz
   > cd swift-5.3-RELEASE-ubuntu20.04/usr
   > sudo mkdir /opt/swift
   > sudo cp -r * /opt/swift
   ```
- `javac`/`java`. We used these [instructions](https://computingforgeeks.com/install-oracle-java-openjdk-14-on-ubuntu-debian-linux/)
   to install the Java SE 15 Hotspot compiler:
   ```
   > sudo apt update
   > sudo add-apt-repository ppa:linuxuprising/java
   > sudo apt -y install oracle-java15-installer
   > sudo apt -y install oracle-java15-set-default
   > java --version
   java 15.0.1 2020-10-20
   Java(TM) SE Runtime Environment (build 15.0.1+9-18)
   Java HotSpot(TM) 64-Bit Server VM (build 15.0.1+9-18, mixed mode, sharing)
   ```

The benchmarks can now be build using:

```
> cd test/bench
> mkdir build
> cd build
> cmake .. -DCMAKE_BUILD_TYPE=Release
> cmake --build .
```

For some benchmarks, like `cfold`, we may need a large stack, so it may be good to raise the limit:
```
> ulimit -s unlimited
```

We can then run all benchmarks as:
```
> ctest .
```
Or only run benchmarks for one language with `-L <lang>`:
```
> ctest -L koka
```
Or run specific benchmarks using `-R <regex>`,
like the symbolic derivative benchmark:
```
> ctest -R deriv      
Test project /home/daan/dev/koka/test/bench/build
    Start  4: hs-deriv
1/4 Test  #4: hs-deriv .........................   Passed    2.29 sec
    Start 10: kk-deriv
2/4 Test #10: kk-deriv .........................   Passed    1.25 sec
    Start 19: ml-deriv
3/4 Test #19: ml-deriv .........................   Passed    1.73 sec
    Start 25: sw-deriv
4/4 Test #25: sw-deriv .........................   Passed    2.88 sec

100% tests passed, 0 tests failed out of 4
...
```

We can also run the tests using the `test/bench/bench.kk` script instead of
using `ctest` which also measures peak working set and calculates
normalized scores. For example, from the `build` directory, we can run all benchmarks as:
```
> koka -e ../bench
```
Use the `--lang` or `--test` options to specify a comma separated list of
languages or benchmarks:
```
> koka -e ../bench -- --lang=koka,ocaml  --test=rbtree,rbtree-ck
```
The `-i<N>` switch runs `N` iterations on each benchmark and calculates
the average and the error interval.

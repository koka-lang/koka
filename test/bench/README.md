Run on Linux with the following in the path:

* `stack` (build koka first)
* `ocamlopt`
* `swiftc` (install to /opt/swift, or set env var `SWIFT_ROOT`)

Steps:

```
$ cmake -DCMAKE_BUILD_TYPE=Release -S . -B build
$ cmake --build build
$ cd build
$ ctest
```

Select tests by language (ie. CTest label):

```
build$ ctest -L (koka|swift|cpp|haskell|ocaml)
```

Select tests by name fragment:

```
build$ ctest -R nqueens  # R for 'regex'
```

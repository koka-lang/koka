# Utilities

- `bundle.kk`: creates a fresh release bundle.
- `install.`[`bat`,`sh`]: installer scripts that install bundles.
- `minbuild.sh`: a script to run a build with minimal dependencies (if you don't have `stack` or `cabal`).
- `link-`[`min`,`test`,`std`]: wrapper module to build and link most standard libraries for an install bundle.
- `grammar.kk`: build and test the yacc & flex grammar.
- `packaging`: build packages for various Linux distributions.


# Releasing

Compile Koka:

```
$ stack build 
$ stack exec koka  # check if interpreter works

> :l samples/all
> all/main()
...

> :q

$ stack test
```

and create a bundle:

```
$ stack exec koka -- -e util/bundle.kk 
```

Copy the bundles from `bundle/v<version>/koka-v<version>-<os>-<arch>.tar.gz` and upload them.

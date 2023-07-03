# Utilities

- `bundle.kk`: creates a fresh release bundle.
- `install.`[`bat`,`sh`]: installer scripts that install bundles.
- `minbuild.sh`: a script to run a build with minimal dependencies (if you don't have `stack` or `cabal`).
- `link-`[`min`,`test`,`std`]: wrapper module to build and link most standard libraries for an install bundle.
- `grammar.kk`: build and test the yacc & flex grammar.
- `packaging`: build packages for various Linux distributions.


# Releasing

Ensure latest stack:

```
$ stack upgrade
$ stack update
```

Bump the Koka version in files:

- `package.yaml`  (2 places!)
- `util/install.sh`
- `util/install.bat`
- `util/Dockerfile`
- `util/minbuild.sh`

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

(On Windows, to this in an Visual Studio x64 command line tools console).

Test installation:

```
$ util/install.sh ./bundle/v<version>/koka-v<version>-<os>-<arch>.tar.gz
```

Copy the bundles from `bundle/v<version>/koka-v<version>-<os>-<arch>.tar.gz` and upload them.

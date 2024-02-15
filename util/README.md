# Utilities

- `bundle.kk`: creates a fresh release bundle.
- `install.`[`bat`,`sh`]: installer scripts that install bundles.
- `minbuild.sh`: a script to run a build with minimal dependencies (if you don't have `stack` or `cabal`).
- `link-`[`min`,`test`,`std`]: wrapper module to build and link most standard libraries for an install bundle.
- `grammar.kk`: build and test the yacc & flex grammar.
- `packaging`: build packages for various Linux distributions.
- `docs.kk`: generate documentation

# Releasing

Ensure latest stack:

```
$ stack upgrade
$ stack update
```

Bump the Koka version in files:

- `package.yaml`  (2 places!)
- `whatsnew.md`
- `util/install.sh`
- `util/install.bat`
- `util/minbuild.sh`
- `util/Dockerfile`
- `support/vscode/koka.language-koka/package.json`

Check if the `whatsnew.md` is up-to-date as it is shown
once the VS Code extension updates.

## Compile Koka

```
$ stack build
$ stack exec koka  # check if interpreter works

> :l samples/all
> all/main()
...

> :q
```
and run the test suite:

```
$ stack test
```

## Compile the VS Code extension:

```
$ cd support/vscode/koka.language-koka
$ npm install
$ npm run build
$ npm run package
$ cd ../../..
```

## Create a bundle:

```
$ stack exec koka -- -e util/bundle.kk
```

On Windows, do this in an Visual Studio x64 command line tools console, or release without `cl` compiled files (using just `clang-cl`)
(on Windows you may need to set `VCPKG_ROOT` to point to the vcpkg installation directory):

```
$ stack exec koka -- -e util/bundle.kk -- --nocl
```

Test installation:

```
$ util/install.sh ./bundle/v<version>/koka-v<version>-<os>-<arch>.tar.gz
```

or on Windows:

```
$ util/install.bat ./bundle/v<version>/koka-v<version>-<os>-<arch>.tar.gz
```

## Publish

Copy the bundles from `bundle/v<version>/koka-v<version>-<os>-<arch>.tar.gz` and upload them.
Also upload `util/install.bat` and `util/install.sh`.

Test installing those, and uninstall again.

Finally publish the new VS code extension:
```
$ cd support/vscode/koka.language-koka
$ npm run publish
```

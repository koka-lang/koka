# Utilities

- `bundle.kk`: creates a fresh release bundle.
- `install.`[`bat`,`sh`]: installer scripts that install bundles.
- `minbuild.sh`: a script to run a build with minimal dependencies (if you don't have `stack` or `cabal`).
- `link-`[`min`,`test`,`std`]: wrapper module to build and link most standard libraries for an install bundle.
- `grammar.kk`: build and test the yacc & flex grammar.
- `packaging`: build packages for various Linux distributions.
- `docs.kk`: generate documentation
- `reversion.kk`: Update version information 


# Releasing

Ensure latest stack:

```
$ stack upgrade
$ stack update
```

Bump the Koka version using `utils/reversion.kk`:

```
stack exec koka -- -e util/reversion -- -v <version>
```

where `<version>` is without a preceding `v`, like `-v 3.1.3`.
This will update the version in all required places.

Check if the `whatsnew.md` is up-to-date as it is shown once the VS Code extension updates.
Similarly for `readme.md` for the recent releases description.

Check if everything works:

```
$ stack build --fast
$ stack exec koka
...
> :l samples/all
> all/main()
...

> :q

$ stack test
```

## Make the Release

Ensure all changes are committed and the `readme.md` and `whatsnew.md` are updated
Run the following inserting the correct version.

```
git tag v<version>
git push origin v<version>
```

This will automatically start a Github action to create a draft release.
Make sure everything looks good on the `releases` tab and then publish as latest or prerelease.


# Releasing the VS Code extension

If the extension has not changed since the last release, you do not need to re-publish it.


## Compile the VS Code extension

```
$ cd support/vscode/koka.language-koka
$ npm install
$ npm run build
$ npm run package
$ cd ../../..
```

## Publishing the extension

Finally publish the new VS code extension:
```
$ cd support/vscode/koka.language-koka
$ npm run publish
```

# Manual Release Process (Old):

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

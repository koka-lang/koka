
# The Test Suite

To run tests, use stack:

```
$ stack test                                              # All tests
```

or with a debug build:
```
$ stack test --fast 
```

Select specific tests, or update tests using:
```
$ stack test --test-arguments="--match /parc/"                        # One category
$ stack test --test-arguments="--match /parc/parc4/"                  # One specific file
$ stack test --test-arguments="--mode=new --match /parc/parc4/"       # Create output files
$ stack test --test-arguments="--mode=update --match /parc/parc4/"    # Update output files
```

You can also use cabal:
```
$ cabal new-run koka-test -- --match /parc/
```

Options:
```
--mode=<new|update|test>
--match <match>              
--cabal                      # Use cabal to run koka.
--system-ghc                 # If using stack, use --system-ghc option.
```


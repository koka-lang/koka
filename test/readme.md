
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
--mode=<new|update|test>     # create new test output, or update existing one
--match <match>              # only match a specific test or test directory
--cabal                      # Use cabal to run koka.
--system-ghc                 # If using stack, use --system-ghc option.
--target-js                  # Test javascript backend
-O2                          # Use optimization
-O-1                         # Full debug mode with internal runtime assertions enabled
--seq                        # Test sequentially (instead of in parallel) 
```


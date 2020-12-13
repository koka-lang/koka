
# The Test Suite

To run tests, use stack:

```
> stack test                                              # All tests
> stack test --test-arguments="--match /parc/"            # One category
> stack test --test-arguments="--mode new"                # Create output files
> stack test --test-arguments="--mode update"             # Update output files
> stack test --test-arguments="--match /parc/ --mode new" # Combined
```
## Version 0.4.1 (2017-06-28)

- Fixed runtime type error that could occur when shrinking state machine commands (#91, @jystic)

## Version 0.4 (2017-06-28)

- Abstract state machine testing, check out the [process registry example](https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog-example/test/Test/Example/Registry.hs) to see how it works (#89, @jystic)
- `liftCatch`, `liftCatchIO`, `withCatch` functions for isolating exceptions during tests (#89, @jystic)

## Version 0.3 (2017-06-11)

- Exponential range combinators (#43, @chris-martin)
- Roundtrip example, check out the [blog post](http://teh.id.au/posts/2017/06/07/round-trip-property/) (#85, @thumphries)
- `tripping` now displays intermediate value (#85, @jystic)
- `distribute` function for pulling a transformer out to the top level (#83, @jystic)
- `withExceptT` function for executing tests with an inner `ExceptT` (e.g. `Test (ExceptT x m) a`) (#83, @jystic)

## Version 0.2.2 (2017-05-16)

- Fixed scope of `unicode` character generators (#76, @moodmosaic)
- Widen version bounds for some dependencies (#80, @amarpotghan)
- Expose test modules to fix build on nix / hydra (#78, @amarpotghan)
- Fixes for GHC 8.2 RC2 (#77, @erikd)

## Version 0.2.1 (2017-05-09)

- Added `ascii`, `latin1`, `unicode` character generators (#73, @jystic)

## Version 0.2 (2017-05-06)

- Added a quiet test runner which can be activated by setting `HEDGEHOG_VERBOSITY=0` (@jystic)
- Concurrent test runner does not display tests until they are executing (@jystic)
- Test runner now outputs a summary of how many successful / failed tests were run (@jystic)
- `checkSequential` and `checkParallel` now allow for tests to be run without Template Haskell (@jystic)
- Auto-discovery of properties is now available via `discover` instead of being baked in (@jystic)
- `annotate` allows source code to be annotated inline with extra information (@jystic)
- `forAllWith` can be used to generate values without a `Show` instance (@jystic)
- Removed uses of `Typeable` to allow for generating types which cannot implement it (@jystic)

## Version 0.4.1 (2017-06-28)

- Fixed runtime type error that could occur when shrinking state machine commands ([#91][91], [@jystic][jystic])

## Version 0.4 (2017-06-28)

- Abstract state machine testing, check out the [process registry example](https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog-example/test/Test/Example/Registry.hs) to see how it works ([#89][89], [@jystic][jystic])
- `liftCatch`, `liftCatchIO`, `withCatch` functions for isolating exceptions during tests ([#89][89], [@jystic][jystic])

## Version 0.3 (2017-06-11)

- Exponential range combinators ([#43][43], [@chris-martin][chris-martin])
- Roundtrip example, check out the [blog post](http://teh.id.au/posts/2017/06/07/round-trip-property/) ([#85][85], [@thumphries][thumphries])
- `tripping` now displays intermediate value ([#85][85], [@jystic][jystic])
- `distribute` function for pulling a transformer out to the top level ([#83][83], [@jystic][jystic])
- `withExceptT` function for executing tests with an inner `ExceptT` (e.g. `Test (ExceptT x m) a`) ([#83][83], [@jystic][jystic])

## Version 0.2.2 (2017-05-16)

- Fixed scope of `unicode` character generators ([#76][76], [@moodmosaic][moodmosaic])
- Widen version bounds for some dependencies ([#80][80], [@amarpotghan][amarpotghan])
- Expose test modules to fix build on nix / hydra ([#78][78], [@amarpotghan][amarpotghan])
- Fixes for GHC 8.2 RC2 ([#77][77], [@erikd][erikd])

## Version 0.2.1 (2017-05-09)

- Added `ascii`, `latin1`, `unicode` character generators ([#73][73], [@jystic][jystic])

## Version 0.2 (2017-05-06)

- Added a quiet test runner which can be activated by setting `HEDGEHOG_VERBOSITY=0` ([@jystic][jystic])
- Concurrent test runner does not display tests until they are executing ([@jystic][jystic])
- Test runner now outputs a summary of how many successful / failed tests were run ([@jystic][jystic])
- `checkSequential` and `checkParallel` now allow for tests to be run without Template Haskell ([@jystic][jystic])
- Auto-discovery of properties is now available via `discover` instead of being baked in ([@jystic][jystic])
- `annotate` allows source code to be annotated inline with extra information ([@jystic][jystic])
- `forAllWith` can be used to generate values without a `Show` instance ([@jystic][jystic])
- Removed uses of `Typeable` to allow for generating types which cannot implement it ([@jystic][jystic])

[jystic]:
  https://github.com/jystic
[chris-martin]:
  https://github.com/chris-martin
[thumphries]:
  https://github.com/thumphries
[moodmosaic]:
  https://github.com/moodmosaic
[amarpotghan]:
  https://github.com/amarpotghan
[erikd]:
  https://github.com/erikd

[91]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/91
[89]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/89
[85]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/85
[83]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/83
[80]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/80
[78]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/78
[77]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/77
[76]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/76
[73]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/73
[43]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/43

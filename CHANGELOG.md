## Version 0.6.1 (2018-09-22)

- Set stdout/stderr encoding to UTF-8 on Windows ([#218][218], [@moodmosaic][moodmosaic])

## Version 0.6 (2018-05-14)

- Pass [Dieharder][Dieharder] statistical/randomness tests ([#185][185], [@moodmosaic][moodmosaic])
- Catch `readFile` exceptions on the repl ([#184][184], [@thumphries][thumphries])

## Version 0.5.3 (2018-03-12)

- Add `Semigroup` and `Monoid` instances for `GenT` that lift the inner `Monoid` ([#156][156], [@andrewthad][andrewthad])
- `Gen.unicode` no longer generates non-characters ([#154][154], [@johnchandlerburnham][johnchandlerburnham])
- Documentation improvements ([#162][162], [@fisx][fisx])
- Documentation fixes ([#157][157], [@dredozubov][dredozubov])

## Version 0.5.2 (2018-02-05)

- Add doc explaining use of `withTests 1` ([#134][134], [@chris-martin][chris-martin])
- Explicitly define `Semigroup` instance for `Summary` ([#142][142], [@gwils][gwils])
- Depend on `semigroups` ([#140][140], [@LightAndLight][LightAndLight])
- Support `transformers-0.4` ([#150][150], [@gwils][gwils])

## Version 0.5.1 (2017-12-06)

- Only invoke `setNumCapabilities` when using the `-threaded` runtime ([#130][130], [@ekmett][ekmett])
- Correct `mixGamma` oddness check ([#124][124], [@markhibberd][markhibberd])

## Version 0.5 (2017-07-16)

- Parallel state machine testing, allows detection of commands which are not-atomic ([#98][98], [@jystic][jystic])
- Easier to use variables for state machine testing ([#94][94], [@jystic][jystic])
- `MonadGen` class allows the use of transformers like `ReaderT` and `StateT` on the outside of generators ([#99][99], [@jystic][jystic])
- Better error messages for tests which throw exceptions ([#95][95], [@jystic][jystic])
- Separated test input generation and assertions in to `PropertyT` and `TestT` respectively, this allows `TestT` to have a `MonadBaseControl` instance ([#96][96], [@jystic][jystic])
- This document grew links to the pull requests which introduced various changes ([#93][93], [@moodmosaic][moodmosaic])

## Version 0.4.1 (2017-06-28)

- Fixed runtime type error that could occur when shrinking state machine commands ([#91][91], [@jystic][jystic])

## Version 0.4 (2017-06-28)

- Abstract state machine testing, check out Tim Humphries' great [blog post](http://teh.id.au/posts/2017/07/15/state-machine-testing) or the [process registry example](https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog-example/test/Test/Example/Registry.hs) to see how it works ([#89][89], [@jystic][jystic])
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

[Dieharder]:
  https://webhome.phy.duke.edu/~rgb/General/dieharder.php

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
[ekmett]:
  https://github.com/ekmett
[markhibberd]:
  https://github.com/markhibberd
[gwils]:
  https://github.com/gwils
[LightAndLight]:
  https://github.com/LightAndLight
[johnchandlerburnham]:
  https://github.com/johnchandlerburnham
[andrewthad]:
  https://github.com/andrewthad
[dredozubov]:
  https://github.com/dredozubov
[fisx]:
  https://github.com/fisx

[218]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/218
[185]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/185
[184]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/184
[162]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/162
[157]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/157
[156]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/156
[154]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/154
[150]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/150
[142]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/142
[140]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/140
[134]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/134
[130]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/130
[124]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/124
[99]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/99
[98]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/98
[96]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/96
[95]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/95
[94]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/94
[93]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/93
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

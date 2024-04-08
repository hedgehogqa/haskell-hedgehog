## Version 1.5 (unreleased)

* Remove redundant `Show` constraints on `evalMaybe`, `evalMaybeM`

## Version 1.4 (2023-08-07)

* Fix skipping to tests/shrinks when tests have been discarded ([#489][489], [@ChickenProp][ChickenProp])

## Version 1.3 (2023-06-22)

* Better documentation for `Var` ([#491][491], [@endgame][endgame])
* Bump upper bounds for `ansi-terminal` ([#486][486], [@mpilgrem][mpilgrem])
* Better documentation for `Gen.filter[T]`, `Gen.mapMaybe[T]`, `Tree.prune` ([#485][485], [@ChickenProp][ChickenProp])
* Update Github CI actions, exclude Haddocks for old GHCs ([#482][482], [@ysangkok][ysangkok])
* Support GHC 9.6 ([#481][481], [@ysangkok][ysangkok])
* Bump upper bounds for `resourcet` and `primitive` ([#478][478], [@shlevy][shlevy])
* Export `Hedgehog.Internal.Seed.seed` ([#477][477], [@sol][sol])
* Better documentation for `sample` ([#468][468], [@parsonsmatt][parsonsmatt])
* Replace exceptions dependency with safe-exceptions ([#466][466], [@ocharles][ocharles])
* Generalise `Hedgehog.Gen.element` ([#411](411), [@ocharles][ocharles])

## Version 1.2 (2022-08-28)

* Allow skipping to a specific test number or shrink result ([#454][454], [@ChickenProp][ChickenProp])
  * Export shrinkPathCompress and shrinkPathDecompress ([#462][462], [@mbg][mbg])
* Support GHC 9.4 ([#461][461], [@ysangkok][ysangkok])
* Allow newer dependencies ([#457][457], [@ysangkok][ysangkok])
* Add Gen.subset ([#451][451], [@chris-martin][chris-martin])
* Add example for Gen.subsequence ([#450][450], [@chris-martin][chris-martin])
* Don't drop actions depending on shrunk predecessors ([#453][453], [@ChickenProp][ChickenProp])

## Version 1.1.2 (2022-09-02)

* Support GHC 9.4 ([#461][461], [@ysangkok][ysangkok])
* Allow newer dependencies ([#457][457], [@ysangkok][ysangkok])

## Version 1.1.1 (2022-01-29)

* Support using fixed seed via `HEDGEHOG_SEED` ([#446][446], [@simfleischman][simfleischman] / [@moodmosaic][moodmosaic])
* Compatibility with text-2.0 ([#443][443], [@sjakobi][sjakobi])
* Better 'cover' example code in haddocks ([#423][423], [@jhrcek][jhrcek])

## Version 1.1 (2022-01-27)

- Replace HTraversable with TraversableB (from barbies) ([#412][412], [@ocharles][ocharles])
- Support GHC 9.2 ([#436][436], [@patrickt][patrickt])

## Version 1.0.5 (2021-03-12)

- GHC 9 Support ([#392][392], [@utdemir][utdemir])
- Use binary shrinking for integral ([#413][413], [@HuwCampbell][HuwCampbell])
- Build tree from values instead of wrapping and unwrapping ([#414][414], [@HuwCampbell][HuwCampbell])
- Don't shrink the action chosen in state machine testing ([#415][415], [@HuwCampbell][HuwCampbell])
- Support shrinking 1-bit numbers for CLaSH ([#397][397], [@jonfowler][jonfowler] / [@jacobstanley][jacobstanley])

## Version 1.0.4 (2020-12-11)

- Bump ansi-terminal to 0.11 ([#394][394], [@mpilgrem][mpilgrem])
- Clean up hedgehog.cabal for GHC 8.0+ ([#391][391], [@felixonmars][felixonmars])
- Bump random to 1.2 ([#396][396], [@felixonmars][felixonmars])
- Improve the distribution of `Range.scaleLinear` ([#405][405], [@jonfowler][jonfowler] / [@moodmosaic][moodmosaic])
- Change `Gen.frequency` generator immediately shrink ([#406][406], [@ocharles][ocharles] / [@HuwCampbell][HuwCampbell])
- Add `Property.evalMaybe`, `Property.evalMaybeM` and `Property.evalEitherM` ([#381][381], [@markus1189][markus1189] / [@moodmosaic][moodmosaic])
- Bump QuickCheck to 2.14 ([#409][409], [@lehins][lehins])
- Bump bytestring to 0.11 ([#408][408], [@Bodigrim][Bodigrim])
- Minor Haddock formatting improvments ([#398][398], [@sshine][sshine] / [@moodmosaic][moodmosaic])

## Version 1.0.3 (2020-06-26)

- Bump cabal-version to 1.10 ([#390][390], [@moodmosaic][moodmosaic])
- Don't swallow errors if we can't find the source file ([#387][387], [@HuwCampbell][HuwCampbell])
- Add `Property.evalNF` ([#384][384], [@dcastro][dcastro])
- Add `Gen.either` and `Gen.either_` ([#382][382], [@dcastro][dcastro])
- Add `filterT`, `justT`, and `mapMaybeT` to `Gen` exports ([#366][366], [@kquick][kquick])
- Bump pretty-show to 1.10 which supports quasi-quotes ([#365][365], [@jacobstanley][jacobstanley])
- Remove `undefined` in `GenT`'s `MonadWriter` instance ([#344][344], [@HuwCampbell][HuwCampbell])
- Make `Tree.interleave` logarithmtic rather than linear ([#313][313], [@edsko][edsko])

## Version 1.0.2 (2020-01-10)
- Support GHC 8.10  ([#376][376], [@sjakobi][sjakobi])
- Speed up `Tree.splits` ([#349][349], [@treeowl][treeowl])
- Speed up `Gen.shuffle` ([#348][348], [@treeowl][treeowl])
- Add docs on the bounds of `Size` ([#346][346], [@chris-martin][chris-martin])
- Fix performance issues with color handling ([#345][345], [@stolyaroleh][stolyaroleh])
- Add `mapMaybe`, `mapMaybeT`, in `Tree` and `Gen` ([#339][339], [@treeowl][treeowl])
- Fix some formatting bugs in Haddock ([#332][332], [@sshine][sshine])
- Add `MonadGen` instances for `StateT` ([#321][321], [#330][330], [@HuwCampbell][HuwCampbell] / [@tomjaguarpaw][tomjaguarpaw] / [@symbiont-sam-halliday][symbiont-sam-halliday])
- Add `MonadBaseControl` instance for `PropertyT` ([#328][328], [@treeowl][treeowl])

## Version 1.0.1 (2019-09-16)
- Add compatibility with GHC 8.8 ([#319][319], [@erikd][erikd])
- Include location of failed assertion in report. This enables editors to more easily parse the location of failed test assertions, and provide links/jump functionality ([#308][308], [@owickstrom][owickstrom])
- Stop using filter to define unicode ([#303][303], [@ajmcmiddlin][ajmcmiddlin])
- Export LabelName from main module ([#299][299], [@erikd][erikd])

## Version 1.0 (2019-05-13)
- Add histograms to labels / coverage ([#289][289], [@jacobstanley][jacobstanley])
- Improved shrinking of lists ([#276][276], [@jacobstanley][jacobstanley] / [@edsko][edsko])
- Simplify `MonadGen`, this breaks the use of `StateT` on the outside of a `GenT` for the time being, it still works fine on the inside though and you can use `distributeT` to run it ([#276][276], [@jacobstanley][jacobstanley])
- Change `Applicative` `GenT` to use zipping ([#272][272], [@jacobstanley][jacobstanley] / [@edsko][edsko])
- Rename `Tree` -> `TreeT`, `Node` -> `NodeT` ([#272][272], [@jacobstanley][jacobstanley])
- `diff` function which takes any `a -> a -> Bool` comparison function ([#196][196], [@chessai][chessai] / [@jacobstanley][jacobstanley])
- Labelling of test runs via `label`, `collect` ([#262][262], [@ruhatch][ruhatch] / [@jacobstanley][jacobstanley])
- Classification of test runs via `cover`, `classify` ([#253][253], [@felixmulder][felixmulder] / [@jacobstanley][jacobstanley])
- Define proper `Applicative` instances for `NodeT`, `TreeT` and `GenT` ([#173][173][@sjakobi][sjakobi])
- `MonadFail` instance for `PropertyT` ([#267][267], [@geigerzaehler][geigerzaehler])
- `MonadResource` instance for `PropertyT` ([#268][268], [@geigerzaehler][geigerzaehler])
- Example for the `tripping` function ([#258][258], [@HuwCampbell][HuwCampbell])
- Improve documentation for state machine testing ([#252][252], [@endgame][endgame])
- `runTests` function for running tests from a top level executable, this was later renamed to `defaultMain` as is the de facto convention ([#168][168], [@erikd][erikd])
- Show output variables when parallel state machine testing fails to linearise ([#235][235], [@HuwCampbell][HuwCampbell])
- Note about `enumBounded` danger ([#202][202], [@thumphries][thumphries])
- Expose `discoverPrefix` to find prefixed properties ([#229][229], [@ruhatch][ruhatch])
- Remove use of `unix` package and replace with `lookupEnv` ([#226][226], [@puffnfresh][puffnfresh])

## Version 0.6.1 (2018-09-22)

- Fix UTF-8 related rendering bugs on Windows ([#218][218], [@moodmosaic][moodmosaic])
- Verify that our SplitMix/Seed avoids pathological γ-values ([#207][207], [@moodmosaic][moodmosaic])
- Avoid weak gamma values in Hedgehog.Internal.Seed ([#198][198], [@moodmosaic][moodmosaic])

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

- Parallel state machine testing, allows detection of commands which are not-atomic ([#98][98], [@jacobstanley][jacobstanley])
- Easier to use variables for state machine testing ([#94][94], [@jacobstanley][jacobstanley])
- `MonadGen` class allows the use of transformers like `ReaderT` and `StateT` on the outside of generators ([#99][99], [@jacobstanley][jacobstanley])
- Better error messages for tests which throw exceptions ([#95][95], [@jacobstanley][jacobstanley])
- Separated test input generation and assertions in to `PropertyT` and `TestT` respectively, this allows `TestT` to have a `MonadBaseControl` instance ([#96][96], [@jacobstanley][jacobstanley])
- This document grew links to the pull requests which introduced various changes ([#93][93], [@moodmosaic][moodmosaic])

## Version 0.4.1 (2017-06-28)

- Fixed runtime type error that could occur when shrinking state machine commands ([#91][91], [@jacobstanley][jacobstanley])

## Version 0.4 (2017-06-28)

- Abstract state machine testing, check out Tim Humphries' great [blog post](http://teh.id.au/posts/2017/07/15/state-machine-testing) or the [process registry example](https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog-example/test/Test/Example/Registry.hs) to see how it works ([#89][89], [@jacobstanley][jacobstanley])
- `liftCatch`, `liftCatchIO`, `withCatch` functions for isolating exceptions during tests ([#89][89], [@jacobstanley][jacobstanley])

## Version 0.3 (2017-06-11)

- Exponential range combinators ([#43][43], [@chris-martin][chris-martin])
- Roundtrip example, check out the [blog post](http://teh.id.au/posts/2017/06/07/round-trip-property/) ([#85][85], [@thumphries][thumphries])
- `tripping` now displays intermediate value ([#85][85], [@jacobstanley][jacobstanley])
- `distribute` function for pulling a transformer out to the top level ([#83][83], [@jacobstanley][jacobstanley])
- `withExceptT` function for executing tests with an inner `ExceptT` (e.g. `Test (ExceptT x m) a`) ([#83][83], [@jacobstanley][jacobstanley])

## Version 0.2.2 (2017-05-16)

- Fixed scope of `unicode` character generators ([#76][76], [@moodmosaic][moodmosaic])
- Widen version bounds for some dependencies ([#80][80], [@amarpotghan][amarpotghan])
- Expose test modules to fix build on nix / hydra ([#78][78], [@amarpotghan][amarpotghan])
- Fixes for GHC 8.2 RC2 ([#77][77], [@erikd][erikd])

## Version 0.2.1 (2017-05-09)

- Added `ascii`, `latin1`, `unicode` character generators ([#73][73], [@jacobstanley][jacobstanley])

## Version 0.2 (2017-05-06)

- Added a quiet test runner which can be activated by setting `HEDGEHOG_VERBOSITY=0` ([@jacobstanley][jacobstanley])
- Concurrent test runner does not display tests until they are executing ([@jacobstanley][jacobstanley])
- Test runner now outputs a summary of how many successful / failed tests were run ([@jacobstanley][jacobstanley])
- `checkSequential` and `checkParallel` now allow for tests to be run without Template Haskell ([@jacobstanley][jacobstanley])
- Auto-discovery of properties is now available via `discover` instead of being baked in ([@jacobstanley][jacobstanley])
- `annotate` allows source code to be annotated inline with extra information ([@jacobstanley][jacobstanley])
- `forAllWith` can be used to generate values without a `Show` instance ([@jacobstanley][jacobstanley])
- Removed uses of `Typeable` to allow for generating types which cannot implement it ([@jacobstanley][jacobstanley])

[Dieharder]:
  https://webhome.phy.duke.edu/~rgb/General/dieharder.php

[jacobstanley]:
  https://github.com/jacobstanley
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
[puffnfresh]:
  https://github.com/puffnfresh
[ruhatch]:
  https://github.com/ruhatch
[HuwCampbell]:
  https://github.com/HuwCampbell
[endgame]:
  https://github.com/endgame
[geigerzaehler]:
  https://github.com/geigerzaehler
[sjakobi]:
  https://github.com/sjakobi
[felixmulder]:
  https://github.com/felixmulder
[chessai]:
  https://github.com/chessai
[edsko]:
  https://github.com/edsko
[ajmcmiddlin]:
  https://github.com/ajmcmiddlin
[owickstrom]:
  https://github.com/owickstrom
[treeowl]:
  https://github.com/treeowl
[tomjaguarpaw]:
  https://github.com/tomjaguarpaw
[symbiont-sam-halliday]:
  https://github.com/symbiont-sam-halliday
[sshine]:
  https://github.com/sshine
[stolyaroleh]:
  https://github.com/stolyaroleh
[kquick]:
  https://github.com/kquick
[dcastro]:
  https://github.com/dcastro
[Bodigrim]:
  https://github.com/Bodigrim
[lehins]:
  https://github.com/lehins
[markus1189]:
  https://github.com/markus1189
[ocharles]:
  https://github.com/ocharles
[jonfowler]:
  https://github.com/jonfowler
[felixonmars]:
  https://github.com/felixonmars
[mpilgrem]:
  https://github.com/mpilgrem
[utdemir]:
  https://github.com/utdemir
[patrickt]:
  https://github.com/patrickt
[simfleischman]:
  https://github.com/simfleischman
[ChickenProp]:
  https://github.com/ChickenProp
[ysangkok]:
  https://github.com/ysangkok
[mbg]:
  https://github.com/mbg
[jhrcek]:
  https://github.com/jhrcek


[parsonsmatt]:
  https://github.com/parsonsmatt
[shlevy]:
  https://github.com/shlevy


[491]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/491
[489]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/489
[486]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/486
[485]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/485
[482]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/482
[481]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/481
[478]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/478
[477]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/477
[468]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/468
[466]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/466
[462]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/462
[461]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/461
[457]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/457
[454]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/454
[453]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/453
[451]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/451
[450]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/450
[446]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/446
[443]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/443
[436]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/436
[423]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/423
[415]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/415
[414]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/414
[413]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/413
[412]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/412
[411]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/411
[409]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/409
[408]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/408
[406]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/406
[405]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/405
[398]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/398
[397]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/397
[396]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/396
[394]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/394
[392]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/392
[391]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/391
[390]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/390
[387]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/387
[384]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/384
[382]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/382
[381]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/381
[376]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/376
[366]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/366
[365]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/365
[349]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/349
[348]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/348
[346]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/346
[345]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/345
[344]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/344
[339]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/339
[332]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/332
[330]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/330
[328]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/328
[321]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/321
[319]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/319
[313]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/313
[308]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/308
[303]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/303
[299]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/299
[289]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/289
[276]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/276
[272]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/272
[268]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/268
[267]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/267
[262]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/262
[258]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/258
[253]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/253
[252]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/252
[235]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/235
[229]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/229
[226]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/226
[218]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/218
[207]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/207
[202]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/202
[198]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/198
[196]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/196
[185]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/185
[184]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/184
[173]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/173
[168]:
  https://github.com/hedgehogqa/haskell-hedgehog/pull/168
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

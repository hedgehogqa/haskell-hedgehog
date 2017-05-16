## Version 0.2.2 (2017-05-16)

- Fixed scope of `unicode` character generators.
- Widen version bounds for some dependencies.
- Expose test modules to fix build on nix / hydra.

## Version 0.2.1 (2017-05-09)

- Added `ascii`, `latin1`, `unicode` character generators.

## Version 0.2 (2017-05-06)

- Added a quiet test runner which can be activated by setting `HEDGEHOG_VERBOSITY=0`
- Concurrent test runner does not display tests until they are executing.
- Test runner now outputs a summary of how many successful / failed tests were run.
- `checkSequential` and `checkParallel` now allow for tests to be run without Template Haskell.
- Auto-discovery of properties is now available via `discover` instead of being baked in.
- `annotate` allows source code to be annotated inline with extra information.
- `forAllWith` can be used to generate values without a `Show` instance.
- Removed uses of `Typeable` to allow for generating types which cannot implement it.

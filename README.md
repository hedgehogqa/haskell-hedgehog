hedgehog [![Hackage][hackage-shield]][hackage] [![Travis][travis-shield]][travis]
========

> Hedgehog will eat all your bugs.

<img src="https://github.com/hedgehogqa/haskell-hedgehog/raw/master/img/hedgehog-logo.png" width="307" align="right"/>

[Hedgehog](http://hedgehog.qa/) is a modern property-based testing
system, in the spirit of QuickCheck. Hedgehog uses integrated shrinking,
so shrinks obey the invariants of generated values by construction.

## Features

- Integrated shrinking, shrinks obey invariants by construction.
- Generators allow monadic effects.
- Range combinators for full control over the scope of generated numbers and collections.
- Equality and roundtrip assertions show a diff instead of the two inequal values.
- Template Haskell test runner which executes properties concurrently.

## Example

The main module, [Hedgehog][haddock-hedgehog], includes almost
everything you need to get started writing property tests with Hedgehog.

It is designed to be used alongside [Hedgehog.Gen][haddock-hedgehog-gen]
and [Hedgehog.Range][haddock-hedgehog-range] which should be imported
qualified. You also need to enable Template Haskell so the Hedgehog test
runner can find your properties.

```hs
{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
```

Once you have your imports set up, you can write a simple property:

```hs
prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
```

And add the Template Haskell splice which will discover your properties:

```hs
tests :: IO Bool
tests =
  checkConcurrent $$(discover)
```

If you prefer to avoid macros, you can specify the group of properties
to run manually instead:

```hs
tests :: IO Bool
tests =
  checkConcurrent $ Group "Test.Example" [
      ("prop_reverse", prop_reverse)
    ]
```

You can then load the module in GHCi, and run it:

```
λ tests
━━━ Test.Example ━━━
  ✓ prop_reverse passed 100 tests.

```

 [hackage]: http://hackage.haskell.org/package/hedgehog
 [hackage-shield]: http://img.shields.io/hackage/v/hedgehog.svg?style=flat

 [travis]: https://travis-ci.org/hedgehogqa/haskell-hedgehog
 [travis-shield]: https://travis-ci.org/hedgehogqa/haskell-hedgehog.svg?branch=master

 [haddock-hedgehog]: http://hackage.haskell.org/package/hedgehog/docs/Hedgehog.html
 [haddock-hedgehog-gen]: http://hackage.haskell.org/package/hedgehog/docs/Hedgehog-Gen.html
 [haddock-hedgehog-range]: http://hackage.haskell.org/package/hedgehog/docs/Hedgehog-Range.html

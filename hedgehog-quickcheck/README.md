hedgehog-quickcheck [![Hackage][hackage-shield]][hackage]
===================

> Hedgehog will eat all your bugs.

<img src="https://github.com/hedgehogqa/haskell-hedgehog/raw/master/img/hedgehog-logo.png" width="307" align="right"/>

Use [QuickCheck](http://hackage.haskell.org/package/QuickCheck) generators in [Hedgehog](http://hedgehog.qa/) and vice versa.

## Example

The [Hedgehog.Gen.QuickCheck][haddock-hedgehog-gen-quickcheck] module
allows the use of QuickCheck generators inside Hedgehog.

```hs
{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import qualified Hedgehog.Range as Range
```

Once you have your imports set up, you can write a property which mixes
QuickCheck and Hedgehog generators together:

```hs
prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.arbitrary :: Gen Char)
    reverse (reverse xs) === xs
```

And add the Template Haskell splice which will discover your properties:

```hs
tests :: IO Bool
tests =
  checkParallel $$(discover)
```

You can then load the module in GHCi, and run it:

```
λ tests
━━━ Test.Example ━━━
  ✓ prop_reverse passed 100 tests.
```

[hackage]:
  http://hackage.haskell.org/package/hedgehog-quickcheck
[hackage-shield]:
  https://img.shields.io/hackage/v/hedgehog-quickcheck.svg

[haddock-hedgehog-gen-quickcheck]:
  http://hackage.haskell.org/package/hedgehog-quickcheck/docs/Hedgehog-Gen-QuickCheck.html

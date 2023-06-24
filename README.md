<!--
Apologies to those who are able to read this. Unfortunately, Hackage
doesn't seem to render the HTML portion of the markdown spec so you may
be better off paying us a visit on GitHub instead:
https://github.com/hedgehogqa/haskell-hedgehog

https://preview.webflow.com/preview/hedgehogqa?utm_medium=preview_link&utm_source=dashboard&utm_content=hedgehogqa&preview=e42e956627c1ec686ee73fc48a20fb71&workflow=preview
-->

<div align="center">

<img width="400" src="https://github.com/hedgehogqa/haskell-hedgehog/raw/master/img/hedgehog-text-logo.png" />

# Release with confidence.

[![Hackage][hackage-shield]][hackage] [![GitHub CI][github-shield]][github-ci]

<div align="left">

[Hedgehog](http://hedgehog.qa/) automatically generates a comprehensive array of test cases, exercising your software in ways human testers would never imagine.

Generate hundreds of test cases automatically, exposing even the most insidious of corner cases. Failures are automatically simplified, giving developers coherent, intelligible error messages.

## Features

- Integrated shrinking, shrinks obey invariants by construction.
- Abstract state machine testing.
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
  checkParallel $$(discover)
```

If you prefer to avoid macros, you can specify the group of properties
to run manually instead:

```hs
{-# LANGUAGE OverloadedStrings #-}

tests :: IO Bool
tests =
  checkParallel $ Group "Test.Example" [
      ("prop_reverse", prop_reverse)
    ]
```

You can then load the module in GHCi, and run it:

```
λ tests
━━━ Test.Example ━━━
  ✓ prop_reverse passed 100 tests.

```

## In Memory of Jacob Stanley

As we come to the end of our guide to haskell-hedgehog, we'd like to take a moment to remember and honor one of its key contributors, our dear friend, author and co-founder, Jacob Stanley.

Jacob's passion for Haskell and his commitment to creating high-quality, reliable software was truly inspiring. His work has shaped haskell-hedgehog in countless ways, and without him, it wouldn't be the project it is today.

Jacob passed away unexpectedly on April 9th. His absence is deeply felt, but his impact on this project, and on all of us who had the privilege to work with him, remains. We continue to maintain and develop haskell-hedgehog in his memory and in honor of his dedication to excellence in programming.

As you explore haskell-hedgehog, and possibly contribute to its ongoing development, we invite you to join us in remembering Jacob Stanley — a tremendous developer, collaborator, and friend.

<div align="center">
<br />
<img width="307" src="https://github.com/hedgehogqa/haskell-hedgehog/raw/master/img/hedgehog-logo-grey.png" />

## Contributors

<a href="https://github.com/hedgehogqa/haskell-hedgehog/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=hedgehogqa/haskell-hedgehog" />
</a>

 [hackage]: http://hackage.haskell.org/package/hedgehog
 [hackage-shield]: https://img.shields.io/hackage/v/hedgehog.svg?style=flat

 [github-shield]: https://github.com/hedgehogqa/haskell-hedgehog/actions/workflows/ci.yaml/badge.svg
 [github-ci]: https://github.com/hedgehogqa/haskell-hedgehog/actions/workflows/ci.yaml

 [haddock-hedgehog]: http://hackage.haskell.org/package/hedgehog/docs/Hedgehog.html
 [haddock-hedgehog-gen]: http://hackage.haskell.org/package/hedgehog/docs/Hedgehog-Gen.html
 [haddock-hedgehog-range]: http://hackage.haskell.org/package/hedgehog/docs/Hedgehog-Range.html

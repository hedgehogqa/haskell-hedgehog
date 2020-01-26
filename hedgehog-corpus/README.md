hedgehog-corpus
===============
Collections of strings for testing things.

Usage
---------------

When you need some sensible human readable strings for property testing.

``` haskell

import qualified Test.QuickCheck as Q
import qualified Hedgehog.Corpus as Corpus

prop_test =
  Q.forAll (Q.elements Corpus.agile) $ \name ->
    ...

```

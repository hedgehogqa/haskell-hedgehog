hedgehog-corpus
===============
Collections of strings for testing things.

Usage
---------------

When you need some sensible human readable strings for property testing.

``` haskell
import           Data.Text (Text)

import           Hedgehog
import qualified Hedgehog.Corpus as Corpus
import qualified Hedgehog.Gen as Gen

genName :: Gen Text
genName =
  Gen.element Corpus.agile

genBetterName :: Gen Text
genBetterName =
  mconcat [
      Gen.element Corpus.agile
    , pure " "
    , Gen.element Corpus.animals
    ]
```

# Style Guide for Pull Requests

To ensure consistency throughout the codebase and to aid any of you that are
interested in contributing, please follow these instructions when structuring
your code:

## Indent using multiples of two spaces.

The most general rule is to keep everything aligned to a multiple of two spaces,
so we always wrap after `=` and `in`, like so:

```haskell
let
  foo =
    if quux xs0 then
      unlines xs0
    else
      unlines xs1
in
  length foo
```

Note the placement of the `then` immediately after the `if condition`, this
ensures the two space indentation is maintained because the positive case is
then indented on the following line.

This applies to data type definitions as well:

```haskell
data Foo =
  Foo {
      fooFieldA :: Type 
    , fooB :: Type
    } deriving (Show)

-- OR

data Foo =
  Foo {
      fooFieldA :: Type 
    , fooSecondField :: Type
    } deriving (Show)
```

```haskell
data Bar =
    A
  | B
  | C
  deriving (Show)
```

## When type signatures require newlines.

Long type signatures such as this are acceptable.

```haskell
function :: (MonadTest m, Applicative f, Traversable t) => t a -> Text -> f (a -> b) -> m ()
function = ...
```

But should a type signature become so long that is not easy to read on a single
line, please use the following as guides. Remembering to maintain the two space
indentation:

```haskell
function :: ( 
    MonadTest m
  , Applicative f
  , Traversable t
  ) 
  => t a 
  -> Text 
  -> f (a -> b) 
  -> m ()
function = ...
```

If the function uses a `forall` then include it before the first parenthesis:

```haskell
function :: forall m f t b. ( 
    MonadTest m
  , Applicative f
  , Traversable t
  ) 
  => t a 
  -> Text 
  -> f (a -> b) 
  -> m ()
function = ...
```

A line length limit of approximately 72-80 characters is suggested, but it is
not a strict rule. A line that is 87 characters long because it needs to be,
won't result in your code being rejected. :)

## Keep to single word names as much as possible.

Avoid names like `LogOfUpdates` and use single word names like `Journal`. This
applies to functions, variables, data types, and everything else.

## Avoid abbreviations / acronyms.

Avoid acronyms and abbreviations. Names such as x, x0, x1, y, xs are fine for
locals where types are obvious.

## No type synonyms, use `newtype`.

Avoid:

```haskell
type Bar = [String]
```

Instead:

```haskell
newtype Bar =
  Bar {
      unBar :: [String]
    } deriving (Show)
```

## Maintain indentation for typeclass definitions.

Avoid :

```haskell
instance Monoid Foo where
  mappend = (<>)
  mempty = Foo mempty
```

Put these on new lines, for example:

```haskell
instance Monoid Foo where
  mappend =
    (<>)
  mempty =
    Foo mempty
```

## Do not use `where`, only `let`.

Avoid:

```haskell
function :: MonadTest m => Foo -> Bar -> m ()
function foo bar = otherFunction foo newBar
  where
    newBar = someOtherFunction bar
```

Instead use a `let` expression:

```haskell
function :: MonadTest m => Foo -> Bar -> m ()
function foo bar = 
  let
    newBar =
      someOtherFunction bar
  in
    otherFunction foo newBar
```
Note that the two space indentation is maintained for the variables in the `let` binding.

## Avoid pattern matching and guards in function definitions.

Avoid pattern matching out variables and use `if` expressions over guards. The
following function would be rejected and you will be asked to change it to the
subsequent example.

```haskell
function :: Foo -> Bar
function (Foo fooA nestedBar)
  | null fooA = mempty
  | otherwise = nestedBar
```

Use an `if` or a `case` instead, bearing in mind that the indentation
requirements still apply. 

```haskell
function :: Foo -> Bar
function foo =
  if null (fooA foo) then 
    mempty
  else
    nestedBar foo
```

Use of the `LambdaCase` language extension is encouraged. Using this extension
you would rewrite the following function:

```haskell
foo :: Quux -> String
foo (Foo xs) =
  unlines xs
foo (Bar xs) =
  unlines xs
```

To this layout:

```haskell
foo :: Quux -> String
foo = \case
  Foo xs ->
    unlines xs
  Bar xs ->
    unlines xs
```

## Avoid using primes when naming things.

Using decimal numbers instead is preferred as they are harder to miss when
reading the code:

Avoid: `x'`,`x''`.

Instead: `x0`, `x1`.

## Use typeclass constraints where possible.

This works better with how Hedgehog tests are normally written and allows for
more general use, and leaning on parametricity is always helpful.

Avoid:
```haskell
... :: ... -> PropertyT IO ()
--
... :: ... -> Gen a
```

Instead:
```haskell
... :: MonadTest m => ... -> m ()
--
... :: MonadGen g => ... -> g a
```


If you require `IO` within the `m` then use the `MonadIO` constraint:

Instead:
```haskell
... :: (MonadIO m, MonadTest m) => ... -> m ()
```

## Module declarations and imports

The general order of module declarations is:

1. GHC/HADDOCK options pragmas
2. LANGUAGE pragmas
3. Module haddock documentation
4. Module declaration

Any change to the layout of imports should aim for the minimal number of changes
to lines and spacing. Adding a single function to an import list and then
applying `stylish-haskell` can result in a huge diff when it comes to reviewing
the code. So try to use it sparingly.

The two space indentation rule still applies for module headers:

```haskell
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Hedgehog.Internal.Source (
    LineNo(..)
  , ColumnNo(..)
  , Span(..)
  , getCaller

  -- * Re-exports from "GHC.Stack"
  , CallStack
  , HasCallStack
  , callStack
  , withFrozenCallStack
  ) where
```

ALWAYS USE EXPLICIT IMPORT LISTS!

Do not:

```haskell
import GHC.Conc
```

Use qualified imports and make the alias something easily identifiable, avoid
single letter aliases.

```haskell
import qualified GHC.Conc as Conc
import qualified Text.Read as Read
import qualified Data.Text as Text
```

If necessary you can include periods in the alias name:

```haskell
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
```

In cases where the API doesn't overlap, this is better:

```haskell
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
```

If you need to use types from a module then they should be in their own import
declaration:

```haskell
import           GHC.Conc (TVar)
import qualified GHC.Conc as Conc
```

## Haddocks / Comments required for all public facing functions

If the function is available to users then it needs to have documentation. Where
appropriate this should include examples of use.

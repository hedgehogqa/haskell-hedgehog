# Style Guide for Pull Requests

To ensure consistency throughout the codebase and to aid any of you that are
interested in contributing, please follow these instructions when structuring
your code:

# Indentation.

The most general rule is to try and keep everything aligned to a multiple of two
spaces, so we always wrap after `=` and `in`, like so:

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
    , fooFieldB :: Type
    } deriving Show
```

```haskell
data Bar =
    A
  | B
  | C
  deriving Show
```

# Long type signatures are displayed on newlines.

Avoid:

```haskell
function :: (MonadTest m, Applicative f, Traversable t) => t a -> Text -> f (a -> b) -> m ()
function = ...
```

Instead maintain the two space indentation:

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

# Keep to single word names as much as possible.

Try to avoid names like: `LogOfUpdates` and use single word names like `Journal`.

This applies to functions, variables, data types, and everything else.

# Avoid abbreviations / acronyms.

Avoid acronyms and abbreviations. Names such as x, x0, x1, y, xs are fine for
locals where types are obvious.

# No type synonyms, use `newtype`.

Avoid:

```haskell
type Bar = [String]
```

Instead:

```haskell
newtype Bar =
  Bar {
    unBar :: [String] 
    } deriving Show
```

# Maintain indentation for typeclass definitions.

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

# Do not use `where`, only `let`.

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

# Avoid pattern matching and guards in function defintions.

Avoid pattern matching out variables, and use `if` expressions over guards. The
following function would be rejected and you will be asked to change it to the
subsequent example.

```haskell
function :: Foo -> Bar
function (Foo fooA nestedBar)
  | null fooA = mempty
  | otherwise = nestedBar
```

Use an `if` or a `case` instead, bearing in mind that the indentation
requirements still apply:

```haskell
function :: Foo -> Bar
function foo =
  if null (fooA foo) then 
    mempty
  else
    nestedBar foo
```

# Avoid using primes when naming things.

Using decimal numbers instead is preferred as they are harder to miss when
reading the code:

Avoid: `x'`,`x''`.
Use: `x0`, `x1`.

# Use the `MonadTest` typeclass over concrete `PropertyT`.

This works better with how Hedgehog tests are normally written and allows for
more general use.

Avoid:
```haskell
... :: ... -> PropertyT IO ()
```

Instead:
```haskell
... :: MonadTest m => ... -> m ()
```
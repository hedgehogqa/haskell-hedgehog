hedgehog [![Hackage version](https://img.shields.io/hackage/v/hedgehog.svg?style=flat)](http://hackage.haskell.org/package/hedgehog) [![Build Status](https://travis-ci.org/hedgehogqa/haskell-hedgehog.svg?branch=master)](https://travis-ci.org/hedgehogqa/haskell-hedgehog)
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

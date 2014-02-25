Idempotent Monoids
==================

Idempotent monoids in Haskell.

This is a typeclass, `Idempotent` (a subclass of `Monoid`) indicating
that a monoid satisfies the law:

```haskell
a `mappend` a == a
```

[![Build Status](https://travis-ci.org/prophile/idempotent.png?branch=master)](https://travis-ci.org/prophile/idempotent)


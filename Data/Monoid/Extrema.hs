module Data.Monoid.Extrema(Min(Min), getMin, Max(Max), getMax) where

import Data.Monoid
import Data.Monoid.Idempotent

newtype Min x = Min { getMin :: x } deriving (Show, Read, Ord, Eq)
newtype Max x = Max { getMax :: x } deriving (Show, Read, Ord, Eq)

instance (Bounded x, Ord x) => Monoid (Min x) where
  mempty = Min maxBound
  Min x `mappend` Min y = Min (x `min` y)

instance (Bounded x, Ord x) => Idempotent (Min x)

instance (Bounded x, Ord x) => Monoid (Max x) where
  mempty = Max minBound
  Max x `mappend` Max y = Max (x `max` y)

instance (Bounded x, Ord x) => Idempotent (Max x)


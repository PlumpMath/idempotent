-- | Idempotent monoids.
module Data.Monoid.Idempotent(Idempotent) where

import Data.Monoid

-- | The class of monoids that are also idempotent.
--
-- Instances must satisfy the following law:
--
-- * @mappend a a = a@
class (Monoid m) => Idempotent m

instance Idempotent ()
instance Idempotent (First a)
instance Idempotent (Last a)
instance Idempotent Any
instance Idempotent All
instance Idempotent Ordering
instance (Idempotent m) => Idempotent (Dual m)
instance (Idempotent m) => Idempotent (r -> m)
instance (Idempotent a, Idempotent b) => Idempotent (a, b)
instance (Idempotent a, Idempotent b, Idempotent c) => Idempotent (a, b, c)
instance (Idempotent a, Idempotent b, Idempotent c, Idempotent d) => Idempotent (a, b, c, d)
instance (Idempotent a, Idempotent b, Idempotent c, Idempotent d, Idempotent e) => Idempotent (a, b, c, d, e)


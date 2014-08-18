-- | Idempotent monoids.
module Data.Monoid.Idempotent(Idempotent) where

import Data.Monoid

import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Lazy as ML
--import qualified Data.Map.Strict as MS
import qualified Data.IntMap.Lazy as IML
--import qualified Data.IntMap.Strict as IMS

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

instance Ord a => Idempotent (Set.Set a)
instance Idempotent IntSet.IntSet
instance Ord a => Idempotent (ML.Map a b)
--instance Ord a => Idempotent (MS.Map a b) -- same type
instance Idempotent (IML.IntMap a)
--instance Idempotent (IMS.IntMap a) -- same type


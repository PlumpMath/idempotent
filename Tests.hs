{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Prelude hiding ((.), id)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import Data.Monoid

import Data.Monoid.Idempotent

main :: IO ()
main = hspec $ do
  describe "unit" $ do
    it "satisfies the idempotence law" $ do
      () `mappend` () `shouldBe` ()

  describe "First" $ do
    it "satisfies the idempotence law" $ property $ do
      \x -> First x `mappend` First x `shouldBe` First (x :: Maybe Bool)

  describe "Last" $ do
    it "satisfies the idempotence law" $ property $ do
      \x -> Last x `mappend` Last x `shouldBe` Last (x :: Maybe Bool)

  describe "Any" $ do
    it "satisfies the idempotence law for False" $ do
      Any False `mappend` Any False `shouldBe` Any False

    it "satisfies the idempotence law for True" $ do
      Any True `mappend` Any True `shouldBe` Any True

  describe "All" $ do
    it "satisfies the idempotence law for False" $ do
      All False `mappend` All False `shouldBe` All False

    it "satisfies the idempotence law for True" $ do
      All True `mappend` All True `shouldBe` All True

  describe "Ordering" $ do
    it "satisfies the idempotence law for LT" $ do
      LT `mappend` LT `shouldBe` LT
    it "satisfies the idempotence law for EQ" $ do
      EQ `mappend` EQ `shouldBe` EQ
    it "satisfies the idempotence law for GT" $ do
      GT `mappend` GT `shouldBe` GT

  describe "functions" $ do
    it "satisy the idempotence law" $ property $ do
      \f x -> let f' = apply f in (f' `mappend` f') x `shouldBe` (f' (x :: Int) :: Ordering)

  describe "duals" $ do
    it "satisfy the idempotence law" $ property $ do
      \x -> Dual x `mappend` Dual x `shouldBe` Dual (x :: Ordering)

  describe "products" $ do
    describe "with two elements" $ do
      it "satisfy the idempotence law" $ property $ do
        \x y -> (x, y) `mappend` (x, y) `shouldBe` (x :: Ordering, y :: Ordering)

    describe "with three elements" $ do
      it "satisfy the idempotence law" $ property $ do
        \x y z -> (x, y, z) `mappend` (x, y, z) `shouldBe` (x :: Ordering, y :: Ordering, z :: Ordering)

    describe "with four elements" $ do
      it "satisfy the idempotence law" $ property $ do
        \x y z w -> (x, y, z, w) `mappend` (x, y, z, w) `shouldBe` (x :: Ordering, y :: Ordering, z :: Ordering, w :: Ordering)

    describe "with five elements" $ do
      it "satisfy the idempotence law" $ property $ do
        \x y z w v -> (x, y, z, w, v) `mappend` (x, y, z, w, v) `shouldBe` (x :: Ordering, y :: Ordering, z :: Ordering, w :: Ordering, v :: Ordering)

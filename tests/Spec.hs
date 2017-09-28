{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Data.AdditiveGroup
import Data.Monoid.Average
import Data.VectorSpace
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (monoid)


instance Arbitrary (Average Int) where
  arbitrary = Average <$> arbitrary

instance EqProp (Average Int) where
  x =-= y = getAverage (fmap fromIntegral x :: Average Double) =-= getAverage (fmap fromIntegral y)

main :: IO ()
main =
  hspec $
    describe "Average" $ do
      testBatch $ monoid (undefined :: Average Int)
      describe "laws for: Num" $ do
        describe "addition" $ do
          it "associativity" . property $ isAssoc @(Average Int) (+)
          it "commutative" . property $ isCommut @(Average Int) (+)
          it "left identity" . property $ leftId @(Average Int) (+) 0
          it "right identity" . property $ rightId @(Average Int) (+) 0
        describe "multiplication" $ do
          it "associativity" . property $ isAssoc @(Average Int) (*)
          it "commutative" . property $ isCommut @(Average Int) (*)
          it "left identity" . property $ leftId @(Average Int) (*) 1
          it "right identity" . property $ rightId @(Average Int) (*) 1
      describe "laws for: vector space" $ do
        it "associativity" . property $ isAssoc @(Average Int) (^+^)
        it "commutative" . property $ isCommut @(Average Int) (^+^)
        it "left identity" . property $ leftId @(Average Int) (^+^) zeroV
        it "right identity" . property $ rightId @(Average Int) (^+^) zeroV
        describe "closure" $ do
          it "distributive: c u v" . property $ \(c, u, v :: Average Int) ->
            c *^ (u ^+^ v) =-= (c *^ u) ^+^ (c *^ v)
          it "distributive: c d v" . property $ \(c, d, v :: Average Int) ->
            (c ^+^ d) *^ v =-= c *^ v ^+^ d *^ v
          it "associativity" . property $ \(c, d, v :: Average Int) ->
            c *^ (d *^ v) =-= (c * d) *^ v
          it "unitary" . property $ \(v :: Average Int) ->
            1 *^ v =-= v

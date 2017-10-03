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

getAverage = average
-- pureA = pure

instance Arbitrary (Average Rational) where
  arbitrary = pureA <$> arbitrary

-- instance Arbitrary (Average (Rational -> Rational)) where
  -- arbitrary = pureA <$> id

instance EqProp (Average Rational) where
  x  =-= y = (average x == average y) =-= True

main :: IO ()
main =
  hspec $
    describe "Average" $ do
      testBatch $ monoid (undefined :: Average Rational)
      describe "laws for: Num" $ do
        describe "addition" $ do
          it "associativity" . property $ isAssoc @(Average Rational) (+)
          it "commutative" . property $ isCommut @(Average Rational) (+)
          it "left identity" . property $ leftId @(Average Rational) (+) 0
          it "right identity" . property $ rightId @(Average Rational) (+) 0
        describe "multiplication" $ do
          it "associativity" . property $ isAssoc @(Average Rational) (*)
          it "commutative" . property $ isCommut @(Average Rational) (*)
          it "left identity" . property $ leftId @(Average Rational) (*) 1
          it "right identity" . property $ rightId @(Average Rational) (*) 1
      describe "laws for: vector space" $ do
        it "associativity" . property $ isAssoc @(Average Rational) (^+^)
        it "commutative" . property $ isCommut @(Average Rational) (^+^)
        it "left identity" . property $ leftId @(Average Rational) (^+^) zeroV
        it "right identity" . property $ rightId @(Average Rational) (^+^) zeroV
        it "left inverse" . property $ \(a :: Average Rational) -> negateV a ^+^ a =-= zeroV
        it "right inverse" . property $ \(a :: Average Rational) -> a ^+^ negateV a =-= zeroV
        describe "closure" $ do
          it "distributive: c u v" . property $ \(c, u, v :: Average Rational) ->
            c *^ (u ^+^ v) =-= (c *^ u) ^+^ (c *^ v)
          it "distributive: c d v" . property $ \(c, d, v :: Average Rational) ->
            (c ^+^ d) *^ v =-= c *^ v ^+^ d *^ v
          it "associativity" . property $ \(c, d, v :: Average Rational) ->
            c *^ (d *^ v) =-= (c * d) *^ v
          it "unitary" . property $ \(v :: Average Rational) ->
            1 *^ v =-= v

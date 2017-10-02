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
import Test.QuickCheck.Classes (applicative, functor, monoid)


instance Arbitrary (Average Int) where
  arbitrary = Average <$> (getPositive <$> arbitrary) <*> arbitrary

instance Arbitrary (Average (Int -> Int)) where
  arbitrary = Average <$> (getPositive <$> arbitrary) <*> arbitrary

instance EqProp (Average Int) where
  x =-= y = getAverage (fmap fromIntegral x :: Average Double) =-= getAverage (fmap fromIntegral y)

main :: IO ()
main =
  hspec $
    describe "Average" $ do
      testBatch $ monoid (undefined :: Average Int)
      testBatch $ functor (undefined :: Average (Int, Int, Int))
      describe "laws for: additive group" $ do
        it "associativity" . property $ isAssoc @(Average Int) (^+^)
        it "commutative" . property $ isCommut @(Average Int) (^+^)
        it "left identity" . property $ leftId @(Average Int) (^+^) zeroV
        it "right identity" . property $ rightId @(Average Int) (^+^) zeroV

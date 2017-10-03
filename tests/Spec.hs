{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Data.Monoid.Average
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (functor, monoid)


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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : stable
-- Portability : non-portable
--
--  Provides a monoid for calculating arithmetic means.
--
-------------------------------------------------------------------------------------

module Data.Monoid.Average (
    Average(..),
    getAverage,
    mayAverage
  ) where

import Prelude hiding ((**))

import Control.Applicative
import Control.Monad
import Data.AdditiveGroup
import Data.Maybe
import Data.Semigroup
import Data.Typeable
import Data.VectorSpace

-- |
-- A monoid for 'Average' values.
--
-- This average encapsulates length and sum in a space efficient form.
--
-- >>> getAverage $ foldMap averageDatum [1,2,3]
-- 2.0
--
data Average a = Average { averageWeight :: !Int, averageSum :: !a }
  deriving (Show, Typeable, Functor)

instance (Fractional a, Eq a) => Eq (Average a) where
  a == b = getAverage a == getAverage b

instance (Fractional a, Ord a) => Ord (Average a) where
  a `compare` b = getAverage a `compare` getAverage b

instance Applicative Average where
  pure = Average 1
  Average wf f <*> Average wx x = Average (wf + wx) $ f x

instance Num a => Num (Average a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Fractional a, Num a) => Fractional (Average a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance (Real a, Fractional a) => Real (Average a) where
  toRational = toRational . getAverage

instance Floating a => Floating (Average a) where
  pi = pure pi
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  sin = fmap sin
  tan = fmap tan
  cos = fmap cos
  asin = fmap asin
  atan = fmap atan
  acos = fmap acos
  sinh = fmap sinh
  tanh = fmap tanh
  cosh = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

instance Num n => Semigroup (Average n) where
  Average lx nx <> Average ly ny = Average (lx + ly) (nx + ny)

instance Num n => Monoid (Average n) where
  mappend = (<>)
  mempty = Average 0 0

instance AdditiveGroup a => AdditiveGroup (Average a) where
  zeroV = Average 0 zeroV
  (^+^) = liftA2 (^+^)
  negateV = fmap negateV

instance VectorSpace a => VectorSpace (Average a) where
  type Scalar (Average a) = Scalar a
  s *^ avg  = fmap (s *^) avg


{-
instance Arbitrary a => Arbitrary (Average a) where
  arbitrary = fmap Average arbitrary
-}

-- | Return the average of all monoidal components. If given 'mempty', return zero.
getAverage :: Fractional a => Average a -> a
getAverage = fromMaybe 0 . mayAverage

-- | Return the average of all monoidal components. If given 'mempty', return 'Nothing'.
mayAverage :: Fractional a => Average a -> Maybe a
mayAverage (Average 0 _) = Nothing
mayAverage (Average l x) = Just $ x / fromIntegral l

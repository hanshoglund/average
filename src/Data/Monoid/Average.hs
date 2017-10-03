
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

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

import Data.Typeable
import Data.Maybe
import Data.Function (on)
import Data.Semigroup
import Data.AdditiveGroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad
import Control.Applicative
import Data.MonoTraversable

-- |
-- A monoid for 'Average' values.
--
-- Represented by a running sum and a weight (i.e. a count of how many elements
-- have been summed, which allows calculating the average).
--
-- >>> getAverage $ foldMap averageDatum [1,2,3]
-- 2.0
--
data Average a = Average { averageWeight :: !Int, runningSum :: !a }
  deriving (Show, Typeable)

type instance Element (Average a) = a
instance (VectorSpace v, Fractional (Scalar v)) => MonoFunctor (Average v) where
  omap f (Average w a) = Average w $ w' *^ f (a ^/ w')
   where w' = fromIntegral w

instance MonoPointed (Average v) where
  opoint = Average 1
  
avgLiftA2 :: (VectorSpace v, Fractional (Scalar v))
             => (v -> v -> v) -> Average v -> Average v -> Average v
avgLiftA2 f (Average wx xΣ) (Average wy yΣ)
    = Average wΠ $ (f (xΣ^/fromIntegral wx) (yΣ^/fromIntegral wy))
                     ^* fromIntegral wΠ
 where wΠ = wx*wy

instance (VectorSpace v, Fractional (Scalar v)) => Semigroup (Average v) where
  a <> b = Average (on (+) averageWeight a b) (on (^+^) runningSum a b)

instance (VectorSpace v, Fractional (Scalar v)) => Monoid (Average v) where
  mempty = Average 0 zeroV
  mappend = (<>)
    
instance (VectorSpace v, Fractional (Scalar v), Eq v) => Eq (Average v) where
  a == b = getAverage a == getAverage b

instance (VectorSpace v, Fractional (Scalar v), Ord v) => Ord (Average v) where
  a `compare` b = getAverage a `compare` getAverage b

instance (Fractional a, VectorSpace a, Scalar a ~ a) => Num (Average a) where
  (+) = avgLiftA2 (+)
  (*) = avgLiftA2 (*)
  negate = omap negate
  abs    = omap abs
  signum = omap signum
  fromInteger = opoint . fromInteger
  
instance (Fractional a, VectorSpace a, Scalar a ~ a) => Fractional (Average a) where
  (/) = avgLiftA2 (/)
  fromRational = opoint . fromRational

instance (RealFrac a, VectorSpace a, Scalar a ~ a) => Real (Average a) where
  toRational = toRational . getAverage

instance (Floating a, VectorSpace a, Scalar a ~ a) => Floating (Average a) where
  pi = opoint pi
  exp = omap exp
  sqrt = omap sqrt
  log = omap log
  sin = omap sin
  tan = omap tan
  cos = omap cos
  asin = omap asin
  atan = omap atan
  acos = omap acos
  sinh = omap sinh
  tanh = omap tanh
  cosh = omap cosh
  asinh = omap asinh
  atanh = omap atanh
  acosh = omap acosh
  
instance (VectorSpace a, Fractional (Scalar a)) => AdditiveGroup (Average a) where
  zeroV = opoint zeroV
  (^+^) = avgLiftA2 (^+^)
  negateV = omap negateV

instance (VectorSpace a, Fractional (Scalar a)) => VectorSpace (Average a) where
  type Scalar (Average a) = Scalar a
  s *^ Average w v = Average w $ s*^v

instance (AffineSpace a, VectorSpace a, Diff a ~ a, Fractional (Scalar a))
                => AffineSpace (Average a) where
  type Diff (Average a) = Average (Diff a)
  (.-.) = (^-^)
  (.+^) = (^+^)

{-
instance Arbitrary a => Arbitrary (Average a) where
  arbitrary = fmap Average arbitrary
-}

-- | Return the average of all monoidal components. If given 'mempty', return zero.
getAverage :: (VectorSpace v, Fractional (Scalar v)) => Average v -> v
getAverage = maybe zeroV id . mayAverage

-- | Return the average of all monoidal components. If given 'mempty', return 'Nothing'.
mayAverage :: (VectorSpace v, Fractional (Scalar v)) => Average v -> Maybe v
mayAverage (Average 0 _) = Nothing
mayAverage (Average l x) = Just $ x ^/ fromIntegral l

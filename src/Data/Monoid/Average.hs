
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables, ViewPatterns #-}

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
    pureA,
    average,
    maybeAverage
  ) where

import Prelude hiding ((**))

import Data.Typeable
import Data.Maybe
import Data.Semigroup hiding (Sum)
import Data.AdditiveGroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad
import Control.Applicative
import Data.Functor.Identity
-- import Data.Functor.Representable
import Control.Monad.Writer hiding (Sum)
import Data.Coerce


-- |
-- A monoid for 'Average' values.
--
-- This is actually just the free monoid with an extra function 'average' for
-- extracing the (arithmetic) mean. This function is used to implement 'Real',
-- so you can use 'Average' whenever a ('Monoid', 'Real') is required.
--
-- >>> toRational $ mconcat [1,2::Average Rational]
-- 3 % 2
-- >>> toRational $ mconcat [1,2::Sum Rational]
-- 3 % 1
-- >>> toRational $ mconcat [1,2::Product Rational]
-- 2 % 1
--
newtype Average a = Average { getAverage :: (a, a) }
  deriving (Show, Typeable, Functor)
instance (Fractional a, Num a) => Monoid (Average a) where
  mempty = Average(0,0)
  (Average (a,wa)) `mappend` (Average (b,wb)) = Average(a + b, wa+wb)


pureA x = Average (x,1)

-- Writer (Sum Double) a -- doesn't work as
    -- mempty = (0,Weight 0) GOOD
    -- pure x = (x,Weight 0) BAD        -- actually OK
-- V2 a
    -- mempty = (0,0) GOOD
    -- pure x = (x,x) BAD               -- actually OK too

-- deriving instance Monoid (Average a)
-- deriving instance Num a => Num (Average a)
-- deriving instance Fractional a => Fractional (Average a)
-- deriving instance Real a => Real (Average a)
-- deriving instance Floating a => Floating (Average a)
-- deriving instance AdditiveGroup a => AdditiveGroup (Average a)

-- instance (Num w, Num a, f ~ Identity) => Num (WriterT w f a)
-- instance (Fractional w, Fractional a, f ~ Identity) => Fractional (WriterT w f a)
-- instance (Real w, Real a, f ~ Identity) => Real (WriterT w f a)
-- instance (Floating w, Floating a, f ~ Identity) => Floating (WriterT w f a)
-- instance (AdditiveGroup w, AdditiveGroup a, f ~ Identity) => AdditiveGroup (WriterT w f a)


-- , VectorSpace, AffineSpace

-- instance Num a => Monoid (Average a) where
--   mempty = coerce ((0 :: a),(1::Double))
--   mappend (coerce -> (a::a,as::Double)) (coerce -> (b::a,bs::Double)) = coerce (a+b,as+bs)
--
-- instance (Fractional a, Eq a) => Eq (Average a) where
--   a == b = average a == average b
--
-- instance (Fractional a, Ord a) => Ord (Average a) where
--   a `compare` b = average a `compare` average b

-- What should (+) and (*) do for Average values?
--
-- The important thing is to preserve scalar addition and multiplication (for example
-- scaling all components of) an average value by some constant factor, so we can just as
-- well use the standard list instance. What about averages with more components? I *think*
-- 'average' is a linear map, so they would work as expected:
--
-- >>> average (2<>2<>3)+average (3<>3)
-- 16 % 3
-- >>> average $ (2<>2<>3)+(3<>3)
-- 16 % 3
-- >>> average (mconcat [5,6,9])*average (mconcat[-1,0])
-- (-10) % 3
-- >>> average $ (mconcat [5,6,9])*(mconcat[-1,0])
-- (-10) % 3
--

instance (VectorSpace a, Num a, Fractional a, Eq a) => Num (Average a) where
  (+) = mappend
  s * v = average s *^ v
  negate = negateV
  -- abs    = fmap abs
  -- signum = fmap signum
  fromInteger = pureA . fromInteger

instance (VectorSpace a, Num a, Fractional a, Eq a) => Fractional (Average a) where
  v / s = v ^/ average s
  fromRational = pureA . fromRational
--
-- instance (Real a, Fractional a) => Real (Average a) where
--   toRational = toRational . average
--
-- instance Floating a => Floating (Average a) where
--   pi = pure pi
--   exp = fmap exp
--   sqrt = fmap sqrt
--   log = fmap log
--   sin = fmap sin
--   tan = fmap tan
--   cos = fmap cos
--   asin = fmap asin
--   atan = fmap atan
--   acos = fmap acos
--   sinh = fmap sinh
--   tanh = fmap tanh
--   cosh = fmap cosh
--   asinh = fmap asinh
--   atanh = fmap atanh
--   acosh = fmap acosh

instance (Fractional a, AdditiveGroup a, VectorSpace a) => AdditiveGroup (Average a) where
  zeroV = mempty
  (^+^) = mappend
  negateV v = (-1) *^ v

instance (Fractional a, VectorSpace a) => VectorSpace (Average a) where
  type Scalar (Average a) = a
  s *^ (Average (a,aw)) = (Average (s*a,s*aw))

-- instance (AdditiveGroup a, Scalar a ~ Average a, Fractional a, AffineSpace a) => AffineSpace (Average a) where
--   type Diff (Average a) = Average a
--   p1 .-. p2 = p1 ^-^ p2
--   p .+^ v   = p + v

{-
instance Arbitrary a => Arbitrary (Average a) where
  arbitrary = fmap Average arbitrary
-}

-- | Return the average of all monoidal components. If given 'mempty', return zero.
average :: (Eq a, Fractional a) => Average a -> a
average = fromMaybe 0 . maybeAverage

-- | Return the average of all monoidal components. If given 'mempty', return 'Nothing'.
maybeAverage :: (Eq a, Fractional a) => Average a -> Maybe a
maybeAverage (Average (a,0)) = Nothing
maybeAverage (Average (a,b)) = Just (a / b)
-- maybeAverage (Average []) = Nothing
-- maybeAverage (Average xs) = Just $ sum xs / fromIntegral (length xs)

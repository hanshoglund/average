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
    mayAverage,
    averageDatum
  ) where

import Prelude hiding ((**))

import Control.Applicative
import Control.Monad
import Data.AdditiveGroup
import Data.Function (on)
import Data.Maybe
import Data.Semigroup
import Data.Typeable

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

averageDatum :: n -> Average n
averageDatum = Average 1

instance (Fractional a, Eq a) => Eq (Average a) where
  a == b = getAverage a == getAverage b

instance (Fractional a, Ord a) => Ord (Average a) where
  a `compare` b = getAverage a `compare` getAverage b

instance Num n => Semigroup (Average n) where
  Average lx nx <> Average ly ny = Average (lx + ly) (nx + ny)

instance Num n => Monoid (Average n) where
  mappend = (<>)
  mempty = Average 0 0

instance AdditiveGroup a => AdditiveGroup (Average a) where
  zeroV = Average 0 zeroV
  x ^+^ y = Average (on (+) averageWeight x y) (on (^+^) averageSum x y)
  negateV = fmap negateV

-- | Return the average of all monoidal components. If given 'mempty', return zero.
getAverage :: Fractional a => Average a -> a
getAverage = fromMaybe 0 . mayAverage

-- | Return the average of all monoidal components. If given 'mempty', return 'Nothing'.
mayAverage :: Fractional a => Average a -> Maybe a
mayAverage (Average 0 _) = Nothing
mayAverage (Average l x) = Just $ x / fromIntegral l

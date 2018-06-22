{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EnumRandom where

import System.Random
import Test.QuickCheck

newtype Enum' a = Enum' a
  deriving (Bounded, Enum, Show)

enumRandomR :: (RandomGen g, Enum e) => (e, e) -> g -> (e, g)
enumRandomR  (lo,hi) gen = 
    let (int, gen') = randomR (fromEnum lo, fromEnum hi) gen in (toEnum int, gen')

enumRandom  :: (RandomGen g, Enum e, Bounded e) => g -> (e, g)
enumRandom gen = 
    let (e, gen') = enumRandomR (minBound, maxBound) gen in (e, gen')

instance (Enum a, Bounded a) => Arbitrary (Enum' a) where
  arbitrary = choose (minBound, maxBound)

instance (Enum a, Bounded a) => Random (Enum' a) where
  random = enumRandom
  randomR = enumRandomR

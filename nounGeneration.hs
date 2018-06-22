{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NounGeneration where

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

data Consonant = P | T | K | M | N | S | L deriving (Show, Enum, Bounded)
data Vowel = A | E | I | O | U deriving (Show, Enum, Bounded)

data Syllable = Syllable Consonant Vowel Consonant
instance Show Syllable where
    show (Syllable c1 v c2) = show c1 ++ show v ++ show c2

randomSyllable :: StdGen -> (Syllable, StdGen)
randomSyllable g = let (Enum' c1, g') = (random g)
                       (Enum' v, g'') = (random g')
                       (Enum' c2, g''') = (random g'')
                   in (Syllable c1 v c2, g''')

manyRandomSyllablesAcc :: StdGen -> Int -> [Syllable] -> ([Syllable], StdGen)
manyRandomSyllablesAcc g x acc 
                  | x <= 0 = (acc, g) 
                  | otherwise = let (s, g') = randomSyllable g
                                in manyRandomSyllablesAcc g' (x-1) (s:acc)

manyRandomSyllables :: StdGen -> Int -> ([Syllable], StdGen)
manyRandomSyllables g x = manyRandomSyllablesAcc g x []

getRandomSeed :: IO Int
getRandomSeed = do 
  randomSrc <- getStdGen
  return $ fst $ random $ randomSrc

main = do
  seed <- getRandomSeed
  putStrLn $ "Seed: " ++ (show seed)
  let (syl,_) = manyRandomSyllables (mkStdGen seed) 10
  mapM_ (\x -> putStrLn (show x)) syl

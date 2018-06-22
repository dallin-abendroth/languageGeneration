module Syllable where

data Consonant = P | T | K | M | N | S | L deriving (Show, Enum, Bounded)
data Vowel = A | E | I | O | U deriving (Show, Enum, Bounded)

data Syllable = Syllable Consonant Vowel Consonant
instance Show Syllable where
    show (Syllable c1 v c2) = show c1 ++ show v ++ show c2

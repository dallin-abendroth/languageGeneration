module SyllableGen where

import Syllable
import EnumRandom
import System.Random

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

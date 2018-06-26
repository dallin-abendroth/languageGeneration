module SyllableGen where

import Syllable
import EnumRandom
import System.Random
import Data.Array

chooseRandomItem :: StdGen -> [a] -> (a, StdGen)
chooseRandomItem g xs = let (index, g') = randomR (0, length xs) g
                            item = xs !! index
                        in (item, g')

generateConsonantOrthography :: StdGen -> [Consonant] -> [(Consonant, String)] -> ([(Consonant, String)], StdGen)
generateConsonantOrthography g rest acc = 
      case rest of 
       [] -> (acc, g)
       x:xs -> let (y, g') = chooseRandomItem g 
                                $ getConsMappings x
               in generateConsonantOrthography g' xs ((x,y):acc)

generateVowelOrthography :: StdGen -> [Vowel] -> [(Vowel, String)] -> ([(Vowel, String)], StdGen)
generateVowelOrthography g rest acc = 
      case rest of 
       [] -> (acc, g)
       x:xs -> let (y, g') = chooseRandomItem g 
                                $ getVowelMappings x
               in generateVowelOrthography g' xs ((x,y):acc)

genOrthography :: StdGen -> ((Array Consonant String, Array Vowel String), StdGen)
genOrthography g = let (consonants, g') = generateConsonantOrthography g [(minBound::Consonant)..] []
                       (vowels, g'')    = generateVowelOrthography g' [(minBound::Vowel)..] []
                   in ((array (minBound :: Consonant, maxBound :: Consonant) consonants,
                        array (minBound :: Vowel, maxBound :: Vowel) vowels), g'')

romanizePhoneme :: Phoneme -> (Array Consonant String, Array Vowel String) -> String
romanizePhoneme (ConsonantPhoneme c) orth = (fst orth) ! c
romanizePhoneme (VowelPhoneme v) orth = (snd orth) ! v

romanizeSyllable :: Syllable -> (Array Consonant String, Array Vowel String) -> String
romanizeSyllable (Syllable c1 v c2) orth = 
    romanizePhoneme (ConsonantPhoneme c1) orth ++ romanizePhoneme (VowelPhoneme v) orth ++ romanizePhoneme (ConsonantPhoneme c2) orth

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

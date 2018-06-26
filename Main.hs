module Main where

import Syllable
import SyllableGen
import System.Random

getRandomSeed :: IO Int
getRandomSeed = do 
  randomSrc <- getStdGen
  return $ fst $ random $ randomSrc

main = do
  seed <- getRandomSeed
  putStrLn $ "Seed: " ++ (show seed)
  let (syl,g') = manyRandomSyllables (mkStdGen seed) 10
  let (ortho,_) = genOrthography g'
  mapM_ (\x -> putStrLn (romanizeSyllable x ortho)) syl

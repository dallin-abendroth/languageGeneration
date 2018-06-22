module Main where

import SyllableGen
import System.Random

getRandomSeed :: IO Int
getRandomSeed = do 
  randomSrc <- getStdGen
  return $ fst $ random $ randomSrc

main = do
  seed <- getRandomSeed
  putStrLn $ "Seed: " ++ (show seed)
  let (syl,_) = manyRandomSyllables (mkStdGen seed) 10
  mapM_ (\x -> putStrLn (show x)) syl

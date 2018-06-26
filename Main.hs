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
  let (syl,g') = manyRandomSyllables (mkStdGen seed) 10
  let (ortho,g'') = genOrthography g'
  mapM_ (\x -> putStrLn ((show x)++" = "++(romanizeSyllable x ortho))) syl

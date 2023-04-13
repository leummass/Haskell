module Main (main) where

import System.Random

main :: IO ()

main = do
    gen <- newStdGen
    let random_number = fst $ randomR (1, 10) gen :: Int
    print random_number


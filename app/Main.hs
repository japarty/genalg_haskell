{- |
Module      : Main
Description : Algorithm searches exact sentence starting from generated list of ASCII values, and is based on genetic algorithm
Copyright   : Czech D., 2020
              Partyka J., 2020
License     : BSD-3
Maintainer  : jakubpart@gmail.com
Stability   : quite stable
Portability : POSIX
-}

module Main (main, genalg) where

import Lib

main :: IO ()
main = do
    putStrLn "Genetic Algorithm"
    putStrLn "Searched sentence: "
    sWord <- getLine
    putStrLn "Population size (has to be even number): "
    sSize <- getLine
    putStrLn "Maximum generations: "
    sGen <- getLine
    putStrLn "Cross chance: "
    sCrossChan <- getLine
    putStrLn "Mutation chance: "
    sMutChan <- getLine

    
    let size = read sSize :: Int
        gen = read sGen :: Int
        mutchan = read sMutChan :: Float
        crosschan = read sCrossChan :: Float
        chromosomes = length sWord
        word = t2a sWord
        ind = genList chromosomes
    pop <- genpop size chromosomes
    let przyst = [fitness word x | x <- pop]
    pop <- genalg 1 word pop mutchan size crosschan chromosomes gen
    print (a2t $ head pop)
    putStrLn "The end"

-- | Body of genetic algorithm. For each iteration prints tuple = (iteration, sum of differences in pop, example ind).
--  If one of the conditions is fullfiled recursion stops and returns population.
genalg :: Int -> [Int] -> [[Int]] -> Float -> Int -> Float -> Int -> Int -> IO [[Int]]
genalg i word pop mutchan size crosschan chromosomes gen =
  if sum [fitness word x | x <- pop] == 0 || i>gen
  then return pop
  else do print (i, sum [fitness word x | x <- pop], a2t $ head pop)
          pop <- mutWrap pop mutchan size chromosomes
          pop <- selectWrap word pop size
          pop <- crossDWrap pop crosschan chromosomes size
          genalg (i+1) word pop mutchan size crosschan chromosomes gen
          
module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Haskell genalg"
    putStrLn "Podaj wyraz"
    sWord <- getLine
    putStrLn "Size of population"
    sSize <- getLine
    putStrLn "Generations"
    sGen <- getLine
    putStrLn "Mutation chance"
    sMutChan <- getLine
    putStrLn "Crossing chance"
    sCrossChan <- getLine
    let size = read sSize :: Int
        gen = read sGen :: Int
--        mutchan = read sMutChan :: Float
--        crosschan = read sCrossChan :: Float
--        chromosomes = length sWord
--    genPop size chromosomes
    putStrLn sWord
--    putStrLn gen
--    putStrLn mutchan
--    putStrLn crosschan
--    putStrLn chromosomes
    someFunc

--TODO: sama funkcja algorytmu genetycznego (sklejenie powyższych funkcji tak, by było wykonywane)
--OGÓLNY TEMPLATE
--generuj
--oceń
--while przystosowanie != 1.0 or iter_index >=1000 do:
--  print (iter_index, przystosowanie)
--  selekcja
--  krzyżowanie
--  mutacja
--return
--skonwertuj z ASCII na tekst


--genalg ::
--genalg =
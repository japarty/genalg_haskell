module Main where

import Lib
import System.Random

main :: IO ()
main = do
    putStrLn "Algorytm genetyczny"
    
    putStrLn "Podaj slowo docelowe: "
    sWord <- getLine
    putStrLn "Podaj rozmiar populacji: "
    sSize <- getLine
    putStrLn "Podaj maksymalna liczbe generacji: "
    sGen <- getLine
    putStrLn "Podaj szanse na mutacje: "
    sMutChan <- getLine
    putStrLn "Podaj szanse na skrzyzowanie: "
    sCrossChan <- getLine
    
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
    putStrLn $ show $ a2t $ pop !! 1
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

genalg :: Int -> [Int] -> [[Int]] -> Float -> Int -> Float -> Int -> Int -> IO [[Int]]
genalg i word pop mutchan size crosschan chromosomes gen =
  if sum [fitness word x | x <- pop] == 0 || i>gen
  then return pop
  else do putStrLn $ show (sum [fitness word x | x <- pop])
          pop <- mutWrap pop mutchan size chromosomes
          pop <- selectWrap word pop size
          pop <- crossDWrap pop crosschan chromosomes size
--          return pop
          genalg (i+1) word pop mutchan size crosschan chromosomes gen
          
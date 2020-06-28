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
        geny = read sGen :: Int
        mutchan = read sMutChan :: Float
        crosschan = read sCrossChan :: Float
        chromosomes = length sWord
        word = t2a sWord
        ind = genList chromosomes
    putStrLn $ show word
    pop <- genpop size chromosomes
    putStrLn $ show pop
    let przyst = [fitness word x | x <- pop]
    putStrLn $ show przyst
    pop <- genalg word pop mutchan size chromosomes
    putStrLn $ show pop
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

--genalg ::
genalg word pop mutchan size chromosomes =
  if sum [fitness word x | x <- pop] == 0
  then return pop
  else do putStrLn $ show (sum [fitness word x | x <- pop])
          pop <- mutWrap pop mutchan size chromosomes
          pop <- selectWrap word pop size
          return pop
--          genalg word pop mutchan size cromosomes
          
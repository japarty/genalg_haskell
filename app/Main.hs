module Main where

import Lib
import System.Random

main :: IO ()
main = do

    --gen <- newStdGen
    
    --take 1 $ randomRs (0, 10) gen
    -- powinno wziąć 1 losowa liczbe z przedzialu 0-10

    --print $ randomList 5 0 10 gen
    -- powinno wygenerowac 5-elementowa, losowa liste z przedzialu 0-10
    
    putStrLn "Algorytm genetyczny"
    
    putStrLn "Podaj słowo docelowe: "
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
        --pop = genPop size chromosomes
        
    --putStrLn $ show pop
    someFunc

-- tutaj musimy wygenrować populacje, wg podanych wyżej parametrow (liczba osobnikow, ilosc genow)

-- potem, gdy juz ja bedziemy mieli - cos takiego: [[Int]], to jesli chcemy wywolywac funkcje na losowych osobnikach, to robimy tak: [[x]] !! y - gdzie y to jakas nasza wylosowana liczba oznaczajaca osobnika

-- bedziemy musieli takie liczby losowac do mutacji - zeby wylosowac osobnika, gen, ktory chcemy zmienic oraz na co ma on zostac zmieniony

-- oraz do krzyzowania, wtedy musimy wylosowac dwoch osobnikow oraz miejsce, w ktorym maja sie przeciac

-- nastepnie wywolujemy selekcje, ktora nam stwarza nowa populacje

-- i oceniamy przystosowanie osobnikow, jesli roznica miedzy jakims osobnikiem a naszym celem - podanym slowem wynosi 0 to zamykamy program, jesli nie to od nowa

-- to wszystko generealnie bedziemy musieli robic tu w mainie, korzystajac z pojedynczych funkcji z lib






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
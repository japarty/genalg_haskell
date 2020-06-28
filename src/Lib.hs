{-
Module      : Lib
Description : Genetic algorythm
Copyright   : Czech D., 2020
              Partyka J., 2020
              Sołtysiak W., 2020
License     : GPL-2
Maintainer  : jakubpart@gmail.com
Stability   : unstable %nie wiem, może będzie stable
Portability : POSIX
-}

module Lib
    ( t2a
    , a2t
    , fitness
    , mutacja
    , krzyzowanie
    , wyciagnij
    , someFunc
    , genList
    , genpop
   -- , randomList
   , mutwrap
   , posList
   , floatList
    ) where

import Data.List
import Data.Char
import System.Random


--genList :: Int -> [Int]
genList n = sequence $ replicate n $ randomRIO (0,127::Int)
posList n len = sequence $ replicate n $ randomRIO (0,len-1::Int)
floatList n = sequence $ replicate n $ randomRIO (0,1::Float)
--genpop s n = replicate s $ sequence $ replicate n $ randomRIO (1,32::Int)
--genpop s n = replicate s x
--  where 
--    do 
--      x <- genpop s n

-- wszystkie są generowane tak samo, więc do naprawy, ale chcę robić inne funkcje już więc chwilowo zostawiam
genpop s c = do
  x <- genList c
  let a = [x | _ <- [1..s]]
  return a
  
-- | Zamiana stringow na liste intow
-- = self explanatory, dziala na male i duze litery
t2a :: [Char] -> [Int]
t2a [] = []
t2a (x:xs) = [ord x] ++ t2a xs


-- | Zamiana listy intow na stringi
-- = self explanatory, dziala na male i duze litery
a2t :: [Int] -> [Char]
a2t [] = []
a2t (x:xs) = [chr (x)] ++ a2t xs


-- | Przystosowanie osobnika
-- = Funkcja sprawdza jak podobny jest podany osobnik do wyznaczonego wzorca
-- == Funkcja pobiera dwie listy: pierwsza wzorcowa, a druga do porownania z nia, zwraca liczbe oznaczajaca sume "odleglosci" wszystkich miejsc w liscie porownywanej od wzorca
fitness :: [Int] -> [Int] -> Int
fitness [] [] = 0
fitness (x:xs) (y:ys) = abs (x - y) + fitness xs ys


-- | Mutacja osobnika
-- = Funkcja zmienia jeden z genow osobnika, na nowy
-- == Funkcja pobiera osobnika - liste, liczbe oznaczajaca pozycje w liscie (liczac od 0), ktora ma ulec zmianie oraz nowy element, ktory ma sie znalezc w podanym miejscu, po czym zwraca zmodyfikowana liste
mutacja :: [Int] -> Int -> Int -> [Int]
mutacja xs y z = take y xs ++ [z] ++ (reverse(take ((length xs) - (y + 1)) (reverse xs)))

mutwrap pop mutchan size chromosomes = do mutlist <- floatList size
                                          positionlist <- posList size chromosomes
                                          newellist <- genList size
                                          let ziplist = zip4 pop mutlist positionlist newellist
                                              x = [if b<mutchan then mutacja a c d else a | (a,b,c,d) <- ziplist]
                                          return x

-- | Mutacja osobnikow
-- = Funkcja krzyzuje dwoch osobnikow w wyznaczonym miejscu
-- == Funkcja pobiera dwoch osobnikow - listy, liczbe oznaczajaca miejsce przeciecia sie tych dwoch list (liczac od 0), a nastepnie zwraca pare uporzadkowana z tymi dwoma osobnikami po tym, jak wyznaczone czesci tych list zamienily sie miejscami
krzyzowanie :: [Int] -> [Int] -> Int -> ([Int],[Int])
krzyzowanie xs ys z = (take z xs ++ (reverse(take ((length ys) - z) (reverse ys))), take z ys ++ (reverse(take ((length xs) - z) (reverse xs))))


-- | Wyciaganie pojedynczego osobnika z pary uporzadkowanej
-- = Funkcja "wyciaga" pierwszego lub drugiego osobnika z pary uporzadkwanej
-- == Funkcja pobiera pare uporzadkowana oraz liczbe oznaczajaca, ktory element tej pary zostanie "wyciagniety". 0 oznacza pierwszy element, a jakakolwiek inna liczba drugi element
wyciagnij :: ([Int],[Int]) -> Int -> [Int]
wyciagnij (xs,ys) z = if z == 0 then xs else ys


-- | Pojedynek na przystosowanie dwoch osobnikow
-- = Funkcja porownuje dwoch osobnikow pod wzgledem ich przystosowania (zblizenia do podanego wzorca)
-- == Funkcja pobiera docelowy wzorzec, a nastepnie dwoch osobnikow, nastepnie porownuje przystosowania tych dwoch osobnikow (ich podobienstwo do wzorca), by zwrocic bardziej przystosowanego - tego z mniejsca roznica od wzorca
turniej :: [Int] -> [Int] -> [Int] -> [Int]
turniej [] [] [] = []
turniej x y z = if (fitness x y) < (fitness x z) then y else z

-- TODO genalg, typ będzie jakiś taki :: [[Int]] -> [Int] -> ... -> [[Int]], gdzie po kolei mamy: stara populacja, wzorzecz, cos tam cookolwiek bedzie potrzebne do wywoylania innych funkcji, nowa populacja

someFunc :: IO ()
someFunc = putStrLn "Over"
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
    , mute
    , selection
    , cross
    , crossOut
    , someFunc
    , genList
    , genpop
    , mutWrap
    , posList
    , floatList
    , tourList
    , selectWrap
    , crossWrap
    , crossDWrap
    ) where

import Data.List
import Data.Char
import System.Random

genList n = sequence $ replicate n $ randomRIO (0,127::Int)
posList n len = sequence $ replicate n $ randomRIO (0,len-1::Int)
floatList n = sequence $ replicate n $ randomRIO (0,1::Float)
tourList n = sequence $ replicate n $ randomRIO (0,n-1::Int)

-- wszystkie są generowane tak samo, więc do naprawy, ale chcę robić inne funkcje już więc chwilowo zostawiam
genpop :: Int -> Int -> IO [[Int]]
genpop s c = do
  x <- genList c
  let a = [x | _ <- [1..s]]
  return a

--
--genpop pop 0 _ = []
--genpop pop s c = do x <- genList c
--                 let pop = pop ++ [x]
--                 return (genpop pop (s-1) c)
  
-- | Zamiana stringow na liste intow
-- = self explanatory, dziala na male i duze litery
t2a :: [Char] -> [Int]
t2a [] = []
t2a (x:xs) = [ord x] ++ t2a xs


-- | Zamiana listy intow na stringi
-- * self explanatory, dziala na male i duze litery
a2t :: [Int] -> [Char]
a2t [] = []
a2t (x:xs) = [chr x] ++ a2t xs

------------------------------------------------------------------------------------------------------------------------

-- | Przystosowanie osobnika
-- = Funkcja sprawdza jak podobny jest podany osobnik do wyznaczonego wzorca
-- == Funkcja pobiera dwie listy: pierwsza wzorcowa, a druga do porownania z nia, zwraca liczbe oznaczajaca sume "odleglosci" wszystkich miejsc w liscie porownywanej od wzorca
fitness :: [Int] -> [Int] -> Int
fitness [] [] = 0
fitness (x:xs) (y:ys) = abs (x - y) + fitness xs ys

------------------------------------------------------------------------------------------------------------------------

-- | Mutacja osobnika
-- = Funkcja zmienia jeden z genow osobnika, na nowy
-- == Funkcja pobiera osobnika - liste, liczbe oznaczajaca pozycje w liscie (liczac od 0), ktora ma ulec zmianie oraz nowy element, ktory ma sie znalezc w podanym miejscu, po czym zwraca zmodyfikowana liste
mute :: [Int] -> Int -> Int -> [Int]
mute xs y z = take y xs ++ [z] ++ reverse(take ((length xs) - (y + 1)) (reverse xs))

mutWrap :: [[Int]] -> Float -> Int -> Int -> IO [[Int]]
mutWrap pop mutchan size chromosomes = do mutlist <- floatList size
                                          positionlist <- posList size chromosomes
                                          newellist <- genList size
                                          let ziplist = zip4 pop mutlist positionlist newellist
                                              x = [if b<mutchan then mute a c d else a | (a,b,c,d) <- ziplist]
                                          return x

------------------------------------------------------------------------------------------------------------------------

-- | Mutacja osobnikow
-- = Funkcja krzyzuje dwoch osobnikow w wyznaczonym miejscu
-- == Funkcja pobiera dwoch osobnikow - listy, liczbe oznaczajaca miejsce przeciecia sie tych dwoch list (liczac od 0), a nastepnie zwraca pare uporzadkowana z tymi dwoma osobnikami po tym, jak wyznaczone czesci tych list zamienily sie miejscami
cross :: [Int] -> [Int] -> Int -> ([Int],[Int])
cross xs ys z = (take z xs ++ (reverse(take ((length ys) - z) (reverse ys))), take z ys ++ (reverse(take ((length xs) - z) (reverse xs))))


-- | Wyciaganie pojedynczego osobnika z pary uporzadkowanej
-- = Funkcja "wyciaga" pierwszego lub drugiego osobnika z pary uporzadkwanej
-- == Funkcja pobiera pare uporzadkowana oraz liczbe oznaczajaca, ktory element tej pary zostanie "wyciagniety". 0 oznacza pierwszy element, a jakakolwiek inna liczba drugi element
crossOut :: ([Int],[Int]) -> Int -> [Int]
crossOut (xs,ys) z = if z == 0 then xs else ys

crossWrap :: [[Int]] -> Float -> Int -> [Int] -> [Float] -> [[Int]]
crossWrap [] _ _ _ _ = []
crossWrap _ _ _ [] _ = []
crossWrap _ _ _ _ [] = []
crossWrap (p1:p2:pt) crosschan size (c:ct) (cc:cct) | cc < crosschan = do [crossOut (cross p1 p2 c) 0] ++ [crossOut (cross p1 p2 c) 1] ++ crossWrap pt crosschan size ct cct
                                                    | otherwise = [p1] ++ [p2] ++ crossWrap pt crosschan size ct cct

crossDWrap :: [[Int]] -> Float -> Int -> Int -> IO [[Int]]
crossDWrap pop crosschan chromosomes size = do let halfsize = size `div` 2
                                               cutList <- posList halfsize chromosomes
                                               crosschanList <- floatList halfsize
                                               let x = crossWrap pop crosschan size cutList crosschanList
                                               return x
------------------------------------------------------------------------------------------------------------------------
-- | Pojedynek na przystosowanie dwoch osobnikow
-- = Funkcja porownuje dwoch osobnikow pod wzgledem ich przystosowania (zblizenia do podanego wzorca)
-- == Funkcja pobiera docelowy wzorzec, a nastepnie dwoch osobnikow, nastepnie porownuje przystosowania tych dwoch osobnikow (ich podobienstwo do wzorca), by zwrocic bardziej przystosowanego - tego z mniejsca roznica od wzorca
selection :: [Int] -> [Int] -> [Int] -> [Int]
selection [] [] [] = []
selection x y z = if (fitness x y) < (fitness x z) then y else z

selectWrap :: [Int] -> [[Int]] -> Int -> IO [[Int]]
selectWrap word pop size = do first_fighters <- tourList size
                              second_fighters <- tourList size
                              let x = [selection word (pop !! a) (pop !! b) | (a,b) <- zip first_fighters second_fighters]
                              return x

------------------------------------------------------------------------------------------------------------------------

someFunc :: IO ()
someFunc = putStrLn "Over"
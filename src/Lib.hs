module Lib
    ( Ind (..)
    , Pop (..)
    , genInd
    , genPop
    , someFunc
    ) where

--import System.Random (randomR)


data Ind =
    Ind { chromosome :: [Int], fitness :: Int }
    deriving(Show, Read, Eq)

data Pop =
    Pop { size :: Int, inds :: [Ind] }
    deriving(Show, Read, Eq)

--TODO: text to ASCII
--pobiera zdanie podane jako input, zwraca listę z numerami ascii znaków

--TODO: ASCII to text
--jak wyżej, tylko odwrotnie

--TODO: generowanie populacji
-- input to Int = n osobników, zwraca obiekt Pop z n * Ind

genPop :: Int -> Int -> Pop
genPop s r = Pop s fs
    where
        fs = [genInd r | x <- [1..s]]

genInd :: Int -> Ind
genInd s = Ind fs 0
    where
        fs = [ 1 | y <- [1..s]]
        
--TODO: ocena przystosowania
--fit_eval :: Pop -> Pop

--TODO: selekcja
--selekcja rankingowa,
--selection :: Pop -> Pop

--for i in range(populacja):
--    a = losowy pierwszy woj
--    b = losowy drugi woj
--    if a_fit>b_fit:
--        append_nowa_pop(a)
--    else:
--        append_nowa_pop(b)

--TODO: krzyżowanie
--crossing :: Float -> Pop -> Pop

--TODO: mutacja
--mutation :: Float -> Pop -> Pop




--x = getStdRandom (randomR (1,6))
someFunc :: IO ()
someFunc = putStrLn "Over"
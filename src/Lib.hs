{- |
Module      : Lib
Description : Functions used in this genetic algorithm basic implementation
Copyright   : Czech D., 2020
              Partyka J., 2020
              Sołtysiak W., 2020
License     : BSD-3
Maintainer  : jakubpart@gmail.com
Stability   : quite stable
-}

module Lib
    ( t2a
    , a2t
    , genList
    , genpop
    , posList
    , floatList
    , tourList
    , fitness
    , mute
    , mutWrap
    , selection
    , selectWrap
    , cross
    , crossOut
    , crossWrap
    , crossDWrap
    ) where

import Data.List
import Data.Char
import System.Random

-- | Generate individual
genList :: Int -> IO [Int]
genList n = sequence $ replicate n $ randomRIO (0,127::Int)

-- | Generate gene index number for n individuals
posList :: Int -> Int -> IO [Int]
posList n len = sequence $ replicate n $ randomRIO (0,len-1::Int)

-- | Generate n Float values between <0,1)
floatList :: Int -> IO [Float]
floatList n = sequence $ replicate n $ randomRIO (0,1::Float)

-- | Generate n indexes of individuals
tourList :: Int -> IO [Int]
tourList n = sequence $ replicate n $ randomRIO (0,n-1::Int)

-- | Generate population
genpop :: 
          Int         -- ^ size of population
       -> Int         -- ^ length of individual
       -> IO [[Int]]  -- ^ population
genpop s c = if s==1
                then do x <- genList c
                        return [x]
                else do x <- genList c
                        xs <- genpop (s-1) c
                        return (x:xs)
  
-- | String to ASCII conversion
t2a :: [Char] -- ^ string
    -> [Int]  -- ^ list of ASCII values
t2a [] = []
t2a (x:xs) = ord x : t2a xs


-- | ASCII to string conversion
a2t :: [Int]  -- ^ list of ASCII values
    -> [Char] -- ^ string
a2t [] = []
a2t (x:xs) = chr x : a2t xs

------------------------------------------------------------------------------------------------------------------------

-- | Fitness evaluation of single individual
fitness :: [Int] -- ^ perfect 
        -> [Int] -- ^ individual
        -> Int   -- ^ sum of differences between perfect and individual
fitness [] [] = 0
fitness (x:xs) (y:ys) = abs (x - y) + fitness xs ys

------------------------------------------------------------------------------------------------------------------------

-- | Basic mutation of individual (change value of gene into something else)
mute :: [Int]  -- ^ individual
     -> Int    -- ^ place of mutation (counted from 0)
     -> Int    -- ^ new value
     -> [Int]  -- ^ new individual
mute xs y z = take y xs ++ [z] ++ reverse(take (length xs - (y + 1)) (reverse xs))

-- | Wrap function for mutation
mutWrap :: [[Int]]     -- ^ population
        -> Float       -- ^ chance of mutation
        -> Int         -- ^ size of population
        -> Int         -- ^ chromosomes
        -> IO [[Int]]  -- ^ new population
mutWrap pop mutchan size chromosomes = do mutlist <- floatList size
                                          positionlist <- posList size chromosomes
                                          newellist <- genList size
                                          let ziplist = zip4 pop mutlist positionlist newellist
                                              x = [if b<mutchan then mute a c d else a | (a,b,c,d) <- ziplist]
                                          return x

------------------------------------------------------------------------------------------------------------------------
-- | Basic crossing, mixes 2 individuals across cut point
cross :: [Int] -- ^ first individual
      -> [Int] -- ^ second individual
      -> Int   -- ^ cut point
      -> ([Int],[Int]) 
cross xs ys z = (take z xs ++ reverse (take (length ys - z) (reverse ys)), take z ys ++ reverse (take (length xs - z) (reverse xs)))


-- | Returns chosen individual from tuple
crossOut :: ([Int],[Int]) -- ^ tuple
         -> Int           -- ^ index of element
         -> [Int]         -- ^ individual
crossOut (xs,ys) z = if z == 0 then xs else ys

-- | Wrap function for crossing
crossWrap :: [[Int]] -- ^ population
          -> Float   -- ^ chance of crossing
          -> Int     -- ^ size of population
          -> [Int]   -- ^ cut points for each crossing
          -> [Float] -- ^ cut chance 
          -> [[Int]] -- ^ new population
crossWrap [] _ _ _ _ = []
crossWrap _ _ _ [] _ = []
crossWrap _ _ _ _ [] = []
crossWrap (p1:p2:pt) crosschan size (c:ct) (cc:cct) | cc < crosschan = [crossOut (cross p1 p2 c) 0] ++ [crossOut (cross p1 p2 c) 1] ++ crossWrap pt crosschan size ct cct
                                                    | otherwise = [p1] ++ [p2] ++ crossWrap pt crosschan size ct cct

-- | Handles operations used during crossing but needed to be completed inside do block 
crossDWrap :: [[Int]]    -- ^ population 
           -> Float      -- ^ chance of crossvalidation
           -> Int        -- ^ number of chromosomes
           -> Int        -- ^ size of population
           -> IO [[Int]] -- ^ new population
crossDWrap pop crosschan chromosomes size = do let halfsize = size `div` 2
                                               cutList <- posList halfsize chromosomes
                                               crosschanList <- floatList halfsize
                                               let x = crossWrap pop crosschan size cutList crosschanList
                                               return x
------------------------------------------------------------------------------------------------------------------------
-- | From a pair of individuals it choses more alike compared to the model individual
selection :: [Int] -- ^ model individual
          -> [Int] -- ^ first individual
          -> [Int] -- ^ second individual
          -> [Int] -- ^ best individual
selection [] [] [] = []
selection x y z = if fitness x y < fitness x z then y else z

-- | Wrap function for selection
selectWrap :: [Int]      -- ^ model individual
           -> [[Int]]    -- ^ population
           -> Int        -- ^ size of population
           -> IO [[Int]] -- ^ new population
selectWrap word pop size = do first_fighters <- tourList size
                              second_fighters <- tourList size
                              let x = [selection word (pop !! a) (pop !! b) | (a,b) <- zip first_fighters second_fighters]
                              return x
{-# LANGUAGE BangPatterns #-}

-- https://www.redblobgames.com/grids/hexagons/ for all things hex grid

module Main where

import Control.Applicative
import Data.List            (foldl')
import Data.List.Split      (splitOn)

data Dir = North
         | South
         | NorthWest
         | SouthWest
         | NorthEast
         | SouthEast
         deriving (Eq, Ord, Show)

instance Read Dir where
    readsPrec _ (d1:d2:ds)
        | d1 == 'n' && d2 == 'e' = [(NorthEast, ds)]
        | d1 == 'n' && d2 == 'w' = [(NorthWest, ds)]
        | d1 == 's' && d2 == 'e' = [(SouthEast, ds)]
        | d1 == 's' && d2 == 'w' = [(SouthWest, ds)]
    readsPrec _ (d1:ds)
        | d1 == 'n' = [(North, ds)]
        | d1 == 's' = [(South, ds)]

move' :: (Int, Int, Int) -> Dir -> (Int, Int, Int)
move' (x, y, z) North = (x + 0, y + 1, z + (-1))
move' (x, y, z) South = (x + 0, y + (-1), z + 1)
move' (x, y, z) NorthWest = (x + (-1), y + 1, z + 0)
move' (x, y, z) NorthEast = (x + 1, y + 0, z + (-1))
move' (x, y, z) SouthWest = (x + (-1), y + 0, z + 1)
move' (x, y, z) SouthEast = (x + 1, y + (-1), z + 0)

move :: ((Int, Int, Int), Int) -> Dir -> ((Int, Int, Int), Int)
move (pos, m) d =
    let pos'@(x, y, z) = move' pos d
        m' = max (abs x + abs y + abs z) m
    in m' `seq` (pos', m')

main :: IO ()
main = do

    ms <- (map read . splitOn ",") <$> getContents :: IO [Dir]

    let ((final_x, final_y, final_z), m) = foldl' move ((0, 0, 0), 0) ms

    print ((abs final_x + abs final_y + abs final_z) `div` 2, m `div` 2)

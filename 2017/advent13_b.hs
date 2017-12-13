{-# LANGUAGE BangPatterns #-}


module Main where


import Control.Applicative
import Data.List.Split       (splitOn)

import Debug.Trace

parse :: String -> (Int, Int)
parse s =
    let p:d:_ = map read $ splitOn ": " s
    in (p, d)

walk :: [(Int, Int)]   -- ^ scanners
     -> Int            -- ^ time
     -> Int            -- ^ delay
     -> Bool           -- ^ caught
walk [] _ _ = False
walk sss@((d, r):ss) !t delay
    | d == t - delay = mod t (r + r - 2) == 0 || walk ss (t + 1) delay
    | otherwise      = walk sss (t + 1) delay

main = do
    scanners <- map parse . lines <$> getContents
    let delay = head $ filter (\d -> not $ walk scanners d d) [0..]
    print delay

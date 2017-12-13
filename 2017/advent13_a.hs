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
     -> Int            -- ^ accumulated severity
     -> Int            -- ^ final severity
walk [] _ s = s
walk sss@((d, r):ss) !t !s
    | d == t    = if mod t (2 * r - 2) == 0
                    then walk ss (t + 1) (s + d * r)
                    else walk ss (t + 1) s
    | otherwise = walk sss (t + 1) s

main = do
    scanners <- map parse . lines <$> getContents
    let s = walk scanners 0 0
    print s

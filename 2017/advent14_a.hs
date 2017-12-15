{-# LANGUAGE BangPatterns #-}

module Main where

import Debug.Trace

import Control.Applicative
import Data.Bits
import Data.Char
import Data.List.Split       (splitOn)
import System.Environment    (getArgs)

import Text.Printf

select :: [Int]    -- ^ digits
       -> Int      -- ^ current pos
       -> Int      -- ^ length to take
       -> [Int]
select ds pos l =
    let size = length ds
        (prefix, suffix) = splitAt pos ds
    in if pos + l - 1 >= size
        then let tl =  take l $ suffix ++ ds
                 tl' = reverse $ tl
                 (selection, new_prefix) = splitAt (size - length prefix) tl'
                 overflow = l - (size - pos)
             in new_prefix ++ drop (l - (size - pos)) prefix ++ selection
        else let (selection, new_suffix) = splitAt l suffix
             in prefix ++ reverse selection ++ new_suffix

process :: [Int]    -- ^ digits
        -> Int      -- ^ skip size
        -> Int      -- ^ pos
        -> [Int]    -- ^ lengths
        -> ([Int], Int, Int)    -- ^ resulting List, skip and pos
process ds skip pos [] = (ds, skip, pos)
process ds !skip pos (l:ls) =
    let ds' = select ds pos l
        pos' = (pos + l + skip) `mod` length ds
        skip' = skip + 1
    in process ds' skip' pos' ls

process' :: [Int]
         -> Int
         -> Int
         -> [Int]
         -> Int
         -> [Int]
process' ds skip pos ls c
    | c == 0    = ds
    | otherwise = let (ds', skip', pos') = process ds skip pos ls
                  in process' ds' skip' pos' ls (c - 1)


{-
i = i - ((i >> 1) & 0x55555555);
     i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
     return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;

-}
countBits :: Int -> Int
countBits 0 = 0
countBits 1 = 1
countBits d = d `mod` 2 + countBits (d `shiftR` 1)

densify :: [Int]
        -> [Int]
densify [] = []
densify xs = densify' (take 16 xs) : densify (drop 16 xs)
  where densify' = foldr1 xor

main = do
    let suffix = [17, 31, 73, 47, 23]

    input <- head <$> getArgs
    let lss = map (\d -> map ord $ input ++ "-" ++ show d) [0..127]

    let ds = [0..255]
        skip = 0
        sparses = map (\ls -> process' ds skip 0 (ls ++ suffix) 64) lss
        denses = map densify sparses
        hashes = map (map countBits) denses

    putStr $ unlines $ map show hashes
    print $ sum $ map sum hashes

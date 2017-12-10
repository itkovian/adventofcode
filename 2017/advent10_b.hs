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
             in trace ("Wrapping. tl:" ++ show tl ++ " selection: " ++ show selection ++ " new_prefix: " ++ show new_prefix ++ " overflow: " ++ show overflow) $ new_prefix ++ drop (l - (size - pos)) prefix ++ selection
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
        pos' = (pos + l + skip) `mod` (length ds)
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


densify :: [Int]
        -> [Int]
densify [] = []
densify xs = densify' (take 16 xs) : densify (drop 16 xs)
  where densify' = foldr1 xor

main = do
    m <- read . head <$> getArgs   :: IO Int
    let suffix = [17, 31, 73, 47, 23]

    ls <- map ord <$> getContents

    let ds = [0..m]
        skip = 0
        sparse = process' ds skip 0 (ls ++ suffix) 64
        dense = densify sparse

        hash = concat $ map (printf "%02x") dense

    putStrLn hash

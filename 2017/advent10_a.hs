{-# LANGUAGE BangPatterns #-}

module Main where

import Debug.Trace

import Control.Applicative
import Data.List.Split       (splitOn)
import System.Environment    (getArgs)

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
        -> [Int]    -- ^ resulting List
process ds _ _ [] = ds
process ds !skip pos (l:ls) =
    let ds' = select ds pos l
        pos' = (pos + l + skip) `mod` (length ds)
        skip' = skip + 1
    in process ds' skip' pos' ls


main = do
    m <- read . head <$> getArgs   :: IO Int
    ls <- (map read . splitOn ",") <$> getContents

    let ds = [0..m]
        skip = 0
        final = process ds skip 0 ls

    print $ final!!0 * final!!1

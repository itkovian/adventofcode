{-# LANGUAGE BangPatterns #-}

module Main where

import Debug.Trace

import           Control.Applicative
import           Data.Bits
import           Data.Char
import qualified Data.Graph            as G
import           Data.List             (groupBy)
import           Data.List.Split       (splitOn)
import           System.Environment    (getArgs)

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
                 tl' = reverse tl
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

countBits :: Int -> Int
countBits 0 = 0
countBits 1 = 1
countBits d = d `mod` 2 + countBits (d `shiftR` 1)

densify :: [Int]
        -> [Int]
densify [] = []
densify xs = densify' (take 16 xs) : densify (drop 16 xs)
  where densify' = foldr1 xor

toV :: Int -> Int -> Int
toV row col = row `shiftL` 8 + col

bitify :: [Int] -> [Bool]
bitify = concatMap bt
   where bt d = reverse $ map (testBit d) [0..7]

hConns :: [Bool] -> Int -> [(Int, Int)]
hConns bs row =
    let is = map (map snd) $ filter (fst . head) $ groupBy (\a b -> fst a == fst b) $ zip bs [0..]
        vs = map (map (toV row)) is
    in concatMap tuplify vs
  where tuplify :: [Int] -> [(Int, Int)]
        tuplify [] = []
        tuplify [x] = [(x,x), (x,-x)]
        tuplify (x:y:[]) = [(x,y)]
        tuplify (x:y:xs) = (y, x): (x,y) : tuplify (y:xs)

vConns :: [Bool] -> [Bool] -> Int -> [(Int, Int)]
vConns l k row =
    let is = filter (\(a, b, _) -> a && b) $ zip3 l k [0..]
    in map (\(_, _, c) -> (toV row c, toV (row + 1) c)) is -- ++ map (\(_, _, c) -> (toV (row +1) c, toV row c)) is

buildGraph :: [[Bool]]
        -> Int
        -> Int                        -- ^ current row
        -> [[(Int, Int)]] -- ^ accumulated edges
        -> ([[(Int, Int)]], G.Graph )                   -- ^ final graph
buildGraph [] t _ es = (es, G.buildG (- toV t t, toV t t) $ concat es)
buildGraph [l] t row es = buildGraph [] t (row + 1) (hConns l row : es)
buildGraph (l:k:ls) t row es =
    let es' = hConns l row : vConns l k row : es
    in buildGraph (k:ls) t (row + 1) es'

main = do
    let suffix = [17, 31, 73, 47, 23]

    input <- head <$> getArgs
    let lss = map (\d -> map ord $ input ++ "-" ++ show d) [0..127]

    let ds = [0..255]
        skip = 0
        sparses = map (\ls -> process' ds skip 0 (ls ++ suffix) 64) lss
        denses = map densify sparses
        t = 128

        (_, g) = buildGraph (take t $ map (take t . bitify) denses) t 0 []

    print $ length $ filter (\(G.Node r s) -> not $ null s) $  G.components g

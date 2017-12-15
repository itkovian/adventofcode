{-# LANGUAGE BangPatterns #-}



module Main where

import Data.Bits
import Data.Maybe

generate :: Integer   -- ^ factor
         -> Integer   -- ^ divisor
         -> Integer   -- ^ current
         -> Integer   -- ^ next
generate !f !d !c = (c * f) `mod` d


check :: (Integer, Integer)
      -> Maybe Bool
check (a, b) =
    if a .&. 0xffff == b .&. 0xffff then Just True else Nothing

main = do

    let genA = 618
        genB = 814

        facA = 16807
        facB = 48271

        divisor = 2147483647


        genAs = iterate (generate facA divisor) genA
        genBs = iterate (generate facB divisor) genB

    print $ length . mapMaybe check . take 40000000 $ zip genAs genBs

    print $ length . mapMaybe check . take 5000000 $ zip (filter (\c -> c `mod` 4 == 0) genAs) (filter (\c -> c `mod` 8 == 0) genBs)

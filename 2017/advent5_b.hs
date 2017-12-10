{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Applicative
import qualified Data.IntMap.Strict      as M

update :: Int -> Int -> Maybe Int
update _ index = if index >= 3 then Just (index - 1) else Just (index + 1)
{-# INLINE update #-}

solve :: Int            -- ^ counter
      -> Int            -- ^ index
      -> M.IntMap Int   -- ^ array
      -> Int            -- ^ final count when we go out of bounds
solve !c !i !m =
    case M.updateLookupWithKey update i m of
        (Nothing, _) -> c
        (Just inc, m') -> solve (c+1) (i+inc) m'

main = do

    ds <- (map read . lines) <$> getContents

    let m = M.fromList $ zip [0..] ds

    print $ solve 0 0 m

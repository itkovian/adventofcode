{-# LANGUAGE BangPatterns #-}

module Main
    ( main
    ) where

import           Control.Applicative
import           Data.List           (foldl', sort)
import           Data.List.Split     (splitOn)
import qualified Data.Map.Strict     as M
import           Data.Maybe
import qualified Data.Set            as S

import           Debug.Trace

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char
          deriving (Eq, Show, Ord)

parse :: String -> Move
parse ('s':s) = Spin $ read s
parse ('x':s) = let a:b:_ = splitOn "/" s in Exchange (read a) (read b)
parse ('p':s) = let a:b:_ = splitOn "/" s in Partner (head a) (head b)


swapMap :: Int -> Int -> M.Map Int Char -> M.Map Char Int -> (M.Map Int Char, M.Map Char Int)
swapMap x y m m' =
    let a = fromJust $ M.lookup x m
        b = fromJust $ M.lookup y m
    in (M.insert y a $ M.insert x b m, M.insert a y $ M.insert b x m')
{-# INLINE swapMap #-}

swapMap' :: Char -> Char -> M.Map Int Char -> M.Map Char Int -> (M.Map Int Char, M.Map Char Int)
swapMap' a b m m' =
    let x = fromJust $ M.lookup a m'
        y = fromJust $ M.lookup b m'
    in (M.insert y a $ M.insert x b m, M.insert a y $ M.insert b x m')
{-# INLINE swapMap' #-}

move :: (M.Map Int Char, M.Map Char Int) -> Move -> (M.Map Int Char, M.Map Char Int)
move (!ps, !cs) (Spin n) =
    let pss = M.toList ps
        d = length pss
        pss' = map (\(i, c) -> ((i + n) `mod` d, c)) pss
    in (M.fromList pss' , M.fromList $ map (\(i, c) -> (c, i)) pss')
move (!ps, !cs) (Exchange x y) = swapMap x y ps cs
move (!ps, !cs) (Partner a b) = swapMap' a b ps cs

dance moves (pMap, cMap) _ = foldl' move (pMap, cMap) moves


main = do
    moves <- map parse . splitOn "," <$> getContents

    let str = "abcdefghijklmnop"

    let cMap = M.fromList $ zip str [0..]
        pMap = M.fromList $ zip [0..] str

    let pos = dance moves (pMap, cMap) 1
        perm = map snd $  sort $ M.toList $ fst pos

    let pss = map ((map snd . sort .  M.toList) . fst) $ take 100 $ iterate (\m -> dance moves m 1) (pMap, cMap)

    putStrLn $ unlines $ map show $ sort $zip pss [0..]

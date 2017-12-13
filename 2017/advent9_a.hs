{-# LANGUAGE BangPatterns #-}

module Main where


import Control.Applicative


parse :: Int -> Int -> Int -> Bool -> String -> String -> (Int, Int)
parse accum _ count _ [] _ = (accum, count)
parse !accum !depth !count garbage (c:cs) stack
    | garbage && c == '>' = parse accum depth count False cs stack
    | garbage && c == '!' = parse accum depth count garbage (tail cs) stack
    | garbage && (c == '{' || c == '}') = parse accum depth (count + 1) garbage cs stack
    | garbage = parse accum depth (count + 1) garbage cs stack
    | c == '<' = parse accum depth count True cs stack
    | c == '{' = parse accum (depth + 1) count garbage cs (c:stack)
    | c == '}' = parse (accum + depth) (depth - 1) count garbage cs (tail stack)
    | otherwise = parse accum depth count garbage cs stack


main = do
    input <- getContents

    let c = parse 0 0 0 False input []

    print c

module Main where

summarise _ [] s = s
summarise d (x:[]) s = summarise d [] (if x == d then s+x else s)
summarise d (x:y:xs) s = summarise d (y:xs) (if x == y then s+x else s)

main = do
    input <- getLine

    let digits = map (\d -> read [d] :: Int) input

    print $ summarise (head digits) digits 0

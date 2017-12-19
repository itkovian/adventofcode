{-# LANGUAGE BangPatterns #-}


module Main where

move :: Int    -- ^ step
     -> Int    -- ^ stepsize
     -> Int    -- ^ current position
     -> Int    -- ^ current second element
     -> Int    -- ^ value after the starting 0
move !step stepsize !pos !second
    | step == 50000000 = second
    | otherwise =
        let buffersize = step + 1
            pos' = (pos + stepsize) `mod` buffersize
            second' = if pos' == 0 then step + 1 else second
        in move (step + 1) stepsize (pos' + 1) second'


main :: IO ()
main = do
    let input = 370

        p = move 0 input 0 0

    print $ p

{-# LANGUAGE BangPatterns #-}


module Main where

move :: Int    -- ^ step
     -> Int    -- ^ stepsize
     -> Int    -- ^ current position
     -> [Int]  -- ^ current buffer
     -> (Int, [Int])    -- ^ value after the last insert
move !step stepsize !pos bs
    | step == 2017 = (pos, bs)
    | otherwise =
        let buffersize = step + 1
            pos' = (pos + stepsize) `mod` buffersize
            (prefix, suffix) = splitAt (pos' + 1) bs
        in move (step + 1) stepsize (length prefix) (prefix ++ (step+1) : suffix)


main :: IO ()
main = do
    let input = 370

        (p, values) = move 0 input 0 [0]


    print values
    print $ head $ drop (p + 1) values

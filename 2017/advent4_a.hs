module Main where

import Control.Applicative
import Data.List

main = do

    passwords <- (map (group . sort . words) . lines) <$> getContents

    print $ length $ filter (all ((==) True) . map (((==) 1) . length))  passwords

module Main where

import Data.List (intercalate)
import Prelude hiding (Left, Right)
import System.Environment

data Dir = Up
         | Left
         | Down
         | Right
         deriving (Eq, Show, Ord)

nextDir Up = Left
nextDir Left = Down
nextDir Down = Right
nextDir Right = Up

coords :: Int            -- ^ target value
       -> Int            -- ^ current value
       -> Int            -- ^ increment
       -> (Int, Int)     -- ^ current coord
       -> Dir            -- ^ Direction to move in
       -> Int            -- ^ count
       -> (Int, Int)     -- ^ target coords
coords target current inc (x, y) d count
    | current == target = (x, y)
    | current + inc > target =
        case d of
            Up    -> (x, y + (target - current))
            Left  -> (x - (target - current), y)
            Down  -> (x, y - (target - current))
            Right -> (x + (target - current), y)
    | otherwise =
        let (x', y') = case d of
                Up    -> (x, y+inc)
                Left  -> (x - inc, y)
                Down  -> (x, y - inc)
                Right -> (x + inc, y)
            nextInc = if count == 0 then inc + 1 else inc
            nextCount = if count == 0 then 1 else 0
        in coords target (current + inc) nextInc (x', y') (nextDir d) nextCount


main = do
    v <- fmap (read . head) $ getArgs

    let (x, y) = coords v 1 1 (0, 0) Right 1

    print (x, y)
    print $ abs x + abs y


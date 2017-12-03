module Main where

import Control.Applicative
import Data.List (intercalate)
import Data.Maybe
import Prelude hiding (Left, Right)
import System.Environment
import qualified Data.Map as M

data Dir = Up
         | Left
         | Down
         | Right
         deriving (Eq, Show, Ord)

nextDir Up = Left
nextDir Left = Down
nextDir Down = Right
nextDir Right = Up

nextCoord (x, y) d =
    case d of
        Up    -> (x, y + 1)
        Left  -> (x - 1, y)
        Down  -> (x, y - 1)
        Right -> (x + 1, y)


coords :: Int                    -- ^ target value
       -> Int                    -- ^ current value
       -> Int                    -- ^ increment
       -> Int                    -- ^ current step in this increment
       -> (Int, Int)             -- ^ current coord
       -> Dir                    -- ^ direction to move in
       -> Int                    -- ^ count to see when we need to increase the increment
       -> M.Map (Int, Int) Int   -- ^ mapping coordinates to values
       -> Int                    -- ^ first value that is larger than the target
coords target current inc step (x, y) d count m
    | current > target = current
    | otherwise =
        let (x', y') = nextCoord (x, y) d
            (step', d') = if step + 1  == inc then (0, nextDir d) else (step + 1, d)
            inc' = if count - 1 == 0 then inc + 1 else inc
            count' = if count - 1 == 0 then 2 * inc' else count - 1

            value = sum . catMaybes $ map (`M.lookup` m) [(x',y'-1), (x'-1, y'-1), (x'+1, y'-1), (x',y'), (x'-1, y'), (x'+1, y'), (x',y'+1), (x'-1, y'+1), (x'+1, y'+1)]

        in coords target value inc' step' (x', y') d' count' (M.insert (x', y') value m)


main = do
    v <- (read . head) <$> getArgs

    let x = coords v 1 1 0 (0, 0) Right 2 $ M.insert (0,0) 1 M.empty

    print x


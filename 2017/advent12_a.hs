module Main where

import           Control.Applicative
import qualified Data.Graph           as G
import           Data.List            (nub)
import           Data.List.Split      (splitOn)
import qualified Data.Tree            as T

parse :: String -> (Int, [Int])
parse l =
    let l':ts = splitOn ", " l
        s:_:t:_ = words l'
    in (read s, map read (t:ts))

makeGraph :: [(Int, [Int])] -> G.Graph
makeGraph cs =
    let edges = nub $ [(a, t) | (a, ts) <- cs, t <- ts] ++ [(t, a) | (a, ts) <- cs, t <- ts]
        bs = map fst cs
        lower = minimum bs
        upper = maximum bs
    in G.buildG (lower, upper) edges


main = do
    input <- lines <$> getContents

    let connections = map parse input
        g = makeGraph connections
        cs = G.components g
        target = head cs

    print target
    print $ length $ T.flatten target
    print $ length cs

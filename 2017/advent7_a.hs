module Main where

import           Control.Applicative
import           Data.List.Split      (splitOn)
import qualified Data.Set             as S

parse :: String
      -> (String, Int, [String])
parse l =
    let (p:v:xs) = splitOn " " l
        v' = read $ init . tail $ v
        xs' = case xs of
            (arrow:discs) -> discs
            _             -> []
    in (p, v' , xs')


insertCarried :: S.Set String -> (String, Int, [String]) -> S.Set String
insertCarried s (_, _, cs) = foldr S.insert s cs

main = do

    inputs <- (map parse . lines) <$> getContents

    let carriers = filter (\(_, _, xs) -> not $ null xs) inputs
        carried = foldl insertCarried S.empty carriers

        bottoms = filter (`S.notMember` carried) $ map (\(c, _, _) -> c) carriers

    putStrLn $ show bottoms

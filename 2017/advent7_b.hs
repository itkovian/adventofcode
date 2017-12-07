module Main where

import           Control.Applicative
import           Data.List            (sort, groupBy)
import           Data.List.Split      (splitOn)
import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Map.Strict      as M

import Debug.Trace

type ProgramMap = M.Map String (Int, S.Set String)

-- | A topological sort that takes ordering of the available leaves into account
topologicalSort :: ProgramMap -> [String]
topologicalSort m = tsort [] (let l = getLeaves m in {-trace ("leaves: "  ++ show l)-} l) m
  where getLeaves :: ProgramMap -> S.Set String
        getLeaves m = S.fromList $ M.keys $ M.filter (S.null . snd) m

        tsort :: [String] -> S.Set String -> ProgramMap -> [String]
        tsort s leaves m | S.null leaves = reverse s
        tsort s leaves m = tsort (l:s) (let l = leaves' in {-trace ("leaves: " ++ show l)-} l) m'
          where l = S.findMin leaves
                m' = removeLeaf m l
                leaves' = getLeaves m'

removeLeaf :: ProgramMap -> String -> ProgramMap
removeLeaf m l = let (m', m'') =  M.partition (S.member l . snd) m
                 in M.delete l $ M.union m'' $ M.map (\(t, xs) -> (t, S.delete l xs)) m'

isLeaf :: ProgramMap -> String -> Bool
isLeaf m p = S.null . snd . fromJust $ M.lookup p m

isLeaf' :: (Int, S.Set String) -> Bool
isLeaf' = S.null . snd


parse :: String -> (String, Int, [String])
parse l =
    let (p:v:xs) = splitOn " " l
        v' = read $ init . tail $ v
        xs' = case xs of
            (arrow:discs) -> discs
            _             -> []
    in (p, v' , xs')


insertPartialTower :: M.Map String (Int, S.Set String) -> (String, Int, [String]) -> M.Map String (Int, S.Set String)
insertPartialTower m (k, w, ds) = M.insert k (w, S.fromList ds) m

same :: [Int] -> Bool
same [] = True
same xs = let s = S.fromList xs in S.size s == 1

reduction :: [Int] -> [String] -> ProgramMap -> Int
reduction ws cs pm =
    let (w, c) = head $ head $ filter (\g -> length g == 1) $ groupBy (\(w1, c1) (w2, c2) -> w1 == w2) $ sort $ zip ws cs
        minw = minimum ws
        maxw = maximum ws
        cw = fst . fromJust $ M.lookup c pm
    in if w == minw
        then cw + maxw - minw
        else cw - maxw + minw



insertWeights :: ProgramMap -> String -> M.Map String Int -> M.Map String Int
insertWeights pm p m =
    let (w, children) = fromJust $ M.lookup p pm
        children' = S.toList children
        accumWeights = mapMaybe (`M.lookup` m) children'
    in case accumWeights of
        [] -> M.insert p w m
        ws -> if same ws
                then M.insert p (w + sum ws) m
                else trace ("Found the culpit with adjusted weight " ++ show (reduction ws children' pm)) $ m


main = do

    inputs <- (map parse . lines) <$> getContents

    let towerMap = foldl insertPartialTower M.empty inputs
        towerMap' = towerMap
        walkorder = topologicalSort towerMap
        weightMap = foldr (insertWeights towerMap') M.empty $ reverse walkorder

    putStrLn $ "Done: " ++ show weightMap

module Main where



import           Control.Applicative
import           Control.Monad
import qualified Data.Map.Strict     as M
import           Data.Maybe
type Register = String

data Op = O (Int -> Int) Register

data Comp = C (Int -> Int -> Bool) Register Int

parse :: String -> (Op, Comp)
parse line =
    let targetReg:action:value:_:sourceReg:cmp:value':_ = words line
        op = case action of
                "inc" -> O (\v -> v + read value) targetReg
                "dec" -> O (\v -> v - read value) targetReg
        comp = case cmp of
                "==" -> C (==) sourceReg (read value')
                "!=" -> C (/=) sourceReg (read value')
                ">"  -> C (>) sourceReg (read value')
                "<"  -> C (<) sourceReg (read value')
                ">=" -> C (>=) sourceReg (read value')
                "<=" -> C (<=) sourceReg (read value')
    in (op, comp)

check :: M.Map Register Int -> Comp -> Bool
check m (C c r v) =
    let current = fromMaybe 0 (M.lookup r m)
    in c current v

process :: (Op, Comp) -> (Int, M.Map Register Int) -> (Int, M.Map Register Int)
process (O op r, comp) (h, m)
    | check m comp =
        let old = fromMaybe 0 (M.lookup r m)
            new = op old
        in (max h new, M.insert r new m)
    | otherwise = (h, m)

main = do

    input <- lines <$> getContents
    let instructions = map parse input
    let (h, m) = foldr process (0, M.empty) $ reverse instructions

    print (h, maximum $ M.elems m)

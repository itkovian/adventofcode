{-# LANGUAGE BangPatterns #-}

module Main where

import Debug.Trace

import Control.Applicative
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as M

type Registers = M.Map Char Int

data Value = Reg Char
           | I Int
           deriving (Eq, Show, Ord)

data Instr = Sound Value
           | Set Char Value
           | Add Char Value
           | Mul Char Value
           | Mod Char Value
           | Recover Value
           | Jgz Char Value
           deriving (Eq, Ord, Show)

mkValue :: String -> Value
mkValue s@(d:_)
    | isAlpha d = Reg d
    | otherwise = I (read s)

parse :: String -> Instr
parse s
    | instr == "snd" = Sound (mkValue $ args !! 0)
    | instr == "set" = Set (head $ args !! 0) (mkValue $ args !! 1)
    | instr == "add" = Add (head $ args !! 0) (mkValue $ args !! 1)
    | instr == "mul" = Mul (head $ args !! 0) (mkValue $ args !! 1)
    | instr == "mod" = Mod (head $ args !! 0) (mkValue $ args !! 1)
    | instr == "rcv" = Recover (mkValue $ args !! 0)
    | instr == "jgz" = Jgz (head $ args !! 0) (mkValue $ args !! 1)
  where instr:args = words s

regValue :: Registers
         -> Char
         -> Int
regValue rs r = fromMaybe 0 (M.lookup r rs)

run :: Registers
    -> M.Map Int Instr
    -> Int         -- ^ PC
    -> Int         -- ^ Last sound played
    -> Int
run rs !mis !pc sound =
    let instr = fromJust $ M.lookup pc mis
    in trace (show instr) $ case instr of
        Sound (I v) -> run rs mis (pc + 1) v
        Sound (Reg r) -> run rs mis (pc + 1) $ regValue rs r
        Set r (I v) -> let rs' = M.insert r v rs in run rs' mis (pc + 1) sound
        Set r (Reg r') -> let v' = regValue rs r'
                          in let rs' = M.insert r v' rs in run rs' mis (pc + 1) sound
        Add r (I v) -> let v' = regValue rs r
                       in let mrs' = M.insert r (v' + v) rs
                          in run mrs' mis (pc + 1) sound
        Add r (Reg r') -> let v' = regValue rs r
                          in let v'' = regValue rs r'
                             in let mrs' = M.insert r (v' + v'') rs
                                in run mrs' mis (pc + 1) sound
        Mul r (I v) -> let v' = regValue rs r
                       in let mrs' = M.insert r (v' * v) rs
                          in run mrs' mis (pc + 1) sound
        Mul r (Reg r') -> let v' = regValue rs r
                          in let v'' = regValue rs r'
                             in let mrs' = M.insert r (v' * v'') rs
                                in run mrs' mis (pc + 1) sound
        Mod r (I v) -> let v' = regValue rs r
                       in let mrs' = M.insert r (v' `mod` v) rs
                          in run mrs' mis (pc + 1) sound
        Mod r (Reg r') -> let v' = regValue rs r
                          in let v'' = regValue rs r'
                             in let mrs' = M.insert r (v' `mod` v'') rs
                                in run mrs' mis (pc + 1) sound
        Recover (I v) -> if v /= 0 then sound else run rs mis (pc + 1) sound
        Recover (Reg r) -> if regValue rs r /= 0 then sound else run rs mis (pc + 1) sound
        Jgz r (I v) -> let pc' = if regValue rs r > 0 then pc + v else pc + 1
                       in run rs mis pc' sound
        Jgz r (Reg r') -> let offset = regValue rs r'
                          in let pc' = if regValue rs r > 0 then pc + offset else pc + 1
                             in run rs mis pc' sound


main :: IO ()
main = do
    instr <- map parse . lines <$> getContents

    let sound = run M.empty (M.fromList $ zip [0..] instr) 0 (-1)

    print sound

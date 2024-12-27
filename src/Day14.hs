module Main where

import qualified PressXToGrids as Grid
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Data.List (partition, groupBy)

dishP :: Parser (Grid.Grid Char)
dishP = block

flush :: Char -> String -> String
flush c = uncurry (++) . partition (==c)

flushAll :: Char -> String -> String
flushAll c = concatMap (flush c) . groupBy cmp
  where
    -- groupBy hack to find all groups without walls
    cmp slow fast = slow /= '#' && fast /= '#'

load :: Char -> String -> [Int]
load c xs = [if x == c then n-i else 0 | (i, x) <- zip [0..] xs]
  where
    n = length xs

-- applies a row-wise operation to each column of a grid by rotating it anticlockwise
mapU :: ([a] -> [b]) -> [[a]] -> [[b]]
mapU f =  Grid.r1 . map f . Grid.r3

sum2D :: Num a => [[a]] -> a
sum2D = sum . map sum

solve1 :: Solver
solve1 = show . sum2D . mapU (load 'O' . flushAll 'O') . mustParse dishP

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2

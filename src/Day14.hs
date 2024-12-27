module Main where

import Data.Map ((!))
import qualified Data.Map as Map
import qualified PressXToGrids as Grid
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Data.List (partition, groupBy, foldl')

dishP :: Parser (Grid.Grid Char)
dishP = block

flush :: [String] -> [String]
flush = map flushRow
  where
    flushRow = concatMap flushGrp . groupBy cmp
    flushGrp = uncurry (++) . partition (=='O')
    cmp slow fast = slow /= '#' && fast /= '#'

load :: [String] -> [[Int]]
load = map rowLoad
  where
    rowLoad xs = [if x == 'O' then n-i else 0 | (i, x) <- zip [0..] xs]
      where
        n = length xs

cycleDish :: [String] -> [String]
cycleDish grid = foldl' (flip ($)) grid ops
  where
    ops = replicate 4 (Grid.r1 . flush)

-- optimized w cycle detection
cycleDish' :: Int -> [String] -> [String]
cycleDish' n grid = foldl' (flip ($)) grid ops
  where
    (i, j) = detectCycle cycleDish grid
    n' = ((n - i) `mod` (j - i)) + i
    ops = replicate n' cycleDish

detectCycle :: Ord a => (a -> a) -> a -> (Int, Int)
detectCycle = go Map.empty
  where
    go acc f x
      | Map.member x acc = (acc ! x, n)
      | otherwise = go (Map.insert x n acc) f (f x)
      where
        n = Map.size acc

sum2D :: Num a => [[a]] -> a
sum2D = sum . map sum

solve1 :: Solver
solve1 = show . sum2D . load . flush . Grid.r3 . mustParse dishP

solve2 :: Solver
solve2 = show . sum2D . load . cycleDish' 1000000000 . Grid.r3 . mustParse dishP



main :: IO ()
main = runCLI solve1 solve2

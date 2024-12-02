module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

card :: Parser (Pair [Int])
card = do
  anyChar `manyTill` char ':'
  wins <- ints
  char '|'
  vals <- ints
  return (wins, vals)

matches :: Pair [Int] -> Int
matches (wins, vals) = length $ filter (`elem` wins) vals

points :: Int -> Int
points 0 = 0
points 1 = 1
points n = 2 * points (n-1)

total :: [Int] -> Int
total xs = f 0 [] xs
  where
    f :: Int -> [Pair Int] -> [Int] -> Int
    f y acc [] = y
    f y acc (x:xs) =
      let
        n = 1 + sum [n | (t, n) <- acc, t > 0]
        acc' = [(t-1, n) | (t, n) <- acc]
      in
        f (y+n) ((x, n):acc') xs

solve1 :: Solver
solve1 = show . sum . (points . matches <$>) . mustParse (linesOf card)

solve2 :: Solver
solve2 = show . total . map matches . mustParse (linesOf card)

main :: IO ()
main = runCLI solve1 solve2

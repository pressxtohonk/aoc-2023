module Main where

import qualified Data.Map as Map
import PressXToBoard ((!))
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)

universeP :: Parser (Board.Board Char)
universeP = Board.fromLists <$> block

findLines :: (a -> Bool) -> Board.Board a -> ([Bool], [Bool])
findLines p board@(Board.Board nrow ncol cells) = (emptyRows, emptyCols)
  where
    rows = [0 .. nrow - 1]
    cols = [0 .. ncol - 1]
    emptyRows = [all p [board ! (r, c) | c <- cols] | r <- rows]
    emptyCols = [all p [board ! (r, c) | r <- rows] | c <- cols]

find :: (a -> Bool) -> Board.Board a -> [Board.Pos]
find p (Board.Board _ _ cells) = Map.keys (Map.filter p cells)

weightedL1 :: [Int] -> [Int] -> Board.Pos -> Board.Pos -> Int
weightedL1 weightR weightC (r1, c1) (r2, c2) = weightedDist weightR r1 r2 + weightedDist weightC c1 c2

weightedDist :: [Int] -> Int -> Int -> Int
weightedDist w a b
  | a == b = 0
  | a >= b = weightedDist w b a
  | otherwise = w !! a + weightedDist w (a + 1) b

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

solve1 :: Solver
solve1 input = show $ sum [dist i j | i <- galaxies, j <- galaxies, i < j]
  where
    universe = mustParse universeP input
    galaxies = find (== '#') universe
    (emptyRows, emptyCols) = findLines (== '.') universe
    rowDists = [if empty then 2 else 1 | empty <- emptyRows]
    colDists = [if empty then 2 else 1 | empty <- emptyCols]
    dist = weightedL1 rowDists colDists

solve2 :: Solver
solve2 input = show $ sum [dist i j | i <- galaxies, j <- galaxies, i < j]
  where
    universe = mustParse universeP input
    galaxies = find (== '#') universe
    (emptyRows, emptyCols) = findLines (== '.') universe
    rowDists = [if empty then 1000000 else 1 | empty <- emptyRows]
    colDists = [if empty then 1000000 else 1 | empty <- emptyCols]
    dist = weightedL1 rowDists colDists

main :: IO ()
main = runCLI solve1 solve2

module Main where

import Control.Monad.RWS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)

gridP :: Parser (Board.Board Char)
gridP = Board.fromLists <$> block

find :: (a -> Bool) -> Board.Board a -> Board.Pos
find p (Board.Board _ _ cells) =
  head . Map.keys . Map.filter p $ cells

type Positions = Set.Set Board.Pos
type Walk = RWS (Board.Board Char) [Positions] Positions

step :: Positions -> Walk Positions
step layer = do
  tell [layer]
  peers <- asks (\b -> concatMap (Board.peers b) layer)
  let nexts = Set.fromList peers
  modify (Set.union nexts)
  return nexts

walk :: Int -> Board.Board Char -> (Positions, Positions, [Positions])
walk n board@(Board.Board nrow ncol cells) = runRWS action walls Set.empty
  where
    start = find (=='S') board
    walls = Board.Board nrow ncol (Map.filter (=='#') cells)
    action = foldl (>>=) (return $ Set.singleton start) (replicate n step)

count :: Int -> Board.Board Char -> Int
count steps board = solution
  where
    nrow = Board.nrow board
    ncol = Board.ncol board

    start = find (=='S') board
    moveStart pos
      = (\b -> Board.fill b start '.') 
      . (\b -> Board.fill b pos 'S')

    size (layer, _, _) = Set.size layer

    -- odd grids share parity with the middle grid after an odd number of moves
    -- even grids share parity with the middle grid after an even number of moves
    --
    -- after 26501365 moves:
    --   the middle grid has odd parity
    --   the furthest grids along NWSE have odd parity
    --   outermost grids alternate between odd and even parity

    -- from center moving outwards
    full0 = size $ walk 130 board
    full1 = size $ walk 131 board

    -- from corners moving diagonally
    nw0 = size $ walk (65-1) (moveStart (nrow-1, ncol-1) board)
    ne0 = size $ walk (65-1) (moveStart (nrow-1, 0) board)
    sw0 = size $ walk (65-1) (moveStart (0, ncol-1) board)
    se0 = size $ walk (65-1) (moveStart (0, 0) board)

    nw1 = size $ walk (65*3) (moveStart (nrow-1, ncol-1) board)
    ne1 = size $ walk (65*3) (moveStart (nrow-1, 0) board)
    sw1 = size $ walk (65*3) (moveStart (0, ncol-1) board)
    se1 = size $ walk (65*3) (moveStart (0, 0) board)

    -- from sides moving orthogonally
    n1 = size $ walk 130 (moveStart (nrow-1, 65) board)
    s1 = size $ walk 130 (moveStart (0, 65) board)
    w1 = size $ walk 130 (moveStart (65, ncol-1) board)
    e1 = size $ walk 130 (moveStart (65, 0) board)

    -- math out the solution
    n = steps `div` nrow
    a = full0 * n^2
    b = full1 * (n-1)^2
    c = (nw0 + ne0 + sw0 + se0) * n
    d = (nw1 + ne1 + sw1 + se1) * (n-1)
    e = n1 + s1 + e1 + w1
    solution = a + b + c + d + e

solve1 :: Solver
solve1 = show . (\(a, s, w) -> Set.size a) . walk 64 . mustParse gridP

solve2 :: Solver
solve2 = show . count 26501365 . mustParse gridP

main :: IO ()
main = runCLI solve1 solve2

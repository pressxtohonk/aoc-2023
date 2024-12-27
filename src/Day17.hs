module Main where

import Control.Monad ((>=>))
import Data.Maybe (mapMaybe, maybeToList, catMaybes, fromJust)
import qualified PressXToAlgos as Algos
import qualified PressXToBoard as Board
import PressXToBoard ((?))
import PressXToParse
import PressXToSolve (Solver, runCLI)

gridP :: Parser (Board.Board Int)
gridP = Board.fromLists . map (map (read . (:[]))) <$> block

type Move = (Board.Pos, Board.Dir, Int)

stepWith :: Board.Board Int -> (Int, Move) -> Maybe (Int, Move)
stepWith board (cost, (pos, dir, n)) = do
  let (p, d) = Board.step (pos, dir)
  c <- (cost +) <$> board ? p
  return (c, (p, d, n+1))

turnLWith :: Board.Board Int -> (Int, Move) -> Maybe (Int, Move)
turnLWith board (cost, (pos, dir, _)) = do
  let (p, d) = Board.turnL (pos, dir)
  return (cost, (p, d, 0))

turnRWith :: Board.Board Int -> (Int, Move) -> Maybe (Int, Move)
turnRWith board (cost, (pos, dir, _)) = do
  let (p, d) = Board.turnR (pos, dir)
  return (cost, (p, d, 0))

shortestPaths :: Board.Board Int -> [[(Int, Move)]]
shortestPaths board@(Board.Board nrow ncol cells) =
  combine
    (searchFrom ((0, 0), Board.R, 0))
    (searchFrom ((0, 0), Board.D, 0))
  where
    combine xs ys
      | minCost xs < minCost ys = xs
      | minCost xs > minCost ys = ys
      | otherwise = xs ++ ys

    searchFrom start = Algos.dijkstra next 0 start done

    next :: Int -> Move -> [(Int, Move)]
    next c move
      = filter (\(_, (_, _, n)) -> n <= 3)
      . catMaybes
      $ [ Just (c, move) >>= step
        , Just (c, move) >>= turnL >>= step
        , Just (c, move) >>= turnR >>= step
        ]

    step = stepWith board
    turnL = turnLWith board
    turnR = turnRWith board

    done :: Move -> Bool
    done  (pos, _, _) = pos == (nrow-1, ncol-1)

shortestPaths' :: Board.Board Int -> [[(Int, Move)]]
shortestPaths' board@(Board.Board nrow ncol cells) =
  combine
    (Algos.dijkstra next costR startR done)
    (Algos.dijkstra next costD startD done)
  where
    combine xs ys
      | minCost xs < minCost ys = xs
      | minCost xs > minCost ys = ys
      | otherwise = xs ++ ys

    startWith dir = fromJust $ step4 (0, ((0, 0), dir, 0))
    (costR, startR) = startWith Board.R
    (costD, startD) = startWith Board.D

    next :: Int -> Move -> [(Int, Move)]
    next c move
      = filter (\(_, (_, _, n)) -> n <= 10)
      . catMaybes
      $ [ Just (c, move) >>= step
        , Just (c, move) >>= turnL >>= step4
        , Just (c, move) >>= turnR >>= step4
        ]

    step = stepWith board
    step4 = step >=> step >=> step >=> step
    turnL = turnLWith board
    turnR = turnRWith board

    done :: Move -> Bool
    done  (pos, _, _) = pos == (nrow-1, ncol-1)

minCost :: [[(Int, Move)]] -> Int
minCost = minimum . map (fst . last)

solve1 :: Solver
solve1 = show . minCost . shortestPaths . mustParse gridP

solve2 :: Solver
solve2 = show . minCost . shortestPaths' . mustParse gridP

main :: IO ()
main = runCLI solve1 solve2

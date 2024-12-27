module Main where

import qualified PressXToAlgos as Algos
import qualified PressXToBoard as Board
import PressXToBoard ((?))
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Data.Maybe (mapMaybe, maybeToList, catMaybes)
import Control.Monad (guard)

gridP :: Parser (Board.Board Int)
gridP = Board.fromLists . map (map (read . (:[]))) <$> block

newtype Move = Move (Board.Pos, Board.Dir, Int) deriving (Eq, Ord)

step (Move(pos, dir, n)) =
  let (p, d) = Board.step (pos, dir)
   in Move(p, d, n+1)

turnL (Move(pos, dir, _)) =
  let (p, d) = Board.step . Board.turnL $ (pos, dir)
   in Move(p, d, 1)

turnR (Move(pos, dir, _)) =
  let (p, d) = Board.step . Board.turnR $ (pos, dir)
   in Move(p, d, 1)

shortestPaths :: Board.Board Int -> [[(Int, Move)]]
shortestPaths board@(Board.Board nrow ncol cells) =
  combine
    (searchFrom (Move ((0, 0), Board.R, 0)))
    (searchFrom (Move ((0, 0), Board.D, 0)))
  where
    combine xs ys
      | minCost xs < minCost ys = xs
      | minCost xs > minCost ys = ys
      | otherwise = xs ++ ys

    searchFrom start = Algos.dijkstra next 0 start done

    next :: Int -> Move -> [(Int, Move)]
    next c move =
      catMaybes
        [ withCost step (c, move)
        , withCost turnL (c, move)
        , withCost turnR (c, move)
        ]

    withCost f (cost, move) = do
      let Move (p, d, n) = f move
      guard (n <= 3)
      c <- board ? p
      return (cost+c, Move (p, d, n))

    done :: Move -> Bool
    done (Move (pos, _, _)) = pos == (nrow-1, ncol-1)

minCost :: [[(Int, Move)]] -> Int
minCost = minimum . map (fst . last)

solve1 :: Solver
solve1 = show . minCost . shortestPaths . mustParse gridP

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2

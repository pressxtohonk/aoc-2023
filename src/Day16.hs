module Main where

import Control.Monad ((=<<))
import Control.Monad.State
import qualified Data.Map as Map
import PressXToBoard ((?))
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)

gridP :: Parser (Board.Board Char)
gridP = Board.fromLists <$> block

type Seen = Map.Map Board.Pos [Board.Move]
type SeenState = State Seen

-- step through the grid till no steps are avaolable
walk :: Board.Board Char -> Board.Move -> SeenState [Board.Move]
walk board move@(pos, _)
  | Board.hasCell board pos = do
      moves <- step board move
      concat <$> mapM (walk board) moves
  | otherwise = return []

step :: Board.Board Char -> Board.Move -> SeenState [Board.Move]
step board move@(pos, dir) = result
  where
    result :: SeenState [Board.Move]
    result = do
      seen <- get
      modify (Map.insertWith (++) pos [move])
      case Map.lookup pos seen of
        Just moves | move `elem` moves -> return []
        _ -> next

    next :: SeenState [Board.Move]
    next = case board ? pos of
      Just '.' -> continue
      Just '-' -> case dir of
        Board.U -> split
        Board.D -> split
        Board.L -> continue
        Board.R -> continue
      Just '|' -> case dir of
        Board.U -> continue
        Board.D -> continue
        Board.L -> split
        Board.R -> split
      Just '/' -> case dir of
        Board.U -> turnR
        Board.D -> turnR
        Board.L -> turnL
        Board.R -> turnL
      Just '\\' -> case dir of
        Board.U -> turnL
        Board.D -> turnL
        Board.L -> turnR
        Board.R -> turnR
      Nothing -> return []

    stepL = Board.step . Board.turnL
    stepR = Board.step . Board.turnR
    turnL = return [stepL move]
    turnR = return [stepR move]
    split = return [stepL move, stepR move]
    continue = return [Board.step move]

footprint :: Board.Board Char -> Board.Move -> Seen
footprint board start = execState (walk board start) Map.empty

solve1 :: Solver
solve1 = show . Map.size . (`footprint` start) . mustParse gridP
  where
    start = ((0, 0), Board.R)

solve2 :: Solver
solve2 input = show $ maximum (Map.size . footprint board <$> starts)
  where
    board = mustParse gridP input
    nrow = Board.nrow board
    ncol = Board.ncol board
    rows = [0..nrow-1]
    cols = [0..ncol-1]
    starts = concat 
      [ [((0, c), Board.D) | c <- cols]
      , [((r, 0), Board.R) | r <- rows]
      , [((nrow-1, c), Board.U) | c <- cols]
      , [((r, ncol-1), Board.L) | r <- rows]
      ]

main :: IO ()
main = runCLI solve1 solve2

module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

draw :: Parser (Trio Int)
draw = do
  n <- int
  space
  c <- many alphaNum
  case c of
    "red"   -> return (n, 0, 0)
    "green" -> return (0, n, 0)
    "blue"  -> return (0, 0, n)
    _ -> error $ "not a colour: " ++ c

game :: Parser (Int, [[Trio Int]])
game = do
  string "Game "
  gameId <- int
  string ": "
  draws <- (draw `sepBy` string ", ") `sepBy` string "; "
  return (gameId, draws)

combine :: [Trio Int] -> Trio Int
combine = foldl (\(a, b, c) (a', b', c') -> (max a a', max b b', max c c')) (0, 0, 0)

valid :: Trio Int -> Bool
valid (r, g, b) = (r <= 12) && (g <= 13) && (b <= 14)

solve1 :: Solver
solve1 input = show $ sum [ id | (id, draws) <- games, possible draws]
  where
    games = mustParse (linesOf game) input
    possible = valid . combine . concat

solve2 :: Solver
solve2 input = show $ sum [ power draws | (id, draws) <- games ]
  where
    games = mustParse (linesOf game) input
    power = (\(r,g,b) -> r*g*b) . combine . concat

main :: IO ()
main = runCLI solve1 solve2

module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Data.Char (isDigit)
import Text.Parsec

type Part = (Trio Int, Int)
type Gear = (Pair Int, Char)

partP :: Parser Part
partP = do
  many $ satisfy (not . isDigit)
  lb  <- getPosition 
  val <- int
  ub  <- getPosition
  many $ satisfy (not . isDigit)
  let
    r = sourceLine lb
    i = sourceColumn lb
    j = sourceColumn ub
  return ((r, i, j-1), val)

gearP :: Parser Gear
gearP = do
  many $ choice [digit, char '.', endOfLine]
  pos <- getPosition
  val <- anyChar
  many $ choice [digit, char '.', endOfLine]
  let
    r = sourceLine pos
    c = sourceColumn pos
  return ((r, c), val)

adj :: Part -> Gear -> Bool
adj ((pr, pi, pj), _) ((gr, gc), _) =
  (abs (pr - gr) <= 1) && (pi-1 <= gc) && (gc <= pj+1)

ratio :: Gear -> [Part] -> Int
ratio gear parts =
  case [ snd p | p <- parts, adj p gear ] of
    [a,b] -> a * b
    _ -> 0

solve1 :: Solver
solve1 input = show $ sum [ snd p | p <- parts, any (adj p) gears ]
  where
    parts = mustParse (many partP) input
    gears = mustParse (many gearP) input

solve2 :: Solver
solve2 input = show $ sum [ ratio g parts | g <- gears ]
  where
    parts = mustParse (many partP) input
    gears = mustParse (many gearP) input

main :: IO ()
main = runCLI solve1 solve2

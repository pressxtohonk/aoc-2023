module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

type Race = Pair Int

doc :: Parser [Race]
doc = do
  ts <- anyChar `manyTill` char ':' >> ints
  ds <- anyChar `manyTill` char ':' >> ints
  return $ zip ts ds

doc' :: Parser Race
doc' = do
  ts <- anyChar `manyTill` char ':' >> list (many1 digit)
  ds <- anyChar `manyTill` char ':' >> list (many1 digit)
  return (read (concat ts), read (concat ds))

margin :: Race -> Int
margin (t, d) = length $ filter (>d) [x * (t-x) | x <- [0..t]]

solve1 :: Solver
solve1 = show . product . map margin . mustParse doc

solve2 :: Solver
solve2 = show . margin . mustParse doc'

main :: IO ()
main = runCLI solve1 solve2

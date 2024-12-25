module Main where

import Data.List (intercalate, repeat, replicate)
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, choice, manyTill, sepBy, space, try)

data Condition = O | D | U deriving (Eq, Show)

conditionP :: Parser Condition
conditionP =
  choice . map try $
    [ O <$ char '.',
      D <$ char '#',
      U <$ char '?'
    ]

springP :: Parser ([Condition], [Int])
springP = (,) <$> conditionP `manyTill` space <*> int `sepBy` char ','

numMatches :: [Condition] -> [Int] -> Int
numMatches [] [] = 1
numMatches [] ns = 0
numMatches (O : t) [] = numMatches t []
numMatches (D : _) [] = 0
numMatches (U : t) [] = numMatches t []
numMatches xs@(h : t) (n : ns) = accept + reject
  where
    accept = if h /= O && matches xs n then numMatches (drop (n + 1) xs) ns else 0
    reject = if h /= D then numMatches t (n : ns) else 0

matches :: [Condition] -> Int -> Bool
matches xs n
  | length xs < n = False
  | length xs > n = O `notElem` take n xs && xs !! n /= D
  | otherwise = O `notElem` xs

solve1 :: Solver
solve1 = show . sum . map (uncurry numMatches) . mustParse (linesOf springP)

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2

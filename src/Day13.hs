module Main where

import Control.Applicative
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Data.List (transpose)
import Data.Maybe (fromJust, listToMaybe)

patternsP :: Parser [[[Char]]]
patternsP = linesOf block

reflection :: (Eq a) => [a] -> [Int]
reflection (x:xs) = go [x] xs
  where
    go ls rs@(x:xs)
      | ls `reflect` rs = length ls : go (x:ls) xs
      | otherwise = go (x:ls) xs
    go _ [] = []
    reflect l r = and (zipWith (==) l r)

notes :: Eq a => [[a]] -> Int
notes pat = fromJust ((100 *) <$> row <|> col)
  where
    col = listToMaybe . reflection . transpose $ pat
    row = listToMaybe . reflection $ pat

smudged :: (Eq a) => [[a]] -> [Int]
smudged (x:xs) = go [x] xs
  where
    go ls rs@(x:xs)
      | diffs ls rs == 1 = length ls : go (x:ls) xs
      | otherwise = go (x:ls) xs
    go _ [] = []
    diffs ls rs = sum (zipWith diff ls rs)
    diff l r = length . filter id $ zipWith (/=) l r

notes' :: Eq a => [[a]] -> Int
notes' pat = fromJust ((100 *) <$> row <|> col)
  where
    col = listToMaybe . smudged . transpose $ pat
    row = listToMaybe . smudged $ pat

solve1 :: Solver
solve1 = show . sum . map notes . mustParse patternsP

solve2 :: Solver
solve2 = show . sum . map notes' . mustParse patternsP

main :: IO ()
main = runCLI solve1 solve2

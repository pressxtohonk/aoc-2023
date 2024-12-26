module Main where

import Data.List (intercalate, repeat, replicate)
import Data.MemoTrie (memo2)
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, manyTill, sepBy, space, try, oneOf)

conditionP :: Parser Char
conditionP = oneOf ".#?"

springP :: Parser (String, [Int])
springP = (,) <$> conditionP `manyTill` space <*> int `sepBy` char ','

numMatches :: String -> [Int] -> Int
numMatches = memo2 go
  where
    go [] [] = 1
    go [] ns = 0
    go ('.' : t) [] = numMatches t []
    go ('#' : _) [] = 0
    go ('?' : t) [] = numMatches t []
    go xs@(h : t) (n : ns) = accept + reject
      where
        accept = if h /= '.' && matches xs n then numMatches (drop (n + 1) xs) ns else 0
        reject = if h /= '#' then numMatches t (n : ns) else 0

matches :: String -> Int -> Bool
matches xs n
  | length xs < n = False
  | length xs > n = '.' `notElem` take n xs && xs !! n /= '#'
  | otherwise = '.' `notElem` xs

unfold :: Int -> (String, [Int]) -> (String, [Int])
unfold n (xs, ns) = (fx xs, fn ns)
  where
    fx = intercalate "?" . replicate n
    fn = concat . replicate n

solve1 :: Solver
solve1 = show . sum . map (uncurry numMatches) . mustParse (linesOf springP)

solve2 :: Solver
solve2 = show. sum . map (uncurry numMatches . unfold 5) . mustParse (linesOf springP)

main :: IO ()
main = runCLI solve1 solve2

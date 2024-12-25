module Main where

import Data.List (cycle, isSuffixOf, scanl)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (alphaNum, choice, many, manyTill, newline, string, try)

data Dir = L | R deriving (Show)

instructionP :: Parser Dir
instructionP =
  choice . map try $
    [ L <$ string "L",
      R <$ string "R"
    ]

branchP :: Parser (String, String, String)
branchP = do
  label <- alphaNum `manyTill` string " = ("
  nodeL <- alphaNum `manyTill` string ", "
  nodeR <- alphaNum `manyTill` string ")\n"
  return (label, nodeL, nodeR)

mapP :: Parser ([Dir], [Trio String])
mapP = do
  instructions <- instructionP `manyTill` eol
  newline
  branches <- many branchP
  return (instructions, branches)

toTree :: [Trio String] -> Map String (Pair String)
toTree = Map.fromList . map (\(a, l, r) -> (a, (l, r)))

walk :: Map String (Pair String) -> [Dir] -> String -> [String]
walk nodes dirs node = scanl step node (cycle dirs)
  where
    step :: String -> Dir -> String
    step x L = fst (nodes ! x)
    step x R = snd (nodes ! x)

path :: String -> String -> ([Dir], [Trio String]) -> [String]
path source target (dirs, branches) = takeWhile (/= target) $ walk (toTree branches) dirs source

paths :: (String -> Bool) -> (String -> Bool) -> ([Dir], [Trio String]) -> [[String]]
paths source target (dirs, branches) = takeWhile (not . target) . walk tree dirs <$> sources
  where
    tree = toTree branches
    sources = filter source (Map.keys tree)

solve1 :: Solver
solve1 = show . length . path "AAA" "ZZZ" . mustParse mapP

-- LCM works as all paths cycle with exactly one (..A) node and one (..Z) node
solve2 :: Solver
solve2 = show . foldr (lcm . length) 1 . paths ("A" `isSuffixOf`) ("Z" `isSuffixOf`) . mustParse mapP

main :: IO ()
main = runCLI solve1 solve2

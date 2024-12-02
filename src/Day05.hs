module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec
import Data.List (intercalate)

-- reads a block for a list of (src, len) ranges
seedP :: Parser [Pair Int]
seedP = do
  anyChar `manyTill` lookAhead (string ": ")
  string ": "
  pair int `sepBy1` char ' '

-- reads a block for a list of (dst, src, len) maps
mapsP :: Parser [Trio Int]
mapsP = do
  line >> eol -- ignore header
  linesOf (trio int)

almanacP :: Parser ([Pair Int], [[Trio Int]])
almanacP = do
  blks <- blocks
  case blks of
    (seedBlk:mapBlks) ->
      let
        seeds = mustParse seedP . unlines $ seedBlk
        maps = mustParse mapsP . unlines <$> mapBlks
      in
        return ((\(x, dx) -> (x, x+dx-1)) <$> seeds, maps)
    _ -> error "error parsing almanac"

-- Partitions an interval to 2 sets of intervals:
-- - a mapped set, and an umapped set
part :: Trio Int -> Pair Int -> Pair [Pair Int]
part (dst, src, len) (lb, ub) =
  let
    lb'  = max lb src
    ub'  = min ub (src+len-1)
    add xs (a, b) = if a <= b then (a, b):xs else xs
    map xs (a, b) = (a-src+dst, b-src+dst):xs
  in
    if lb' > ub'
      then ( [], [(lb, ub)] )
      else ( [] `map` (lb', ub')
           , [] `add` (lb, lb'-1) `add` (ub'+1, ub)
           )

runMap :: [Trio Int] -> [Pair Int] -> [Pair Int]
runMap trios pairs = ys ++ xs
  where
    (ys, xs) = foldr combine ([], pairs) trios
    combine m (ys, []) = (ys, []) 
    combine m (ys, xs) =
      let (new, old) = unzip (part m <$> xs)
       in (concat (ys:new), concat old)

runMaps :: [[Trio Int]] -> [Pair Int] -> [Pair Int]
runMaps maps pairs = foldr runMap pairs (reverse maps)

solve1 :: Solver
solve1 input = 
  intercalate "\n"
    [ show $ pairs
    , show $ runMaps (take 1 trios) pairs
    , show $ runMaps (take 2 trios) pairs
    , show $ runMaps (take 3 trios) pairs
    , show $ runMaps (take 4 trios) pairs
    , show $ runMaps (take 5 trios) pairs
    , show $ runMaps (take 6 trios) pairs
    , show $ runMaps (take 7 trios) pairs
    ]
  where
    (pairs, trios) = mustParse almanacP input

solve2 :: Solver
solve2 input = show $ minimum [ lb | (lb, _) <- runMaps trios pairs ]
  where
    (pairs, trios) = mustParse almanacP input

main :: IO ()
main = runCLI solve1 solve2

module Main where

import qualified Data.IntMap.Strict as Map
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (anyChar, lookAhead, many, sepEndBy, oneOf)
import Data.Char (isSpace, ord)
import Data.Foldable (Foldable(foldl'))

data Op = Get String | Put String String deriving (Eq, Show)

strip :: String -> String
strip = reverse . f . reverse . f
  where f = dropWhile isSpace

split :: Char -> String -> [String]
split _ [] = []
split c xs = case dropWhile (== c) xs of
  [] -> []
  xs ->
    let (z, zs) = break (== c) xs
     in z : split c zs

hash :: String -> Int
hash = foldl (\z x -> ((z + ord x) * 17) `mod` 256) 0

op :: String -> Op
op txt = case (split '=' txt, split '-' txt) of
  ([k, v], _) -> Put k v
  (_, [k]) -> Get k
  _ -> error $ "unknown op: " ++ show txt

type HashMap = Map.IntMap [(String, String)]

run :: Op -> HashMap -> HashMap
run op = case op of
  Get key -> Map.adjust (get key) (hash key)
  Put key val -> Map.adjust (put key val) (hash key)

get :: String -> [(String, String)] -> [(String, String)]
get key = filter (\(k, _) -> k /= key)

put :: String -> String -> [(String, String)] -> [(String, String)]
put key val xs
  | any (\(k, v) -> k == key) xs =
      [if k == key then (key, val) else (k, v) | (k, v) <- xs]
  | otherwise = (key, val):xs

focalLengths :: HashMap -> Int
focalLengths = Map.foldlWithKey' combine 0
  where
    combine :: Int -> Int -> [(String, String)] -> Int
    combine acc hashKey = foldl' combine' acc . zip [0..] . reverse
      where
        combine' acc' (i, (_, x)) = acc' + (hashKey + 1) * (i + 1) * read x

solve1 :: Solver
solve1 = show . sum . map hash . split ',' . strip

solve2 :: Solver
solve2 = show . focalLengths . foldr (run . op) hashmap . reverse . split ',' . strip
  where
    hashmap = Map.fromList $ zip [0..255] (repeat [])

main :: IO ()
main = runCLI solve1 solve2

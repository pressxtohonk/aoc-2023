module Main where

import Control.Monad (foldM)
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, manyTill, sepBy, space, try, oneOf)

type Coord = Trio Int
type Brick = Pair Coord

type View = Map.Map (Pair Int) Cell
data Cell = Cell 
  { x :: Int
  , y :: Int
  , height :: Int
  , brick :: Brick
  } deriving (Eq, Show)

coordP :: Parser Coord
coordP = (,,) <$> int <* char ',' <*> int <* char ',' <*> int

brickP :: Parser Brick
brickP = (,) <$> coordP <* char '~' <*> coordP

stackP :: Parser [Brick]
stackP = linesOf brickP

zMin :: Brick -> Int
zMin ((_, _, z1), (_, _, z2)) = min z1 z2

inFallOrder :: [Brick] -> [Brick]
inFallOrder = sortBy (\a b -> compare (zMin a) (zMin b))

viewFromAbove :: Brick -> View
viewFromAbove brick@((x1, y1, z1), (x2, y2, z2))
  | x1 /= x2 = Map.fromList [((x, y1), (Cell x y1 1 brick)) | x <- [x1..x2]]
  | y1 /= y2 = Map.fromList [((x1, y), (Cell x1 y 1 brick)) | y <- [y1..y2]]
  | otherwise = Map.singleton (x1, y1) (Cell x1 y1 (z2-z1+1) brick)

heightAt :: View -> Pair Int -> Maybe Int
heightAt view xy = height <$> Map.lookup xy view

intersection :: View -> View -> (View, Maybe Brick)
intersection above below =
  let
    -- identify where views touch
    overlaps = Map.elems (Map.intersection below above)
    offset = foldr max 0 (height <$> overlaps)

    -- identify bricks that solely support the current view
    bricks = [brick cell | cell <- overlaps, height cell == offset]
    unsafe = case nub bricks of
      [brick] -> Just brick
      _ -> Nothing

    -- stack current view on top of previous view
    merged = Map.union ((\c -> c { height = height c + offset }) <$> above) below

  in (merged, unsafe)

type Tracked = State (Set.Set Brick)

merge :: View -> View -> Tracked View
merge prev curr = do
  let (merged, unsafe) = intersection curr prev
  modify (\s -> foldr Set.insert s unsafe)
  return merged

simulate :: [View] -> (View, Set.Set Brick)
simulate [] = error "simulate expected non-empty input"
simulate (x:xs) =
  let
    initialBricks = Set.fromList (brick <$> (x:xs >>= Map.elems))
    (finalView, unsafeBricks) = runState (foldM merge x xs) Set.empty
  in
    (finalView, Set.difference initialBricks unsafeBricks)

solve1 :: Solver
solve1 = show . Set.size . (\(view, bricks) -> bricks) . simulate . map viewFromAbove . inFallOrder . mustParse stackP

solve2 :: Solver
solve2 = show

debug :: Solver
debug = show .inFallOrder . mustParse stackP

main :: IO ()
main = runCLI solve1 solve2

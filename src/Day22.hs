module Main where

import Control.Monad (foldM, guard)
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

data Graph a = Graph
  { nodes :: [a]
  , edges :: [Pair a]
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

merge :: View -> View ->  State [Pair Brick] View
merge below above = do
  let
    -- identify where views touch
    overlaps = Map.elems (Map.intersection below above)
    offset = foldr max 0 (height <$> overlaps)

    -- identify bricks that support other bricks
    currBricks = nub (brick <$> Map.elems above)
    contacts = [(brick cell, currBrick) | currBrick <- currBricks
                                        , cell <- overlaps
                                        , height cell == offset]

    -- stack current view on top of previous view
    merged = Map.union ((\c -> c { height = height c + offset }) <$> above) below

  modify (++ contacts)
  return merged

simulate :: [View] -> (View, [Pair Brick])
simulate [] = error "simulate expected non-empty input"
simulate (x:xs) = runState (foldM merge x xs) []

toGraph :: [Brick] -> Graph Brick
toGraph bricks =
  let
    nodes = bricks
    edges = snd . simulate . map viewFromAbove . inFallOrder $ bricks
  in
    Graph nodes edges

findSafeNodes :: Graph Brick -> [Brick]
findSafeNodes graph@(Graph nodes edges) =
  let
    supports = Map.fromListWith Set.union [(hi, Set.singleton lo) | (lo, hi) <- edges]
    critical = Map.filter (\lows -> Set.size lows == 1) supports
  in
    Set.toList (Set.difference (Set.fromList nodes) (Set.unions critical))

countFalls :: Graph Brick -> Map.Map Brick (Set.Set Brick)
countFalls (Graph nodes edges) =
  let
    preds = Map.fromListWith Set.union [(hi, Set.singleton lo) | (lo, hi) <- edges]
    succs = Map.fromListWith Set.union [(lo, Set.singleton hi) | (lo, hi) <- edges]

    -- find branches where nodes are only supported by roots
    branches :: Set.Set Brick -> Set.Set Brick -> Set.Set Brick
    branches prevs roots = Set.fromList $ do
      root <- Set.toList roots
      node <- Set.toList $ Map.findWithDefault Set.empty root succs
      let
        deps = Map.findWithDefault Set.empty node preds
        seen = Set.union prevs roots
      guard (Set.isSubsetOf deps seen)
      return node

    -- build the maximal subtree completely supported by roots
    subtree :: Set.Set Brick -> Set.Set Brick -> Set.Set Brick
    subtree prevs roots 
      | Set.null roots = Set.empty
      | otherwise = 
        let
          prevs' = Set.union prevs roots
          roots' = branches prevs roots
        in Set.union roots' (subtree prevs' roots')

    -- update a map with tree sizes for a given root node
    update :: Brick -> Map.Map Brick (Set.Set Brick) -> Map.Map Brick (Set.Set Brick)
    update root = Map.insert root (subtree Set.empty (Set.singleton root))

  in foldr update Map.empty nodes

solve1 :: Solver
solve1 = show . length . findSafeNodes . toGraph . mustParse stackP

solve2 :: Solver
solve2 = show . sum . (Set.size <$>) . countFalls . toGraph . mustParse stackP

main :: IO ()
main = runCLI solve1 solve2

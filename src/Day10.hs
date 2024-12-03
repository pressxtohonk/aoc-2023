module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.Maybe (catMaybes, maybeToList)
import Data.List (transpose)

data Pipe
  = Ground
  | Start
  | NS
  | EW
  | NE
  | NW
  | SW
  | SE
  deriving (Show, Eq)

pipe :: Parser (SourcePos, Pipe)
pipe = do
  pos <- getPosition
  p <- choice
    [ Ground <$ char '.'
    , Start <$ char 'S'
    , NS <$ char '|'
    , EW <$ char '-'
    , NE <$ char 'L'
    , NW <$ char 'J'
    , SW <$ char '7'
    , SE <$ char 'F'
    ]
  return (pos, p)

grid :: Parser [(SourcePos, Pipe)]
grid = do
  pipes <- concat <$> linesOf (many1 pipe)
  return [ (pos, p) | (pos, p) <- pipes, p /= Ground ]

type Nodes = Map SourcePos Pipe

walkN :: Nodes -> (SourcePos, Pipe) -> [(SourcePos, Pipe)]
walkN nodes (pos, p) = (pos, p) : do
  guard $ p `elem` [Start, NS, NE, NW]
  let pos' = pos `setSourceLine` (sourceLine pos - 1)
  p' <- maybeToList (Map.lookup pos' nodes)
  case p' of
    NS -> walkN nodes (pos', p')
    SW -> walkW nodes (pos', p')
    SE -> walkE nodes (pos', p')
    _ -> []

walkS :: Nodes -> (SourcePos, Pipe) -> [(SourcePos, Pipe)]
walkS nodes (pos, p) = (pos, p) : do
  guard $ p `elem` [Start, NS, SE, SW]
  let pos' = pos `setSourceLine` (sourceLine pos + 1)
  p' <- maybeToList (Map.lookup pos' nodes)
  case p' of
    NS -> walkS nodes (pos', p')
    NW -> walkW nodes (pos', p')
    NE -> walkE nodes (pos', p')
    _ -> []

walkE :: Nodes -> (SourcePos, Pipe) -> [(SourcePos, Pipe)]
walkE nodes (pos, p) = (pos, p) : do
  guard $ p `elem` [Start, EW, NE, SE]
  let pos' = pos `setSourceColumn` (sourceColumn pos + 1)
  p' <- maybeToList (Map.lookup pos' nodes)
  case p' of
    EW -> walkE nodes (pos', p')
    NW -> walkN nodes (pos', p')
    SW -> walkS nodes (pos', p')
    _ -> []


walkW :: Nodes -> (SourcePos, Pipe) -> [(SourcePos, Pipe)]
walkW nodes (pos, p) = (pos, p) : do
  guard $ p `elem` [Start, EW, NW, SW]
  let pos' = pos `setSourceColumn` (sourceColumn pos - 1)
  p' <- maybeToList (Map.lookup pos' nodes)
  case p' of
    EW -> walkW nodes (pos', p')
    NE -> walkN nodes (pos', p')
    SE -> walkS nodes (pos', p')
    _ -> []

distinct :: Eq a => [a] -> Bool
distinct (x:xs) = (x `notElem` xs) && distinct xs
distinct _ = True

solve1 :: Solver
solve1 input = show $ 1 + length (takeWhile distinct (tail steps))
  where
    pipes = mustParse grid input
    nodes = Map.fromList pipes
    start = head [ (pos, Start) | (pos, Start) <- pipes ]
    steps = transpose
      [ walkN nodes start
      , walkS nodes start
      , walkE nodes start
      , walkW nodes start
      ]

solve2 :: Solver
solve2 input = show steps
  where
    pipes = mustParse grid input
    nodes = Map.fromList pipes
    start = head [ (pos, Start) | (pos, Start) <- pipes ]
    steps = transpose
      [ walkN nodes start
      , walkS nodes start
      , walkE nodes start
      , walkW nodes start
      ]

main :: IO ()
main = runCLI solve1 solve2

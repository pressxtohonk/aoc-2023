module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec
import Data.List (group, sort, sortBy, partition)
import Data.Function (on)

data Card = Joker | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA
  deriving (Show, Eq, Ord)

data Hand
  = HighCard [Card]
  | OnePair [Card]
  | TwoPair [Card]
  | ThreeOfAKind [Card]
  | FullHouse [Card]
  | FourOfAKind [Card]
  | FiveOfAKind [Card]
  deriving (Show, Eq, Ord)

card :: Parser Card
card = choice
  [ C2 <$ char '2'
  , C3 <$ char '3'
  , C4 <$ char '4'
  , C5 <$ char '5'
  , C6 <$ char '6'
  , C7 <$ char '7'
  , C8 <$ char '8'
  , C9 <$ char '9'
  , CT <$ char 'T'
  , CJ <$ char 'J'
  , CQ <$ char 'Q'
  , CK <$ char 'K'
  , CA <$ char 'A'
  ]

card' :: Parser Card
card' = choice [Joker <$ char 'J', card]

organize :: [Card] -> [[Card]]
organize = sortBy (compare `on` (negate . length)) . group . sort

classify :: [[Card]] -> [Card] -> Hand
classify groups = 
  case groups of
    [[_,_,_,_,_]]             -> FiveOfAKind
    [[_,_,_,_], [_]]          -> FourOfAKind
    [[_,_,_], [_,_]]          -> FullHouse
    [[_,_,_], [_], [_]]       -> ThreeOfAKind
    [[_,_], [_,_], [_]]       -> TwoPair
    [[_,_], [_], [_], [_]]    -> OnePair
    [[_], [_], [_], [_], [_]] -> HighCard
    _ -> error $ "invalid group: " ++ show groups

hand :: Parser Hand
hand = do
  cards <- count 5 card
  let
    groups = organize cards
  return $ classify groups cards

hand' :: Parser Hand
hand' = do
  cards <- count 5 card'
  let
    -- remove jokers before organizing hand
    (jokers, rest) = partition (==Joker) cards
    groups = case organize rest of
      []    -> [jokers]      -- return jokers as the only group
      (h:t) -> (jokers++h):t -- return jokers to the highest value group
  return $ classify groups cards

keyVal :: Parser k -> Parser v -> Parser (k, v)
keyVal key val = (,) <$> key <* gap <*> val

score :: [(Hand, Int)] -> Int
score bids = f 0 1 (sort bids)
  where
    f acc rank [] = acc
    f acc rank ((_, bid):xs) = f (acc+rank*bid) (rank+1) xs

solve1 :: Solver
solve1 = show . score . mustParse (linesOf (keyVal hand int))

solve2 :: Solver
solve2 = show . score . mustParse (linesOf (keyVal hand' int))

main :: IO ()
main = runCLI solve1 solve2

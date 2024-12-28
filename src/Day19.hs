module Main where

import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (newline, string, manyTill, sepBy, letter, choice, try, many)

type Part = (Int, Int, Int, Int)

data Cat = X | M | A | S deriving (Eq, Ord, Show)

data Rule
  = IfGT Cat Int String
  | IfLT Cat Int String
  | Goto String
  deriving (Eq, Ord, Show)

type Workflows = Map.Map String [Rule]

catP :: Parser Cat
catP =
  choice . map try $
    [ X <$ string "x"
    , M <$ string "m"
    , A <$ string "a"
    , S <$ string "s"
    ]

ruleP :: Parser Rule
ruleP =
  choice . map try $
    [ IfGT <$> catP <* string ">" <*> int <* string ":" <*> many letter
    , IfLT <$> catP <* string "<" <*> int <* string ":" <*> many letter
    , Goto <$> many letter
    ]

workflowP :: Parser (String, [Rule])
workflowP = do
  label <- letter `manyTill` string "{"
  rules <- ruleP `sepBy` string ","
  string "}"
  return (label, rules)

partP :: Parser Part
partP = do
  string "{x="
  x <- int
  string ",m="
  m <- int
  string ""
  string ",a="
  a <- int
  string ",s="
  s <- int
  string "}"
  return (x, m, a, s)

inputP :: Parser (Workflows, [Part])
inputP = do
  workflows <- Map.fromList <$> linesOf workflowP
  newline
  parts <- linesOf partP
  return (workflows, parts)

nextLabel :: Part -> Rule -> Maybe String
nextLabel (x, m, a, s) rule = case rule of
  IfGT cat val label -> if rating cat > val then Just label else Nothing
  IfLT cat val label -> if rating cat < val then Just label else Nothing
  Goto label -> Just label
  where
    rating X = x
    rating M = m
    rating A = a
    rating S = s

run :: Workflows -> Part -> [String]
run workflows part = go [] "in"
  where
    go :: [String] -> String -> [String]
    go acc "A" = "A":acc
    go acc "R" = "R":acc
    go acc label = case Map.lookup label workflows of
      Just rules -> go (label:acc) (head $ mapMaybe (nextLabel part) rules)
      _ -> acc

filterAccepted :: Workflows -> [Part] -> [Part]
filterAccepted workflows = filter $ (["A"] `isPrefixOf`) . run workflows

rating :: Part -> Int
rating (x, m, a, s) = x + m + a + s

countAccepted :: Workflows -> [Part] -> Int
countAccepted workflows _ = go "in" (1, 1, 1, 1) (4000, 4000, 4000, 4000)
  where
    go :: String -> Part -> Part -> Int
    go label lb@(x1, m1, a1, s1) ub@(x2, m2, a2, s2)
      | x1 > x2 || m1 > m2 || a1 > a2 || s1 > s2 = 0
      | label == "A" = numBetween lb ub
      | label == "R" = 0
      | otherwise = case Map.lookup label workflows of
          Just rules -> go' lb ub rules
          _ -> 0

    -- This sucks but,,,
    go' :: Part -> Part -> [Rule] -> Int
    go' lb@(x1, m1, a1, s1) ub@(x2, m2, a2, s2) (rule:rest) = case rule of
      IfGT X val next -> go next (val+1, m1, a1, s1) ub + go' lb (val, m2, a2, s2) rest
      IfGT M val next -> go next (x1, val+1, a1, s1) ub + go' lb (x2, val, a2, s2) rest
      IfGT A val next -> go next (x1, m1, val+1, s1) ub + go' lb (x2, m2, val, s2) rest
      IfGT S val next -> go next (x1, m1, a1, val+1) ub + go' lb (x2, m2, a2, val) rest
      IfLT X val next -> go next lb (val-1, m2, a2, s2) + go' (val, m1, a1, s1) ub rest
      IfLT M val next -> go next lb (x2, val-1, a2, s2) + go' (x1, val, a1, s1) ub rest
      IfLT A val next -> go next lb (x2, m2, val-1, s2) + go' (x1, m1, val, s1) ub rest
      IfLT S val next -> go next lb (x2, m2, a2, val-1) + go' (x1, m1, a1, val) ub rest
      Goto next -> go next lb ub

numBetween :: Part -> Part -> Int
numBetween (x1, m1, a1, s1) (x2, m2, a2, s2) = dx * dm * da * ds
  where
    dx = max 0 (x2 - x1 + 1)
    dm = max 0 (m2 - m1 + 1)
    da = max 0 (a2 - a1 + 1)
    ds = max 0 (s2 - s1 + 1)

solve1 :: Solver
solve1 = show . sum . map rating . uncurry filterAccepted . mustParse inputP

solve2 :: Solver
solve2 = show . uncurry countAccepted . mustParse inputP

main :: IO ()
main = runCLI solve1 solve2

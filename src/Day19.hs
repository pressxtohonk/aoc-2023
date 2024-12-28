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

solve1 :: Solver
solve1 = show . sum . map rating . uncurry filterAccepted . mustParse inputP

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2

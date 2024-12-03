module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap

data Inst = L | R deriving (Show, Eq)

type Node = String
type Network = HashMap Node (Node, Node)

inst :: Parser [Inst]
inst = many $ choice [L <$ char 'L', R <$ char 'R']

node :: Parser Node
node = count 3 alphaNum

edge :: Parser (Node, (Node, Node))
edge = do
  source <- node
  string " = ("
  targets <- (,) <$> node <* string ", " <*> node 
  char ')'
  return (source, targets)

network :: Parser ([Inst], Network)
network = do
  sections <- blocks
  case sections of
    [instTxt, edgeTxt] -> 
      return (insts, HashMap.fromList edges)
        where
          insts = mustParse inst (unlines instTxt)
          edges = mustParse (linesOf edge) (unlines edgeTxt)
    _ -> error $ "error parsing sections: " ++ show sections

walk :: Node -> ([Inst], Network) -> [Node]
walk src (insts, net) = f src (cycle insts)
  where
    f i (x:xs)
      | x == L = i : f (fst $ net ! i) xs
      | x == R = i : f (snd $ net ! i) xs

walk' :: [Node] -> ([Inst], Network) -> Int
walk' srcs (insts, net) = f 0 srcs (cycle insts)
  where
    f acc ns (x:xs)
      | p ns = acc
      | x == L  = f (acc+1) (map l ns) xs
      | x == R  = f (acc+1) (map r ns) xs
    p = all (\n -> last n == 'Z')
    l = fst . (net !)
    r = snd . (net !)

zipMany :: [[String]] -> [[String]]
zipMany lists = map head lists : zipMany (map tail lists)

numTill :: (a -> Bool) -> [a] -> Int
numTill p [] = error "condition never met"
numTill p (x:xs)
  | not (p x) = 1 + numTill p xs
  | otherwise = 0


solve1 :: Solver
solve1 = show 
       . length
       . takeWhile (/="ZZZ") 
       . walk "AAA"
       . mustParse network

-- leaks memory somewhere ):
solve2 :: Solver
solve2 input = show $ walk' inits (insts, net)
  where
    (insts, net) = mustParse network input
    inits = [ n | n <- HashMap.keys net, last n == 'A' ]
    -- walks = [ walk n (insts, net) | n <- inits ]
    -- steps = zipMany walks

main :: IO ()
main = runCLI solve1 solve2

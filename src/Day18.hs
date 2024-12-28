module Main where

import Numeric (readHex)
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, choice, try, many1, between, space, alphaNum, string)

type Step = (Board.Dir, Int, String)

dirP :: Parser Board.Dir
dirP =
  choice . map try $
    [ Board.U <$ char 'U'
    , Board.D <$ char 'D'
    , Board.L <$ char 'L'
    , Board.R <$ char 'R'
    ]

hexP :: Parser String
hexP = between (string "(#") (char ')') (many1 alphaNum)

digPlanP :: Parser Step
digPlanP = (,,) <$> dirP <* space <*> int <* space <*> hexP

step :: Int -> Board.Dir -> Board.Pos -> Board.Pos
step n dir (r, c) = case dir of
  Board.U -> (r-n, c)
  Board.D -> (r+n, c)
  Board.L -> (r, c-n)
  Board.R -> (r, c+n)

walkFrom :: Board.Pos -> [Step] -> [Board.Pos]
walkFrom start = foldr add [start]
  where
    add :: Step -> [Board.Pos] -> [Board.Pos] 
    add (dir, n, _) acc@(pos:_) = step n dir pos : acc

-- legendary shoelace formula,,,
shoelace :: [Board.Pos] -> Int
shoelace points = abs $ (posProd - negProd) `div` 2
  where
    n = length points
    (xs, ys) = unzip points
    xs' = take n . tail $ cycle xs
    ys' = take n . tail $ cycle ys
    posProd = sum $ zipWith (*) xs ys'
    negProd = sum $ zipWith (*) ys xs'

area :: [Step] -> Int
area = shoelace . walkFrom (0, 0)

perimeter :: [Step] -> Int
perimeter steps = sum [n | (_, n, _) <- steps ]

areaFromMidpoints :: [Step] -> Int
areaFromMidpoints steps = area steps + ((perimeter steps `div` 2) + 1)

decodeHex :: [Step] -> [Step]
decodeHex = map decode
  where
    decode (_, _, hex) = case (`divMod` 16) . fst . head . readHex $ hex of
      (n, 0) -> (Board.R, n, hex)
      (n, 1) -> (Board.D, n, hex)
      (n, 2) -> (Board.L, n, hex)
      (n, 3) -> (Board.U, n, hex)
      _ -> error $ "cannot decode " ++ show hex 

solve1 :: Solver
solve1 = show . areaFromMidpoints . mustParse (linesOf digPlanP)

solve2 :: Solver
solve2 = show . areaFromMidpoints . decodeHex . mustParse (linesOf digPlanP)

main :: IO ()
main = runCLI solve1 solve2

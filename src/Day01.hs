module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Data.Char (isDigit)
import Text.Parsec

document :: Parser [String]
document = block

val1 :: String -> Int
val1 xs = read [head ys, last ys]
  where
    ys = filter isDigit xs

val2 :: String -> Int
val2 xs = read [mustParse numL xs, mustParse numR (reverse xs)]

skipTill :: Parser a -> Parser a
skipTill p = anyChar `manyTill` lookAhead p *> p

numL :: Parser Char
numL = skipTill . choice . fmap try $
  [ digit
  , '1' <$ string "one"
  , '2' <$ string "two"
  , '3' <$ string "three"
  , '4' <$ string "four"
  , '5' <$ string "five"
  , '6' <$ string "six"
  , '7' <$ string "seven"
  , '8' <$ string "eight"
  , '9' <$ string "nine"
  ]

numR :: Parser Char
numR = skipTill . choice . fmap try $
  [ digit
  , '1' <$ string (reverse "one")
  , '2' <$ string (reverse "two")
  , '3' <$ string (reverse "three")
  , '4' <$ string (reverse "four")
  , '5' <$ string (reverse "five")
  , '6' <$ string (reverse "six")
  , '7' <$ string (reverse "seven")
  , '8' <$ string (reverse "eight")
  , '9' <$ string (reverse "nine")
  ]

solve1 :: Solver
solve1 = show . sum . map val1 . mustParse document

solve2 :: Solver
solve2 = show . sum . map val2 . mustParse document

main :: IO ()
main = runCLI solve1 solve2

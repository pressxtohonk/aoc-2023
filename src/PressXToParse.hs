module PressXToParse where

import Control.Monad (void)
import Text.Parsec

type Pair a = (a, a)
type Trio a = (a, a, a)

type Parser = Parsec String ()

mustParse :: Parser a -> String -> a
mustParse parser input = case parse parser "" input of
  Left err -> error (show err)
  Right x -> x

-- Token parsers
gap :: Parser String
gap = many (char ' ')

int :: Parser Int
int = do
  sign <- option "" (string "-")
  nums <- many1 digit
  return $ read (sign ++ nums)

list :: Parser a -> Parser [a]
list item = gap >> item `sepEndBy` gap

ints :: Parser [Int]
ints = list int

eol :: Parser ()
eol = void endOfLine

end :: Parser ()
end = eol <|> eof

-- Schema parsers
line :: Parser String
line = nonEmpty $ anyChar `manyTill` lookAhead end

block :: Parser [String]
block = linesOf line

blocks :: Parser [[String]]
blocks = linesOf block

-- custom combinators
nonEmpty :: Foldable f => Parser (f a) -> Parser (f a)
nonEmpty parser = do
  token <- parser
  if null token
    then fail "expected non-empty parse result"
    else return token

linesOf :: Parser a -> Parser [a]
linesOf item = item `endBy1` end

pair :: Parser a -> Parser (Pair a)
pair item = do
  a <- item
  spaces
  b <- item
  return (a, b)

trio :: Parser a -> Parser (Trio a)
trio item = do
  a <- item
  spaces
  b <- item
  spaces
  c <- item
  return (a, b, c)


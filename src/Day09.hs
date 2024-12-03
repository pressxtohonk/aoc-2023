module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)

report :: Parser [[Int]]
report = linesOf (nonEmpty ints)

diff :: [Int] -> [Int]
diff [] = error "cannot diff empty list"
diff [_] = []
diff (a:b:rest) = (b-a) : diff (b:rest)

encode :: [Int] -> (Int, [Int])
encode [] = error "cannot encode empty list"
encode xs = (head xs, diff xs)

decode :: (Int, [Int]) -> [Int]
decode (x, []) = [x]
decode (x, dx:rest) = x : decode (x+dx, rest)

compress :: [Int] -> [Int]
compress = f []
  where
    f acc xs
      | any (/=0) xs = let (a, dx) = encode xs in f (a:acc) dx
      | otherwise    = acc

uncompress :: [Int] -> [Int]
uncompress = f (repeat 0)
  where
    f acc [] = acc
    f acc (x:xs) = f (decode (x, acc)) xs

extrapolate :: [Int] -> [Int]
extrapolate = uncompress . compress

predict :: [Int] -> Int
predict xs = extrapolate xs !! length xs

solve1 :: Solver
solve1 = show . sum . map predict . mustParse report

solve2 :: Solver
solve2 = show . sum . map (predict . reverse) . mustParse report

main :: IO ()
main = runCLI solve1 solve2

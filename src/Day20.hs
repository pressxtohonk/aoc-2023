module Main where

import Control.Monad.RWS
import Data.List (foldl', transpose, sort)
import Data.Map ((!))
import qualified Data.Map as Map
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (string, sepBy, letter, many, manyTill, choice, try)

data Pulse = Hi | Lo deriving (Eq, Ord, Show)

toggle :: Pulse -> Pulse
toggle Hi = Lo
toggle Lo = Hi

data Module
  = FlipFlop
  | Conjunction
  | Broadcaster
  deriving Show

type Wire = (Module, String, String) -- mod in = out

connsP :: Module -> Parser [Wire]
connsP mod = do
  src <- letter `manyTill` string " -> "
  dst <- many letter `sepBy` string ", "
  return $ (mod, src,) <$> reverse dst

wiresP :: Parser [Wire]
wiresP =
  fmap concat . linesOf . choice . map try $
    [ try (string "%") >> connsP FlipFlop
    , try (string "&") >> connsP Conjunction
    , connsP Broadcaster
    ]

type Modules = Map.Map String (Module, [String], [String])
type Pulses = Map.Map String (Map.Map String Pulse)
type System = RWS Modules [Signal] Pulses
type Signal = (String, Pulse, String)

getModules :: [Wire] -> Modules
getModules wires = Map.fromListWith update (srcs ++ dsts)
  where
    srcs = [(j, (m, [i], [])) | (m, i, j) <- wires]
    dsts = [(i, (m, [], [j])) | (m, i, j) <- wires]
    update (m, s1, d1) (_, s2, d2) = (m, s1++s2, d1++d2)

getPulses :: [Wire] -> Pulses
getPulses wires = Map.fromListWith Map.union (self ++ rest)
  where
    self = [(k, Map.singleton k (initialize m)) | (m, _, k) <- wires]
    rest = [(j, Map.singleton i (initialize m)) | (m, i, j) <- wires]
    initialize FlipFlop = Lo
    initialize Conjunction = Lo
    initialize Broadcaster = Lo

-- propagates a signal breadth first through the system till convergence
walk :: Signal -> System ()
walk signal = go [signal]
  where
    go [] = return ()
    go signals = mapM step signals >>= go . concat

step :: Signal -> System [Signal]
step (prev, pulse, label) = do
  tell [(prev, pulse, label)]
  modify $ Map.insertWith Map.union label (Map.singleton prev pulse)
  (mod, srcs, dsts) <- reader (! label)
  case mod of
    FlipFlop -> do
      state <- gets ((! label) . (! label))
      if pulse == Hi
        then return []
        else report dsts (toggle state)
    Conjunction -> do
      inputs <- mapM (\s -> gets ((! s) . (! label))) srcs
      if all (==Hi) inputs
        then report dsts Lo
        else report dsts Hi
    Broadcaster -> do
      report dsts pulse
    where
      report :: [String] -> Pulse -> System [Signal]
      report dsts output = do
        modify $ Map.insertWith Map.union label (Map.singleton label output)
        return $ (label, output,) <$> dsts

simulate :: System a -> [Wire] -> (a, Pulses, [Signal])
simulate action wires = runRWS action (getModules wires) (getPulses wires)

countPulses :: (a, Pulses, [Signal]) -> (Int, Int)
countPulses (_, _, signals) = foldl' combine (0, 0) signals
  where
    combine (ls, hs) (_, Lo, _) = (ls+1, hs)
    combine (ls, hs) (_, Hi, _) = (ls, hs+1)

coreDump :: System String
coreDump = do
  dump <- gets Map.unions
  let write Lo = ('0':)
      write Hi = ('1':)
  return $ foldr write "" dump

-- Try organize bit order to make it easier to spot patterns
formatDump :: ([String], Pulses, [Signal]) -> String
formatDump (dump, _, _) = unlines . transpose . sort . transpose $ dump

solve1 :: Solver
solve1 = show . uncurry (*) . countPulses . simulate actions . mustParse wiresP
  where
    actions = replicateM_ 1000 $ walk ("button", Lo, "broadcaster")

solve2 :: Solver
solve2 = const . show $ foldl' lcm 1 [3917, 3919, 4007,4027]

-- Dumps the machine state in binary for the first 4096 steps
-- The dump reveals the machine is made of 4 binary counters
-- Counters have cycle lengths: 3917, 3919, 4007,4027
debug :: Solver
debug = formatDump . simulate actions . mustParse wiresP
  where
    actions = replicateM (2^12) $ do
     walk ("button", Lo, "broadcaster")
     coreDump

main :: IO ()
main = runCLI solve1 solve2

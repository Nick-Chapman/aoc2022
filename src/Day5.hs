module Day5 (main) where

import Misc (check,look)
import Par4 (Par,parse,terminated,separated,lit,alts,key,int,char,nl,many,dot)
import Data.Maybe (catMaybes)
import Data.List as List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- setup <$> readFile "input/day5.sam"
  inp <- setup <$> readFile "input/day5.input"
  print ("day5, part1 (sam)", check "CMZ" $ part1 sam)
  print ("day5, part1", check "TPGVQPFDH" $ part1 inp)
  print ("day5, part2 (sam)", check "MCD" $ part2 sam)
  print ("day5, part2 (sam)", check "DMRDFRHHH" $ part2 inp)
    where
      part1 = go Part1
      part2 = go Part2

data Part = Part1 | Part2
data State = State (Map Int Stack) deriving Show
type Stack = [Crate]

go :: Part -> Setup -> String
go part (lines,ops) = finish (foldl step (begin lines) ops)
  where

    begin :: [Line] -> State
    begin xss = do
      State (Map.fromList (zip [1..] (map catMaybes (transpose xss))))

    step :: State -> Op -> State
    step (State m) (Op n s t) = do
      let a = look s m
      let b = look t m
      let (a1,a2) = (take n a, drop n a)
      let a' = a2
      let b' = (case part of Part1 -> reverse a1; Part2 -> a1) ++ b
      State $ Map.insert s a' $ Map.insert t b' $ m

    finish (State m) = do
      let n = maximum (Map.keys m)
      [ head (look k m) | k <- [1..n] ]


type Setup = ([Line],[Op])
type Line = [Maybe Crate]
data Op = Op Int StackId StackId deriving Show
type StackId = Int
type Crate = Char

setup :: String -> Setup
setup = parse gram
  where
    gram :: Par Setup
    gram = do
      xs <- terminated nl line
      do _ <- many dot; nl; nl -- skip numeric column labels
      ys <- terminated nl op
      pure (xs,ys)

    line :: Par Line
    line = separated (lit ' ') maybeCrate

    maybeCrate :: Par (Maybe Crate)
    maybeCrate = alts [ Just <$> crate, do key "   "; pure Nothing ]

    crate :: Par Crate
    crate = do
      lit '['
      c <- char
      lit ']'
      pure c

    op = do
      key "move "
      i <- int
      key " from "
      s <- int
      key " to "
      t <- int
      pure $ Op i s t

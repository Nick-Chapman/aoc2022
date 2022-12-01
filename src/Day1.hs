module Day1 (main) where

import Misc (check,splitOn)
import Data.List as List (sort)

main :: IO ()
main = do
  s <- readFile "input/day1.input"
  let inp = parse s
  print ("day1, part1", check 71506 $ part1 inp)
  print ("day2, part2", check 209603 $ part2 inp)

type Setup = [[Int]]

parse :: String -> Setup
parse = map (map (read @Int)) . splitOn "" . lines

part1 :: Setup -> Int
part1 = maximum . map sum

part2 :: Setup -> Int
part2 = sum . take 3 . reverse . List.sort . map sum

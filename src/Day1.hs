module Day1 (main) where

import Misc (check)
import Par4 (Par,parse,separated,terminated,nl,int)
import Data.List as List (sort)

main :: IO ()
main = do
  inp <- parse gram <$> readFile "input/day1.input"
  print ("day1, part1", check 71506 $ part1 inp)
  print ("day2, part2", check 209603 $ part2 inp)

type Setup = [[Int]]

part1 :: Setup -> Int
part1 = maximum . map sum

part2 :: Setup -> Int
part2 = sum . take 3 . reverse . List.sort . map sum

gram :: Par Setup
gram = separated nl (terminated nl int)

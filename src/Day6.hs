module Day6 (main) where

import Misc (check,nub)
import Data.List (tails)

main :: IO ()
main = do
  let sam = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  inp <- readFile "input/day6.input"
  print ("day6, part1 (sam)", check 7 $ part1 sam)
  print ("day6, part1", check 1848 $ part1 inp)
  print ("day6, part2 (sam)", check 19 $ part2 sam)
  print ("day6, part2", check 2308 $ part2 inp)
  where
    part1 = go 4
    part2 = go 14

go :: Int -> String -> Int
go n inp =
  head [ i
       | (i,xs) <- zip [n::Int ..] (tails inp)
       , length (nub (take n xs)) == n
       ]

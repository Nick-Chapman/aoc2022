module Day4 (main) where

import Misc (check)
import Par4 (Par,parse,terminated,nl,int,lit)

main :: IO ()
main = do
  inp <- parse gram <$> readFile "input/day4.input"
  print ("day4, part1", check 560 $ part1 inp)
  print ("day4, part2", check 839 $ part2 inp)

type Setup = [Line]
type Line = (Int,Int,Int,Int)

gram :: Par Setup
gram = terminated nl line
  where
    line = do
      a <- int; lit '-'
      b <- int; lit ','
      c <- int; lit '-'
      d <- int; pure (a,b,c,d)

part1 :: Setup -> Int
part1 xs = length (filter contained xs)
  where contained (a,b,c,d) = (a <= c && b >= d) || (c <= a && d >= b)

part2 :: Setup -> Int
part2 xs = length (filter (not . disjoint) xs)
  where disjoint (a,b,c,d) = (b < c) || (a > d)

module Day3 (main) where

import Misc (check,the,hist,nub)
import qualified Data.Map as Map
import Data.Char as Char (ord)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  inp <- lines <$> readFile "input/day3.input"
  print ("day3, part1", check 8072 $ part1 inp)
  print ("day3, part2", check 2567 $ part2 inp)

type Setup = [String]

prio :: Char -> Int
prio c
  | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise = undefined

part1 :: Setup -> Int
part1 lines = sum (map perLine lines)
  where
    perLine line = do
      let n = length line `div` 2
      let (xs,ys) = splitAt n line
      the [ prio k
          | (k,v) <- Map.toList (hist (nub xs ++ nub ys))
          , v == 2
          ]

part2 :: Setup -> Int
part2 lines = sum (map per3lines (chunksOf 3 lines))
  where
    per3lines chunk = do
      let [xs,ys,zs] = chunk
      the [ prio k
          | (k,v) <- Map.toList (hist (nub xs ++ nub ys ++ nub zs))
          , v == 3
          ]

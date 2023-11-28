{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Day3 (main) where

import Misc (check,the)
import Data.Char as Char (ord)
import Data.List.Split (chunksOf)
import qualified Set as Set
import Set (intersection)

main :: IO ()
main = do
  inp <- lines <$> readFile "input/day3.input"
  print ("day3, part1", check 8072 $ part1 inp)
  print ("day3, part2", check 2567 $ part2 inp)

type Setup = [String]

prio :: Char -> Int
prio c = if
  | 'a' <= c && c <= 'z' -> ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' -> ord c - ord 'A' + 27
  | otherwise -> undefined

part1 :: Setup -> Int
part1 = sum . map perLine
  where
    perLine line = do
      let n = length line `div` 2
      let (xs,ys) = splitAt n line
      the [ prio c
          | c <- Set.toList (Set.fromList xs `intersection` Set.fromList ys)
          ]

part2 :: Setup -> Int
part2 = sum . map per3lines . chunksOf 3
  where
    per3lines chunk = do
      let [xs,ys,zs] = chunk
      the [ prio c
          | c <- Set.toList (Set.fromList xs `intersection` Set.fromList ys
                             `intersection` Set.fromList zs)
          ]

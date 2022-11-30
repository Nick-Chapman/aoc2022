module Day1 (main) where

import Misc (check)
import Par4 (Par,parse,terminated,nl,int)

main :: IO ()
main = do
  inp <- load "input/day1.input"
  print ("day1, part1", check 5 $ part1 inp)

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par [Line]
gram = terminated nl int

type Setup = [Line]
type Line = Int

part1 :: Setup -> Int
part1 = length

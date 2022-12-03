module DayX (main) where

import Misc (check)
import Par4 --(Par,parse,many,terminated,sat,nl)

main :: IO ()
main = do
  inp <- parse gram <$> readFile "input/day3.input"
  mapM_ print (zip [1::Int ..] inp)
  print ("dayX, part1", check 2000 $ part1 inp)

type Setup = [String]

gram :: Par Setup
gram = terminated nl line
  where
    line = many dot
    dot = sat $ \c -> c /= '\n'

part1 :: Setup -> Int
part1 = length

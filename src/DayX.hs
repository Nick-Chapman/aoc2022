module DayX (main) where

import Misc (check)
import Par4 --(Par,parse,many,terminated,sat,nl)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day16.sam"
  mapM_ print (zip [1::Int ..] sam)

  res <- part1 sam
  print ("dayX, part1", check 0 $ res)

part1 :: Setup -> IO Int
part1 xs = do
  print "part1"
  pure (length xs)

type Setup = [String]

gram :: Par Setup
gram = terminated nl line
  where
    line = many dot
    dot = sat $ \c -> c /= '\n'

module Day25 (main) where

import Misc (check)
import Par4 (Par,parse,many,terminated,sat,nl)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day25.sam"
  inp <- parse gram <$> readFile "input/day25.input"
  print ("day25, part1", check "2=-1=0" $ part1 sam)
  print ("day25, part1", check "20-1-11==0-=0112-222" $ part1 inp)

part1 :: Setup -> String
part1 = toSnarfu . sum . map fromSnarfu

fromSnarfu :: String -> Int
fromSnarfu s =
  sum [ ofChar c * (5 ^ n) | (n,c) <- zip [0::Int ..] (reverse s) ]
  where
    ofChar = \case
      '=' -> -2
      '-' -> -1
      '0' -> 0
      '1' -> 1
      '2' -> 2
      x -> error (show ("fromSnarfu",x))

toSnarfu :: Int -> String
toSnarfu = loop ""
  where
    loop :: String -> Int -> String
    loop acc n =
      if n == 0 then acc else do
        let (d,m) = (n `div` 5, n `mod` 5)
        case m of
          0 -> loop ('0':acc) d
          1 -> loop ('1':acc) d
          2 -> loop ('2':acc) d
          3 -> loop ('=':acc) (d+1)
          4 -> loop ('-':acc) (d+1)
          x -> error (show ("toSnarfu",x))

type Setup = [String]

gram :: Par Setup
gram = terminated nl line
  where
    line = many dot
    dot = sat $ \c -> c /= '\n'

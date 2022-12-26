
module Day15 (main) where

import Misc (check,the)
import Par4 (Par,parse,terminated,nl,key,opt,lit,int)
import RangeISet (Set,empty,fromRange,union,removeElem,size,invert,toList)

main :: IO ()
main = do
  --sam <- parse gram <$> readFile "input/day15.sam"
  inp <- parse gram <$> readFile "input/day15.input"
  --print ("day15, part1 (sam)", check 26 $ part1 10 sam)
  print ("day15, part1", check 5716881 $ part1 2000000 inp)
  --print ("day15, part2 (sam)", check 56000011 $ part2 20 sam)
  print ("day15, part2", check 10852583132904 $ part2 4000000 inp)

part1 :: Int -> Setup -> Int
part1 y readings = do
  let blocked = foldl union empty (map (blockedOnLine y) readings)
  let beaconsOnLine = [ x | Reading { beacon = (x,y') } <- readings, y' == y ]
  size (foldl removeElem blocked beaconsOnLine)

part2 :: Int -> [Reading] -> Int
part2 max readings =
  the [ 4000000 * x + y
      | y <- [0..max]
      , let blocked = map (blockedOnLine y) readings
      , let open = (0,max) `invert` foldl union empty blocked
      , size open == 1
      , let [x] = toList open
      ]

blockedOnLine :: Int -> Reading -> Set
blockedOnLine y Reading{beacon,sensor} = do
  let (sx,sy) = sensor
  let z = mdist sensor beacon - abs (y - sy)
  if z < 0 then empty else do fromRange ( sx - z, sx + z )

mdist :: Pos -> Pos -> Int
mdist (x1,y1) (x2,y2) = abs(x1-x2) + abs(y1-y2)

type Setup = [Reading]
data Reading = Reading { sensor :: Pos, beacon :: Pos } deriving Show
type Pos = (Int,Int)

gram :: Par Setup
gram = terminated nl reading
  where
    reading = do
      key "Sensor at "
      sensor <- pos
      key ": closest beacon is at "
      beacon <- pos
      pure Reading { sensor, beacon }

    pos = do
      key "x="
      x <- sint
      key ", y="
      y <- sint
      pure (x,y)

    sint = do
      s <- opt (lit '-')
      i <- int
      pure $ case s of Nothing -> i; Just () -> (-1) * i

module Day18(main) where

import Misc (check)
import Par4 (Par,parse,terminated,nl,int,lit)
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day18.sam"
  inp <- parse gram <$> readFile "input/day18.input"
  print ("day18, part1 (sam)", check 64 $ part1 sam)
  print ("day18, part1", check 4504 $ part1 inp)

part1 :: Setup -> Int
part1 ps = do
  let a = Set.size $ Set.fromList [ q | p@(x,y,z) <- ps, q <- [p,(x+1,y,z)] ]
  let b = Set.size $ Set.fromList [ q | p@(x,y,z) <- ps, q <- [p,(x,y+1,z)] ]
  let c = Set.size $ Set.fromList [ q | p@(x,y,z) <- ps, q <- [p,(x,y,z+1)] ]
  (2 * (a+b+c)) - (length ps * 6)

type Setup = [Pos]
type Pos = (Int,Int,Int)

gram :: Par Setup
gram = terminated nl pos
  where
    pos = do
      x <- int
      lit ','
      y <- int
      lit ','
      z <- int
      pure (x,y,z)

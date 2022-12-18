module Day18(main) where

import Misc (check)
import Par4 (Par,parse,terminated,nl,int,lit)
import qualified Data.Set as Set
import Data.Set (Set,difference,union,notMember)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day18.sam"
  inp <- parse gram <$> readFile "input/day18.input"
  print ("day18, part1 (sam)", check 64 $ part1 sam)
  print ("day18, part1", check 4504 $ part1 inp)
  print ("day18, part2 (sam)", check 58 $ part2 sam)
  print ("day18, part2", check 2556 $ part2 inp)

part1 :: Setup -> Int
part1 ps = do
  let a = Set.size $ Set.fromList [ q | p@(x,y,z) <- ps, q <- [p,(x+1,y,z)] ]
  let b = Set.size $ Set.fromList [ q | p@(x,y,z) <- ps, q <- [p,(x,y+1,z)] ]
  let c = Set.size $ Set.fromList [ q | p@(x,y,z) <- ps, q <- [p,(x,y,z+1)] ]
  (2 * (a+b+c)) - (length ps * 6)

part2 :: Setup -> Int
part2 ps = do
  let (a,b) = (minimum is, maximum is)
        where is = [ i | (x,y,z) <- ps, i <- [x,y,z] ]

  let solid = Set.fromList ps
  let
    inBounds :: Pos -> Bool
    inBounds (x,y,z) = x>=a && x<=b && y>=a && y<=b && z>=a && z<=b

    step :: Pos -> [Pos]
    step (x,y,z) =
      [ (x-1,y,z), (x+1,y,z), (x,y-1,z), (x,y+1,z), (x,y,z-1), (x,y,z+1) ]

    flow :: Set Pos -> Set Pos
    flow ps =
      Set.fromList [ q
                   | p <- Set.toList ps
                   , q <- step p
                   , inBounds q
                   , q `notMember` solid
                   ]
  let
    loop :: Int -> Set Pos -> Set Pos -> Set Pos
    loop i acc frontier = do
      if Set.size frontier == 0 then acc else do
        let acc' = acc `union` frontier
        let frontier' = flow frontier `difference` acc
        loop (i+1) acc' frontier'

  let all = Set.fromList [ (x,y,z) | x <- [a..b], y <- [a..b], z <- [a..b] ]
  let space = all `difference` solid
  let shell = Set.fromList [ (x,y,z) | (x,y,z) <- Set.toList all
                             , x==a || x==b || y==a || y==b || z==a || z==b ]
  let init = shell `difference` solid
  let reach = loop 0 Set.empty init
  let notReach = Set.toList $ space `difference` reach
  part1 ps - part1 notReach

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

module Day9 (main) where

import Misc (check,nub)
import Par4 (Par,parse,terminated,nl,lit,int,alts)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day9.sam"
  sam2 <- parse gram <$> readFile "input/day9.sam2"
  inp <- parse gram <$> readFile "input/day9.input"
  print ("day9, part1 (sam)", check 13 $ part1 sam)
  print ("day9, part1", check 6563 $ part1 inp)
  print ("day9, part2 (sam)", check 1 $ part2 sam)
  print ("day9, part2 (sam2)", check 36 $ part2 sam2)
  print ("day9, part2", check 2653 $ part2 inp)
  where
    part1 = partX 2
    part2 = partX 10

partX :: Int -> Setup -> Int
partX n setup = do
  let dirs = setup >>= \(d,i) -> take i (repeat d)
  let s0 = take n (repeat (0,0))
  length $ nub [ last ps | ps <- scanl step s0 dirs ]

type State = [Pos]
type Pos = (Int,Int)

step :: State -> Dir -> State
step ps dir = case ps of
  [] -> error "no knots"
  p1:ps -> scanl pull (move dir p1) ps

move :: Dir -> Pos -> Pos
move d (x,y) = case d of U -> (x,y-1); D -> (x,y+1); L -> (x-1,y); R -> (x+1,y)

pull :: Pos -> Pos -> Pos
pull p1 p2 = do
  let (sign,mag) = split (p1 `sub` p2)
  if close mag then p2 else do sign `add` p2
  where
    add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
    sub (x1,y1) (x2,y2) = (x1-x2,y1-y2)
    split (x,y) = ((sign x,sign y), (abs x, abs y))
    sign a = if | a < 0 -> -1 | a > 0 -> 1 | otherwise -> 0
    close (x,y) = x <= 1 && y <= 1

type Setup = [Line]
type Line = (Dir,Int)
data Dir = U | D | L | R deriving Show

gram :: Par Setup
gram = terminated nl line
  where
    line = do d <- dir; lit ' '; i <- int; pure (d,i)
    dir = alts [ i 'U' U, i 'D' D, i 'L' L, i 'R' R ]
    i c v = do lit c; pure v

module Day17 (main) where

import Data.Set (Set)
import Misc (check)
import Par4 (Par,parse,many,nl,alts,lit)
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day17.sam"
  inp <- parse gram <$> readFile "input/day17.input"
  print ("day17, part1 (sam)", check 3068 $ part1 sam)
  print ("day17, part1", check 3106 $ part1 inp)

type Setup = [LR]
data LR = L | R deriving Show

gram :: Par Setup
gram = do xs <- many lr; nl; pure xs
  where
    lr = alts [ do lit '<'; pure L
              , do lit '>'; pure R]

type Pos = (Int,Int)

inBounds :: Pos -> Bool
inBounds (x,y) = x>=1 && x<=7 && y>=1

moveRight :: Pos -> Pos
moveRight (x,y) = (x+1,y)

moveLeft :: Pos -> Pos
moveLeft (x,y) = (x-1,y)

moveDown :: Pos -> Pos
moveDown (x,y) = (x,y-1)

locate :: Pos -> Pos -> Pos
locate (x1,y1) (x,y) = (x+x1,y+y1)

shapes :: [Set Pos]
shapes = map Set.fromList
  [ [ (0,0), (1,0), (2,0), (3,0) ]
  , [ (1,0), (0,1), (1,1), (2,1), (1,2) ]
  , [ (0,0), (1,0), (2,0), (2,1), (2,2) ]
  , [ (0,0), (0,1), (0,2), (0,3) ]
  , [ (0,0), (1,0), (0,1), (1,1) ]
  ]

data State = State
  { solid :: Set Pos
  , wind :: [LR]
  , wind0 :: [LR]
  }

part1 :: Setup -> Int
part1 wind0 = do
  let s0 = State { wind = [] , wind0 = wind0 , solid = Set.empty}
  height (dropN 2022 s0)

height :: State -> Int
height State{solid} =
  maximum [ y | (_,y) <- (0,0) : Set.toList solid ]

dropN :: Int -> State -> State
dropN n s =
  if n < 5 then dropList (take n shapes) s else dropN (n-5) (dropList shapes s)

dropList :: [Set Pos] -> State -> State
dropList us s =
  foldl drop1 s us

drop1 :: State -> Set Pos -> State
drop1 s0 u = do
  let fall = place u s0
  loopUntilLand fall s0

place :: Set Pos -> State -> Set Pos
place u s = do
  Set.map (locate (3, height s + 4)) u

loopUntilLand :: Set Pos -> State -> State
loopUntilLand f0 s1 = do
  let (f1,s2) = blow (f0,s1)
  let f2 = Set.map moveDown f1
  let imp = impossible f2 s2
  if not imp then loopUntilLand f2 s2 else freeze f1 s2

blow :: (Set Pos, State) -> (Set Pos, State)
blow (f0,s0@State{wind,wind0}) = do
  case wind of
    [] -> blow (f0, s0 { wind = wind0 })
    w:wind -> do
      let s1 = s0 { wind }
      let move = case w of L -> moveLeft; R -> moveRight
      let f1 = Set.map move f0
      ((if impossible f1 s0 then f0 else f1), s1)

impossible :: Set Pos -> State -> Bool
impossible f State{solid} =
  any (not . inBounds) f
  || not (Set.null (f `Set.intersection` solid))

freeze :: Set Pos -> State -> State
freeze f s@State{solid} = do
  s { solid = solid `Set.union` f }


{-landscape :: Int -> State -> IO ()
landscape i State{solid} = do
  let ps = Set.toList solid
  let ys = [ maximum (0:[ y | (x,y) <- ps, x == c ]) | c <- [1..7]]
  let min = minimum ys
  let hs = [ y - min | y <- ys ]
  let steps = [ abs (a-b) | (a,b) <- zip hs (tail hs) ]
  let s = maximum steps
  if s /= 1 then pure () else
    print (i,hs,steps,s)-}

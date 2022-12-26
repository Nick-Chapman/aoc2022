
module Day14 (main) where

import Data.Set (Set,member,insert)
import Misc (check)
import Par4 (Par,parse,separated,terminated,nl,key,int,lit)
import qualified Data.Set as Set (fromList)

main :: IO ()
main = do
  --sam <- parse gram <$> readFile "input/day14.sam"
  inp <- parse gram <$> readFile "input/day14.input"
  --print ("day14, part1 (sam)", check 24 $ part1 sam)
  print ("day14, part1", check 964 $ part1 inp)
  --print ("day14, part2 (sam)", check 93 $ part2 sam)
  print ("day14, part2", check 32041 $ part2 inp)
  where
    part1 = partX Part1
    part2 = partX Part2

type Setup = [[Pos]]
type Pos = (Int,Int)

gram :: Par Setup
gram = terminated nl line
  where
    line = separated (key " -> ") pos
    pos = do x <- int; lit ','; y <- int; pure (x,y)

data State = State { filled :: Set Pos } deriving Show

data Part = Part1 | Part2

partX :: Part -> Setup -> Int
partX part setup = do
  let step = case part of Part1 -> step1; Part2 -> step2
  let deepest = maximum [ y | (_,y) <- concat setup ]
  let s0 = initState setup
  let
    run :: State -> [State]
    run s = s : case step deepest s of
      Nothing -> []
      Just s' -> run s'
  let ss = run s0
  length ss - 1

step1 :: Int -> State -> Maybe State
step1 deepest State{filled} = flow (500,0)
  where
    blocked = (`member` filled)
    flow :: Pos -> Maybe State
    flow p = do
      let fill = State { filled = p `insert` filled }
      if p `atDepth` deepest then Nothing else do
        if (blocked p) then error (show ("p-is-filled:",p)) else do
          let pd = down p
          if not (blocked pd) then flow pd else do
            let pl = downLeft p
            if not (blocked pl) then flow pl else do
              let pr = downRight p
              if not (blocked pr) then flow pr else
                Just $ fill

step2 :: Int -> State -> Maybe State
step2 deepest State{filled} = flow (500,0)
  where
    blocked = (`member` filled)
    flow :: Pos -> Maybe State
    flow p = do
      let fill = State { filled = p `insert` filled }
      if p `atDepth` (deepest+1) then Just fill else -- changed here
        if (blocked p) then Nothing else do -- and here
          let pd = down p
          if not (blocked pd) then flow pd else do
            let pl = downLeft p
            if not (blocked pl) then flow pl else do
              let pr = downRight p
              if not (blocked pr) then flow pr else
                Just fill

initState :: Setup -> State
initState pss = do
  State { filled = Set.fromList (concat (map rock pss)) }
  where
    rock :: [Pos] -> [Pos]
    rock ps  = concat (zipWith bar ps (tail ps))

    bar :: Pos -> Pos -> [Pos]
    bar p1 p2 = do
      if horizonal p1 p2 then hbar p1 p2 else
        if vertical p1 p2 then vbar p1 p2 else
          error (show ("rock",p1,p2))

    hbar (x1,y) (x2,_) = [ (x,y) | x <- [ min x1 x2 .. max x1 x2 ] ]
    vbar (x,y1) (_,y2) = [ (x,y) | y <- [ min y1 y2 .. max y1 y2 ] ]

horizonal :: Pos -> Pos -> Bool
horizonal (_,y1) (_,y2) = y1==y2

vertical :: Pos -> Pos -> Bool
vertical (x1,_) (x2,_) = x1==x2

atDepth :: Pos -> Int -> Bool
atDepth (_,y) d = y == d

down :: Pos -> Pos
down (x,y) = (x,y+1)

downLeft :: Pos -> Pos
downLeft (x,y) = (x-1,y+1)

downRight :: Pos -> Pos
downRight (x,y) = (x+1,y+1)

module Day17 (main) where

import Set (Set)
import Data.Map (Map)
import Misc (check)
import Par4 (Par,parse,many,nl,alts,lit)
import qualified Set as Set
import qualified Data.Map.Strict  as Map

main :: IO ()
main = do
  --sam <- parse gram <$> readFile "input/day17.sam"
  inp <- parse gram <$> readFile "input/day17.input"
  --print ("day17, part1 (sam)", check 3068 $ part1 sam)
  print ("day17, part1", check 3106 $ part1 inp)
  --res <- part2 sam
  --print ("day17, part2 (sam)", check 1514285714288 $ res)
  res <- part2 inp
  print ("day17, part2", check 1537175792495 $ res)

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
  , wc :: Int
  }

initState :: [LR] -> State
initState wind0 = do
  let wind = wind0 ++ wind
  State { wc = 0, wind, solid = Set.empty }

part1 :: Setup -> Int
part1 wind0 = do
  let s0 = initState wind0
  let inf = shapes ++ inf
  height (dropList (take 2022 inf) s0)

data V = V {bm :: Int, wm :: Int, steps :: [Int] } deriving (Eq,Ord,Show)
data I = I {n :: Int, h:: Int} deriving Show
type M = Map V I

part2 :: Setup -> IO Int
part2 wind0 = do
  let s0 = initState wind0
  let inf = shapes ++ inf
  loop Map.empty inf 0 s0
  where
    wz = length wind0

    loop :: M -> [Set Pos] -> Int -> State -> IO Int
    loop m upcoming n s = do
      let steps = landscape s
      let flat = maximum steps <= 2
      case flat of
        False ->
          loop m (tail upcoming) (n+1) (drop1 s (head upcoming))
        True -> do
          let State{wc} = s
          let v = V { bm = n `mod` 5, wm = wc `mod` wz, steps }
          let info = I { n, h = height s}
          --print (v,info)
          case Map.lookup v m of
            Nothing -> do
              let m' = Map.insert v info m
              loop m' (tail upcoming) (n+1) (drop1 s (head upcoming))
            Just info1 -> do
              --print info1
              --print info
              let I{n=n1,h=h1} = info1
              let I{n=n2,h=h2} = info
              let nx = n2-n1
              let tril = 1000000000000
              let factor = ((tril-n1) `div` nx)
              let short = tril - (n1 + factor * nx)
              let s2 = dropList (take short upcoming) s
              let h3 = height s2
              pure $ h1 + factor * (h2-h1) + (h3-h2)

    landscape :: State -> [Int]
    landscape State{solid} = do
      let ps = Set.toList solid
      let ys = [ maximum (0:[ y | (x,y) <- ps, x == c ]) | c <- [1..7]]
      let min = minimum ys
      let hs = [ y - min | y <- ys ]
      let steps = [ abs (a-b) | (a,b) <- zip hs (tail hs) ]
      steps

height :: State -> Int
height State{solid} =
  maximum [ y | (_,y) <- (0,0) : Set.toList solid ]

dropList :: [Set Pos] -> State -> State
dropList us s = foldl drop1 s us

drop1 :: State -> Set Pos -> State
drop1 s u = loopUntilLand (place u s) s

place :: Set Pos -> State -> Set Pos
place u s = Set.map (locate (3, height s + 4)) u

loopUntilLand :: Set Pos -> State -> State
loopUntilLand f0 s0 = do
  let State{wc,wind} = s0
  let s1 = s0 { wind = tail wind, wc = wc + 1 }
  let move = case head wind of L -> moveLeft; R -> moveRight
  let f1 = Set.map move f0
  let f2 = if impossible f1 s0 then f0 else f1
  let f3 = Set.map moveDown f2
  if impossible f3 s1 then freeze f2 s1 else loopUntilLand f3 s1

impossible :: Set Pos -> State -> Bool
impossible f State{solid} =
  any (not . inBounds) f
  || not (Set.null (f `Set.intersection` solid))

freeze :: Set Pos -> State -> State
freeze f s@State{solid} = do
  s { solid = solid `Set.union` f }

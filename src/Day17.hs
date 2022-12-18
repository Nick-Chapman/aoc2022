module Day17 (main) where

import Misc (check)
import Par4 --(Par,parse,many,terminated,sat,nl)
import qualified Data.Set as Set
import Data.Set (Set)

main :: IO ()
main = do
  _sam <- parse gram <$> readFile "input/day17.sam"
  _inp <- parse gram <$> readFile "input/day17.input"

  --res <- part1 _sam
  --print ("day17, part1", check 0 $ res) -- 3068

  res <- part1 _inp
  print ("day17, part1", check 0 $ res) --3106

type Setup = [LR]

gram :: Par Setup
gram = do xs <- many lr; nl; pure xs
  where
    lr = alts [ do lit '<'; pure L
              , do lit '>'; pure R]

part1 :: Setup -> IO Int
part1 q = do
  print "part1"
  let ss = sim q
  let pr (i,s) = print (i, height s, s)
  mapM_ pr (zip [0::Int ..] ss)
  pure 99

data State = State
  { solid :: Set Pos
  , falling :: Set Pos
  , wind :: [LR]
  , upcoming :: [Set Pos]
  }

instance Show State where
  show State{falling,solid} =
    show (Set.size solid, Set.toList falling)

type Pos = (Int,Int)
data LR = L | R deriving Show

sim :: Setup -> [State]
sim q = loop (initState q)
  where
    loop s = s : loop2 (blow s)
    loop2 s = s : loop (fall s)


initState :: [LR] -> State
initState wind = do
  let upcoming = shapes ++ upcoming
  let windy = wind ++ windy
  State
    { wind = windy
    , upcoming = tail (take 2023 upcoming)
    , falling = Set.map (locate (3,4)) (head upcoming)
    , solid = Set.empty
    }


shapes :: [Set Pos]
shapes = map Set.fromList
  [ [ (0,0), (1,0), (2,0), (3,0) ]
  , [ (1,0), (0,1), (1,1), (2,1), (1,2) ]
  , [ (0,0), (1,0), (2,0), (2,1), (2,2) ]
  , [ (0,0), (0,1), (0,2), (0,3) ]
  , [ (0,0), (1,0), (0,1), (1,1) ]
  ]

fall :: State -> State
fall s@State{falling} = do
  let s' = s { falling = Set.map moveDown falling }
  if impossible s' then freeze s else s'

freeze :: State -> State
freeze s@State{solid,falling,upcoming} = do
  let s' = s { solid = solid `Set.union` falling }
  let maxY = height s'
  s'
    { falling = Set.map (locate (3,maxY+4)) (head upcoming)
    , upcoming = tail upcoming
    }

height :: State -> Int
height State{solid} =
  maximum [ y | (_,y) <- (0,0) : Set.toList solid ]

blow :: State -> State
blow s@State{falling,wind} = do
  let move = case wind of [] -> undefined; L:_ -> moveLeft; R:_ -> moveRight
  let s' = s { falling = Set.map move falling }
  (if impossible s' then s else s') { wind = tail wind }

impossible :: State -> Bool
impossible State{falling,solid} =
  any (not . inBounds) falling
  ||
  not (Set.null (falling `Set.intersection` solid))

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

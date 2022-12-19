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

data State = State
  { solid :: Set Pos
  , falling :: Set Pos
  , wind :: [LR]
  , upcoming :: [Set Pos]
  }

type Pos = (Int,Int)

initState :: [LR] -> State
initState wind = do
  let upcoming = shapes ++ upcoming
  let windy = wind ++ windy
  State
    { wind = windy
    , upcoming = tail (take 2022 upcoming)
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

part1 :: Setup -> Int
part1 q = do
  loop (initState q)
  where
    loop s = case fall (blow s) of Right s -> loop s; Left h -> h

blow :: State -> State
blow s@State{falling,wind} = do
  case wind of
    [] -> undefined
    w:wind -> do
      let move = case w of L -> moveLeft; R -> moveRight
      let s' = s { falling = Set.map move falling }
      (if impossible s' then s else s') { wind }

fall :: State -> Either Int State
fall s@State{falling} = do
  let s' = s { falling = Set.map moveDown falling }
  if impossible s' then freeze s else Right s'

freeze :: State -> Either Int State
freeze s@State{solid,falling} = do
  let s' = s { solid = solid `Set.union` falling }
  case nextRock s' of
    Nothing -> Left (height s')
    Just s'' -> Right s''

nextRock :: State -> Maybe State
nextRock s@State{upcoming} =
  case upcoming of
    [] -> Nothing
    u:upcoming -> do
      Just $ s
        { falling = Set.map (locate (3, height s + 4)) u
        , upcoming
        }

height :: State -> Int
height State{solid} =
  maximum [ y | (_,y) <- (0,0) : Set.toList solid ]

impossible :: State -> Bool
impossible State{falling,solid} =
  any (not . inBounds) falling
  || not (Set.null (falling `Set.intersection` solid))

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

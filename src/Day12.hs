module Day12 (main) where

import Data.Map (Map)
import Data.Set (Set)
import Misc (check,look,the)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  --sam <- lines <$> readFile "input/day12.sam"
  inp <- lines <$> readFile "input/day12.input"

  --print ("day12, part1 (sam)", check 31 $ part1 sam)
  print ("day12, part1", check 394 $ part1 inp)
  --print ("day12, part2 (sam)", check 29 $ part2 sam)
  print ("day12, part2", check 388 $ part2 inp)

data World = World -- start from 'E', search backwards to 'S' or 'a'
  { step :: Pos -> [Pos]
  , start :: Pos  --'E'
  , target :: Pos -- 'S'
  , low :: Set Pos -- 'S' or 'a'
  }

type Pos = (Int,Int)

nextdoor :: Pos -> [Pos]
nextdoor (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

initW :: [String] -> World
initW css = do
  let depth = length css
  let width = length (head css)
  let
    inbounds :: Pos -> Bool
    inbounds (x,y) = x >=1 && x <= width && y>=1 && y<=depth
  let
    m :: Map Pos Char
    m = Map.fromList [ ((x,y),c) | (y,cs) <- zip [1..] css, (x,c) <- zip [1..] cs]
  let
    height :: Pos -> Char
    height p = dealSE $ look p m
      where dealSE = \case 'S' -> 'a'; 'E' -> 'z'; c -> c
  let
    step p = do
      let h = height p
      [ p' | p' <- nextdoor p, inbounds p', let h' = height p' , h <= inc h' ]
        where inc :: Char -> Char
              inc c = Char.chr (Char.ord c + 1)

  let start = the [ p | (p,'E') <- Map.toList m ]
  let target = the [ p | (p,'S') <- Map.toList m ]
  let low = Set.fromList $ target : [ p | (p,'a') <- Map.toList m ]

  World {step,start,target,low}

part1 :: [String] -> Int
part1 q = do
  let World{step,start,target} = initW q
  let xs = bfs step [start]
  let (ys,_) = break (target `Set.member`) xs
  length ys

part2 :: [String] -> Int
part2 q = do
  let World{step,start,low} = initW q
  let xs = bfs step [start]
  let (ys,_) = break (not . Set.null . (low `Set.intersection`)) xs
  length ys

bfs :: forall a. Ord a => (a -> [a]) -> [a] -> [Set a]
bfs step initial = loop Set.empty (Set.fromList initial)
  where
    loop :: Set a -> Set a -> [Set a]
    loop acc frontier = do
      if Set.null frontier then [] else do
        let acc' = acc `Set.union` frontier
        let frontier' =
              Set.fromList [ a2
                           | a1 <- Set.toList frontier
                           , a2 <- step a1
                           , a2 `notElem` acc'
                           ]
        frontier : loop acc' frontier'

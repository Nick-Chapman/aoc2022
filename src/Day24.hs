module Day24 (main) where

import Misc (check,the)
import Par4 (Par,parse,many,terminated,sat,nl)
import Data.Set (Set)
import Data.Map (Map)
--import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Data.Array (Array,(!),listArray)


main :: IO ()
main = do
  --sam <- parse gram <$> readFile "input/day24.sam"
  inp <- parse gram <$> readFile "input/day24.input"
  --res <- part1 sam
  --print ("day24, part1 (sam)", check 18 $ res)
  res <- part1 inp
  print ("day24, part1", check 322 $ res)
  --res <- part2 sam
  --print ("day24, part2 (sam)", check 54 $ res)
  res <- part2 inp
  print ("day24, part2", check 974 $ res)


type Setup = [String]

gram :: Par Setup
gram = terminated nl line
  where
    line = many dot
    dot = sat $ \c -> c /= '\n'


part1 :: Setup -> IO Int
part1 setup = do
  w <- initWorld setup
  let World{start=(x,y),target} = w
  Node{t} <- search w Node{x,y,t=0} target
  pure t


part2 :: Setup -> IO Int
part2 setup = do
  w <- initWorld setup
  let World{start=start@(x,y),target} = w
  let n0 = Node{x,y,t=0}
  n1 <- search w n0 target
  --print ("n1",n1)
  n2 <- search w n1 start
  --print ("n2",n2)
  n3 <- search w n2 target
  --print ("n2",n3)
  let Node{t} = n3
  pure t


search :: World -> Node -> Pos -> IO Node
search w node (tx,ty) = loop 0 (initSS w node)
  where

    reachGoal :: Node -> Bool
    reachGoal Node{x,y} = x==tx && y==ty

    loop :: Int -> SS -> IO Node
    loop i ss = do
      let (_c,ns,ss1) = expandSS ss
      let reached = [ p | p <- Set.toList ns, reachGoal p ]
      let finished = not (null reached)
      --print ("loop",i,_c,ns)
      --print ("loop",i,_c)
      case finished of
        True -> do
          pure (the reached)
        False -> do
          let ns' = [ n' | n <- Set.toList ns, n' <- stepN w n ]
          let ss2 = foldl (insertSS w) ss1 ns'
          loop (i+1) ss2


data World = World { winds :: Array Int BlizzardMap
                   , width :: Int
                   , height :: Int
                   , start :: Pos
                   , target :: Pos
                   }

initWorld :: Setup -> IO World
initWorld css = do
  let height = length css - 2
  let width = length (head css) - 2
  let start = (0,-1)
  let target = (width-1,height)
  let bm0 = initBM css
  let windsL = iterate (stepBM w) bm0
      winds = listArray (0,1000) windsL
      w = World { winds, width, height, start, target }
  pure w


data SS = SS (Map Cost (Set Node)) deriving Show
type Cost = Int

emptySS :: SS
emptySS = SS Map.empty

--instance Show SS where show (SS m) = show [ (k,Set.size v) | (k,v) <- Map.toList m ]

insertSS :: World -> SS -> Node -> SS
insertSS w (SS m) node = do
  let cost = computeCost w node
  SS (Map.alter (\case Nothing -> Just (Set.singleton node)
                       Just s -> Just (Set.insert node s)
                ) cost m)

expandSS :: SS -> (Cost,Set Node,SS)
expandSS (SS m) = do
  case Map.minViewWithKey m of
    Nothing -> error "expandSS"
    Just ((c,ns),m) -> (c,ns,SS m)


initSS :: World -> Node -> SS
initSS w node = do
  insertSS w emptySS node

computeCost :: World -> Node -> Int
computeCost World{target=(tx,ty)} Node{x,y,t} = do
  let f = t
  let g = abs(tx-x) + abs(ty-y)
  f+g


data Node = Node { x:: Int , y:: Int, t:: Int } deriving (Eq,Ord)
instance Show Node where show Node{x,y,t} = show (x,y,t)

stepN :: World -> Node -> [Node]
stepN w Node{x,y,t} =
  [ n'
  | n'@(Node x' y' _) <- [ Node (x+1) y (t+1)
                         , Node (x-1) y (t+1)
                         , Node x (y+1) (t+1)
                         , Node x (y-1) (t+1)
                         , Node x y (t+1)
                         ]
  , inBounds w (x',y')
  , not (windy w n')
  ]

inBounds :: World -> Pos -> Bool
inBounds World{width,height,start,target} p@(x,y) =
  p == start || p == target || (x >= 0 && x < width && y >=0 && y < height)

windy :: World -> Node -> Bool
windy World{winds} Node{x,y,t} =
  (winds ! t) `hasBlizzardAt` (x,y)

initBM :: Setup -> BlizzardMap
initBM css = do
  let
    get :: Char -> Set Pos
    get g = Set.fromList [ (x,y)
                         | (y,cs) <- zip [(-1)..] css
                         , (x,c) <- zip [(-1)..] cs
                         , c == g
                         ]
  BM { u = get '^', d = get 'v', l = get '<', r = get '>' }

data BlizzardMap = BM { u:: Set Pos, d:: Set Pos, l:: Set Pos, r:: Set Pos }
  deriving Show

hasBlizzardAt :: BlizzardMap -> Pos -> Bool
hasBlizzardAt BM{u,d,l,r} pos =
  pos `Set.member` u ||
  pos `Set.member` d ||
  pos `Set.member` l ||
  pos `Set.member` r

stepBM :: World -> BlizzardMap -> BlizzardMap
stepBM World{width,height} BM{u,d,l,r} =
  BM
  { u = Set.map up u
  , d = Set.map down d
  , l = Set.map left l
  , r = Set.map right r }
    where
      up (x,y) = (x,(y-1) `mod` height)
      down (x,y) = (x,(y+1) `mod` height)
      left (x,y) = ((x-1) `mod` width, y)
      right (x,y) = ((x+1) `mod` width, y)

type Pos = (Int,Int)

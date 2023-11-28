module Day22 (main) where

import Misc (check)
import Par4 (Par,parse,many,terminated,nl,alts,lit,int)
import qualified Set as Set
import qualified Data.Map as Map
import Set (Set)
import Data.Map (Map)
import Data.List (sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day22.sam"
  inp <- parse gram <$> readFile "input/day22.input"
  print ("day22, part1 (sam)", check 6032 $ part1 sam)
  print ("day22, part1", check 27492 $ part1 inp)
  print ("day22, part2 (sam)", check 5031 $ part2 samCube sam)
  print ("day22, part2", check 78291 $ part2 inpCube inp)

part1 :: Setup -> Int
part1 q = do
  let Setup {tiles,path} = q
  let w = initWorld mkEdgeMap1 tiles
  let pos0 = findInitPos w
  let s0 = (pos0,R)
  computePW (walkPath w s0 path)

mkEdgeMap1 :: Set Pos -> Map (Dir,Pos) (Dir,Pos)
mkEdgeMap1 valid = do
  let
    edge :: Dir -> [Pos]
    edge d =
      [ p
      | p <- Set.toList valid
      , let p' = stepDirect p d
      , not (p' `Set.member` valid)
      ]
  let u = zip (repeat U) $ sortBy (comparing fst) (edge U)
  let d = zip (repeat D) $ sortBy (comparing fst) (edge D)
  let l = zip (repeat L) $ sortBy (comparing snd) (edge L)
  let r = zip (repeat R) $ sortBy (comparing snd) (edge R)
  Map.fromList (zip u d ++ zip d u ++ zip l r ++ zip r l)

part2 :: CubeSpec -> Setup -> Int
part2 cubeSpec q = do
  let Setup {tiles,path} = q
  let w = initWorld (mkEdgeMap2 cubeSpec) tiles
  let pos0 = findInitPos w
  let s0 = (pos0,R)
  computePW (walkPath w s0 path)

data Face = F1 | F2 | F3 | F4 | F5 | F6

data CubeSpec = CubeSpec
  { faceLocations :: Face -> (Int,Int)
  , cutEdges :: [((Face,Dir),(Face,Dir))]
  }

{-
sample:
    ..1.
    234.
    ..56
-}
samCube :: CubeSpec
samCube = CubeSpec
  {
    faceLocations = \case
      F1 -> (2,0)
      F2 -> (0,1)
      F3 -> (1,1)
      F4 -> (2,1)
      F5 -> (2,2)
      F6 -> (3,2)
  ,
    cutEdges =
      [ ((F1,L),(F3,U))
      , ((F1,U),(F2,U))
      , ((F1,R),(F6,R))
      , ((F4,R),(F6,U))
      , ((F5,L),(F3,D))
      , ((F2,D),(F5,D))
      , ((F2,L),(F6,D))
      ]
  }

{-
  real:
    .12
    .3.
    45.
    6..
-}
inpCube :: CubeSpec
inpCube = CubeSpec
  {
    faceLocations = \case
      F1 -> (1,0)
      F2 -> (2,0)
      F3 -> (1,1)
      F4 -> (0,2)
      F5 -> (1,2)
      F6 -> (0,3)
  ,
    cutEdges =
      [ ((F1,U), (F6,L))
      , ((F1,L), (F4,L))
      , ((F3,L), (F4,U))
      , ((F2,R), (F5,R))
      , ((F2,D), (F3,R))
      , ((F2,U), (F6,D))
      , ((F6,R), (F5,D))
      ]
  }

mkEdgeMap2 :: CubeSpec -> Set Pos -> Map (Dir,Pos) (Dir,Pos)
mkEdgeMap2 CubeSpec{faceLocations,cutEdges} valid = do
  let mx = maximum [ x | (x,_) <- Set.toList valid ]
  let my = maximum [ y | (_,y) <- Set.toList valid ]
  let size = max mx my `div` 4
  let
    clock :: Int -> Dir -> Pos
    clock i = \case
      U -> (i,1)
      L -> (1,1+size-i)
      D -> (1+size-i,size)
      R -> (size,i)
  let
    anti :: Int -> Dir -> Pos
    anti i = \case
      U -> (1+size-i,1)
      R -> (size,1+size-i)
      D -> (i,size)
      L -> (1,i)
  let
    expand :: (Int -> Dir -> Pos) -> (Face,Dir) -> [(Dir,Pos)]
    expand spin (f,d) = do
      let (x0,y0) = faceLocations f
      [ (d,(size*x0+x,size*y0+y))
        | i <- [1..size]
        , let (x,y) = spin i d
        ]
  let
    bridge :: ((Face,Dir),(Face,Dir)) -> [((Dir,Pos),(Dir,Pos))]
    bridge (x,y) = do
      let xx = expand clock x
      let yy = expand anti y
      zip xx yy ++ zip yy xx

  Map.fromList (cutEdges >>= bridge)

type State = (Pos,Dir)

computePW :: (Pos,Dir) -> Int
computePW ((x,y),d) = 1000 * y + 4 * x +
  case d of R -> 0; D -> 1; L -> 2; U -> 3

walkPath :: World -> State -> Path -> State
walkPath w s (Path i tis) = foldl walkTI (walkI i s) tis
  where
    walkTI :: State -> (Turn,Int)-> State
    walkTI s (t,i) = walkI i (walkT t s)

    walkI :: Int -> State -> State
    walkI i = foldl (.) id (replicate i (step w))

walkT :: Turn -> State -> State
walkT t (p,d) = case t of
  TL -> (p, turnL d)
  TR -> (p, turnR d)

step :: World -> (Pos,Dir) -> (Pos,Dir)
step w s = do
  let s'@(pos',_) = stepUnchecked s
  if invalid w pos' then error (show ("invalid",s,"->",s')) else
    if walled w pos' then s else s'
  where
    stepUnchecked :: (Pos,Dir) -> (Pos,Dir)
    stepUnchecked s@(p,d) =
      case stepAcrossEdge w s of
        Nothing -> (stepDirect p d, d)
        Just pd -> pd

stepDirect :: Pos -> Dir -> Pos
stepDirect (x,y) = \case
  U -> (x,y-1)
  D -> (x,y+1)
  L -> (x-1,y)
  R -> (x+1,y)

turnL :: Dir -> Dir
turnL = \case U -> L; L -> D; D -> R; R -> U

turnR :: Dir -> Dir
turnR = \case U -> R; L -> U; D -> L; R -> D

data World = World
  { wall :: Set Pos
  , valid :: Set Pos
  , edgeMap :: EM
  } deriving Show

type EM = Map (Dir,Pos) (Dir,Pos)

initWorld :: (Set Pos -> EM) -> [[Tile]] -> World
initWorld mkEdgeMap tss = do
  let edgeMap = mkEdgeMap valid
  World { wall, valid, edgeMap }
  where
    valid = open `Set.union` wall
    wall = seek Wall
    open = seek Open
    seek :: Tile -> Set Pos
    seek sought = do
      Set.fromList
        [ (x,y) | (y,ts) <- zip [1..] tss
                , (x,t) <- zip [1..] ts
                , t == sought ]

findInitPos :: World -> Pos
findInitPos World{valid} =
  ( minimum [ x
            | (x,y) <- Set.toList valid
            , y == 1 ]
  , 1 )

stepAcrossEdge :: World -> (Pos,Dir) -> Maybe (Pos,Dir)
stepAcrossEdge World{edgeMap} (p,d) =
  case Map.lookup (d,p) edgeMap of
    Nothing -> Nothing
    Just (d',p') -> Just (p',opposite d')
      where
        opposite = turnL . turnL

walled :: World -> Pos -> Bool
walled World {wall} pos = pos `Set.member` wall

invalid :: World -> Pos -> Bool
invalid World {valid} pos = not (pos `Set.member` valid)

type Pos = (Int,Int)
data Dir = U | D | L | R deriving (Eq,Ord,Show)

data Setup = Setup { tiles:: [[Tile]], path :: Path } deriving Show
data Tile = Wall | Open | Missing deriving (Eq,Show)
data Path = Path Int [(Turn,Int)] deriving Show
data Turn = TL | TR deriving Show

gram :: Par Setup
gram = do
  tiles <- terminated nl line
  nl
  p <- path; nl
  pure Setup { tiles, path = p }
  where
    line :: Par [Tile]
    line = do
      t1 <- tile
      ts <- many tile
      pure (t1:ts)

    tile :: Par Tile
    tile = alts
      [ do lit ' '; pure Missing
      , do lit '.'; pure Open
      , do lit '#'; pure Wall
      ]

    path :: Par Path
    path = do
      i <- int
      tis <- many (do t <- turn; i <- int; pure (t,i))
      pure (Path i tis)

    turn :: Par Turn
    turn = alts
      [ do lit 'L'; pure TL
      , do lit 'R'; pure TR
      ]

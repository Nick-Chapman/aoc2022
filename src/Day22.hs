module Day22 (main) where

import Misc (check)
import Par4 (Par,parse,many,terminated,nl,alts,lit,int)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.List (sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day22.sam"
  inp <- parse gram <$> readFile "input/day22.input"
  print ("day22, part1 (sam)", check 6032 $ part1 sam)
  print ("day22, part1", check 27492 $ part1 inp)

part1 :: Setup -> Int
part1 q = do
  let Setup {tiles,path} = q
  let w = initWorld tiles
  let pos0 = findInitPos w
  let s0 = (pos0,R)
  let s = walkPath w s0 path
  pw s

type State = (Pos,Dir)

pw :: (Pos,Dir) -> Int
pw ((x,y),d) = 1000 * y + 4 * x +
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
  , edgeMap :: Map (Dir,Pos) (Dir,Pos)
  } deriving Show

initWorld :: [[Tile]] -> World
initWorld tss = do
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
  let edgeMap =
        Map.fromList (zip u d ++ zip d u ++ zip l r ++ zip r l)
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
    Just (_,p') -> Just (p',d)

walled :: World -> Pos -> Bool
walled World {wall} pos = pos `Set.member` wall

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

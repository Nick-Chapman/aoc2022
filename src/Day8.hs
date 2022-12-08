module Day8 (main) where

import Misc (check)
import Data.Map (Map)
import qualified Data.Map as Map (fromList,lookup)

main :: IO ()
main = do
  sam <- lines <$> readFile "input/day8.sam"
  inp <- lines <$> readFile "input/day8.input"
  print ("day8, part1 (sam)", check 21 $ part1 sam)
  print ("day8, part1", check 1681 $ part1 inp)
  print ("day8, part2 (sam)", check 8 $ part2 sam)
  print ("day8, part2", check 201684 $ part2 inp)

type Setup = [String]

type Pos = (Int,Int)
type Height = Char
data Dir = U | D | L | R deriving Show

part1 :: Setup -> Int
part1 css = length (filter visisble inner) + 2 * (width + depth - 2)
  where
    Grid {width,depth,height,inner,inBounds} = makeGrid css

    visisble :: Pos -> Bool
    visisble p = do
      any id [ all id [ height p' < height p | p' <- view d p ] | d <- [U,D,L,R] ]

    view :: Dir -> Pos -> [Pos]
    view d p = takeWhile inBounds (iterate (step d) (step d p))

part2 :: Setup -> Int
part2 css = maximum [ scenicScore p | p <- inner ]
  where
    Grid {height,inner,inBounds} = makeGrid css

    scenicScore :: Pos -> Int
    scenicScore p =
      product [ viewDist (height p) (step d p) d | d <- [U,D,L,R]]

    viewDist :: Height -> Pos -> Dir -> Int
    viewDist h p d =
      if not (inBounds p) then 0 else do
        if height p >= h then 1 else do
          1 + viewDist h (step d p) d

step :: Dir -> Pos -> Pos
step d (x,y) = case d of
  U -> (x,y-1)
  D -> (x,y+1)
  L -> (x-1,y)
  R -> (x+1,y)

data Grid = Grid
  { width :: Int
  , depth :: Int
  , height :: Pos -> Height
  , inner :: [Pos]
  , inBounds :: Pos -> Bool
  }

makeGrid :: [String] -> Grid
makeGrid css = Grid {width,depth,height,inner,inBounds}
  where
    width = length (head css)
    depth = length css
    m :: Map Pos Height =
      Map.fromList
      [ ((x,y),c)
      | (y,cs) <- zip [1::Int .. ] css
      , (x,c) <- zip [1::Int ..] cs
      ]
    inner = [ (x,y) | x <- [2..width-1], y <- [2..depth-1] ]
    height p = maybe (error (show p)) id $ Map.lookup p m
    inBounds (x,y) = x>0 && y>0 && x<= width && y <= depth

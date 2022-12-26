module Day23 (main) where

import Data.Maybe (maybeToList)
import Data.Set (Set)
import Misc (collate,check)
import Par4 (Par,parse,many,terminated,sat,nl)
import Prelude hiding (round,init)
import qualified Data.Set as Set

main :: IO ()
main = do
  --sam <- parse gram <$> readFile "input/day23.sam"
  inp <- parse gram <$> readFile "input/day23.input"
  --print ("day23, part1 (sam)", check 110 $ part1 sam)
  print ("day23, part1", check 4195 $ part1 inp)
  --print ("day23, part2 (sam)", check 20 $ part2 sam)
  print ("day23, part2", check 1069 $ part2 inp)

part1 :: Setup -> Int
part1 xs = do
  let
    loop :: Int -> State -> Int
    loop n s = do
      if n == 10 then extent s else do
        case round s of
          Nothing -> undefined
          Just s' -> loop (n+1) s'
  loop 0 (init xs)

part2 :: Setup -> Int
part2 xs = do
  let
    loop :: Int -> State -> Int
    loop n s = do
      case round s of
        Nothing -> n+1
        Just s' -> loop (n+1) s'
  loop 0 (init xs)

data State = State { elves :: Set Pos, dirs :: [Dir] }
type Pos = (Int,Int)
data Dir = N | S | W | E deriving Show

instance Show State where
  show State{elves,dirs} =
    show (elves,head dirs)

init :: [String] -> State
init css = do
  let elves = Set.fromList [ (x,y)
                           | (y,cs) <- zip [1..] css
                           , (x,c) <- zip [1..] cs
                           , c =='#'
                           ]
  let dirs =  [N,S,W,E] ++ dirs
  State { elves, dirs }

extent :: State -> Int
extent State{elves} = do
  let z = Set.size elves
  let xs = map fst (Set.toList elves)
  let ys = map snd (Set.toList elves)
  ((1 + maximum xs - minimum xs) * (1 + maximum ys - minimum ys)) - z

round :: State -> Maybe State
round s@State{elves,dirs} = do
  case proposals s of
    [] -> Nothing
    ps -> do
      let (old,new) = unzip ps
      let elves' =
            (elves `Set.difference` Set.fromList old)
            `Set.union` Set.fromList new
      Just $ State { elves = elves', dirs = tail dirs }

proposals :: State -> [(Pos,Pos)]
proposals s@State{elves} =
  [ (p,p')
  | (p',[p]) <- collate [ (p',p)
                        | p <- Set.toList elves
                        , p' <- maybeToList (proposal s p)
                        ]
  ]

proposal :: State -> Pos -> Maybe Pos
proposal s@State{dirs,elves} p =
  if not (any (`Set.member` elves) (eight p)) then Nothing else do
    case [ p' | d <- take 4 dirs , p' <- maybeToList (free s p d)] of
      [] -> Nothing
      a:_ -> Just a


eight :: Pos -> [Pos]
eight (x,y) = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y+1),
               (x,y+1), (x+1,y+1), (x+1,y), (x-1,y)]

free :: State -> Pos -> Dir -> Maybe Pos
free State{elves} p d = do
  let (p1,p2,p3) = looks p d
  if any (`Set.member` elves) [p1,p2,p3] then Nothing else Just p2

looks :: Pos -> Dir -> (Pos,Pos,Pos)
looks (x,y) = \case
  N -> ( (x-1,y-1), (x,y-1), (x+1,y-1) )
  S -> ( (x-1,y+1), (x,y+1), (x+1,y+1) )
  E -> ( (x+1,y-1), (x+1,y), (x+1,y+1) )
  W -> ( (x-1,y-1), (x-1,y), (x-1,y+1) )

type Setup = [String]

gram :: Par Setup
gram = terminated nl line
  where
    line = many dot
    dot = sat $ \c -> c /= '\n'

module Day16 (main) where

import Control.Monad.State (State,evalState,get,modify)
import Data.Map (Map)
import Data.Set (Set)
import Misc (check,look)
import Par4 (Par,parse,many,terminated,separated,sat,nl,key,int,alts)
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day16.sam"
  inp <- parse gram <$> readFile "input/day16.input"
  print ("day16, part1 (sam)", check 1651 $ part1 sam)
  print ("day16, part1", check 1741 $ part1 inp)
  print ("day16, part2 (sam)", check 1707 $ part2 sam)
  print ("day16, part2", check 2316 $ part2 inp) -- quite slow: 105sec
  where
    part1 = partX (step1,init1)
    part2 = partX (step2,init2)

type Setup = [Line]
data Line = Line Vid Int [Vid] deriving Show
type Vid = String

gram :: Par Setup
gram = terminated nl line
  where
    line = do
      key "Valve "
      x <- vid
      key " has flow rate="
      i <- int
      alts [key "; tunnels lead to valves "
           ,key "; tunnel leads to valve "]
      xs <- separated (key ", ") vid
      pure $ Line x i xs
    vid = many (sat Char.isAlpha)


type Node1 = (Vid,Int,Set Vid)

init1 :: Set Vid -> Node1
init1 nz = ("AA",30,nz)

step1 :: World -> Node1 -> [(Int,Node1)]
step1 World{flow,distance} (v1,n1,op1) =
  [ (value, (v2,n2,op2))
  | v2 <- Set.toList op1
  , let d = distance (v1,v2) + 1
  , let n2 = n1 - d
  , n2 > 0
  , let value = n2 * flow v2
  , let op2 = Set.delete v2 op1
  ]


type Node2 = (Vid,(Int,Vid),Int,Set Vid)

init2 :: Set Vid -> Node2
init2 nz = ("AA",(0,"AA"),26,nz)

step2 :: World -> Node2 -> [(Int,Node2)]
step2 World{flow,distance} (a,(q,b),n1,op1) =
  [ (value
    , case flip of
        False -> (v,(q-d,b),n1-d,op2)
        True ->  (b,(d-q,v),n1-q,op2)
    )
  | v <- Set.toList op1
  , let d = distance (a,v) + 1
  , let flip = d >= q
  , let n2 = n1 - d
  , (n2 > 0) -- || flip
  , let value = n2 * flow v
  , let op2 = Set.delete v op1
  ]


type Graph n = (World -> n -> [(Int,n)] , Set Vid -> n)

partX :: Ord n => Graph n -> Setup -> Int
partX (step,init) lines = do
  let nz = Set.fromList [ v | Line v flow _ <- lines, flow /= 0 ]
  let important = Set.fromList ["AA"] `Set.union` nz
  let a0 = Access $ Map.fromList [ ((v1,v2),1) | Line v1 _ vs <- lines, v2 <- vs ]
  let
    loop :: Int -> Access -> Access
    loop i a = do
      let a' = composeA a a `unionA` a
      if a' == a then a else loop (i+1) a'

  let a1 = loop 0 a0
  let a2 = Access $ Map.filterWithKey
        (\(s,d) _ ->
           s /= d &&
           (s `Set.member` important) &&
           (d `Set.member` important)
        ) (deAccess a1)

  let w = initWorld a2 lines
  explore (step w) (init nz)


data Access = Access { deAccess :: Map (Vid,Vid) Int } deriving (Eq,Show)

composeA :: Access -> Access -> Access
composeA (Access m1) (Access m2) = Access $ Map.fromList
  [ ((s1,d2),n1+n2) | ((s1,d1),n1) <- Map.toList m1
                    , ((s2,d2),n2) <- Map.toList m2, d1 == s2 ]

unionA :: Access -> Access -> Access
unionA (Access m1) (Access m2) = Access $ Map.unionWith min m1 m2


data World = World
  { flow :: Vid -> Int
  , distance :: (Vid,Vid) -> Int
  }

initWorld :: Access -> [Line] -> World
initWorld (Access aMap) lines  = do
  let
    flow :: Vid -> Int
    flow k = Misc.look k m
      where m = Map.fromList [ (v,flow) | Line v flow _ <- lines, flow /= 0 ]
  let
    distance :: (Vid,Vid) -> Int
    distance p = Misc.look p aMap
  World { flow, distance }


-- generic graph exploration, maximizing path-summed value
explore :: forall n. Ord n => (n -> [(Int,n)]) -> n -> Int
explore step init = evalState (search init) Map.empty
  where
    search :: n -> State (Map n Int) Int
    search n1 = do
      getCache n1 >>= \case
        Just res -> do pure res
        Nothing -> do
          xs <- sequence [ (+value) <$> search n2 | (value,n2) <- step n1 ]
          let res = maximum (0:xs)
          setCache n1 res
          pure res

    getCache n = Map.lookup n <$> get
    setCache n res = modify (Map.insert n res)

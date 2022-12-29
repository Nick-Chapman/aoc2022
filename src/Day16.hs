module Day16 (main) where

import Control.Monad (ap,liftM)
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
  res <- part1 sam
  print ("day16, part1 (sam)", check 1651 $ res)
  res <- part1 inp
  print ("day16, part1", check 1741 $ res)

  --res <- _part2 sam
  --print ("day16, part2 (sam)", check 1707 $ res)

  pure ()

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

part1 :: Setup -> IO Int
part1 lines = do
  --mapM_ print lines
  let nz = Set.fromList [ v | Line v flow _ <- lines, flow /= 0 ]
  let important = Set.fromList ["AA"] `Set.union` nz
  --print important
  let a0 = Access $ Map.fromList [ ((v1,v2),1) | Line v1 _ vs <- lines, v2 <- vs ]
  let
    loop :: Int -> Access -> IO Access
    loop i a = do
      --print i
      let a' = composeA a a `unionA` a
      if a' == a then pure a else loop (i+1) a'

  a1 <- loop 0 a0
  let a2 = Access $ Map.filterWithKey
        (\(s,d) _ ->
           s /= d &&
           (s `Set.member` important) &&
           (d `Set.member` important)
        ) (deAccess a1)

  pure $ searchTop1 a2 lines nz


data Access = Access { deAccess :: Map (Vid,Vid) Int } deriving (Eq,Show)

composeA :: Access -> Access -> Access
composeA (Access m1) (Access m2) = Access $ Map.fromList
  [ ((s1,d2),n1+n2) | ((s1,d1),n1) <- Map.toList m1
                    , ((s2,d2),n2) <- Map.toList m2, d1 == s2 ]

unionA :: Access -> Access -> Access
unionA (Access m1) (Access m2) = Access $ Map.unionWith min m1 m2

searchTop1 :: Access -> [Line] -> Set Vid -> Int
searchTop1 (Access aMap) lines nz = run (search ("AA",30,nz))
  where
    flow :: Vid -> Int
    flow k = Misc.look k m
      where m = Map.fromList [ (v,flow) | Line v flow _ <- lines, flow /= 0 ]

    distance :: (Vid,Vid) -> Int
    distance p = Misc.look p aMap

    search :: Key -> Eff Int
    search key@(v1,n,op) = do
      if n < 2 || Set.null op then pure 0 else do
        GetCache key >>= \case
          Just res -> do
            pure res
          Nothing -> do
            let as = [ (+ ((n-d-1) * flow v2)) <$> search (v2,n-d-1,Set.delete v2 op)
                     | v2 <- Set.toList op, v1 /= v2
                     , let d = distance (v1,v2)
                     ]
            let bs = [ search (v2,n-d,op)
                     | v2 <- Set.toList op, v1 /= v2
                     , let d = distance (v1,v2)
                     ]
            xs <- sequence (as++bs)
            let res = maximum (0:xs)
            SetCache key res
            pure res

data Eff x where -- search effect
  Ret :: x -> Eff x
  Bind :: Eff x -> (x -> Eff y) -> Eff y
  SetCache :: Key -> Int -> Eff ()
  GetCache :: Key -> Eff (Maybe Int)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

run :: Eff x -> x
run e = loop cache0 e $ \_cache a -> a
  where
    loop :: Cache -> Eff a -> (Cache -> a -> b) -> b
    loop cache e k = do
     case e of
      Ret x -> k cache x
      Bind e f -> loop cache e $ \cache a -> loop cache (f a) k
      SetCache key res -> k (Map.insert key res cache) ()
      GetCache key -> k cache (Map.lookup key cache)

type Key = (Vid,Int,Set Vid)

type Cache = Map Key Int

cache0 :: Cache
cache0 = Map.empty

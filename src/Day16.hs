module Day16 (main) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Data.Set (Set)
import Misc (check,look)
import Par4 (Par,parse,many,terminated,separated,sat,nl,key,int,alts)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day16.sam"
  inp <- parse gram <$> readFile "input/day16.input"
  res <- part1 sam
  print ("dayX, part1 (sam)", check 1651 $ res)
  res <- part1 inp
  print ("dayX, part1", check 1741 $ res) -- **NOT 1739**

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

    vid :: Par Vid
    vid = many (sat Char.isAlpha)

part1 :: Setup -> IO Int
part1 xs = do
  --mapM_ print (zip [1::Int ..] xs)
  let w = initWorld xs
  res <- run (searchTop w)
  pure res

data World = World
  { step :: Vid -> [Vid]
  , flow :: Vid -> Int
  }

initWorld :: Setup -> World
initWorld lines = do
  let m = Map.fromList [ (x,(i,xs)) | Line x i xs <- lines ]
  let flow x = fst (look x m)
  let step x = snd (look x m)
  World { flow, step }

type Key = (Vid,Int,Set Vid)

searchTop :: World -> Eff Int
searchTop World{step,flow} = search ("AA",30,Set.empty)
  where
    search :: Key -> Eff Int
    search key@(v,n,open) = do
      --Log ("search",key)
      if n < 2 then pure 0 else do
        GetCache key >>= \case
          Just res -> do
            pure res
          Nothing -> do
            let tryOpen = not (v `Set.member` open) && (flow v > 0)
            let released = (n-1) * flow v
            let as = [ (+ released) <$> search (v',n-2,Set.insert v open) | v' <- step v ]
            let bs = [ search (v',n-1,open) | v' <- step v ]
            xs <- sequence (if tryOpen then as ++ bs else bs)
            let res = maximum xs
            SetCache key res
            pure res

data Eff x where -- search effect
  Ret :: x -> Eff x
  Bind :: Eff x -> (x -> Eff y) -> Eff y
  Log :: Show a => a -> Eff ()
  SetCache :: Key -> Int -> Eff ()
  GetCache :: Key -> Eff (Maybe Int)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

run :: Eff x -> IO x
run e = loop Map.empty e $ \_s' a -> pure a
  where
    loop :: State -> Eff a -> (State -> a -> IO b) -> IO b
    loop s e k = do
     case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      Log a -> do putStrLn ("log: " ++ show a); k s ()
      SetCache key res -> k (Map.insert key res s) ()
      GetCache key -> k s (Map.lookup key s)

type State = Map Key Int

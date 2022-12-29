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

    vid :: Par Vid
    vid = many (sat Char.isAlpha)

data World = World
  { step :: Vid -> [Vid]
  , flow :: Vid -> Int
  , toOpen :: Set Vid
  }

initWorld :: Setup -> World
initWorld lines = do
  let m = Map.fromList [ (x,(i,xs)) | Line x i xs <- lines ]
  let flow x = fst (look x m)
  let step x = snd (look x m)
  let toOpen = Set.fromList [ x | Line x i _ <- lines, i > 0 ]
  World { flow, step, toOpen }

type Key = (Vid,Vid,Int,Set Vid)

part1 :: Setup -> IO Int
part1 xs = do
  let w = initWorld xs
  (res,_ticks) <- run (searchTop1 w)
  print ("ticks",_ticks)
  pure res

searchTop1 :: World -> Eff Int
searchTop1 World{step,flow,toOpen=toOpen0} = search ("*","AA",30,toOpen0)
  where
    search :: Key -> Eff Int
    search key@(ele,v,n,toOpen) = do
      Tick
      if n < 2 || Set.null toOpen then pure 0 else do
        GetCache key >>= \case
          Just res -> do
            pure res
          Nothing -> do
            let tryOpen = v `Set.member` toOpen
            let released = (n-1) * flow v
            let as = if tryOpen then [(+released) <$> search (ele,v,n-1,Set.delete v toOpen)] else []
            let bs = [ search (ele,v',n-1,toOpen) | v' <- step v ]
            xs <- sequence (as++bs)
            let res = maximum xs
            SetCache key res
            pure res

data Eff x where -- search effect
  Ret :: x -> Eff x
  Bind :: Eff x -> (x -> Eff y) -> Eff y
  Tick :: Eff ()
  Log :: Show a => a -> Eff ()
  SetCache :: Key -> Int -> Eff ()
  GetCache :: Key -> Eff (Maybe Int)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

run :: Eff x -> IO (x,Int)
run e = loop state0 e $ \State{ticks} a -> pure (a,ticks)
  where
    loop :: State -> Eff a -> (State -> a -> IO b) -> IO b
    loop s@State{cache,ticks} e k = do
     case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      Tick -> do
        --(if ticks `mod` 100000 == 0 then print (ticks `div` 100000) else pure ())
        k s { ticks = ticks + 1 } ()
      Log a -> do putStrLn ("log: " ++ show a); k s ()
      SetCache key res ->
        k s { cache = Map.insert key res cache } ()
      GetCache key ->
        k s (Map.lookup key cache)

data State = State { cache :: Map Key Int, ticks :: Int }

state0 :: State
state0 = State { cache = Map.empty, ticks = 0 }

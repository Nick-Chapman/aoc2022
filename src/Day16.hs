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
  _inp <- parse gram <$> readFile "input/day16.input"
  res <- part1 sam
  print ("day16, part1 (sam)", check 1651 $ res)

  res <- part1 _inp
  print ("day16, part1", check 1741 $ res) -- **NOT 1739**

  res <- part2 sam
  print ("day16, part2 (sam)", check 1707 $ res)

  --res <- part2 _inp -- TOO SLOW FOR THIS
  --print ("day16, part2", check 999 $ res)

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
  --print ("ticks",_ticks)
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


part2 :: Setup -> IO Int
part2 xs = do
  let w = initWorld xs
  (res,_ticks) <- run (searchTop2 w)
  --print ("ticks",_ticks)
  pure res

searchTop2 :: World -> Eff Int
searchTop2 w@World{toOpen=toOpen0} = search ("AA","AA",26,toOpen0)
  where
    search :: Key -> Eff Int
    search key@(aa,bb,n,toOpen) = do
     if bb < aa then search (bb,aa,n,toOpen) else do
      Tick
      --Log key
      if n < 2 || Set.null toOpen then pure 0 else do
        GetCache key >>= \case
          Just res -> do
            pure res
          Nothing -> do
            let bs = [ (q+) <$> search key' | (q,key') <- explore w key ]
            xs <- sequence bs
            let res = maximum xs
            SetCache key res
            pure res

explore :: World -> Key -> [(Int,Key)]
explore World{step,flow} (ele,me,n,toOpen) = do
  let qm = (n-1) * flow me
  let qe = (n-1) * flow ele
  if
    | me == ele -> do
        let tryOpen = ele `Set.member` toOpen
        if
          | tryOpen -> do -- ele will wlays be the one to do the open
              let as = [ (qe,(ele, me',n-1,Set.delete ele toOpen))
                       | me' <- step me
                       ]
              let bs = [ (0,(ele',me',n-1,toOpen))
                       | me' <- step me
                       , ele' <- step ele
                       ]
              (as ++ bs)
          | otherwise -> do
              let bs = [ (0,(ele',me',n-1,toOpen))
                       | me' <- step me
                       , ele' <- step ele
                       ]
              bs

    | otherwise -> do
        let meCanOpen = me `Set.member` toOpen
        let eleCanOpen = ele `Set.member` toOpen

        case (meCanOpen,eleCanOpen) of
          (False,False) -> do
              let bs = [ (0,(ele',me',n-1,toOpen)) -- we both step
                       | me' <- step me
                       , ele' <- step ele
                       ]
              bs

          (False,True) -> do
              let as = [ (qe,(ele,me',n-1,Set.delete ele toOpen)) -- ele opens; I step
                       | me' <- step me
                       ]
              let bs = [ (0,(ele',me',n-1,toOpen)) -- we both step
                       | me' <- step me
                       , ele' <- step ele
                       ]
              (as ++ bs)

          (True,False) -> do
              let as = [ (qm,(ele',me,n-1,Set.delete me toOpen)) -- I open; ele steps
                       | ele' <- step ele
                       ]
              let bs = [ (0,(ele',me',n-1,toOpen)) -- we both step
                       | me' <- step me
                       , ele' <- step ele
                       ]
              (as ++ bs)


          (True,True) -> do
              let as = [ (qe+qm,(ele,me,n-1,Set.delete ele (Set.delete me toOpen))) ] -- both open
              let bs = [ (qe,(ele,me',n-1,Set.delete ele toOpen)) -- ele opens; I step
                       | me' <- step me
                       ]
              let cs = [ (qm,(ele',me,n-1,Set.delete me toOpen)) -- I open; ele steps
                       | ele' <- step ele
                       ]
              let ds = [ (0,(ele',me',n-1,toOpen)) -- we both step
                       | me' <- step me
                       , ele' <- step ele
                       ]
              (as ++ bs ++ cs ++ ds)




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

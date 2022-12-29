module Day16 (main) where

--import Control.Monad (ap,liftM)
--import Data.Map (Map)
--import Data.Set (Set)
import Misc --(check,look)
import Par4 (Par,parse,many,terminated,separated,sat,nl,key,int,alts)
import qualified Data.Char as Char
--import qualified Data.Map.Strict as Map
--import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day16.sam"
  _inp <- parse gram <$> readFile "input/day16.input"
  res <- part1 sam
  print ("day16, part1 (sam)", check 1651 $ res)
  --res <- part1 _inp
  --print ("day16, part1", check 1741 $ res)
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
part1 xs = do
  mapM_ print xs
  pure (length xs)

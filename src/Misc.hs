
module Misc (check,readInts,collate,look) where

import Data.Map (Map)
import qualified Data.Map as Map

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

readInts :: FilePath -> IO [Int]
readInts path = do
  str <- readFile path
  pure $ map read (words str)

collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])

look :: (Ord k, Show k) => k -> Map k v -> v
look k m = maybe (error (show ("look",k))) id $ Map.lookup k m

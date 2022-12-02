
module Misc (check,readInts,collate,look,splitOn,theHead) where

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

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimter xs = loop [] xs
  where
    loop acc = \case
      [] -> [reverse acc]
      x:xs ->
        if x == delimter
        then reverse acc : loop [] xs
        else loop (x:acc) xs

theHead :: [a] -> a
theHead = \case [x] -> x; _ -> undefined

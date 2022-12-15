
-- sets of integers, implemented as ascending lists of ranges
module RangeISet
  ( Set
  , empty
  , fromRange
  , size
  , union
  , removeElem
  , invert
  , toList
  ) where

data Set = Set [Ran] deriving Show
type Ran = (Int,Int)

empty :: Set
empty = Set []

fromRange :: (Int,Int) -> Set
fromRange (l,r) = if r < l then error "fromRange" else Set [(l,r)]

union :: Set -> Set -> Set
union set (Set rs) = foldr add_range set rs
  where
    add_range :: Ran -> Set -> Set
    add_range (a,b) (Set xs) = case xs of
      [] -> fromRange (a,b)
      (c,d):xs ->
        if
          | b < c-1 -> do Set ((a,b) : (c,d) : xs)
          | a > d+1 -> do
              let Set xs' = add_range (a,b) (Set xs)
              Set ((c,d) : xs')
          | otherwise -> do
              add_range (min a c, max b d) (Set xs)

removeElem :: Set -> Int -> Set
removeElem set@(Set xs) e =
  case xs of
    [] -> set
    (a,b):xs -> if
      | e < a -> set
      | a <= e && e <= b -> pushRan (a,e-1) $ pushRan (e+1,b) $ Set xs
      | otherwise -> do
          let Set xs' = removeElem (Set xs) e
          Set ((a,b) : xs')

invert :: (Int,Int) -> Set -> Set
invert (a,b) (Set xs) = do
  let (lo,hi) = unzip xs
  let rs = zip ([a] ++ map (+1) hi) (map (\x -> x-1) lo ++ [b])
  foldr pushRan empty rs

pushRan :: (Int,Int) -> Set -> Set
pushRan (a,b) set@(Set xs) = if a>b then set else Set ((a,b):xs)

size :: Set -> Int
size (Set rs) = sum (map (\(l,r) -> 1+r-l) rs)

toList :: Set -> [Int]
toList (Set xs) = xs >>= \(l,r) -> [l..r]

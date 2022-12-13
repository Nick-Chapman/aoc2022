module Day13 (main) where

import Data.List (sort)
import Misc (check,the)
import Par4 (Par,parse,separated,nl,alts,int,lit,opt)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day13.sam"
  inp <- parse gram <$> readFile "input/day13.input"
  print ("day13, part1 (sam)", check 13 $ sum (part1 sam))
  print ("day13, part1", check 6240 $ sum (part1 inp))
  print ("day13, part2 (sam)", check 140 $ part2 sam)
  print ("day13, part2", check 23142 $ part2 inp)

part1 :: Setup -> [Int]
part1 pps = [ i | (i,pp) <- zip [1..] pps, cmp pp == LT ]

part2 :: Setup -> Int
part2 pps = do
  let m2 = L [L [I 2]]
  let m6 = L [L [I 6]]
  let ps = sort ([m2,m6] ++ [ p | (p1,p2) <- pps, p <- [p1,p2] ])
  let x1 = the [ i | (i,p) <- zip [1..] ps, p == m2 ]
  let x2 = the [ i | (i,p) <- zip [1..] ps, p == m6 ]
  x1 * x2

instance Ord Packet where
  compare x y = cmp (x,y)

instance Eq Packet where
  (==) x y = cmp (x,y) == EQ

cmp :: PP -> Ordering
cmp = \case
  (I x, I y ) -> if x < y then LT else if x==y then EQ else GT
  (L xs,L ys) -> cmpL (xs,ys)
  (I x, L ys) -> cmpL ([I x],ys)
  (L xs,I y ) -> cmpL (xs,[I y])

cmpL :: ([Packet],[Packet]) -> Ordering
cmpL = \case
  ([],[]) -> EQ
  ([],_:_) -> LT
  (_:_,[]) -> GT
  (x:xs,y:ys) ->
    case cmp (x,y) of
      LT -> LT
      EQ -> cmpL (xs,ys)
      GT -> GT

type Setup = [PP]
type PP = (Packet,Packet)
data Packet = I Int | L [Packet] deriving Show

gram :: Par Setup
gram = separated nl pp
  where
    pp = do p1 <- packet; nl; p2 <- packet; nl; pure (p1,p2)
    packet = alts [ I <$> int, L <$> list ]
    list = do
      lit '['
      ps <- maybe [] id <$> opt (separated (lit ',') packet)
      lit ']'
      pure ps

module Day11 (main) where

import Misc (check,look)
import Par4 (Par,parse,separated,nl,key,lit,alts,int,char)

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List  as List

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day11.sam"
  inp <- parse gram <$> readFile "input/day11.input"

  print ("day11, part1 (sam)", check 10605 $ part1 sam)
  print ("day11, part1", check 108240 $ part1 inp)

  resS2 <- part2 sam
  print ("day11, part1 (sam)", check 2713310158 $ resS2)
  resI2 <- part2 inp
  print ("day11, part1 (sam)", check 25712998901 $ resI2)

----------------------------------------------------------------------

type Setup = [(MonkeyId,Desc)]
data Desc = Desc [Int] Op Decide deriving Show
data Op = Add Int | Mul Int | Square deriving Show
data Decide = Decide Int MonkeyId MonkeyId deriving Show
type MonkeyId = Char

gram :: Par Setup
gram = separated nl monkey
  where
    monkey :: Par (MonkeyId,Desc)
    monkey = do
      key "Monkey "
      m <- mid
      lit ':'
      nl
      key "  Starting items: "
      xs <- separated (key ", ") int
      nl
      o <- op
      d <- decide
      pure (m, Desc xs o d)

    op :: Par Op
    op = do
      key "  Operation: new = "
      e <- exp
      nl
      pure e

    exp :: Par Op
    exp = alts
      [ do key "old + "; i <- int; pure (Add i)
      , do
          key "old * "
          alts [ do i <- int; pure (Mul i)
               , do key "old"; pure Square
               ]
      ]

    decide :: Par Decide
    decide = do
      key "  Test: divisible by "
      d <- int
      nl
      key "    If true: throw to monkey "
      a <- mid
      nl
      key "    If false: throw to monkey "
      b <- mid
      nl
      pure (Decide d a b)

    mid :: Par MonkeyId
    mid = char

----------------------------------------------------------------------

data State1 = State1
  { hold :: Map MonkeyId [Int]
  , count :: Map MonkeyId Int
  }

instance Show State1 where show State1{count} = show count

initState1 :: Setup -> State1
initState1 q = do
  let xs = [ (k,ws) | (k,Desc ws _ _) <- q ]
  State1 { hold = Map.fromList xs, count = Map.empty }

part1 :: Setup -> Int
part1 q = do
  let ss = iterate round (initState1 q)
  let s' = head (drop 20 ss)
  let State1{count} = s'
  let [a,b] = take 2 $ reverse (List.sort (map snd (Map.toList count)))
  (a*b)
  where
    round :: State1 -> State1
    round s0 = foldl (flip turn) s0 (map fst q)

    turn :: MonkeyId -> State1 -> State1
    turn k = loop
      where
        loop :: State1 -> State1
        loop s0 = do
          let (opt,s) = inspect k s0
          case opt of
            Nothing -> s
            Just w -> do
              let w' = worry k w
              let k' = decide k w'
              loop (chuckTo k' w' s)

    odm :: Map MonkeyId (Op,Decide)
    odm = Map.fromList [ (k,(o,d)) | (k,Desc _ o d) <- q ]

    worry :: MonkeyId -> Int -> Int
    worry k w = do
      let (op,_) = look k odm
      apply w op `div` 3

    apply :: Int -> Op -> Int
    apply w = \case
      Add i -> w + i
      Mul i -> w * i
      Square -> w * w

    decide :: MonkeyId -> Int -> MonkeyId
    decide k w = do
      let (_,Decide d k1 k2) = look k odm
      if w `mod` d == 0 then k1 else k2

    inspect :: MonkeyId -> State1 -> (Maybe Int, State1)
    inspect k s0@State1{hold,count} = do
      let ws = look k hold
      case ws of
        [] -> (Nothing, s0)
        w:ws -> do
          let s :: State1 = s0 { count = Map.insertWith (+) k 1 count }
          (Just w, s { hold = Map.insert k ws hold })

    chuckTo :: MonkeyId -> Int -> State1 -> State1
    chuckTo k w s@State1{hold} = do
      let ws = look k hold
      s { hold = Map.insert k (ws++[w]) hold }


----------------------------------------------------------------------

data State2 = State2
  { hold :: Map MonkeyId [Inty]
  , count :: Map MonkeyId Int
  }

instance Show State2 where show State2{count} = show count

initState2 :: Setup -> State2
initState2 q = do
  let xs = [ (k,map ofInt ws) | (k,Desc ws _ _) <- q ]
  State2 { hold = Map.fromList xs, count = Map.empty }

part2 :: Setup -> IO Int
part2 q = do
  let
    loop :: Int -> State2 -> IO State2
    loop i s = do
      if (i `mod` 1000 == 0) then print (i,s) else pure ()
      if (i == 10000) then return s else loop (i+1) (round s)
  s' <- loop 0 (initState2 q)
  let State2{count} = s'
  let [a,b] = take 2 $ reverse (List.sort (map snd (Map.toList count)))
  pure (a * b)

  where
    round :: State2 -> State2
    round s0 = foldl (flip turn) s0 (map fst q)

    turn :: MonkeyId -> State2 -> State2
    turn k = loop
      where
        loop :: State2 -> State2
        loop s0 = do
          let (opt,s) = inspect k s0
          case opt of
            Nothing -> s
            Just w -> do
              let w' = worry k w
              let k' = decide k w'
              loop (chuckTo k' w' s)

    odm :: Map MonkeyId (Op,Decide)
    odm = Map.fromList [ (k,(o,d)) | (k,Desc _ o d) <- q ]

    worry :: MonkeyId -> Inty -> Inty
    worry k w = do
      let (op,_) = look k odm
      apply w op -- `div` 3

    apply :: Inty -> Op -> Inty
    apply w = \case
      Add i -> w `add` i
      Mul i -> w `mul` i
      Square -> square w

    decide :: MonkeyId -> Inty -> MonkeyId
    decide k w = do
      let (_,Decide d k1 k2) = look k odm
      if w `divisible` d then k1 else k2

    inspect :: MonkeyId -> State2 -> (Maybe Inty, State2)
    inspect k s0@State2{hold,count} = do
      let ws = look k hold
      case ws of
        [] -> (Nothing, s0)
        w:ws -> do
          let s :: State2 = s0 { count = Map.insertWith (+) k 1 count }
          (Just w, s { hold = Map.insert k ws hold })

    chuckTo :: MonkeyId -> Inty -> State2 -> State2
    chuckTo k w s@State2{hold} = do
      let ws = look k hold
      s { hold = Map.insert k (ws++[w]) hold }

----------------------------------------------------------------------
{-
data Inty = Inty Int deriving (Show)

ofInt :: Int -> Inty
ofInt a = Inty a

divisible :: Inty -> Int -> Bool
divisible (Inty a) b = a `mod` b == 0

add :: Inty -> Int -> Inty
add (Inty a) b = Inty (a+b)

mul :: Inty -> Int -> Inty
mul (Inty a) b = Inty (a*b)

square :: Inty -> Inty
square (Inty a) = Inty (a*a)
-}

data Inty
  = B Int
  | A Inty Int
  | M Inty Int
  | S Inty
  deriving Show

ofInt :: Int -> Inty
ofInt = B

add :: Inty -> Int -> Inty
add = A

mul :: Inty -> Int -> Inty
mul = M

square :: Inty -> Inty
square = S

divisible :: Inty -> Int -> Bool
divisible i d = i `modi` d == 0

modi :: Inty -> Int -> Int
modi i d = case i of
  B x -> x `mod` d
  S i -> let r = i `modi` d in (r*r) `mod` d
  M i x -> let r = i `modi` d in (r*x) `mod` d
  A i x -> let r = i `modi` d in (r+x) `mod` d



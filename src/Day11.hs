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
  resS <- part1 sam
  print ("day11, part1 (sam)", check 10605 $ resS)
  resI <- part1 inp
  print ("day11, part1", check 108240 $ resI)
  pure ()

data State = State
  { hold :: Map MonkeyId [Int]
  , count :: Map MonkeyId Int
  } deriving Show

initState :: Setup -> State
initState q = do
  let xs = [ (k,ws) | (k,Desc ws _ _) <- q ]
  State { hold = Map.fromList xs, count = Map.empty }


part1 :: Setup -> IO Int
part1 q = do
  let ss = iterate round (initState q)
  let s' = head (drop 20 ss)
  let State{count} = s'
  let [a,b] = take 2 $ reverse (List.sort (map snd (Map.toList count)))
  pure (a*b)
  where
    round :: State -> State
    round s0 = foldl (flip turn) s0 (map fst q)

    turn :: MonkeyId -> State -> State
    turn k = loop
      where
        loop :: State -> State
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
      Add i -> w+i
      Mul i -> w*i
      Square -> w*w

    decide :: MonkeyId -> Int -> MonkeyId
    decide k w = do
      let (_,Decide d k1 k2) = look k odm
      if w `mod` d == 0 then k1 else k2


inspect :: MonkeyId -> State -> (Maybe Int, State)
inspect k s0@State{hold,count} = do
  let ws = look k hold
  case ws of
    [] -> (Nothing, s0)
    w:ws -> do
      let s = s0 { count = Map.insertWith (+) k 1 count }
      (Just w, s { hold = Map.insert k ws hold })

chuckTo :: MonkeyId -> Int -> State -> State
chuckTo k w s@State{hold} = do
  let ws = look k hold
  s { hold = Map.insert k (ws++[w]) hold }

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

module Day21 (main) where

import Misc (check,look,the)
import Par4 (Par,parse,many,terminated,sat,nl,key,alts,int)
import qualified Data.Char as Char
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day21.sam"
  inp <- parse gram <$> readFile "input/day21.input"
  print ("day21, part1 (sam)", check 152 $ part1 sam)
  print ("day21, part1", check 104272990112064 $ part1 inp)

  resS <- part2 sam
  print ("day21, part2 (sam)", check 301 $ resS)

  res <- part2_slow (resS-200) sam
  print ("day21, part2 (sam)", check 301 $ res)

  resI <- part2 inp
  --print ("day21, part2", check (-928028391) $ res) -- WRONG
  print ("day21, part2", check 165569196 $ resI) -- NOT TRIED, BUT WRONG

  res <- part2_slow resI inp
  print ("day21, part2", check 0 $ res)


part2 :: Setup -> IO Int
part2 lines = do
  let
    (ra,rb) =
      the [ (a,b) | Line { dest = "root" , exp = Bin Add a b } <- lines ]

    m = Map.fromList [ (dest,eval exp) | Line { dest, exp } <- lines ]

    eval :: Exp -> V
    eval = \case
      Num n -> numV n
      Bin op a b -> evalO op (evalN a) (evalN b)

    evalO :: Op -> V -> V -> V
    evalO = \case
      Add -> addV
      Sub -> subV
      Mul -> mulV
      Div -> divV

    evalN :: Name -> V
    evalN = \case
      "humn" -> V { h = 1, k = 0, d = 1 }
      x -> look x m

  let v1 :: V = evalN ra
  let v2 :: V = evalN rb
  print v1
  print v2
  let res = equateV v1 v2
  print ("res",res)
  pure res

equateV :: V -> V -> Int
equateV (V h1 k1 d1) (V 0 k2 1) = (d1*k2 - k1) `div` h1
equateV v1 v2 = error (show ("equate",v1,v2))

numV :: Int -> V
addV,subV,mulV,divV :: V -> V -> V

data V = V { h :: Int, k :: Int, d :: Int } deriving Show
numV k = V { h = 0, k, d = 1 }

addV (V h1 k1 d1) (V h2 k2 d2) = mkV (h1*d2+h2*d1) (k1*d2+k2*d1) (d1*d2)
subV (V h1 k1 d1) (V h2 k2 d2) = mkV (h1*d2-h2*d1) (k1*d2-k2*d1) (d1*d2)

--mulV (V h1 k1 d1) (V 0 k2 1) = mkV (d1*k2*h1) (d1*k2*k1) d1
--mulV (V 0 k1 1) (V h2 k2 d2) = mkV (d2*k1*h2) (d2*k1*k2) d2
mulV (V h1 k1 d1) (V 0 k2 1) = mkV (k2*h1) (k2*k1) d1
mulV (V 0 k1 1) (V h2 k2 d2) = mkV (k1*h2) (k1*k2) d2
mulV v1 v2 = error (show ("mul",v1,v2))

--divV (V 0 k1 d1) (V 0 k2 1) = V 0 k1 (d1*k2)

divV (V h1 k1 d1) (V 0 k2 1) = mkV h1 k1 (d1*k2)
--divV (V h1 k1 1) (V 0 k2 1) = mkV h1 k1 k2
divV v1 v2 = error (show ("div",v1,v2))

mkV :: Int -> Int -> Int -> V
mkV 0 k d | d /= 0 = V 0 (k `div` d) 1
mkV h k d = V h k d

{-

--addV (V h1 k1 1) (V h2 k2 1) = V (h1+h2) (k1+k2) 1
--addV (V 0 k1 1) (V h2 k2 d2) = V h2 (k2+k1*d2) d2
--addV (V h1 k1 d1) (V 0 k2 1) = V h1 (k1+k2*d1) d1

--addV (V 0 k1 d1) (V 0 k2 d2) = V 0 (k1*d2+d1*k2) (d1*d2)
--addV (V 0 k1 d1) (V 0 k2 1) = V 0 (k1+d1*k2) d1  -- subsumed
--addV (V 0 k1 1) (V 0 k2 d2) = V 0 (k2+d2*k1) d2  -- subsumed

--addV v1 v2 = error (show ("add",v1,v2))

--subV (V h1 k1 d1) (V 0 k2 d2) = V (h1*d2) (k1*d2-d1*k2) (d1*d2) --subsumbed
--subV (V 0 k1 d1) (V 0 k2 d2) = V 0 (k1*d2-d1*k2) (d1*d2) --subsumed
--subV (V 0 k1 d1) (V 0 k2 1) = V 0 (k1-d1*k2) d1 -- subsumed

--subV (V h1 k1 1) (V h2 k2 1) = V (h1-h2) (k1-k2) 1
--subV (V h1 k1 d1) (V 0 k2 1) = V h1 (k1-k2*d1) d1
--subV (V 0 k1 1) (V h2 k2 d2) = V (-h2) (k2-k1*d2) d2
--subV v1 v2 = error (show ("sub",v1,v2))


--mulV (V 0 k1 1) (V h2 k2 1) = V (k1*h2) (k1*k2) 1 --subsumed

--mulV (V 0 k1 1) (V h2 k2 d2) = V (k1*h2) (k1*k2) d2
--mulV (V h1 k1 d1) (V 0 k2 1) = V (h1*k2) (k1*k2) d1

--divV (V 0 k1 1) (V 0 k2 1) = V 0 (k1 `div` k2) 1
--divV (V h1 k1 d1) (V 0 k2 1) = V h1 k1 (d1*k2)

--divV (V h1 k1 1) (V 0 k2 1) = V h1 k1 k2 --subsumed

-}

part1 :: Setup -> Int
part1 lines = do
  let
    m = Map.fromList [ (dest,eval exp) | Line { dest, exp } <- lines ]

    eval :: Exp -> Int
    eval = \case
      Num n -> n
      Bin op a b -> evalO op (evalN a) (evalN b)

    evalO :: Op -> Int -> Int -> Int
    evalO = \case
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
      Div -> div

    evalN :: Name -> Int
    evalN x = look x m

  evalN "root"

part2_slow :: Int -> Setup -> IO Int
part2_slow clue lines = do

  let
    (ra,rb) =
      the [ (a,b) | Line { dest = "root" , exp = Bin Add a b } <- lines ]

    test :: Int -> Bool
    test humn = do
      let
        m = Map.fromList [ (dest,eval exp) | Line { dest, exp } <- lines ]

        eval :: Exp -> Int
        eval = \case
          Num n -> n
          Bin op a b -> evalO op (evalN a) (evalN b)

        evalO :: Op -> Int -> Int -> Int
        evalO = \case
          Add -> (+)
          Sub -> (-)
          Mul -> (*)
          Div -> div

        evalN :: Name -> Int
        evalN = \case
          "humn" -> humn
          x -> look x m

      evalN ra == evalN rb

  let
    loop offset = do
      if offset `mod` 100 == 0 then print (clue+offset,clue-offset) else pure ()
      if test (clue+offset) then pure (clue+offset) else
        if test (clue-offset) then pure (clue-offset) else
          loop (offset+1)

  --pure $ head [ n | n <- [clue..], test n ]
  loop 0


type Setup = [Line]
data Line = Line { dest :: Name, exp :: Exp } deriving Show
data Exp = Num Int | Bin Op Name Name deriving Show
data Op = Add | Mul | Sub | Div deriving Show
type Name = String

gram :: Par Setup
gram = terminated nl line
  where
    line = do
      dest <- name
      key ": "
      e <- exp
      pure Line { dest, exp = e }

    exp :: Par Exp
    exp = alts
      [ Num <$> int
      , do a <- name; alts
             [ do key " + "; b <- name; pure $ Bin Add a b
             , do key " - "; b <- name; pure $ Bin Sub a b
             , do key " * "; b <- name; pure $ Bin Mul a b
             , do key " / "; b <- name; pure $ Bin Div a b
             ]
      ]
    name = many (sat Char.isAlpha)

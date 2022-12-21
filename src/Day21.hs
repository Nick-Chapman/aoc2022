module Day21 (main) where

import Misc (check,look)
import Par4 (Par,parse,many,terminated,sat,nl,key,alts,int)
import qualified Data.Char as Char
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day21.sam"
  inp <- parse gram <$> readFile "input/day21.input"
  print ("day21, part1 (sam)", check 152 $ part1 sam)
  print ("day21, part1", check 104272990112064 $ part1 inp)

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

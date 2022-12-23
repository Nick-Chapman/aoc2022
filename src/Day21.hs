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
  print ("day21, part2 (sam)", check 301 $ part2 sam)
  print ("day21, part2", check 3220993874133 $ part2 inp)

type Setup = [Def]
data Def = Def { dest :: Name, exp :: Exp } deriving Show
data Exp = Num Int | Bin Op Arg Arg deriving Show
data Op = Add | Mul | Sub | Div deriving Show
data Arg = AN Name | AI Int deriving Show
type Name = String

gram :: Par Setup
gram = terminated nl line
  where
    line = do
      dest <- name
      key ": "
      e <- exp
      pure Def { dest, exp = e }

    exp :: Par Exp
    exp = alts
      [ Num <$> int
      , do a <- arg; alts
             [ do key " + "; b <- arg; pure $ Bin Add a b
             , do key " - "; b <- arg; pure $ Bin Sub a b
             , do key " * "; b <- arg; pure $ Bin Mul a b
             , do key " / "; b <- arg; pure $ Bin Div a b
             ]
      ]
    arg = AN <$> name
    name = many (sat Char.isAlpha)


part1 :: Setup -> Int
part1 = part1g "root"

part1g :: Name -> Setup -> Int
part1g root lines = do
  let
    m = Map.fromList [ (dest,eval exp) | Def { dest, exp } <- lines ]

    eval :: Exp -> Int
    eval = \case
      Num n -> n
      Bin op a b -> evalO op (evalN a) (evalN b)

    evalO :: Op -> Int -> Int -> Int
    evalO = \case
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
      Div -> divPerfect

    evalN :: Arg -> Int
    evalN = \case
      AN x -> look x m
      AI i -> i

  evalN (AN root)

divPerfect :: Int -> Int -> Int
divPerfect a b =
  if a `mod` b /= 0 then error (show ("divPerfect",a,b)) else
    a `div` b

part2 :: Setup -> Int
part2 lines = do
  let
    (ra,rb) =
      the [ (a,b) | Def { dest = "root" , exp = Bin Add a b } <- lines ]

    m = Map.fromList [ (dest,eval dest exp) | Def { dest, exp } <- lines ]

    evalN :: Arg -> V
    evalN = \case
      AN "humn" -> C "humn" []
      AN x -> look x m
      AI{} -> undefined

    eval :: Name -> Exp -> V
    eval dest = \case
      Num i -> I i
      Bin op a b -> evalO op (evalN a) (evalN b)

      where
        evalO :: Op -> V -> V -> V
        evalO = \case
          Add -> add
          Sub -> sub
          Mul -> mul
          Div -> divide

        add,sub,mul,divide :: V -> V -> V

        add (I i1) (I i2) = I(i1+i2)
        add (C x ds) (I i) = b ds x (Bin Sub (AN dest) (AI i))
        add (I i) (C x ds) = b ds x (Bin Sub (AN dest) (AI i))
        add _ _ = undefined

        sub (I i1) (I i2) = I (i1-i2)
        sub (C x ds) (I i) = b ds x (Bin Add (AN dest) (AI i))
        sub (I i) (C x ds) = b ds x (Bin Sub (AI i) (AN dest))
        sub _ _ = undefined

        mul (I i1) (I i2) = I (i1*i2)
        mul (C x ds) (I i) = b ds x (Bin Div (AN dest) (AI i))
        mul (I i) (C x ds) = b ds x (Bin Div (AN dest) (AI i))
        mul _ _ = undefined

        divide (I i1) (I i2) = I (i1 `divPerfect` i2)
        divide (C x ds) (I i) = b ds x (Bin Mul (AN dest) (AI i))
        divide (I _) (C _ _) = undefined
        divide _ _ = undefined

        b ds x e = C dest ([Def x e] ++ ds)

    equate :: V -> V -> [Def]
    equate (C x ds) (I i) = [Def x (Bin Add (AI 0) (AI i))] ++ ds
    equate v1 v2 = error (show ("equate",v1,v2))

  let v1 = evalN ra
  let v2 = evalN rb
  part1g "humn" (equate v1 v2)

data V = I Int | C Name [Def] deriving Show

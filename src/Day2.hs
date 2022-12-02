module Day2 (main) where

import Misc (check)
import Par4 (Par,parse,terminated,alts,nl,lit)

main :: IO ()
main = do
  inp <- parse gram <$> readFile "input/day2.input"
  print ("day2, part1", check 11475 $ part1 inp)
  print ("day2, part2", check 16862 $ part2 inp)

gram :: Par Setup
gram = terminated nl line
  where
    i c v = do lit c; pure v
    line = do
      them <- alts [ i 'A' A, i 'B' B, i 'C' C ]
      lit ' '
      me <- alts [ i 'X' X, i 'Y' Y, i 'Z' Z ]
      pure (them,me)

type Setup = [(Them,Me)]

data Them    = A | B | C
data Me      = X | Y | Z
data Go      = R | P | S
data Outcome = W | D | L deriving Eq

part1 :: Setup -> Int
part1 xs =
  sum [ scoreOutcome (outcomeForPlayer2 p1 p2) + scoreGo p2
      | (them,me) <- xs
      , let p1 = goThem them
      , let p2 = goMe me
      ]

part2 :: Setup -> Int
part2 xs =
  sum [ scoreOutcome outcome + scoreGo p2
      | (them,me) <- xs
      , let p1 = goThem them
      , let outcome = outcomeMe me
      , let p2 = makeOutcome p1 outcome
      ]

goThem :: Them -> Go
goThem = \case A -> R; B -> P; C -> S

goMe :: Me -> Go
goMe = \case X -> R; Y -> P; Z -> S

outcomeMe :: Me -> Outcome
outcomeMe = \case X -> L; Y -> D; Z -> W

scoreGo :: Go -> Int
scoreGo = \case R -> 1; P -> 2; S -> 3

scoreOutcome :: Outcome -> Int
scoreOutcome = \case W -> 6; D -> 3; L -> 0

outcomeForPlayer2 :: Go -> Go -> Outcome
outcomeForPlayer2 = x
  where
    x R R = D; x R P = W; x R S = L;
    x P R = L; x P P = D; x P S = W;
    x S R = W; x S P = L; x S S = D;

makeOutcome :: Go -> Outcome -> Go
makeOutcome p1 outcome =
  the_head [ p2 | p2 <- [R,P,S], outcomeForPlayer2 p1 p2 == outcome ]
  where
    the_head = \case [x] -> x; _ -> undefined

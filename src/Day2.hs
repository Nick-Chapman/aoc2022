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
    line = do them <- abc; lit ' '; me <- def; pure (them,me)
    abc = alts [ do lit 'A'; pure A
               , do lit 'B'; pure B
               , do lit 'C'; pure C ]
    def = alts [ do lit 'X'; pure X
               , do lit 'Y'; pure Y
               , do lit 'Z'; pure Z ]

type Setup = [Round]
type Round = (Them,Me)

data Them =  A | B | C deriving (Eq,Show) -- Rock,Paper,Sci
data Me   =  X | Y | Z deriving (Eq,Show) -- Rock,Paper,Sci

part1 :: Setup -> Int
part1 xs = sum [ game (t,m) + score m | (t,m) <- xs ]

score :: Me -> Int
score = \case X -> 1; Y -> 2; Z -> 3

game :: (Them,Me) -> Int
game = \case
  (A,X) -> 3; (A,Y) -> 6; (A,Z) -> 0
  (B,X) -> 0; (B,Y) -> 3; (B,Z) -> 6
  (C,X) -> 6; (C,Y) -> 0; (C,Z) -> 3

part2 :: Setup -> Int
part2 xs = sum [ game (t,m) + score m | (t,r) <- xs, let m = makeR (t,r) ]

makeR :: (Them,Me) -> Me
makeR = \case
  (A,X) -> Z -- rock,loose -> sci
  (A,Y) -> X -- rock,draw -> rock
  (A,Z) -> Y -- rock,win -> paper

  (B,X) -> X -- paper,loose -> rock
  (B,Y) -> Y -- paper,draw -> paper
  (B,Z) -> Z -- paper,win -> sci

  (C,X) -> Y -- sci,loose -> paper
  (C,Y) -> Z -- sci,draw --> sci
  (C,Z) -> X -- sci,win --> rock

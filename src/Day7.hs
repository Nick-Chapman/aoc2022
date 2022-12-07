module Day7 (main) where

import Misc (check)
import Par4 (Par,parse,many,sat,nl,key,alts,lit,int)

main :: IO ()
main = do
  sam <- makeFS <$> parse gram <$> readFile "input/day7.sam"
  inp <- makeFS <$> parse gram <$> readFile "input/day7.input"
  print ("day7, part1 (sam)", check 95437 $ part1 sam)
  print ("day7, part1", check 1390824 $ part1 inp) -- 36482576 -- Noo!
  print ("day7, part2 (sam)", check 24933642 $ part2 sam)
  print ("day7, part2", check 7490863 $ part2 inp)

part1 :: FS -> Int
part1 fs = do
  sum [ z | z <- dirSizes fs, z <= 100000 ]

part2 :: FS -> Int
part2 fs = do
  let tot = totSize fs
  let unused = 70000000 - tot
  let required = 30000000 - unused
  minimum [ z | z <- dirSizes fs, z > required ]

dirSizes :: FS -> [Int]
dirSizes (root@(FS subs)) = [totSize root] ++ concat (map doSub subs)
  where
    doSub = \case
      F _ -> []
      D fs -> dirSizes fs

totSize :: FS -> Int
totSize (FS subs) = sum (map tot subs)
  where
    tot = \case
      F z -> z
      D fs -> totSize fs

data FS = FS [Sub] deriving Show
data Sub = F Int | D FS deriving Show

makeFS :: [Interaction] -> FS
makeFS = loop [] []
  where
    loop :: [Sub] -> [[Sub]] -> [Interaction] -> FS
    loop acc stack = \case
      [] -> finish acc stack
      LS xs:is -> case acc of [] -> loop [ F z | z <- xs ] stack is;  _:_ -> error "ls"
      CD Down:is-> loop [] (acc:stack) is
      CD Root:is -> case stack of [] -> loop acc stack is; _ -> error "root"
      CD Up:is -> case stack of up:stack -> loop (D (FS acc) : up) stack is; [] -> error "up"

    finish :: [Sub] -> [[Sub]] -> FS
    finish acc = \case
      [] -> FS acc
      up:stack -> finish (D (FS acc) : up) stack

type Setup = [Interaction]
data Interaction = CD Dest | LS [Int] deriving Show
data Dest = Root | Up | Down deriving Show

gram :: Par Setup
gram = many interaction
  where
    interaction = do key "$ "; alts [ cd, ls ]
    cd = do key "cd "; d <- dest; nl; pure (CD d)
    dest = alts [slash, dotdot, relative]
    slash = do key "/"; pure Root
    dotdot = do key ".."; pure Up
    relative = do _ <- name; pure Down
    ls = do key "ls"; nl; LS <$> many info
    info = alts [filesize,direxists]
    filesize = do i <- int; lit ' '; name; nl; pure i
    direxists = do key "dir "; name; nl; pure 0
    name = do _ <- many dot; pure ()
    dot = sat $ \c -> c /= '\n'

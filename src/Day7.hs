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
dirSizes (root@(FS _ ts)) = [totSize root] ++ concat (map dirSizes ts)

totSize :: FS -> Int
totSize (FS xs ts) = sum xs + sum (map totSize ts)

data FS = FS [Int] [FS] deriving Show

makeFS :: [Interaction] -> FS
makeFS = loop [] [] []
  where
    loop :: [Int] -> [FS] -> [([Int],[FS])] -> [Interaction] -> FS
    loop xs ts stack = \case
      [] -> finish xs ts stack
      LS xs:is -> loop xs ts stack is
      CD Down:is-> loop [] [] ((xs,ts):stack) is
      CD Root:is -> loop xs ts stack is
      CD Up:is ->
        case stack of
          [] -> error "up"
          (xs',ts'):stack ->
            loop xs' (FS xs ts : ts') stack is

    finish :: [Int] -> [FS] -> [([Int],[FS])] -> FS
    finish xs ts = \case
      [] -> FS xs ts
      (xs',ts'):stack -> finish xs' (FS xs ts : ts') stack

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

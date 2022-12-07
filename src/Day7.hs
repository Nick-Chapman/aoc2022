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
      i:is -> do
        case i of
          LS xs -> case acc of [] -> loop [ F z | FileSize z _ <- xs ] stack is;  _:_ -> error "ls"
          CD (Rel __n) -> loop [] (acc:stack) is
          CD Root -> case stack of [] -> loop acc stack is; _ -> error "root"
          CD DotDot ->
            case stack of
              [] -> error "dotdot"
              acc2:stack -> loop ([D (FS acc)]++acc2) stack is

    finish :: [Sub] -> [[Sub]] -> FS
    finish acc = \case
      [] -> FS acc
      acc2:stack -> finish ([D (FS acc)]++acc2) stack

type Setup = [Interaction]
data Interaction = CD Dest | LS [Info] deriving Show
data Dest = Root | DotDot | Rel Name deriving Show
type Name = String
data Info = FileSize Int Name | DirExists Name deriving Show

gram :: Par Setup
gram = many interaction
  where
    interaction = do key "$ "; alts [ cd, ls ]
    cd = do key "cd "; d <- dest; nl; pure (CD d)
    dest = alts [slash, dotdot, relative]
    slash = do key "/"; pure Root
    dotdot = do key ".."; pure DotDot
    relative = Rel <$> name
    ls = do key "ls"; nl; LS <$> many info
    info = alts [filesize,direxists]
    filesize = do i <- size; lit ' '; n <- name; nl; pure (FileSize i n)
    direxists = do key "dir "; n <- name; nl; pure (DirExists n)
    name = many dot
    dot = sat $ \c -> c /= '\n'
    size = int

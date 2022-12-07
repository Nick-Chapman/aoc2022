module Day7 (main) where

import Misc (check,collate)
import Par4 (Par,parse,many,sat,nl,key,alts,lit,int)

main :: IO ()
main = do
  let makeFS = structureFS . buildEntries
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
      F _ _ -> []
      D _ fs -> dirSizes fs

totSize :: FS -> Int
totSize (FS subs) = sum (map tot subs)
  where
    tot = \case
      F _ z -> z
      D _ fs -> totSize fs

data FS = FS [Sub] deriving Show
data Sub = F Name Int | D Name FS deriving Show

structureFS :: [Entry] -> FS
structureFS = \case
  [] -> FS []
  es -> FS ([ D n (structureFS es') | (n,es') <- subs es ] ++
            [ F n z | (n,z) <- files es ])
  where
    subs :: [Entry] -> [(Name,[Entry])]
    subs es = collate [ (x,(xs,z)) | (x:xs,z) <- es, xs /= [] ]

    files :: [Entry] -> [(Name,Int)]
    files es = [ (n,z) | ([n],z) <- es ]

type Entry = (Path,Int)
type Path = [Name]

buildEntries :: [Interaction] -> [Entry]
buildEntries = loop [] []
  where
    loop :: [Name] -> [Entry] -> [Interaction] -> [Entry]
    loop rpath acc = \case
      [] -> acc
      i:is -> do
        case i of
          CD Root -> loop [] acc is
          CD (Rel n) -> loop (n:rpath) acc is
          CD DotDot -> loop (tail rpath) acc is
          LS xs -> loop rpath ([ (reverse (n:rpath),z) | FileSize z n <- xs ] ++ acc) is

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

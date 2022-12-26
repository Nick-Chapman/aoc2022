module Day20 (main) where

import Misc (check,the)
import Par4 (Par,parse,terminated,nl,opt,lit,int)

main :: IO ()
main = do
  --sam <- parse gram <$> readFile "input/day20.sam"
  inp <- parse gram <$> readFile "input/day20.input"
  --print ("day20, part1 (sam)", check 3 $ part1 sam)
  print ("day20, part1", check 6712 $ part1 inp)
  --print ("day20, part2 (sam)", check 1623178306 $ part2 sam)
  print ("day20, part2", check 1595584274798 $ part2 inp)
    where
      part1 = partX 1 1
      part2 = partX 811589153 10

gram :: Par [Int]
gram = terminated nl sint
  where
    sint = do
      s <- opt (lit '-')
      i <- int
      pure $ case s of Nothing -> i; Just () -> (-1) * i

partX :: Int -> Int -> [Int] -> Int
partX key nRounds xs = a+b+c
  where

    z :: Int
    z = length xs

    a = el (1000 + zero) final
    b = el (2000 + zero) final
    c = el (3000 + zero) final

    el :: Int -> [a] -> a
    el i xs = head (drop (i `mod` z) xs)

    zero = the [i | (i,x) <- zip [0..] final, x == 0 ]

    final = go xs

    go :: [Int] -> [Int]
    go = map snd . (!! nRounds) . iterate (steps 0) . zip [0..] . map (* key)

    steps :: Int -> [(Int,Int)] -> [(Int,Int)]
    steps n ps = do
      if n == z then ps else do
        let (i,j) = the [ (i,j) | (i,(n',j)) <- zip [0::Int ..] ps, n==n' ]
        steps (n+1) (move i j ps)

    move :: Int -> Int -> [a] -> [a]
    move i j xs = slide (j `mod` (z-1)) (rot i xs)

    rot :: Int -> [a] -> [a]
    rot n xs = drop n xs ++ take n xs

    slide :: Int -> [a] -> [a]
    slide n = \case
      [] -> undefined
      y:ys -> take n ys ++ [y] ++ drop n ys

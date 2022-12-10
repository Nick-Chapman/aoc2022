module Day10 (main) where

import Misc (check)
import Par4 (Par,parse,terminated,nl,alts,key,opt,lit,int)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day10.sam"
  inp <- parse gram <$> readFile "input/day10.input"

  print ("day10, part1 (sam)", check 13140 $ part1 sam)
  print ("day10, part1", check 11780 $ part1 inp)

  print "day10, part2 (sam)..."
  let sam_expected =
        ["##..##..##..##..##..##..##..##..##..##.."
        ,"###...###...###...###...###...###...###."
        ,"####....####....####....####....####...."
        ,"#####.....#####.....#####.....#####....."
        ,"######......######......######......####"
        ,"#######.......#######.......#######....."]
  mapM_ putStrLn (check sam_expected (part2 sam))

  print "day10, part2 ..."
  let expected = -- PZULBAUA
        ["###..####.#..#.#....###...##..#..#..##.."
        ,"#..#....#.#..#.#....#..#.#..#.#..#.#..#."
        ,"#..#...#..#..#.#....###..#..#.#..#.#..#."
        ,"###...#...#..#.#....#..#.####.#..#.####."
        ,"#....#....#..#.#....#..#.#..#.#..#.#..#."
        ,"#....####..##..####.###..#..#..##..#..#."]
  mapM_ putStrLn (check expected (part2 inp))

  where
    expand = \case
      Noop -> [Noop]
      Addx i -> [Noop,Addx i]

    step x = \case
      Noop -> x
      Addx i -> x+i

    part1 :: Setup -> Int
    part1 = sum . loop 1 1 . (>>= expand)
      where
        loop :: Int -> Int -> [Op] -> [Int]
        loop cyc x = \case
          [] -> []
          op:ops -> do
            let after = loop (cyc+1) (step x op) ops
            let now = (cyc+20) `mod` 40 == 0
            if now then cyc * x : after else after

    part2 :: Setup -> [String]
    part2 ops0 = chunksOf 40 (loop 0 1 (ops0 >>= expand))
      where
        loop :: Int -> Int -> [Op] -> [Char]
        loop cyc x = \case
          [] -> []
          op:ops -> do
            let on1 = (cyc `mod` 40) == x-1
            let on2 = (cyc `mod` 40) == x
            let on3 = (cyc `mod` 40) == x+1
            let pix = if on1 || on2 || on3 then '#' else '.'
            pix : loop (cyc+1) (step x op) ops

type Setup = [Op]
data Op = Noop | Addx Int deriving Show

gram :: Par Setup
gram = terminated nl line
  where
    line = alts
      [ do key "noop"; pure Noop, do key "addx "; i <- sint; pure (Addx i) ]
    sint = do
      s <- opt (lit '-')
      i <- int
      pure $ case s of Nothing -> i; Just () -> (-1) * i

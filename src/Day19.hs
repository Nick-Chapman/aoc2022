module Day19 (main) where

import Misc (check)
import Par4 (Par,parse,terminated,nl,key,int)

main :: IO ()
main = do
  _sam <- parse gram <$> readFile "input/day19.sam"
  inp <- parse gram <$> readFile "input/day19.input"
{-
  res <- part1 _sam [9,12]
  print ("day19, part1 (sam)", check 33 $ res)
-}
  res <- part1 inp [0,5,0,0,0,5,0,2,1,0,2,3,3,7,1,9,0,1,0,0,0,9,4,5,8,2,2,1,1,9]
  print ("day19, part1", check 1480 $ res)
{-
  res <- _part2 _sam [56,62]
  print ("day19, part2", check (56*62) $ res)
-}
  res <- part2 (take 3 inp) [6,44,12]
  print ("day19, part2", check (6*44*12) $ res)

part1 :: Setup -> [Int] -> IO Int
part1 xs expected = do
  let maxN = 24
  gs <- mapM (explore maxN) (zip xs expected)
  pure $ sum [ g * i | (i,g) <- zip [1::Int ..] gs ]

part2 :: Setup -> [Int] -> IO Int
part2 xs expected = do
  let maxN = 32
  gs <- mapM (explore maxN) (zip xs expected)
  pure $ product gs

type Res = [State]

explore :: Int -> (Blue,Int) -> IO Int
explore maxN (blue@Blue{index=_index},expected) = do
  --print ("explore",_index)
  let ss = loop 0 state0
  let
    see :: Int -> Int -> Res -> IO Int
    see gMax n = \case
      [] -> pure (check gMax expected)
      State{g}:more -> do
        let! _ = if g > gMax then print (g,n) else pure ()
        see (max g gMax) (n+1) more

  see 0 1 ss
  where
    loop :: Int -> State -> [State]
    loop n s = do
      if n == maxN then [s] else do
        a <- [B,R,C]
        loopA a n s

    loopA :: Action -> Int -> State -> [State]
    loopA a n s = do
      if n == maxN then [s] else do
        -- Heuristic #1: Always build a geode cracker if we can.
        case build G blue s of
          Just s -> loopA a(n+1) (advance s)
          Nothing -> do
            case build a blue s of
              Just s -> loop (n+1) (advance s)
              Nothing -> loopA a (n+1) (advance s)


data Action = R | C | B | G deriving Show

build :: Action -> Blue -> State -> Maybe State
build = \case
  R -> buildR
  C -> buildC
  B -> buildB
  G -> buildG

buildR :: Blue -> State -> Maybe State
buildR Blue{rr_cost=rUse} s@State{rr,r,xrr,did} = do
  -- Heuristic #2: Never buy more that 4 oRe Robots.
  if rr >= 4 then Nothing else
    if rUse > r then Nothing else
      Just s { xrr = xrr + 1, r = r - rUse, did = R : did }

buildC :: Blue -> State -> Maybe State
buildC Blue{cr_cost=rUse} s@State{r,xcr,did} = do
  if rUse > r then Nothing else
    Just s { xcr = xcr + 1, r = r - rUse, did = C : did }

buildB :: Blue -> State -> Maybe State
buildB Blue{br_cost=(rUse,cUse)} s@State{br=_,r,c,xbr,did} = do
  if rUse > r || cUse > c then Nothing else
    Just s { xbr = xbr + 1, r = r - rUse, c = c - cUse, did = B : did }

buildG :: Blue -> State -> Maybe State
buildG Blue{gr_cost=(rUse,bUse)} s@State{r,b,xgr,did} = do
  if rUse > r || bUse > b then Nothing else
    Just s { xgr = xgr + 1, r = r - rUse, b = b - bUse, did = G : did }

data State = State
  { r :: Int -- #oRe
  , c :: Int -- #Clay
  , b :: Int -- #oBsidian
  , g :: Int -- #cracked Geodes
  , rr :: Int -- #oRe collecting Robots
  , cr :: Int -- #Clay collecting Robots
  , br :: Int -- #oBsidian collecting Robots
  , gr :: Int -- #Geode cracking Robots

  , xrr :: Int -- #oRe collecting Robots being built
  , xcr :: Int -- #Clay collecting Robots being built
  , xbr :: Int -- #oBsidian collecting Robots being built
  , xgr :: Int -- #Geode cracking Robots being built

  , did :: [Action]
  }
  deriving Show

state0 :: State
state0 = State
  { r=0, c=0, b=0, g=0
  , rr=1, cr=0, br=0, gr=0
  , xrr=0, xcr=0, xbr=0, xgr=0
  , did = []
  }

advance :: State -> State
advance State{r,c,b,g,rr,cr,br,gr,xrr,xcr,xbr,xgr,did} =
  State { r = r + rr
        , c = c + cr
        , b = b + br
        , g = g + gr
        , rr = rr + xrr
        , cr = cr + xcr
        , br = br + xbr
        , gr = gr + xgr
        , xrr = 0
        , xcr = 0
        , xbr = 0
        , xgr = 0
        , did
        }


type Setup = [Blue]

data Blue = Blue
  { index :: Int
  , rr_cost :: Int -- oRe Robot cost (in ore)
  , cr_cost :: Int -- Clay Robot cost (in ore)
  , br_cost :: (Int,Int) -- oBsidian Robot cost (in ore/clay)
  , gr_cost :: (Int,Int) -- Geode Robot cost (in ore/obsidian)
  } deriving Show


gram :: Par Setup
gram = terminated nl blue
  where
    blue = do
      key "Blueprint "
      index <- int
      key ": Each ore robot costs "
      rr_cost <- int
      key " ore. Each clay robot costs "
      cr_cost <- int
      key " ore. Each obsidian robot costs "
      br_cost <- ii
      key " clay. Each geode robot costs "
      gr_cost <- ii
      key " obsidian."
      pure Blue { index, rr_cost, cr_cost, br_cost, gr_cost }

    ii :: Par (Int,Int)
    ii = do
      o <- int
      key " ore and "
      c <- int
      pure (o,c)

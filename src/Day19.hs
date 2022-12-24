module Day19 (main) where

import Misc (check)
import Par4 (Par,parse,terminated,nl,key,int)

main :: IO ()
main = do
  _sam <- parse gram <$> readFile "input/day19.sam"
  _inp <- parse gram <$> readFile "input/day19.input"

  -- Too slow to run part1 on sample's 2nd blueprint...
  --res <- partX 24 _sam
  --print ("day19, part1 (sam)", check 33 $ res) -- 1*9 + 2*12

  -- But it works on my actual input!...
  res <- partX 24 _inp
  print ("day19, part1", check 1480 $ res)

  -- But too slow or part2...
  --res <- partX 32 (take 3 _inp)
  --print ("day19, part2", check 0 $ res)


partX :: Int -> Setup -> IO Int
partX n xs = do
  print "part1"
  qs <- mapM (explore n) xs
  pure (sum qs)


type Res = [State]

explore :: Int -> Blue -> IO Int
explore maxN blue@Blue{index} = do
  print ("explore",blue)
  let ss = loop 0 state0
  let
    see :: Int -> Int -> Res -> IO Int
    see gMax n = \case
      [] -> do
        let q = index * gMax
        print ("index:",index,"gMax:",gMax,"--> quality:",q)
        pure q

      State{g}:more -> do
        if g > gMax then print (g,n) else pure ()
        see (max g gMax) (n+1) more

  see 0 1 ss
  where

    loop :: Int -> State -> Res
    loop n s = do
      {-case build G blue s of
        Just s -> do
          loop (n+1) (advance s)
        Nothing -> do-}
          a <- [G,B,R,C]
          loopA n s a

    loopA :: Int -> State -> Action -> Res
    loopA n s a = do
      if n == maxN then [s] else do
        case build a blue s of
          Just s -> do
            loop (n+1) (advance s)
          Nothing -> do
            loopA (n+1) (advance s) a


data Action = N | R | C | B | G deriving Show

build :: Action -> Blue -> State -> Maybe State
build = \case
  R -> buildR
  C -> buildC
  B -> buildB
  G -> buildG
  N -> \_ -> Just

buildR :: Blue -> State -> Maybe State
buildR Blue{rr_cost=rUse} s@State{r,xrr,did} = do
  if rUse > r then Nothing else
    Just s { xrr = xrr + 1, r = r - rUse, did = R : did }

buildC :: Blue -> State -> Maybe State
buildC Blue{cr_cost=rUse} s@State{r,xcr,did} = do
  if rUse > r then Nothing else
    Just s { xcr = xcr + 1, r = r - rUse, did = C : did }

buildB :: Blue -> State -> Maybe State
buildB Blue{br_cost=(rUse,cUse)} s@State{r,c,xbr,did} = do
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

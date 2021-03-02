{-# LANGUAGE InstanceSigs #-}

module Task8 ( allHealthyGrid
             , illInTheMiddle
             , ConstsHolder (..)
             , evolve
             , evolveCustom
             , evolveDefault
             , showPatients
             ) where

import Control.Comonad ( Comonad (..) )
import Control.Monad ( liftM2 )
import System.Random ( RandomGen (..), StdGen, mkStdGen )
import Data.Map ( Map, (!) )
import qualified Data.Map as Map ( fromList )

data ListZipper a = LZ [a] a [a]

listLeft, listRight :: ListZipper a -> ListZipper a
listLeft  (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _ = error "listLeft"

listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _ = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

-- |Builds zipper from list
fromList :: [a] -> ListZipper a
fromList lst = LZ l (head lst) r
  where
    fromListHelper :: [a] -> Bool -> ([a], [a])
    fromListHelper (cur : rest) isLeft =
      let
        (ll, rr) = fromListHelper rest (not isLeft)
      in
        if isLeft
        then (cur : ll, rr)
        else (ll, cur : rr)
    fromListHelper [] _ = ([], [])

    (l,r) = fromListHelper (tail lst) True

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (z a -> z a)
            -> (z a -> z a)
            -> z a
            -> ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

instance Functor ListZipper where
  fmap :: (a -> b) -> ListZipper a -> ListZipper b
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

up, down :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft  g)
down (Grid g) = Grid (listRight g)

left, right :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft  g)
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right
vertical   = genericMove up   down

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f grid = Grid $ (\row -> f <$> row) <$> unGrid grid

instance Comonad Grid where
    extract :: Grid a -> a
    extract = gridRead

    duplicate :: Grid a -> Grid (Grid a)
    duplicate = Grid . fmap horizontal . vertical

-- |Some consts used during simulations
data ConstsHolder = ConstsHolder { status2maxLength :: Map PatientStatus Int
                                 , probability :: Double
                                 }

-- |Some example consts
defaultConsts :: ConstsHolder
defaultConsts = ConstsHolder (Map.fromList [(Ill, 5), (Healthy, (-1)), (Immune, 3), (Incubating, 4)]) 0.3

-- |Enum of patient status
data PatientStatus = Ill
                  | Incubating
                  | Immune
                  | Healthy
                  deriving ( Eq, Ord, Show )

-- |Display status as char
showStatus :: PatientStatus -> Char
showStatus Ill = '#'
showStatus Healthy = '·'
showStatus Immune = '@'
showStatus Incubating = '·'

-- |Next status if status changes
nextStatus :: PatientStatus -> PatientStatus
nextStatus Ill = Immune
nextStatus Immune = Healthy
nextStatus Incubating = Ill
nextStatus Healthy = Healthy

-- |Data representation of human participating in simulation
data Patient = Patient { pstatus :: PatientStatus
                       , pdaysStatus :: Int
                       , pgen :: StdGen
                       } deriving ( Show )

-- |Invokes next on generator n times
nextN :: RandomGen g => Int -> g -> ([Int], g)
nextN x g
  | x == 0 =
    ([], g)
  | otherwise =
    let
      (val, newGen) = next g
      (nexts, resGen) = nextN (x - 1) newGen
    in
      (val : nexts, resGen)

-- |Checks given double probability on given generator
checkProbability :: StdGen -> Double -> (Bool, StdGen)
checkProbability g p =
  let
    range = genRange g
    (val, newg) = next g
    isSucced = (fromIntegral (val - fst range)) / (fromIntegral (snd range - fst range))
  in
    (isSucced < p, newg)

-- |Updates status using current status duration and maximum possible duration
updateStatus :: PatientStatus -> Int -> Int -> PatientStatus
updateStatus status howLong maxLong =
  if (howLong >= maxLong)
  then (nextStatus status)
  else status

-- |Update patient using amount of ill neighbours
updatePatient :: ConstsHolder -> Int -> Patient -> Patient
updatePatient holder illCnt (Patient s howLong g) =
  let
    maxTime = status2maxLength holder ! s
    illProbability =
      if (s == Healthy)
      then 1 - (1 - probability holder) ** fromIntegral illCnt
      else 0
    (isBecomeIll, newGen) = checkProbability g illProbability
  in
    if (isBecomeIll)
    then Patient Incubating 0 newGen
    else Patient (updateStatus s (howLong + 1) maxTime) (if (howLong + 1 == maxTime) then 0 else (howLong + 1)) newGen

neighbours :: [Grid a -> Grid a]
neighbours =
  horizontals ++ verticals ++ liftM2 (.) horizontals verticals
    where
      horizontals = [left, right]
      verticals   = [up, down]

-- |Counts of ill patients from given
illCount :: [Patient] -> Int
illCount = length . filter ((\status -> status == Ill || status == Incubating) . pstatus)

-- |Counts ill pneighbours
illNeighbours :: Grid Patient -> Int
illNeighbours g = illCount
                  $ map (\direction -> extract $ direction g) neighbours

-- |Rule used to calulate next step
rule :: ConstsHolder -> Grid Patient -> Patient
rule holder g =
  let
    illCnt = illNeighbours g
  in
    updatePatient holder illCnt (extract g)

-- |Function to go to next step
evolve :: ConstsHolder -> Grid Patient -> Grid Patient
evolve holder = extend (rule holder)

-- |Evolve n steps using given consts
evolveCustom :: ConstsHolder -> Int -> Grid Patient -> Grid Patient
evolveCustom consts steps grid = iterate (evolve consts) grid !! steps

-- |Evolve n steps using default consts
evolveDefault :: Int -> Grid Patient -> Grid Patient
evolveDefault steps grid = iterate (evolve defaultConsts) grid !! steps

-- |Shows grid as list using given size
gridToList :: Int -> Grid a -> [[a]]
gridToList sz grid = foldMap ((:[]) . flip toList sz) $ flip toList sz $ unGrid grid

-- |Shows grid of patients
showPatients :: Int -> Grid Patient -> [[Char]]
showPatients sz grid = map (map (showStatus . pstatus)) $ gridToList sz grid

-- |Builds grid with all healthy patients using given salt
allHealthyGrid :: Int -> Grid Patient
allHealthyGrid salt =
  let
    salts = fst $ nextN (-1) (mkStdGen salt)
    patients = map (\s -> Patient Healthy 0 (mkStdGen s)) salts
  in
    Grid $ duplicate $ fromList patients

-- |Builds grid with all healthy patients and one ill in the middle using given salt
illInTheMiddle :: Int -> Grid Patient
illInTheMiddle salt =
  let
    (middle : rest : _) = fst $ nextN 2 (mkStdGen salt)
  in
    gridWrite (Patient Ill 0 (mkStdGen middle)) (allHealthyGrid rest)

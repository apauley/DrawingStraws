import System.Environment (getArgs)
import System.Random
import Data.List
import Data.Map (Map, empty, insert, lookup)

type ShortPos = Int
type Straw = (ShortPos, Bool)
type BunchOfStraws = [Straw]

type Count = Int
type StatsMap = Map ShortPos Count

main = do
    seed <- newStdGen
    args <- getArgs

    let (numStraws, numDraws) = parseArgs args

    let shorts = take numDraws $ randomShortPositions numStraws seed
    print shorts

    let bunch = bunchOfStraws numStraws $ head shorts
    putStrLn "First bunch:"
    print bunch
    putStrLn ""

    putStrLn "First draw:"
    print $ drawStraws bunch
    putStrLn ""

    let draws = take numDraws $ map drawStraws $ bunchesOfStraws numStraws $ randomShortPositions numStraws seed

    let statsMap = calcStats draws
    putStrLn "Stats:"
    print statsMap
    putStrLn ""


calcStats :: [ShortPos] -> StatsMap
calcStats = foldl updateStats Data.Map.empty

updateStats :: StatsMap -> ShortPos -> StatsMap
updateStats statsMap shortPos =
  Data.Map.insert shortPos count statsMap
  where count = increment statsMap shortPos

increment :: StatsMap -> ShortPos -> Count
increment statsMap shortPos =
  case (Data.Map.lookup shortPos statsMap) of
    Nothing    -> 1
    Just count -> count + 1

-- Trying to simulate action of drawing straws by recursively looking at
-- a decreasing set of straws until a short is found.
drawStraws :: BunchOfStraws -> ShortPos
drawStraws (straw:tail) = if isShort then shortPos else drawStraws tail
  where (shortPos, isShort) = straw

bunchesOfStraws :: Int -> [ShortPos] -> [BunchOfStraws]
bunchesOfStraws numStraws positions = map (bunchOfStraws numStraws) positions

bunchOfStraws :: Int -> ShortPos -> BunchOfStraws
bunchOfStraws numStraws shortPos = map (\i -> (i, i == shortPos)) [1..numStraws]

randomShortPositions :: Int -> StdGen -> [ShortPos]
randomShortPositions numStraws = randomRs (1,numStraws)

parseArgs :: [String] -> (Int, Int)
parseArgs args = (read $ args !! 0, read $ args !! 1)

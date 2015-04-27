import System.Environment (getArgs)
import System.Random
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

type ShortPos = Int
type Straw = (ShortPos, Bool)
type BunchOfStraws = [Straw]

type Count = Int
type StatsMap = Map ShortPos Count

main = do
    seed <- newStdGen
    args <- getArgs

    let (numStraws, numDraws) = parseArgs args
    let msg = "Counting short straw occurrences over " ++ show numDraws ++
              " draws, with " ++ show numStraws ++ " straws in each bunch.\n"
    putStrLn msg

    let shortStream = randomShortPositions seed
    let draws = take numDraws $ drawStream shortStream numStraws

    let statsMap = calcStats draws
    putStrLn "Stats:"
    putStrLn $ statsMessage statsMap
    putStrLn ""

-- Trying to simulate the action of drawing straws by recursively looking
-- at a decreasing set of straws until a short is found.
drawStraws :: BunchOfStraws -> ShortPos
drawStraws (straw:tail) = if isShort then shortPos else drawStraws tail
  where (shortPos, isShort) = straw

drawStream :: (Int -> [ShortPos]) -> Int -> [ShortPos]
drawStream shortStream numStraws = map drawStraws $ bunchesOfStraws shortStream numStraws

bunchesOfStraws :: (Int -> [ShortPos]) -> Int -> [BunchOfStraws]
bunchesOfStraws shortStream numStraws  = map (bunchOfStraws numStraws) $ shortStream numStraws

bunchOfStraws :: Int -> ShortPos -> BunchOfStraws
bunchOfStraws numStraws shortPos = map (\i -> (i, i == shortPos)) [1..numStraws]

randomShortPositions :: StdGen -> Int -> [ShortPos]
randomShortPositions seed numStraws = randomRs (1,numStraws) seed

statsMessage :: StatsMap -> String
statsMessage = show

calcStats :: [ShortPos] -> StatsMap
calcStats = foldl updateStats Map.empty

updateStats :: StatsMap -> ShortPos -> StatsMap
updateStats statsMap shortPos =
  Map.insert shortPos count statsMap
  where count = increment statsMap shortPos

increment :: StatsMap -> ShortPos -> Count
increment statsMap shortPos =
  case (Map.lookup shortPos statsMap) of
    Nothing    -> 1
    Just count -> count + 1

parseArgs :: [String] -> (Int, Int)
parseArgs args = (read $ args !! 0, read $ args !! 1)

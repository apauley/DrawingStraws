import System.Environment (getArgs)
import Data.Time (getCurrentTime)
import System.Random
import Data.List

type ShortPos = Int
type Straw = (ShortPos, Bool)
type BunchOfStraws = [Straw]

type Count = Int
type StatsMap = [(ShortPos, Count)]

main = do
  seed <- newStdGen
  args <- getArgs

  let (numStraws, numDraws) = parseArgs args
  let msg = "Counting short straw occurrences over " ++ show numDraws ++
            " draws, with " ++ show numStraws ++ " straws in each bunch.\n"
  logTime msg

  let shortStream = randomShortPositions seed
  let draws = take numDraws $ drawStream shortStream numStraws

  logTime $ "Performed " ++ show (length draws) ++ " draws."
  logTime "Counting position occurrences...\n"

  let statsMap = calcStats draws
  putStrLn $ statsMessage statsMap

  logTime "Done."

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
statsMessage statsMap = "Short straw position counts:\n" ++ counters ++ "\n" ++ total
  where counters = foldl countStr "" statsMap
        total    = "Total: " ++ show (foldl (+) 0 $ map snd statsMap) ++ "\n"

countStr :: String -> (ShortPos, Count) -> String
countStr acc (pos, count) = acc ++ show pos ++ ":\t" ++ show count ++ "\n"

calcStats :: [ShortPos] -> StatsMap
calcStats = frequency

frequency :: Ord a => [a] -> [(a, Int)]
frequency xs = map (\l -> (head l, length l)) (group (sort xs))

parseArgs :: [String] -> (Int, Int)
parseArgs args = (read $ args !! 0, read $ args !! 1)

logTime :: String -> IO ()
logTime msg = do
  time <- getCurrentTime
  putStrLn $ show time ++ " | " ++ msg

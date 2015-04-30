import System.Environment (getArgs)

import Data.Time (getCurrentTime)
import Data.List

import System.Random

import Control.Exception
import Control.DeepSeq

type ShortPos = Int
type Straw = (ShortPos, Bool)
type BunchOfStraws = [Straw]

type Count = Int
type ShortFrequency = [(ShortPos, Count)]

main :: IO ()
main = do
  seed <- newStdGen
  args <- getArgs

  let (numStraws, numDraws) = parseArgs args
  let msg = "Counting short straw occurrences over " ++ show numDraws ++
            " draws, with " ++ show numStraws ++ " straws in each bunch.\n"
  logTime msg

  let shortStream = randomShortPositions seed
  draws <- evaluate $ deep $ take numDraws $ drawStream shortStream numStraws

  logTime $ "Performed " ++ show (length draws) ++ " draws."
  logTime "Counting position occurrences...\n"

  let freq = shortFrequency draws
  putStrLn $ summary freq

  logTime "Done."

-- Trying to simulate the action of drawing straws by recursively looking
-- at a decreasing set of straws until a short is found.
drawStraws :: BunchOfStraws -> ShortPos
drawStraws [] = error "No short straw in bunch!"
drawStraws (straw:straws) = if isShort then shortPos else drawStraws straws
  where (shortPos, isShort) = straw

drawStream :: (Int -> [ShortPos]) -> Int -> [ShortPos]
drawStream shortStream numStraws = map drawStraws $ bunchesOfStraws shortStream numStraws

bunchesOfStraws :: (Int -> [ShortPos]) -> Int -> [BunchOfStraws]
bunchesOfStraws shortStream numStraws  = map (bunchOfStraws numStraws) $ shortStream numStraws

bunchOfStraws :: Int -> ShortPos -> BunchOfStraws
bunchOfStraws numStraws shortPos = map (\i -> (i, i == shortPos)) [1..numStraws]

randomShortPositions :: StdGen -> Int -> [ShortPos]
randomShortPositions seed numStraws = randomRs (1,numStraws) seed

summary :: ShortFrequency -> String
summary freq = "Short straw position counts:\n" ++ counters ++ "\n" ++ total
  where counters = foldl countStr "" freq
        total    = "Total: " ++ show (foldl (+) 0 $ map snd freq) ++ "\n"

countStr :: String -> (ShortPos, Count) -> String
countStr acc (pos, count) = acc ++ show pos ++ ":\t" ++ show count ++ "\n"

shortFrequency :: [ShortPos] -> ShortFrequency
shortFrequency = frequency

frequency :: Ord a => [a] -> [(a, Int)]
frequency xs = map (\l -> (head l, length l)) (group (sort xs))

parseArgs :: [String] -> (Int, Int)
parseArgs args = (read $ args !! 0, read $ args !! 1)

logTime :: String -> IO ()
logTime msg = do
  time <- getCurrentTime
  putStrLn $ show time ++ " | " ++ msg

deep :: NFData a => a -> a
deep a = deepseq a a

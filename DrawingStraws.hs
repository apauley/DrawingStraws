import System.Environment (getArgs)
import System.Random
import Data.List

type ShortPos = Int
type Straw = (ShortPos, Bool)
type BunchOfStraws = [Straw]

main = do
    seed <- newStdGen
    args <- getArgs

    print args
    let (numStraws, numDraws) = parseArgs args

    let shorts = take numDraws $ randomShortPositions numStraws seed
    print shorts

    let bunch = bunchOfStraws numStraws $ head shorts
    print bunch
    print $ drawStraws bunch

-- Trying to simulate action of drawing straws by recursively looking at
-- a decreasing set of straws until a short is found.
drawStraws :: BunchOfStraws -> ShortPos
drawStraws (straw:tail) = if isShort then shortPos else drawStraws tail
  where (shortPos, isShort) = straw

bunchOfStraws :: Int -> ShortPos -> BunchOfStraws
bunchOfStraws numStraws shortPos = map (\i -> (i, i == shortPos)) [1..numStraws]

randomShortPositions :: Int -> StdGen -> [ShortPos]
randomShortPositions numStraws = randomRs (1,numStraws)

parseArgs :: [String] -> (Int, Int)
parseArgs args = (read $ args !! 0, read $ args !! 1)

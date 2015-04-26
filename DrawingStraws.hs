import System.Environment (getArgs)
import System.Random
import Data.List

type Straw = Int

main = do
    seed <- newStdGen
    args <- getArgs

    print args
    let (numStraws, numDraws) = parseArgs args

    let shorts = take numDraws $ shortStraws numStraws seed
    print shorts


shortStraws :: Int -> StdGen -> [Straw]
shortStraws numStraws = randomRs (1,numStraws)

parseArgs :: [String] -> (Int, Int)
parseArgs args = (read $ args !! 0, read $ args !! 1)

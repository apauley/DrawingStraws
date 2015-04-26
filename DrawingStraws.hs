import System.Environment (getArgs)
import System.Random
import Data.List

main = do
    seed <- newStdGen
    args <- getArgs

    print args
    let (numStraws, numDraws) = parseArgs args
    print numStraws

    let shorts = take numDraws $ shortStraws numStraws seed
    print shorts

parseArgs :: [String] -> (Int, Int)
parseArgs args = (read $ args !! 0, read $ args !! 1)

shortStraws :: Int -> StdGen -> [Int]
shortStraws numStraws = randomRs (1,numStraws)

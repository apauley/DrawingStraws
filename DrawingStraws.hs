import System.Environment (getArgs)
import System.Random
import Data.List

type Straw = (Int, Bool)

main = do
    seed <- newStdGen
    args <- getArgs

    print args
    let (numStraws, numDraws) = parseArgs args

    let shorts = take numDraws $ randomShortStraws numStraws seed
    print shorts

drawStraw :: [Straw] -> Straw
drawStraw = head

randomShortStraws :: Int -> StdGen -> [Int]
randomShortStraws numStraws = randomRs (1,numStraws)

parseArgs :: [String] -> (Int, Int)
parseArgs args = (read $ args !! 0, read $ args !! 1)

import System.Environment (getArgs)
import System.Random
import Data.List

main = do
    seed <- newStdGen
    args <- getArgs

    print args
    let numStraws = read $ head args :: Int
    print numStraws

    let shorts = take 10 $ shortStraws numStraws seed
    print shorts

shortStraws :: Int -> StdGen -> [Int]
shortStraws numStraws = randomRs (1,numStraws)

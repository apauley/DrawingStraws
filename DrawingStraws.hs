import System.Random
import Data.List

main = do
    seed  <- newStdGen
    let shorts = take 10 $ short_straws seed
    print shorts

short_straws :: StdGen -> [Int]
short_straws = randomRs (1,10)

import System.Random
import Data.List

main = do
    seed  <- newStdGen
    let rs = randomlist 10 seed
    print rs

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

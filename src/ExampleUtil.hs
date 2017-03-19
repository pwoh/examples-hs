module ExampleUtil where

import System.Random
import Data.List
import Data.Array.Accelerate                            as A

-- Takes a size and a seed to produce a list of doubles
randomlist :: Int -> StdGen -> [Double]
randomlist n = Data.List.take n . unfoldr (Just . random)

-- Takes a size and a list of doubles to produce an Accelerate vector
toAccVector :: Int -> [Double] -> Acc (Vector Double)
toAccVector size rs = A.use $ A.fromList (Z :. size) $ rs

-- Do nothing 
noop size = do
    return ()
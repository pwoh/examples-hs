module Saxpy (saxpyRandom) where
import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.CUDA as AC
import System.Random
import ExampleUtil

saxpy :: Acc (Vector Double) -> Acc (Vector Double) -> Exp Double -> Acc (Vector Double)
saxpy xs ys a = A.zipWith (\x y -> a * x + y) xs ys

saxpyRandom size = do
  seed <- newStdGen
  let rs = randomlist size seed
  let test = toAccVector size rs
  x <- putStr $ (show $ AC.run $ saxpy test test (A.constant 11.0))
  return ()

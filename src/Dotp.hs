module Dotp (dotpRandom) where
import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.CUDA as AC
import System.Random
import ExampleUtil

dotp :: Acc (Vector Double) -> Acc (Vector Double) -> Acc (Scalar Double)
dotp xs ys = fold (+) 0 (A.zipWith (*) xs ys)

dotpRandom size = do
  seed <- newStdGen
  let rs = randomlist size seed
  let test = toAccVector size rs
  x <- putStr $ (show $ AC.run $ dotp test test)
  return ()
module MMultReplicate (multiplyMMRandom) where

import Lib

import Prelude                                          as P
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate
          (Acc, Array, Exp, Any(Any), All(All), Z(Z), (:.)((:.)))
import Data.Array.Accelerate.CUDA as AC
import Data.List
import Control.Monad
import System.Random
import ExampleUtil
--TODO remove unused

---XTypeOperators
--http://bobkonf.de/2015/slides/thielemann.pdf

multiplyMMRandom size = do
  seed <- newStdGen
  let rs = randomlist (size*size) seed
  let test = toMatrix size rs
  x <- putStr $ (show $ A.arraySize $ A.arrayShape $ AC.run $ multiplyMatrixMatrix test test)
  return ()

type Matrix ix a = A.Acc (Array (ix :. Int :. Int) a)

matrixShape ::
   (A.Shape ix, A.Slice ix, A.Elt a) =>
   Matrix ix a -> Exp ix :. Exp Int :. Exp Int
matrixShape m = A.unlift $ A.shape m

swapIndex ::
   Exp ix :. Exp Int :. Exp Int -> Exp ix :. Exp Int :. Exp Int
swapIndex (ix :. r :. c) = (ix :. c :. r)

transpose ::
   (A.Shape ix, A.Slice ix, A.Elt a) =>
   Matrix ix a -> Matrix ix a
transpose m =
   A.backpermute
      (A.lift $ swapIndex $ matrixShape m)
      (A.lift . swapIndex . A.unlift)
      m

multiplyMatrixMatrix ::
   (A.Shape ix, A.Slice ix, A.IsNum a, A.Elt a) =>
   Matrix ix a -> Matrix ix a -> Matrix ix a
multiplyMatrixMatrix x y =
   case (matrixShape x, matrixShape y) of
      (_ :. rows :. _cols, _ :. _rows :. cols) ->
         A.fold1 (+) $ MMultReplicate.transpose $
         A.zipWith (*)
            (A.replicate (A.lift $ Any :. All :. All :. cols) x)
            (A.replicate (A.lift $ Any :. rows :. All :. All) y)

-- Takes a size and a list of doubles and produces a Matrix 
toMatrix :: Int -> [Double] -> Matrix Z Double
toMatrix size rs = A.use $ A.fromList (Z :. size :. size) $ rs



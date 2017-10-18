import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.CUDA as AC
import Data.List
import System.Environment
import Control.Monad
import MMultDivConq
import MMultReplicate 
import Dotp
import Saxpy
import BlackScholes
import ExampleUtil

-- "Usage: example-hs [vector or matrix size] [times to run] [function to run]"
main = do 
  [sizeStr, timesStr, functionStr] <- getArgs
  let size = read sizeStr :: Int
  let times = read timesStr :: Int
  let f = case functionStr of
        "noop" -> noop
        "dotp" -> dotpRandom
        "saxpy" -> saxpyRandom
        "bs" -> blackScholesRandom
        "mmult_repl" -> MMultReplicate.multiplyMMRandom
        "mmult_divconq" -> MMultDivConq.multiplyMMRandom --Note this gives a 2x1 instead of 1x1 when the size 1 is used. Bug?
  replicateM_ times (f size)

---XTypeOperators
-- ghc -threaded --make -XConstraintKinds Main.hs 
-- stack exec examples-hs-exe 2 mmult_repl 1

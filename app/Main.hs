import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.CUDA as AC
import System.Random
import Data.List
import System.Environment
import Control.Monad
import MMultDivConq
import MMultReplicate 
import Dotp
import ExampleUtil

-- 1 arg not valid
-- 2 args the first one is the name of the function, TODO name of function & times to repl, and include dotp with this
main = do 
  args <- getArgs
  let size = read (head args) :: Int
  let times = case (Data.List.length args) of
	--1 -> 1
	2 -> read (head (Data.List.tail args)) :: Int
  replicateM_ times (MMultDivConq.multiplyMMRandom size)

---XTypeOperators
-- ghc -threaded --make -XConstraintKinds Main.hs 

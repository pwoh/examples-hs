{-# LANGUAGE FlexibleContexts #-}
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.CUDA        as AC 
import Data.Array.Accelerate.Interpreter        as I 
import Data.Array.Accelerate.System.Random.MWC        -- random numbers

import Criterion.Main                                 -- benchmarking

import Prelude as P

import Text.Read
import Text.Printf
import System.Environment

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold1 (+) ( A.zipWith (*) xs ys )

main :: IO ()
main = do
  argv <- getArgs

  let (n, argv') =
        case argv of
          (s:rest) | Just v <- readMaybe s -> (v,      rest)
          _                                -> (100000, argv)

  printf "benchmarking with size: %d\n" n

  gen  <- createSystemRandom
--  xs   <- randomArrayWith gen uniform (Z :. n)
--  ys   <- randomArrayWith gen uniform (Z :. n)
  xs   <- (randomArrayWith gen uniform (Z :. n)) :: IO (Vector (Float, Float, Float))

  withArgs argv' $ defaultMain
    [ 
      bench "bs/acctf" $ nf (AC.run . blackscholes . A.use) (xs :: Vector (Float, Float, Float))
      --bench "dotp/acctf" $ nf (AC.run1 (dotp (use xs))) ys
    ]

  return ()





psy :: Acc (Vector (Float,Float,Float))
psy = use $ fromList (Z:.3) [(5.0,1.0,0.25),(17.0,50.0,5.0),(30.0,100.0,10.0)]

riskfree, volatility :: P.Floating a => a
riskfree   = 0.02
volatility = 0.30

horner :: P.Num a => [a] -> a -> a
horner coeff x = x * foldr1 madd coeff
  where
    madd a b = a + x*b
cnd' :: P.Floating a => a -> a
cnd' d =
  let poly     = horner coeff
      coeff    = [0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]
      rsqrt2pi = 0.39894228040143267793994605993438
      k        = 1.0 / (1.0 + 0.2316419 * abs d)
  in
  rsqrt2pi * exp (-0.5*d*d) * poly k


blackscholes :: (P.Floating a, A.Floating a, A.Ord a) => Acc (Vector (a, a, a)) -> Acc (Vector (a, a))
blackscholes = A.map go
  where
  go x =
    let (price, strike, years) = A.unlift x
        r       = A.constant riskfree 
        v       = A.constant volatility
        v_sqrtT = v * sqrt years
        d1      = (log (price / strike) + (r + 0.5 * v * v) * years) / v_sqrtT
        d2      = d1 - v_sqrtT
        cnd d   = let c = cnd' d in d A.> 0 ? (1.0 - c, c)
        cndD1   = cnd d1
        cndD2   = cnd d2
        x_expRT = strike * exp (-r * years)
    in
    A.lift ( price * cndD1 - x_expRT * cndD2 --V_call
           , x_expRT * (1.0 - cndD2) - price * (1.0 - cndD1)) --V_put


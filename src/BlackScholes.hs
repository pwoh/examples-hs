{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module BlackScholes (

  blackScholesRandom

) where


import Prelude                                                  as P

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar                        as A
import Data.Array.Accelerate.CUDA as AC
import System.Random
import ExampleUtil

blackScholesRandom size = do
    s1 <- newStdGen
    s2 <- newStdGen
    s3 <- newStdGen
    let rs = random3Tuplelist size s1 s2 s3
    let test = toAccVector3 size rs
    x <- putStr $ show $ AC.run $ blackscholes test
    return ()

-- Black-Scholes option pricing ------------------------------------------------
--

riskfree, volatility :: P.Floating a => a
riskfree   = 0.02
volatility = 0.30

horner :: P.Num a => [a] -> a -> a
horner coeff x = x * foldr1 madd coeff
  where
    madd a b = a + x*b
-- see below what this means.
-- basically using horner's rule to reduce number of multiplications required
-- to factor out the powers

-- Polynomial approximation of the cumulative normal distribution function
-- 6 decimal place accuracy
cnd' :: P.Floating a => a -> a
cnd' d =
  let poly     = horner coeff
      coeff    = [0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]
      rsqrt2pi = 0.39894228040143267793994605993438
      k        = 1.0 / (1.0 + 0.2316419 * abs d)
  in
  rsqrt2pi * exp (-0.5*d*d) * poly k
-- note poly k expands out to a 5th order polynomial:
-- (k * (a1 + k * (a2 + k * (a3 + k * (a4 + k * a5)))))
-- also note that CND(-d) = 1-CND(d)

-- V_call = S*cnd(d1) - X*exp(-rT)*cnd(d2)
-- V_put = X*exp(-rT)*cnd(-d2) - S*cnd(-d1)
-- S = current option price
-- d1 = Formula given below
-- d2 = Formula given below
-- X = strike price
-- r = continuously compounded risk free interest rate
-- T = time to expiration
-- v = implied volatility for the underlying stock

-- a is a type variable, they are all the same type
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
-- NB: Lift and Unlift both happen on the GPU.


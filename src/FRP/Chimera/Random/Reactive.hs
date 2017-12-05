{-# LANGUAGE Arrows #-}
module FRP.Chimera.Random.Reactive 
  (
    randomBoolS
  , randomBoolS_

  , randomElemS
  , randomElemS_

  , randomExpS
  , randomExpS_
  ) where

import Control.Monad.Random
import Control.Monad.Trans.MSF.Random 

import FRP.BearRiver

randomBoolS :: MonadRandom m => Double -> MSF m a Bool
randomBoolS p = proc _ -> do
  r <- getRandomRS ((0, 1) :: (Double, Double)) -< ()
  returnA -< r <= p

randomBoolS_ :: MonadRandom m => MSF m Double Bool
randomBoolS_ = proc p -> do
  r <- getRandomRS ((0, 1) :: (Double, Double)) -< ()
  returnA -< r <= p

randomElemS :: MonadRandom m => [b] -> MSF m a b
randomElemS bs = proc _ -> do
  let len = length bs
  idx <- getRandomRS_ -< (0, len - 1) 
  returnA -< (bs !! idx)

randomElemS_ :: MonadRandom m => MSF m [a] a
randomElemS_ = proc as -> do
  let len = length as
  idx <- getRandomRS_ -< (0, len - 1) 
  returnA -< (as !! idx)

randomExpS :: MonadRandom m => Double -> MSF m a Double
randomExpS lambda = proc _ -> do
  r <- getRandomRS ((0, 1) :: (Double, Double)) -< ()
  returnA -< ((-log r) / lambda)

randomExpS_ :: MonadRandom m => MSF m Double Double
randomExpS_ = proc lambda -> do
  r <- getRandomRS ((0, 1) :: (Double, Double)) -< ()
  returnA -< ((-log r) / lambda)
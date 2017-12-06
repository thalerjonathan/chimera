module FRP.Chimera.Random.Monadic 
  (
    randomBoolM
  , randomExpM
  , randomElemM
  , randomShuffleM
  , avoidM
  ) where

import Control.Monad.Random
import Control.Monad.State

import FRP.Chimera.Random.Pure 

randomBoolM :: MonadRandom m => Double -> m Bool
randomBoolM p = getRandomR (0.0, 1.0) >>= (\r -> return $ p >= r)

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
randomExpM :: MonadRandom m => Double -> m Double
randomExpM lambda = avoidM 0 >>= (\r -> return $ ((-log r) / lambda))

randomElemM :: MonadRandom m => [a] -> m a
randomElemM as = do
  let len = length as
  idx <- getRandomR (0, len - 1) 
  return (as !! idx)

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
avoidM :: (Random a, Eq a, MonadRandom m) => a -> m a
avoidM x = do
  r <- getRandom
  if (r == x) 
    then avoidM x
    else return r

randomShuffleM :: (MonadState g m, RandomGen g, MonadRandom m) => [a] -> m [a]
randomShuffleM as = do
  g <- get
  let (as', g') = fisherYatesShuffle g as
  put g'
  return as'

{-
randomShuffleM :: (RandomGen g) => [a] -> Rand g [a]
randomShuffleM _as = do
  g <- get
  let (as', g') = fisherYatesShuffle g as
  put g'
  return as'-}
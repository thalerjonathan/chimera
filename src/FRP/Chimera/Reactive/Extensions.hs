{-# LANGUAGE Arrows               #-}
module FRP.Chimera.Reactive.Extensions 
  (
    superSamplingUniform
  , afterExp
  ) where

import Control.Monad.Random
import Control.Monad.Trans.MSF.Reader
import FRP.BearRiver

import FRP.Chimera.Random.Reactive 

-- | Performs uniform super sampling on the provided signal-function with n samples
-- 
-- This is done by dividing the current time-delta into N equally spaced samples 
-- and evalutating the signal function with these subsamples and always the same input. 
superSamplingUniform :: Monad m 
                     => Int          -- ^ Number of samples into which the current 
                                     --   time-delta is being uniformly subdivided. 
                                     --   If LT 0, exactly 1 sample will be generated
                     -> SF m a b     -- ^ The signal-function to sample
                     -> SF m a [b]   -- ^ The new signal-function which performs the
                                     --   super sampling, length of [b] is number of samples 
                                     --   (except in case of LT 0, then its 1).
superSamplingUniform n sf 
  | n <= 1    = sf >>> arr return
  | otherwise = readerS $ proc (dt, a) -> do
    let superDt = dt / fromIntegral n
        dtas    = replicate n (superDt, a)
    mapMSF (runReaderS sf) -< dtas

afterExp :: MonadRandom m 
         => Time 
         -> b
         -> SF m a (Event b)
afterExp tAvg b = switch afterExpSwitching (\tExp -> after tExp b)
  where
    afterExpSwitching :: MonadRandom m => SF m a (Event b, Event Time)
    afterExpSwitching = proc _ -> do
      tExp <- randomExpS tAvg -< ()
      returnA -< (NoEvent, Event tExp)
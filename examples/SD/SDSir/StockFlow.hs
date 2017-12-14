{-# LANGUAGE Arrows #-}
module StockFlow 
  (
    susceptibleStock
  , infectiousStock
  , recoveredStock

  , infectionRateFlow
  , recoveryRateFlow
  ) where

import FRP.BearRiver
import FRP.Chimera

import Model

------------------------------------------------------------------------------------------------------------------------
-- STOCKS
------------------------------------------------------------------------------------------------------------------------
susceptibleStock :: Stock
susceptibleStock initValue = proc ain -> do
  let infectionRate = flowInFrom infectionRateFlowId ain

  stockValue <- (initValue+) ^<< integral -< (-infectionRate)

  setAgentObservableS -< stockValue
  stockOutToS -< (stockValue, infectionRateFlowId)

  returnA -< ()

infectiousStock :: Stock
infectiousStock initValue = proc ain -> do
  let infectionRate = flowInFrom infectionRateFlowId ain
  let recoveryRate = flowInFrom recoveryRateFlowId ain

  stockValue <- (initValue+) ^<< integral -< (infectionRate - recoveryRate)
  
  setAgentObservableS -< stockValue
  stockOutToS -< (stockValue, infectionRateFlowId)
  stockOutToS -< (stockValue, recoveryRateFlowId)

  returnA -< ()

recoveredStock :: Stock
recoveredStock initValue = proc ain -> do
  let recoveryRate = flowInFrom recoveryRateFlowId ain

  stockValue <- (initValue+) ^<< integral -< recoveryRate

  setAgentObservableS -< stockValue

  returnA -< ()
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- FLOWS
------------------------------------------------------------------------------------------------------------------------
infectionRateFlow :: Flow
infectionRateFlow = proc ain -> do
  let susceptible = stockInFrom susceptibleStockId ain 
  let infectious = stockInFrom infectiousStockId ain

  let flowValue = (infectious * contactRate * susceptible * infectivity) / totalPopulation
  
  flowOutToS -< (flowValue, susceptibleStockId)
  flowOutToS -< (flowValue, infectiousStockId)

  returnA -< ()

recoveryRateFlow :: Flow
recoveryRateFlow = proc ain -> do
  let infectious = stockInFrom infectiousStockId ain

  let flowValue = infectious / avgIllnessDuration

  flowOutToS -< (flowValue, infectiousStockId)
  flowOutToS -< (flowValue, recoveredStockId)

  returnA -< ()
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE RecursiveDo #-}

-- This financial model is described in
-- Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk.
--
-- It illustrates how you can use the Monte-Carlo simulation and 
-- define external parameters for the system of recursive diffential 
-- equations to provide the Sensitivity Analysis. 
--
-- To enable the parallel simulation, you should compile it
-- with option -threaded and then pass in other options +RTS -N2 -RTS
-- to the executable if you have a dual core processor without
-- hyper-threading. Also you can increase the number
-- of parallel threads via option -N if you have a more modern
-- processor.

module Model 
       (-- * Simulation Model
        model, 
        -- * Variable Names
        netIncomeName,
        netCashFlowName,
        npvIncomeName,
        npvCashFlowName,
        -- * External Parameters
        Parameters(..), 
        defaultParams,
        randomParams) where

import Control.Monad

import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

-- | The model parameters.
data Parameters =
  Parameters { paramsTaxDepreciationTime    :: Parameter Double,
               paramsTaxRate                :: Parameter Double,
               paramsAveragePayableDelay    :: Parameter Double,
               paramsBillingProcessingTime  :: Parameter Double,
               paramsBuildingTime           :: Parameter Double,
               paramsDebtFinancingFraction  :: Parameter Double,
               paramsDebtRetirementTime     :: Parameter Double,
               paramsDiscountRate           :: Parameter Double,
               paramsFractionalLossRate     :: Parameter Double,
               paramsInterestRate           :: Parameter Double,
               paramsPrice                  :: Parameter Double,
               paramsProductionCapacity     :: Parameter Double,
               paramsRequiredInvestment     :: Parameter Double,
               paramsVariableProductionCost :: Parameter Double }

-- | The default model parameters.
defaultParams :: Parameters
defaultParams =
  Parameters { paramsTaxDepreciationTime    = 10,
               paramsTaxRate                = 0.4,
               paramsAveragePayableDelay    = 0.09,
               paramsBillingProcessingTime  = 0.04,
               paramsBuildingTime           = 1,
               paramsDebtFinancingFraction  = 0.6,
               paramsDebtRetirementTime     = 3,
               paramsDiscountRate           = 0.12,
               paramsFractionalLossRate     = 0.06,
               paramsInterestRate           = 0.12,
               paramsPrice                  = 1,
               paramsProductionCapacity     = 2400,
               paramsRequiredInvestment     = 2000,
               paramsVariableProductionCost = 0.6 }

-- | Random parameters for the Monte-Carlo simulation.
randomParams :: IO Parameters
randomParams =
  do averagePayableDelay    <- memoParameter $ randomUniform 0.07 0.11
     billingProcessingTime  <- memoParameter $ randomUniform 0.03 0.05
     buildingTime           <- memoParameter $ randomUniform 0.8 1.2
     fractionalLossRate     <- memoParameter $ randomUniform 0.05 0.08
     interestRate           <- memoParameter $ randomUniform 0.09 0.15
     price                  <- memoParameter $ randomUniform 0.9 1.2
     productionCapacity     <- memoParameter $ randomUniform 2200 2600
     requiredInvestment     <- memoParameter $ randomUniform 1800 2200
     variableProductionCost <- memoParameter $ randomUniform 0.5 0.7
     return defaultParams { paramsAveragePayableDelay    = averagePayableDelay,
                            paramsBillingProcessingTime  = billingProcessingTime,
                            paramsBuildingTime           = buildingTime,
                            paramsFractionalLossRate     = fractionalLossRate,
                            paramsInterestRate           = interestRate,
                            paramsPrice                  = price,
                            paramsProductionCapacity     = productionCapacity,
                            paramsRequiredInvestment     = requiredInvestment,
                            paramsVariableProductionCost = variableProductionCost }

-- | This is the model itself that returns experimental data.
model :: Parameters -> Simulation Results
model params =
  mdo let getParameter f = liftParameter $ f params

      -- the equations below are given in an arbitrary order!

      bookValue <- integ (newInvestment - taxDepreciation) 0
      let taxDepreciation = bookValue / taxDepreciationTime
          taxableIncome = grossIncome - directCosts - losses
                          - interestPayments - taxDepreciation
          production = availableCapacity
          availableCapacity = ifDynamics (time .>=. buildingTime)
                              productionCapacity 0
          taxDepreciationTime = getParameter paramsTaxDepreciationTime
          taxRate = getParameter paramsTaxRate
      accountsReceivable <- integ (billings - cashReceipts - losses)
                            (billings / (1 / averagePayableDelay
                                         + fractionalLossRate))
      let averagePayableDelay = getParameter paramsAveragePayableDelay
      awaitingBilling <- integ (price * production - billings)
                         (price * production * billingProcessingTime)
      let billingProcessingTime = getParameter paramsBillingProcessingTime
          billings = awaitingBilling / billingProcessingTime
          borrowing = newInvestment * debtFinancingFraction
          buildingTime = getParameter paramsBuildingTime
          cashReceipts = accountsReceivable / averagePayableDelay
      debt <- integ (borrowing - principalRepayment) 0
      let debtFinancingFraction = getParameter paramsDebtFinancingFraction
          debtRetirementTime = getParameter paramsDebtRetirementTime
          directCosts = production * variableProductionCost
          discountRate = getParameter paramsDiscountRate
          fractionalLossRate = getParameter paramsFractionalLossRate
          grossIncome = billings
          interestPayments = debt * interestRate
          interestRate = getParameter paramsInterestRate
          losses = accountsReceivable * fractionalLossRate
          netCashFlow = cashReceipts + borrowing - newInvestment
                        - directCosts - interestPayments
                        - principalRepayment - taxes
          netIncome = taxableIncome - taxes
          newInvestment = ifDynamics (time .>=. buildingTime)
                          0 (requiredInvestment / buildingTime)
      npvCashFlow <- npv netCashFlow discountRate 0 1
      npvIncome <- npv netIncome discountRate 0 1
      let price = getParameter paramsPrice
          principalRepayment = debt / debtRetirementTime
          productionCapacity = getParameter paramsProductionCapacity
          requiredInvestment = getParameter paramsRequiredInvestment
          taxes = taxableIncome * taxRate
          variableProductionCost = getParameter paramsVariableProductionCost

      return $
        results 
        [resultSource netIncomeName "Net income" netIncome,
         resultSource netCashFlowName "Net cash flow" netCashFlow,
         resultSource npvIncomeName "NPV income" npvIncome,
         resultSource npvCashFlowName "NPV cash flow" npvCashFlow]

-- the names of the variables we are interested in
netIncomeName   = "netIncome"
netCashFlowName = "netCashFlow"
npvIncomeName   = "npvIncome"
npvCashFlowName = "npvCashFlow"

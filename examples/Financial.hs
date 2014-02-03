
{-# LANGUAGE RecursiveDo #-}

-- This financial model is described in
-- Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk.
--
-- It illustrates how you can use the Monte-Carlo simulation
-- and define external parameters. Here the system of recursive
-- diffential equations is used but the paradigm can be any
-- supported by Aivika including DES or agent-base modeling
-- or their combination.
--
-- To enable the parallel simulation, you should compile it
-- with option -threaded and then pass in other options +RTS -N2 -RTS
-- to the executable if you have a dual core processor without
-- hyper-threading. Also you can increase the number
-- of parallel threads via option -N if you have a more modern
-- processor.

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
model :: Parameters -> Simulation ExperimentData
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

      experimentDataInStartTime
        [(netIncomeName, seriesEntity "Net income" netIncome),
         (netCashFlowName, seriesEntity "Net cash flow" netCashFlow),
         (npvIncomeName, seriesEntity "NPV income" npvIncome),
         (npvCashFlowName, seriesEntity "NPV cash flow" npvCashFlow)]

-- the names of the variables we are interested in
netIncomeName   = "netIncome"
netCashFlowName = "netCashFlow"
npvIncomeName   = "npvIncome"
npvCashFlowName = "npvCashFlow"

-- the simulation specs
specs = Specs 0 5 0.015625 RungeKutta4 SimpleGenerator

-- | The experiment for the Monte-Carlo simulation.
monteCarloExperiment :: Experiment
monteCarloExperiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1000,
    experimentTitle = "Financial Model (the Monte-Carlo simulation)",
    experimentDescription = "Financial Model (the Monte-Carlo simulation) as described in " ++
                            "Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk.",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The deviation chart for Net Income and Cash Flow",
         deviationChartSeries = [Left netIncomeName, 
                                 Left netCashFlowName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The deviation chart for Net Present Value of Income and Cash Flow",
         deviationChartSeries = [Left npvIncomeName, 
                                 Left npvCashFlowName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Histogram for Net Income and Cash Flow",
         finalHistogramSeries = [netIncomeName, netCashFlowName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Histogram for Net Present Value of Income and Cash Flow",
         finalHistogramSeries = [npvIncomeName, npvCashFlowName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Summary for Net Income and Cash Flow",
         finalStatsSeries = [netIncomeName, netCashFlowName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Summary for Net Present Value of Income and Cash Flow",
         finalStatsSeries = [npvIncomeName, npvCashFlowName] } ] }

-- | The experiment with single simulation run.
singleExperiment :: Experiment
singleExperiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentTitle = "Financial Model",
    experimentDescription = "Financial Model as described in " ++
                            "Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk.",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Time series of Net Income and Cash Flow",
         timeSeries = [Left netIncomeName, 
                       Left netCashFlowName] },
       
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Time series of Net Present Value for Income and Cash Flow",
         timeSeries = [Left npvIncomeName, 
                       Left npvCashFlowName] },

       outputView $ defaultTableView {
         tableTitle = "Table",
         tableSeries = [netIncomeName, netCashFlowName,
                        npvIncomeName, npvCashFlowName] } ] }

main = do
  
  -- run the ordinary simulation
  putStrLn "*** The simulation with default parameters..."
  runExperiment singleExperiment $ model defaultParams
  putStrLn ""

  -- run the Monte-Carlo simulation
  putStrLn "*** The Monte-Carlo simulation..."
  randomParams >>= runExperimentParallel monteCarloExperiment . model
